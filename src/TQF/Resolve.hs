{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module TQF.Resolve
    ( resolveModule
    , resolveGenericType
    , Resolved
    , LValue(..)
    ) where

import           Control.Arrow
import           Control.Error
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Either.Extra         (fromRight, mapLeft)
import           Data.Foldable             (foldlM, foldrM)
import           Data.List                 (intercalate)
import qualified Data.Map                  as Map
import           Data.String.Pretty        (Pretty (prettyPrint))
import           TQF.AST
import           TQF.AST.Annotated
import           TQF.Parser                (Parsed, ParsedType (..))
import           TQF.Resolve.Env
import           TQF.Resolve.Types
import qualified TQF.Type                  as Type

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f   = f
applyWhen False id = id

resolveModule
    :: Monad m
    => (ResolveableModule -> m CompiledModule)
    -> Module Parsed
    -> m (Either EnvError (Module Resolved, CompiledModule))
resolveModule loadModule m@Module{..} = flip resolveModule' m <$> foldlM handleImport emptyEnv moduleImports
    where
        handleImport env ImportStatement{..} = do
            compiledModule <- loadModule importName
            let qualifiedName = fromMaybe importName importAs
            return $ importModuleToEnv qualifiedName compiledModule
                $ applyWhen (not importQualified) (importModuleToEnv [] compiledModule) env

resolveModule' :: Environment -> Module Parsed -> Either EnvError (Module Resolved, CompiledModule)
resolveModule' env Module{..} = do
    (decls, env') <- foldlM (\(ds,e) d -> first (:ds) <$> addTopLevelDeclToEnv moduleName e d) ([],envNewScope env) moduleDeclarations
    decls' <- traverse (uncurry $ resolveDeclaration env') decls
    let moduleOut = Module
            { moduleName = moduleName
            , moduleImports = moduleImports
            , moduleDeclarations = decls'
            }
    let compiledModule = foldMap (toCompiledModule . unAnnot) decls'
    return (moduleOut, compiledModule)

toCompiledModule :: Declaration_ Resolved -> CompiledModule
toCompiledModule FunctionDecl{..}
    = mempty { modLIdents = Map.singleton (lIdentName $ unAnnot functionName) (unAnnot functionName) }
toCompiledModule VariableDecl{..}
    = mempty { modLIdents = Map.singleton (lIdentName $ unAnnot variableName) (unAnnot variableName) }
toCompiledModule TypeDecl{..}
    = mempty { modUIdents = Map.singleton (unAnnot typeName) (Type.GenericType (unTypeName . unAnnot <$> typeParams) (unAnnot typeValue)) }
toCompiledModule CommandDecl{..}
    = mempty { modLIdents = Map.singleton (lIdentName $ unAnnot commandName) (unAnnot commandName) }
toCompiledModule ExternalFunctionDecl{..}
    = mempty { modLIdents = Map.singleton (lIdentName $ unAnnot functionName) (unAnnot functionName) }
toCompiledModule ExternalVariableDecl{..}
    = mempty { modLIdents = Map.singleton (lIdentName $ unAnnot variableName) (unAnnot variableName) }

addTopLevelDeclToEnv
    :: ResolveableModule
    -> Environment
    -> Declaration Parsed
    -> Either EnvError
        ((Either (Annot Type.GenericType) (Annot ModLIdentDecl), Declaration Parsed),Environment)
addTopLevelDeclToEnv mod env decl = do
    toAdd <- identForDecl mod env (unAnnot decl)
    env' <- either
            (\(n,x) -> envAdd (UIdent [] <$> n) (unAnnot x) env)
            (\(n,x) -> envAdd (LIdent [] <$> n) (unAnnot x) env)
            toAdd
    return ((snd +++ snd $ toAdd, decl), env')

addTypeParams :: [Annot TypeName] -> Environment -> Either EnvError Environment
addTypeParams = flip (foldrM (\x-> envAdd (UIdent [] <$> x) (Type.GenericType [] $ Type.extra $ unTypeName $ unAnnot x)) )

identForDecl :: ResolveableModule -> Environment -> Declaration_ Parsed -> Either EnvError (Either (Annot TypeName, Annot Type.GenericType) (Annot VarName, Annot ModLIdentDecl))
identForDecl mod env FunctionDecl{..} = do
    env' <- addTypeParams functionTypeParams env
    ret <- resolveType env' functionType
    args <- traverse (resolveType env' . fst) functionArguments
    let sqfName = intercalate "_" (unTypeName <$> mod) ++ "_fnc_" ++ unVarName (unAnnot functionName)
    return $ Right $ (functionName,) $ Annot (pos functionName) ModLIdentDecl
            { lIdentModule = mod
            , lIdentName = unAnnot functionName
            , lIdentType = Type.GenericType (unTypeName . unAnnot <$> functionTypeParams)
                $ Type.code ( unAnnot <$> args) (unAnnot ret)
            , lIdentKind = ValueKind
            , lIdentSQFName = sqfName
            , lIdentId = -1
            }
identForDecl mod env VariableDecl{..} = do
    typ <- resolveType env variableType
    let sqfName = intercalate "_" (unTypeName <$> mod) ++ "_" ++ unVarName (unAnnot variableName)
    return $ Right $ (variableName,) $ Annot (pos variableName) ModLIdentDecl
            { lIdentModule = mod
            , lIdentName = unAnnot variableName
            , lIdentType = Type.GenericType [] $ unAnnot typ
            , lIdentKind = ValueKind
            , lIdentSQFName = sqfName
            , lIdentId = -1
            }
identForDecl _ env TypeDecl{..} = do
    env' <- addTypeParams typeParams env
    typ <- resolveType env' typeValue
    return $ Left (typeName, Annot (pos typeName) $ Type.GenericType (unTypeName . unAnnot <$> typeParams) $ unAnnot typ)
identForDecl mod env CommandDecl{..} = do
    env' <- addTypeParams commandTypeParams env
    ret <- resolveType env' commandReturnType
    args <- traverse (resolveType env' . fst) commandArgs
    return $ Right $ (commandName,) $ Annot (pos commandName) ModLIdentDecl
            { lIdentModule = mod
            , lIdentName = unAnnot commandName
            , lIdentType = Type.GenericType (unTypeName . unAnnot <$> commandTypeParams) $ Type.code (unAnnot <$> args) $ unAnnot ret
            , lIdentKind = toKind args
            , lIdentSQFName = commandSQF
            , lIdentId = -1
            }
    where
        toKind :: [a] -> IdentKind
        toKind []  = NularCommandKind
        toKind [_] = UnaryCommandKind
        toKind _   = BinaryCommandKind
identForDecl mod env ExternalFunctionDecl{..} = do
    env' <- addTypeParams functionTypeParams env
    ret <- resolveType env' functionType
    args <- traverse (resolveType env' . fst) functionArguments
    return $ Right $ (functionName,) $ Annot (pos functionName) ModLIdentDecl
            { lIdentModule = mod
            , lIdentName = unAnnot functionName
            , lIdentType = Type.GenericType (unTypeName . unAnnot <$> functionTypeParams) $ Type.code (unAnnot <$> args) $ unAnnot ret
            , lIdentKind = ValueKind
            , lIdentSQFName = functionSQFName
            , lIdentId = -1
            }
identForDecl mod env ExternalVariableDecl{..} = do
    typ <- resolveType env variableType
    return $ Right $ (variableName,) $ Annot (pos variableName) $ ModLIdentDecl
            { lIdentModule = mod
            , lIdentName = unAnnot variableName
            , lIdentType = Type.GenericType [] $ unAnnot typ
            , lIdentKind = ValueKind
            , lIdentSQFName = variableSQFName
            , lIdentId = -1
            }

resolveDeclaration :: Environment -> Either (Annot Type.GenericType) (Annot ModLIdentDecl) -> Declaration Parsed -> Either EnvError (Declaration Resolved)
resolveDeclaration env idnt = traverse (resolveDeclaration' env idnt)

resolveDeclaration' :: Environment -> Either (Annot Type.GenericType) (Annot ModLIdentDecl) -> Declaration_ Parsed -> Either EnvError (Declaration_ Resolved)
resolveDeclaration' env idnt FunctionDecl{..} = do
    env' <- addTypeArgsToEnv functionTypeParams env
    ret <- resolveType env functionType
    (args,env'') <- resolveArgs env' functionArguments
    stmt <- fst <$> resolveBlock env'' functionContent
    return FunctionDecl
        { functionName = fromRight (error "Function returned a type decl") idnt
        , functionType = ret
        , functionTypeParams
        , functionArguments = args
        , functionContent = stmt
        }
resolveDeclaration' env idnt VariableDecl{..} = do
    typ <- resolveType env variableType
    return VariableDecl
        { variableName = fromRight (error "Variable returned a type decl") idnt
        , variableType = typ
        }
resolveDeclaration' env' _ TypeDecl{..} = do
    env <- addTypeArgsToEnv typeParams env'
    typ <- resolveType env typeValue
    return TypeDecl
        { typeName = typeName
        , typeParams
        , typeValue = typ
        }
resolveDeclaration' env' idnt CommandDecl{..} = do
    env <- addTypeArgsToEnv commandTypeParams env'
    ret <- resolveType env commandReturnType
    (args,_) <- resolveArgs env commandArgs
    return CommandDecl
        { commandName = fromRight (error "Command returned a type decl") idnt
        , commandSQF = commandSQF
        , commandTypeParams
        , commandReturnType = ret
        , commandArgs = args
        }
resolveDeclaration' env' idnt ExternalFunctionDecl{..} = do
    env <- addTypeArgsToEnv functionTypeParams env'
    ret <- resolveType env functionType
    (args,_) <- resolveArgs env functionArguments
    return ExternalFunctionDecl
        { functionName = fromRight (error "External Function returned a type decl") idnt
        , functionType = ret
        , functionTypeParams
        , functionArguments = args
        , functionSQFName = functionSQFName
        }
resolveDeclaration' env idnt ExternalVariableDecl{..} = do
    typ <- resolveType env variableType
    return ExternalVariableDecl
        { variableName = fromRight (error "Variable returned a type decl") idnt
        , variableType = typ
        , variableSQFName = variableSQFName
        }

resolveArgs
    :: Environment
    -> [(Annot ParsedType, Annot VarName)]
    -> Either EnvError ([(Annot (Type.Type' String), Annot ModLIdentDecl)], Environment)
resolveArgs env args = runStateT (traverse (\(typ,var) -> do
        e <- get
        rtyp <- lift $ resolveType e typ
        let decl = createLocalDecl e rtyp $ unAnnot var
        e' <- lift $ envAdd (LIdent [] <$> var) decl e
        put e'
        return (rtyp,Annot (pos var) decl)) args) env

addTypeArgsToEnv :: [Annot TypeName] -> Environment -> Either EnvError Environment
addTypeArgsToEnv typeArgs env = foldrM (\x -> envAdd (UIdent [] <$> x) $ Type.GenericType [] $ Type.extra $ unTypeName $ unAnnot x) env typeArgs

createLocalDecl :: Environment -> Annot (Type.Type' String) -> VarName -> ModLIdentDecl
createLocalDecl env typ var = ModLIdentDecl
            { lIdentModule = []
            , lIdentName = var
            , lIdentType = Type.GenericType [] $ unAnnot typ
            , lIdentKind = ValueKind
            , lIdentSQFName = "_" ++ unVarName var
            , lIdentId = envCount (LIdent [] var) env
            }

resolveStatement :: Environment -> Statement Parsed -> Either EnvError (Statement Resolved, Environment)
resolveStatement env x = do
    (x', env) <- resolveStatement' env (unAnnot x)
    return (Annot (pos x) x', env)

resolveStatement' :: Environment -> Statement_ Parsed -> Either EnvError (Statement_ Resolved, Environment)
resolveStatement' env (VariableDeclaration typ name mExpr) = do
    typ' <- resolveType env typ
    resExpr <- traverse (fmap fst . resolveExpr env) mExpr
    let decl = createLocalDecl env typ' <$> name
    env' <- envAdd (LIdent [] <$> name) (unAnnot decl) env
    return $ (,env') $ VariableDeclaration typ' decl resExpr
resolveStatement' env (Assignment var expr) = do
    var' <- resolveLValue . fst =<< resolveExpr env var
    expr' <- fst <$> resolveExpr env expr
    return $ (,env) $ Assignment var' expr'
resolveStatement' env (Expr x) = first Expr <$> resolveExpr env x

resolveLValue :: Expr Resolved -> Either EnvError (LValue Resolved)
resolveLValue e@(Annot _ (Variable x))
    = case lIdentKind $ identName $ unAnnot x of
        ValueKind -> return $ LValueVar x
        _         -> Left $ EnvInvalidLvalue e
resolveLValue (Annot _ (FieldAccess x field))
    = return $ LValueField x field
resolveLValue x = Left $ EnvInvalidLvalue x

resolveExpr :: Environment -> Expr Parsed -> Either EnvError (Expr Resolved, Environment)
resolveExpr env x = do
    (x', env) <- resolveExpr' env (unAnnot x)
    return (Annot (pos x) x', env)

resolveExpr' :: Environment -> Expr_ Parsed -> Either EnvError (Expr_ Resolved, Environment)
resolveExpr' env (Variable x) =
    (,env) . Variable <$> traverse (resolveValue (pos x) env) x
resolveExpr' env (FuncCall n args) = do
    (n',env') <- resolveExpr env n
    fmap (,env') $ FuncCall n' <$> traverse (fmap fst . resolveExpr env) args
resolveExpr' env (BoolLiteral x) =
    return (BoolLiteral x, env)
resolveExpr' env (NumLiteral x) =
    return (NumLiteral x, env)
resolveExpr' env (StringLiteral x) =
    return (StringLiteral x, env)
resolveExpr' env (ArrayExpr xs) =
    (,env) . ArrayExpr <$> traverse (fmap fst . resolveExpr env) xs
resolveExpr' env (Cast typ x) =
    fmap (,env) $ Cast <$> resolveType env typ <*> (fst <$> resolveExpr env x)
resolveExpr' env (FieldAccess e field) = do
    (e',env') <- resolveExpr env e
    return $ (,env') $ FieldAccess e' field
resolveExpr' env (Tuple xs) =
    fmap (,env) $ Tuple <$> traverse (fmap fst . resolveExpr env) xs
resolveExpr' env (BinOp op l r) =
    fmap (,env) $ BinOp op <$> (fst <$> resolveExpr env l) <*> (fst <$> resolveExpr env r)
resolveExpr' env (UnOp op x) =
    (,env) . UnOp op . fst <$> resolveExpr env x
resolveExpr' env (IfStatement cond stmts) = do
    (cond', env') <- resolveExpr env cond
    stmts' <- traverse (fmap fst . resolveBlock env') stmts
    return $ (,env) $ IfStatement cond' stmts'
resolveExpr' env (WhileLoop cond stmt) = do
    (cond', env') <- resolveExpr env cond
    stmt' <- fst <$> resolveBlock env' stmt
    return (WhileLoop cond' stmt', env)
resolveExpr' env NilLit = return (NilLit, env)

resolveBlock :: Environment -> [Statement Parsed] -> Either EnvError ([Statement Resolved], Environment)
resolveBlock env exprs = runStateT (traverse f exprs) $ envNewScope env
    where
        f :: Statement Parsed -> StateT Environment (Either EnvError) (Statement Resolved)
        f stmt = do
            env <- get
            (stmt', env') <- lift $ resolveStatement env stmt
            put env'
            return stmt'

resolveValue :: Range -> Environment -> Ident Parsed -> Either EnvError (Ident Resolved)
resolveValue r env (Ident lIdent args) = do
    i <- envLookup r env lIdent
    args' <- traverse (resolveType env) args
    return $ Ident i args'

resolveType :: Environment -> Annot ParsedType -> Either EnvError (Annot (Type.Type' String))
resolveType env typ = let
    lookupF (idnt,args) = do
        genTyp <- envLookup (pos typ) env (idnt::UIdent)
        args' <- traverse (resolveType env) args
        mapLeft (EnvNotFound . fmap prettyPrint) $ resolveGenericType (pos typ) genTyp (unAnnot <$> args')
    in Annot (pos typ) <$> Type.resolveType lookupF (unParsedType $ unAnnot typ)

resolveGenericType :: Range -> Type.GenericType -> [Type.Type' String] -> Either (Annot UIdent) (Type.Type' String)
resolveGenericType _ (Type.GenericType [] val) _ = Right val
resolveGenericType r (Type.GenericType argNames val) args = let
    -- TODO: Improve hardness of argument handling here - check matching lengths etc.
    env' = Map.fromList $ zip argNames args
    lookupF x = note (Annot r $ UIdent [] $ TypeName x) $ Map.lookup x env'
    in Type.resolveType lookupF val
