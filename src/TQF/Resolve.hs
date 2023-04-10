{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module TQF.Resolve
    ( resolveModule
    , resolveGenericType
    ) where

import           Control.Arrow
import           Control.Error
import           Control.Monad.State
import           Data.Either.Extra   (fromRight, mapLeft)
import           Data.List           (intercalate)
import qualified Data.Map            as Map
import           Data.Tuple.Extra    (firstM)
import           TQF.AST
import           TQF.AST.Annotated
import           TQF.Resolve.Env
import qualified TQF.Type            as Type

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f   = f
applyWhen False id = id

resolveModule
    :: Monad m
    => (ResolveableModule -> m CompiledModule)
    -> Module Parsed
    -> m (Either EnvError (Module Resolved, CompiledModule))
resolveModule loadModule m@Module{..} = flip resolveModule' m <$> foldM handleImport emptyEnv moduleImports
    where
        handleImport env ImportStatement{..} = do
            compiledModule <- loadModule importName
            let qualifiedName = fromMaybe importName importAs
            return $ importModuleToEnv qualifiedName compiledModule
                $ applyWhen (not importQualified) (importModuleToEnv [] compiledModule) env

resolveModule' :: Environment -> Module Parsed -> Either EnvError (Module Resolved, CompiledModule)
resolveModule' env Module{..} = do
    (decls, env') <- foldM (\(ds,e) d -> first (:ds) <$> addTopLevelDeclToEnv moduleName e d) ([],env) moduleDeclarations
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
    = mempty { modLIdents = Map.singleton (lIdentName functionName) functionName }
toCompiledModule VariableDecl{..}
    = mempty { modLIdents = Map.singleton (lIdentName variableName) variableName }
toCompiledModule TypeDecl{..}
    = mempty { modUIdents = Map.singleton typeName (Type.GenericType (unTypeName <$> typeParams) (unAnnot typeValue)) }
toCompiledModule CommandDecl{..}
    = mempty { modLIdents = Map.singleton (lIdentName commandName) commandName}
toCompiledModule ExternalFunctionDecl{..}
    = mempty { modLIdents = Map.singleton (lIdentName functionName) functionName }
toCompiledModule ExternalVariableDecl{..}
    = mempty { modLIdents = Map.singleton (lIdentName variableName) variableName }

addTopLevelDeclToEnv
    :: ResolveableModule
    -> Environment
    -> Declaration Parsed
    -> Either EnvError
        ((Either Type.GenericType ModLIdentDecl, Declaration Parsed),Environment)
addTopLevelDeclToEnv mod env decl = do
    toAdd <- identForDecl mod env (unAnnot decl)
    let env' = either
            (\(n,x) -> addUIdent (UIdent [] n) x env)
            (\(n,x) -> addLIdent (LIdent [] n) x env)
            toAdd
    return ((snd +++ snd $ toAdd, decl), env')

addTypeParams :: [TypeName] -> Environment -> Environment
addTypeParams = flip (foldr (\x-> addUIdent (UIdent [] x) (Type.GenericType [] $ Type.extra $ unTypeName x)))

identForDecl :: ResolveableModule -> Environment -> Declaration_ Parsed -> Either EnvError (Either (TypeName, Type.GenericType) (VarName, ModLIdentDecl))
identForDecl mod env FunctionDecl{..} = do
    let env' = addTypeParams functionTypeParams env
    ret <- resolveType env' functionType
    args <- traverse (resolveType env' . fst) functionArguments
    let sqfName = intercalate "_" (unTypeName <$> mod) ++ "_fnc_" ++ unVarName functionName
    return $ Right $ (functionName,) $ ModLIdentDecl
            { lIdentModule = mod
            , lIdentName = functionName
            , lIdentType = Type.GenericType (unTypeName <$> functionTypeParams)
                $ Type.code ( unAnnot <$> args) (unAnnot ret)
            , lIdentKind = ValueKind
            , lIdentSQFName = sqfName
            }
identForDecl mod env VariableDecl{..} = do
    typ <- resolveType env variableType
    let sqfName = intercalate "_" (unTypeName <$> mod) ++ "_" ++ unVarName variableName
    return $ Right $ (variableName,) $ ModLIdentDecl
            { lIdentModule = mod
            , lIdentName = variableName
            , lIdentType = Type.GenericType [] $ unAnnot typ
            , lIdentKind = ValueKind
            , lIdentSQFName = sqfName
            }
identForDecl _ env TypeDecl{..} = do
    let env' = addTypeParams typeParams env
    typ <- resolveType env' typeValue
    return $ Left (typeName, Type.GenericType (unTypeName <$> typeParams) $ unAnnot typ)
identForDecl mod env CommandDecl{..} = do
    let env' = addTypeParams commandTypeParams env
    ret <- resolveType env' commandReturnType
    args <- traverse (resolveType env' . fst) commandArgs
    return $ Right $ (commandName,) $ ModLIdentDecl
            { lIdentModule = mod
            , lIdentName = commandName
            , lIdentType = Type.GenericType (unTypeName <$> commandTypeParams) $ Type.code (unAnnot <$> args) $ unAnnot ret
            , lIdentKind = toKind args
            , lIdentSQFName = commandSQF
            }
    where
        toKind :: [a] -> IdentKind
        toKind []  = NularCommandKind
        toKind [_] = UnaryCommandKind
        toKind _   = BinaryCommandKind
identForDecl mod env ExternalFunctionDecl{..} = do
    let env' = addTypeParams functionTypeParams env
    ret <- resolveType env' functionType
    args <- traverse (resolveType env' . fst) functionArguments
    return $ Right $ (functionName,) ModLIdentDecl
            { lIdentModule = mod
            , lIdentName = functionName
            , lIdentType = Type.GenericType (unTypeName <$> functionTypeParams) $ Type.code (unAnnot <$> args) $ unAnnot ret
            , lIdentKind = ValueKind
            , lIdentSQFName = functionSQFName
            }
identForDecl mod env ExternalVariableDecl{..} = do
    typ <- resolveType env variableType
    return $ Right $ (variableName,) ModLIdentDecl
            { lIdentModule = mod
            , lIdentName = variableName
            , lIdentType = Type.GenericType [] $ unAnnot typ
            , lIdentKind = ValueKind
            , lIdentSQFName = variableSQFName
            }

resolveDeclaration :: Environment -> Either Type.GenericType ModLIdentDecl -> Declaration Parsed -> Either EnvError (Declaration Resolved)
resolveDeclaration env idnt = traverse (resolveDeclaration' env idnt)

resolveDeclaration' :: Environment -> Either Type.GenericType ModLIdentDecl -> Declaration_ Parsed -> Either EnvError (Declaration_ Resolved)
resolveDeclaration' env' idnt FunctionDecl{..} = do
    let env = addTypeArgsToEnv functionTypeParams env'
    ret <- resolveType env functionType
    args <- traverse (fmap (fst &&& uncurry createLocalDecl) . firstM (resolveType env)) functionArguments
    stmt <- fst <$> resolveStmts (addArgsToEnv args env) functionContent
    return FunctionDecl
        { functionName = fromRight (error "Function returned a type decl") idnt
        , functionType = ret
        , functionTypeParams
        , functionArguments = args
        , functionContent = stmt
        }
    where
        addArgsToEnv :: [(Annot (Type.Type' String), ModLIdentDecl)] -> Environment -> Environment
        addArgsToEnv args env = foldr ((\n -> addLIdent (LIdent [] $ lIdentName n) n) . snd) env args
resolveDeclaration' env idnt VariableDecl{..} = do
    typ <- resolveType env variableType
    return VariableDecl
        { variableName = fromRight (error "Variable returned a type decl") idnt
        , variableType = typ
        }
resolveDeclaration' env' _ TypeDecl{..} = do
    let env = addTypeArgsToEnv typeParams env'
    typ <- resolveType env typeValue
    return TypeDecl
        { typeName = typeName
        , typeParams
        , typeValue = typ
        }
resolveDeclaration' env' idnt CommandDecl{..} = do
    let env = addTypeArgsToEnv commandTypeParams env'
    ret <- resolveType env commandReturnType
    args <-traverse (fmap (fst &&& uncurry createLocalDecl) . firstM (resolveType env)) commandArgs
    return CommandDecl
        { commandName = fromRight (error "Command returned a type decl") idnt
        , commandSQF = commandSQF
        , commandTypeParams
        , commandReturnType = ret
        , commandArgs = args
        }
resolveDeclaration' env' idnt ExternalFunctionDecl{..} = do
    let env = addTypeArgsToEnv functionTypeParams env'
    ret <- resolveType env functionType
    args <- traverse (fmap (fst &&& uncurry createLocalDecl) . firstM (resolveType env)) functionArguments
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

addTypeArgsToEnv :: [TypeName] -> Environment -> Environment
addTypeArgsToEnv typeArgs env = foldr (\x -> addUIdent (UIdent [] x) $ Type.GenericType [] mempty) env typeArgs

createLocalDecl :: Annot (Type.Type' String) -> VarName -> ModLIdentDecl
createLocalDecl typ var = ModLIdentDecl
            { lIdentModule = []
            , lIdentName = var
            , lIdentType = Type.GenericType [] $ unAnnot typ
            , lIdentKind = ValueKind
            , lIdentSQFName = "_" ++ unVarName var
            }

resolveStatement :: Environment -> Statement Parsed -> Either EnvError (Statement Resolved, Environment)
resolveStatement env x = do
    (x', env) <- resolveStatement' env (unAnnot x)
    return (Annot (pos x) x', env)

resolveStatement' :: Environment -> Statement_ Parsed -> Either EnvError (Statement_ Resolved, Environment)
resolveStatement' env (VariableDeclaration typ name mExpr) = do
    typ' <- resolveType env typ
    resExpr <- traverse (fmap fst . resolveExpr env) mExpr
    let env' = addLIdent (LIdent [] name) (createLocalDecl typ' name) env
    return $ (,env') $ VariableDeclaration typ' name resExpr
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
resolveExpr' env (FuncCall n args) =
    fmap (,env) $ FuncCall <$> traverse (resolveValue (pos n) env) n <*> traverse (fmap fst . resolveExpr env) args
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
    stmts' <- traverse (fmap fst . resolveStmts env') stmts
    return $ (,env) $ IfStatement cond' stmts'
resolveExpr' env (WhileLoop cond stmt) = do
    (cond', env') <- resolveExpr env cond
    stmt' <- fst <$> resolveStmts env' stmt
    return (WhileLoop cond' stmt', env)
resolveExpr' env NilLit = return (NilLit, env)

resolveStmts :: Environment -> [Statement Parsed] -> Either EnvError ([Statement Resolved], Environment)
resolveStmts env exprs = runStateT (traverse f exprs) env
    where
        f :: Statement Parsed -> StateT Environment (Either EnvError) (Statement Resolved)
        f stmt = do
            env <- get
            (stmt', env') <- lift $ resolveStatement env stmt
            put env'
            return stmt'

resolveValue :: Range -> Environment -> Ident Parsed -> Either EnvError (Ident Resolved)
resolveValue r env (Ident lIdent args) = do
    i <- lookupLIdent r env lIdent
    args' <- traverse (resolveType env) args
    return $ Ident i args'

resolveType :: Environment -> Annot ParsedType -> Either EnvError (Annot (Type.Type' String))
resolveType env typ = let
    lookupF (idnt,args) = do
        genTyp <- lookupUIdent (pos typ) env idnt
        args' <- traverse (resolveType env) args
        mapLeft (EnvNotFound . Left) $ resolveGenericType (pos typ) genTyp (unAnnot <$> args')
    in Annot (pos typ) <$> Type.resolveType lookupF (unParsedType $ unAnnot typ)

resolveGenericType :: Range -> Type.GenericType -> [Type.Type' String] -> Either (Annot UIdent) (Type.Type' String)
resolveGenericType _ (Type.GenericType [] val) _ = Right val
resolveGenericType r (Type.GenericType argNames val) args = let
    -- TODO: Improve hardness of argument handling here - check matching lengths etc.
    env' = Map.fromList $ zip argNames args
    lookupF x = note (Annot r $ UIdent [] $ TypeName x) $ Map.lookup x env'
    in Type.resolveType lookupF val
