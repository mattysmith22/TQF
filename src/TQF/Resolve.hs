{-# LANGUAGE RecordWildCards, TupleSections #-}
module TQF.Resolve
    ( resolveModule
    ) where

import TQF.AST
import TQF.AST.Annotated
import TQF.Resolve.Env
import Control.Monad (foldM, liftM)
import Control.Arrow
import TQF.Type (Type)
import qualified TQF.Type as Type
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Maybe
import Data.Either.Extra (mapLeft, fromRight)
import Data.Tuple.Extra (firstM)
import Control.Monad.State
import Data.List.NonEmpty ( NonEmpty((:|)) )
import Control.Monad.Trans.Writer.Lazy

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f = f
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
    = mempty { modUIdents = Map.singleton typeName (unAnnot typeValue) }
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
        ((Either Type ModLIdentDecl, Declaration Parsed),Environment)
addTopLevelDeclToEnv mod env decl = do
    toAdd <- identForDecl mod env (unAnnot decl)
    let env' = either
            (\(n,x) -> addUIdent (UIdent [] n) x env)
            (\(n,x) -> addLIdent ([], n) x env)
            toAdd
    return ((snd +++ snd $ toAdd, decl), env')

identForDecl :: ResolveableModule -> Environment -> Declaration_ Parsed -> Either EnvError (Either (TypeName, Type) (VarName, ModLIdentDecl))
identForDecl mod env FunctionDecl{..} = do
    ret <- resolveType env functionType
    args <- traverse (resolveType env . fst) functionArguments
    let sqfName = intercalate "_" (unTypeName <$> mod) ++ "_fnc_" ++ unVarName functionName
    return $ Right $ (functionName,) $ ModLIdentDecl
            { lIdentModule = mod
            , lIdentName = functionName
            , lIdentType = unAnnot $ Type.code <$> sequence args <*> ret
            , lIdentKind = ValueKind
            , lIdentSQFName = sqfName
            }
identForDecl mod env VariableDecl{..} = do
    typ <- resolveType env variableType
    let sqfName = intercalate "_" (unTypeName <$> mod) ++ "_" ++ unVarName variableName
    return $ Right $ (variableName,) $ ModLIdentDecl
            { lIdentModule = mod
            , lIdentName = variableName
            , lIdentType = unAnnot typ
            , lIdentKind = ValueKind
            , lIdentSQFName = sqfName
            }
identForDecl mod env TypeDecl{..} = do
    typ <- resolveType env typeValue
    return $ Left (typeName, unAnnot typ)
identForDecl mod env CommandDecl{..} = do
    ret <- resolveType env commandReturnType
    args <- traverse (resolveType env . fst) commandArgs
    return $ Right $ (commandName,) $ ModLIdentDecl
            { lIdentModule = mod
            , lIdentName = commandName
            , lIdentType = unAnnot $ Type.code <$> sequence args <*> ret
            , lIdentKind = toKind args
            , lIdentSQFName = commandSQF
            }
    where
        toKind :: [a] -> IdentKind
        toKind [] = NularCommandKind
        toKind [_] = UnaryCommandKind
        toKind _ = BinaryCommandKind
identForDecl mod env ExternalFunctionDecl{..} = do
    ret <- resolveType env functionType
    args <- traverse (resolveType env . fst) functionArguments
    return $ Right $ (functionName,) ModLIdentDecl
            { lIdentModule = mod
            , lIdentName = functionName
            , lIdentType = unAnnot $ Type.code <$> sequence args <*> ret
            , lIdentKind = ValueKind
            , lIdentSQFName = functionSQFName
            }
identForDecl mod env ExternalVariableDecl{..} = do
    typ <- resolveType env variableType
    return $ Right $ (variableName,) ModLIdentDecl
            { lIdentModule = mod
            , lIdentName = variableName
            , lIdentType = unAnnot typ
            , lIdentKind = ValueKind
            , lIdentSQFName = variableSQFName
            }

resolveDeclaration :: Environment -> Either Type ModLIdentDecl -> Declaration Parsed -> Either EnvError (Declaration Resolved)
resolveDeclaration env idnt = traverse (resolveDeclaration' env idnt)

resolveDeclaration' :: Environment -> Either Type ModLIdentDecl -> Declaration_ Parsed -> Either EnvError (Declaration_ Resolved)
resolveDeclaration' env idnt d@FunctionDecl{..} = do
    ret <- resolveType env functionType
    args <- traverse (fmap (fst &&& uncurry createLocalDecl) . firstM (resolveType env)) functionArguments
    stmt <- fst <$> resolveStatement (addArgsToEnv args env) functionContent
    return FunctionDecl
        { functionName = fromRight (error "Function returned a type decl") idnt
        , functionType = ret
        , functionArguments = args
        , functionContent = stmt
        }
    where
        addArgsToEnv :: [(Annot Type, ModLIdentDecl)] -> Environment -> Environment
        addArgsToEnv args env = foldr (\(t, n) -> addLIdent ([], lIdentName n) n) env args
resolveDeclaration' env idnt VariableDecl{..} = do
    typ <- resolveType env variableType
    return VariableDecl 
        { variableName = fromRight (error "Variable returned a type decl") idnt 
        , variableType = typ
        }
resolveDeclaration' env idnt TypeDecl{..} = do
    typ <- resolveType env typeValue
    return TypeDecl
        { typeName = typeName
        , typeValue = typ
        }
resolveDeclaration' env idnt CommandDecl{..} = do
    ret <- resolveType env commandReturnType
    args <-traverse (fmap (fst &&& uncurry createLocalDecl) . firstM (resolveType env)) commandArgs
    return CommandDecl
        { commandName = fromRight (error "Command returned a type decl") idnt
        , commandSQF = commandSQF
        , commandReturnType = ret
        , commandArgs = args
        }
resolveDeclaration' env idnt ExternalFunctionDecl{..} = do
    ret <- resolveType env functionType
    args <- traverse (fmap (fst &&& uncurry createLocalDecl) . firstM (resolveType env)) functionArguments
    return ExternalFunctionDecl
        { functionName = fromRight (error "External Function returned a type decl") idnt
        , functionType = ret
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

createLocalDecl :: Annot Type -> VarName -> ModLIdentDecl
createLocalDecl typ var = ModLIdentDecl
            { lIdentModule = []
            , lIdentName = var
            , lIdentType = unAnnot typ
            , lIdentKind = ValueKind
            , lIdentSQFName = "_" ++ unVarName var
            }

resolveStatement :: Environment -> Statement Parsed -> Either EnvError (Statement Resolved, Environment)
resolveStatement env stmt = (\(Annot p (a, b)) -> (Annot p a, b)) <$> traverse (resolveStatement' env) stmt

resolveStatement' :: Environment -> Statement_ Parsed -> Either EnvError (Statement_ Resolved, Environment)
resolveStatement' env (CodeBlock stmts) = first CodeBlock <$> runStateT (traverse f stmts) env
    where
        f :: Statement Parsed -> StateT Environment (Either EnvError) (Statement Resolved)
        f stmt = do
            env <- get
            (stmt',env') <- lift $ resolveStatement env stmt
            put env'
            return stmt'
resolveStatement' env VariableDeclaration{..} = do
    typ <- resolveType env varDeclType
    resExpr <- traverse (resolveExpr env) varDeclValue
    let env' = addLIdent ([], varDeclName) (createLocalDecl typ varDeclName) env
    return $ (,env') VariableDeclaration
        { varDeclType = typ
        , varDeclName = varDeclName
        , varDeclValue = resExpr
        }
resolveStatement' env FunctionCall{..} = do
    function <- traverse (lookupLIdent (pos functionCallName) env) functionCallName
    args <- traverse (resolveExpr env) functionCallArgs
    return $ (,env) FunctionCall
        { functionCallName = function
        , functionCallArgs = args
        }
resolveStatement' env Assignment{..} = do
    var <- traverse (lookupLIdent (pos assignmentVariable) env) assignmentVariable
    expr <- resolveExpr env assignmentValue
    return $ (,env) Assignment
        { assignmentVariable = var
        , assignmentValue = expr
        }
resolveStatement' env IfStatement{..} = do
    expr <- resolveExpr env ifStatementCondition
    stmtTrue <- fst <$> resolveStatement env ifStatementTrue
    stmtFalse <- fmap fst <$> traverse (resolveStatement env) ifStatementFalse
    return $ (,env) IfStatement
        { ifStatementCondition = expr
        , ifStatementTrue = stmtTrue
        , ifStatementFalse = stmtFalse
        }
resolveStatement' env WhileLoop{..} = do
    expr <- resolveExpr env whileLoopCondition
    stmt <- fst <$> resolveStatement env whileLoopStatement
    return $ (,env) WhileLoop
        { whileLoopCondition = expr
        , whileLoopStatement = stmt
        }
resolveStatement' env (Return x) = do
    expr <- traverse (resolveExpr env) x
    return (Return expr, env)

resolveExpr :: Environment -> Expr Parsed -> Either EnvError (Expr Resolved)
resolveExpr env = traverse (resolveExpr' env)

resolveExpr' :: Environment -> Expr_ Parsed -> Either EnvError (Expr_ Resolved)
resolveExpr' env (Variable x) =
    Variable <$> traverse (lookupLIdent (pos x) env) x
resolveExpr' env (FuncCall n args) =
    FuncCall <$> traverse (lookupLIdent (pos n) env) n <*> traverse (resolveExpr env) args
resolveExpr' env (BoolLiteral x) =
    return $ BoolLiteral x
resolveExpr' env (NumLiteral x) =
    return $ NumLiteral x
resolveExpr' env (StringLiteral x) =
    return $ StringLiteral x
resolveExpr' env (ArrayExpr xs) =
    ArrayExpr <$> traverse (resolveExpr env) xs
resolveExpr' env (Cast typ x) =
    Cast <$> resolveType env typ <*> resolveExpr env x
resolveExpr' env (Tuple xs) =
    Tuple <$> traverse (resolveExpr env) xs
resolveExpr' env (BinOp op l r) =
    BinOp op <$> resolveExpr env l <*> resolveExpr env r
resolveExpr' env (UnOp op x) =
    UnOp op <$> resolveExpr env x

resolveType :: Environment -> Annot ASTType -> Either EnvError (Annot Type)
resolveType env typ = traverse (Type.resolveType (lookupUIdent (pos typ) env)) typ