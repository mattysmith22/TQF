{-# LANGUAGE RecordWildCards, TupleSections #-}
module TQF.Resolve
    ( resolveModule
    ) where

import TQF.AST
import TQF.AST.Annotated
import TQF.Environment
import Control.Monad (foldM, liftM)
import Control.Arrow (first)
import TQF.Type (Type)
import qualified TQF.Type as Type
import Data.Maybe
import Data.Either.Extra (mapLeft)
import Data.Tuple.Extra (firstM)
import Control.Monad.State
import Data.List.NonEmpty ( NonEmpty((:|)) )

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f = f
applyWhen False id = id

resolveModule
    :: Monad m
    => (ResolveableModule -> m CompiledModule)
    -> Module Parsed
    -> m (Either EnvError (Module Resolved))
resolveModule loadModule m@Module{..} = flip resolveModule' m <$> foldM handleImport emptyEnv moduleImports
    where
        handleImport env ImportStatement{..} = do
            compiledModule <- loadModule importName
            let qualifiedName = fromMaybe importName importAs
            return $ importModuleToEnv qualifiedName compiledModule
                $ applyWhen (not importQualified) (importModuleToEnv [] compiledModule) env          

resolveModule' :: Environment -> Module Parsed -> Either EnvError (Module Resolved) 
resolveModule' env Module{..} = do
    env' <- foldM (addTopLevelDeclToEnv moduleName) env moduleDeclarations
    decls <- traverse (resolveDeclaration env') moduleDeclarations
    return
        Module
            { moduleName = moduleName
            , moduleImports = moduleImports
            , moduleDeclarations = decls
            }

addTopLevelDeclToEnv :: ResolveableModule -> Environment -> Declaration Parsed -> Either EnvError Environment
addTopLevelDeclToEnv mod env (Annot r FunctionDecl{..}) = do
    ret <- resolveType env functionType
    args <- traverse (resolveType env . fst) functionArguments
    let toAdd = ModFunction (mod, functionName) (unAnnot <$> args) (unAnnot ret)
    return $ addLIdent ([], functionName) toAdd env
addTopLevelDeclToEnv mod env (Annot r VariableDecl{..}) = do
    typ <- resolveType env variableType
    let toAdd = ModGlobalVariable (mod, variableName) (unAnnot typ)
    return $ addLIdent ([], variableName) toAdd env
addTopLevelDeclToEnv mod env (Annot r TypeDecl{..}) = do
    typ <- resolveType env typeValue
    return $ addUIdent (UIdent [] typeName) (unAnnot typ) env
addTopLevelDeclToEnv mod env (Annot r CommandDecl{..}) = do
    ret <- resolveType env commandReturnType
    args <- traverse (resolveType env) commandArgs
    return $ addCommand commandName (unAnnot <$> args, unAnnot ret) env
addTopLevelDeclToEnv mod env (Annot r ExternalFunctionDecl{..}) = do
    ret <- resolveType env functionType
    args <- traverse (resolveType env . fst) functionArguments
    let toAdd = ModExternalReference functionSQFName (Type.code (unAnnot <$> args) (unAnnot ret))
    return $ addLIdent ([], functionName) toAdd env
addTopLevelDeclToEnv mod env (Annot r ExternalVariableDecl{..}) = do
    typ <- resolveType env variableType
    let toAdd = ModExternalReference variableSQFName (unAnnot typ)
    return $ addLIdent ([], variableName) toAdd env

resolveDeclaration :: Environment -> Declaration Parsed -> Either EnvError (Declaration Resolved)
resolveDeclaration env = traverse (resolveDeclaration' env)

resolveDeclaration' :: Environment -> Declaration_ Parsed -> Either EnvError (Declaration_ Resolved)
resolveDeclaration' env FunctionDecl{..} = do
    ret <- resolveType env functionType
    args <- traverse (firstM (resolveType env)) functionArguments
    stmt <- fst <$> resolveStatement (addArgsToEnv (first unAnnot <$> args) env) functionContent
    return FunctionDecl
        { functionName = functionName
        , functionType = ret
        , functionArguments = args
        , functionContent = stmt
        }
    where
        addArgsToEnv :: [(Type, VarName)] -> Environment -> Environment
        addArgsToEnv args env = foldr (\(t, n) -> addLIdent ([], n) (ModLocalVariable n t)) env args
resolveDeclaration' env VariableDecl{..} = do
    typ <- resolveType env variableType
    return VariableDecl 
        { variableName = variableName
        , variableType = typ
        }
resolveDeclaration' env TypeDecl{..} = do
    typ <- resolveType env typeValue
    return TypeDecl
        { typeName = typeName
        , typeValue = typ
        }
resolveDeclaration' env CommandDecl{..} = do
    ret <- resolveType env commandReturnType
    args <- traverse (resolveType env) commandArgs
    return CommandDecl
        { commandName = commandName
        , commandReturnType = ret
        , commandArgs = args
        }
resolveDeclaration' env ExternalFunctionDecl{..} = do
    ret <- resolveType env functionType
    args <- traverse (firstM $ resolveType env) functionArguments
    return ExternalFunctionDecl
        { functionName = functionName
        , functionType = ret
        , functionArguments = args
        , functionSQFName = functionSQFName
        }
resolveDeclaration' env ExternalVariableDecl{..} = do
    typ <- resolveType env variableType
    return ExternalVariableDecl
        { variableName = variableName
        , variableType = typ
        , variableSQFName = variableSQFName
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
    let env' = addLIdent ([], varDeclName) (ModLocalVariable varDeclName (unAnnot typ)) env
    return $ (,env') VariableDeclaration
        { varDeclType = typ
        , varDeclName = varDeclName
        , varDeclValue = resExpr
        }
resolveStatement' env FunctionCall{..} = do
    function <- traverse (lookupLIdent env) functionCallName
    args <- traverse (resolveExpr env) functionCallArgs
    return $ (,env) FunctionCall
        { functionCallName = function
        , functionCallArgs = args
        }
resolveStatement' env Assignment{..} = do
    var <- traverse (lookupLIdent env) assignmentVariable
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
resolveStatement' env (DirectCallStmt x args) = do
    (,env) . DirectCallStmt (lookupCommand env x) <$> traverse (resolveExpr env) args

resolveExpr :: Environment -> Expr Parsed -> Either EnvError (Expr Resolved)
resolveExpr env = traverse (resolveExpr' env)

resolveExpr' :: Environment -> Expr_ Parsed -> Either EnvError (Expr_ Resolved)
resolveExpr' env (Variable x) =
    Variable <$> traverse (lookupLIdent env) x
resolveExpr' env (FuncCall n args) =
    FuncCall <$> traverse (lookupLIdent env) n <*> traverse (resolveExpr env) args
resolveExpr' env (BoolLiteral x) =
    return $ BoolLiteral x
resolveExpr' env (NumLiteral x) =
    return $ NumLiteral x
resolveExpr' env (StringLiteral x) =
    return $ StringLiteral x
resolveExpr' env (ArrayExpr xs) =
    ArrayExpr <$> traverse (resolveExpr env) xs
resolveExpr' env (DirectCall x args) =
    DirectCall (lookupCommand env x) <$> traverse (resolveExpr env) args
resolveExpr' env (Cast typ x) =
    Cast <$> resolveType env typ <*> resolveExpr env x
resolveExpr' env (Tuple xs) =
    Tuple <$> traverse (resolveExpr env) xs

resolveType :: Environment -> Annot ASTType -> Either EnvError (Annot Type)
resolveType env = traverse (Type.resolveType (lookupUIdent env))