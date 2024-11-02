{-# LANGUAGE RecordWildCards #-}
module SQF.FunctionConfig
    ( ModuleConfig(..)
    , writeModuleConfigs
    ) where

data ModuleConfig = ModuleConfig
    { modulePath      :: FilePath
    , moduleTag       :: String
    , moduleFunctions :: [String]
    }

newline :: Int -> ShowS
newline x = showString $ "\n" ++ replicate (x*4) ' '

writeModuleConfigs :: [ModuleConfig] -> ShowS
writeModuleConfigs mcs
    = showString "// This file is autogenerated by TQF - do not modify manually"
    . flip (foldr (\mc -> newline 0 . writeModuleConfig mc)) mcs
    . newline 0

writeModuleConfig :: ModuleConfig -> ShowS
writeModuleConfig ModuleConfig{..}
    = showString ("class TQF_" <> moduleTag <> " {")
    . newline 1
    . showString ("tag = \"" <> moduleTag <> "\";")
    . newline 1
    . showString "class Functions {"
    . newline 2
    . showString ("file = \"" <> modulePath <> "\"")
    . flip (foldr (\f -> newline 2 . showString ("class " <> f <> "{};"))) moduleFunctions
    . newline 1
    . showString "};"
    . newline 0
    . showString "};"
