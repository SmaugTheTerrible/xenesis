{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Xenesis.Core.Analyzer where

import           Control.Monad      (when)
import           Data.List          (nub)
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO       (readFile)
import qualified Data.Tree          as Tree
import           Data.Yaml
import           GHC.Generics       (Generic)

import System.FilePath

type AName = T.Text

type AType = AName

type Pair = (AName, AName)

data ASymbol =
  ASymbol
    { aSymbolName :: AName
    , aSymbolType :: AType
    , aSideEffects :: [ASide]
    , usedFuncs   :: [Pair]
    }
  deriving (Show, Generic)

data AModule =
  AModule
    { aModuleName  :: AName
    , usedModules  :: [AName]
    , aModuleData  :: [AName]
    , aModuleFuncs :: [ASymbol]
    }
  deriving (Show, Generic)

data AComponent =
  AComponent
    { aComponentName    :: AName
    , aComponentModules :: [AName]
    }
  deriving (Show, Generic)

data ASystem =
  ASystem
    { aSystemName    :: AName
    , aSystemModules :: [AModule]
    }
  deriving (Show, Generic)

data ASide =
  ASide AName ASideType
  deriving (Show, Generic)

type ASideBase = (AName, ASideType)

data ASideType
  = Read
  | Write
  deriving (Eq, Show)

data AComponentDescription =
  AComponentDescription
    { aComponentDescName    :: AName
    , aComponentDescModules :: [FilePath]
    }
  deriving (Show, Generic)

data ASystemDescription =
  ASystemDescription
    { aSystemDescName    :: AName
    , aSystemDescModules :: [FilePath]
    }
  deriving (Show, Generic)

instance FromJSON ASymbol where
  parseJSON = withObject "ASymbol" $ \v -> ASymbol <$> v .: "symbol" <*> v .:? "type" .!= "" <*> parseASide v <*> v .:? "use" .!= []

parseASide :: Object -> Parser [ASide]
parseASide v = do
  ss <- v .:? "side" .!= []
  mapM (\ (name, t) -> pure $ ASide name t) ss

instance FromJSON AModule where
  parseJSON =
    withObject "AModule" $ \v ->
      AModule <$> v .: "module" <*> v .:? "use" .!= [] <*> v .:? "types" .!= [] <*> v .:? "symbols" .!= []

instance FromJSON AComponentDescription where
  parseJSON = withObject "AComponent" $ \v -> AComponentDescription <$> v .: "component" <*> v .:? "modules" .!= []

instance FromJSON ASystemDescription where
  parseJSON = withObject "ASystem" $ \v -> ASystemDescription <$> v .: "system" <*> v .:? "modules" .!= []

instance FromJSON ASideType where
  parseJSON = withText "ASideType" $ \case
                                         "write" -> pure Write
                                         "read" -> pure Read

loadModule :: FilePath -> IO (Either ParseException AModule)
loadModule = decodeFileEither

loadComponent :: FilePath -> IO (Either ParseException AComponentDescription)
loadComponent = decodeFileEither

loadSystem :: Bool -> FilePath -> IO ASystem
loadSystem verbose file = do
  let dir = takeDirectory file
  Right (ASystemDescription name ms) <- decodeFileEither file
  when verbose $ print "System file loaded."
  lms <-
    mapM
      (\m -> do
         when verbose $ print $ "loading: " ++ m
         Right mod <- loadModule (dir </> m)
         pure mod)
      ms
  pure $ ASystem name lms

prettyPrintErr = prettyPrintParseException

name :: String -> AName
name = T.pack

whoUse :: ASystem -> AName -> AName -> [Pair]
whoUse system moduleName funcName =
  let modules = aSystemModules system
      pair = (moduleName, funcName)
      isFuncUsePair :: (AName, AName) -> ASymbol -> Bool
      isFuncUsePair p func = elem p $ usedFuncs func
      getUsePairs :: (AName, AName) -> AModule -> [(AName, AName)]
      getUsePairs p mod = [(aModuleName mod, aSymbolName fun) | fun <- aModuleFuncs mod, isFuncUsePair p fun]
   in concatMap (getUsePairs pair) modules

getUsedModules :: AModule -> [AName]
getUsedModules = nub . map fst . concatMap usedFuncs . aModuleFuncs

getModuleByName :: ASystem -> AName -> Maybe AModule
getModuleByName sys name =
  case filter (\m -> aModuleName m == name) (aSystemModules sys) of
    []    -> Nothing
    [mod] -> Just mod

modulesTree :: ASystem -> AName -> Tree.Tree AName
modulesTree sys name =
  let ms = maybe [] getUsedModules (getModuleByName sys name)
   in Tree.Node name $ map (modulesTree sys) $ filter (/= name) ms

startSeq :: T.Text
startSeq = "!!--"

extract :: FilePath -> IO (Either ParseException AModule)
extract file = do
  content <- extractAnalyzeData file
  pure $ decodeEither' $ encodeUtf8 content
  where
    extractAnalyzeData :: FilePath -> IO T.Text
    extractAnalyzeData file = do
      content <- Data.Text.IO.readFile file
      let getData l =
            case T.splitOn startSeq l of
              [_]    -> ""
              [_, x] -> x
      pure $ T.unlines $ filter (not . T.null) [x | line <- T.lines content, let x = getData line]

useTree :: ASystem -> AName -> AName -> Tree.Tree Pair
useTree sys mod sym =
  let pair = (mod, sym)
      useLvl = whoUse sys mod sym
   in Tree.Node pair $ map (uncurry (useTree sys)) useLvl

drawUseTree :: Tree.Tree Pair -> IO ()
drawUseTree t = putStr $ Tree.drawTree $ fmap (\(m, s) -> show m <> "::" <> show s) t

drawModulesTree :: Tree.Tree AName -> IO ()
drawModulesTree t = putStr $ Tree.drawTree $ fmap T.unpack t
