{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Xenesis.Core.Analyzer where

import           Control.Monad      (when)
import           Data.List          (nub)
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO       (readFile)
import qualified Data.Tree          as Tree
import           Data.Yaml
import           GHC.Generics       (Generic)

import           System.FilePath

type AName = T.Text

type AType = AName

type Pair = (AName, AName)

type AModuleId = AName

type ASymbolId = AName

type ASideId = AName

data ASymbol =
  ASymbol
    { aSymbolName  :: ASymbolId
    , aSymbolType  :: AType
    , aSideEffects :: [ASide]
    , usedFuncs    :: [Pair]
    }
  deriving (Show, Generic)

data AModule =
  AModule
    { aModuleName  :: AModuleId
    , usedModules  :: [AModuleId]
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
  ASide ASideType AName
  deriving (Eq, Show, Generic)

type ASideBase = (ASideType, ASideId)

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
  parseJSON =
    withObject "ASymbol" $ \v ->
      ASymbol <$> v .: "symbol" <*> v .:? "type" .!= "" <*> parseASide v <*>
      v .:? "use" .!= []

parseASide :: Object -> Parser [ASide]
parseASide v = do
  ss <- v .:? "side" .!= []
  mapM (\(name, t) -> pure $ ASide name t) ss

instance FromJSON AModule where
  parseJSON =
    withObject "AModule" $ \v ->
      AModule <$> v .: "module" <*> v .:? "use" .!= [] <*> v .:? "types" .!= [] <*>
      v .:? "symbols" .!= []

instance FromJSON AComponentDescription where
  parseJSON =
    withObject "AComponent" $ \v ->
      AComponentDescription <$> v .: "component" <*> v .:? "modules" .!= []

instance FromJSON ASystemDescription where
  parseJSON =
    withObject "ASystem" $ \v ->
      ASystemDescription <$> v .: "system" <*> v .:? "modules" .!= []

instance FromJSON ASideType where
  parseJSON =
    withText "ASideType" $ \case
      "out" -> pure Write
      "in" -> pure Read

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

whoUse :: ASystem -> AModuleId -> ASymbolId -> [Pair]
whoUse system moduleName funcName =
  let modules = aSystemModules system
      pair = (moduleName, funcName)
      isFuncUsePair :: (AModuleId, ASymbolId) -> ASymbol -> Bool
      isFuncUsePair p func = elem p $ usedFuncs func
      getUsePairs ::
           (AModuleId, ASymbolId) -> AModule -> [(AModuleId, ASymbolId)]
      getUsePairs p mod =
        [ (aModuleName mod, aSymbolName fun)
        | fun <- aModuleFuncs mod
        , isFuncUsePair p fun
        ]
   in concatMap (getUsePairs pair) modules

whoAffected :: ASystem -> AModuleId -> ASymbolId -> [Pair]
whoAffected system moduleId symbolId =
  let modules = aSystemModules system
      sides =
        let [ss] =
              [ aSideEffects s
              | m <- modules
              , aModuleName m == moduleId
              , s <- aModuleFuncs m
              , aSymbolName s == symbolId
              ]
         in ss
      opposite sideType =
        ASide
          (if sideType == Write
             then Read
             else Write)
      opposites = map (\(ASide t n) -> opposite t n) sides
      isFuncAffected :: ASide -> ASymbol -> Bool
      isFuncAffected side func = elem side $ aSideEffects func
      getAffected :: [ASide] -> AModule -> [(AModuleId, ASymbolId)]
      getAffected sides mod =
        let affected side =
              [ (aModuleName mod, aSymbolName fun)
              | fun <- aModuleFuncs mod
              , isFuncAffected side fun
              ]
         in concatMap affected sides
   in concatMap (getAffected opposites) modules

getUsedModules :: AModule -> [AModuleId]
getUsedModules = nub . map fst . concatMap usedFuncs . aModuleFuncs

getModuleByName :: ASystem -> AModuleId -> Maybe AModule
getModuleByName sys name =
  case filter (\m -> aModuleName m == name) (aSystemModules sys) of
    []    -> Nothing
    [mod] -> Just mod

modulesTree :: ASystem -> AModuleId -> Tree.Tree AModuleId
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
      pure $
        T.unlines $
        filter
          (not . T.null)
          [x | line <- T.lines content, let x = getData line]

data NodeType
  = UseNode
  | SideNode
  deriving (Show, Eq)

useTree :: ASystem -> AModuleId -> ASymbolId -> Tree.Tree Pair
useTree sys mod sym =
  let pair = (mod, sym)
      useLvl = whoUse sys mod sym
      sideLvl = whoAffected sys mod sym
   in Tree.Node pair $ map (uncurry (useTree sys)) (useLvl ++ sideLvl)

drawUseTree :: Tree.Tree Pair -> IO ()
drawUseTree t =
  putStr $ Tree.drawTree $ fmap (\(m, s) -> show m <> "::" <> show s) t

drawModulesTree :: Tree.Tree AModuleId -> IO ()
drawModulesTree t = putStr $ Tree.drawTree $ fmap T.unpack t
