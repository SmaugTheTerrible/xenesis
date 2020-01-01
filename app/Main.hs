module Main where

import           Control.Monad       (void)
import           Data.Semigroup      ((<>))
import           Options.Applicative

import Utils

data Options =
  Analyze
    { operation  :: AnalyzeOperation
    , moduleName :: String
    , symName    :: String
    , system     :: FilePath
    }
  deriving (Show)

data AnalyzeOperation
  = WhoUse
  | DrawUseTree
  deriving (Show)

main :: IO ()
main = do
  o <- execParser opts
  case o of
    Analyze DrawUseTree mod sym sys -> drawTree sys mod sym

opts :: ParserInfo Options
opts = info (options <**> helper) (fullDesc <> header "Util for system analyzis." <> progDesc "")

options :: Parser Options
options = subparser $ command "analyze" (info (analyzeParser <**> helper) idm)

analyzeParser :: Parser Options
analyzeParser = Analyze <$> operation' <*> module' <*> symbol <*> system'

system' :: Parser FilePath
system' = argument str (metavar "SYSTEM")

module' :: Parser String
module' = strOption (long "module" <> metavar "MODULE" <> short 'm' <> help "Module name.")

symbol :: Parser String
symbol = strOption (long "symbol" <> metavar "SYMBOL" <> short 's' <> help "Symbol name.")

operation' :: Parser AnalyzeOperation
operation' =
  flag' WhoUse (short 'w' <> help "Gets a tree of usage provided by Module and Symbol") <|>
  flag' DrawUseTree (short 'd' <> help "Draw usage tree provided by Module and Symbol into stdout.")
