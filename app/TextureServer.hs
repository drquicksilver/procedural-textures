module Main (main) where

import Network.Wai.Handler.Warp (run)
import Options.Applicative
import Server (ServerConfig (..), defaultServerConfig, serverApp)

data Options = Options
  { optionPort :: Int
  , optionExamples :: FilePath
  , optionRamps :: FilePath
  , optionStatic :: FilePath
  }

main :: IO ()
main = do
  options <- execParser (info (optionsParser <**> helper) (fullDesc <> progDesc "Serve the texture editor and its rendering API"))
  let config =
        defaultServerConfig
          { configExamplesDir = optionExamples options
          , configRampsDir = optionRamps options
          , configStaticDir = optionStatic options
          }
  app <- serverApp config
  putStrLn ("Texture editor on http://localhost:" <> show (optionPort options) <> "/")
  run (optionPort options) app

optionsParser :: Parser Options
optionsParser =
  Options
    <$> option auto (long "port" <> metavar "PORT" <> value 8080 <> showDefault <> help "Port to listen on")
    <*> strOption (long "examples" <> metavar "DIR" <> value (configExamplesDir defaultServerConfig) <> showDefault <> help "Directory of example documents")
    <*> strOption (long "ramps" <> metavar "DIR" <> value (configRampsDir defaultServerConfig) <> showDefault <> help "Directory of built-in ramps")
    <*> strOption (long "static" <> metavar "DIR" <> value (configStaticDir defaultServerConfig) <> showDefault <> help "Built frontend to serve")
