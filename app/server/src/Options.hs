module Options
  ( Options (..)
  , getOptions
  ) where

import           Lib.Prelude
import           Prelude             (String)

import           Data.Text           (Text, pack)
import           Network.Socket      (HostName)

import           Options.Applicative

data Options = Options
  { optPort            :: Int
  , optMongoHost       :: HostName
  , optAssetDir        :: String
  , optTexInputs       :: FilePath
  , optMongoCollection :: Text
  }

txtOption :: Mod OptionFields String -> Parser Text
txtOption = fmap pack . strOption

getOptions :: IO Options
getOptions = execParser $ info (helper <*> options)
  (  fullDesc
  )

options :: Parser Options
options = Options
  <$> option auto (  long "port"
                  <> short 'p'
                  <> metavar "PORT"
                  <> value 3000
                  <> showDefault
                  <> help "Server port"
                  )
  <*> strOption   (  long "mongo-host"
                  <> short 'm'
                  <> metavar "HOSTNAME"
                  <> value "127.0.0.1"
                  <> showDefault
                  <> help "Hostname of Mongo database"
                  )
  <*> strOption   (  long "asset-directory"
                  <> short 'a'
                  <> metavar "ASSETDIR"
                  <> value "../client/public"
                  <> showDefault
                  <> help "directory to static files and js"
                  )
  <*> strOption   (  long "texinputs"
                  <> short 't'
                  <> metavar "TEXINPUTS"
                  <> value "."
                  <> showDefault
                  <> help "directory that should be made available to the \
                          \LaTeX report generator."
                  )
  <*> txtOption   (  long "database-name"
                  <> short 'd'
                  <> metavar "DATABASE"
                  <> value "herculus"
                  <> showDefault
                  <> help "Database name for Mongo database"
                  )
