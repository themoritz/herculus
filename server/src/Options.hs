module Options
  ( Options (..)
  , getOptions
  ) where

import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Network.Socket      (HostName)

import           Options.Applicative

data Options = Options
  { optPort            :: Int
  , optMongoHost       :: HostName
  , optAssetDir        :: String
  , optMongoCollection :: Text
  }

txtOption :: Mod OptionFields String -> Parser Text
txtOption = fmap Text.pack . strOption

getOptions :: IO Options
getOptions = execParser $ info (helper <*> options)
  (  fullDesc
  <> progDesc "Serves the app"
  <> header   "Server"
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
                  <> value "../assets/public"
                  <> showDefault
                  <> help "directory to static files and js"
                  )
  <*> txtOption   (  long "database-name"
                  <> short 'd'
                  <> metavar "DATABASE"
                  <> value "herculus"
                  <> showDefault
                  <> help "Database name for Mongo database"
                  )
