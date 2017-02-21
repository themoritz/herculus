-- |

module Options
  ( Options (..)
  , getOptions
  ) where

import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as Text

import           Options.Applicative

data Options = Options
  { optTarget            :: String
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> options)
  (  fullDesc
  )

options :: Parser Options
options = Options
  <$> strOption   (  long "target"
                  <> short 't'
                  <> metavar "TARGET"
                  <> value "./src"
                  <> showDefault
                  <> help "Target folder for the generated code"
                  )
