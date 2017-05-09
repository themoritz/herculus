-- |

module Options
  ( Options (..)
  , getOptions
  ) where

import           Lib.Prelude

import           Options.Applicative

data Options = Options
  { optGenPrelude :: Bool
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> options)
  (  fullDesc
  )

options :: Parser Options
options = Options
  <$> switch   (  long "prelude"
               <> short 'p'
               <> showDefault
               <> help "Generate documentation for the prelude."
               )
