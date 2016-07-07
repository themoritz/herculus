module Handler.Rest where

import Data.Text

import Servant

import Lib
import Lib.Api.Rest

handle :: ServerT Routes IO
handle =
       handleProject
  :<|> handleTable
  :<|> undefined

handleProject :: ServerT ProjectRoutes IO
handleProject =
       handleProjectCreate
  :<|> handleProjectList

handleProjectCreate :: Text -> IO (Id Project)
handleProjectCreate name = do
  undefined

handleProjectList :: IO [(Id Project, Text)]
handleProjectList = undefined

handleTable :: ServerT TableRoutes IO
handleTable = undefined
