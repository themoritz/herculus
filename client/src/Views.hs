{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Views where

import           Control.Lens        hiding (view)

import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map

import           Data.Foldable       (for_)
import           Data.Text           (Text)
import qualified Data.Text           as Text

import           React.Flux
import           React.Flux.Internal (toJSString)

import           Lib.Model
import           Lib.Model.Project
import           Lib.Model.Table
import           Lib.Types

import           Store
import           Views.Table

import           Control.DeepSeq     (NFData)
import           GHC.Generics        (Generic)

app :: ReactView ()
app = defineControllerView "app" store $ \st () ->
  cldiv_ "container" $ do
    cldiv_ "menubar" $ do
      cldiv_ "logo" "TABLES"
      projects_ (st ^. stateProjects) (st ^. stateProjectId)
      case st ^. stateProjectId of
        Nothing -> pure ()
        Just prjId -> tables_ (st ^. stateTables) (st ^. stateTableId) prjId
      case st ^. stateError of
        Nothing -> pure ()
        Just t -> do
          h3_ "Error"
          elemText t
    cldiv_ "tableGrid" $ tableGrid_ st
    cldiv_ "footer" $ a_
      [ "href" $= "mailto:Moritz <mdrexl@fastmail.fm>, Ruben <ruben.moor@gmail.com>"
      , "className" $= "link-on-dark"
      , "target" $= "_blank"
      ] "Contact"

--

projects_ :: [Entity Project] -> Maybe (Id Project) -> ReactElementM eh ()
projects_ !ps !mProj = view projects (ps, mProj) mempty

projects :: ReactView ([Entity Project], Maybe (Id Project))
projects = defineView "projects" $ \(ps, mProj) -> cldiv_ "projects" $ do
  ul_ $ for_ ps $ \p -> project_ p (Just (entityId p) == mProj)
  inputNew_ "Add project..." (dispatch . ProjectsCreate . Project)

project_ :: Entity Project -> Bool -> ReactElementM eh ()
project_ !p !s = viewWithSKey project (toJSString $ show $ entityId p) (p, s) mempty

project :: ReactView (Entity Project, Bool)
project = defineView "project" $ \(Entity i p, selected) ->
  li_
    [ classNames
      [ ("link", True)
      , ("active", selected)
      ]
    , onClick $ \_ _ -> dispatch $ ProjectsLoadProject i
    ] $ elemText $ projectName p

--

tables_ :: Map (Id Table) Table -> Maybe (Id Table) -> Id Project -> ReactElementM eh ()
tables_ !ts !mTbl !prj = view tables (ts, mTbl, prj) mempty

tables :: ReactView (Map (Id Table) Table, Maybe (Id Table), Id Project)
tables = defineView "tables" $ \(ts, mTbl, projId) ->
  let ts' = map tupleToEntity $ Map.toList ts
  in cldiv_ "tables" $ do
    ul_ $ for_ ts' $ \t -> table_' t (Just (entityId t) == mTbl)
    inputNew_ "Add table..." (dispatch . TablesCreate . Table projId)

--

table_' :: Entity Table -> Bool -> ReactElementM eh ()
table_' !table' !selected = viewWithSKey table (toJSString $ show $ entityId table') (table', selected) mempty

data TableViewState = TableViewState
  { editable     :: Bool
  , name         :: Text
  , hasNameError :: Bool
  } deriving (Generic, Show, NFData)

initialTableViewState :: TableViewState
initialTableViewState = TableViewState False "" False

table :: ReactView (Entity Table, Bool)
table = defineStatefulView "table" initialTableViewState $ \state (Entity tableId table, selected) ->
  let saveHandler state = (dispatch $ TableSetName tableId (name state), Just state { editable = False })
      inputKeyDownHandler _ evt state
        | keyCode evt == 13 && not (Text.null $ name state) = saveHandler state -- 13 = Enter
        | keyCode evt == 27 = ([] , Just state { editable = False }) -- 27 = ESC
        | otherwise = ([], Just state)
  in li_ [ classNames
             [ ("active", selected)
             , ("link", True)
             ]
         , onClick $ \_ _ _ -> (dispatch $ TablesLoadTable tableId, Nothing)
         ] $
     if editable state
     then div_ $
       input_
         [  classNames
            [ ("inp", True)
            , ("inp-error", hasNameError state)
            ]
         , "value" &= name state
         , onChange $ \evt state ->
             let value = target evt "value"
             in ([], Just state { name = value, hasNameError = Text.null value})
         , onKeyDown inputKeyDownHandler
         ]
     else div_ $ do
       span_ $ elemText $ table ^. tableName

       -- button_
       --   [ "className" $= "pure link-on-dark"
       --   , onClick $ \_ _ state -> ([], Just state { editable = True, name = table  ^. tableName })
       --   ] $ faIcon_ "pencil"

--

inputNew_ :: Text -> (Text -> [SomeStoreAction]) -> ReactElementM eh ()
inputNew_ !p !cb = view inputNew (p, cb) mempty

inputNew :: ReactView (Text, Text -> [SomeStoreAction])
inputNew = defineStatefulView "inputNew" ("" :: Text) $ \curText (p, cb) ->
  input_
    [ "placeholder" &= p
    , "value" &= curText
    , onChange $ \evt _ -> ([], Just $ target evt "value")
    , onKeyDown $ \_ evt curState ->
        if keyCode evt == 13 && not (Text.null curState) -- 13 = Enter
           then (cb curState, Just "")
           else ([], Nothing)
    ]
