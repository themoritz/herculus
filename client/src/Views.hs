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

import           Lib.Model.Project
import           Lib.Model.Table
import           Lib.Types

import           Store
import           Views.Table

import           Helper              (keyENTER, keyESC)

import           Control.DeepSeq     (NFData)
import           GHC.Generics        (Generic)

app :: ReactView ()
app = defineControllerView "app" store $ \st () -> do
  screencasts_
  cldiv_ "container" $ do
    cldiv_ "menubar" $ do
      cldiv_ "logo" "Herculus"
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

screencasts_ :: ReactElementM eh ()
screencasts_ = view screencasts () mempty

screencasts :: ReactView ()
screencasts = defineStatefulView "screencasts" (True, 0 :: Int) $ \(open, selected) () ->
    if open
    then cldiv_ "screencasts-container" $ cldiv_ "screencasts" $ do
      cldiv_ "toc" $ ul_ $ do
        for_ (zip [0..] screencastContent) $ \(i, (title, _, _)) ->
          li_ [ onClick $ \_ _ -> const ([], Just (True, i))
              , classNames [ ("active", i == selected) ]
              ] $ cldiv_ "wrapper" $ do
          cldiv_ "cell" $ faIcon_ "caret-right fa-fw"
          cldiv_ "cell" $ elemText title
        li_ [ onClick $ \_ _ -> const ([], Just (False, 0))
            , "className" $= "goto-tool"
            ] $ do
          faIcon_ "rocket fa-fw"
          "Open the Tool!"
      let (title, desc, img) = screencastContent !! selected
      cldiv_ "entry" $ do
        cldiv_ "cast" $ do
          h1_ $ elemText title
          img_ [ "src" $= img ] mempty
        cldiv_ "description" desc
      cldiv_ "controls" $ if selected == length screencastContent - 1
        then clbutton_ "" (const ([], Just (False, 0))) $ do
               faIcon_ "rocket"
               "Open the Tool!"
        else clbutton_ "" (const ([], Just (True, selected + 1))) $ do
               faIcon_ "arrow-right"
               "Next"
    else mempty
  where
    screencastContent =
      [ ( "Spreadsheet-like Look and Feel"
        , ul_ $ do
            li_ "Quickly create tables and columns in a responsive interface"
            li_ "Fill the cells with your data or use formulas"
            li_ "Have changes to your data propagate automatically"
        , "img/spreadsheet.final.gif"
        )
      , ( "Expressive Types"
        , ul_ $ do
            li_ "Choose the data type you need for each column"
            li_ "Use the row type to have cells with records from other tables"
            li_ "Use the date picker for date columns or the dropdown menu \
                 \to select rows."
            li_ "Select and edit more elaborate types like a list of rows of\
                \ Books, with unlimited flexibility"
        , "img/types.final.gif"
        )
      , ( "Powerful Formulas"
        , ul_ $ do
            li_ "Use formulas as simple as \"$A * $B\" or as \
                \advanced as \"sum (map getPrice $Books)\""
            li_ "Create custom formulas with a few lines of code using the\
                \ expressive built-in language"
            li_ "Have your formulas automatically type-checked to avoid bugs"
        , "img/formulas.final.gif"
        )
      , ( "Automated Reports"
        , ul_ $ do
            li_ "Create a PDF-report from your data with a few clicks, or \
                \choose any other output format"
            li_ "Choose among different formatting languages and fine-tune \
                \your custom template in detail"
            li_ "Make use of the built-in template language to fill the \
                \report with your own data"
        , "img/reports.final.gif"
        )
      ]

--

projects_ :: Map (Id Project) Project -> Maybe (Id Project) -> ReactElementM eh ()
projects_ !ps !mProj = view projects (ps, mProj) mempty

projects :: ReactView (Map (Id Project) Project, Maybe (Id Project))
projects = defineView "projects" $ \(ps, mProj) ->
  cldiv_ "projects" $ do
    ul_ $ for_ (Map.toList ps) $ \(i, p) -> project_ i p (Just i == mProj)
    inputNew_ "Add project..." (dispatch . ProjectsCreate . Project)

data ProjectViewState = ProjectViewState
  { pEditable  :: Bool
  , pName      :: Text
  , pNameError :: Bool
  } deriving (Generic, Show, NFData)

initialProjectViewState :: ProjectViewState
initialProjectViewState = ProjectViewState False "" False

project_ :: Id Project -> Project -> Bool -> ReactElementM eh ()
project_ !projectId !project' !selected = viewWithSKey project (toJSString $ show projectId) (projectId, project', selected) mempty

project :: ReactView (Id Project, Project, Bool)
project = defineStatefulView "project" initialProjectViewState $ \state (projectId, project', selected) ->
  let saveHandler st = (dispatch $ ProjectSetName projectId (pName st), Just st { pEditable = False })
      inputKeyDownHandler _ evt st
        | keyENTER evt && not (Text.null $ pName st) = saveHandler st
        | keyESC evt = ([] , Just st { pEditable = False })
        | otherwise = ([], Just st)
  in li_
     [ classNames
       [ ("link", True)
       , ("active", selected)
       ]
     , onClick $ \_ _ _ -> (dispatch $ ProjectsLoadProject projectId, Nothing)
     ] $
     if pEditable state
     then div_ $
          input_
          [  classNames
             [ ("inp", True)
             , ("inp-error", pNameError state)
             ]
          , "value" &= pName state
          , onChange $ \evt st ->
              let value = target evt "value"
              in ([], Just st { pName = value, pNameError = Text.null value})
          , onKeyDown inputKeyDownHandler
          ]
     else div_ $ do
       span_ $ elemText $ project' ^. projectName
       button_
         [ "className" $= "pure link-on-dark"
         , onClick $ \_ _ st -> ([], Just st { pEditable = True, pName = project' ^. projectName})
         ] $ faIcon_ "pencil"

--

tables_ :: Map (Id Table) Table -> Maybe (Id Table) -> Id Project -> ReactElementM eh ()
tables_ !ts !mTbl !prj = view tables (ts, mTbl, prj) mempty

tables :: ReactView (Map (Id Table) Table, Maybe (Id Table), Id Project)
tables = defineView "tables" $ \(ts, mTbl, projId) ->
  cldiv_ "tables" $ do
    ul_ $ for_ (Map.toList ts) $ \(tableId, table') -> table_' tableId table' (Just tableId == mTbl)
    inputNew_ "Add table..." (dispatch . TablesCreate . Table projId)

--

table_' :: Id Table -> Table -> Bool -> ReactElementM eh ()
table_' !tableId !table' !selected = viewWithSKey table (toJSString $ show tableId) (tableId, table', selected) mempty

data TableViewState = TableViewState
  { tEditable  :: Bool
  , tName      :: Text
  , tNameError :: Bool
  } deriving (Generic, Show, NFData)

initialTableViewState :: TableViewState
initialTableViewState = TableViewState False "" False

table :: ReactView (Id Table, Table, Bool)
table = defineStatefulView "table" initialTableViewState $ \state (tableId, table', selected) ->
  let saveHandler st = (dispatch $ TableSetName tableId (tName st), Just st { tEditable = False })
      inputKeyDownHandler _ evt st
        | keyENTER evt && not (Text.null $ tName st) = saveHandler st
        | keyESC evt = ([] , Just st { tEditable = False })
        | otherwise = ([], Just st)
  in li_ [ classNames
             [ ("active", selected)
             , ("link", True)
             ]
         , onClick $ \_ _ _ -> (dispatch $ TablesLoadTable tableId, Nothing)
         ] $
     if tEditable state
     then div_ $
       input_
         [  classNames
            [ ("inp", True)
            , ("inp-error", tNameError state)
            ]
         , "value" &= tName state
         , onChange $ \evt st ->
             let value = target evt "value"
             in ([], Just st { tName = value, tNameError = Text.null value})
         , onKeyDown inputKeyDownHandler
         ]
     else div_ $ do
       span_ $ elemText $ table' ^. tableName

       button_
         [ "className" $= "pure link-on-dark"
         , onClick $ \_ _ state -> ([], Just state { tEditable = True, tName = table'  ^. tableName })
         ] $ faIcon_ "pencil"

       button_
         [ "className" $= "pure link-on-dark"
         , onClick $ \evt _ _ -> (stopPropagation evt : dispatch (TableDelete tableId), Nothing)
         ] $ faIcon_ "times"
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
        if keyENTER evt && not (Text.null curState)
           then (cb curState, Just "")
           else ([], Nothing)
    ]
