module Herculus.Project.TableList where

import Herculus.Prelude
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import DOM.Event.KeyboardEvent (code)
import Data.Map (Map)
import Data.String (length)
import Herculus.Monad (Herc)
import Herculus.Project.Data (TableDesc, descTable)
import Herculus.Utils (cldiv_, faIcon_)
import Lib.Api.Schema.Project (Command(..))
import Lib.Custom (Id)
import Lib.Model.Table (Table, tableName)

data Query a
  = SetNewTableName String a
  | CreateNew a
  | StartEdit (Id Table) a
  | SetNewName String a
  | SaveEdit a
  | GoTable (Id Table) a
  | DeleteTable (Id Table) a
  | CancelEdit a
  | Update Input a

type Input =
  { selected :: Maybe (Id Table)
  , tables :: Map (Id Table) TableDesc
  }

data Output
  = Command Command
  | SelectTable (Id Table)

type State =
  { input :: Input
  , newTableName :: String
  , editing :: Maybe (Id Table)
  , newName :: String
  }

comp :: H.Component HH.HTML Query Input Output Herc
comp = H.component
  { initialState:
    { input: _
    , newTableName: ""
    , editing: Nothing
    , newName: ""
    }
  , render
  , eval
  , receiver: Just <<< H.action <<< Update
  }

  where

  render :: State -> H.ComponentHTML Query
  render st = cldiv_ "table-list"
    (map renderTable (Map.toAscUnfoldable st.input.tables) <>
    [ HH.input
      [ HP.value st.newTableName
      , HP.class_ (H.ClassName "header-input")
      , HP.placeholder "Add table..."
      , HE.onValueInput (HE.input SetNewTableName)
      , HE.onKeyDown \e -> case code e of
          "Enter" -> Just (H.action CreateNew)
          _       -> Nothing
      ]
    ])

    where

    renderTable :: Tuple (Id Table) TableDesc -> H.ComponentHTML Query
    renderTable (Tuple i desc) = HH.div
      [ HE.onClick (HE.input_ $ GoTable i)
      , HP.classes
        [ H.ClassName "table-list__item left px2 py1 bold"
        , H.ClassName selected
        ]
      ]
      [ case Just i == st.editing of
          true -> HH.input
            [ HP.value st.newName
            , HP.autofocus true
            , HP.class_ (H.ClassName "header-input")
            , HE.onValueInput (HE.input SetNewName)
            , HE.onKeyDown \e -> case code e of
                "Enter"  -> Just (H.action SaveEdit)
                "Escape" -> Just (H.action CancelEdit)
                _        -> Nothing
            -- , HE.onBlur (HE.input_ CancelEdit)
            ]
          false -> HH.span_
            ([ HH.text (desc._descTable ^. tableName)
            ] <> actions)
      ]
      where
        actions = case Just i == st.input.selected of
          false -> []
          true ->
            [ HH.button
              [ HP.class_ (H.ClassName "button--pure button--on-dark table-list__item-action align-middle ml2")
              , HP.title "Change table name"
              , HE.onClick (HE.input_ $ StartEdit i)
              ]
              [ faIcon_ "pencil" ]
            , HH.button
              [ HP.class_ (H.ClassName "button--pure button--on-dark table-list__item-action align-middle ml1")
              , HP.title "Delete table (careful!)"
              , HE.onClick (HE.input_ $ DeleteTable i)
              ]
              [ faIcon_ "times" ]
            ]
        selected = case Just i == st.input.selected of
          true -> "table-list__item--selected"
          false -> ""

  eval :: Query ~> H.ComponentDSL State Query Output Herc
  eval (SetNewTableName name next) = do
    modify _{ newTableName = name }
    pure next

  eval (CreateNew next) = do
    { newTableName } <- H.get
    when (length newTableName > 0) $ do
      H.raise $ Command $ CmdTableCreate newTableName
      modify _{ newTableName = "" }
    pure next

  eval (StartEdit t next) = do
    tables <- H.gets _.input.tables
    case Map.lookup t tables of
      Nothing -> pure unit
      Just table ->
        modify _
          { editing = Just t
          , newName = table ^. descTable <<< tableName
          }
    pure next
  
  eval (SetNewName name next) = do
    modify _{ newName = name }
    pure next

  eval (SaveEdit next) = do
    { editing, newName } <- H.get
    case editing of
      Nothing -> pure unit
      Just t -> when (length newName > 0) do
        H.raise $ Command $ CmdTableSetName t newName
        modify _{ editing = Nothing }
    pure next

  eval (GoTable t next) = do
    H.raise $ SelectTable t
    pure next

  eval (DeleteTable t next) = do
    H.raise $ Command $ CmdTableDelete t
    pure next

  eval (CancelEdit next) = do
    modify _{ editing = Nothing }
    pure next

  eval (Update input next) = do
    modify _{ input = input }
    pure next
