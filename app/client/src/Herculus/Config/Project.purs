module Herculus.Config.Project where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Herculus.Ace as Ace
import Herculus.Config.Common.Header as Header
import Herculus.Modal as Modal
import Herculus.Router as R
import Control.Monad.Eff.Ref (Ref)
import Halogen.Component.ChildPath (type (<\/>), type (\/), cp1, cp2, cp3)
import Herculus.Config.Common (errorList, section, withDelay)
import Herculus.Monad (Herc, gotoRoute, withApi)
import Herculus.Utils (cldiv_)
import Lib.Api.Rest (deleteProjectDeleteByProjectId, postProjectLintModule, postProjectRunCommandsByProjectId)
import Lib.Api.Schema.Project (Command(CmdProjectSetModule, CmdProjectSetName), Project, projectId, projectModuleSource, projectName)
import Lib.Compiler.Error (Error)

data Query a
  = Initialize a
  | Update Input a
  | Reset a
  | Save a
  | SetName String a
  | SetModuleSrc String a
  | Close' a
  | Delete a
  | ReallyDelete a

type Input =
  { project :: Project
  }

type State =
  { input :: Input
  , tmpModuleSrc :: Maybe String
  , errors :: Array Error
  , delayRef :: Maybe (Ref Boolean)
  }

type Child =
  Ace.Query <\/>
  Header.Query <\/>
  Modal.Query Boolean <\/>
  Const Void

type SlotPath = Array Int

type Slot =
  Unit \/
  Unit \/
  Unit \/
  Unit

data Output
  = Close

getModuleSource :: State -> String
getModuleSource st =
  fromMaybe (st.input.project ^. projectModuleSource) st.tmpModuleSrc

deleteConfirmInput :: Modal.Input Boolean
deleteConfirmInput =
  { title: "Delete Project"
  , text: "Do you really want to delete this project? This cannot be undone \
          \at the moment."
  , actions:
    [ { icon: "close red", label: "Cancel", value: false }
    , { icon: "check green", label: "Delete", value: true }
    ]
  }

--------------------------------------------------------------------------------

comp :: H.Component HH.HTML Query Input Output Herc
comp = H.lifecycleParentComponent
  { initialState:
    { input: _
    , tmpModuleSrc: Nothing
    , errors: []
    , delayRef: Nothing
    }
  , render
  , eval
  , receiver: Just <<< H.action <<< Update
  , initializer: Just $ H.action Initialize
  , finalizer: Nothing
  }

render :: State -> H.ParentHTML Query Child Slot Herc
render st = HH.div_
  [ HH.slot' cp2 unit Header.comp
      { name: st.input.project ^. projectName
      , unsaved: isJust st.tmpModuleSrc
      , subTitle: HH.text "Project"
      }
      \o -> Just $ H.action case o of
        Header.SetName n -> SetName n
        Header.Close -> Close'
        Header.Delete -> Delete
        Header.Reset -> Reset
        Header.Save -> Save
  , let
      head = cldiv_ "bold"
        [ HH.text "Project Definitions" ]
      body = cldiv_ ""
        [ cldiv_ "my1 column-config__editor"
          [ HH.slot' cp1 unit Ace.comp
                     { mode: "ace/mode/haskell"
                     }
                     \(Ace.TextChanged f) -> Just $ H.action $ SetModuleSrc f
          ]
        , errorList st.errors
        ]
    in
      section "code" head body
  , HH.slot' cp3 unit Modal.comp deleteConfirmInput $ case _ of
      true  -> Just $ H.action ReallyDelete
      false -> Nothing
  ]

eval :: Query ~> H.ParentDSL State Query Child Slot Output Herc
eval = case _ of

  Initialize next -> do
    updateAce
    pure next
    
  Update input next -> do
    modify _ { input = input }
    updateAce
    pure next

  SetName name next -> do
    st <- get
    let
      call = postProjectRunCommandsByProjectId
               [CmdProjectSetName name]
               (st.input.project ^. projectId)
    withApi call $ const $ pure unit
    pure next

  SetModuleSrc src next -> do
    modify _{ tmpModuleSrc = Just src }
    withDelay 700 lint
    pure next

  Reset next -> do
    modify _{ tmpModuleSrc = Nothing }
    updateAce
    lint
    pure next

  Close' next -> do
    H.raise Close
    pure next

  Delete next -> do
    _ <- H.query' cp3 unit $ H.action Modal.Open
    pure next

  ReallyDelete next -> do
    projId <- gets \st -> st.input.project ^. projectId
    withApi (deleteProjectDeleteByProjectId projId) \_ ->
      gotoRoute $ R.LoggedIn R.ProjectOverview
    pure next

  Save next -> do
    st <- get
    let
      call = postProjectRunCommandsByProjectId
               [CmdProjectSetModule $ getModuleSource st]
               (st.input.project ^. projectId)
    withApi call \_ ->
      modify _{ tmpModuleSrc = Nothing }
    pure next

updateAce :: H.ParentDSL State Query Child Slot Output Herc Unit
updateAce = do
  st <- get
  _ <- H.query' cp1 unit $ H.action $ Ace.SetText $ getModuleSource st
  pure unit

lint :: H.ParentDSL State Query Child Slot Output Herc Unit
lint = do
  st <- get
  let
    call = postProjectLintModule $ getModuleSource st
  withApi call \errs -> do
    _ <- H.query' cp1 unit $ H.action $ Ace.SetAnnotations errs
    modify _{ errors = errs }
    pure unit

