{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Views.ProjectOverview where

import           Control.DeepSeq     (NFData)
import           Control.Lens        ((^.))
import           Data.Foldable       (for_)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics        (Generic)
import           React.Flux          (ReactElementM, ReactView, button_,
                                      classNames, cldiv_, defineStatefulView,
                                      defineView, div_, elemText, faIcon_, h1_,
                                      input_, li_, onChange, onClick, onKeyDown,
                                      p_, span_, stopPropagation, target, ul_,
                                      view, viewWithSKey, ($=), (&=))
import           React.Flux.Internal (toJSString)

import           Action              (Action (ProjectDelete, ProjectSetName, ProjectsCreate, ProjectsLoadProject))
import           Helper              (keyENTER, keyESC)
import           Lib.Model.Project   (Project, projectName)
import           Lib.Types           (Id)
import           Store               (dispatch)
import           Views.Combinators   (inputNew_)

data TileState = TileState
  { tsEditable  :: Bool
  , tsName      :: Text
  , tsNameError :: Bool
  } deriving (Generic, Show, NFData)

initialTileState :: TileState
initialTileState = TileState False "" False

projectsOverview_ :: Map (Id Project) Project -> ReactElementM eh ()
projectsOverview_ !ps = view projectsOverview ps mempty

projectsOverview :: ReactView (Map (Id Project) Project)
projectsOverview = defineView "projects" $ \ps -> cldiv_ "projects" $ do
  h1_ "My projects"
  div_
    [ classNames [ ("tile", True)
                 , ("new" , True)
                 ]
    ] $ p_ $ do
      "Create new ..."
      inputNew_ "Project name" (dispatch . ProjectsCreate)
  for_ (Map.toList ps) $ uncurry projectTile_

projectTile_ :: Id Project -> Project -> ReactElementM eh ()
projectTile_ !projectId !project = viewWithSKey projectTile (toJSString $ show projectId) (projectId, project) mempty

projectTile :: ReactView (Id Project, Project)
projectTile = defineStatefulView "project" initialTileState $
    \st@TileState{..} (projectId, project) ->
      if tsEditable
        then
          div_
            [ classNames [ ("tile", True) ]
            ] $ input_
              [ "value" &= tsName
              , onChange $ \evt st ->
                let value = target evt "value"
                in  ([], Just st { tsName = value, tsNameError = Text.null value})
              , onKeyDown inputKeyDownHandler
              ]
        else
          div_
            [ classNames [ ("tile", True) ]
            , onClick $ \_ _ _ ->
                (dispatch $ ProjectsLoadProject projectId, Nothing)
            ] $ do
              span_ $ elemText $ project ^. projectName
              button_
                [ "className" $= "pure"
                , onClick $ \ev _ st ->
                    ([stopPropagation ev],
                     Just st { tsEditable = True
                             , tsName = project ^. projectName
                             }
                    )
                ] $ faIcon_ "pencil"
              button_
                [ "className" $= "pure"
                , onClick $ \ev _ _ ->
                    (stopPropagation ev : dispatch (ProjectDelete projectId), Nothing)
                ] $ faIcon_ "times"
  where
    -- saveHandler :: TileState -> ([SomeStoreAction], Maybe TileState)
    saveHandler st@TileState{..} =
      (dispatch $ ProjectSetName tsName, Just st { tsEditable = False })

    inputKeyDownHandler _ evt st@TileState{..}
      | keyENTER evt && not (Text.null tsName) = saveHandler st
      | keyESC evt = ([] , Just st { tsEditable = False })
      | otherwise = ([], Just st)

--      then div_ $ input_
--             [ classNames
--               [ ("inp", True)
--               , ("inp-error", pNameError state)
--               ]
--             , "value" &= pName state
--             , onChange $ \evt st ->
--                 let value = target evt "value"
--                 in ([], Just st { pName = value, pNameError = Text.null value})
--             , onKeyDown inputKeyDownHandler
--             ]
--      else div_ $ do
--        span_ $ elemText $ project' ^. projectName
--        button_
--          [ "className" $= "pure link-on-dark"
--          , onClick $ \ev _ st -> ([stopPropagation ev], Just st { pEditable = True, pName = project' ^. projectName})
--          ] $ faIcon_ "pencil"
--
