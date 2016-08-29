module Views.Common where

import Control.Lens
import Data.Maybe (fromMaybe)

import React.Flux

data EditBoxProps = EditBoxProps
  { _ebpValue :: Text
  , _ebpOnSave :: (Text -> [SomeAction])
  }

data EditBoxState = EditBoxState
  { _ebsValue :: Maybe Text
  , _ebsIsEditing :: Bool
  }

emptyEditBox :: EditBoxState
emptyEditBox = EditBoxState Nothing False

editBox_ :: Text -> EditBoxProps -> ReactElementM eh ()
editBox_ name props = view (column name) props mempty

editBox :: ReactView EditBoxProps
editBox name = defineStatefulView name emptyEditBox $ \state props -> do
  let value = state ^. ebsValue :? props ^. ebpValue
  case st ^. ebsIsEditing of
    True  -> input_
      [ "placeholder" &= name
      , "value" &= value
      , classnames [("editing", state ^. ebsisediting)]
      , onchange $ \evt st -> ([], just $ st & ebsvalue ~. target evt "value")
      , onblur $ \evt _ -> ([], just state)
      ]
    False -> div_
      [ onFocus $ \evt st -> ([], Just $ st & ebsIsEditing ~. True)
      ] $

(:?) :: Maybe a -> a -> a
(:?) = flip fromMaybe
