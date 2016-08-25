module Views.Column where

import           React.Flux

import           Control.Monad    (forM_)
import qualified Data.Map         as Map
import           Data.Map         (Map)
import           Data.Maybe       (fromMaybe)
import           Data.Text        (Text)
import qualified Data.Text        as Text

import           Lib.Model
import           Lib.Model.Column
import           Lib.Types

import           Store

type SelDtCallback     = DataType -> StatefulViewEventHandler (Maybe DataType)
type SelDtEventHandler = StatefulViewEventHandler (Maybe DataType)

data Branch
  = BBool
  | BString
  | BNumber
  | BTime
  | BRecord
  | BList
  | BMaybe
  deriving (Eq, Ord)

toBranch :: DataType -> Branch
toBranch = \case
  DataBool     -> BBool
  DataString   -> BString
  DataNumber   -> BNumber
  DataTime     -> BTime
  DataRecord _ -> BRecord
  DataList   _ -> BList
  DataMaybe  _ -> BMaybe

branches :: Map Text Branch
branches = [ ("Bool"  , BBool  )
           , ("String", BString)
           , ("Number", BNumber)
           , ("Time"  , BTime  )
           , ("Record", BRecord)
           , ("List"  , BList  )
           , ("Maybe" , BMaybe )
           ]

column_ :: Entity Column -> ReactElementM eh ()
column_ !c = view column c mempty

column :: ReactView (Entity Column)
column = defineView "column" $ \c -> do
  columnTitle_ c
  selectDatatype_ c

columnTitle_ :: Entity Column -> ReactElementM eh ()
columnTitle_ !c = view columnTitle c mempty

columnTitle :: ReactView (Entity Column)
columnTitle = defineStatefulView "columnTitle" Nothing $ \curText (Entity i col) -> do
  input_
    [ "placeholder" &= ("column name" :: Text)
    , "value" &= fromMaybe (columnName col) curText
    , onChange $ \evt _ -> ([], Just $ Just $ target evt "value")
    , onKeyDown $ \_ evt curState ->
        let txt = fromMaybe "" curState
        in  if keyCode evt == 13 && not (Text.null txt) -- 13 = Enter
              then (dispatch $ ColumnRename i txt, Just curState)
              else ([], Nothing)
    ]
  button_
    [ onClick $ \_ _ _ -> (dispatch $ TableDeleteColumn i, Nothing)
    ] $ "-"
  br_ []
  span_ "Data type "

selectDatatype_ :: Entity Column -> ReactElementM eh ()
selectDatatype_  c = view selectDatatype c mempty

selectDatatype :: ReactView (Entity Column)
selectDatatype = defineStatefulView "selectDataType" Nothing $ \curDt c -> do
  dataType_ c (\dt -> (dispatch $ ColumnSetDataType i dt, Nothing))

dataType_ :: Entity Column -> SelDtCallback -> ReactElementM SelDtEventHandler ()
dataType_ !(Entity i _) !cb =
  select_
    [ onChange $ \evt curState -> case Map.lookup (target evt "value") branches of
        Just BBool   -> cb DataBool
        Just BString -> cb DataString
        Just BNumber -> cb DataNumber
        Just BTime   -> cb DataTime
        Just BMaybe  ->
    ] $ forM_ (Map.toList branches) $ \(label, branch) -> do
          option_ $ elemText label
