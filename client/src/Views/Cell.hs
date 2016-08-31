module Views.Cell
  ( cell_
  , CellProps (..)
  ) where

import Control.Monad (when)
import Control.Lens hiding (view)

import Data.Maybe
import Data.Monoid
import Data.Text (Text, unpack, pack, intercalate)
import qualified Data.Map.Strict as Map
import Data.Foldable

import Text.Read (readMaybe)

import React.Flux

import Lib.Model.Column
import Lib.Model.Cell
import Lib.Model.Types
import Lib.Types

import Store
import Views.Foreign
import Views.Common

type CellCallback a = a -> [SomeStoreAction]

data CellProps = CellProps
  { cellColId   :: !(Id Column)
  , cellRecId   :: !(Id Record)
  , cellColumn  :: !Column
  , cellContent :: !CellContent
  }

cell_ :: CellProps -> ReactElementM eh ()
cell_ !props = view cell props mempty

cell :: ReactView CellProps
cell = defineStatefulView "cell" False $ \open CellProps{..} ->
  case cellContent of
    CellError msg ->
      elemText $ "Error: " <> msg
    CellValue val ->
      value_ (columnInputType cellColumn)
             (columnDataType cellColumn)
             val
             (\v -> dispatch $ CellSetValue cellColId cellRecId v)

--

value_ :: InputType -> DataType -> Value -> CellCallback Value
       -> ReactElementM eh ()
value_ !inpType !datType !val !cb = view value (inpType, datType, val, cb) mempty

value :: ReactView (InputType, DataType, Value, CellCallback Value)
value = defineView "value" $ \(inpType, datType, val, cb) -> case datType of
  DataBool -> case val of
    VBool b -> cellBool_ inpType b (cb . VBool)
    _ -> mempty
  DataString -> case val of
    VString s -> cellString_ inpType s (cb . VString)
    _ -> mempty
  DataNumber -> case val of
    VNumber n -> cellNumber_ inpType n (cb . VNumber)
    _ -> mempty
  DataTime -> case val of
    VTime t -> cellTime_ inpType t (cb . VTime)
    _ -> mempty
  DataRecord t -> case val of
    VRecord mr -> cellRecord_ inpType mr t (cb . VRecord)
    _ -> mempty
  DataList t -> case val of
    VList vs -> cellList_ inpType t vs (cb . VList)
    _ -> mempty
  DataMaybe t -> case val of
    VMaybe v -> cellMaybe_ inpType t v (cb . VMaybe)
    _ -> mempty

cellBool_ :: InputType -> Bool -> CellCallback Bool
          -> ReactElementM ViewEventHandler ()
cellBool_ !inpType !b !cb = view cellBool (inpType, b, cb) mempty

cellBool :: ReactView (InputType, Bool, CellCallback Bool)
cellBool = defineView "cellBool" $ \(inpType, b, cb) -> case inpType of
  ColumnInput ->
    input_
      [ "type" $= "checkbox"
      , "checked" @= b
      , onChange $ \_ -> cb (not b)
      ]
  ColumnDerived ->
    elemText $ if b then "True" else "False"

cellString_ :: InputType -> Text -> CellCallback Text
            -> ReactElementM ViewEventHandler ()
cellString_ !inpType !s !cb = view cellString (inpType, s, cb) mempty

cellString :: ReactView (InputType, Text, CellCallback Text)
cellString = defineView "cellString" $ \(inpType, s, cb) -> case inpType of
  ColumnInput ->
    editBox_ EditBoxProps
      { editBoxValue = s
      , editBoxPlaceholder = ""
      , editBoxClassName = "string"
      , editBoxShow = id
      , editBoxValidator = Just
      , editBoxOnSave = cb
      }
  ColumnDerived ->
    elemText s

cellNumber_ :: InputType -> Number -> CellCallback Number
            -> ReactElementM ViewEventHandler ()
cellNumber_ !inpType !n !cb = view cellNumber (inpType, n, cb) mempty

cellNumber :: ReactView (InputType, Number, CellCallback Number)
cellNumber = defineView "cellNumber" $ \(inpType, n, cb) ->
  case inpType of
    ColumnInput ->
      editBox_ EditBoxProps
        { editBoxValue = n
        , editBoxPlaceholder = "0"
        , editBoxClassName = "number"
        , editBoxShow = pack . show
        , editBoxValidator = \s -> Number <$> (readMaybe $ unpack s)
        , editBoxOnSave = cb
        }
    ColumnDerived ->
      elemString $ show n

cellTime_ :: InputType -> Time -> CellCallback Time
          -> ReactElementM ViewEventHandler ()
cellTime_ !inpType !t !cb = view cellTime (inpType, t, cb) mempty

cellTime :: ReactView (InputType, Time, CellCallback Time)
cellTime = defineStatefulView "cellTime" Nothing $ \invalidTmp (inpType, t, cb) -> case inpType of
  ColumnInput -> do
    let curT = fromMaybe (formatTime "%F" t) invalidTmp
        handleChange :: Text -> StatefulViewEventHandler (Maybe Text)
        handleChange x _ = case parseTime "%F" x of
          Nothing -> ([], Just $ Just x)
          Just t' -> (cb t', Just Nothing)
    datePicker_ $ DatePickerProps
      { datePickerSelected = curT
      , datePickerPlaceholderText = "Please select a day"
      , datePickerDateFormat = "YYYY-MM-DD"
      , datePickerOnChange = handleChange
      , datePickerClassNames = [("invalid", isJust invalidTmp)]
      }
  ColumnDerived ->
    elemString $ show t

cellRecord_ :: InputType -> Maybe (Id Record) -> Id Table -> CellCallback (Maybe (Id Record))
            -> ReactElementM ViewEventHandler ()
cellRecord_ !inpType !mr !t !cb = view cellRecord (inpType, mr, t, cb) mempty

cellRecord :: ReactView (InputType, Maybe (Id Record), Id Table, CellCallback (Maybe (Id Record)))
cellRecord = defineControllerView "cellRecord" store $ \st (inpType, mr, t, cb) -> do
  onDidMount_ (dispatch $ CacheRecordsGet t) mempty
  let records = fromMaybe Map.empty $ st ^. stateCacheRecords . at t
      showPairs = intercalate ", " . map (\(n, v) -> n <> ": " <> (pack . show) v) . Map.elems
  case inpType of
    ColumnInput ->
      select_
        [ "defaultValue" &= case mr of
            Nothing -> ""
            Just r  -> show r
        , classNames [("invalid", isNothing mr)]
        , onChange $ \evt ->
            let val = target evt "value"
            in if val == ""
                  then cb Nothing
                  else case readMaybe val of
                         Nothing -> []
                         Just newId -> cb $ Just newId
        ] $ do when (isNothing mr) $ option_
                 [ "value" $= ""
                 ] $ ""
               for_ (Map.toList records) $ \(i, record) -> option_
                 [ "value" &= show i
                 ] $ elemText $ showPairs record
    ColumnDerived -> case mr of
      Nothing -> "impossible: invalid record in derived cell"
      Just r -> case Map.lookup r records of
        Nothing -> mempty
        Just fields ->
          ul_ $ for_ fields $ \(k, content) -> li_ $ do
            elemText $ k <> ": " <> (pack . show) content


cellList_ :: InputType -> DataType -> [Value] -> CellCallback [Value]
          -> ReactElementM ViewEventHandler ()
cellList_ !inpType !datType !vs !cb = view cellList (inpType, datType, vs, cb) mempty

cellList :: ReactView (InputType, DataType, [Value], CellCallback [Value])
cellList = defineView "cellList" $ \(inpType, datType, vs, cb) -> case inpType of
  ColumnInput -> do
    button_
      [ onClick $ \_ _ ->
          let CellValue newV = defaultContentPure datType
          in  cb (newV : vs)
      ] "New"
    ul_ $ for_ (zip [0..] vs) $ \(i, v) -> li_ $ do
      let listMod ind x xs = let (h, t) = splitAt ind xs in h <> (x : drop 1 t)
          listDel ind xs   = let (h, t) = splitAt ind xs in h <> drop 1 t
      value_ inpType datType v (\nv -> cb (listMod i nv vs))
      button_
        [ onClick $ \_ _ -> cb (listDel i vs)
        ] "Del"
  ColumnDerived ->
    ul_ $ for_ vs $ \v -> li_ $ value_ inpType datType v (const [])

cellMaybe_ :: InputType -> DataType -> Maybe Value -> CellCallback (Maybe Value)
           -> ReactElementM ViewEventHandler ()
cellMaybe_ !inpType !datType !mVal !cb = view cellMaybe (inpType, datType, mVal, cb) mempty

cellMaybe :: ReactView (InputType, DataType, Maybe Value, CellCallback (Maybe Value))
cellMaybe = defineView "cellMaybe" $ \(inpType, datType, mVal, cb) -> case inpType of
  ColumnInput -> case mVal of
    Nothing -> do
      let CellValue new = defaultContentPure datType
      button_
        [ onClick $ \_ _ -> cb (Just new)
        ] "Add"
    Just val -> do
      button_
        [ onClick $ \_ _ -> cb Nothing
        ] "Del"
      value_ inpType datType val (cb . Just)
  ColumnDerived -> case mVal of
    Nothing -> "Nothing"
    Just val -> value_ inpType datType val (cb . Just)
