{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Views.Cell
  ( dataCell_
  , reportCell_
  , DataCellProps (..)
  , ReportCellProps (..)
  ) where

import           Control.DeepSeq
import           Control.Lens      hiding (view)
import           Control.Monad     (when)

import           Data.Foldable
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text         (Text, intercalate, pack, unpack)

import           GHC.Generics

import           Text.Read         (readMaybe)

import           React.Flux

import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Record
import           Lib.Model.Table
import           Lib.Types

import           Store
import           Views.Combinators
import           Views.Common
import           Views.Foreign

-- data cell

type CellCallback a = a -> [SomeStoreAction]

data DataCellProps = DataCellProps
  { dataCellColId   :: !(Id Column)
  , dataCellRecId   :: !(Id Record)
  , dataCellColData :: !DataCol
  , dataCellContent :: !CellContent
  }

data CellState = CellState (Map (Id Column, Id Record) Bool)

data CellAction = Toggle (Id Column, Id Record)
  deriving (Generic, NFData)

cellStore :: ReactStore CellState
cellStore = mkStore (CellState Map.empty)

instance StoreData CellState where
  type StoreAction CellState = CellAction
  transform (Toggle (c, r)) (CellState m) = pure $
    CellState $ m & at (c, r) . non False %~ not

dataCell_ :: DataCellProps -> ReactElementM eh ()
dataCell_ !props = view dataCell props mempty

dataCell :: ReactView DataCellProps
dataCell = defineControllerView "cell" cellStore $ \(CellState m) DataCellProps{..} ->
  case dataCellContent of
    CellError _ ->
      clspan_ "error" "Error" -- TODO: what to do with the error message here?
                              --       putting it in every row seems unnecessary
    CellValue val ->
      let open = fromMaybe False $ Map.lookup (dataCellColId, dataCellRecId) m
          inpType = dataCellColData ^. dataColIsDerived
          datType = dataCellColData ^. dataColType
          needsEx = needsExpand (datType, inpType)
          inline = value_ (if needsEx then Compact else Full)
                          inpType
                          datType
                          val
                          (dispatch . CellSetValue dataCellColId dataCellRecId)
      in
      case needsEx of
        True -> cldiv_ "compactWrapper" $ do
          cldiv_ "compact" inline
          cldiv_ "expand" $ do
            let buttonCls = "expand" <> if open then " open" else ""
            faButton_ buttonCls [SomeStoreAction cellStore $ Toggle (dataCellColId, dataCellRecId)]
            when open $ cldiv_ "expanded" $ cldiv_ "body" $
              value_ Full
                     inpType
                     datType
                     val
                     (dispatch . CellSetValue dataCellColId dataCellRecId)
        False -> inline

needsExpand :: (DataType, IsDerived) -> Bool
needsExpand (dt, it) = case (dt, it) of
  (DataBool,     _)             -> False
  (DataString,   _)             -> False
  (DataNumber,   _)             -> False
  (DataTime,     _)             -> False
  (DataRecord _, NotDerived)   -> False
  (DataRecord _, Derived) -> True
  (DataList _,   _)             -> True
  (DataMaybe sub,  _)           -> needsExpand (sub, it)

data Mode
  = Compact
  | Full

--

value_ :: Mode -> IsDerived -> DataType -> Value -> CellCallback Value
       -> ReactElementM eh ()
value_ !mode !inpType !datType !val !cb =
  view value (mode, inpType, datType, val, cb) mempty

value :: ReactView (Mode, IsDerived, DataType, Value, CellCallback Value)
value = defineView "value" $ \(mode, inpType, datType, val, cb) -> case datType of
  DataBool -> case val of
    VBool b -> cellBool_ mode inpType b (cb . VBool)
    _ -> mempty
  DataString -> case val of
    VString s -> cellString_ mode inpType s (cb . VString)
    _ -> mempty
  DataNumber -> case val of
    VNumber n -> cellNumber_ mode inpType n (cb . VNumber)
    _ -> mempty
  DataTime -> case val of
    VTime t -> cellTime_ mode inpType t (cb . VTime)
    _ -> mempty
  DataRecord t -> case val of
    VRecord mr -> cellRecord_ mode inpType mr t (cb . VRecord)
    _ -> mempty
  DataList t -> case val of
    VList vs -> cellList_ mode inpType t vs (cb . VList)
    _ -> mempty
  DataMaybe t -> case val of
    VMaybe v -> cellMaybe_ mode inpType t v (cb . VMaybe)
    _ -> mempty

cellBool_ :: Mode -> IsDerived -> Bool -> CellCallback Bool
          -> ReactElementM ViewEventHandler ()
cellBool_ !mode !inpType !b !cb =
  view cellBool (mode, inpType, b, cb) mempty

cellBool :: ReactView (Mode, IsDerived, Bool, CellCallback Bool)
cellBool = defineView "cellBool" $ \(mode, inpType, b, cb) ->
  case (mode, inpType) of
    (Full, NotDerived) ->
      input_
        [ "type" $= "checkbox"
        , "checked" @= b
        , onChange $ \_ -> cb (not b)
        ]
    _ ->
      elemText $ if b then "True" else "False"

cellString_ :: Mode -> IsDerived -> Text -> CellCallback Text
            -> ReactElementM ViewEventHandler ()
cellString_ !mode !inpType !s !cb =
  view cellString (mode, inpType, s, cb) mempty

cellString :: ReactView (Mode, IsDerived, Text, CellCallback Text)
cellString = defineView "cellString" $ \(mode, inpType, s, cb) ->
  case (mode, inpType) of
    (Full, NotDerived) ->
      editBox_ EditBoxProps
        { editBoxValue = s
        , editBoxPlaceholder = ""
        , editBoxClassName = "string"
        , editBoxShow = id
        , editBoxValidator = Just
        , editBoxOnSave = cb
        }
    _ ->
      elemText s

cellNumber_ :: Mode -> IsDerived -> Number -> CellCallback Number
            -> ReactElementM ViewEventHandler ()
cellNumber_ !mode !inpType !n !cb =
  view cellNumber (mode, inpType, n, cb) mempty

cellNumber :: ReactView (Mode, IsDerived, Number, CellCallback Number)
cellNumber = defineView "cellNumber" $ \(mode, inpType, n, cb) ->
  case (mode, inpType) of
    (Full, NotDerived) ->
      editBox_ EditBoxProps
        { editBoxValue = n
        , editBoxPlaceholder = "0"
        , editBoxClassName = "number"
        , editBoxShow = pack . show
        , editBoxValidator = \s -> Number <$> (readMaybe $ unpack s)
        , editBoxOnSave = cb
        }
    _ ->
      cldiv_ "number" $ elemString $ show n

cellTime_ :: Mode -> IsDerived -> Time -> CellCallback Time
          -> ReactElementM ViewEventHandler ()
cellTime_ !mode !inpType !t !cb =
  view cellTime (mode, inpType, t, cb) mempty

cellTime :: ReactView (Mode, IsDerived, Time, CellCallback Time)
cellTime = defineStatefulView "cellTime" Nothing $
  \invalidTmp (mode, inpType, t, cb) -> case (mode, inpType) of
    (Full, NotDerived) -> do
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
    _ ->
      elemShow t

cellRecord_ :: Mode -> IsDerived -> Maybe (Id Record) -> Id Table
            -> CellCallback (Maybe (Id Record))
            -> ReactElementM ViewEventHandler ()
cellRecord_ !mode !inpType !mr !t !cb =
  view cellRecord (mode, inpType, mr, t, cb) mempty

cellRecord :: ReactView ( Mode, IsDerived, Maybe (Id Record), Id Table
                        , CellCallback (Maybe (Id Record))
                        )
cellRecord = defineControllerView "cellRecord" store $
  \st (mode, inpType, mr, t, cb) -> cldiv_ "record" $ do
    onDidMount_ (dispatch $ CacheRecordsGet t) mempty
    let records = fromMaybe Map.empty $ st ^. stateCacheRecords . at t
        showPairs = intercalate ", " .
                    map (\(c, v) -> (c ^. columnName) <> ": " <> (pack . show) v) .
                    Map.elems
    case mode of
      Compact -> case mr of
        Nothing -> "impossible: invalid record in derived cell"
        Just r -> case Map.lookup r records of
          Nothing -> mempty
          Just fields -> elemText $ showPairs fields
      Full -> case inpType of
        NotDerived ->
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
                     ] ""
                   for_ (Map.toList records) $ \(i, record) -> option_
                     [ "value" &= show i
                     ] $ elemText $ showPairs record
        Derived -> case mr of
          Nothing -> "Impossible: invalid record in derived cell"
          Just r -> case Map.lookup r records of
            Nothing -> mempty
            Just fields ->
              for_ fields $ \(c, content) -> cldiv_ "field" $ do
                cldiv_ "key" $ elemText $ c ^. columnName
                cldiv_ "content" $ case (content, c ^. columnKind) of
                  (CellValue v, ColumnData dat) -> value_ mode
                                                          inpType
                                                          (dat ^. dataColType)
                                                          v
                                                          (const [])
                  (CellError _, _)              -> clspan_ "error" "Error"
                  _                             -> mempty


cellList_ :: Mode -> IsDerived -> DataType -> [Value] -> CellCallback [Value]
          -> ReactElementM ViewEventHandler ()
cellList_ !mode !inpType !datType !vs !cb =
  view cellList (mode, inpType, datType, vs, cb) mempty

cellList :: ReactView (Mode, IsDerived, DataType, [Value], CellCallback [Value])
cellList = defineView "cellList" $ \(mode, inpType, datType, vs, cb) -> cldiv_ "list" $
  case mode of
    Compact -> do
      clspan_ "info" $ elemShow $ length vs
      for_ (take 4 vs) $ \v -> cldiv_ "element" $
        value_ mode inpType datType v (const [])
    Full -> case inpType of
      NotDerived -> do
        for_ (zip [0..] vs) $ \(i, v) -> cldiv_ "element-wrapper" $ cldiv_ "element input" $ do
          let listMod ind x xs = let (h, t) = splitAt ind xs in h <> (x : drop 1 t)
              listDel ind xs   = let (h, t) = splitAt ind xs in h <> drop 1 t
          cldiv_ "delete" $ button_
            [ onClick $ \_ _ -> cb (listDel i vs)
            ] $ faIcon_ "minus-circle"
          cldiv_ "content" $ value_ mode inpType datType v (\nv -> cb (listMod i nv vs))
        cldiv_ "new" $ button_
          [ onClick $ \_ _ ->
              let CellValue newV = defaultContentPure datType
              in  cb (vs <> [newV])
          ] $ faIcon_ "plus-circle"
      Derived ->
        for_ vs $ \v -> cldiv_ "element-wrapper" $ cldiv_ "element" $ cldiv_ "content" $
          value_ mode inpType datType v (const [])

cellMaybe_ :: Mode -> IsDerived -> DataType -> Maybe Value
           -> CellCallback (Maybe Value)
           -> ReactElementM ViewEventHandler ()
cellMaybe_ !mode !inpType !datType !mVal !cb =
  view cellMaybe (mode, inpType, datType, mVal, cb) mempty

cellMaybe :: ReactView (Mode, IsDerived, DataType, Maybe Value, CellCallback (Maybe Value))
cellMaybe = defineView "cellMaybe" $ \(mode, inpType, datType, mVal, cb) ->
  case (mode, inpType) of
    (Full, NotDerived) -> case mVal of
      Nothing -> cldiv_ "maybe nothing" $ do
        let CellValue new = defaultContentPure datType
        cldiv_ "button" $ button_
          [ onClick $ \_ _ -> cb $ Just new
          ] $ faIcon_ "plus-circle"
        cldiv_ "content" "Nothing"
      Just val -> cldiv_ "maybe just" $ do
        cldiv_ "button" $ button_
          [ onClick $ \_ _ -> cb Nothing
          ] $ faIcon_ "minus-circle"
        cldiv_ "content" $
          value_ mode inpType datType val (cb . Just)
    _ -> case mVal of
      Nothing -> "Nothing"
      Just val -> value_ mode inpType datType val (cb . Just)

 -- report cell

data ReportCellProps = ReportCellProps
  { reportCellColId :: !(Id Column)
  , reportCellColReport :: !ReportCol
  }

reportCell_ :: ReportCellProps -> ReactElementM eh ()
reportCell_ !p = view reportCell p mempty

reportCell :: ReactView ReportCellProps
reportCell = defineView "reportCell" $ \props -> mempty
