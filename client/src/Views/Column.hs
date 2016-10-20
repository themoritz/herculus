{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Views.Column where

import           Control.Applicative ((<|>))
import           Control.DeepSeq     (NFData)
import           Control.Lens        hiding (view)
import           Control.Monad       (join, when)
import           Data.Foldable       (for_)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe, isJust)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           React.Flux
import           Text.Read           (readMaybe)

import           Lib.Model
import           Lib.Model.Auth      (SessionKey)
import           Lib.Model.Column
import           Lib.Model.Table
import           Lib.Types

import           Action              (Action (ColumnAction, TableDeleteColumn, GetTableCache),
                                      TableCache)
import           Store               (dispatch, stateColumns, stateTableCache,
                                      store)
import           Views.Combinators
import           Views.Common        (EditBoxProps (..), editBox_)
import           Views.Foreign

import           Action.Column

-- state of column controller view

--

-- branch selection types

--

type SelBranchCallback = DataType -> [SomeStoreAction]
type SelTableCallback  = Id Table -> [SomeStoreAction]
type SelDtEventHandler = StatefulViewEventHandler (Maybe Branch)

data Branch
  = BBool
  | BString
  | BNumber
  | BTime
  | BRecord
  | BList
  | BMaybe
  deriving (Eq, Ord, Generic, NFData, Read, Show)

toBranch :: DataType -> Branch
toBranch = \case
  DataBool     -> BBool
  DataString   -> BString
  DataNumber   -> BNumber
  DataTime     -> BTime
  DataRecord _ -> BRecord
  DataList   _ -> BList
  DataMaybe  _ -> BMaybe

branches :: Map Branch Text
branches = Map.fromList
  [ (BBool  , "Bool"  )
  , (BString, "String")
  , (BNumber, "Number")
  , (BTime  , "Time"  )
  , (BRecord, "Row"   )
  , (BList  , "List"  )
  , (BMaybe , "Maybe" )
  ]

reportLangs :: Map (Maybe ReportLanguage) Text
reportLangs = Map.fromList
  [ (Nothing                    , "Plaintext" )
  , (Just ReportLanguageMarkdown, "Markdown"  )
  , (Just ReportLanguageLatex   , "Latex"     )
  , (Just ReportLanguageHTML    , "HTML"      )
  ]

reportFormats :: Map ReportFormat Text
reportFormats = Map.fromList
  [ (ReportFormatPlain , "Plaintext")
  , (ReportFormatPDF   , "PDF"      )
  , (ReportFormatHTML  , "HTML"     )
  -- , ("Markdown" , ReportFormatMarkdown )
  ]

subType :: DataType -> Maybe DataType
subType (DataList  a) = Just a
subType (DataMaybe a) = Just a
subType _             = Nothing

recordTableId :: DataType -> Maybe (Id Table)
recordTableId (DataRecord tId) = Just tId
recordTableId _                = Nothing

--

-- views

--

column_ :: Maybe SessionKey -> Entity Column -> ReactElementM eh ()
column_ !sKey !c = view column (sKey, c) mempty

column :: ReactView (Maybe SessionKey, Entity Column)
column = defineView "column" $ \(sKey, c@(Entity i col)) -> cldiv_ "column" $ do
  cldiv_ "head" $ do
    editBox_ EditBoxProps
      { editBoxValue       = col ^. columnName
      , editBoxPlaceholder = "Name..."
      , editBoxClassName   = "columnName"
      , editBoxShow        = id
      , editBoxValidator   = Just
      , editBoxOnSave      = dispatch . ColumnAction i . Rename
      }
    columnInfo_ sKey c
  columnConfig_ sKey c

-- column info, a summary of datatype and isDerived

columnInfo_ :: Maybe SessionKey -> Entity Column -> ReactElementM eh ()
columnInfo_ !sKey !c = view columnInfo (sKey, c) mempty

columnInfo :: ReactView (Maybe SessionKey, Entity Column)
columnInfo = defineView "column info" $ \(sKey, Entity _ col) ->
  cldiv_ "info" $
    case col ^. columnKind of
      ColumnData dat -> do
        case dat ^. dataColIsDerived of
          Derived    -> faIcon_ "superscript fa-fw"
          NotDerived -> faIcon_ "i-cursor fa-fw"
        dataTypeInfo_ sKey (dat ^. dataColType)
      ColumnReport rep -> do
        faIcon_ "file-text-o"
        reportInfo_ rep

-- configure column

columnConfig_ :: Maybe SessionKey -> Entity Column -> ReactElementM eh ()
columnConfig_ !sKey !c = view columnConfig (sKey, c) mempty

columnConfig :: ReactView (Maybe SessionKey, Entity Column)
columnConfig = defineControllerView "column configuration" store $
  \st (sKey, Entity i col) -> cldiv_ "config" $ do
    let mError = case col ^. columnKind of
          ColumnData dat ->
            case (dat ^. dataColIsDerived, dat ^. dataColCompileResult) of
              (Derived, CompileResultError msg) -> Just msg
              _                                 -> Nothing
          ColumnReport rep ->
            case rep ^. reportColCompiledTemplate of
              CompileResultError msg -> Just msg
              _                      -> Nothing
    button_ (
      maybe [] (\msg -> [ "title" &= msg ]) mError <>
      [ classNames
          [ ("openIcon", True)
          , ("pure", True)
          , ("error", isJust mError)
          ]
      , onClick $ \_ _ ->
          [ SomeStoreAction store $ ColumnAction i $ SetVisibility True ]
      ] ) $ faIcon_ "gear fa-2x"
    -- TODO: check what this does
    when (st ^. stateColumns . at i ^? _Just . stVisible == Just True) $
      case col ^. columnKind of
        ColumnData   dat -> dataColConf_ sKey i dat
        ColumnReport rep -> reportColConf_ i rep

-- column kind: report

reportInfo_ :: ReportCol -> ReactElementM eh ()
reportInfo_ !r = view reportInfo r mempty

reportInfo :: ReactView ReportCol
reportInfo = defineView "report info" $ \r -> clspan_ "reportInfo" $ do
  let format = case r ^. reportColFormat of
        ReportFormatPlain     -> "Plaintext"
        ReportFormatPDF       -> "PDF"
        ReportFormatHTML      -> "HTML"
        -- ReportFormatMarkdown  -> "Markdown"
      lang = case r ^. reportColLanguage of
        Nothing               -> "Plaintext"
        Just ReportLanguageMarkdown -> "Markdown"
        Just ReportLanguageLatex    -> "Latex"
        Just ReportLanguageHTML     -> "HTML"
  elemText $ "Report " <> lang <> " "
  faIcon_ "long-arrow-right"
  elemText format

reportColConf_ :: Id Column -> ReportCol -> ReactElementM eh ()
reportColConf_ !i !r = view reportColConf (i, r) mempty

reportColConf :: ReactView (Id Column, ReportCol)
reportColConf = defineControllerView "report column config" store $
  \st (i, rep) -> cldiv_ "dialog report" $ do
    let mError = case rep ^. reportColCompiledTemplate of
          CompileResultError msg -> Just msg
          _                      -> Nothing
        lang = join (st ^. stateColumns . at i ^? _Just . stTmpReportLanguage) ?: rep ^. reportColLanguage
        format = join (st ^. stateColumns . at i ^? _Just . stTmpReportFormat) ?: rep ^. reportColFormat
        template = join (st ^. stateColumns . at i ^? _Just . stTmpReportTemplate) ?: rep ^. reportColTemplate
    cldiv_ "bodyWrapper" $ do
      cldiv_ "body" $ do
        cldiv_ "language" $ selReportLanguage_ i lang
        cldiv_ "separator" $ faIcon_ "long-arrow-right"
        cldiv_ "format" $ selReportFormat_ i format
      -- input field for template code
      inputTemplate_ i template lang
    for_ mError $ \errMsg -> cldiv_ "error" $ do
      clspan_ "title" "Error"
      cldiv_  "body" $ elemText errMsg
    let cancelActions = SomeStoreAction store . ColumnAction i <$>
          [ SetVisibility False
          , UnsetTmpReportLang
          , UnsetTmpReportFormat
          , UnsetTmpReportTemplate
          ]
        deleteActions = [ SomeStoreAction store $ TableDeleteColumn i ]
        saveActions   = SomeStoreAction store . ColumnAction i <$>
          -- hide dialog
          [ SetVisibility False
          , ReportColUpdate (template, format, lang)
          , UnsetTmpReportLang
          , UnsetTmpReportFormat
          , UnsetTmpReportTemplate
          -- datatype selection
          ]
    confButtons_ cancelActions deleteActions saveActions

selReportLanguage_ :: Id Column -> Maybe ReportLanguage -> ReactElementM eh ()
selReportLanguage_ !i !lang = view selReportLanguage (i, lang) mempty

selReportLanguage :: ReactView (Id Column, Maybe ReportLanguage)
selReportLanguage = defineView "select report lang" $ \(i, lang) ->
  clspan_ "select" $ do
    "Input language"
    select_
      [ "defaultValue" &= show lang
      , onInput $ \evt ->
          let lang' = readMaybe (target evt "value") -- Maybe (Maybe a)
                ?: Nothing -- unexpected
          in  [SomeStoreAction store $ ColumnAction i $ SetTmpReportLang lang' ]
      ] $ optionsFor_ reportLangs

selReportFormat_ :: Id Column -> ReportFormat -> ReactElementM eh ()
selReportFormat_ !i !f = view selReportFormat (i, f) mempty

selReportFormat :: ReactView (Id Column, ReportFormat)
selReportFormat = defineView "select report format" $ \(i, format) ->
  clspan_ "select" $ do
    "Output format"
    select_
      [ "defaultValue" &= show format
      , onInput $ \evt ->
          let format' = readMaybe (target evt "value")
                ?: ReportFormatPlain -- unexpected
          in  [SomeStoreAction store $ ColumnAction i $ SetTmpReportFormat format' ]
      ] $ optionsFor_ reportFormats

inputTemplate_ :: Id Column -> Text -> Maybe ReportLanguage -> ReactElementM eh ()
inputTemplate_ !c !t !lang = view inputTemplate (c, t, lang) mempty

inputTemplate :: ReactView (Id Column, Text, Maybe ReportLanguage)
inputTemplate = defineView "input template" $ \(i, t, lang) -> do
  let mode = case lang of
        Nothing                     -> "text/plain"
        Just ReportLanguageMarkdown -> "text/x-gfm"
        Just ReportLanguageLatex    -> "text/x-stex"
        Just ReportLanguageHTML     -> "text/html"
  div_
    [ "className" $= "inputTemplate"
    ] $ codemirror_ CodemirrorProps
          { codemirrorMode = mode
          , codemirrorTheme = "default"
          , codemirrorReadOnly = CodemirrorEnabled
          , codemirrorValue = t
          , codemirrorOnChange = \v ->
              [ SomeStoreAction store $ ColumnAction i $ SetTmpReportTemplate v ]
          }

-- column kind: data

dataTypeInfo_ :: Maybe SessionKey -> DataType -> ReactElementM eh ()
dataTypeInfo_ !sKey !dt = view dataTypeInfo (sKey, dt) mempty

dataTypeInfo :: ReactView (Maybe SessionKey, DataType)
dataTypeInfo = defineControllerView "datatype info" store $
  \st (sKey, dt) -> span_ [ "className" $= "dataType" ] $
    case dt of
      DataBool     -> "Bool"
      DataString   -> "String"
      DataNumber   -> "Number"
      DataTime     -> "Time"
      DataRecord t -> do onDidMount_ [SomeStoreAction store $ GetTableCache sKey ] mempty
                         let tblName = Map.lookup t (st ^. stateTableCache)
                                    ?: "missing table"
                         elemText $ "Row from " <> tblName
      DataList   d -> do "List ("
                         dataTypeInfo_ sKey d
                         ")"
      DataMaybe  d -> do "Maybe ("
                         dataTypeInfo_ sKey d
                         ")"

dataColConf_ :: Maybe SessionKey -> Id Column -> DataCol -> ReactElementM eh ()
dataColConf_ !sKey !i !d = view dataColConf (sKey, i, d) mempty

dataColConf :: ReactView (Maybe SessionKey, Id Column, DataCol)
dataColConf = defineControllerView "data column configuration" store $
  \st (sKey, i, dat) -> cldiv_ "dialog data" $ do
      let mError = case (dat ^. dataColIsDerived, dat ^. dataColCompileResult) of
            (Derived, CompileResultError msg) -> Just msg
            _                                 -> Nothing
          isDerived = join (st ^. stateColumns . at i ^? _Just . stTmpIsFormula)
            ?: dat ^. dataColIsDerived
          formula = join (st ^. stateColumns . at i ^? _Just . stTmpFormula)
            ?: dat ^. dataColSourceCode
          dt = join (st ^. stateColumns . at i ^? _Just . stTmpDataType)
            ?: dat ^. dataColType
      cldiv_ "bodyWrapper" $ cldiv_ "body" $ do
        cldiv_ "datatype" $
          selDatatype_ sKey i dat (st ^. stateTableCache)
        cldiv_ "formula" $ do
          checkIsFormula_ i isDerived
          -- input field for formula
          inputFormula_ i formula isDerived
      for_ mError $ \errMsg -> cldiv_ "error" $ do
        clspan_ "title" "Error"
        cldiv_  "body" $ elemText errMsg
      let cancelActions = SomeStoreAction store . ColumnAction i <$>
            [ SetVisibility False
            , UnsetTmpDataType
            , UnsetTmpFormula
            , UnsetTmpIsFormula
            ]
          -- TODO: figure out what changed before initiating ajax
          -- in the Nothing case: nothing has changed
          deleteActions = [ SomeStoreAction store $ TableDeleteColumn i ]
          saveActions = SomeStoreAction store . ColumnAction i <$>
            -- hide dialog
            [ SetVisibility False
            -- is formula and formula
            , DataColUpdate (dt, isDerived, formula)
            , UnsetTmpDataType
            , UnsetTmpIsFormula
            , UnsetTmpFormula
            ]
      confButtons_ cancelActions deleteActions saveActions

confButtons_ :: [SomeStoreAction]
             -> [SomeStoreAction]
             -> [SomeStoreAction]
             -> ReactElementM eh ()
confButtons_ cancelActions deleteActions saveActions =
  view confButtons (cancelActions, deleteActions, saveActions) mempty

confButtons :: ReactView ([SomeStoreAction], [SomeStoreAction], [SomeStoreAction])
confButtons = defineView "column configuration buttons" $
  \(cancelActions, deleteActions, saveActions) -> cldiv_ "buttons" $ do
      cldiv_ "left" $ span_
        [ "className" $= "link"
        , onClick $ \_ _ -> cancelActions
        ] "Cancel"
      cldiv_ "right" $ do
        clbutton_ "button delete" deleteActions $ do
          faIcon_ "close"
          "Delete column"
        clbutton_ "button" saveActions $ do
          faIcon_ "check"
          "Save"

-- select datatype

selDatatype_ :: Maybe SessionKey -> Id Column -> DataCol -> TableCache -> ReactElementM eh ()
selDatatype_  !sKey !i !dat !m = view selDatatype (sKey, i, dat, m) mempty

selDatatype :: ReactView (Maybe SessionKey, Id Column, DataCol, TableCache)
selDatatype = defineView "selectDataType" $ \(sKey, i, dat, tables) ->
    selBranch_ sKey (Just $ dat ^. dataColType) tables $
      \dt -> [SomeStoreAction store $ ColumnAction i $ SetTmpDataType dt]

selBranch_ :: Maybe SessionKey -> Maybe DataType -> TableCache -> SelBranchCallback -> ReactElementM eh ()
selBranch_ !sKey !mDt !tables !cb = view selBranch (sKey, mDt, tables, cb) mempty

selBranch :: ReactView (Maybe SessionKey, Maybe DataType, TableCache, SelBranchCallback)
selBranch = defineStatefulView "selBranch" Nothing $ \curBranch (sKey, mDt, tables, cb) -> do
  let defDt = DataNumber
      selectedBranch = curBranch <|> toBranch <$> mDt ?: toBranch defDt
  cldiv_ "selBranch" $ select_
    [ "defaultValue" &= show selectedBranch
    , onInput $ \evt _ -> case readMaybe (target evt "value") of
        Just BBool   -> (cb DataBool  , Just $ Just BBool  )
        Just BString -> (cb DataString, Just $ Just BString)
        Just BNumber -> (cb DataNumber, Just $ Just BNumber)
        Just BTime   -> (cb DataTime  , Just $ Just BTime  )
        Just BMaybe  -> (cb $ DataMaybe defDt, Just $ Just BMaybe)
        Just BList   -> (cb $ DataList  defDt, Just $ Just BList)
        Just BRecord -> ([], Just $ Just BRecord)
        Nothing      -> error "selBranch: unexpected: Nothing"
    ] $ optionsFor_ branches
  case selectedBranch of
    BMaybe  -> selBranch_ sKey (subType =<< mDt) tables (cb . DataMaybe)
    BList   -> selBranch_ sKey (subType =<< mDt) tables (cb . DataList)
    BRecord -> selTable_ sKey (recordTableId =<< mDt) tables (cb . DataRecord)
    _       -> pure ()

selTable_ :: Maybe SessionKey -> Maybe (Id Table) -> TableCache -> SelTableCallback -> ReactElementM eh ()
selTable_ !sKey !mTableId !tables !cb = view selTable (sKey, mTableId, tables, cb) mempty

selTable :: ReactView (Maybe SessionKey, Maybe (Id Table), TableCache, SelTableCallback)
selTable = defineView "select table" $ \(sKey, mTableId, tables, cb) -> do
  onDidMount_ [SomeStoreAction store $ GetTableCache sKey] mempty
  select_
    [ "defaultValue" &= fromMaybe "" (show <$> mTableId)
    , onChange $ \evt -> maybe [] cb $ readMaybe $ target evt "value"
    ] $ do option_ [ "value" $= "" ] $ elemText ""
           optionsFor_ tables

-- checkbox for "is formula"

checkIsFormula_ :: Id Column -> IsDerived -> ReactElementM eh ()
checkIsFormula_ !i !d = view checkIsFormula (i, d) mempty

checkIsFormula :: ReactView (Id Column, IsDerived)
checkIsFormula = defineView "checkIsFormula" $ \(i, isDerived) -> do
    let checked = case isDerived of
          Derived    -> True
          NotDerived -> False
    input_
      [ "type"    $= "checkbox"
      , "checked" &= checked
      , onChange $ \_ ->
          -- flip the checked status
          let newInpType = if checked then NotDerived else Derived
          in  [ SomeStoreAction store $ ColumnAction i $ SetTmpIsFormula newInpType ]
      ]
    span_ [] "Use formula"

-- input field for formula (i.a.)

inputFormula_ :: Id Column -> Text -> IsDerived -> ReactElementM eh ()
inputFormula_ !i !f !d = view inputFormula (i, f, d) mempty

inputFormula :: ReactView (Id Column, Text, IsDerived)
inputFormula = defineView "input formula" $ \(i, formula, inpTyp) -> do
  let (readOnly, roFlag) = case inpTyp of
        Derived    -> (CodemirrorEnabled, False)
        NotDerived -> (CodemirrorDisabled, True)
  div_
    [ classNames
      [ ("inputFormula", True  )
      , ("disabled",      roFlag)
      ]
    ] $ codemirror_ CodemirrorProps
          { codemirrorMode = "text/x-ocaml"
          , codemirrorTheme = "default"
          , codemirrorReadOnly = readOnly
          , codemirrorValue = formula
          , codemirrorOnChange = \v ->
              [ SomeStoreAction store $ ColumnAction i $ SetTmpFormula v ]
          }

--

-- util

--

optionsFor_ :: Show a => Map a Text -> ReactElementM eh ()
optionsFor_ m = for_ (Map.toList m) $ \(value, label) ->
  option_ [ "value" &= show value ] $ elemText label

(?:) :: Maybe a -> a -> a
(?:) = flip fromMaybe

infixl 3 ?:
