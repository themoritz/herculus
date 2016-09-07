{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Views.Column where

import           Control.Lens              hiding (view)
import           React.Flux

import           Control.Applicative       ((<|>))
import           Control.DeepSeq           (NFData)
import           Control.Monad             (when)
import           Data.Foldable             (for_)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe, isJust)
import           Data.Monoid               ((<>))
import           Data.Proxy
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           Text.Read                 (readMaybe)

import           Lib.Api.Rest              (TableListGlobal)
import           Lib.Model
import           Lib.Model.Column
import           Lib.Model.Table
import           Lib.Types
import           React.Flux.Addons.Servant (request)

import           Store
import           Views.Combinators
import           Views.Common              (EditBoxProps (..), editBox_)
import           Views.Foreign

-- state of column controller view

type TableCache = Map (Id Table) Text

data ColConfState = ColConfState
  { _ccsTmpDataType       :: Map (Id Column) DataType
  , _ccsTmpIsFormula      :: Map (Id Column) IsDerived
  , _ccsTmpFormula        :: Map (Id Column) Text
  , _ccsVisible           :: Set (Id Column) -- not in set === false
  , _ccsTableCache        :: TableCache
  , _ccsTmpReportLanguage :: Map (Id Column) (Maybe ReportLanguage)
  , _ccsTmpReportFormat   :: Map (Id Column) ReportFormat
  , _ccsTmpReportTemplate :: Map (Id Column) Text
  }

makeLenses ''ColConfState

data ColConfAction
  = ColumnSetTmpDataType       (Id Column) DataType
  | ColumnUnsetTmpDataType     (Id Column)
  | ColumnSetTmpIsFormula      (Id Column) IsDerived
  | ColumnUnsetTmpIsFormula    (Id Column)
  | ColumnSetTmpFormula        (Id Column) Text
  | ColumnUnsetTmpFormula      (Id Column)
  | ColumnSetVisibility        (Id Column) Bool
  | ColumnGetTableCache
  | ColumnSetTableCache        TableCache
  | ColumnSetTmpReportLang     (Id Column) (Maybe ReportLanguage)
  | ColumnUnsetTmpReportLang   (Id Column)
  | ColumnSetTmpReportFormat   (Id Column) ReportFormat
  | ColumnUnsetTmpReportFormat (Id Column)
  | ColumnSetTmpReportTemplate       (Id Column) Text
  | ColumnUnsetTmpReportTemplate     (Id Column)
  deriving (Typeable, Generic, NFData)

instance StoreData ColConfState where
  type StoreAction ColConfState = ColConfAction
  transform action st = case action of
    ColumnSetTmpDataType i dt ->
      pure $ st & ccsTmpDataType . at i .~ Just dt
    ColumnUnsetTmpDataType i ->
      pure $ st & ccsTmpDataType . at i .~ Nothing
    ColumnSetTmpIsFormula i it ->
      pure $ st & ccsTmpIsFormula . at i .~ Just it
    ColumnUnsetTmpIsFormula i ->
      pure $ st & ccsTmpIsFormula . at i .~ Nothing
    ColumnSetTmpFormula i s ->
      pure $ st & ccsTmpFormula . at i .~ Just s
    ColumnUnsetTmpFormula i ->
      pure $ st & ccsTmpFormula . at i .~ Nothing

    ColumnSetVisibility i b -> pure $ case b of
      True  -> st & ccsVisible %~ Set.insert i
      False -> st & ccsVisible %~ Set.delete i

    ColumnGetTableCache -> do
      when (Map.null $ st ^. ccsTableCache) $
        request api (Proxy :: Proxy TableListGlobal) $ pure . \case
          Left (_, e) -> dispatch $ GlobalSetError $ Text.pack e
          Right ts    -> [SomeStoreAction colConfStore $ ColumnSetTableCache $ toTableMap ts]
      pure st
    ColumnSetTableCache m ->
      pure $ st & ccsTableCache .~ m

    ColumnSetTmpReportLang i lang ->
      pure $ st & ccsTmpReportLanguage . at i .~ Just lang
    ColumnUnsetTmpReportLang i ->
      pure $ st & ccsTmpReportLanguage . at i .~ Nothing
    ColumnSetTmpReportFormat i format ->
      pure $ st & ccsTmpReportFormat . at i .~ Just format
    ColumnUnsetTmpReportFormat i ->
      pure $ st & ccsTmpReportFormat . at i .~ Nothing
    ColumnSetTmpReportTemplate i templ ->
      pure $ st & ccsTmpReportTemplate . at i .~ Just templ
    ColumnUnsetTmpReportTemplate i ->
      pure $ st & ccsTmpReportTemplate . at i .~ Nothing

colConfStore :: ReactStore ColConfState
colConfStore = mkStore $ ColConfState Map.empty
                                      Map.empty
                                      Map.empty
                                      Set.empty
                                      Map.empty
                                      Map.empty
                                      Map.empty
                                      Map.empty

-- helper

toTableMap :: [Entity Table] -> TableCache
toTableMap = Map.fromList . map (\(Entity i Table{..}) -> (i, tableName))

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
  , (BRecord, "Record")
  , (BList  , "List"  )
  , (BMaybe , "Maybe" )
  ]

reportLangs :: Map (Maybe ReportLanguage) Text
reportLangs = Map.fromList
  [ (Nothing              , "Plaintext" )
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

column_ :: Entity Column -> ReactElementM eh ()
column_ !c = view column c mempty

column :: ReactView (Entity Column)
column = defineView "column" $ \c@(Entity i col) -> cldiv_ "column" $ do
  cldiv_ "head" $ do
    editBox_ EditBoxProps
      { editBoxValue       = col ^. columnName
      , editBoxPlaceholder = "Column name..."
      , editBoxClassName   = "columnName"
      , editBoxShow        = id
      , editBoxValidator   = Just
      , editBoxOnSave      = dispatch . ColumnRename i
      }
    columnInfo_ c
  columnConfig_ c

-- column info, a summary of datatype and isDerived

columnInfo_ :: Entity Column -> ReactElementM eh ()
columnInfo_ !c = view columnInfo c mempty

columnInfo :: ReactView (Entity Column)
columnInfo = defineView "column info" $ \(Entity _ col) ->
  cldiv_ "info" $
    case col ^. columnKind of
      ColumnData dat -> do
        case dat ^. dataColIsDerived of
          Derived    -> faIcon_ "superscript fa-fw"
          NotDerived -> faIcon_ "i-cursor fa-fw"
        dataTypeInfo_ $ dat ^. dataColType
      ColumnReport rep -> do
        faIcon_ "file-text-o"
        reportInfo_ rep

-- configure column

columnConfig_ :: Entity Column -> ReactElementM eh ()
columnConfig_ !c = view columnConfig c mempty

columnConfig :: ReactView (Entity Column)
columnConfig = defineControllerView "column configuration" colConfStore $
  \state (Entity i col) -> cldiv_ "config" $ do
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
          [ SomeStoreAction colConfStore $ ColumnSetVisibility i True ]
      ] ) $ faIcon_ "gears fa-2x"
    when (Set.member i $ state ^. ccsVisible) $
      case col ^. columnKind of
        ColumnData   dat -> dataColConf_ i dat
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
reportColConf = defineControllerView "report column config" colConfStore $
  \state (i, rep) -> cldiv_ "dialog report" $ do
    let mError = case rep ^. reportColCompiledTemplate of
          CompileResultError msg -> Just msg
          _                      -> Nothing
        lang = state ^. ccsTmpReportLanguage . at i ?: rep ^. reportColLanguage
        format = state ^. ccsTmpReportFormat . at i ?: rep ^. reportColFormat
        template = state ^. ccsTmpReportTemplate . at i ?: rep ^. reportColTemplate
    cldiv_ "bodyWrapper" $ cldiv_ "body" $ do
      cldiv_ "language" $ selReportLanguage_ i lang
      cldiv_ "separator" $ faIcon_ "long-arrow-right"
      cldiv_ "format" $ selReportFormat_ i format
    -- input field for template code
    inputTemplate_ i template lang
    for_ mError $ \errMsg -> cldiv_ "error" $ do
      clspan_ "title" "Error"
      cldiv_  "body" $ elemText errMsg
    let cancelActions =
          [ SomeStoreAction colConfStore $ ColumnSetVisibility i False
          , SomeStoreAction colConfStore $ ColumnUnsetTmpReportLang i
          , SomeStoreAction colConfStore $ ColumnUnsetTmpReportFormat i
          , SomeStoreAction colConfStore $ ColumnUnsetTmpReportTemplate i
          ]
        deleteActions = [ SomeStoreAction store $ TableDeleteColumn i ]
        saveActions   =
          -- hide dialog
          [ SomeStoreAction colConfStore $ ColumnSetVisibility i False
          , SomeStoreAction store        $ ColumnSetReportLang i lang
          , SomeStoreAction colConfStore $ ColumnUnsetTmpReportLang i
          , SomeStoreAction store        $ ColumnSetReportFormat i format
          , SomeStoreAction colConfStore $ ColumnUnsetTmpReportFormat i
          , SomeStoreAction store        $ ColumnSetReportTemplate i template
          , SomeStoreAction colConfStore $ ColumnUnsetTmpReportTemplate i
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
          in  [SomeStoreAction colConfStore $ ColumnSetTmpReportLang i lang' ]
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
          in  [SomeStoreAction colConfStore $ ColumnSetTmpReportFormat i format' ]
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
              [ SomeStoreAction colConfStore $ ColumnSetTmpReportTemplate i v ]
          }

-- column kind: data

dataTypeInfo_ :: DataType -> ReactElementM eh ()
dataTypeInfo_ !dt = view dataTypeInfo dt mempty

dataTypeInfo :: ReactView DataType
dataTypeInfo = defineControllerView "datatype info" colConfStore $
  \state dt -> span_ [ "className" $= "dataType" ] $
    case dt of
      DataBool     -> "Bool"
      DataString   -> "String"
      DataNumber   -> "Number"
      DataTime     -> "Time"
      DataRecord t -> do onDidMount_ [SomeStoreAction colConfStore ColumnGetTableCache] mempty
                         let tableName = Map.lookup t (state ^. ccsTableCache)
                                      ?: "missing table"
                         elemText $ "Records of " <> tableName
      DataList   d -> do "List ("
                         dataTypeInfo_ d
                         ")"
      DataMaybe  d -> do "Maybe ("
                         dataTypeInfo_ d
                         ")"

dataColConf_ :: Id Column -> DataCol -> ReactElementM eh ()
dataColConf_ !i !d = view dataColConf (i, d) mempty

dataColConf :: ReactView (Id Column, DataCol)
dataColConf = defineControllerView "data column configuration" colConfStore $
  \state (i, dat) -> cldiv_ "dialog data" $ do
      let mError = case (dat ^. dataColIsDerived, dat ^. dataColCompileResult) of
            (Derived, CompileResultError msg) -> Just msg
            _                                 -> Nothing
          isDerived = state ^. ccsTmpIsFormula . at i ?: dat ^. dataColIsDerived
          formula = state ^. ccsTmpFormula . at i ?: dat ^. dataColSourceCode
      cldiv_ "bodyWrapper" $ cldiv_ "body" $ do
        cldiv_ "datatype" $
          selDatatype_ i dat (state ^. ccsTableCache)
        cldiv_ "formula" $ do
          checkIsFormula_ i isDerived
          -- input field for formula
          inputFormula_ i formula isDerived
      for_ mError $ \errMsg -> cldiv_ "error" $ do
        clspan_ "title" "Error"
        cldiv_  "body" $ elemText errMsg
      let cancelActions =
            [ SomeStoreAction colConfStore $ ColumnSetVisibility i False
            , SomeStoreAction colConfStore $ ColumnUnsetTmpDataType i
            , SomeStoreAction colConfStore $ ColumnUnsetTmpFormula i
            , SomeStoreAction colConfStore $ ColumnUnsetTmpIsFormula i
            ]
          dataTypeActions = case state ^. ccsTmpDataType . at i of
            Nothing -> []
            Just dt -> [ SomeStoreAction store        $ ColumnSetDt i dt
                       , SomeStoreAction colConfStore $ ColumnUnsetTmpDataType i
                       ]
          -- TODO: figure out what changed before initiating ajax
          -- in the Nothing case: nothing has changed
          deleteActions = [ SomeStoreAction store $ TableDeleteColumn i ]
          saveActions =
            -- hide dialog
            [ SomeStoreAction colConfStore $ ColumnSetVisibility i False
            -- is formula and formula
            , SomeStoreAction store        $ ColumnSetFormula i (isDerived, formula)
            , SomeStoreAction colConfStore $ ColumnUnsetTmpIsFormula i
            , SomeStoreAction colConfStore $ ColumnUnsetTmpFormula i
            -- datatype selection
            ] ++ dataTypeActions
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

selDatatype_ :: Id Column -> DataCol -> TableCache -> ReactElementM eh ()
selDatatype_  i dat m = view selDatatype (i, dat, m) mempty

selDatatype :: ReactView (Id Column, DataCol, TableCache)
selDatatype = defineView "selectDataType" $ \(i, dat, tables) ->
    selBranch_ (Just $ dat ^. dataColType) tables $
      \dt -> [SomeStoreAction colConfStore $ ColumnSetTmpDataType i dt]

selBranch_ :: Maybe DataType -> TableCache -> SelBranchCallback -> ReactElementM eh ()
selBranch_ mDt tables cb = view selBranch (mDt, tables, cb) mempty

selBranch :: ReactView (Maybe DataType, TableCache, SelBranchCallback)
selBranch = defineStatefulView "selBranch" Nothing $ \curBranch (mDt, tables, cb) -> do
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
    BMaybe  -> selBranch_ (subType =<< mDt) tables (cb . DataMaybe)
    BList   -> selBranch_ (subType =<< mDt) tables (cb . DataList)
    BRecord -> selTable_  (recordTableId =<< mDt) tables (cb . DataRecord)
    _       -> pure ()

selTable_ :: Maybe (Id Table) -> TableCache -> SelTableCallback -> ReactElementM eh ()
selTable_ mTableId tables cb = view selTable (mTableId, tables, cb) mempty

selTable :: ReactView (Maybe (Id Table), TableCache, SelTableCallback)
selTable = defineView "select table" $ \(mTableId, tables, cb) -> do
  onDidMount_ [SomeStoreAction colConfStore ColumnGetTableCache] mempty
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
          in  [ SomeStoreAction colConfStore $ ColumnSetTmpIsFormula i newInpType ]
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
              [ SomeStoreAction colConfStore $ ColumnSetTmpFormula i v ]
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
