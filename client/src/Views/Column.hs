{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Views.Column where

import           Control.Applicative       ((<|>))
import           Control.DeepSeq           (NFData)
import           Control.Lens              hiding (view)
import           Control.Monad             (join, when)

import           Data.Foldable             (for_)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe, isJust)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)

import           GHC.Generics              (Generic)
import           Text.Read                 (readMaybe)

import           React.Flux

import           Lib.Api.Rest              (Command (..))
import           Lib.Model
import           Lib.Model.Auth            (SessionKey)
import           Lib.Model.Column
import           Lib.Model.Project         (ProjectClient)
import           Lib.Model.Table
import           Lib.Types

import           Store                     (dispatchProjectCommand)
import           Views.Combinators
import           Views.Common              (EditBoxProps (..), editBox_)
import           Views.Foreign

import qualified Views.Column.ConfigDialog as Dialog

type TableNames        = Map (Id Table) Text
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
  DataRowRef _ -> BRecord
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
recordTableId (DataRowRef tId) = Just tId
recordTableId _                = Nothing

--

-- views

--

column_ :: Id ProjectClient -> SessionKey -> TableNames -> Entity Column -> ReactElementM eh ()
column_ !projectId !sKey !tables !c = view column (projectId, sKey, tables, c) mempty

column :: ReactView (Id ProjectClient, SessionKey, TableNames, Entity Column)
column = defineView "column" $ \(projectId, sKey, tables, c@(Entity i col)) -> cldiv_ "column" $ do
  cldiv_ "head" $ do
    editBox_ EditBoxProps
      { editBoxValue       = col ^. columnName
      , editBoxPlaceholder = "Name..."
      , editBoxClassName   = "columnName"
      , editBoxShow        = id
      , editBoxValidator   = Just
      , editBoxOnSave      = dispatchProjectCommand . CmdColumnSetName i
      }
    columnInfo_ projectId sKey tables c
  columnConfig_ projectId sKey tables c

-- column info, a summary of datatype and isDerived

columnInfo_ :: Id ProjectClient -> SessionKey -> TableNames -> Entity Column -> ReactElementM eh ()
columnInfo_ !projectId !sKey !tables !c = view columnInfo (projectId, sKey, tables, c) mempty

columnInfo :: ReactView (Id ProjectClient, SessionKey, TableNames, Entity Column)
columnInfo = defineView "column info" $ \(projectId, sKey, tables, Entity _ col) ->
  cldiv_ "info" $
    case col ^. columnKind of
      ColumnData dat -> do
        case dat ^. dataColIsDerived of
          Derived    -> faIcon_ "superscript fa-fw"
          NotDerived -> faIcon_ "i-cursor fa-fw"
        dataTypeInfo_ projectId sKey tables (dat ^. dataColType)
      ColumnReport rep -> do
        faIcon_ "file-text-o"
        reportInfo_ rep

-- configure column

columnConfig_ :: Id ProjectClient -> SessionKey -> TableNames -> Entity Column -> ReactElementM eh ()
columnConfig_ !projectId !sKey !tables !c = view columnConfig (projectId, sKey, tables, c) mempty

columnConfig :: ReactView (Id ProjectClient, SessionKey, TableNames, Entity Column)
columnConfig = defineControllerView "column configuration" Dialog.store $
  \st (projectId, sKey, tables, Entity i col) -> cldiv_ "config" $ do
    let mError = getColumnError col
    button_ (
      maybe [] (\msg -> [ "title" &= msg ]) mError <>
      [ classNames
          [ ("openIcon", True)
          , ("pure", True)
          , ("error", isJust mError)
          ]
      , onClick $ \_ _ -> Dialog.dispatch $ Dialog.SetVisibility i True
      ] ) $ faIcon_ "gear fa-2x"
    when (st ^? Dialog.atDialog i . Dialog.stVisible == Just True) $
      case col ^. columnKind of
        ColumnData   dat -> dataColConf_ projectId sKey tables i dat
        ColumnReport rep -> reportColConf_ i rep

-- column kind: report

reportInfo_ :: ReportCol -> ReactElementM eh ()
reportInfo_ !r = view reportInfo r mempty

reportInfo :: ReactView ReportCol
reportInfo = defineView "report info" $ \r -> clspan_ "reportInfo" $ do
  let format = case r ^. reportColFormat of
        ReportFormatPlain -> "Plaintext"
        ReportFormatPDF   -> "PDF"
        ReportFormatHTML  -> "HTML"
        -- ReportFormatMarkdown  -> "Markdown"
      lang = case r ^. reportColLanguage of
        Nothing                     -> "Plaintext"
        Just ReportLanguageMarkdown -> "Markdown"
        Just ReportLanguageLatex    -> "Latex"
        Just ReportLanguageHTML     -> "HTML"
  elemText $ "Report " <> lang <> " "
  faIcon_ "long-arrow-right"
  elemText format

reportColConf_ :: Id Column -> ReportCol -> ReactElementM eh ()
reportColConf_ !i !r = view reportColConf (i, r) mempty

reportColConf :: ReactView (Id Column, ReportCol)
reportColConf = defineControllerView "report column config" Dialog.store $
  \st (i, rep) -> cldiv_ "dialog report" $ do
    let mError = case rep ^. reportColCompiledTemplate of
          CompileResultError msg -> Just msg
          _                      -> Nothing
        lang = join (st ^? Dialog.atDialog i . Dialog.stTmpReportLanguage)
          ?: rep ^. reportColLanguage
        format = join (st ^? Dialog.atDialog i . Dialog.stTmpReportFormat)
          ?: rep ^. reportColFormat
        template = join (st ^? Dialog.atDialog i . Dialog.stTmpReportTemplate)
          ?: rep ^. reportColTemplate
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
    let cancelActions = Dialog.mkAction <$>
          [ Dialog.SetVisibility i False
          , Dialog.UnsetTmpReportLang i
          , Dialog.UnsetTmpReportFormat i
          , Dialog.UnsetTmpReportTemplate i
          ]
        deleteActions = dispatchProjectCommand $ CmdColumnDelete i
        saveActions   =
          dispatchProjectCommand (CmdReportColUpdate i template format lang) <>
            map Dialog.mkAction
              [ Dialog.SetVisibility i False
              , Dialog.UnsetTmpReportLang i
              , Dialog.UnsetTmpReportFormat i
              , Dialog.UnsetTmpReportTemplate i
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
          in  Dialog.dispatch $ Dialog.SetTmpReportLang i lang'
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
          in  Dialog.dispatch $ Dialog.SetTmpReportFormat i format'
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
          , codemirrorOnChange = Dialog.dispatch . Dialog.SetTmpReportTemplate i
          }

-- column kind: data

dataTypeInfo_ :: Id ProjectClient -> SessionKey -> TableNames -> DataType -> ReactElementM eh ()
dataTypeInfo_ !projectId !sKey !tables !dt = view dataTypeInfo (projectId, sKey, tables, dt) mempty

dataTypeInfo :: ReactView (Id ProjectClient, SessionKey, TableNames, DataType)
dataTypeInfo = defineView "datatype info" $
  \(projectId, sKey, tables, dt) -> span_ [ "className" $= "dataType" ] $
    case dt of
      DataBool     -> "Bool"
      DataString   -> "String"
      DataNumber   -> "Number"
      DataTime     -> "Time"
      DataRowRef t -> do
        let tblName = Map.lookup t tables
                        ?: "missing table"
        elemText $ "Row from " <> tblName
      DataList   d -> do "List ("
                         dataTypeInfo_ projectId sKey tables d
                         ")"
      DataMaybe  d -> do "Maybe ("
                         dataTypeInfo_ projectId sKey tables d
                         ")"

dataColConf_ :: Id ProjectClient
             -> SessionKey
             -> TableNames
             -> Id Column
             -> DataCol
             -> ReactElementM eh ()
dataColConf_ !projectId !sKey !tables !i !d =
  view dataColConf (projectId, sKey, tables, i, d) mempty

dataColConf :: ReactView (Id ProjectClient, SessionKey, TableNames, Id Column, DataCol)
dataColConf = defineControllerView "data column configuration" Dialog.store $
  \st (projectId, sKey, tables, i, dat) -> cldiv_ "dialog data" $ do
      let mError = case (dat ^. dataColIsDerived, dat ^. dataColCompileResult) of
            (Derived, CompileResultError msg) -> Just msg
            _                                 -> Nothing
          isDerived = join (st ^? Dialog.atDialog i . Dialog.stTmpIsFormula)
            ?: dat ^. dataColIsDerived
          formula = join (st ^? Dialog.atDialog i . Dialog.stTmpFormula)
            ?: dat ^. dataColSourceCode
          dt = join (st ^? Dialog.atDialog i . Dialog.stTmpDataType)
            ?: dat ^. dataColType
      cldiv_ "bodyWrapper" $ cldiv_ "body" $ do
        cldiv_ "datatype" $
          selDatatype_ projectId sKey i dat tables
        cldiv_ "formula" $ do
          checkIsFormula_ i isDerived
          inputFormula_ i formula isDerived
      for_ mError $ \errMsg -> cldiv_ "error" $ do
        clspan_ "title" "Error"
        cldiv_  "body" $ elemText errMsg
      let cancelActions = Dialog.mkAction <$>
            [ Dialog.SetVisibility i False
            , Dialog.UnsetTmpDataType i
            , Dialog.UnsetTmpFormula i
            , Dialog.UnsetTmpIsFormula i
            ]
          -- TODO: figure out what changed before initiating ajax
          -- in the Nothing case: nothing has changed
          deleteActions = dispatchProjectCommand $ CmdColumnDelete i
          saveActions =
            dispatchProjectCommand (CmdDataColUpdate i dt isDerived formula) <>
              map Dialog.mkAction
                [ Dialog.SetVisibility i False
                , Dialog.UnsetTmpDataType i
                , Dialog.UnsetTmpIsFormula i
                , Dialog.UnsetTmpFormula i
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

selDatatype_ :: Id ProjectClient
             -> SessionKey
             -> Id Column
             -> DataCol
             -> TableNames
             -> ReactElementM eh ()
selDatatype_  !projectId !sKey !i !dat !tables =
  view selDatatype (projectId, sKey, i, dat, tables) mempty

selDatatype :: ReactView (Id ProjectClient,
                          SessionKey,
                          Id Column,
                          DataCol,
                          TableNames
                         )
selDatatype =
  defineView "selectDataType" $ \(projectId, sKey, i, dat, tables) ->
    selBranch_ projectId sKey (Just $ dat ^. dataColType) tables $
      Dialog.dispatch . Dialog.SetTmpDataType i

selBranch_ :: Id ProjectClient
           -> SessionKey
           -> Maybe DataType
           -> TableNames
           -> SelBranchCallback
           -> ReactElementM eh ()
selBranch_ !projectId !sKey !mDt !tables !cb =
  view selBranch (projectId, sKey, mDt, tables, cb) mempty

selBranch :: ReactView (Id ProjectClient,
                        SessionKey,
                        Maybe DataType,
                        TableNames,
                        SelBranchCallback
                       )
selBranch = defineStatefulView "selBranch" Nothing $
  \curBranch (projectId, sKey, mDt, tables, cb) -> do
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
      BMaybe  -> selBranch_ projectId sKey (subType =<< mDt) tables (cb . DataMaybe)
      BList   -> selBranch_ projectId sKey (subType =<< mDt) tables (cb . DataList)
      BRecord -> selTable_ projectId sKey (recordTableId =<< mDt) tables (cb . DataRowRef)
      _       -> pure ()

selTable_ :: Id ProjectClient
          -> SessionKey
          -> Maybe (Id Table)
          -> TableNames
          -> SelTableCallback
          -> ReactElementM eh ()
selTable_ !projectId !sKey !mTableId !tables !cb =
  view selTable (projectId, sKey, mTableId, tables, cb) mempty

selTable :: ReactView (Id ProjectClient,
                       SessionKey,
                       Maybe (Id Table),
                       TableNames,
                       SelTableCallback
                      )
selTable = defineView "select table" $ \(projectId, sKey, mTableId, tables, cb) ->
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
          in  Dialog.dispatch $ Dialog.SetTmpIsFormula i newInpType
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
          , codemirrorOnChange =
              Dialog.dispatch . Dialog.SetTmpFormula i
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
