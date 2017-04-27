module Herculus.Ace where

import Herculus.Prelude
import Ace as Ace
import Ace.Anchor as Anchor
import Ace.Document as Document
import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Range as Range
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ace (ACE)
import Ace.Types (Anchor, Editor)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Halogen.Query.HalogenM (halt)
import Herculus.Monad (Herc)
import Lib.Compiler.AST.Position (Pos(Pos), Span(Span))
import Lib.Compiler.Error (Error(..))
import Unsafe.Coerce (unsafeCoerce)

data Query a
  = Initialize a
  | Update Input a
  | SetAnnotations (Array Error) a
  | SetText String a
  | Finalize a
  | HandleChange (H.SubscribeStatus -> a)

type State =
  { input :: Input
  , anchors :: Array Anchor
  , editor :: Maybe Editor
  }

data LintType
  = LintError
  | LintWarn
  | LintInfo

type Input =
  { mode :: String
  }

data Output
  = TextChanged String

comp :: H.Component HH.HTML Query Input Output Herc
comp = H.lifecycleComponent
  { initialState:
      { input: _
      , anchors: []
      , editor: Nothing
      }
  , render
  , eval
  , receiver: Just <<< H.action <<< Update
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  }

render :: State -> H.ComponentHTML Query
render _ = HH.div [ HP.ref (H.RefLabel "ace") ] []

eval :: Query ~> H.ComponentDSL State Query Output Herc
eval = case _ of
  Initialize next -> do
    H.getHTMLElementRef (H.RefLabel "ace") >>= case _ of
      Nothing -> halt "Ace: Could not find elemet to attach to."
      Just el -> do
        editor <- liftEff $ Ace.editNode el Ace.ace
        session <- liftEff $ Editor.getSession editor
        modify _
          { editor = Just editor
          }
        { input } <- H.get
        liftEff do
          setBlockScrollingInfinity editor
          Session.setMode input.mode session
          Session.setUseSoftTabs true session
          Session.setTabSize 2 session
          Editor.setTheme "ace/theme/github" editor
          Editor.setMaxLines 100 editor
          Editor.setFontSize "11pt" editor
          Editor.setHighlightActiveLine false editor
        -- Subscribe to changes
        H.subscribe $ H.eventSource_
          (Session.onChange session)
          (H.request HandleChange)
    pure next

  Finalize next -> do
    modify _
      { editor = Nothing
      }
    pure next

  Update input next -> do
    mEditor <- H.gets _.editor
    case mEditor of
      Nothing -> halt "Ace not properly initialized."
      Just editor -> do
        session <- liftEff $ Editor.getSession editor
        liftEff do
          -- Update mode
          Ace.TextMode currentMode <- Session.getMode session
          when (input.mode /= currentMode) do
            void $ Session.setMode input.mode session
    pure next

  SetText text next -> do
    mEditor <- H.gets _.editor
    case mEditor of
      Nothing -> halt "Ace not properly initialized."
      Just editor -> do
        session <- liftEff $ Editor.getSession editor
        liftEff do
          -- Update value
          current <- Editor.getValue editor
          when (text /= current) do
            void $ Editor.setValue text (Just (-1)) editor
    pure next

  SetAnnotations errs next -> do
    mEditor <- H.gets _.editor
    case mEditor of
      Nothing -> halt "Ace not properly initialized."
      Just editor -> do
        session <- liftEff $ Editor.getSession editor
        doc <- liftEff $ Session.getDocument session
        -- Underline
        { anchors } <- get
        anchors' <- liftEff do
          -- Delete anchors and markers
          for_ anchors Anchor.detach
          markers <- Session.getMarkers session
          for_ markers \i ->
            Session.removeMarker (unsafeCoerce i).id session
          -- New
          for errs \(Error err) -> do
            let r = decompSpan err.errSpan
            startAnchor <- Document.createAnchor
              (r.startLine - 1) (r.startCol - 1) doc
            endAnchor <- Document.createAnchor
              (r.endLine - 1) r.endCol doc
            let
              getAnchorRange = do
                Ace.Position {row: startRow, column: startColumn}
                  <- Anchor.getPosition startAnchor
                Ace.Position {row: endRow, column: endColumn}
                  <- Anchor.getPosition endAnchor
                Range.create startRow startColumn endRow endColumn
              addMarker r =
                Session.addMarker r "linter-error" "text" false session
            range <- getAnchorRange
            marker <- addMarker range
            markerRef <- newRef marker
            let
              rerender _ = do
                m <- readRef markerRef
                Session.removeMarker m session
                r <- getAnchorRange
                m' <- addMarker r
                writeRef markerRef m'
            Anchor.onChange startAnchor rerender
            Anchor.onChange endAnchor rerender
            pure [startAnchor, endAnchor]
        modify _{ anchors = join anchors' }
        -- Annotations
        let annos = errs <#> \(Error err) ->
              let r = decompSpan err.errSpan
              in { row: r.startLine - 1
                 , column: r.startCol - 1
                 , text: err.errMsg
                 , type: "error"
                 }
        liftEff $ Session.setAnnotations annos session
        pure next

  HandleChange reply -> do
    mEditor <- H.gets _.editor
    case mEditor of
      Nothing -> halt "Ace not properly initialized."
      Just editor -> do
        value <- liftEff $ Editor.getValue editor
        H.raise $ TextChanged value
    pure (reply H.Listening)

decompSpan
  :: Span
  -> { startCol :: Int, startLine :: Int, endCol :: Int, endLine :: Int }
decompSpan sp =
  let 
    span = case sp of Span s -> s
    start = case span.spanStart of Pos p -> p
    end = case span.spanEnd of Pos p -> p
  in
    { startCol: start.posCol
    , startLine: start.posLine
    , endCol: end.posCol
    , endLine: end.posLine
    }

foreign import setBlockScrollingInfinity
  :: forall eff. Editor -> Eff (ace :: ACE | eff) Unit
