{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Docs
  ( moduleDoc
  ) where

import           Lib.Prelude                  hiding (empty, (<$>))

import           Data.Functor.Foldable
import qualified Data.Text                    as T

import           Text.PrettyPrint.Leijen.Text

import           Lib.Compiler.AST
import           Lib.Compiler.AST.Common
import           Lib.Compiler.Check.Extract
import           Lib.Compiler.Parse.State
import           Lib.Compiler.Pretty
import           Lib.Compiler.Type

section :: Text -> Doc
section str = textStrict "##" <+> textStrict str

decl :: Text -> Doc
decl str = textStrict "###" <+> textStrict str

subdecl :: Text -> Doc
subdecl str = textStrict "####" <+> textStrict str

code :: Doc -> Doc
code c =
  textStrict "```haskell" <$$>
  c <$$>
  textStrict "```"

inlineCode :: Doc -> Doc
inlineCode c = char '`' <> c <> char '`'

header :: Text -> Doc
header str = case fst (splitDocString str) of
  Nothing -> empty
  Just h  -> section h

docString :: Text -> Doc
docString str = vsep $ map pretty $ snd (splitDocString str)

-- | Splits into header and actual docstring
splitDocString :: Text -> (Maybe Text, [Text])
splitDocString ds = case T.lines ds of
  [] -> (Nothing, [])
  h:rest -> case T.stripPrefix "# " h of
    Just h' -> (Just h', rest)
    Nothing -> (Nothing, h:rest)

paragraphs :: [Doc] -> Doc
paragraphs = vsep . punctuate line . mapMaybe go
  where go doc = if isEmpty doc then Nothing else Just doc

typeAsArg :: SourceType -> Doc
typeAsArg t =
  let (doc, need) = histo typeDoc (stripAnn t)
  in if need > 0 then parens doc else doc

moduleDoc :: Module -> Doc
moduleDoc (extractDecls -> decls) =
  paragraphs $ (textStrict "# Function Reference") : (join $ map goDecl decls)
  where
  instances = extractInstanceDecls decls
  getInstances cls = flip filter instances $ \ExInstanceDecl {..} ->
    getText (fst iHead) == cls
  goDecl = \case

    EDData ExDataDecl {..} ->
      [ header dDocString
      , decl name
      , code (textStrict "type" <+> textStrict name <+> hsep (map goArg dArgs))
      , docString dDocString
      ] <> constrs
      where
        SourceText _ name = dName
        goArg = textStrict . getText
        constrs = if null dConstrs
          then []
          else [subdecl "Constructors", paragraphs (map goConstr dConstrs)]
        goConstr (ds, n, args) =
          textStrict "*  " <+> align (paragraphs
            [ inlineCode (textStrict (getText n) <+> hsep (map typeAsArg args))
            , docString ds
            ]
          )

    EDClass ExClassDecl {..} ->
      [ header cDocString
      , decl h
      , code ( textStrict "interface" <+> hsep (map goSuper cSupers) <+>
             textStrict h <+> textStrict p <+> textStrict "where"
           )
      , docString cDocString
      , subdecl "Methods"
      , methods
      , subdecl "Implementations"
      , insts
      ]
      where
        (SourceText _ h, SourceText _ p) = cHead
        goSuper (SourceText _ cls, SourceText _ param) =
          textStrict cls <+> textStrict param <+> textStrict "=>"
        methods = paragraphs (map goMethod cMethods)
        goMethod ExTypeDecl {..} =
          textStrict "*  " <+> align (paragraphs
            [ inlineCode $ (textStrict $ getText tName) <+> colon <+>
                     textStrict (prettyPolyType $ map stripAnn tPolyType)
            , docString tDocString
            ]
          )
        insts = vsep (map goInst $ getInstances h)
        goInst ExInstanceDecl {..} =
          textStrict "*  " <+> inlineCode (
            hsep (map goConstr iConstraints) <+>
            textStrict h <+> typeAsArg (snd iHead)
          )
          where
            goConstr (SourceText _ c, t) =
              textStrict c <+> typeAsArg t <+> textStrict "=>"

    EDInstance _ -> []

    EDType ExTypeDecl {..} ->
      [ header tDocString
      , decl name
      , code (textStrict name <+> colon <+> poly)
      , docString tDocString
      ]
      where
        poly = textStrict $ prettyPolyType $ map stripAnn tPolyType
        SourceText _ name = tName

    EDValue _ -> []

    EDFixity ExFixityDecl {..} ->
      [ decl $ "(" <> op <> ")"
      , textStrict $
          "Infix operator alias for [" <> alias <> "](#" <> alias <>
          "). _" <> goAssoc <> "-associative, precedence: " <> show p <> "_"
      , docString fDocString
      ]
      where
        Infix assoc p = fFixity
        goAssoc = case assoc of
          AssocL -> "left"
          AssocR -> "right"
          AssocN -> "non"
        SourceText _ op = fOperator
        SourceText _ alias = fAlias
