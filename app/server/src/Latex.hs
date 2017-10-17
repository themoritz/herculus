{-# LANGUAGE OverloadedStrings #-}

module Latex
  ( makePDF
  ) where

import           Prelude

import           Control.Monad              (unless, when)
import qualified Data.ByteString            as BS
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.List                  (isInfixOf)
import           Data.Monoid                ((<>))
import           System.Directory
import           System.Environment
import           System.Exit                (ExitCode (..))
import           System.FilePath
import           System.IO                  (stderr, stdout)

import           Text.Pandoc.Options        (WriterOptions (..))
import           Text.Pandoc.Process        (pipeProcess)
import           Text.Pandoc.Shared         (withTempDir)
import qualified Text.Pandoc.UTF8           as UTF8

makePDF
  :: WriterOptions
  -> String -> String
  -> FilePath
  -> IO (Either ByteString ByteString)
makePDF opts program source texInputs = withTempDir "tex2pdf." $ \tmpdir -> do
  let args = writerLaTeXArgs opts
  setEnv "TEXINPUTS" (".:" <> texInputs <> ":")
  if program `elem` ["pdflatex", "lualatex", "xelatex"]
     then tex2pdf (writerVerbose opts) args tmpdir program source
     else return $ Left $ UTF8.fromStringLazy $ "Unknown program " ++ program

tex2pdf
  :: Bool           -- ^ Verbose output
  -> [String]       -- ^ Arguments to the latex-engine
  -> FilePath       -- ^ temp directory for output
  -> String         -- ^ tex program
  -> String         -- ^ tex source
  -> IO (Either ByteString ByteString)
tex2pdf verbose args tmpDir program source = do
  let numruns = if "\\tableofcontents" `isInfixOf` source
                   then 3  -- to get page numbers
                   else 2  -- 1 run won't give you PDF bookmarks
  (exit, log', mbPdf) <- runTeXProgram verbose program args 1 numruns tmpDir source
  case (exit, mbPdf) of
       (ExitFailure _, _)      -> do
          let logmsg = extractMsg log'
          let extramsg =
                case logmsg of
                     x | "! Package inputenc Error" `BC.isPrefixOf` x
                           && program /= "xelatex"
                       -> "\nTry running pandoc with --latex-engine=xelatex."
                     _ -> ""
          return $ Left $ logmsg <> extramsg
       (ExitSuccess, Nothing)  -> return $ Left ""
       (ExitSuccess, Just pdf) -> return $ Right pdf

-- parsing output

extractMsg :: ByteString -> ByteString
extractMsg log' = do
  let msg'  = dropWhile (not . ("!" `BC.isPrefixOf`)) $ BC.lines log'
  let (msg'',rest) = break ("l." `BC.isPrefixOf`) msg'
  let lineno = take 1 rest
  if null msg'
     then log'
     else BC.unlines (msg'' ++ lineno)

-- running tex programs

-- Run a TeX program on an input bytestring and return (exit code,
-- contents of stdout, contents of produced PDF if any).  Rerun
-- a fixed number of times to resolve references.
runTeXProgram :: Bool -> String -> [String] -> Int -> Int -> FilePath -> String
              -> IO (ExitCode, ByteString, Maybe ByteString)
runTeXProgram verbose program args runNumber numRuns tmpDir source = do
    let file = tmpDir </> "input.tex"
    exists <- doesFileExist file
    unless exists $ UTF8.writeFile file source
    let programArgs = ["-halt-on-error", "-interaction", "nonstopmode",
         "-output-directory", tmpDir] ++ args ++ [file]
    env' <- getEnvironment
    let sep = [searchPathSeparator]
    let texinputs = maybe (tmpDir ++ sep) ((tmpDir ++ sep) ++)
          $ lookup "TEXINPUTS" env'
    let env'' = ("TEXINPUTS", texinputs) :
                  [(k,v) | (k,v) <- env', k /= "TEXINPUTS"]
    when (verbose && runNumber == 1) $ do
      putStrLn "[makePDF] temp dir:"
      putStrLn tmpDir
      putStrLn "[makePDF] Command line:"
      putStrLn $ program ++ " " ++ unwords (map show programArgs)
      putStr "\n"
      putStrLn "[makePDF] Environment:"
      mapM_ print env''
      putStr "\n"
      putStrLn $ "[makePDF] Contents of " ++ file ++ ":"
      BL.readFile file >>= BL.putStr
      putStr "\n"
    (exit, out, err) <- pipeProcess (Just env'') program programArgs BL.empty
    when verbose $ do
      putStrLn $ "[makePDF] Run #" ++ show runNumber
      BL.hPutStr stdout out
      BL.hPutStr stderr err
      putStr "\n"
    if runNumber <= numRuns
       then runTeXProgram verbose program args (runNumber + 1) numRuns tmpDir source
       else do
         let pdfFile = replaceDirectory (replaceExtension file ".pdf") tmpDir
         pdfExists <- doesFileExist pdfFile
         pdf <- if pdfExists
                   -- We read PDF as a strict bytestring to make sure that the
                   -- temp directory is removed on Windows.
                   -- See https://github.com/jgm/pandoc/issues/1192.
                   then (Just . BL.fromChunks . (:[])) `fmap` BS.readFile pdfFile
                   else return Nothing
         return (exit, out <> err, pdf)
