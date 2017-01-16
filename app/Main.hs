module Main where

import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (pack)
import Data.Version (showVersion)
import Data.Yaml (decodeEither', encode)
import Filesystem.Path.CurrentOS (directory)
import Options.Applicative as O
import Paths_pdf_slave (version)
import Prelude hiding (FilePath)
import Shelly

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BZ

import Text.PDF.Slave.Template
import Text.PDF.Slave.Render

-- | Define actions that CLI can do
data Command =
  -- | Usual mode, render PDF for template
    GeneratePDF
  -- | Make a all-in bundle from template distributed over files
  | PackBundle
  -- | Make a template distributed over files from all-in bundle
  | UnpackBundle
  -- | Print version number
  | PrintVersion

-- | CLI options
data Options = Options {
  -- | Path to template file, if missing, stdin is used
  templatePath       :: Maybe FilePath
  -- | Path to output pdf file, if missing, stdout is used
, pdfOutputPath      :: Maybe FilePath
  -- | Path to JSON input file for overwritting input data of bundle
, inputOverwritePath :: Maybe FilePath
  -- | If the flag is set, temporary dirs won't be nuked after execution
, preserveTempDirs   :: Bool
  -- | Store bundle in JSON format instead of YAML
, outputBundleJson   :: Bool
  -- | CLI action
, cliCommand         :: Command
}

-- | Same as 'strOption' but parses Text
filePathOption :: Mod OptionFields String -> Parser FilePath
filePathOption m = fmap (fromText . pack) $ strOption m

-- | Parser of CLI options
optionsParser :: Parser Options
optionsParser = Options
  <$> optional (filePathOption $
       long "template"
    <> help "Path to template or bundle YAML description, if missing stdin is used"
    <> metavar "TEMPLATE_YAML_PATH"
    )
  <*> optional (filePathOption $
       long "output"
    <> help "Path to output PDF file, if missing stdout is used"
    <> metavar "OUTPUT_PDF_PATH"
    )
  <*> optional (filePathOption $
       long "input"
    <> help ("You can specify JSON file as input that will overwrite default input"
      <> " for bundle or template input file" )
    <> metavar "INPUT_JSON_PATH"
    )
  <*> switch (
       long "preserve-temp"
    <> help "If the flag is set, temporary dirs won't be nuked after execution"
    )
  <*> switch (
       long "json"
    <> help "Store bundle in JSON format instead of YAML"
    )
  <*> commandParser

-- | Parser of CLI commands
commandParser :: Parser Command
commandParser = subparser $
     (O.command "pdf" $ info (pure GeneratePDF) $ progDesc "Generate PDF from template")
  <> (O.command "pack" $ info (pure PackBundle) $ progDesc "Construct all-in bundle from template distributed over files")
  <> (O.command "unpack" $ info (pure UnpackBundle) $ progDesc "Deconstruct all-in bundle to several files")
  <> (O.command "version" $ info (pure PrintVersion) $ progDesc "Print program version")

-- | Execute PDF slave
pdfSlave :: Options -> IO ()
pdfSlave Options{..} = case cliCommand of
  PrintVersion -> putStrLn $ "pdf-slave " <> showVersion version
  _ -> doMainActions
  where
  doMainActions = shelly $ do
    -- define base directory
    baseDir <- case templatePath of
      Nothing -> pwd
      Just p -> canonic $ directory p
    -- read template
    (templateContent, contentFilename) <- case templatePath of
      Nothing -> do
        cnt <- liftIO $ BS.getContents
        return (cnt, fromText "<stdin>")
      Just p -> (,p) <$> readBinary p
    -- read optional input overwrite
    optInput <- case inputOverwritePath of
      Nothing -> return Nothing
      Just p -> do
        cnt <- readBinary p
        case A.eitherDecode' . BZ.fromStrict $ cnt of
          Left e -> fail $ "Failed to read input overwrite file: " <> show e
          Right i -> return $ Just i
    -- process particular CLI action
    case cliCommand of
      -- Generate PDF from template or bundle
      GeneratePDF -> do
        -- parse template contents
        res <- parseBundleOrTemplate contentFilename templateContent
        -- render
        bs <- case res of
          Left bundle -> do
            let bundle' = fromMaybe bundle $ (\i -> bundle { templateInput = i }) <$> optInput
            renderBundleToPDF bundle' nuke
          Right template -> do
            inputOverwrite <- sequence $ fmap toTextWarn inputOverwritePath
            let template' = fromMaybe template $
                  (const $ template { templateFileInput = inputOverwrite }) <$> optInput
            renderTemplateToPDF template' baseDir nuke
        -- output results
        case pdfOutputPath of
          Nothing -> liftIO $ BS.putStr bs
          Just outputPath -> writeBinary outputPath bs

      -- Pack bundle from distrubuted template format
      PackBundle -> case decodeEither' templateContent of
        Left e -> fail $ "Failed to parse template: " <> show e
        Right tf -> do
          res <- loadTemplateInMemory tf baseDir
          case res of
            Left e -> fail $ "Failed to pack bundle: " <> show e
            Right t -> do
              let bs = if outputBundleJson then BZ.toStrict $ A.encode t else encode t
              case pdfOutputPath of
                Nothing -> liftIO $ BS.putStr bs
                Just outputPath -> do
                  writeBinary outputPath bs
                  outputPath' <- toTextWarn outputPath
                  echo $ "Bundle is written to " <> outputPath'

      -- Unpack bundle to distributed template format
      UnpackBundle -> case decodeEither' templateContent of
        Left e -> fail $ "Failed to parse template: " <> show e
        Right t -> case pdfOutputPath of
          Nothing -> fail "Expecting --output parameter as destination folder"
          Just outputFolder -> do
            mkdir_p outputFolder
            tf <- storeTemplateInFiles t outputFolder
            let templateFilename = outputFolder </> templateName t <.> "yaml"
            writeBinary templateFilename $ encode tf
            outputFolder' <- toTextWarn outputFolder
            echo $ "Bundle is unpacked to " <> outputFolder'
      _ -> return ()

  nuke = not preserveTempDirs

main :: IO ()
main = execParser opts >>= pdfSlave
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "CLI interface for PDF slave."
     <> header "pdf-slave - tool for building PDF documents from HTex templates" )
