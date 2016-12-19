module Main where

import Control.Monad.IO.Class
import Data.Monoid
import Data.Text (pack)
import Data.Yaml (decodeEither', encode)
import Filesystem.Path.CurrentOS (directory)
import Options.Applicative as O
import Prelude hiding (FilePath)
import Shelly

import qualified Data.ByteString as BS

import Text.PDF.Slave.Template
import Text.PDF.Slave.Render

-- | Template mode either a all-in-one-file or located in distinct files
data TemplateMode = Bundle | Files
  deriving (Show, Read)

-- | Define actions that CLI can do
data Command =
  -- | Usual mode, render PDF for template
    GeneratePDF
  -- | Make a all-in bundle from template distributed over files
  | PackBundle
  -- | Make a template distributed over files from all-in bundle
  | UnpackBundle

-- | CLI options
data Options = Options {
  -- | Path to template file, if missing, stdin is used
  templatePath  :: Maybe FilePath
  -- | Either full bundle or partially stored at files
, templateMode  :: TemplateMode
  -- | Path to output pdf file, if missing, stdout is used
, pdfOutputPath :: Maybe FilePath
  -- | CLI action
, cliCommand    ::  Command
}

-- | Same as 'strOption' but parses Text
filePathOption :: Mod OptionFields String -> Parser FilePath
filePathOption m = fmap (fromText . pack) $ strOption m

-- | Parser of CLI options
optionsParser :: Parser Options
optionsParser = Options
  <$> optional (filePathOption $
       long "template"
    <> help "Path to template YAML description, if missing stdin is used"
    <> metavar "TEMPLATE_YAML_PATH"
    )
  <*> flag Files Bundle (
       long "bundle"
    <> help "If set program expects all-in YAML template."
    )
  <*> optional (filePathOption $
       long "output"
    <> help "Path to output PDF file, if missing stdout is used"
    <> metavar "OUTPUT_PDF_PATH"
    )
  <*> commandParser

-- | Parser of CLI commands
commandParser :: Parser Command
commandParser = subparser $
     (O.command "pdf" $ info (pure GeneratePDF) $ progDesc "Generate PDF from template")
  <> (O.command "pack" $ info (pure PackBundle) $ progDesc "Construct all-in bundle from template distributed over files")
  <> (O.command "unpack" $ info (pure UnpackBundle) $ progDesc "Deconstruct all-in bundle to several files")

-- | Execute PDF slave
pdfSlave :: Options -> IO ()
pdfSlave Options{..} = shelly $ do
  -- define base directory
  baseDir <- case templatePath of
    Nothing -> pwd
    Just p -> canonic $ directory p
  -- read template
  templateContent <- case templatePath of
    Nothing -> liftIO $ BS.getContents
    Just p -> readBinary p
  -- process particular CLI action
  case cliCommand of
    GeneratePDF -> do
      -- how to render template and print output
      let render t = do
            bs <- renderTemplateToPDF t baseDir
            case pdfOutputPath of
              Nothing -> liftIO $ BS.putStr bs
              Just outputPath -> writeBinary outputPath bs
      -- template mode affects type of template record
      case templateMode of
        Bundle -> case decodeEither' templateContent of
          Left e -> fail $ "Failed to parse template: " <> show e
          Right t -> withTmpDir $ \folder -> do
            tf <- storeTemplateInFiles t folder folder
            render tf
        Files -> case decodeEither' templateContent of
          Left e -> fail $ "Failed to parse template: " <> show e
          Right t -> render t
    PackBundle -> case decodeEither' templateContent of
      Left e -> fail $ "Failed to parse template: " <> show e
      Right tf -> do
        res <- loadTemplateInMemory tf
        case res of
          Left e -> fail $ "Failed to pack bundle: " <> show e
          Right t -> do
            let bs = encode t
            case pdfOutputPath of
              Nothing -> liftIO $ BS.putStr bs
              Just outputPath -> do
                writeBinary outputPath bs
                outputPath' <- toTextWarn outputPath
                echo $ "Bundle is written to " <> outputPath'
    UnpackBundle -> case decodeEither' templateContent of
      Left e -> fail $ "Failed to parse template: " <> show e
      Right t -> case pdfOutputPath of
        Nothing -> fail "Expecting --output parameter as destination folder"
        Just outputFolder -> do
          mkdir_p outputFolder
          tf <- storeTemplateInFiles t outputFolder outputFolder
          let templateFilename = outputFolder </> templateName t <.> "yaml"
          writeBinary templateFilename $ encode tf
          outputFolder' <- toTextWarn outputFolder
          echo $ "Bundle is unpacked to " <> outputFolder'

main :: IO ()
main = execParser opts >>= pdfSlave
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "CLI interface for PDF slave."
     <> header "pdf-slave - tool for building PDF documents from HTex templates" )
