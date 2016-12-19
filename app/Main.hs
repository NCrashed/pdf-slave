module Main where

import Control.Monad.IO.Class
import Data.Monoid
import Data.Text (pack)
import Data.Yaml (decodeEither')
import Filesystem.Path.CurrentOS (directory)
import Options.Applicative
import Prelude hiding (FilePath)
import Shelly

import qualified Data.ByteString as BS

import Text.PDF.Slave.Template
import Text.PDF.Slave.Render

-- | Template mode either a all-in-one-file or located in distinct files
data TemplateMode = Bundle | Files
  deriving (Show, Read)

-- | CLI options
data Options = Options {
  -- | Path to template file, if missing, stdin is used
  templatePath  :: Maybe FilePath
  -- | Either full bundle or partially stored at files
, templateMode  :: TemplateMode
  -- | Path to output pdf file, if missing, stdout is used
, pdfOutputPath :: Maybe FilePath
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

-- | Execute PDF slave
pdfSlave :: Options -> IO ()
pdfSlave Options{..} = shelly $ do
  -- define base directory
  baseDir <- case templatePath of
    Nothing -> pwd
    Just p -> pure $ directory p
  -- read template
  templateContent <- case templatePath of
    Nothing -> liftIO $ BS.getContents
    Just p -> readBinary p
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
        tf <- storeTemplateInFiles t folder
        render tf
    Files -> case decodeEither' templateContent of
      Left e -> fail $ "Failed to parse template: " <> show e
      Right t -> render t


main :: IO ()
main = execParser opts >>= pdfSlave
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "CLI interface for PDF slave."
     <> header "pdf-slave - tool for building PDF documents from HTex templates" )