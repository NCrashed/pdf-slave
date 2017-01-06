-- | Rendering of templates
module Text.PDF.Slave.Render(
    PDFContent
  , PDFRenderException(..)
  , displayPDFRenderException
  , renderBundleOrTemplateFromFile
  , renderFromFileBundleToPDF
  , renderFromFileToPDF
  , renderBundleToPDF
  , renderTemplateToPDF
  , loadTemplateInMemory
  , storeTemplateInFiles
  -- * Low-level
  , DepFlags
  , DepFlag(..)
  , renderPdfTemplate
  , renderTemplate
  , renderTemplateDep
  , parseBundleOrTemplate
  , parseBundleOrTemplateFromFile
  ) where

import Control.Monad (join, forM_)
import Control.Monad.Catch
import Data.Aeson (Value(..))
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Set (Set)
import Data.Yaml (ParseException, decodeEither')
import Filesystem.Path (dropExtension, directory)
import GHC.Generics
import Prelude hiding (FilePath)
import Shelly

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BZ
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Text.PDF.Slave.Template

-- | Contents of PDF file
type PDFContent = ByteString

-- | Errors that are thrown by rendering functions
data PDFRenderException =
    TemplateFormatError FilePath ParseException -- ^ Failed to parse template YAML
  | BundleFormatError FilePath ParseException -- ^ Failed to parse template bundle YAML
  -- | Failed to parse file in both formats: bundle and template file.
  | BundleOrTemplateFormatError FilePath ParseException ParseException
  | InputFileFormatError FilePath String -- ^ Failed to parse JSON input
  deriving (Generic, Show)

instance Exception PDFRenderException

-- | Convert PDF rendering exception to user readable format
displayPDFRenderException :: PDFRenderException -> String
displayPDFRenderException e = case e of
  TemplateFormatError f pe -> "Failed to parse template file " <> show f
    <> ", reason: " <> show pe
  BundleFormatError f pe -> "Failed to parse template bundle file " <> show f
    <> ", reason: " <> show pe
  BundleOrTemplateFormatError f peBundle peTemplate -> "Failed to parse template file " <> show f <> ". "
    <> "\n Tried bundle format: " <> show peBundle
    <> "\n Tried template format: " <> show peTemplate
  InputFileFormatError f es -> "Failed to parse template input file " <> show f
    <> ", reason: " <> show es

-- | Helper to render either a bundle or distributed template from file to PDF.
renderBundleOrTemplateFromFile ::
     FilePath -- ^ Path to either bundle 'Template' or template 'TemplateFile'
  -> Maybe Value -- ^ Overwrite of input JSON for bundle
  -> Sh PDFContent
renderBundleOrTemplateFromFile filename bundleInput = do
  res <- parseBundleOrTemplateFromFile filename
  let baseDir = directory filename
  case res of
    Left bundle -> do
      let bundle' = fromMaybe bundle $ fmap (\i -> bundle { templateInput = Just i }) bundleInput
      renderBundleToPDF bundle'
    Right template -> renderTemplateToPDF template baseDir

-- | Try to parse either a bundle or template file
parseBundleOrTemplateFromFile :: FilePath -- ^ Path to either 'Template' or 'TemplateFile'
  -> Sh (Either Template TemplateFile)
parseBundleOrTemplateFromFile filename =
  parseBundleOrTemplate filename =<< readBinary filename

-- | Try to parse either a bundle or template file
parseBundleOrTemplate :: FilePath -- ^ Source of data (file or stdin, etc)
  -> ByteString -- ^ Contents of either 'Template' or 'TemplateFile'
  -> Sh (Either Template TemplateFile)
parseBundleOrTemplate filename cnt = case decodeEither' cnt of
  Right bundle -> return $ Left bundle
  Left eBundle -> case decodeEither' cnt of
    Right template -> return $ Right template
    Left eTemplate -> throwM $ BundleOrTemplateFormatError filename eBundle eTemplate

-- | Helper to render from all-in bundle template
renderFromFileBundleToPDF :: FilePath -- ^ Path to 'Template' all-in bundle
  -> Maybe Value -- ^ Overwrite of input JSON for bundle
  -> Sh PDFContent
renderFromFileBundleToPDF filename bundleInput = do
  cnt <- readBinary filename
  case decodeEither' cnt of
    Left e -> throwM $ BundleFormatError filename e
    Right bundle -> do
      let bundle' = fromMaybe bundle $ fmap (\i -> bundle { templateInput = Just i }) bundleInput
      renderBundleToPDF bundle'

-- | Helper to render from template file
renderFromFileToPDF :: FilePath -- ^ Path to 'TemplateFile'
  -> Sh PDFContent
renderFromFileToPDF filename = do
  cnt <- readBinary filename
  case decodeEither' cnt of
    Left e -> throwM $ TemplateFormatError filename e
    Right template -> renderTemplateToPDF template (directory filename)

-- | Unpack bundle, render the template, cleanup and return PDF
renderBundleToPDF :: Template -- ^ Input all-in template
  -> Sh PDFContent
renderBundleToPDF bundle = withTmpDir $ \unpackDir -> do
  template <- storeTemplateInFiles bundle unpackDir
  renderTemplateToPDF template unpackDir

-- | Render template and return content of resulted PDF file
renderTemplateToPDF :: TemplateFile -- ^ Input template
  -> FilePath -- ^ Base directory
  -> Sh PDFContent -- ^ Output PDF file
renderTemplateToPDF t@TemplateFile{..} baseDir = withTmpDir $ \outputFolder -> do
  -- Parse global input file and pass it as inherited input
  minput <- case templateFileInput of
    Nothing -> return Nothing
    Just inputName -> do
      let inputNamePath = fromText inputName
      cnt <- readBinary (baseDir </> inputNamePath)
      case A.eitherDecode' . BZ.fromStrict $ cnt of
        Left e -> throwM $ InputFileFormatError inputNamePath e
        Right a -> return $ Just a
  renderPdfTemplate minput t baseDir outputFolder
  readBinary (outputFolder </> templateFileName <.> "pdf")

-- | Low-level render of template from .htex to .pdf that is recursively used for dependencies
renderPdfTemplate :: Maybe Value -- ^ Inherited input from parent
  -> TemplateFile -- ^ Template to render
  -> FilePath -- ^ Base directory
  -> FilePath -- ^ Output folder
  -> Sh ()
renderPdfTemplate minput t@TemplateFile{..} baseDir outputFolder = do
  flags <- renderTemplate minput t baseDir outputFolder
  -- define commands of compilation pipe
  let pdflatex = bash "pdflatex" [
          "-synctex=1"
        , "-interaction=nonstopmode"
        , toTextArg $ templateFileName <.> "tex" ]
      bibtex = bash "bibtex" [
          toTextArg $ templateFileName <.> "aux" ]
  -- read flags and construct pipe
  chdir outputFolder $ do
    _ <- if S.member NeedBibtex flags
      then pdflatex -|- bibtex -|- pdflatex -|- pdflatex
      else pdflatex
    return ()

-- | Low-level render of template from .htex to .tex that is recursively used for dependencies
renderTemplate :: Maybe Value -- ^ Inherited input from parent
  -> TemplateFile -- ^ Template to render
  -> FilePath -- ^ Base directory
  -> FilePath -- ^ Output folder
  -> Sh DepFlags -- ^ Flags that affects compilation upper in the deptree
renderTemplate minput TemplateFile{..} baseDir outputFolder = do
  mkdir_p outputFolder
  let renderDepenency = renderTemplateDep minput baseDir outputFolder
  depFlags <- M.traverseWithKey renderDepenency templateFileDeps
  let
      bodyName = dropExtension (fromText templateFileBody)
      haskintex = bash "haskintex" $ [
          "-overwrite"
        , "-verbose"
        , "-werror"
        , toTextArg $ baseDir </> bodyName ]
        ++ templateFileHaskintexOpts
  -- input file might be missing, if missing we can inject input from parent
  let outputFixedInputName = outputFolder </> (("input" :: FilePath) <.> "json")
  case templateFileInput of
    Nothing -> whenJust minput $ \input -> do
      writeBinary outputFixedInputName $ BZ.toStrict . A.encode $ input
    Just inputName -> do
      let inputPath = baseDir </> inputName
      -- copy in two places as the user might expect that input name would be equal
      -- to specified in template file and copy to standard place in case of unpacked
      -- bundle behavior.
      let outputInputPaths = [outputFolder </> inputName, outputFixedInputName]
      forM_ outputInputPaths $ cp inputPath
  _ <- chdir outputFolder haskintex
  return $ F.foldMap id depFlags -- merge flags

-- | Collected dependency markers (for instance, that we need bibtex compilation)
type DepFlags = Set DepFlag

-- | Dependency marker that is returned from 'renderTemplateDep'
data DepFlag = NeedBibtex -- ^ We need a bibtex compliation
  deriving (Generic, Show, Ord, Eq)

-- | Render template dependency
renderTemplateDep :: Maybe Value -- ^ Inherited input from parent
  -> FilePath -- ^ Base directory
  -> FilePath  -- ^ Output folder
  -> TemplateName -- ^ Dependency name
  -> TemplateDependencyFile -- ^ Dependency type
  -> Sh DepFlags
renderTemplateDep minput baseDir outputFolder name dep = case dep of
  BibtexDepFile -> do
    let file = fromText name
        outputFile = outputFolder </> file
    mkdir_p (directory outputFile)
    cp (baseDir </> file) outputFile
    return $ S.singleton NeedBibtex
  TemplateDepFile template -> do
    let subFolder = baseDir </> fromText name
        outputSubFolder = outputFolder </> fromText name
    renderTemplate minput' template subFolder outputSubFolder
  TemplatePdfDepFile template -> do
    let subFolder = baseDir </> fromText name
        outputSubFolder = outputFolder </> fromText name
    renderPdfTemplate minput' template subFolder outputSubFolder
    return mempty
  OtherDepFile -> do
    let file = fromText name
        outputFile = outputFolder </> file
    mkdir_p (directory outputFile)
    cp (baseDir </> file) outputFile
    return mempty
  where
    -- Try to find subsection in input that refer to the dependency
    minput' :: Maybe Value
    minput' = join $ flip fmap minput $ \case
      Object o -> H.lookup name o
      _ -> Nothing

-- | Same as 'when', but for 'Just' values.
whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust Nothing _ = pure ()
whenJust (Just a) f = f a

-- | Load all external references of template into memory
loadTemplateInMemory :: TemplateFile -> FilePath -> Sh (Either String Template)
loadTemplateInMemory TemplateFile{..} baseDir = do
  inputCnt <- case templateFileInput of
    Nothing -> return $ Right Nothing
    Just fname -> do
      cnt <- readBinary $ baseDir </> fromText fname
      return $ fmap Just . A.eitherDecode' . BZ.fromStrict $ cnt
  body <- readfile $ baseDir </> fromText templateFileBody
  deps <- M.traverseWithKey loadDep templateFileDeps
  return $ Template
    <$> pure templateFileName
    <*> inputCnt
    <*> pure body
    <*> sequence deps
    <*> pure templateFileHaskintexOpts
  where
    loadDep name d = let
      filename = baseDir </> fromText name
      in case d of
        BibtexDepFile -> do
          cnt <- readfile filename
          return . pure $ BibtexDep cnt
        TemplateDepFile body -> do
          tmpl <- loadTemplateInMemory body filename
          return $ TemplateDep <$> tmpl
        TemplatePdfDepFile body -> do
          tmpl <- loadTemplateInMemory body filename
          return $ TemplatePdfDep <$> tmpl
        OtherDepFile -> do
          cnt <- readBinary filename
          return . pure $ OtherDep cnt

-- | Extract all external references of template into file system
storeTemplateInFiles :: Template -> FilePath -> Sh TemplateFile
storeTemplateInFiles Template{..} folder = do
  mkdir_p folder
  relInputName <- case templateInput of
    Nothing -> return Nothing
    Just input -> do
      let inputName = folder </> ("input" :: FilePath) <.> "json"
      writeBinary inputName $ BZ.toStrict $ A.encode input
      fmap Just $ relativeTo folder inputName
  let bodyName = folder </> templateName <.> "htex"
  mkdir_p $ directory bodyName
  writefile bodyName templateBody
  relBodyName <- relativeTo folder bodyName
  deps <- M.traverseWithKey storeDep templateDeps
  return $ TemplateFile {
      templateFileName = templateName
    , templateFileInput = fmap toTextIgnore relInputName
    , templateFileBody = toTextIgnore relBodyName
    , templateFileDeps = deps
    , templateFileHaskintexOpts = templateHaskintexOpts
    }
  where
    storeDep name d = case d of
      BibtexDep body -> do
        let bodyName = folder </> name
        mkdir_p $ directory bodyName
        writefile bodyName body
        return BibtexDepFile
      TemplateDep template -> do
        let subfolderName = folder </> fromText name
        mkdir_p subfolderName
        dep <- storeTemplateInFiles template subfolderName
        return $ TemplateDepFile dep
      TemplatePdfDep template -> do
        let subfolderName = folder </> fromText name
        mkdir_p subfolderName
        dep <- storeTemplateInFiles template subfolderName
        return $ TemplatePdfDepFile dep
      OtherDep body -> do
        let bodyName = folder </> name
        mkdir_p $ directory bodyName
        writeBinary bodyName body
        return OtherDepFile
