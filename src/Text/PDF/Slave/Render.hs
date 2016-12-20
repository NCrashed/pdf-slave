-- | Rendering of templates
module Text.PDF.Slave.Render(
    PDFContent
  , renderTemplateToPDF
  -- * Low-level
  , DepFlags
  , DepFlag(..)
  , renderPdfTemplate
  , renderTemplate
  , renderTemplateDep
  ) where

import Control.Monad (join)
import Data.Aeson (Value(..))
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Set (Set)
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

-- | Render template and return content of resulted PDF file
renderTemplateToPDF :: TemplateFile -- ^ Input template
  -> FilePath -- ^ Base directory
  -> Sh PDFContent -- ^ Output PDF file
renderTemplateToPDF t@TemplateFile{..} baseDir = withTmpDir $ \outputFolder -> do
  -- Parse global input file and pass it as inherited input
  minput <- case templateFileInput of
    Nothing -> return Nothing
    Just inputName -> do
      cnt <- readBinary inputName
      case A.eitherDecode' . BZ.fromStrict $ cnt of
        Left e -> fail $ "Cannot decode input file: " <> show e
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
      bodyName = dropExtension templateFileBody
      haskintex = bash "haskintex" $ [
          "-overwrite"
        , "-verbose"
        , toTextArg $ baseDir </> bodyName ]
        ++ templateFileHaskintexOpts
  -- input file might be missing, if missing we can inject input from parent
  case templateFileInput of
    Nothing -> whenJust minput $ \input -> do
      let filename = outputFolder </> (templateFileName <> "_input") <.> "json"
      writeBinary filename $ BZ.toStrict . A.encode $ input
    Just inputName -> do
      let inputPath = baseDir </> inputName
      cp inputPath $ outputFolder </> inputName
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