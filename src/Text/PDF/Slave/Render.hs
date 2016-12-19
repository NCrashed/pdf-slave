-- | Rendering of templates
module Text.PDF.Slave.Render(
    PDFContent
  , renderTemplateFile
  -- * Low-level
  , DepFlags
  , DepFlag(..)
  , renderPdfTemplate
  , renderTemplate
  , renderTemplateDep
  ) where

import Data.ByteString (ByteString)
import Data.Set (Set)
import Filesystem.Path.CurrentOS (directory)
import GHC.Generics
import Prelude hiding (FilePath)
import Shelly

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Text.PDF.Slave.Template

-- | Contents of PDF file
type PDFContent = ByteString

-- | Render template and return content of resulted PDF file
renderTemplateFile :: Template -- ^ Input template
  -> Sh PDFContent -- ^ Output PDF file
renderTemplateFile t@Template{..} = withTmpDir $ \outputFolder -> do
  renderPdfTemplate t outputFolder
  readBinary (outputFolder </> templateName <.> "pdf")

-- | Low-level render of template from .htex to .pdf that is recursively used for dependencies
renderPdfTemplate :: Template -- ^ Template to render
  -> FilePath -- ^ Output folder
  -> Sh ()
renderPdfTemplate t@Template{..} outputFolder = do
  flags <- renderTemplate t outputFolder
  -- define commands of compilation pipe
  let pdflatex = bash "pdflatex" [
          "-synctex=1"
        , "-interaction=nonstopmode"
        , toTextArg $ outputFolder </> templateName <.> "tex" ]
      bibtex = bash "bibtex" [
          toTextArg $ outputFolder </> templateName <.> "aux" ]
  -- read flags and construct pipe
  _ <- if S.member NeedBibtex flags
    then pdflatex -|- bibtex -|- pdflatex
    else pdflatex
  return ()

-- | Low-level render of template from .htex to .tex that is recursively used for dependencies
renderTemplate :: Template -- ^ Template to render
  -> FilePath -- ^ Output folder
  -> Sh DepFlags -- ^ Flags that affects compilation upper in the deptree
renderTemplate Template{..} outputFolder = do
  let htexFile = outputFolder </> templateName <.> "htex"
  writefile htexFile templateBody
  depFlags <- M.traverseWithKey (renderTemplateDep outputFolder) templateDeps
  _ <- bash "haskintex" [toTextArg htexFile]
  return $ F.foldMap id depFlags -- merge flags

-- | Collected dependency markers (for instance, that we need bibtex compilation)
type DepFlags = Set DepFlag

-- | Dependency marker that is returned from 'renderTemplateDep'
data DepFlag = NeedBibtex -- ^ We need a bibtex compliation
  deriving (Generic, Show, Ord, Eq)

-- | Render template dependency
renderTemplateDep :: FilePath  -- ^ Output folder
  -> TemplateName -- ^ Name of dependency (includes file extension)
  -> TemplateDependency -- ^ Dependency type
  -> Sh DepFlags
renderTemplateDep outputFolder name dep = case dep of
  BibtexDep bibContent -> do
    let filename = outputFolder </> name
    mkdir_p $ directory filename
    writefile filename bibContent
    return $ S.singleton NeedBibtex
  TemplateDep template -> renderTemplate template outputFolder
  TemplatePdfDep template -> do
    renderPdfTemplate template outputFolder
    return mempty
  OtherDep cnt -> do
    let filename = outputFolder </> name
    mkdir_p $ directory filename
    writeBinary filename cnt
    return mempty