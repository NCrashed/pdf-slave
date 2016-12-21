-- | Reexports
module Text.PDF.Slave(
  -- * Template definition
    TemplateName
  , TemplateInput
  , TemplateBody
  , TemplateBibtex
  , DependencyBody
  , BibTexBody
  , TemplateDependency(..)
  , Template(..)
  , TemplateDependencyFile(..)
  , TemplateFile(..)
  -- ** Bundle helpers
  , loadTemplateInMemory
  , storeTemplateInFiles
  -- * Template rendering to PDF
  , PDFContent
  , PDFRenderException(..)
  , displayPDFRenderException
  , renderBundleOrTemplateFromFile
  , renderFromFileBundleToPDF
  , renderFromFileToPDF
  , renderBundleToPDF
  , renderTemplateToPDF
  ) where

import Text.PDF.Slave.Render
import Text.PDF.Slave.Template