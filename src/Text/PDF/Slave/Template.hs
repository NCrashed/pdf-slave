-- | Defines document template
module Text.PDF.Slave.Template(
    TemplateName
  , TemplateInput
  , TemplateBody
  , TemplateBibtex
  , DependencyBody
  , BibTexBody
  , TemplateDependency(..)
  , Template(..)
  ) where

import Data.Text
import Data.Yaml
import GHC.Generics
import Data.ByteString (ByteString)

import qualified Data.Map.Strict as M

-- | Template unique name
type TemplateName = Text

-- | A template takes simple YAML document as input
type TemplateInput = Value

-- | Template body is text with .htex content
type TemplateBody = Text

-- | Template can define additional bibtex database
type TemplateBibtex = Text

-- | Dependency can be a binary file
type DependencyBody = ByteString

-- | Content of bibtex file
type BibTexBody = Text

-- | Template has different types of dependencies, each type of the dependecy
-- has own affect on rendering pipe.
data TemplateDependency =
  -- | Bibtex file for references to other documents. Need call to bibtex
    BibtexDep BibTexBody
  -- | HTex file that need to be compiled to .tex file
  | TemplateDep Template
  -- | HTex file that need to be compiled to .pdf file
  | TemplatePdfDep Template
  -- | Any other file that doesn't need a compilation (listings, images, etc)
  | OtherDep DependencyBody
  deriving (Generic, Show)

-- | Description of document template
data Template = Template {
  -- | Template has human readable name
    templateName      :: TemplateName
  -- | Template expects input in YAML format
  , templateInput     :: TemplateInput
  -- | Template contents
  , templateBody      :: TemplateBody
  -- | Template dependencies (bibtex, listings, other htex files)
  , templateDeps      :: M.Map TemplateName TemplateDependency
  } deriving (Generic, Show)