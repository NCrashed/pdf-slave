-- | Defines document template
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.PDF.Slave.Template(
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
  -- * Helpers
  , loadTemplateInMemory
  , storeTemplateInFiles
  ) where

import Control.Monad (mzero)
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text as T
import Data.Yaml
import Filesystem.Path.CurrentOS (directory)
import GHC.Generics
import Prelude hiding (FilePath)
import Shelly as Sh

import qualified Data.ByteString.Base64 as B64
import qualified Data.Map.Strict        as M
import qualified Data.Text.Encoding     as T

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

instance FromJSON TemplateDependency where
  parseJSON (Object o) = do
    depType <- o .: "type"
    case T.toLower . T.strip $ depType of
      "bibtex"       -> BibtexDep       <$> o .: "body"
      "template"     -> TemplateDep     <$> o .: "body"
      "template_pdf" -> TemplatePdfDep  <$> o .: "body"
      "other"        -> do
        t <- o .: "body"
        either (\e -> fail $ "Cannot decode dependency body (base64): " <> e) (return . OtherDep) $
          B64.decode . T.encodeUtf8 $ t
      _ -> fail $ "Unknown template type " <> unpack depType
  parseJSON _ = mzero

instance ToJSON TemplateDependency where
  toJSON d = case d of
    BibtexDep body -> object [
        "type" .= ("bibtex" :: Text)
      , "body" .= body
      ]
    TemplateDep body -> object [
        "type" .= ("template" :: Text)
      , "body" .= body
      ]
    TemplatePdfDep body -> object [
        "type" .= ("template_pdf" :: Text)
      , "body" .= body
      ]
    OtherDep bs -> object [
        "type" .= ("template_pdf" :: Text)
      , "body" .= (T.decodeUtf8 . B64.encode $ bs)
      ]

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

instance FromJSON Template where
  parseJSON (Object o) = Template
    <$> o .: "name"
    <*> o .: "input"
    <*> o .: "body"
    <*> o .: "dependencies"
  parseJSON _ = mzero

instance ToJSON Template where
  toJSON Template{..} = object [
      "name"         .= templateName
    , "input"        .= templateInput
    , "body"         .= templateBody
    , "dependencies" .= templateDeps
    ]

-- | Same as 'TemplateDependency' but keeps contents in separate files
data TemplateDependencyFile =
  -- | Bibtex file for references to other documents. Need call to bibtex
    BibtexDepFile FilePath
  -- | HTex file that need to be compiled to .tex file
  | TemplateDepFile TemplateFile
  -- | HTex file that need to be compiled to .pdf file
  | TemplatePdfDepFile TemplateFile
  -- | Any other file that doesn't need a compilation (listings, images, etc)
  | OtherDepFile FilePath
  deriving (Generic, Show)

instance FromJSON FilePath where
  parseJSON (String s) = return $ Sh.fromText s
  parseJSON _ = mzero

instance ToJSON FilePath where
  toJSON = String . Sh.toTextIgnore

instance FromJSON TemplateDependencyFile where
  parseJSON (Object o) = do
    depType <- o .: "type"
    case T.toLower . T.strip $ depType of
      "bibtex"       -> BibtexDepFile       <$> o .: "body"
      "template"     -> TemplateDepFile     <$> o .: "body"
      "template_pdf" -> TemplatePdfDepFile  <$> o .: "body"
      "other"        -> OtherDepFile        <$> o .: "body"
      _ -> fail $ "Unknown template type " <> unpack depType
  parseJSON _ = mzero

instance ToJSON TemplateDependencyFile where
  toJSON d = case d of
    BibtexDepFile body -> object [
        "type" .= ("bibtex" :: Text)
      , "body" .= body
      ]
    TemplateDepFile body -> object [
        "type" .= ("template" :: Text)
      , "body" .= body
      ]
    TemplatePdfDepFile body -> object [
        "type" .= ("template_pdf" :: Text)
      , "body" .= body
      ]
    OtherDepFile body -> object [
        "type" .= ("template_pdf" :: Text)
      , "body" .= body
      ]

-- | Same as 'Template', but holds info about template content and dependencies
-- in other files.
data TemplateFile = TemplateFile {
  -- | Template has human readable name
    templateFileName      :: TemplateName
  -- | Template expects input in YAML format. The field contains filename of the YAML file.
  , templateFileInput     :: FilePath
  -- | Template contents filename.
  , templateFileBody      :: FilePath
  -- | Template dependencies (bibtex, listings, other htex files)
  , templateFileDeps      :: M.Map TemplateName TemplateDependencyFile
  } deriving (Generic, Show)

instance FromJSON TemplateFile where
  parseJSON (Object o) = TemplateFile
    <$> o .: "name"
    <*> o .: "input"
    <*> o .: "body"
    <*> o .: "dependencies"
  parseJSON _ = mzero

instance ToJSON TemplateFile where
  toJSON TemplateFile{..} = object [
      "name"         .= templateFileName
    , "input"        .= templateFileInput
    , "body"         .= templateFileBody
    , "dependencies" .= templateFileDeps
    ]

-- | Load all external references of template into memory
loadTemplateInMemory :: TemplateFile -> Sh (Either ParseException Template)
loadTemplateInMemory TemplateFile{..} = do
  inputCnt <- readBinary templateFileInput
  body <- readfile templateFileBody
  deps <- traverse loadDep templateFileDeps
  return $ Template
    <$> pure templateFileName
    <*> decodeEither' inputCnt
    <*> pure body
    <*> sequence deps
  where
    loadDep d = case d of
      BibtexDepFile body -> do
        cnt <- readfile body
        return . pure $ BibtexDep cnt
      TemplateDepFile body -> do
        tmpl <- loadTemplateInMemory body
        return $ TemplateDep <$> tmpl
      TemplatePdfDepFile body -> do
        tmpl <- loadTemplateInMemory body
        return $ TemplatePdfDep <$> tmpl
      OtherDepFile body -> do
        cnt <- readBinary body
        return . pure $ OtherDep cnt

-- | Extract all external references of template into file system
storeTemplateInFiles :: Template -> FilePath -> Sh TemplateFile
storeTemplateInFiles Template{..} folder = do
  let inputName = folder </> templateName <> ("_name" :: FilePath) <.> "yaml"
  writeBinary inputName $ encode templateInput
  let bodyName = folder </> templateName <.> "htex"
  mkdir_p $ directory bodyName
  writefile bodyName templateBody
  deps <- M.traverseWithKey storeDep templateDeps
  return $ TemplateFile {
      templateFileName = templateName
    , templateFileInput = inputName
    , templateFileBody = bodyName
    , templateFileDeps = deps
    }
  where
    storeDep name d = case d of
      BibtexDep body -> do
        let bodyName = folder </> name <.> "bib"
        mkdir_p $ directory bodyName
        writefile bodyName body
        return $ BibtexDepFile bodyName
      TemplateDep template -> do
        let subfolderName = Sh.fromText name
        mkdir_p subfolderName
        dep <- storeTemplateInFiles template subfolderName
        return $ TemplateDepFile dep
      TemplatePdfDep template -> do
        let subfolderName = Sh.fromText name
        mkdir_p subfolderName
        dep <- storeTemplateInFiles template subfolderName
        return $ TemplatePdfDepFile dep
      OtherDep body -> do
        let bodyName = folder </> name
        mkdir_p $ directory bodyName
        writeBinary bodyName body
        return $ OtherDepFile bodyName
