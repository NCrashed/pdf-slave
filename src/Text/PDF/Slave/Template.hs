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

import qualified Data.Aeson             as A
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy   as BZ
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
  parseJSON val@(Object o) = do
    depType <- o .: "type"
    case T.toLower . T.strip $ depType of
      "bibtex"       -> BibtexDep       <$> o .: "body"
      "template"     -> TemplateDep     <$> parseJSON val
      "template_pdf" -> TemplatePdfDep  <$> parseJSON val
      "other"        -> do
        (t :: Text) <- o .: "body"
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
    TemplateDep body -> let
      Object o1 = object [ "type" .= ("template" :: Text) ]
      Object o2 = toJSON body
      in Object (o1 <> o2)
    TemplatePdfDep body -> let
      Object o1 = object [ "type" .= ("template_pdf" :: Text) ]
      Object o2 = toJSON body
      in Object (o1 <> o2)
    OtherDep bs -> object [
        "type" .= ("other" :: Text)
      , "body" .= (T.decodeUtf8 . B64.encode $ bs)
      ]

-- | Description of document template
data Template = Template {
  -- | Template has human readable name
    templateName          :: TemplateName
  -- | Template expects input in YAML format
  , templateInput         :: TemplateInput
  -- | Template contents
  , templateBody          :: TemplateBody
  -- | Template dependencies (bibtex, listings, other htex files)
  , templateDeps          :: M.Map TemplateName TemplateDependency
  -- | Additional flags for `haskintex`
  , templateHaskintexOpts :: [Text]
  } deriving (Generic, Show)

instance FromJSON Template where
  parseJSON (Object o) = Template
    <$> o .: "name"
    <*> o .: "input"
    <*> o .: "body"
    <*> o .:? "dependencies" .!= mempty
    <*> o .:? "haskintex-opts" .!= mempty
  parseJSON _ = mzero

instance ToJSON Template where
  toJSON Template{..} = object [
      "name"           .= templateName
    , "input"          .= templateInput
    , "body"           .= templateBody
    , "dependencies"   .= templateDeps
    , "haskintex-opts" .= templateHaskintexOpts
    ]

-- | Same as 'TemplateDependency' but keeps contents in separate files
data TemplateDependencyFile =
  -- | Bibtex file for references to other documents. Need call to bibtex.
  -- Name of dependency is a filename with contents.
    BibtexDepFile
  -- | HTex file that need to be compiled to .tex file
  -- Name of dependency defines a subfolder for the template.
  | TemplateDepFile TemplateFile
  -- | HTex file that need to be compiled to .pdf file
  -- Name of dependency deinfes a subfolder for the template.
  | TemplatePdfDepFile TemplateFile
  -- | Any other file that doesn't need a compilation (listings, images, etc)
  -- Name of dependency is a filename with contents.
  | OtherDepFile
  deriving (Generic, Show)

instance FromJSON FilePath where
  parseJSON (String s) = return $ Sh.fromText s
  parseJSON _ = mzero

instance ToJSON FilePath where
  toJSON = String . Sh.toTextIgnore

instance FromJSON TemplateDependencyFile where
  parseJSON val@(Object o) = do
    depType <- o .: "type"
    case T.toLower . T.strip $ depType of
      "bibtex"       -> pure BibtexDepFile
      "template"     -> TemplateDepFile     <$> parseJSON val
      "template_pdf" -> TemplatePdfDepFile  <$> parseJSON val
      "other"        -> pure OtherDepFile
      _ -> fail $ "Unknown template type " <> unpack depType
  parseJSON _ = mzero

instance ToJSON TemplateDependencyFile where
  toJSON d = case d of
    BibtexDepFile -> object [
        "type" .= ("bibtex" :: Text)
      ]
    TemplateDepFile body -> let
      Object o1 = object [ "type" .= ("template" :: Text) ]
      Object o2 = toJSON body
      in Object (o1 <> o2)
    TemplatePdfDepFile body -> let
      Object o1 = object [ "type" .= ("template_pdf" :: Text) ]
      Object o2 = toJSON body
      in Object (o1 <> o2)
    OtherDepFile -> object [
        "type" .= ("other" :: Text)
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
  -- | Additional flags for `haskintex`
  , templateFileHaskintexOpts :: [Text]
  } deriving (Generic, Show)

instance FromJSON TemplateFile where
  parseJSON (Object o) = TemplateFile
    <$> o .: "name"
    <*> o .: "input"
    <*> o .: "body"
    <*> o .:? "dependencies" .!= mempty
    <*> o .:? "haskintex-opts" .!= mempty
  parseJSON _ = mzero

instance ToJSON TemplateFile where
  toJSON TemplateFile{..} = object [
      "name"           .= templateFileName
    , "input"          .= templateFileInput
    , "body"           .= templateFileBody
    , "dependencies"   .= templateFileDeps
    , "haskintex-opts" .= templateFileHaskintexOpts
    ]

-- | Load all external references of template into memory
loadTemplateInMemory :: TemplateFile -> Sh (Either String Template)
loadTemplateInMemory TemplateFile{..} = do
  inputCnt <- readBinary templateFileInput
  body <- readfile templateFileBody
  deps <- M.traverseWithKey loadDep templateFileDeps
  return $ Template
    <$> pure templateFileName
    <*> A.eitherDecode' (BZ.fromStrict inputCnt)
    <*> pure body
    <*> sequence deps
    <*> pure templateFileHaskintexOpts
  where
    loadDep name d = let
      filename = fromText name
      in case d of
        BibtexDepFile -> do
          cnt <- readfile filename
          return . pure $ BibtexDep cnt
        TemplateDepFile body -> do
          tmpl <- chdir filename $ loadTemplateInMemory body
          return $ TemplateDep <$> tmpl
        TemplatePdfDepFile body -> do
          tmpl <- chdir filename $ loadTemplateInMemory body
          return $ TemplatePdfDep <$> tmpl
        OtherDepFile -> do
          cnt <- readBinary filename
          return . pure $ OtherDep cnt

-- | Extract all external references of template into file system
storeTemplateInFiles :: Template -> FilePath -> FilePath -> Sh TemplateFile
storeTemplateInFiles Template{..} baseDir folder = do
  mkdir_p folder
  let inputName = folder </> (templateName <> "_input") <.> "json"
  writeBinary inputName $ BZ.toStrict $ A.encode templateInput
  relInputName <- relativeTo baseDir inputName
  let bodyName = folder </> templateName <.> "htex"
  mkdir_p $ directory bodyName
  writefile bodyName templateBody
  relBodyName <- relativeTo baseDir bodyName
  deps <- M.traverseWithKey storeDep templateDeps
  return $ TemplateFile {
      templateFileName = templateName
    , templateFileInput = relInputName
    , templateFileBody = relBodyName
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
        let subfolderName = Sh.fromText name
        mkdir_p subfolderName
        dep <- storeTemplateInFiles template baseDir subfolderName
        return $ TemplateDepFile dep
      TemplatePdfDep template -> do
        let subfolderName = Sh.fromText name
        mkdir_p subfolderName
        dep <- storeTemplateInFiles template baseDir subfolderName
        return $ TemplatePdfDepFile dep
      OtherDep body -> do
        let bodyName = folder </> name
        mkdir_p $ directory bodyName
        writeBinary bodyName body
        return OtherDepFile
