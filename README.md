pdf-slave
=========

Tool that compiles `haskintex` (TeX with embedded Haskell) files into PDF documents.
Templates are described in YAML format and can define dependencies.

Features:

* Input JSON file for `htex` file that holds template varying data.

* Option for packing a template in all-in YAML bundle.

* Support for template dependencies that include:
  - Bibtex files
  - Images, listings, other static files.
  - Other `.htex` templates that compiles into TeX and includes into parent template.
  - Other `.htex` templates that compiles into PDF and includes into parent template.

Template reference
==================

Common template consists of several files:

* `template_input.json` - Input data for template in JSON format.

``` JSON
{
  "line-width": 2,
  "spiral-precision": 0.01,
  "spiral-interval": [0,4],
  "spiral-a": 0.1,
  "spiral-b": 4
}
```

* `template.htex` - TeX/LaTeX with embedded Haskell that reads input data from
file `template.json`. You have to provide code at the beginning of file that reads
the inputs, like that:

``` Haskell
\begin{document}

\begin{haskellpragmas}
{-# LANGUAGE OverloadedStrings #-}
\end{haskellpragmas}
\begin{writehaskell}
import Data.Aeson
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Lazy as BS

data Input = Input {
  lineWidth       :: Double
, spiralPrecision :: Double
, spiralInterval  :: (Double, Double)
, spiralA         :: Double
, spiralB         :: Double
}

instance FromJSON Input where
  parseJSON (Object o) = Input
    <$> o .: "line-width"
    <*> o .: "spiral-precision"
    <*> o .: "spiral-interval"
    <*> o .: "spiral-a"
    <*> o .: "spiral-b"

inpt :: Input
inpt = case unsafePerformIO $ fmap eitherDecode' $ BS.readFile "template_input.json" of
  Left e -> error (show e)
  Right a -> a

\end{writehaskell}
```

* `template.yaml` - description of template and its dependencies.

``` YAML
name:  template01             # name of template
input: template01_input.json  # name of input file
body:  template01.htex        # name of .htex file
dependencies: {}              # dependency tree (see below)
haskintex-opts: []            # additional flags to haskintex
```

## Dependencies

There are 4 different types of dependencies:

* `other` - static files that don't require any processing. They could be images,
listings etc. Example of YAML configuration:

  ``` YAML
  dependencies:
    lambda.png: # name of dependency is equal to filename relative to parent template
      type: other # marks type of dependency
    code/demo.hs:
      type: other
  ```

  See [examples/example02](https://github.com/NCrashed/pdf-slave/tree/master/examples/template02) for full example.

* `bibtex` - bibliography files that require additional passes with `bibtex`. Example of YAML configuration:

  ``` YAML
  dependencies:
    biblio.bib: # name of dependency is equal to filename relative to parent template
      type: bibtex # marks type of dependency
  ```

  See [examples/example03](https://github.com/NCrashed/pdf-slave/tree/master/examples/template03) for full example.

* `template` - other `.htex` templates that are included in parent via `\\input{...}` or `\\include{..}`. Example of YAML configuration:

  ``` YAML
  dependencies:
    dep1: # name of dependency defines subfolder where the output tex file is located
      type:  template # marks type of dependency
      name:  dep1
      input: dep1_input.json
      body:  dep1.htex
    dep2:
      type:  template
      name:  dep2
      body:  dep2.htex
      dependencies:
        lambda.png:
          type: other
        code/demo.hs:
          type: other
  ```

  See [examples/example04](https://github.com/NCrashed/pdf-slave/tree/master/examples/template04) for full example.

  **Note that `code/demo.hs` subdependency should be included as `dep2/code/demo.hs` in `dep2.htex` as `dep2.tex` is inlined into parent.**

* `template_pdf` - other `.htex` templates that are included as PDFs into parent template. Example of YAML configuration:

  ``` YAML
  dependencies:
    template01: # name of dependency defines subfolder where the output tex file is located
      type:  template_pdf # marks type of dependency
      name:  template01
      input: template01_input.json
      body:  template01.htex
    template02:
      type:  template_pdf
      name:  template02
      body:  template02.htex
      dependencies:
        lambda.png:
          type: other
        code/demo.hs:
          type: other
  ```

  See [examples/example05](https://github.com/NCrashed/pdf-slave/tree/master/examples/template05) for full example.

### Input propagation

When you work with dependencies you have two options how to handle inputs:

* Define dependency inputs in its own file:

  ``` YAML
  dependencies:
    dep1:
      type:  template
      name:  dep1
      input: dep1_input.json # private input file
      body:  dep1.htex
  ```

* Define dependency inputs in parent file:

  ``` YAML
  name:  template06
  input: template06_input.json # contains inputs for dep1
  body:  template06.htex
  dependencies:
    dep1:
      type:  template
      name:  dep1
      body:  dep1.htex
  ```

  Contents of `template06_input.json`:

  ``` JSON
  {
    "dep1": {
      "line-width": 2,
      "spiral-precision": 0.01,
      "spiral-interval": [0,4],
      "spiral-a": 0.1,
      "spiral-b": 4
    }
  }
  ```

  Note that key of subsection must be equal to name of dependency.

  See [examples/example06](https://github.com/NCrashed/pdf-slave/tree/master/examples/template06) for full example.

## Making bundles

One can pack all `.htex`, `.json`, `.yaml` and all dependencies in single YAML
bundle that can be easily distributed, transmitted between services and stored:

``` bash
cd examples/template01
pdf-slave --template template01.yaml --output template01_bundle.yaml pack
```

As modification of such bundles isn't handy, one can unpack bundle:

``` bash
pdf-slave --template template01_bundle.yaml --output template01_directory unpack
```

Rendering of bundles is handled with the same command that is used for ordinary
templates.

Compilation
===========

You need:

* LaTeX distribution (for instance, texlive or miktex)

* [stack](https://docs.haskellstack.org/en/stable/README/) or system wide GHC 8.0.1 + Cabal 1.24.0.

Compilation with stack:
``` bash
git clone https://github.com/NCrashed/pdf-slave.git
cd pdf-slave
stack install
```

Compilation with cabal:
``` bash
git clone https://github.com/NCrashed/pdf-slave.git
cd pdf-slave
cabal sandbox init
cabal install --dependencies-only
cabal install
```

Running examples
================

You need:

* LaTeX distribution (for instance, texlive or miktex)

* stack or GHC+Cabal (yes, you need GHC to evaluate templates at runtime)

* `pdf-slave` and `haskintex` executables in PATH

Stack users:
``` bash
cd examples/template01
stack install aeson HaTeX
stack exec -- pdf-slave --template template01.yaml --output output.pdf pdf && xdg-open output.pdf
```

Cabal users:
``` bash
cd examples/template01
cabal sandbox init
cabal install aeson HaTeX
pdf-slave --template template01.yaml --output output.pdf pdf && xdg-open output.pdf
```

Docker build
============

1. Run `cook_doocker.sh` script. This will build two images, one `pdf-slave-build` for compilation of binaries and the second one `pdf-slave` for production usage.

2. Usage:

  ``` bash
  docker run -it --rm -v $(pwd)/examples/template01:/data/examples pdf-slave pdf --template examples/template01.yaml --output examples/output.pdf
  xdg-open examples/template01/output.pdf
  ```

3. Or download [precompiled container](https://hub.docker.com/r/ncrashed/pdf-slave/) from Docker Hub.