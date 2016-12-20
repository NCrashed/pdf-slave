pdf-slave
=========

Tool that compiles `haskintex` (TeX with embedded Haskell) files into PDF documents.
Templates are described in YAML format and can define dependencies.

Features:

* Input JSON file for `htex` file that holds template varying data.

* Option for packing a template in all-in YAML bundle.

* Support for template dependencies that include:
  - Bibtex files
  - Images, other static files.
  - Other `.htex` templates
  - Other `.htex` templates that compiles into PDF and included in parent template.

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

TODO: describe dependency tree format

## Making bundles

One can pack all `.htex`, `.json`, `.yaml` and all dependencies in single YAML
bundle that can be easily distributed, transmitted between services and stored:

```
cd examples
pdf-slave --template template01.yaml --output template01_bundle.yaml pack
```

As modification of such bundles isn't handy, one can unpack bundle:

```
pdf-slave --template template01_bundle.yaml --output template01_directory unpack
```

Compilation
===========

You need:

* LaTeX distribution (for instance, texlive or miktex)

* [stack](https://docs.haskellstack.org/en/stable/README/) or system wide GHC 8.0.1 + Cabal 1.24.0.

Compilation with stack:
```
git clone https://github.com/NCrashed/pdf-slave.git
cd pdf-slave
stack install
```

Compilation with cabal:
```
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
```
cd examples
stack install aeson HaTeX
stack exec -- pdf-slave --template template01.yaml --output output.pdf pdf && xdg-open output.pdf
```

Cabal users:
```
cd examples
cabal sandbox init
cabal install aeson HaTeX
pdf-slave --template template01.yaml --output output.pdf pdf && xdg-open output.pdf
```

Docker build
============

1. Run `cook_doocker.sh` script. This will build two images, one `pdf-slave-build` for compilation of binaries and the second one `pdf-slave` for production usage.

2. Usage:

```
docker run -it --rm -v $(pwd)/examples:/data/examples pdf-slave pdf --template examples/template01.yaml --output examples/output.pdf
xdg-open examples/output.pdf
```

3. TODO: Upload to docker hub