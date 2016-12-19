pdf-slave
=========

Work in progress.

Running examples
================

You need  installed and stack:

* LaTeX distribution (for instance, texlive or miktex)

* stack

* `pdf-slave` and `haskintex` executables in PATH

```
cd examples
stack install aeson HaTeX
stack exec -- pdf-slave --template template01.yaml --output pdf.pdf pdf && xdg-open pdf.pdf
```