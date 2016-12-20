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
stack exec -- pdf-slave --template template01.yaml --output output.pdf pdf && xdg-open output.pdf
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