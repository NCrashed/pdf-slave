1.3.1.0
=======

* Added flag `json` to output bundles in JSON format instead of YAML.

1.3.0.0
=======

* Default input file name is now `input.json`, not `<template name>_input.json`.

* Changed recommended way of reading input via `Helper.hs`

* Added flag `preserve-temp` to not nuke temporary files after execution.

1.2.3.0
=======

* Support compilation with GHC 7.10.

1.2.2.0
=======

* Always copy input file to `<template_name>_input.json`.

1.2.1.0
=======

* Execute `haskintex` with `-werror` flag.

1.2.0.0
=======

* Add `version` command to CLI.

* Fix: `renderBundleToPDF` doesn't take base directory.

* Fix: `loadTemplateInMemory` now takes base directory.

* Strong distinguish between bundle and ordinary template format. Bundle templates
have to define `bundle: true` inside the YAML files.

1.1.0.0
=======

* Factor out `pdf-slave-template` package for compiler agnostic format definition.

1.0.1.0
=======

* Added missing reexports into head module.

1.0.0.0
=======

* Initial release.
