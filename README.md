# Guillottine

Guillottine is a simple [guillottine problem](https://en.wikipedia.org/wiki/Guillotine_problem) solver.

Written in Object Pascal language (objfpc), to compile the source code FPC (min. 3.0.0RC) and Lazarus are required.

## Features

* Basic GUI
* Save the picture of the solution
* SimpleSolver
  * More or less based on: [url](http://www.amzi.com/articles/papercutter.htm)
    (however the current solver couldn't find the layout presented in Figure 4.)
  * With some basic heuristics
  * Supports rotatable pieces
  * Supports multiple spaces (sheets)
* Save list of pieces/spaces to csv file

## TODO

* Source code cleanup, dev-doc
* Implement better solver algorithms? ([article](http://www.hindawi.com/journals/aor/2009/732010/))
* Support for more solvers
  * E.g. solvers with multicore CPU support, etc.
  * Solvers as separate binaries / dlls?
* Translations
* Result display improvements:
  * Other (preferably vector) formats (good fpc pdf package?)
  * Legend / scale
* Store and display best non-solution (e.g. with highest area coverage)

## License

MIT License