# Guillottine

Guillottine is a simple [guillottine problem](https://en.wikipedia.org/wiki/Guillotine_problem) solver.

## Features

* Basic GUI
* Save the picture of the solution
* SimpleSolver
  * More or less based on: [url](http://www.amzi.com/articles/papercutter.htm)
    (however our current solver couldn't find the layout presented in Figure 4.)
  * With some basic heuristics
  * Supports rotatable pieces
  * Supports multiple spaces (sheets)
* Save list of pieces/spaces to csv file

## TODO

* Source code cleanup, dev-doc
* Implement better solver algorithms? ([article](http://www.hindawi.com/journals/aor/2009/732010/))
* Support for more solvers
  * E.g. for solvers with multicore CPU support, etc.
  * Solvers as separate binaries / dlls?
* Translations

## License

MIT License