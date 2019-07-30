# f_tomlc99
Fortran interface for cktan's [tomlc99](https://github.com/cktan/tomlc99) library. The interface seems to work, but hasn't been tested extensively, so please use it at your own risk. Suggestions and fixes are welcome. 

See the `example_program.f90` file for available facilities. Note that timestamp and utf8 support have not yet been implemented. One-line compilation with:

`$FC tomlc99.f90 example_program.f90 /path/to/libtoml.a`
