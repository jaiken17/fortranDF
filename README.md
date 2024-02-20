# fortranDF

`fortranDF` is a data frame library for use in modern Fortran programs.

## Features
- can be built with fpm
- `data_frame` data type
- columns can be any intrinsic type
- columns do not all need to be the same type
- columns can be modified or retrieved through the use of headers

## Documentation
See https://github.com/jaiken17/fortranDF/blob/main/docs/docs.md for documentation and getting started.

## Future Plans
- allow columns of different kinds
- add direct mathematics procedures (average, sum, etc.)
- improve IO capabilites (read/write CSV files specifically)

## Dependencies
`split_mod.f90` is a modified version of procedures from [urbanjost/M_strings](https://github.com/urbanjost/M_strings)
