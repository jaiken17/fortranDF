# fortranDF docs

The files within this directory consist of the documentation for fortranDF.

## Table of Contents
1. [Getting Started](#getting_started)
    1. [Installation](#installation)
    2. [Basic Usage](#basic_usage)
    3. [Limitations](#limitiations)
2. [`data_frame` type](#data_frame)
    1. [Advanced Usage](#df_adv_usage)
    2. [child types](#df_child_types)
    3. [procedures](#df_procedures) 
3. [`column` type](#column)
    1. [child types](#col_child_types)
    2. [procedures](#col_procedures)
4. [Utility Modules/Procedures](#utils)
    1. [paramters](#utils_parameters)
    2. [procedures](#utils_procedures)

## Getting Started <a name="getting_started"></a>

### Installation <a name="installation"></a>

To use as a dependency with [fpm](https://github.com/fortran-lang/fpm), simply add the line
```toml
fortranDF.git = "https://github.com/jaiken17/fortranDF"
```
under a `[dependencies]` tag in your `fpm.toml` package manifest. This repo will be automatically cloned into your project and built as a dependency.

Alternatively, you can clone this repo with 
```bash
git clone git@github.com/jaiken17/fortranDF
```
and build with
```bash
fpm build
```
at the root directory of the cloned repo.

**Note**: Compiling with certain versions of gfortran will cause segmentation fauls at runtime. This is a known bug with the compiler. Please see https://github.com/jaiken17/fortranDF/issues/1 for a workaround.

### Basic Usage <a name="basic_usage"></a>
In order to use this library, simply add `use df_fortranDF` to the use section of your program/module and then you can declare an object of `type(data_frame) :: df`.
This instance `df` of the `data_frame` class can then be initialized with 
```fortran
call df%new()
```
You can then either read from a file using 
```fortran
call df%read("filename.txt",.true.)
```
or by appending rank 1 arrays
```fortran
call df%append([1,2,3,4])
```
Both methods of creating `data_frame` instances supports the use of headers. 

Retrieval of data can be done through the use of 'getter' functions. A whol column can be retrieved by
```fortran
real,dimension(:),allocatable :: data
data = df%getr("real_data1")
```
or a single value
```fortran
integer :: nth_value
nth_value = df%geti("integer_data1",n)
```
Just make sure to use the correctly-typed getter function
```fortran
logical,dimension(:),allocatable :: data
data = df%getl("logical_data1")
data = df%getl("integer_data1") ! will not work if integer_data1 is integer type
``` 
There are similar procedures for replacing values/columns.

To deallocate the memory used by a `data_frame` object, simply use
```fortran
call df%destroy()
```
See section [2.3](#df_procedures) or [main.f90](https://github.com/jaiken17/fortranDF/app/main.f90) for more information and examples.

### Limitations <a name="limitations"></a>

Currently, the `data_frame` class is limited to storing and retrieving data. There are plans to allow direct calculations on columns (ie dot product, sum, mean, etc.) however, this can be done simply enough with the getter functions or through class extensions.

I/O is a major limitation at the moment. There is only one format for output that generally is not nice for large data sets. Output of complex-typed columns is completely unsupported. There is only one useable input format, which works weel enough, but does not support the input of strings with spaces.
For now, I/O is relegated to extension classes. See section [2.1](#df_adv_usage) and [fortanMR](https://github.com/jaiken17/fortranMR) for how to develop user-defined I/O procedures.

## The `data_frame` type <a name="data_frame"></a>


## The `column` type <a name="column"></a>


## Utility Modules/Procedures <a name="utils"></a>