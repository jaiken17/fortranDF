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
See [section 2.3](#df_procedures) or [main.f90](https://github.com/jaiken17/fortranDF/app/main.f90) for more information and examples.

### Limitations <a name="limitations"></a>

Currently, the `data_frame` class is limited to storing and retrieving data. There are plans to allow direct calculations on columns (ie dot product, sum, mean, etc.) however, this can be done simply enough with the getter functions or through class extensions.

I/O is a major limitation at the moment. There is only one format for output that generally is not nice for large data sets. Output of complex-typed columns is completely unsupported. There is only one useable input format, which works weel enough, but does not support the input of strings with spaces.
For now, I/O is relegated to extension classes. See [section 2.1](#df_adv_usage) and [fortanMR](https://github.com/jaiken17/fortranMR) for how to develop user-defined I/O procedures.

## The `data_frame` type <a name="data_frame"></a>

The `data_frame` type is the heart of `fortranDF`. It is the main data container and contains all the relevant procedures/methods to be called by the user. It is set up in a similar manner to data frame types in other languages, in that it is primarily focused on *columns* of data. These columns can differ in type from each other, but all elements within a single column must be of the same type. Each column must also have the same number of elements.

Headers can be used with the `data_frame` class, but all columns are required to have headers if one one has a header. All headers must be unique as well (whitespace does not count as unique ie `"  header"` is the same as `"header"`).


### Advanced Usage <a name="df_adv_usage"></a>

For storing column-based data, this library should work very well with the simple procedures already developed. 

#### Input/Output

For I/O, however, it is currently mostly up to the user to write extended types that can read/write to files with their desired formats.

The extended type is simple:
```fortran
type,extends(data_frame) :: example

contains
    procedure,public :: read_file
    procedure,public :: write_file
end type example

```
This type, `example`, inherits all the procedures and class members of the `data_frame` class, in addition to the two new procedures. The procedures `read_file` and `write_file` are then written to read and write to whichever particular format is desired.

#### Using columns in computations

To use a column in computations, simply 'pull' the column out as an array, for example:
```fortran

val = dot_product(df%getr("reals1"),df%getr("reals2"))
```
this will compute the dot production of columns `reals1` and `reals2`. One can also modify a column in a similar way:
```fortran
call df%setr("reals1",df%getr("reals1")/2.0)
```
this will divide every element in column `reals1` by 2. 



### Class Members <a name="df_child_types"></a>
All `data_frame` class members are `private` and thus need to accessed through class methods. This section is thus probably only useful for bug reports/contributions.

#### `integer :: n`
Number of columns.

#### `integer :: col_size`
Number of elements in each column.

#### `integer :: max_char_len`
Maximum character len used for headers and when character variables are used in columns.

#### `logical :: with_headers`
Variable that says whether data frame has headers or not.

#### `character(len=:),dimension(:),allocatable :: headers`
Array containing the headers. Size will always be equal to `n` and length will be equal to `max_char_len`.

#### `type(column),dimension(:),allocatable :: data_cols`
Array containing the actual columns. Size will always be equal to `n`. See [section 3](#column) for more.

### Procedures <a name="df_procedures"></a>
Many of the procedures are overloaded for each intrinsic data type and so in place of declaring their actual type (ie `real(rk) :: var`), they will be 'declared' as `intr_type :: var`. Public procedures will also be labeled as their public name rather than the private procedures that they point to.


#### Public Procedures:

#### `df%new()`
```fortran
subroutine new(char_len)
integer,intent(in),optional :: char_len
```

Subrtoutine that intializes `data_frame` object. Sets `max_char_len` to either `DEFAULT_MAX_CHAR_LEN` or to `char_len` if present.

#### `df%destroy()`
```fortran
subroutine destroy()
```
Subroutine that deallocates any allocatable vars within `data_frame` object.

#### `df%ncols()`
```fortran
pure function ncols() result(n)
integer :: n
```
Function to get number of columns.

#### `df%nrows()`
```fortran
pure function nrows() result(num_rows)
integer :: num_rows
```
Function to get number of rows/number of elements in each column.

#### `df%dtype()`
```fortran
pure function dtype(header)
character(len=*),intent(in) :: header
integer :: dtype
```
```fortran
pure function dtype(j)
integer,intent(in) :: j
integer :: dtype
```
Returns integer corresponding to data type contained within column. Returned value can be compared with parameter integers in `df_utils.f90`. Two calling signatures to choose column either using index, `j`, or named header, `header`.

#### `df%append(col,header)`
```fortran
subroutine append(col,header)
intr_type,dimension(:),intent(in) :: col
character(len=*),intent(in),optional :: header
```
Appends a column onto the `data_frame` object. Optionally include a header for the column. Rule of all columns having headers or no headers is enforced.

#### `df%append_empty*(col_size,header)`
**Note**: '`*`' can be raplced by `r`, `i`, `l`, `ch`, or `c`, corresponding to each of the 5 intrinsic types (`real`, `integer`, `logical`, `character`, `complex`).
```fortran
subroutine append_empty*(col_size,header)
integer,intent(in) :: col_size
character(len=*),intent(in),optional :: header
```
Appends an *empty* (but allocated) column of size `col_size` onto the `data_frame` object. `dtype` is determined through which of the 5 procedures are called. Optionally include a header for the column. Rule of all columns having headers or no headers is enforced.

#### `df%get*()`
**Note**: '`*`' can be raplced by `r`, `i`, `l`, `ch`, or `c`, corresponding to each of the 5 intrinsic types.
```fortran
function get(i,j) return(val)
integer,intent(in) :: i, j
intr_type :: val
```
Returns value located at column `i` and row `j`.
```fortran
function get(header,j) return(val)
character(len=*),intent(in) :: header
integer,intent(in) :: j
intr_type :: val
```
Returns value of row `j` in column corresponding to `header`.
```fortran
function get(i) return(col)
integer,intent(in) :: i
intr_type,dimension(col_size) :: col
```
Returns rank 1 array of `i`th column. `col_size` is type member.
```fortran
function get(header) return(col)
character(len=*),intent(in) :: header
intr_type,dimension(col_size),allocatable :: col
```
Returns rank 1 array of `header` column. `col_size` is type member.

#### `df%set*()`
**Note**: '`*`' can be raplced by `r`, `i`, `l`, `ch`, or `c`, corresponding to each of the 5 intrinsic types.
```fortran
subroutine set(i,j,val)
    integer,intent(in) :: i,j
    intr_type,intent(in) :: val
```
Sets value located at column `i` and row `j` to `val`.
```fortran
subroutine set(header,j,val)
    character(len=*),intent(in) :: header
    integer,intent(in) :: j
    intr_type,intent(in) :: val
```
Sets value located at row `j` in column `header` to `val`.
```fortran
subroutine set(i,col)
    integer,intent(in) :: i
    intr_type,dimension(col_size),intent(in) :: col
```
Sets entire `i`th column to `col`.
```fortran
subroutine set(header,col)
    character(len=*),intent(in) :: header
    intr_type,dimension(col_size),intent(in) :: col
```
Sets entire `header` column to `col`.

#### `df%write(unit)`
```fortran
subroutine write(unit)
    integer,intent(in) :: unit
```
Write outs `data_frame` to `unit` in a specialized format. Currently does not support writing complex type columns.

#### `df%read(filename,has_headers)`
```fortran
subroutine read(filename,has_headers)
    character(len=*),intent(in) :: filename
    logical,intent(in)
```
Reads in `filename` as `data_frame`. Very limited functionality, and very picky format. Cannot read headers or character vars with spaces in them.


## The `column` type <a name="column"></a>

### Child Types <a name="col_child_types"></a>

### Procedures <a name="col_procedures"></a>



## Utility Modules/Procedures <a name="utils"></a>

### Parameters <a name="utils_parameters"></a>

### Procedures <a name="utils_procedures"></a>