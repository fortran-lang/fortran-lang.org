---
layout: page_learn
title: Quickstart tutorial
permalink: /learn/quickstart
navbar: Learn
---
 
This quickstart tutorial assumes familiarity with basic programming concepts such as types, variables, arrays, control flow and functions.


On this page:

- [Setting up a Fortran compiler](#compiler-setup)
- [Hello world program](#hello-world)
- [Variables, arrays & strings](#variables)
- [Operators](#operators)
- [Controlling program flow (if,loops)](#controlling-program-flow)
- [Procedures (functions,subroutines)](#procedures)
- [Modules](#modules)

## Compiler setup

{% include note.html content="Fortran is a <b>compiled language</b> which means that once written, the source code must be passed through a
compiler to produce a machine executable that can be run." %}

In this tutorial, we'll work with the free and open source 
[GNU Fortran compiler (gfortran)](https://gcc.gnu.org/fortran/), 
which is part of the 
[GNU Compiler Colection (GCC)](https://gcc.gnu.org/).

To install gfortran on Linux, use your system package manager.
Otherwise, for macOS or Windows, refer to gfortran binaries from 
[this page](https://gcc.gnu.org/install/binaries.html).

To check if you have _gfortran_ setup correctly, open a terminal and run the following command :

```shell
$> gfortran --version
```

this should output something like:

```
GNU Fortran 7.5.0
Copyright (C) 2017 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
```

## Hello world

Once you have setup up your compiler, open a new file in your favourite code editor and enter the following:

```fortran
program hello
  ! This is a comment line, it is ignored by the compiler
  print *, 'Hello, World!'
end program hello
```

Having saved your program to `hello.f90`, compile at the command line with:
```shell
$> gfortran hello.f90 -o hello
```

{% include note.html content=".f90 is the standard file extension for modern fortran source files.
The 90 refers to the first modern fortran standard in 1990." %}

To run your compiled program:
```shell
$> ./hello
Hello, World!
```

## Variables

Fortran comes with 5 built-in data types:

* `integer` -- for data that represent whole numbers, positive or negative
* `real` -- for floating-point data (not a whole number)
* `complex` -- pair consisting of a real part and an imaginary part
* `character` -- for text data
* `logical` -- for data that represent boolean (true or false) values

### Declaring variables

{% include note.html content="Fortran is a <b>statically typed</b> language which means the type of each
variable is fixed when the program is compiled - variable types cannot change while the program is running." %}

__Example:__ variable declaration

```fortran
program variables
  implicit none

  integer :: amount
  real :: pi
  complex :: frequency
  character :: initial
  logical :: isOkay

end program variables
```

{% include note.html content="Fortran code is __case-insensitive__; you don't have to worry about the
capitalisation of your variable names but it's good practice to keep it consistent." %}

Note the additional statement at the beginning of the program: `implicit none`.
This statement tells the compiler that all variables will be explicitly declared; without 
this statement variables will be implicitly typed according to the letter they begin with.

{% include important.html content="Always use the `implicit none` statement at
the beginning of each program and procedure. Implicit typing is considered bad practice in 
modern programming since it hides information leading to more program errors." %}

__Example:__ variable assignment

```fortran
  amount = 10
  pi = 4.141592
  frequency = (1.0,-0.5)
  initial = 'A'
  isOkay = .false.
```

Characters are surrounded by either single (`'`) or double quotes (`"`).

Logical or boolean values can be either `.true.` or `.false.`.

{% include important.html title="Watch out" content="for assignment at declaration: `integer :: amount = 1`. 
__This is NOT a normal initialisation;__ it implies the `save` attribute which means that the variable retains
its value between procedure calls. Good practice is to initialise your variables separately to their declaration." %}



### Floating-point precision

The desired floating-point precision can be explicitly declared using a `kind` parameter.
The `iso_fortran_env` intrinsic module provides kind parameters for the common 32bit and 64bit floating point types.

__Example:__ explicit real kind
```fortran
program float
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  implicit none

  real(sp) :: float32
  real(dp) :: float64

  float32 = 1.0_sp       ! Explicit suffix for literal constants
  float64 = 1.0_dp

end program float
```

{% include important.html content="Always use a `kind` suffix for floating point literal constants." %}

__Example:__ c-interoperable kinds
```fortran
program float
  use, intrinsic :: iso_c_binding, only: sp=>c_float, dp=>c_double
  implicit none

  real(sp) :: float32
  real(dp) :: float64

end program float
```


### Arrays

{% include important.html content="Arrays in Fortran are __one-based__ by default; this means
that the first element along any dimension is at index 1." %}

__Example:__ static array declaration
```fortran
program arrays
  implicit none

  ! 1D integer array
  integer, dimension(10) :: array1

  ! An equivalent array declaration
  integer :: array2(10)

  ! 2D real array
  real, dimension(10,10) :: array3

  ! Custom lower and upper index bounds
  real :: array4(0:9)
  real :: array5(-5:5)

end program arrays
```

__Example:__ array slicing
```fortran
program array_slice
  implicit none

  integer :: i
  integer :: array1(10)           ! 1D integer array of 10 elements
  integer :: array2(10,10)        ! 2D integer array of 100 elements

  array1 = [1,2,3,4,5,6,7,8,9,10] ! Array constructor
  array1 = [(i,i=1,10)]           ! Implied do loop constructor
  array1(:) = 0                   ! set all elements to zero
  array1(1:5) = 1                 ! set first five elements to one
  array1(6:) = 1                  ! set all elements after five to one
  
  print *,array1(1:10:2)          ! print out elements at odd indices
  print *,array2(:,1)             ! print out the first column in a 2D array
  print *,array1(10:1:-1)         ! print an array in reverse

end program array_slice
```

{% include note.html content="Fortran arrays are stored in __column major__ order; the first
index varies fastest." %}

__Example:__ allocatable (dynamic) arrays
```fortran
program allocatable
  implicit none

  integer, allocatable :: array1(:)
  integer, allocatable :: array2(:,:)

  allocate(array1(10))
  allocate(array2(10,10))

  ...

  deallocate(array1)
  deallocate(array2)

end program allocatable
```

{% include note.html content="Allocatable local arrays are deallocated automatically
when they go out of scope." %}


### Character strings


__Example:__ static character string
```fortran
program string
  implicit none

  character(len=4) :: first_name
  character(len=5) :: last_name
  character(10) :: full_name

  first_name = 'John'
  last_name = 'Smith'

  ! String concatenation
  full_name = first_name//' '//last_name

  print *, full_name 

end program string
```

__Example:__ allocatable character string
```fortran
program allocatable_string
  implicit none

  character(:), allocatable :: first_name
  character(:), allocatable :: last_name

  ! Explicit allocation statement
  allocate(character(4) :: first_name)
  first_name = 'John'

  ! Allocation on assignment
  last_name = 'Smith'

  print *, first_name//' '//last_name

end program allocatable_string
```


## Standard input / output

We can use the `print` statement introduced earlier to print variable values to `stdout`:

```fortran
  print *, 'The value of amount (integer) is: ',amount
  print *, 'The value of pi (real) is: ',pi
  print *, 'The value of frequency (complex) is: ',frequency
  print *, 'The value of initial (character) is: ',initial
  print *, 'The value of isOkay (logical) is: ',isOkay
```

and the `read` statement to read values from `stdin`:

```fortran
program read_value
  implicit none
  integer :: age

  print *, 'Please enter your age: '
  read(*,*) age

  print *, 'Your age is: ',age

end program read_value
```

## Operators

The usual set of arithmetic operators are available, listed in order or precedence:

| Operator &nbsp;  | Description    |
|:----------------:|----------------|
| `**`             | Exponent       |
| `*`              | Multiplication |
| `/ `             | Division       |
| `+`              | Addition       |
| `-`              | Subtraction    |

<br>

{% include important.html title="Watch out" content="for accidental integer division: `1/2` is equal to `0`
because both the numerator and denominator are integers." %}

To form a logical expression the following set of relational operators are available:

| Operator &nbsp;  | Alternative &nbsp;    | Description                                                     |
|:----------------:|:---------------------:|-----------------------------------------------------------------|
| `==`             | `.eq.`                | Tests for equality of two operands                              |
| `/=`             | `.ne.`                | Test for inequality of two operands                             |
| `> `             | `.gt.`                | Tests if left operand is strictly greater than right operand    |
| `< `             | `.lt.`                | Tests if left operand is strictly less than right operand       |
| `>=`             | `.ge.`                | Tests if left operand is greater than or equal to right operand |
| `<=`             | `.le.`                | Tests if left operand is less than or equal to right operand    |

<br>

as well as the following logical operators:

| Operator &nbsp; | Description                                                          |
|:---------------------:|----------------------------------------------------------------|
| `.and.`         | TRUE if both left and right operands are TRUE                        |
| `.or.`          | TRUE if either left or right or both operands are TRUE               |
| `.not.`         | TRUE if right operand is FALSE                                       |
| `.eqv.`         | TRUE if left operand has same logical value as right operand         |
| `.neqv.`        | TRUE if left operand has the opposite logical value as right operand |

<br>





<!-- Fortran has many built-in math functions: -->




## Controlling program flow

### Conditional construct (`if`)

__Example:__ single branch `if`

```fortran
  if (angle < 90.0) then
    print *, 'Angle is acute'
  end if
```

__Example:__ two-branch `if-else`

```fortran
  if (angle < 90.0) then
    print *, 'Angle is acute'
  else
    print *, 'Angle is obtuse'
  end if
```

__Example:__ multi-branch  `if-elseif-else`
```fortran
  if (age < 90.0) then
    print *, 'Angle is acute'
  else if (angle < 180.0) then
    print *, 'Angle is obtuse'
  else
    print *, 'Angle is reflex'
  end if
```


### Loop constructs (`do`)

__Example:__ `do` loop

```fortran
  integer :: i
  do i=1,10
    print *, i
  end do
```

__Example:__ `do` loop with skip

```fortran
  integer :: i
  do i=1,10,2    
    print *, i   ! Print odd numbers
  end do
```


__Example:__ `do while` loop

```fortran
  integer :: i
  i = 1
  do while (i<11)   
    print *, i
    i = i + 1
  end do
```


## Procedures

Fortran has two forms of procedure:

- __Subroutine:__ invoked by a `call` statement
- __Function:__ invoked within an expression or assignment to which it returns a value

Both subroutines and functions have access to variables in the parent scope by _argument association_;
unless the `VALUE` attribute is specified, this is similar to call by reference.

### Subroutines


The subroutine input arguments, known as _dummy arguments_ are specified in parentheses after the subroutine name;
the dummy argument types and attributes are declared within the body of the subroutine just like local variables.

__Example:__

```fortran
! Print matrix A to screen
subroutine print_matrix(n,m,A)
  implicit none
  integer, intent(in) :: n
  integer, intent(in) :: m
  real, intent(in) :: A(n,m)

  integer :: i
  do i=1,n
    print *,A(i,1:m)
  end do

end subroutine print_matrix
```


Note the additional `intent` attribute when declaring the dummy arguments; this optional attribute signifies to the compiler whether the argument
is 'read-only' (`intent(in)`) 'write-only' (`intent(out)`) or 'read-write' (`intent(inout)`) within the procedure.
In this example, the subroutine does not modify its arguments, hence all arguments are `intent(in)`.

{% include tip.html content="It is good practice to always specify the `intent` attribute for
dummy arguments; this allows the compiler to check for unintentional errors and provides self-documentation." %}


We can call this subroutine from a program using a `call` statement:
```fortran
program call_sub
  implicit none
  
  real :: mat(10,20)
  
  mat(:,:) = 0.0

  call print_matrix(10,20,mat)

end program call_sub
```

{% include note.html content="This example uses a so-called _explicit-shape_ array argument since we have passed additional variables to describe
the dimensions of the array `A`; this will not be necessary if we place our subroutine in a module as described later." %}


### Functions

```fortran
! L2 Norm of a vector
function vector_norm(n,vec) result(norm)
  implicit none
  integer, intent(in) :: n
  real, intent(in) :: vec(n)
  real :: norm

  norm = sqrt(sum(vec**2))

end function vector_norm
```

To execute this function:

```fortran
program run_fcn
  implicit none

  real :: v(9)
  real :: vector_norm
  
  v(:) = 9

  print *, 'Vector norm = ',vector_norm(9,v)

end program run_fcn
```

{% include tip.html content="It is good programming practice for functions not to modify their arguments - _i.e._ all function arguments should be `intent(in)` - such
functions are known as `pure` functions. Use subroutines if your procedure needs to modify its arguments." %}


## Modules

Fortran modules contain definitions that are made accessible to programs, procedures and other modules through the `use` statement.
They can contain data objects, type definitions, procedures and interfaces.

- Modules allow controlled scoping extension whereby entity access is made explicit
- Modules automatically generate explicit interfaces required for modern procedures

{% include tip.html content="It is recommended to always place functions and subroutines
within modules." %}

__Example:__ 

```fortran
module my_mod 
  implicit none

  private                          ! All entities are module-private by default
  public public_var, print_matrix  ! Explicitly export public entities

  real, parameter :: public_var = 2
  integer :: private_var

  contains
    
    ! Print matrix A to screen
    subroutine print_matrix(A)
      real, intent(in) :: A(:,:)  ! An assumed-shape dummy argument

      integer :: i
      do i=1,size(A,1)
        print *,A(i,:)
      end do

    end subroutine print_matrix

end module my_mod
```

{% include note.html content="Compare this `print_matrix` subroutine with [that written outside of a module](#subroutines);
we no longer have to explicitly pass the matrix dimensions and can instead take
advantage of _assumed-shape_ arguments since the module will generate the required
explicit interface for us. This results in a much simpler subroutine interface." %}

To `use` the module within a program:
```fortran
program use_mod
  use my_mod
  implicit none

  real :: mat(10,10)

  mat(:,:) = public_var

  call print_matrix(mat)

end program use_mod
```

__Example:__ explicit import list

```fortran
  use my_mod, only: public_var
```

__Example:__ aliased import

```fortran
  use my_mod, only: printMat=>print_matrix
```

{% include note.html content="Each module should be written in a separate .f90 source file. Modules need to be compiled prior to any program units that `use` them." %}



