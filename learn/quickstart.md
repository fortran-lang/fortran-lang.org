---
layout: page
title: Quickstart tutorial
permalink: /learn/quickstart
---

This tutorial is aimed at those with some programming experience in another language
who want to learn Fortran-specific syntax and semantics.

For a more basic introduction to programming in general using Fortran see the [beginner tutorial](beginner).

On this page:

- [Hello world first program](#hello-world)
- [Variables](#variables)
- [Operators](#operators)
- [Controlling program flow (if,loops)](#controlling-program-flow)
- [Procedures (subroutines,functions)](#procedures)
- [Modules](#modules)

## Hello world

In this tutorial series we will use the free and open source [GNU Fortran compiler (gfortran)](https://gcc.gnu.org/fortran/).
If you haven't already setup a Fortan compiler on your system, [see here](setup) for how to do so.

__Example:__ hello world

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

and run:
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

  integer :: count
  real :: pi
  complex :: frequency
  character :: initial
  logical :: isOkay

end program variables
```

{% include note.html content="Fortran code is __case-insensitive__; you don't have to worry about the
capitalisation of your variable names but it's good practice to keep it consistent." %}

Note the additional line at the beginning of the program: `implicit none`.
This statement tells the compiler that all variables will be explicitly declared; without 
this statement variables will be implicitly typed according to the letter they begin with.

{% include important.html content="Always use the <b>implicit none</b> statement at
the beginning of each program and procedure. Implicit typing is considered bad practice in 
modern programming since it hides information leading to more program errors." %}

{% include important.html title="Watch out" content="for assignment at declaration: `integer :: count = 1`. 
__This is NOT a normal initialisation;__ it implies the `save` attribute which means that the variable retains
its value between procedure calls. Good practice is to initialise your variables separately to their declaration." %}


### Floating-point precision

It is good practice to explicitly declare the desired float-point precision
using a `kind` parameter. The `iso_fortran_env` intrinsic module provides kind parameters
for the common 32bit and 64bit floating point types

__Example:__ 
```fortran
program float
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  implicit none

  real(sp) :: float32
  real(dp) :: float64

  float32 = 1.0_sp
  float64 = 1.0_dp

end program float
```

{% include important.html content="Always use a `kind` suffix for floating point literal constants." %}



### Array declaration

__Example:__ array declaration
```fortran
program arrays
  implicit none

  ! 1D integer array
  integer, dimension(10) :: array1

  ! An equivalent array declaration
  integer :: array2(10)

  ! 2D real array
  real, dimension(10,10) :: array3

end program arrays
```

__Example:__ array slicing
```fortran
program array_slice
  implicit none

  integer :: array1(10)
  integer :: array2(10,10)

  array1(:) = 0                 ! set all elements to zero
  array1(1:5) = 1               ! set first five elements to one
  array1(6:) = 1                ! set all elements after five to one

  print *,array1(1:10:2)        ! print out elements at odd indices
  print *,array2(:,1)           ! print out the first column in a 2D array
  print *,array1(10:1:-1)       ! print an array in reverse

end program array_slice
```

## Standard input / output

We can use the `print` statement introduced earlier to print variable values to `stdout`:

```fortran
  print *, 'The value of count (integer) is: ',count
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

The subroutine input arguments, known as _dummy arguments_ are specified in parentheses after the subroutine name;
the dummy argument types and attributes are declared within the body of the subroutine just like local variables.

Note the additional `intent` attribute; this optional attribute signifies to the compiler whether the argument
is 'read-only' (`intent(in)`) 'write-only' (`intent(out)`) or 'read-write' (`intent(inout)`).
In this example, the subroutine does not modify its arguments, hence all arguments are `intent(in)`.

{% include tip.html content="It is good practice to always specify the `intent` attribute for
dummy arguments; this adds robustness against errors and provides a level of self-documentation." %}


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
pure function vector_norm(n,vec) result(norm)
  implicit none
  integer, intent(in) :: n
  real, intent(in) :: vec(n)
  real :: norm

  norm = sqrt(sum(vec**2))

end function vector_norm
```

{% include tip.html content="It is good practice for all function arguments to be `intent(in)`; such
functions are known as `pure` functions. Use subroutines for procedures that need to modify their arguments." %}

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


## Modules

