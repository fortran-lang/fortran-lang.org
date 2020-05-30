---
layout: book
title: Variables
permalink: /learn/quickstart/variables
navbar: Learn
---

Variables store information that can be manipulated by the program.
Fortran is a '_strongly typed_' language, which means that each variable
must have a type.

There are 5 built-in data types in Fortran:

* `integer` -- for data that represent whole numbers, positive or negative
* `real` -- for floating-point data (not a whole number)
* `complex` -- pair consisting of a real part and an imaginary part
* `character` -- for text data
* `logical` -- for data that represent boolean (true or false) values

Before we can use a variable, we must _declare_ it; this tells the compiler
the variable type and any other variable attributes.

{% include note.html content="Fortran is a <b>statically typed</b> language which means the type of each
variable is fixed when the program is compiled - variable types cannot change while the program is running." %}

## Declaring variables

The syntax for declaring variables is:

```fortran
<variable_type> :: <variable_name>
```

where `<variable_type>` is one of the built-in variable types listed above and
`<variable_name>` is the name that you would like to call your variable.

Variable names must start with a letter and can consist of letters, numbers and underscores.
In the following example we declare a variable for each of the built-in types.

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


Once we have declared a variable, we can assign and reassign values to it using the assignment operator `=`.

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


## Standard input / output

In our _Hello World_ example, we printed text to the command window.
This is commonly referred to as writing to `standard output` or `stdout`.

We can use the `print` statement introduced earlier to print variable values to `stdout`:

```fortran
  print *, 'The value of amount (integer) is: ',amount
  print *, 'The value of pi (real) is: ',pi
  print *, 'The value of frequency (complex) is: ',frequency
  print *, 'The value of initial (character) is: ',initial
  print *, 'The value of isOkay (logical) is: ',isOkay
```

In a similar way, we can read values from the command window
using the `read` statement:

```fortran
program read_value
  implicit none
  integer :: age

  print *, 'Please enter your age: '
  read(*,*) age

  print *, 'Your age is: ',age

end program read_value
```

This input source is commonly referred to as `standard input` or `stdin`.


## Expressions
The usual set of arithmetic operators are available, listed in order or precedence:

| Operator &nbsp;  | Description    |
|:----------------:|----------------|
| `**`             | Exponent       |
| `*`              | Multiplication |
| `/ `             | Division       |
| `+`              | Addition       |
| `-`              | Subtraction    |

<br>

__Example:__ 

```fortran
program arithmetic
  implicit none

  real :: pi
  real :: radius
  real :: height
  real :: area
  real :: volume

  pi = 3.141592

  print *, 'Enter cylinder base radius:'
  read(*,*) radius

  print *, 'Enter cylinder height:'
  read(*,*) height

  area = pi*radius**2.0
  volume = area*height

  print *, 'Cylinder radius is: ',radius
  print *, 'Cylinder height is: ',height
  print *, 'Cylinder base area is: ',area
  print *, 'Cylinder volume is: ',volume

end program arithmetic
```






## Floating-point precision

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

In the next part we will learn how to use arrays for storing more than one
value in a variable.