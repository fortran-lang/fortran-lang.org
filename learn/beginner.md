---
layout: page
title: Beginner tutorial
permalink: /learn/beginner
---

This tutorial is aimed at those with little or no programming experience or those
wishing to refresh their knowledge of the basics.

If you already have programming experience see the [quickstart tutorial](quickstart)
to quickly pick up the Fortran-specific syntax and semantics.

On this page:

- [Hello world first program](#hello-world)
- [Variables](#variables)
- [Expressions](#expressions)
- [Controlling program flow (if,loops)](#controlling-program-flow)
- [Arrays](#arrays)

## Hello world

In this tutorial series we will use the free and open source [GNU Fortran compiler (gfortran)](https://gcc.gnu.org/fortran/).
If you haven't already setup a Fortan compiler on your system, [see here](setup) for how to do so.

{% include note.html content="Fortran is a <b>compiled language</b> which means that when written it must be passed through a
compiler to produce a machine executable that can be run." %}

Once you have setup up your compiler, open a new file in your favourite code editor and enter the following:

```fortran
program hello
  ! This is a comment line, it is ignored by the compiler
  print *, 'Hello, World!'
end program hello
```

{% include tip.html content="_Comments_ are programmer annotations to source code that are ignored by
the compiler. In Fortran, comment lines are preceded with `!`. It is good programming practice to comment your code
in places where the code is not self-explanatory." %}

Save the file in an empty directory with the name `hello.f90`.

{% include note.html content=".f90 is the standard file extension for modern fortran source files.
The 90 refers to the first modern fortran standard in 1990." %}

Now to compile this program, first open your terminal and use the `cd` command to change directory to
where you saved your `hello.f90` file:

```shell
$> cd /path/to/hello
```

Now type the following to compile your program:

```shell
$> gfortran hello.f90 -o hello
```

Here we have told the compiler to output the compiled program to an executable file called `hello`.
To run this compiled program:

```shell
$> ./hello
Hello, World!
```

In this first example we have used the `program` keyword followed by our program name to signify
the start of our executable program. This is followed by the code we wish to execute and subsequently
 _closed_ with a matching `end program` to signify where the program ends.

## Variables

Computer programs perform operations on data - this data is stored in _variables_ which each have 
a name and a type.

Fortran comes with 5 built-in data types:

* `integer` -- for data that represent whole numbers, positive or negative
* `real` -- for floating-point data (not a whole number)
* `complex` -- pair consisting of a real part and an imaginary part
* `character` -- for text data
* `logical` -- for data that represent boolean (true or false) values

### Declaring variables

Before we can use a variable, we must first __declare__ the variable - this tells the compiler what
the variable is called, it's data-type and its properties.

{% include note.html content="Fortran is a <b>statically typed</b> language which means the type of each
variable is fixed when the program is compiled - variable types cannot change while the program is running." %}

In the following example program we declare variables of each built-in type:

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

{% include tip.html content="Make sure to give your variables meaningful names and stick
to a consistent naming style; this will save a lot of time for yourself and others
when reading the code in the future." %}

Note the additional line at the beginning of the program: `implicit none`.
This statement tells the compiler that all variables will be explicitly declared; without 
this statement variables will be implicitly typed according to the letter they begin with.

{% include important.html content="Always use the <b>implicit none</b> statement at
the beginning of each program and procedure. Implicit typing is considered bad practice in 
modern programming since it hides information leading to more program errors." %}

### Assigning values

We can assign and reassign values to our variables using the _assignment_ operator `=`:

```fortran
  count = 10
  pi = 4.141592
  frequency = (1.0,-0.5)
  initial = 'A'
  isOkay = .false.
```

Characters are surrounded by either single (`'`) or double quotes (`"`). In the above example, our
character variable only contains space for a single letter; later on we will see how to store multiple
characters (known as a _string_).

Logical or boolean values can be either `.true.` or `.false.`.

{% include important.html title="Watch out" content="for assignment at declaration: `integer :: count = 1`. 
__This is NOT a normal initialisation;__ it implies the `save` attribute which means that the variable retains
its value between procedure calls. Good practice is to initialise your variables separately to their declaration." %}



### Printing values

We can use the `print` statement introduced earlier to print variable values to screen:

```fortran
  print *, 'The value of count (integer) is: ',count
  print *, 'The value of pi (real) is: ',pi
  print *, 'The value of frequency (complex) is: ',frequency
  print *, 'The value of initial (character) is: ',initial
  print *, 'The value of isOkay (logical) is: ',isOkay
```

### Reading values from the command line

Similar to the `print` statement is the `read` statement which allows us to read values
entered by the user at the command line:

```fortran
program read_value
  implicit none
  integer :: age

  print *, 'Please enter your age: '
  read(*,*) age

  print *, 'Your age is: ',age

end program read_value
```

## Expressions

### Arithmetic expressions

Having declared and initialised our variables, we can now perform arithmetic with them.
The usual set of arithmetic operators are available, listed in order or precedence:

| Operator &nbsp;  | Description    |
|:----------------:|----------------|
| `**`             | Exponent       |
| `*`              | Multiplication |
| `/ `             | Division       |
| `+`              | Addition       |
| `-`              | Subtraction    |

<br>

Example:

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

{% include important.html title="Watch out" content="for accidental integer division: `1/2` is equal to `0`
because both the numerator and denominator are integers." %}


<!-- Fortran has many built-in math functions: -->



### Logical expressions

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

One of the powerful advantages of computer algorithms compared to simple mathematical formulae
comes in the form program _branching_ in which the program can decide which instructions to 
execute next based on a logical condition.

There two main forms of controlling program flow:

- _Conditional_ (if): choose program path based on a boolean (true or false) value

- _Loop_: repeat a portion of code multiple times



### Conditional construct (`if`)

In the following example program, a conditional `if` construct is used to print out a
message if the `angle` variable is less than 90 degrees:

```fortran
program conditional
  implicit none

  real :: angle
  angle = 60

  if (angle < 90.0) then
    print *, 'Angle is acute'
  end if

end program conditional
```

In this first example, the code within the `if` construct is __only executed if__ the
test expression (`angle < 90.0`) is true.

{% include tip.html content="It is good practice to indent code within constructs such as `if` and `do`
to make code more readable." %}

We can add alternative branch to the construct using the `else` keyword:

```fortran
program conditional
  implicit none

  real :: angle
  angle = 60

  if (angle < 90.0) then
    print *, 'Angle is acute'
  else
    print *, 'Angle is obtuse'
  end if

end program conditional
```

Now there are two _branches_ in the `if` construct, but __only one branch is executed__ depending
on the logical expression following the `if` keyword.

We can actually add any number of branches using `else if` to specify more conditions:

```fortran
program conditional
  implicit none

  real :: angle
  angle = 60

  if (age < 90.0) then
    print *, 'Angle is acute'
  else if (angle < 180.0) then
    print *, 'Angle is obtuse'
  else
    print *, 'Angle is reflex'
  end if

end program conditional
```

When multiple conditional expressions are used, each conditional expression is tested only if none of the previous
expressions have evaluated to true.



### Loop constructs (`do`)

In the following example program a `do` loop construct is used to print out the numbers in
a geometric sequence.
The `do` loop has an integer _counter_ variable which is used to track which iteration of the loop
is currently executing, in this example we use a common name for this counter variable: `i`.

```fortran
program do_loop
  implicit none

  integer :: i
  integer :: N
  real :: ratio
  real :: initial

  ratio = 0.25
  initial = 1.00
  N = 10

  do i=0,N
    print *, i, initial*(ratio**i)
  end do

end program do_loop
```

When we define the start of the `do` loop we use our counter variable name followed by an equals (`=`) sign
to specify the start value and final value of our counting variable.
In this example we perform 11 loop iterations where `i` varies from `0` to `10`.



## Arrays

More often than not, we need to store and operate on long lists of numbers as opposed to just the single scalar variables
that we have been using so far; in computer programming such lists are called  _arrays_.

{% include tip.html title="Definition:" content="Arrays are _multidimensional_ variables which contain more than value 
where each value is accessed using one or indices." %}

### Array declaration

We can declare arrays of any type. There are two common notations for declaring array variables;
using the `dimension` attribute or by appending the array dimensions in parentheses to the variable name.

Example:

```fortran
program arrays_1d
  implicit none

  ! Declare 1D integer array of 10 elements
  integer, dimension(10) :: array1

  ! Equivalent declaration
  integer :: array2(10)

end program arrays_1d
```

Multidimensional arrays are defined simply with an additional dimension:

```fortran
program arrays_2d
  implicit none

  ! Declare 2D real array of 100 elements
  real, dimension(10,10) :: array1

  ! Equivalent declaration
  real :: array2(10,10)

end program arrays_2d
```

### Accessing array elements

To read, write or reference individual elements of an array we specify the index or indices of 
an element within parentheses after the variable name.

{% include important.html content="Arrays in Fortran are __one-based__ by default; this means
that the first element along any dimension is at index 1." %}

__Example:__ 1D array of square numbers

```fortran
program arrays1
  implicit none

  integer :: i
  integer, dimension(10) :: array1

  do i=1,10            ! Loop over array elements
    array1(i) = i**2   ! Set element i to square of i
  end do

  do i=1,10
    print *,i,array1(i)
  end do

end program arrays1
```

__Example:__ 2D array of products

```fortran
program arrays2
  implicit none

  integer :: i, j
  real, dimension(10,10) :: array2

  do i=1,10
    do j=1,10
      array2(i,j) = i*j
    end do
  end do

end program arrays2
```

### Array slicing

A powerful feature of the Fortran language is its built-in support for array operations;
we can perform operations on all or part of an array using array _slicing_ notation:

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

<!-- ### Array operations -->



