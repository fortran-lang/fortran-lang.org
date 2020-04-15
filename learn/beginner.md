---
layout: page
title: Beginner tutorial
permalink: /learn/beginner
---

This tutorial is aimed at those with little or no programming experience or those
wishing to refresh their knowledge of the basics.

Jump to:

- [Hello world first program](#hello-world)
- [Variables](#variables)
- [Controlling program flow (if,loops)](#controlling-program-flow)
- [Procedures (functions,subroutines)](#procedures)

## Hello world

In this tutorial series we will use the free and open source [GNU Fortran compiler (gfortran)](https://gcc.gnu.org/fortran/).
If you haven't already setup a Fortan compiler on your system, [see here](/learn/setup) for how to do so.

{% include note.html content="Fortran is a <b>compiled language</b> which means that when written it must be passed through a
compiler to produce a machine executable that can be run." %}

Once you have setup up your compiler, open a new file in your favourite code editor and enter the following:

```fortran
program hello
  print *, 'Hello, World!'
end program hello
```

save the file in an empty directory with the name `hello.f90`.

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

### Printing values

We can use the `print` statement introduced earlier to print variable values to screen:

```fortran
print *, 'The value of count (integer) is: ',count
print *, 'The value of pi (real) is: ',pi
print *, 'The value of frequency (complex) is: ',frequency
print *, 'The value of initial (character) is: ',initial
print *, 'The value of isOkay (logical) is: ',isOkay
```


## Arrays

## Controlling program flow

### Conditional construct (`if`)


### Loop constructs (`do`)

## Procedures

### Functions

Here's a function that returns a cube-root of a real number:

```fortran
real function cbrt(x)
  real, intent(in) :: x
  cbrt = x**(1. / 3)
end function cbrt
```

### Subroutines


