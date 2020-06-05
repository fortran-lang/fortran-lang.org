---
layout: book
title: Linking the objects
permalink: /learn/building_programs/linking_pieces
---

Almost all programs, except for the simplest, are built up from
different pieces. We are going to examine such a situation in
more detail.

Here is a general program for tabulating a function (source code in
"tabulate.f90"):

```fortran
    program tabulate
        use user_functions

        implicit none
        real    :: x, xbegin, xend
        integer :: i, steps

        write(*,*) 'Please enter the range (begin, end) and the number of steps:'
        read(*,*)  xbegin, xend, steps

        do i = 0,steps
            x = xbegin + i * (xend - xbegin) / steps
            write(*,'(2f10.4)') x, f(x)
        enddo
    end program tabulate
```

Note the `use` statement - this will be where we define the function `f`.

We want to make the program general, so keep the
specific source code - the implementation of the function `f` -
separated from the general source code. There are several ways to
achieve this, but one is to put it in a different source file. We can
give the general program to a user and they provide a specific source code.

Assume for the sake of the example that the function is implemented in a
source file "function.f90" as:

```fortran
    module user_functions
        implicit none
    contains

    real function f( x )
        real, intent(in) :: x

        f = x - x**2 + sin(x)

    end function f
    end module user_functions
```

To build the program with this specific function, we need to compile two
source files and combine them via the link step into one executable
program. Because the program "tabulate" depends on the module
"function", we need to compile the source file containing our module
first. A sequence of commands to do this is:

```shell
    $ gfortran -c function.f90
    $ gfortran tabulate.f90 function.o
```

The first step compiles the module, resulting in an object file
"function.o" and a module intermediate file, "function.mod". This module
file contains all the information the compiler needs to determine that
the function `f` is defined in this module and what its interface is. This
information is important: it enables the compiler to check that you call
the function in the right way. It might be that you made a mistake and
called the function with two arguments instead of one. If the compiler
does not know anything about the function's interface, then it cannot
check anything.

The second step invokes the compiler in such a way that:

* it compiles the file "tabulate.f90" (using the module file);
* it invokes the linker to combine the object files tabulate.o and function.o into an
executable program - with the default name "a.out" or "a.exe" (if you
want a different name, use the option "-o").

What you do not see in general is that the linker also adds a number of
extra files in this link step, the run-time libraries. These run-time
libraries contain all the "standard" stuff - low-level routines that do
the input and output to screen, the `sine` function and much more.

If you want to see the gory details, add the option "-v". This instructs
the compiler to report all the steps that are in detail.

The end result, the executable program, contains the compiled source
code and various auxiliary routines that make it work. It also contains
references to so-called dynamic run-time libraries (in Windows: DLLs, in
Linux: shared objects or shared libraries). Without these run-time
libraries the program will not start.

