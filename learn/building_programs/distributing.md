---
layout: book
title: Distributing your programs
permalink: /learn/building_programs/distributing
---

When you distribute your programs, there are a number of options you can
choose from:

1. Distribute the entire source code
2. Distribute a pre-built executable program
3. Distribute static or dynamic libraries that people can use

__Option 1: Distribute the entire source code__

By far the simplest — for you as a programmer — is this one: you leave it
up to the user to build it on their own machine. Unfortunately, that
means you will have to have a user-friendly build system in place and
the user will have to have access to suitable compilers. For build systems:
see the previous section.

__Option 2: Distribute a pre-built executable program__

A pre-built program that does not need to be customised, other than via its
input, will still need to come with the various run-time libraries and will
be specific to the operating system/environment it was built for.

The set of run-time libraries differs per operating system and compiler version.
For a freely available compiler like gfortran, the easiest thing is to ask the
user to install that compiler on their system. In the case of Windows: the Cygwin
environment may be called for.

Alternatively, you can supply copies of the run-time libraries together with your
program. Put them in the directory where they can be found at run-time.

Note: On Windows, the Intel Fortran comes with a set of _redistributable_ libraries.
These will need to be made available.

In general: use a tool like "ldd" or "dependency walker" to find out what
libraries are required and consult the documentation of the compiler.

If your program does allow customisation, consider using dynamic libraries for this.
More is said about this below.

__Option 3: Distribute static or dynamic libraries that people can use__

This option is a combination of the first two options. It does put some burden on
the user, as they must create a main program that calls your routines in the
proper way, but they do not need to know much about the build system you used.
You will have to deal with the run-time libraries, though.

If you choose this option, besides the compiled libraries, you will also need to
supply the module intermediate files. These files are compiler-specific, but so are
the static libraries you build.

## Distributing the tabulation program
As shown above, the tabulation program can be built with the user-defined function
in a dynamic library. This enables you to:

* Ship the executable (with the appropriate run-time libraries)
* Provide a skeleton version of the module, something like:

```fortran
module user_functions
    implicit none
contains

real function f( x )
!DEC$ ATTRIBUTES DLLEXPORT :: f
    real, intent(in) :: x

    ! your function body goes here

end function f

end module user_functions
```

* Provide a basic build script with a command like:

```shell
gfortran -o functions.dll functions.f90 -shared
```

or:

```shell
ifort -exe:functions.dll functions.f90 -dll
```

As said, you cannot control that the user has done the right thing — any
DLL "functions.dll" with a function `f` would be accepted, but not necessarily
lead to a successful run.

An alternative set-up would be to change the main program into a subroutine
and have the function as an argument:

```fortran
module tabulation
    implicit none
contains

subroutine tabulate( f )
    interface
        real function f( x )
            real, intent(in) :: x
        end function f
    end interface

    ! your implementation goes here

end subroutine tabulate

end module tabulation
```

Then provide a skeleton main program:

```fortran
program tabulate_f
    use tabulation

    call tabulate( func1 )

contains

real function func1( x )
    real, intent(in) :: x

    ! your function body goes here

end function func1

end program tabulate_f
```

The advantage is that the compiler can check the interface of the
function that is passed and that the user has more freedom in the use of the
functionality provided by your library.
