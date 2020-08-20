---
layout: book
title: Managing libraries (static and dynamic libraries)
permalink: /learn/building_programs/managing_libraries
---

If you need to manage a program built of dozens of source files (and
that is not uncommon!), the command-line needed to specify all the
object files will be very long indeed. This soon becomes tedious or
even impossible to maintain. So a different solution is called for:
create your own libraries.

Libraries contain any number of object files in a compact form, so that
the command-line becomes far shorter:

```shell
$ gfortran -o tabulate tabulate.f90 function.o supportlib.a
```

where "supportlib.a" is a collection of one, two or many object files,
all compiled and then put into a library. The extension ".a" is used by
Linux and Linux-like platforms. On Windows the extension ".lib" is used.

Creating your own libraries is not that complicated: 
on Linux, you can achieve this using a utility like `ar`:

```shell
$ gfortran -c file1.f90 file2.f90
$ gfortran -c file3.f90 ...
$ ar r supportlib.a file1.o file2.o
$ ar r supportlib.a file3.o ...
```

or on Windows using the `lib` utility:

```shell
c:\...> ifort -c file1.f90 file2.f90
c:\...> ifort -c file3.f90 ...
c:\...> lib /out:supportlib.lib file1.obj file2.obj
c:\...> lib supportlib.lib file3.obj ...
```

Note:

* The command `ar` with the option `r` either creates the library (the
name appears after the option) or adds new object files to the library
(or replaces any existing ones).
* The command `lib` will create a new library if you use specify the
option `/out:` with the name of the new library next to it. To add
object files to an existing library, leave out the `/out:` bit.
* On platforms like Linux there is a particular convention to name
libraries. If you name your library like "libname.a" (note the "lib"
prefix), then you can refer to it as `-lname` in the link step.
* Libraries are often sought in directories indicated by an option `-L`
or `/LIBPATH`. This saves you from having to specify the exact path for
every library.

Using libraries you can build very large programs without
having to resort to extremely long command lines.


## Static versus dynamic libraries

The above discussion is tacitly assuming that you are using the so-called
static libraries. Static libraries (or at least parts of their
contents) become an integral part of the executable program. The only
way to change the routines incorporated in the program is by rebuilding
the program with a new version of the library.

A flexible alternative is to use the so-called dynamic libraries. These
libraries remain outside the executable program and as a consequence
can be replaced without rebulding the entire program. Compilers and
indeed the operating system itself rely heavily on such dynamic
libraries. You could consider dynamic libraries as a sort of executable
programs that need a bit of help to be run.

Building dynamic libraries works slightly differently from building
static libraries: you use the compiler/linker instead of a tool
like `ar` or `lib`.

On Linux:

```shell
$ gfortran -fpic -c file1.f90 file2.f90
$ gfortran -fpic -c file3.f90 ...
$ gfortran -shared -o supportlib.so file1.o file2.o file3.o ...
```

On Windows, with the Intel Fortran compiler:

```shell
$ ifort -c file1.f90 file2.f90
$ ifort -c file3.f90 ...
$ ifort -dll -exe:supportlib.dll file1.obj file2.obj file3.obj ...
```

The differences are that:

* You need to specify a compile option on Linux, for gfortran that is `-fpic`,
because the object code is slightly different.
* You need to tell in the link step that you want a dynamic library (on
Linux: a shared object/library, hence the extension ".so"; on Windows:
a dynamic link library)

There is one more thing to be aware of: On Windows you must
explicitly specify that a procedure is to be _exported_, i.e. is visible
in the dynamic library. There are several ways - depending on the
compiler you use - to achieve this. One method is via a so-called
compiler directive:

```fortran
subroutine myroutine( ... )
!GCC$ ATTRIBUTES DLLEXPORT:: myroutine
```

Or, with the Intel Fortran compiler:

```fortran
subroutine myroutine( ... )
!DEC$ ATTRIBUTES DLLEXPORT:: myroutine
```

Besides a dynamic library (DLL), a so-called import library may be
generated.

Because the details differ per compiler, here are two examples:
gfortran on Cygwin and Intel Fortran on Windows. In both cases
we look at the `tabulate` program in the file "tabulate.f90".

## GNU/Linux and gfortran
The `tabulate` program requires a user-defined routine `f`. If we
let it reside in a dynamic library, say "function.dll", we can simply
replace the implementation of the function by putting another dynamic
library in the directory. No need to rebuild the program as such.

On Cygwin it is not necessary to explicitly export a procedure - all
publically visible routines are exported when you build a dynamic library.
Also, no import library is generated.

Since our dynamic library can be built from a single source file, we
can take a shortcut:

```shell
$ gfortran -shared -o function.dll function.f90
```

This produces the files "function.dll" and "function.mod". The
utility `nm` tells us the exact name of the function `f`:

```shell
$ nm function.dll
...
000000054f9d7000 B __dynamically_loaded
                 U __end__
0000000000000200 A __file_alignment__
000000054f9d1030 T __function_MOD_f
000000054f9d1020 T __gcc_deregister_frame
000000054f9d1000 T __gcc_register_frame
...
```

It has received a prefix `__function_MOD_` to distinguish it from any
other routine "f" that might be defined in another module.

The next step is to build the program:

```shell
$ gfortran -o tabulate tabulate.f90 function.dll
```

The DLL and the .mod file are used to build the executable program
with checks on the function's interface, the right name and the reference
to "a" DLL, called "function.dll".

You can replace the shared library "function.dll" by another one, implementing
a different function "f". Of course, you need to be careful to use the correct
interface for this function. The compiler/linker are not invoked anymore, so they
can do no checking.

## Windows and Intel Fortran
The setup is the same as with Linux, but on Windows it is necessary
to explicitly export the routines. And an import library is generated -
this is the library that should be used in the link step.

The source file must contain the compiler directive, otherwise the function `f`
is not exported:

```fortran
real function f( x )
!DEC$ ATTRIBUTES DLLEXPORT :: f
```

Again we take a shortcut:

```shell
$ ifort -exe:function.dll function.f90 -dll
```

This produces the files "function.dll", "function.mod" as well as "function.lib" (and two
other files of no importance here). The "dependency walker" program tells us
that the exact name of the function "f" is `FUNCTION_mp_F`. It is also exported, so that
it can be found by the linker in the next step:

```
$ ifort tabulate.f90 function.lib
```

Note that we need to specify the name of the export library, not the DLL!

(Note also: the Intel Fortran compiler uses the name of the first source file as the
name for the executable - here we do without the `-exe` option.)

Just as under Cygwin, the DLL and the .mod file are used to build the executable program
with checks on the function's interface, the right name and the reference
to "a" DLL, called "function.dll".

You can replace the shared library "function.dll" by another one, but the same
caution is required: while the implementation can be quite different, the
function's interface must be the same.
