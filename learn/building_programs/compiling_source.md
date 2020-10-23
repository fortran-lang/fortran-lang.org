---
layout: book
title: Compiling the source code
permalink: /learn/building_programs/compiling_source
---

The first step in the build process is to compile the source code. The
output from this step is generally known as the object code – a set of
instructions for the computer generated from the human-readable source
code. Different compilers will produce different object codes from the
same source code and the naming conventions are different.

The consequences:

* If you use a particular compiler for one source file, you need to use
the same compiler (or a compatible one) for all other pieces. After
all, a program may be built from many different source files and the
compiled pieces have to cooperate.
* Each source file will be compiled and the result is stored in a file
with an extension like ".o" or ".obj". It is these object files that are
the input for the next step: the link process.

Compilers are complex pieces of software: they have to understand the
language in much more detail and depth than the average programmer. They
also need to understand the inner working of the computer. And then,
over the years they have been extended with numerous options to
customise the compilation process and the final program that will be
built.

But the basics are simple enough. Take the gfortran compiler, part of
the GNU compiler collection. To compile a simple program as the one
above, that consists of one source file, you run the following command,
assuming the source code is stored in the file "hello.f90":

```shell
$ gfortran -c hello.f90
```

This results in a file "hello.o" (as the gfortran compiler uses ".o" as
the extension for the object files).

The option "-c" means: only compile the source files. If you were to
leave it out, then the default action of the compiler is to compile the
source file and start the linker to build the actual executable program.
The command:

```shell
$ gfortran hello.f90
```

results in an executable file, "a.out" on Linux or "a.exe" on
Windows.

Some remarks:

* The compiler may complain about the contents of the source file, if it
finds something wrong with it – a typo for instance or an unknown
keyword. In that case the compilation process is broken off and you will
not get an object file or an executable program. For instance, if
the word "program" was inadvertently typed as "prgoram":

```shell
$ gfortran hello3.f90
hello.f90:1:0:

    1 | prgoram hello
      |
Error: Unclassifiable statement at (1)
hello3.f90:3:17:

    3 | end program hello
      |                 1
Error: Syntax error in END PROGRAM statement at (1)
f951: Error: Unexpected end of file in 'hello.f90'
```

Using this compilation report you can correct the source code and try
again.

* The step without "-c" can only succeed if the source file contains a
main program – characterised by the `program` statement in Fortran.
Otherwise the link step will complain about a missing "symbol", something
along these lines:

```shell
$ gfortran hello2.f90
/usr/lib/../lib64/crt1.o: In function `_start':
(.text+0x20): undefined reference to `main'
collect2: error: ld returned 1 exit status
```

The file "hello2.f90" is almost the same as the file "hello.f90", except
that the keyword `program` has been replaced by the keyword `subroutine`.

The above examples of output from the compiler will differ per compiler
and platform on which it runs. These examples come from the gfortran
compiler running in a Cygwin environment on Windows.

Compilers also differ in the options they support, but in general:

* Options for optimising the code – resulting in faster programs or
smaller memory footprints;
* Options for checking the source code – checks that a variable is not
used before it has been given a value, for instance or checks if some
extension to the language is used;
* Options for the location of include or module files, see below;
* Options for debugging.

