---
layout: book
title: Include files and modules
permalink: /learn/building_programs/include_files
---

Your program can be contained in various source files, all stored in the
same directory or organised in some convenient directory tree. The
details of the organisation depend on personal taste, arrangements made
by the group of developers you belong to, or simply the history of the
program. Whatever the directory structure is, you will encounter a
situation where the compiler needs assistence in order to compile a
particular source file:

* Fortran (and other languages) has the possibility to include an
external file. While this feature has become a bit less useful with the
advent of modules, it still has its uses. Quite often, such "include
files" are stored in a directory separated from the directories
containing the source files, because they are used in several locations.
Quite often the name of that directory is "include".
* As we have seen, compiling source code that defines one or more modules, leads to
the compiler generating so-called "module intermediate files" (with the extension ".mod").
The compiler needs access to these files to be able to read the interfaces
and variables and what not, and based on this information, actually
compile the source code that uses the various modules.

Compilers support options like `-I` to indicate where these include
files and module intermediate files are to be found. Suppose we store
the two files of our `tabulate` program in the following directory
structure:

    tabulate/
        main/
            tabulate.f90
        sub/
            function.f90

Compiling the file "function.f90" with the commands

'''shell
    $ cd sub
    $ gfortran -c function.f90
```

leads to this structure:

        tabulate/
            main/
                tabulate.f90
            sub/
                function.f90
                function.mod
                function.o

To successfully compile and subsequently build the program we need to
tell the compiler where it can find the file "function.mod":

```shell
    $ cd main
    $ gfortran -c tabulate.f90 -I ../sub
    $ gfortran -o tabulate tabulate.o ../sub/function.o
```

The result:

    tabulate/
        main/
            tabulate.f90
            tabulate.o
            tabulate (or tabulate.exe on Windows)
        sub/
            function.f90
            function.mod
            function.o

Notes:

* The details differ per compiler. Sometimes the `-I` option should be
followed by a space and then the name of the directory, sometimes the
directory should come consecutively.
* By default the module intermediate files (.mod) are put in the
same directory as the object files. When your program code is organised
in different subdirectories, they will get scattered over the directory
tree, complicating the compilation process. Luckily,
many compilers allow you to specify the output location for these
files. For gfortran this is `-J`, for instance:
`-J../include` (so that the .mod files could all appear in the
same directory)
* For large programs, consisting of many source files, it is important to
think through what organisation to use.

