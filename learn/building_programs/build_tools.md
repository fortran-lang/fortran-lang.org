---
layout: book
title: Build tools
permalink: /learn/building_programs/build_tools
---

If this seems complicated, well, you are right and we are only
scratching the surface here. The complications arise because of
differences between platforms, differences between compilers/linkers and
because of differences in the way programs are set up. Fortunately,
there are many tools to help configure and maintain the build steps.
We will not try and catalogue them, but give instead a very limited
list of tools that you typically encounter:

* The `make` utility is a classical tool that uses instructions about
how the various components of a program depend on each other to
efficiently compile and link the program (or programs). It takes a
so-called `Makefile` that contains the dependencies.

Simply put:

If a program file is older than any of the libraries and object files
it depends on, the make utility knows it has to rebuild it and goes on
to look at the libraries and object files - are any out of date?

If an object file is older than the corresponding source file, the
make utility knows it has to compile the source file.

* Integrated development tools take care of many of the above details. A
popular cross-platform  tool is Microsoft's [Visual Studio Code](https://code.visualstudio.com/), but others exist,
such as [Atom](https://atom.io/), [Eclipse Photran](https://www.eclipse.org/photran/), and [Code::Blocks](http://www.codeblocks.org/). They offer a graphical
user-interface, but are often very specific for the compiler and
platform.

* Maintenance tools like autotools and CMake can generate Makefiles or
Visual Studio project files via a high-level description. They abstract
away from the compiler and platform specifics.

Here is a very simple example of a `Makefile` as used by the `make` utility,
just to give you an impression:

```
# Collect the macros at the beginning - easier to customise
FC = gfortran
LD = gfortran
FCOPTS = -c
LDOPTS = "-o "

EXE = .exe
OBJ = .o

all: tabulate$(EXE)

tabulate$(EXE) : tabulate$(OBJ) function$(OBJ)
{tab}$(LD) $(LDOPTS)tabulate$(EXE) tabulate.f90 function$(OBJ)

tabulate$(OBJ) : tabulate.f90 function.mod
{tab}$(FC) $(FCOPTS) tabulate.f90

function$(OBJ) : function.f90
{tab}$(FC) $(FCOPTS) function.f90

(A peculiarity of `make` is that in the input file, tab characters are used
in several places - here indicated as "{tab}" - as significant whitespace.)

When stored in a file "Makefile" and "{tab}" replaced by a tab character,
you can run it like:

```shell
$ make
```

(the name `Makefile` is the default, otherwise use the option `-f` to specify
a different file name). Now only change the file "tabulate.f90" and run it
again. You will see that only that file gets compiled again and then the
program is built. The file "function.f90" was not changed, so the object
file and the module intermediate file would remain unchanged, so there
is no need to recompile it.
