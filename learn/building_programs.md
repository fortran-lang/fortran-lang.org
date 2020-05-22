---
layout: page
title: Building programs
permalink: /learn/build_programs
navbar: Learn
---



Draft tutorial: Building programs
---------------------------------

Remark: I only included Linux and Windows as operating systems in this
tutorial, for the simple reason that I have no experience with MacOS*
in any of its guises. While MacOS* is very similar to Linux, it is the
differences that may need to be documented. Some information would be
helpful.

Author: Arjen Markus

dd. 22 may 2020

Introduction: compiled languages
================================

Languages like Fortran, C, C++ and Java, to name but a few, share
certain characteristics: you write code in your language of choice but
then you have to build an executable program from that source code.
Other languages are interpreted - the source code is analysed by a
special program and taken as direct instructions. Two very simple
examples of that type of language: Windows batch files and Linux shell
scripts.

In this tutorial we concentrate on the first type of languages, with
Fortran as the main example. One advantage of compiled languages is that
the build process that you need to build an executable program, is used
to transform the human-readable source code into an efficient program
that can be run on the computer.

Let us have a look at a simple example:

    program hello
        write(*,*) 'Hello!'
    end program hello

This is just about the simplest program you can write in Fortran and it
is certainly a variation on one of the most famous programs. Even though
it is simple to express in source code, a lot of things actually happen
when the executable that is built from this code runs:

* A process is started on the computer in such a way that it can write
to the console - the window (DOS-box, xterm, ...) at which you type the
program's name.
* It writes the text "Hello!" to the console. To do so it must properly
interact with the console.
* When done, it finishes, cleaning up all the resources (memory,
connection to the console etc.) it took.

Fortunately, as a programmer in a high-level language you do not need to
consider all these details. In fact, this is the sort of things that is
taken care of by the build process: the compiler and the linker.


Compiling the source code
-------------------------

The first step in the build process is to compile the source code. The
output from this step is generally known as the object code - a set of
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
above, that consists of one source file, you run the following command:

    $ gfortran -c hello.f90

(assuming the source code is stored in the file "hello.f90")

This results in a file "hello.o" (as the gfortran compiler uses ".o" as
the extension for the object files).

The option "-c" means: only compile the source files. If you were to
leave it out, then the default action of the compiler is to compile the
source file and start the linker to build the actual executable program.
The command:

    $ gfortran hello.f90

results in an executable file, "a.out" (on Linux) or "a.exe" on
Windows.

Some remarks:

* The compiler may complain about the contents of the source file, if it
finds something wrong with it - a typo for instance or an unknown
keyword. In that case the compilation process is broken off and you will
not get an object file or an executable program. For instance, if
the word "program" was inadvertently typed as "prgoram":

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

Using this compilation report you can correct the source code and try
again.

* The step without "-c" can only succeed if the source file contains a
main program - characterised by the "program" statement in Fortran.
Otherwise the link step will complain about a missing "symbol":

        $ gfortran hello2.f90
        /usr/lib/gcc/x86_64-pc-cygwin/9.3.0/../../../../x86_64-pc-cygwin/bin/ld: /usr/lib/gcc/x86_64-pc-cygwin/9.3.0/../../../../lib/libcygwin.a(libcmain.o): in function `main':
        /usr/src/debug/cygwin-3.1.4-1/winsup/cygwin/lib/libcmain.c:37: undefined reference to `WinMain'
        /usr/src/debug/cygwin-3.1.4-1/winsup/cygwin/lib/libcmain.c:37:(.text.startup+0x7f): relocation truncated to fit: R_X86_64_PC32 against undefined symbol `WinMain'
        collect2: error: ld returned 1 exit status

The file "hello2.f90" is almost the same as the file "hello.f90", except
that the keyword "program" has been replaced by "subroutine".

The above examples of output from the compiler will differ per compiler
and platform on which it runs. These examples come from the gfortran
compiler running in a Cygwin environment on Windows.

Compilers also differ in the options they support, but in general:

* Options for optimising the code - resulting in faster programs or
smaller memory footprints.
* Options for checking the source code - checks that a variable is not
used before it has been given a value, for instance or checks if some
extension to the language is used.
* Options for the location of include or module files, see below.
* Options for debugging.


Linking the pieces
------------------

Almost all programs, except for the simplest, are built up from
different pieces. We are going to examine such a situation in
more detail.

Here is a general program for tabulating a function (source code in
"tabulate.f90"):

    program tabulate
        use function

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

Note the use statement - this will be where we define the function f.

We want to make the program general, so keep the
specific source code - the implementation of the function f -
separated from the general source code. There are several ways to
achieve this, but one is to put it in a different source file. We can
give the general program to a user and they provide a specific source code.

Assume for the sake of the example that the function is implemented in a
source file "function.f90" as:

    module function
        implicit none
    contains

    real function f( x )
        real, intent(in) :: x

        f = x - x**2 + sin(x)

    end function f
    end module function

To build the program with this specific function, we need to compile two
source files and combine them via the link step into one executable
program. Because the program "tabulate" depends on the module
"function", we need to compile the source file containing our module
first. A sequence of commands to do this is:

    $ gfortran -c function.f90
    $ gfortran tabulate.f90 function.o

The first step compiles the module, resulting in an object file
"function.o" and a module intermediate file, "function.mod". This module
file contains all the information the compiler needs to determine that
the function f is defined in this module and what its interface is. This
information is crucial: it enables the compiler to check that you call
the function in the right way. It might be that you made a mistake and
called the function with two arguments in stead of one. If the compiler
does not know anything about the function's interface, then it can not
check anything.

The second step invokes the compiler in such a way that:

* it compiles the file "tabulate.f90" (using the module file);
* it invokes the linker to combine the object files tabulate.o and function.o into an
executable program - with the default name "a.out" or "a.exe" (if you
want a different name, use the option "-o").

What you do not see in general is that the linker also adds a number of
extra files in this link step, the run-time libraries. These run-time
libraries contain all the "standard" stuff - low-level routines that do
the input and output to screen, the sine function and much more.

If you want to see the gory details, add the option "-v". This instructs
the compiler to report all the steps that are in detail.

The end result, the executable program, contains the compiled source
code and various auxiliary routines that make it work. It also contains
references to so-called dynamic run-time libraries (in Windows: DLLs, in
Linux: shared objects or shared libraries). Without these run-time
libraries the program will not start.


Run-time libraries
------------------

To illustrate that even a simple program depends on external run-time
libraries, here is the output from the "ldd" utility that reports such
dependencies:

    $ ldd tabulate.exe
            ntdll.dll => /cygdrive/c/WINDOWS/SYSTEM32/ntdll.dll (0x7ff88f2b0000)
            KERNEL32.DLL => /cygdrive/c/WINDOWS/System32/KERNEL32.DLL (0x7ff88e450000)
            KERNELBASE.dll => /cygdrive/c/WINDOWS/System32/KERNELBASE.dll (0x7ff88b9e0000)
            cygwin1.dll => /usr/bin/cygwin1.dll (0x180040000)
            cyggfortran-5.dll => /usr/bin/cyggfortran-5.dll (0x3efd20000)
            cygquadmath-0.dll => /usr/bin/cygquadmath-0.dll (0x3ee0b0000)
            cyggcc_s-seh-1.dll => /usr/bin/cyggcc_s-seh-1.dll (0x3f7000000)

Other compilers or other versions of the same compiler will probably
require different dynamic libraries. As long as you run the program on
the same computer - or, more accurately, within the same environment -
there should be no problem. However, when such a library cannot be
found, you will get (hopefully) an error message and the program stops
immediately.

Therefore it is good to know what libraries are required. On Linux and
Linux-like environments, the "ldd" utility is a great help. On Windows,
you may want to use the "dependency walker" (the latest version, which
works very nicely with Windows 10, is found here: <https://github.com/lucasg/Dependencies>)

Another thing you should know is where the program tries to find these
libraries. That is a vast subject in its own right and full of
complications and history. Here we merely scratch the surface:

_On Linux:_

   * The environment variable `LD_LIBRARY_PATH` is used. It consists of a
list of directories to be searched, each directory separated via colons
(:) from the others. For instance: `/usr/lib:/usr/local/lib` - typical
system directories.
   * At the link step you can also use an option to set `RPATH`, a list
of directories that is put into the executable file itself.
   * Then there are several system directories that are searched.

_On Windows:_

   * The directory containing the executable program may also contain
dynamic libraries.
   * The environment variable "PATH" is used. Again a list of directories
to be searched, but now the separating character is the semicolon (;).
   * A set of system directories is searched.

Unfortunately, the details can change from one version of the operating
system to the next. The above is merely an indication - use tools like
"ldd" or "dependency walker" to find out what libraries are loaded and
where they are found.

If you want to share your program with colleagues or clients or simply
users all over the world, you will have to take care that, besides the
program, you also distribute the libraries it depends on. For more
information: see below.


Include files and modules
-------------------------

Your program can be contained in various source files, all stored in the
same directory or organised in some convenient directory tree. The
details of the organisation depend on personal taste, arrangements made
by the group of developers you belong to, or simply the history of the
program. Whatever the directory structure, you will encounter a
situation where the compiler needs assistence in order to compile a
particular source file:

* Fortran (and other languages) has the possibility to include an
external file. While this feature has become a bit less useful with the
advent of modules, it still has its uses. Quite often, such "include
files" are stored in a directory separated from the directories
containing the source files, because they are used in several locations.
Quite often the name of that directory is "include".
* As we have seen, source code that defines one or more modules, leads to
so-called "module intermediate files" (with the extension ".mod"). The
compiler needs access to these files to be able to read the interfaces
and variables and what not, and based on this information, actually
compile the source code that uses the various modules.

Compilers support options like "-I" to indicate where these include
files and module intermediate files are to be found. Suppose we store
the two files of our tabulation program in the following directory
structure:

    tabulate/
        main/
            tabulate.f90
        sub/
            function.f90

Compiling the file "function.f90" with the commands

    $ cd sub
    $ gfortran -c function.f90

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

    $ cd main
    $ gfortran -c tabulate.f90 -I ../sub
    $ gfortran -o tabulate tabulate.o ../sub/function.o

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

* The details differ per compiler. Sometimes the "-I" option should be
followed by a space and then the name of the directory, sometimes the
directory should come consecutively.
* Many compilers allow you to specify the location for the module
intermediate files. For gfortran this is "-J", for instance:
-J../include (so that the .mod files could all appear in the
same directory)
* For large programs, consisting of many source files, it is important to
think through what organisation to use.


Managing libraries (static and dynamic libraries)
-------------------------------------------------

If you need to manage a program built of dozens of source files (and
that is not uncommon!), the command-line needed to specify all the
object files will be very long indeed. This soon becomes tedious or
even impossible to maintain. So a different solution is called for:
create your own libraries.

Libraries contain any number of object files in a compact form, so that
the command-line becomes far shorter:

    $ gfortran -o tabulate tabulate.f90 function.o supportlib.a

where "supportlib.a" is a collection of one, two or many object files,
all compiled and then put into a library. The extension ".a" is used by
Linux and Linux-like platforms. On Windows the extension ".lib" is used.

Creating your own libraries is not that complicated: you use a utility
like "ar" or (on Windows) "lib" to achieve this:

    $ gfortran -c file1.f90 file2.f90
    $ gfortran -c file3.f90 ...
    $ ar r supportlib.a file1.o file2.o
    $ ar r supportlib.a file3.o ...

or, using the "link" utility:

    c:\...> ifort -c file1.f90 file2.f90
    c:\...> ifort -c file3.f90 ...
    c:\...> lib /out:supportlib.lib file1.obj file2.obj
    c:\...> link supportlib.lib file3.obj ...

Note:

* The command "ar" with the option "r" either creates the library (the
name appears after the option) or adds new object files to the library
(or replaces any existing ones).
* The command "lib" will create a new library if you use specify the
option "/out:" with the name of the new library next to it. To add
object files to an existing library, leave out the "/out:" bit.
* On platforms like Linux there is a particular convention to name
libraries. If you name your library like "libname.a" (note the "lib"
prefix), then you can refer to it as "-lname" in the link step.
* Libraries are often sought in directories indicated by an option "-L"
or "/LIBPATH". This saves you from having to specify the exact path for
every library.

Using libraries you can build very large programs without
having to resort to extremely long command lines.


### Static versus dynamic libraries

The above discussion is tacitly assuming that you are using so-called
static libraries. Static libraries (or at least parts of their
contents) become an integral part of the executable program. The only
way to change the routines incorporated in the program is by rebuilding
the program with a new version of the library.

A flexible alternative is to use so-called dynamic libraries. These
libraries remain outside the executable program and as a consequence you
can replace them without rebulding the entire program. Compilers and
indeed the operating system itself rely heavily on such dynamic
libraries. You could consider dynamic libraries as a sort of executable
programs that need a bit of help to be run.

Building dynamic libraries works slightly differently:

On Linux:

    $ gfortran -fpic -c file1.f90 file2.f90
    $ gfortran -fpic -c file3.f90 ...
    $ gfortran -shared --o supportlib.so file1.o file2.o file3.o ...

On Windows, with the Intel Fortran compiler:

    $ ifort -c file1.f90 file2.f90
    $ ifort -c file3.f90 ...
    $ ifort -dll -exe:supportlib.dll file1.obj file2.obj file3.obj ...

The differences:

* You need to specify a compile option on Linux, "-fpic", because the
object code is slightly different.
* You need to tell in the link step that you want a dynamic library (on
Linux: a shared object/library, hence the extension ".so"; on Windows:
a dynamic link library)

There is one more thing to be aware of: On Windows you must
explicitly specify that a routine is to be _exported_, i.e. is visible
in the dynamic library. There are several ways - depending on the
compiler you use - to achieve this. One method is via a so-called
compiler directive:

    subroutine myroutine( ... )
    !GCC$ ATTRIBUTES DLLEXPORT:: myroutine

Or, with the Intel Fortran compiler:

    subroutine myroutine( ... )
    !DEC$ ATTRIBUTES DLLEXPORT:: myroutine

Besides a dynamic library (DLL), a so-called import library may be
generated.

Because the details differ per compiler, here are two examples:
gfortran on Cygwin and Intel Fortran on Windows. In both cases
we look at the tabulation program.

### Cygwin and gfortran
The "tabulate" program requires a user-defined routine "f". If we
let it reside in a dynamic library, say "function.dll", we can simply
replace the implementation of the function by putting another dynamic
library in the directory. No need to rebuild the program as such.

On Cygwin it is not necessary to explicitly export a routine - all
publically visible routines are exported when you build a dynamic library.
Also, no import library is generated.

Since our dynamic library can be built from a single source file, we
can take a shortcut:

    $ gfortran -shared -o function.dll function.f90

This produces the files "function.dll" and "function.mod". The
utility "nm" tells us the exact name of the function "f":

    $ nm function.dll
    ...
    000000054f9d7000 B __dynamically_loaded
                     U __end__
    0000000000000200 A __file_alignment__
    000000054f9d1030 T __function_MOD_f
    000000054f9d1020 T __gcc_deregister_frame
    000000054f9d1000 T __gcc_register_frame
    ...

It has received a prefix `__function_MOD_` to distinguish it from any
other routine "f" that might be defined in another module.

The next step is to build the program:

    $ gfortran -o tabulate tabulate.f90 function.dll

The DLL and the .mod file are used to build the executable program
with checks on the function's interface, the right name and the reference
to "a" DLL, called "function.dll".

You can replace the shared library "function.dll" by another one, implementing
a different function "f". Of course, you need to be careful to use the correct
interface for this function. The compiler/linker are not invoked anymore, so they
can do no checking.

### Windows and Intel Fortran
The setup is the same as with Cygwin, but on Windows it is necessary
to explicitly export the routines. And an import library is generated -
this is the library that should be used in the link step.

The source file must contain the compiler directive, otherwise the function f
is not exported:

    real function f( x )
    !DEC$ ATTRIBUTES DLLEXPORT :: f

Again we take a shortcut:

    $ ifort -exe:function.dll function.f90 -dll

This produces the files "function.dll", "function.mod" as well as "function.lib" (and two
other files of no importance here). The "dependency walker" program tells us
that the exact name of the function "f" is `FUNCTION_mp_F`. It is also exported, so that
it can be found by the linker in the next step:

    $ ifort tabulate.f90 function.lib

Note that we need to specify the name of the export library, not the DLL!

(Note also: the Intel Fortran compiler uses the name of the first source file as the
name for the executable - here we do without the "-exe" option.)

Just as under Cygwin, the DLL and the .mod file are used to build the executable program
with checks on the function's interface, the right name and the reference
to "a" DLL, called "function.dll".

You can replace the shared library "function.dll" by another one, but the same
caution is required: while the implementation can be quite different, the
function's interface must be the same.


Build tools
-----------

If this seems complicated, well, you are right and we are only
scratching the surface here. The complications arise because of
differences between platforms, differences between compilers/linkers and
because of differences in the way programs are set up. Fortunately,
there are many tools to help configure and maintain the build steps.
We will not try and catalogue them, but give instead a very limited
list of tools that you typically encounter:

* The "make" utility is a classical tool that uses instructions about
how the various components of a program depend on each other to
efficiently compile and link the program (or programs). It takes a
so-called makefile that contains the dependencies.

    Simply put:

    If a program file is older than any of the libraries and object files
it depends on, the make utility knows it has to rebuild it and goes on
to look at the libraries and object files - are any out of date?

    If an object file is older than the corresponding source file, the
make utility knows it has to compile the source file.

* Integrated development tools take care of many of the above details. A
popular tool on Windows is MicroSoft's Visual Studio, but others exist,
such Eclipse (Photran) and Code::Blocks. They offer a graphical
user-interface, but are often very specific for the compiler and
platform.

* Maintenance tools like autotools and CMake can generate makefiles or
Visual Studio project files via a high-level description. They abstract
away from the compiler and platform specifics.

Here is a very simple example of a makefile as used by the make utility,
just to give you an impression:

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

    $ make

(the name "Makefile" is the default, otherwise use the option "-f"). Now
only change the file "tabulate.f90" and run it again. You will see that
only that file gets compiled again and then the program is built. The
file "function.f90" was not changed, so the object file and the module
intermediate file would remain unchanged, so there is no need to recompile
it.


Distributing your programs
--------------------------
When you distributed your programs, there are a number of options you can
choose from:

1. Distribute the entire source code
2. Distribute a pre-built executable program
3. Distribute static or dynamic libraries that people can use

ad 1.

By far the simplest - for you as a programmer - is this one: you leave it
up to the user to build it on their own machine. Unfortunately, that
means you will have to have a user-friendly build system in place and
the user will have to have access to suitable compilers. For build systems:
see the previous section.

ad 2.

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

ad 3.

This option is a combination of the first two options. It does put some burden on
the user, as they must create a main program that calls your routines in the
proper way, but they do not need to know much about the build system you used.
You will have to deal with the run-time libraries, though.

If you choose this option, besides the compiled libraries, you will also need to
supply the module intermediate files. These files are compiler-specific, but so are
the static libraries you build.

### Distributing the tabulation program
As shown above, the tabulation program can be built with the user-defined function
in a dynamic library. This enables you to:

* Ship the executable (with the appropriate run-time libraries)
* Provide a skeleton version of the module, something like:

        module function
            implicit none
        contains

        real function f( x )
        !DEC$ ATTRIBUTES DLLEXPORT :: f
            real, intent(in) :: x

            ... TO BE FILLED IN ...

        end function f
        end module function

* Provide a basic build script:

        gfortran -o function.dll function.f90 -shared

  or:

        ifort -exe:function.dll function.f90 -dll

As said, you cannot control that the user has done the right thing - any
DLL "function.dll" with a function "f" would be accepted, but not necessarily
lead to a successful run.

An alternative set-up would be to change the main program into a subroutine
and have the function as an argument:

    module tabulation
        implicit none
    contains

    subroutine tabulate( f )
        interface
            real function f( x )
                real, intent(in) :: x
            end function f
        end interface

        ... actual implementation

    end subroutine tabulate

    end module tabulation

Then provide a skeleton main program:

    program tabulate_f
        use tabulation

        call tabulate( func1 )
    contains
    real function func1( x )
        real, intent(in) :: x

        ... TO BE FILLED IN ...

    end function func1
    end program tabulate_f

The advantage is that the compiler can check the interface of the
function that is passed and that the user has more freedom in the use of the
functionality provided by your library.

