---
layout: book
title: Build tools
permalink: /learn/building_programs/build_tools
sd_hide_title: true
---

# Build tools

Compiling your Fortran projects by hand can become quite complicated depending
on the number of source files and the interdependencies through the module.
Supporting different compilers and linkers or different platforms can become
increasingly complicated unless the right tools are used to automatically
perform those tasks.

Depending on the size of your project and the purpose of project different
options for the build automation can be used.

First, your integrated development environment probably provides a way to build
your program. A popular cross-platform  tool is Microsoft's
<a href="https://code.visualstudio.com/" target="_blank" rel="noopener">Visual Studio Code</a>,
but others exist, such as
<a href="https://atom.io/" target="_blank" rel="noopener">Atom</a>,
<a href="https://www.eclipse.org/photran/" target="_blank" rel="noopener">Eclipse Photran</a>,
and <a href="http://www.codeblocks.org/" target="_blank" rel="noopener">Code::Blocks</a>.
They offer a graphical user-interface, but are often very specific for the
compiler and platform.

For smaller projects, the rule based build system ``make`` is a common
choice. Based on the rules defined it can perform task like (re)compiling
object files from updated source files, creating libraries and linking
executables.
To use ``make`` for your project you have to encode those rules in ``Makefile``,
which defines the interdependencies of all the final program, the intermediary
object files or libraries and the actual source files.
For a short introduction see [the guide on ``make``](#using-make-as-build-tool).

Maintenance tools like autotools and CMake can generate Makefiles or
Visual Studio project files via a high-level description. They abstract
away from the compiler and platform specifics.

Which of those tools are the best choice for your projects depends on many factors.
Choose a build tool you are comfortable working with, it should not get in your
way while developing. Spending more time on working against your build tools
than doing actual development work can quickly become frustrating.

Also, consider the accessibility of your build tools.
If it is restricted to a specific integrated development environment, can all
developers on your project access it?
If you are using a specific build system, does it work on all platforms you
are developing for?
How large is the entry barrier of your build tools? Consider the learning curve
for the build tools, the perfect build tool will be of no use, if you have to
learn a complex programming language first to add a new source file.
Finally, consider what other project are using, those you are depending on and
those that use (or will use) your project as dependency.


## Using make as build tool

The most well-known and commonly used build system is called ``make``.
It performs actions following rules defined in a configuration file
called ``Makefile`` or ``makefile``, which usually leads to compiling a program
from the provided source code.

{% include tip.html
   content="For an in-depth ``make`` tutorial lookup its info page. There is an online version of this <a href=\"https://www.gnu.org/software/make/manual/make.html\" target=\"_blank\" rel=\"noopener\">info page</a>, available."
%}

We will start with the basics from your clean source directory. Create and open
the file ``Makefile``, we start with a simple rule called *all*:

```make
all:
	echo "$@"
```

After saving the ``Makefile`` run it by executing ``make`` in the same directory.
You should see the following output:

    echo "all"
    all

First, we note that ``make`` is substituting ``$@`` for the name of the rule,
the second thing to note is that ``make`` is always printing the command it is
running, finally, we see the result of running ``echo "all"``.

{% include note.html
   content="We call the entry point of our ``Makefile`` always *all* by convention, but you can choose whatever name you like."
%}

{% capture note %}

You should not have noticed it if your editor is working correctly,
but you have to indent the content of a rule with a tab character.
In case you have problems running the above ``Makefile`` and see an error like

    Makefile:2: *** missing separator.  Stop.

The indentation is probably not correct. In this case replace the indentation
in the second line with a tab character.

{% endcapture %}
{% include note.html title="Note" content=note %}

Now we want to make our rules more complicated, therefore we add another rule:

```make
PROG := my_prog

all: $(PROG)
	echo "$@ depends on $^"

$(PROG):
	echo "$@"
```

Note how we declare variables in ``make``, you should always declare your local
variables with ``:=``. To access the content of a variable we use the ``$(...)``,
note that we have to enclose the variable name in parenthesis.

{% capture note %}

The declaration of variables is usually done with ``:=``, but ``make`` does
support *recursively expanded* variables as well with ``=``.
Normally, the first kind of declaration is wanted, as they are more predictable
and do not have a runtime overhead from the recursive expansion.
{% endcapture %}
{% include note.html title="Note" content=note %}

We introduced a dependency of the rule all, namely the content of the variable
``PROG``, also we modified the printout, we want to see all the dependencies
of this rule, which are stored in the variable ``$^``.
Now for the new rule which we name after the value of the variable ``PROG``,
it does the same thing we did before for the rule *all*, note how the value
of ``$@`` is dependent on the rule it is used in.

Again check by running the ``make``, you should see:

    echo "my_prog"
    my_prog
    echo "all depends on my_prog"
    all depends on my_prog

The dependency has been correctly resolved and evaluated before performing
any action on the rule *all*.
Let's run only the second rule: type ``make my_prog`` and you will only find
the first two lines in your terminal.

The next step is to perform some real actions with ``make``, we take
the source code from the previous chapter here and add new rules to our
``Makefile``:

```make
OBJS := tabulate.o functions.o
PROG := my_prog

all: $(PROG)

$(PROG): $(OBJS)
	gfortran -o $@ $^

$(OBJS): %.o: %.f90
	gfortran -c -o $@ $<
```

We define ``OBJS`` which stands for object files, our program depends on
those ``OBJS`` and for each object file we create a rule to make them from
a source file.
The last rule we introduced is a pattern matching rule, ``%`` is the common
pattern between ``tabulate.o`` and ``tabulate.f90``, which connects our object file
``tabulate.o`` to the source file ``tabulate.f90``.
With this set, we run our compiler, here ``gfortran`` and translate the source
file into an object file, we do not create an executable yet due to the ``-c``
flag.
Note the usage of the ``$<`` for the first element of the dependencies here.

After compiling all the object files we attempt to link the program, we do not
use a linker directly, but ``gfortran`` to produce the executable.

Now we run the build script with ``make``:

    gfortran -c -o tabulate.o tabulate.f90
    tabulate.f90:2:7:
    
        2 |    use user_functions
          |       1
    Fatal Error: Cannot open module file ‘user_functions.mod’ for reading at (1): No such file or directory
    compilation terminated.
    make: *** [Makefile:10: tabulate.f90.o] Error 1

We remember that we have dependencies between our source files, therefore we add
this dependency explicitly to the ``Makefile`` with

```make
tabulate.o: functions.o
```

Now we can retry and find that the build is working correctly. The output should
look like

    gfortran -c -o functions.o functions.f90
    gfortran -c -o tabulate.o tabulate.f90
    gfortran -o my_prog tabulate.o functions.o

You should find *four* new files in the directory now.
Run ``my_prog`` to make sure everything works as expected.
Let's run ``make`` again:

    make: Nothing to be done for 'all'.

Using the timestamps of the executable ``make`` was able to determine, it is
newer than both ``tabulate.o`` and ``functions.o``, which in turn are newer than
``tabulate.f90`` and ``functions.f90``.
Therefore, the program is already up-to-date with the latest code and no
action has to be performed.

In the end, we will have a look at a complete ``Makefile``.

```make
# Disable all of make's built-in rules (similar to Fortran's implicit none)
MAKEFLAGS += --no-builtin-rules --no-builtin-variables
# configuration
FC := gfortran
LD := $(FC)
RM := rm -f
# list of all source files
SRCS := tabulate.f90 functions.f90
PROG := my_prog

OBJS := $(addsuffix .o, $(SRCS))

.PHONY: all clean
all: $(PROG)

$(PROG): $(OBJS)
	$(LD) -o $@ $^

$(OBJS): %.o: %
	$(FC) -c -o $@ $<

# define dependencies between object files
tabulate.f90.o: functions.f90.o user_functions.mod

# rebuild all object files in case this Makefile changes
$(OBJS): $(MAKEFILE_LIST)

clean:
	$(RM) $(filter %.o, $(OBJS)) $(wildcard *.mod) $(PROG)
```

Since you are starting with ``make`` we highly recommend to always include
the first line, like with Fortran's ``implicit none`` we do not want to have
implicit rules messing up our ``Makefile`` in surprising and harmful ways.

Next, we have a configuration section where we define variables, in case you
want to switch out your compiler, it can be easily done here.
We also introduced the ``SRCS`` variable to hold all source files, which is
more intuitive than specifying object files.
We can easily create the object files by appending a ``.o`` suffix using the
functions ``addsuffix``.
The ``.PHONY`` is a special rule, which should be used for all entry points
of your ``Makefile``, here we define two entry point, we already know *all*,
the new *clean* rule deletes all the build artifacts again such that we indeed
start with a clean directory.

Also, we slightly changed the build rule for the object files to account for
appending the ``.o`` suffix instead of substituting it.
Notice that we still need to explicitly define the interdependencies in the
``Makefile``. We also added a dependency for the object files on the ``Makefile``
itself, in case you change the compiler, this will allow you to safely rebuild.

Now you know enough about ``make`` to use it for building small projects.
If you plan to use ``make`` more extensively, we have compiled a few tips
for you as well.

{% capture note %}

In this guide, we avoided and disabled a lot of the commonly used ``make``
features that can be particularly troublesome if not used correctly, we highly
recommend staying away from the builtin rules and variables if you do not feel
confident working with ``make``, but explicitly declare all variables and rules.

You will find that ``make`` is capable tool to automate short interdependent
workflows and to build small projects. But for larger projects, you will
probably soon run against some of it limitations. Usually, ``make`` is therefore
not used alone but combined with other tools to generate the ``Makefile``
completely or in parts.
{% endcapture %}
{% include note.html title="Note" content=note %}


### Recursively expanded variables

Commonly seen in many projects are recursively expanded variables (declared with
``=`` instead of ``:=``). Recursive expansion of your variables allows out-of-order
declaration and other neat tricks with ``make``, since they are defined as rules,
which are expanded at runtime, rather than being defined while parsing.

For example, declaring and using your Fortran flags with this snippet will work
completely fine:

```make
all:
	echo $(FFLAGS)

FFLAGS = $(include_dirs) -O
include_dirs += -I./include
include_dirs += -I/opt/some_dep/include
```

You should find the expected (or maybe unexpected) printout after running ``make``

    echo -I./include -I/opt/some_dep/include -O
    -I./include -I/opt/some_dep/include -O

{% include note.html content="appending with ``+=`` to an undefined variable will produce a recursively expanded variable with this state being inherited for all further appending." %}

While, it seems like an interesting feature to use, it tends to lead to
surprising and unexpected outcomes. Usually, when defining variables like your
compiler, there is little reason to actually use the recursive expansion at all.

The same can easily be archived using the ``:=`` declaration:

```make
all:
	echo $(FFLAGS)

include_dirs := -I./include
include_dirs += -I/opt/some_dep/include
FFLAGS := $(include_dirs) -O
```

{% include important.html content="always think of a ``Makefile`` as a whole set of rules, it must be parsed completely before any rule can be evaluated." %}

You can use whatever kind of variables you like most, mixing them should be done
carefully, of course. It is important to be aware of the differences between the
two kinds and the respective implications.


### Comments and whitespace

There are some caveats with whitespace and comments, which might pop up from
time to time when using ``make``. First, ``make`` does not know of any data
type except for strings and the default separator is just a space.
This means ``make`` will give a hard time trying to build a project which
has spaces in file names. If you encounter such case, renaming the file
is possibly the easiest solution at hand.

Another common problem is leading and trailing whitespace, once introduced,
``make`` will happily carry it along and it does in fact make a difference
when comparing strings in ``make``.

Those can be introduced by comments like

```make
prefix := /usr  # path to install location
install:
	echo "$(prefix)/lib"
```

While the comment will be correctly removed by ``make``, the trailing two spaces
are now part of the variable content. Run ``make`` and check that this is indeed
the case:

```
echo "/usr  /lib"
/usr  /lib
```

To solve this issue, you can either move the comment, or strip the whitespace with
the ``strip`` function instead. Alternatively, you could try to ``join`` the
strings.

```make
prefix := /usr  # path to install location
install:
	echo "$(strip $(prefix))/lib"
	echo "$(join $(join $(prefix), /), lib)"
```

All in all, none of this solutions will make your ``Makefile`` more readable,
therefore, it is prudent to pay extra attention to whitespace and comments when
writing and using ``make``.


## The meson build system

After you have learned the basics of ``make``, which we call a low-level build
system, we will introduce ``meson``, a high-level build system.
While you specify in a low-level build system how to build your program,
you can use a high-level build system to specify what to build.
A high-level build system will deal for you with how and generate
build files for a low-level build system.

There are plenty of high-level build systems available, but we will focus on
``meson`` because it is constructed to be particularly user friendly.
The default low-level build-system of ``meson`` is called ``ninja``.

Let's have a look at a complete ``meson.build`` file:

<!-- meson is unknown by the highlighter, python looks okayish -->
```python
project('my_proj', 'fortran', meson_version: '>=0.49')
executable('my_prog', files('tabulate.f90', 'functions.f90'))
```

And we are already done, the next step is to configure our low-level build system
with ``meson setup build``, you should see output somewhat similar to this

    The Meson build system
    Version: 0.53.2
    Source dir: /home/awvwgk/Examples
    Build dir: /home/awvwgk/Examples/build
    Build type: native build
    Project name: my_proj
    Project version: undefined
    Fortran compiler for the host machine: gfortran (gcc 9.2.1 "GNU Fortran (Arch Linux 9.2.1+20200130-2) 9.2.1 20200130")
    Fortran linker for the host machine: gfortran ld.bfd 2.34
    Host machine cpu family: x86_64
    Host machine cpu: x86_64
    Build targets in project: 1

    Found ninja-1.10.0 at /usr/bin/ninja

The provided information at this point is already more detailed than anything
we could have provided in a ``Makefile``, let's run the build with
``ninja -C build``, which should show something like

    [1/4] Compiling Fortran object 'my_prog@exe/functions.f90.o'.
    [2/4] Dep hack
    [3/4] Compiling Fortran object 'my_prog@exe/tabulate.f90.o'.
    [4/4] Linking target my_prog.

Find and test your program at ``build/my_prog`` to ensure it works correctly.
We note the steps ``ninja`` performed are the same we would have coded up in a
``Makefile`` (including the dependency), yet we did not have to specify them,
have a look at your ``meson.build`` file again:

```python
project('my_proj', 'fortran', meson_version: '>=0.49')
executable('my_prog', files('tabulate.f90', 'functions.f90'))
```

We only specified that we have a Fortran project (which happens to require
a certain version of ``meson`` for the Fortran support) and told ``meson``
to build an executable ``my_prog`` from the files ``tabulate.f90`` and
``functions.f90``.
We had not to tell ``meson`` how to build the project, it figured this out
by itself.

{% capture note %}

``meson`` is a cross-platform build system, the project you just specified
for your program can be used to compile binaries for your native operating
system or to cross-compile your project for other platforms.
Similarly, the ``meson.build`` file is portable and will work on different
platforms as well.
{% endcapture %}
{% include note.html title="Note" content=note %}

The documentation of ``meson`` can be found at the
<a href="https://mesonbuild.com/" target="_blank" rel="noopener">meson-build webpage</a>.


## Creating a CMake project

Similar to ``meson`` CMake is a high-level build system as well and commonly
used to build Fortran projects.

{% capture note %}

CMake follows a slightly different strategy and provides you with a complete
programming language to create your build files.
This is has the advantage that you can do almost everything with CMake,
but your CMake build files can also become as complex as the program you are
building.
{% endcapture %}
{% include note.html title="Note" content=note %}

Start by creating the file ``CMakeLists.txt`` with the content

```cmake
cmake_minimum_required(VERSION 3.7)
project("my_proj" LANGUAGES "Fortran")
add_executable("my_prog" "tabulate.f90" "functions.f90")
```

Similar to ``meson`` we are already done with our CMake build file.
We configure our low-level build files with ``cmake -B build -G Ninja``,
you should see output similar to this

    -- The Fortran compiler identification is GNU 10.2.0
    -- Detecting Fortran compiler ABI info
    -- Detecting Fortran compiler ABI info - done
    -- Check for working Fortran compiler: /usr/bin/f95 - skipped
    -- Checking whether /usr/bin/f95 supports Fortran 90
    -- Checking whether /usr/bin/f95 supports Fortran 90 - yes
    -- Configuring done
    -- Generating done
    -- Build files have been written to: /home/awvwgk/Examples/build

You might be surprised that CMake tries to use the compiler ``f95``, fortunately
this is just a symbolic link to ``gfortran`` on most systems and not the actual
``f95`` compiler.
To give CMake a better hint you can export the environment variable ``FC=gfortran``
rerunning should show the correct compiler name now

    -- The Fortran compiler identification is GNU 10.2.0
    -- Detecting Fortran compiler ABI info
    -- Detecting Fortran compiler ABI info - done
    -- Check for working Fortran compiler: /usr/bin/gfortran - skipped
    -- Checking whether /usr/bin/gfortran supports Fortran 90
    -- Checking whether /usr/bin/gfortran supports Fortran 90 - yes
    -- Configuring done
    -- Generating done
    -- Build files have been written to: /home/awvwgk/Example/build

In a similar manner you could use your Intel Fortran compiler instead to build
your project (set ``FC=ifort``).

CMake provides support for several low-level build files, since the default is
platform specific, we will just use ``ninja`` since we already used it together
with ``meson``. As before, build your project with ``ninja -C build``:

    [1/6] Building Fortran preprocessed CMakeFiles/my_prog.dir/functions.f90-pp.f90
    [2/6] Building Fortran preprocessed CMakeFiles/my_prog.dir/tabulate.f90-pp.f90
    [3/6] Generating Fortran dyndep file CMakeFiles/my_prog.dir/Fortran.dd
    [4/6] Building Fortran object CMakeFiles/my_prog.dir/functions.f90.o
    [5/6] Building Fortran object CMakeFiles/my_prog.dir/tabulate.f90.o
    [6/6] Linking Fortran executable my_prog

Find and test your program at ``build/my_prog`` to ensure it works correctly.
The steps ``ninja`` performed are somewhat different, because there is usually
more than one way to write the low-level build files to accomplish the task
of building a project. Fortunately, we do not have to concern ourselves but have
our build system handle those details for us.

Finally, we will shortly recap on our complete ``CMakeLists.txt`` to specify
our project:

```cmake
cmake_minimum_required(VERSION 3.7)
project("my_proj" LANGUAGES "Fortran")
add_executable("my_prog" "tabulate.f90" "functions.f90")
```

We specified that we have a Fortran project and told CMake to create an executable
``my_prog`` from  the files ``tabulate.f90`` and ``functions.f90``.
CMake knows the details how to build the executable from the specified sources,
so we do not have to worry about the actual steps in the build process.

{% capture note %}

CMake's offical reference can be found at the
<a href="https://cmake.org/cmake/help/latest/", target="_blank" rel="noopener">CMake webpage</a>.
It is organised in manpages, which are also available with your local CMake
installation as well using ``man cmake``. While it covers all functionality of
CMake, it sometimes covers them only very briefly.
{% endcapture %}
{% include note.html title="Note" content=note %}
