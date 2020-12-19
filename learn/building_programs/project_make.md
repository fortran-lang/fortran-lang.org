---
layout: book
title: An introduction to make
permalink: /learn/building_programs/project_make
---

We briefly discussed the basics of ``make``. This chapter gives ideas
and strategies to scale ``make`` for larger projects.

Before going into detail with ``make``, consider a few points:
1. ``make`` is a Unix tool and might give you a hard time when porting to non-Unix
   platforms. That said, there are also different flavors of ``make`` available,
   not all might support the features you want to use.
2. While ``make`` gives you full control over the build process, it also
   means you are responsible for the entire build process, and you have to specify the rules for every detail of your project.
   You might find yourself spending a significant amount of time writing and
   maintaining your ``Makefile`` instead of developing your source code.
3. You can work with your ``Makefile``, but think about other developers
   on your project who may not be familiar with ``make``. How much time do you expect them to spend learning your
   ``Makefile`` and would they be able to debug or add features?
4. Pure ``make`` will not scale. You will soon add auxiliary programs
   to dynamically or statically generate your ``Makefile``. Those introduce
   dependencies and possible sources of errors. The effort needed to test and document those
   tools should not be underestimated.

If you think ``make`` is suitable for your needs, than you can start writing
your ``Makefile``. For this course we will use real world examples from the
package index, which (at the time of writing) use build systems other
than ``make``. This guide should present a general recommended style to write
``make``, but also serve as demonstration of useful and interesting features.

{% include tip.html content="Even if you find ``make`` unsuitable to build your project, it is *the* tool to automate workflows defined by files. Maybe you can leverage its power in a different context." %}


## Getting started

For this part we will work with
<a href="https://github.com/jacobwilliams/fortran-csv-module/tree/1.2.0" target="_blank" rel="noopener"> the Fortran CSV module (v1.2.0)</a>.
Our goal is to write a ``Makefile`` to compile this project to a static library.
Start by cloning the repository

```
git clone https://github.com/jacobwilliams/fortran-csv-module -b 1.2.0
cd fortran-csv-module
```

{% include note.html content="For this part we will work with the code from tag ``1.2.0``, to make it as reproducible as possible. Feel free to use the latest version or another project instead." %}

This project uses FoBiS as build system, and you can check the
``build.sh`` for options used with FoBiS. We are about to write a ``Makefile``
for this project. First, we check the directory structure and the source files

    .
    ├── build.sh
    ├── files
    │   ├── test_2_columns.csv
    │   └── test.csv
    ├── fortran-csv-module.md
    ├── LICENSE
    ├── README.md
    └── src
        ├── csv_kinds.f90
        ├── csv_module.F90
        ├── csv_parameters.f90
        ├── csv_utilities.f90
        └── tests
            ├── csv_read_test.f90
            ├── csv_test.f90
            └── csv_write_test.f90

We find seven different Fortran source files; the four in ``src`` should
be compiled and added to a static library while the three in ``src/tests``
contain individual programs that depend on this static library.

Start by creating a simple ``Makefile``:

```make
# Disable the default rules
MAKEFLAGS += --no-builtin-rules --no-builtin-variables

# Project name
NAME := csv

# Configuration settings
FC := gfortran
AR := ar rcs
LD := $(FC)
RM := rm -f

# List of all source files
SRCS := src/csv_kinds.f90 \
        src/csv_module.F90 \
        src/csv_parameters.f90 \
        src/csv_utilities.f90
TEST_SRCS := src/tests/csv_read_test.f90 \
             src/tests/csv_test.f90 \
             src/tests/csv_write_test.f90

# Create lists of the build artefacts in this project
OBJS := $(addsuffix .o, $(SRCS))
TEST_OBJS := $(addsuffix .o, $(TEST_SRCS))
LIB := $(patsubst %, lib%.a, $(NAME))
TEST_EXE := $(patsubst %.f90, %.exe, $(TEST_SRCS))

# Declare all public targets
.PHONY: all clean
all: $(LIB) $(TEST_EXE)

# Create the static library from the object files
$(LIB): $(OBJS)
	$(AR) $@ $^

# Link the test executables
$(TEST_EXE): %.exe: %.f90.o $(LIB)
	$(LD) -o $@ $^

# Create object files from Fortran source
$(OBJS) $(TEST_OBJS): %.o: %
	$(FC) -c -o $@ $<

# Define all module interdependencies
csv_kinds.mod := src/csv_kinds.f90.o
csv_module.mod := src/csv_module.F90.o
csv_parameters.mod := src/csv_parameters.f90.o
csv_utilities.mod := src/csv_utilities.f90.o
src/csv_module.F90.o: $(csv_utilities.mod)
src/csv_module.F90.o: $(csv_kinds.mod)
src/csv_module.F90.o: $(csv_parameters.mod)
src/csv_parameters.f90.o: $(csv_kinds.mod)
src/csv_utilities.f90.o: $(csv_kinds.mod)
src/csv_utilities.f90.o: $(csv_parameters.mod)
src/tests/csv_read_test.f90.o: $(csv_module.mod)
src/tests/csv_test.f90.o: $(csv_module.mod)
src/tests/csv_write_test.f90.o: $(csv_module.mod)

# Cleanup, filter to avoid removing source code by accident
clean:
	$(RM) $(filter %.o, $(OBJS) $(TEST_OBJS)) $(filter %.exe, $(TEST_EXE)) $(LIB) $(wildcard *.mod)
```

Invoking ``make`` should build the static library and the test executables as
expected:

    gfortran -c -o src/csv_kinds.f90.o src/csv_kinds.f90
    gfortran -c -o src/csv_parameters.f90.o src/csv_parameters.f90
    gfortran -c -o src/csv_utilities.f90.o src/csv_utilities.f90
    gfortran -c -o src/csv_module.F90.o src/csv_module.F90
    ar rcs libcsv.a src/csv_kinds.f90.o src/csv_module.F90.o src/csv_parameters.f90.o src/csv_utilities.f90.o
    gfortran -c -o src/tests/csv_read_test.f90.o src/tests/csv_read_test.f90
    gfortran -o src/tests/csv_read_test.exe src/tests/csv_read_test.f90.o libcsv.a
    gfortran -c -o src/tests/csv_test.f90.o src/tests/csv_test.f90
    gfortran -o src/tests/csv_test.exe src/tests/csv_test.f90.o libcsv.a
    gfortran -c -o src/tests/csv_write_test.f90.o src/tests/csv_write_test.f90
    gfortran -o src/tests/csv_write_test.exe src/tests/csv_write_test.f90.o libcsv.a

There are a few things to note there, a ``make`` build usually interlaces the
build artifacts and the source code, unless you put extra effort into implementing
a build directory.
Also, right now the the source files and dependencies are specified explicitly,
which results in several additional lines even for such a simple project.


## Automatically generated dependencies

The main drawback of ``make`` for Fortran is the missing capability to
determine module dependencies. This is usually solved by either adding those
by hand or automatically scanning the source code with an external tool.
Some compilers (like the Intel Fortran compiler) also offer to generate dependencies in ``make`` format.

Before diving into the dependency generation, we will outline the concept of
a robust take on the dependency problem.
First, we want an approach that can process all source files independently,
while each source file provides (``module``) or requires (``use``) modules.
When generating the dependencies only the name of the source file and the
module files are known, and no information on the object file names should be
required.

If you check the dependency section above you will note that all dependencies are
defined between object files rather than source files. To change this, we can
generate a map from the source files their respective object files:

```make
# Define a map from each file name to its object file
obj = $(src).o
$(foreach src, $(SRCS) $(TEST_SRCS), $(eval $(src) := $(obj)))
```

Note the declaration of ``obj`` as recursively expanded variable, we effectively
use this mechanism to define a function in ``make``. The ``foreach`` function
allows us to loop over all source files, while the ``eval`` function allows us
to generate ``make`` statements and evaluate them for this ``Makefile``.

We adjust the dependencies accordingly as we can now define the name of the
object files through the source file names:

```make
# Define all module interdependencies
csv_kinds.mod := $(src/csv_kinds.f90)
csv_module.mod := $(src/csv_module.F90)
csv_parameters.mod := $(src/csv_parameters.f90)
csv_utilities.mod := $(src/csv_utilities.f90)
$(src/csv_module.F90): $(csv_utilities.mod)
$(src/csv_module.F90): $(csv_kinds.mod)
$(src/csv_module.F90): $(csv_parameters.mod)
$(src/csv_parameters.f90): $(csv_kinds.mod)
$(src/csv_utilities.f90): $(csv_kinds.mod)
$(src/csv_utilities.f90): $(csv_parameters.mod)
$(src/tests/csv_read_test.f90): $(csv_module.mod)
$(src/tests/csv_test.f90): $(csv_module.mod)
$(src/tests/csv_write_test.f90): $(csv_module.mod)
```

The same strategy of creating a map is already used for the module files, now
it is just expanded to the object files as well.

To generate the respective dependency map automatically we will use an
``awk`` script here

```awk
#!/usr/bin/awk -f

BEGIN {
    IGNORECASE = 1
}

$1 ~ /^module$/ &&
$2 ~ /^[a-zA-Z][a-zA-Z1-9_]*$/ {
    if (modc[FILENAME,$2]++ == 0) {
        mod[++im] = sprintf("%s.mod = $(%s)", tolower($2), FILENAME)
    }
}

$1 ~ /^use$/ &&
$2 ~ /^[a-zA-Z][a-zA-Z1-9_]*,?$/ {
    gsub(/,/, "", $2)
    if (usec[FILENAME,$2]++ == 0) {
        use[++iu] = sprintf("$(%s) += $(%s.mod)", FILENAME, tolower($2))
    }
}

$1 ~ /^(#:?)?include$/ &&
$2 ~ /^["'].+["']$/ {
    gsub(/'|"/, "", $2)
    if (incc[FILENAME,$2]++ == 0) {
        inc[++ii] = sprintf("$(%s) += %s", FILENAME, $2)
    }
}

END {
    for (i in mod) print mod[i]
    for (i in use) print use[i]
    for (i in inc) print inc[i]
}
```

This script makes a few assumptions about the source code it parses, so it will not work with all Fortran code, but for this example it will suffice.

{% capture note %}

The above script uses the ``awk`` language, which is designed for the purpose
of text stream processing. In ``awk`` you can define groups which are evaluated
on certain events, *e.g.* when a line matches a specific pattern.

This ``awk`` script defines five groups, two of them use the special pattern
``BEGIN`` and ``END`` which are run before the script starts and after the script
finishes, respectively.
Before the script starts we make the script case-insensitive since we are dealing
with Fortran source code here.
We also use the special variable ``FILENAME`` to determine which file we are
currently parsing and to allow processing multiple files at once.

With the three patterns defined we are looking for ``module``, ``use`` and
``include`` statements as the first space delimited entry. With the used
pattern not all valid Fortran code will be parsed correctly.
A failing example would be:

```fortran
use::my_module,only:proc
```

To make this parsable by the ``awk`` script we can add another group directly
after the ``BEGIN`` group, modifying the stream while processing it with

```awk
{
   gsub(/,|:/, " ")
}
```

In theory you would need a full Fortran parser to deal with continuation lines
and other difficulties. This might be possible to implement in ``awk`` but
would require a huge script in the end.

Also, keep in mind that generating the dependencies should be fast, an expensive
parser can produce a significant overhead when generating dependencies for a large
code base. Making reasonable assumptions can simplify and speed up this step, but
also introduces an error source in your build tools.

{% endcapture %}
{% include tip.html title="Using awk" content=note %}

Make the script executable (``chmod +x gen-deps.awk``) and test it with
``./gen-deps.awk $(find src -name '*.[fF]90')``. You should see output like this:

    csv_utilities.mod = $(src/csv_utilities.f90)
    csv_kinds.mod = $(src/csv_kinds.f90)
    csv_parameters.mod = $(src/csv_parameters.f90)
    csv_module.mod = $(src/csv_module.F90)
    $(src/csv_utilities.f90) += $(csv_kinds.mod)
    $(src/csv_utilities.f90) += $(csv_parameters.mod)
    $(src/csv_kinds.f90) += $(iso_fortran_env.mod)
    $(src/tests/csv_read_test.f90) += $(csv_module.mod)
    $(src/tests/csv_read_test.f90) += $(iso_fortran_env.mod)
    $(src/tests/csv_write_test.f90) += $(csv_module.mod)
    $(src/tests/csv_write_test.f90) += $(iso_fortran_env.mod)
    $(src/tests/csv_test.f90) += $(csv_module.mod)
    $(src/tests/csv_test.f90) += $(iso_fortran_env.mod)
    $(src/csv_parameters.f90) += $(csv_kinds.mod)
    $(src/csv_module.F90) += $(csv_utilities.mod)
    $(src/csv_module.F90) += $(csv_kinds.mod)
    $(src/csv_module.F90) += $(csv_parameters.mod)
    $(src/csv_module.F90) += $(iso_fortran_env.mod)

Note that the scripts output will use recursively expanded variables and not
define any dependencies yet, because out-of-order declaration of variables
might be necessary and we do not want to create any target by accident.
You can verify that the same information as in the above handwritten snippet is
present. The only exception is the additional dependency on the
``iso_fortran_env.mod``, since it is an undefined variable it will just expand
to an empty string and not introduce any further dependencies.

Now, you can finally include this piece in your ``Makefile`` to automate the
dependency generation:

```make
# Disable the default rules
MAKEFLAGS += --no-builtin-rules --no-builtin-variables

# Project name
NAME := csv

# Configuration settings
FC := gfortran
AR := ar rcs
LD := $(FC)
RM := rm -f
GD := ./gen-deps.awk

# List of all source files
SRCS := src/csv_kinds.f90 \
        src/csv_module.F90 \
        src/csv_parameters.f90 \
        src/csv_utilities.f90
TEST_SRCS := src/tests/csv_read_test.f90 \
             src/tests/csv_test.f90 \
             src/tests/csv_write_test.f90

# Define a map from each file name to its object file
obj = $(src).o
$(foreach src, $(SRCS) $(TEST_SRCS), $(eval $(src) := $(obj)))

# Create lists of the build artefacts in this project
OBJS := $(addsuffix .o, $(SRCS))
DEPS := $(addsuffix .d, $(SRCS))
TEST_OBJS := $(addsuffix .o, $(TEST_SRCS))
TEST_DEPS := $(addsuffix .d, $(TEST_SRCS))
LIB := $(patsubst %, lib%.a, $(NAME))
TEST_EXE := $(patsubst %.f90, %.exe, $(TEST_SRCS))

# Declare all public targets
.PHONY: all clean
all: $(LIB) $(TEST_EXE)

# Create the static library from the object files
$(LIB): $(OBJS)
	$(AR) $@ $^

# Link the test executables
$(TEST_EXE): %.exe: %.f90.o $(LIB)
	$(LD) -o $@ $^

# Create object files from Fortran source
$(OBJS) $(TEST_OBJS): %.o: % | %.d
	$(FC) -c -o $@ $<

# Process the Fortran source for module dependencies
$(DEPS) $(TEST_DEPS): %.d: %
	$(GD) $< > $@

# Define all module interdependencies
include $(DEPS) $(TEST_DEPS)
$(foreach dep, $(OBJS) $(TEST_OBJS), $(eval $(dep): $($(dep))))

# Cleanup, filter to avoid removing source code by accident
clean:
	$(RM) $(filter %.o, $(OBJS) $(TEST_OBJS)) $(filter %.d, $(DEPS) $(TEST_DEPS)) $(filter %.exe, $(TEST_EXE)) $(LIB) $(wildcard *.mod)
```

Here additional dependency files are generated for each source file individually
and than included into the main ``Makefile``.
Also, the dependency files are added as dependency to the object files to ensure
they are generated before the object is compiled. The pipe character in
the dependencies defines an order of the rules without a timestamp dependency,
because it is not necessary to recompile an object file in case dependencies are
regenerated and potentially unchanged.

Again, we make use of the ``eval`` function to generate the dependencies in a
``foreach`` loop over all object files. Note that we created a map between
the object files in the dependency files, expanding ``dep`` once yields the
object file name, expanding it again yields the object files it depends on.

Building your project with ``make`` should give an output similar to

    ./gen-deps.awk src/csv_utilities.f90 > src/csv_utilities.f90.d
    ./gen-deps.awk src/csv_parameters.f90 > src/csv_parameters.f90.d
    ./gen-deps.awk src/csv_module.F90 > src/csv_module.F90.d
    ./gen-deps.awk src/csv_kinds.f90 > src/csv_kinds.f90.d
    gfortran -c -o src/csv_kinds.f90.o src/csv_kinds.f90
    gfortran -c -o src/csv_parameters.f90.o src/csv_parameters.f90
    gfortran -c -o src/csv_utilities.f90.o src/csv_utilities.f90
    gfortran -c -o src/csv_module.F90.o src/csv_module.F90
    ar rcs libcsv.a src/csv_kinds.f90.o src/csv_module.F90.o src/csv_parameters.f90.o src/csv_utilities.f90.o
    ./gen-deps.awk src/tests/csv_read_test.f90 > src/tests/csv_read_test.f90.d
    gfortran -c -o src/tests/csv_read_test.f90.o src/tests/csv_read_test.f90
    gfortran -o src/tests/csv_read_test.exe src/tests/csv_read_test.f90.o libcsv.a
    ./gen-deps.awk src/tests/csv_test.f90 > src/tests/csv_test.f90.d
    gfortran -c -o src/tests/csv_test.f90.o src/tests/csv_test.f90
    gfortran -o src/tests/csv_test.exe src/tests/csv_test.f90.o libcsv.a
    ./gen-deps.awk src/tests/csv_write_test.f90 > src/tests/csv_write_test.f90.d
    gfortran -c -o src/tests/csv_write_test.f90.o src/tests/csv_write_test.f90
    gfortran -o src/tests/csv_write_test.exe src/tests/csv_write_test.f90.o libcsv.a

Once the dependency files are generated, ``make`` will only update them if the
source changes and not require to rebuild them again for every invocation.

{% include tip.html content="With correct dependencies you can leverage parallel execution of your ``Makefile``, just use the ``-j`` flag to create multiple ``make`` processes." %}

Since dependencies can now be generated automatically, there is no need to specify
the source files explicitly, the ``wildcard`` function can be used to determine
them dynamically:

```make
# List of all source files
SRCS := $(wildcard src/*.f90) \
        $(wildcard src/*.F90)
TEST_SRCS := $(wildcard src/tests/*.f90)
```
