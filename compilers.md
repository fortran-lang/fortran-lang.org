---
layout: page
title: Fortran Compilers
---

Fortran has several open source and commercial compilers.

## Open source compilers

### GNU Fortran Compiler

[GNU Fortran Compiler (gfortran)](https://gcc.gnu.org/fortran/) is a mature
free and open source compiler, part of the GNU Compiler Collection.

[OpenCoarrays](http://www.opencoarrays.org/) is a library and compiler wrapper
around gfortran which enables the parallel programming features of Fortran 2018
with gfortran.

### LFortran

[LFortran](https://lfortran.org) is a modern, interactive, LLVM-based Fortran
compiler.

### LLVM Flang

[Flang](https://github.com/llvm/llvm-project/tree/master/flang)
is a new front-end for Fortran 2018 that has been recently
added to LLVM.
It is implemented in modern C++ and uses a Fortran-oriented MLIR dialect for lowering to LLVM IR.
This project is under active development.


### Current Flang

[Flang](https://github.com/flang-compiler/flang) is an open source compiler 
based on the NVIDIA/PGI commercial compiler.


## Commercial compilers

### Intel

[Intel Parallel Studio XE](https://software.intel.com/en-us/parallel-studio-xe)
is Intel's suite of compilers, tools, and libraries for Fortran, C, C++, and
Python. Intel offers the Linux edition of their compiler suite for free for
[open source contributors](https://software.intel.com/en-us/parallel-studio-xe/choose-download/open-source-contributor).

### NAG

The latest [NAG Fortran Compiler](https://www.nag.com/nag-compiler)
release (7.0) has extensive support for legacy and modern Fortran features including parallel programming with coarrays, as well as additional support for programming with OpenMP.

The Compiler also provides significant support for Fortran 2018 (atomic
operations, events and tasks, plus other smaller features), almost all of
Fortran 2008, complete coverage of Fortran 2003, and all of OpenMP 3.1. All
platforms include supporting tools for software development: source file
polishers, dependency generator for module and include files, call-graph
generator, interface builder and a precision unifier.

### PGI

[PGI compilers](https://www.pgroup.com/products/index.htm)
deliver the performance you need on CPUs, with OpenACC and CUDA
Fortran for HPC applications development on GPU-accelerated systems. OpenACC and
CUDA programs can run several times faster on a single Tesla V100 GPU compared
to all the cores of a dual-socket server, and interoperate with MPI and OpenMP
to deliver the full power of today’s multi-GPU servers.

### Cray

The [Cray Compiler Environment (CCE)](https://www.cray.com/sites/default/files/SB-Cray-Programming-Environment.pdf)
is the cornerstone innovation of Cray's adaptive computing paradigm. CCE builds
on a well-developed and sophisticated Cray technology base that identifies
regions of computation that are either sequential scalar, vector parallel or
highly multithreaded. It includes optimizing compilers that automatically
exploit the scalar, vector and multithreading hardware capabilities of the Cray
system. CCE supports Fortran, C and C++.

### IBM

[IBM® XL Fortran](https://www.ibm.com/us-en/marketplace/xl-fortran-linux-compiler-power)
for Linux is an industry standards-based programming tool used to develop large
and complex applications in the Fortran programming language. It generates code
that leverages the capabilities of the latest POWER9 architecture and maximizes
your hardware utilization. IBM XL Fortran for Linux optimizes your
infrastructure on IBM Power Systems™ in support of extensive numerical,
scientific and high-performance computing.

### AMD

The [AMD Optimizing C/C++ Compiler (AOCC)](https://developer.amd.com/amd-aocc/)
compiler system is a high performance, production quality code generation tool.
The AOCC environment provides various options to developers when building and
optimizing C, C++, and Fortran applications targeting 32-bit and 64-bit Linux®
platforms. The AOCC compiler system offers a high level of advanced
optimizations, multi-threading and processor support that includes global
optimization, vectorization, inter-procedural analyses, loop transformations,
and code generation. AMD also provides highly optimized libraries, which extract
the optimal performance from each x86 processor core when utilized. The AOCC
Compiler Suite simplifies and accelerates development and tuning for x86
applications.


### ARM

[Linux user-space Fortran compiler](https://developer.arm.com/tools-and-software/server-and-hpc/compile/arm-compiler-for-linux/arm-fortran-compiler).
Tailored for HPC and scientific codes, with support for popular Fortran and
OpenMP standards and tuned for leading server-class Arm-based platforms. Built
on the open source Flang front-end, and the LLVM‑based optimization and code
generation back-end. Available as part of the Arm Compiler for Linux package.
