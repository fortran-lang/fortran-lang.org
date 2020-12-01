---
layout: book
title: Building programs
permalink: /learn/building_programs
author: Arjen Markus, Ondřej Čertík, Milan Curcic, Laurence Kedward, Brad Richardson, Jeremie Vandenplas
---

Languages like Fortran, C, C++ and Java, to name but a few, share
certain characteristics: you write code in your language of choice but
then you have to build an executable program from that source code.
Other languages are interpreted — the source code is analysed by a
special program and taken as direct instructions. Two very simple
examples of that type of language: Windows batch files and Linux shell
scripts.

In this tutorial we concentrate on the first type of languages, with
Fortran as the main example. One advantage of compiled languages is that
the build process that you need to build an executable program, is used
to transform the human-readable source code into an efficient program
that can be run on the computer.

Remark: this tutorial gives examples for the Windows and Linux operating
systems, however the workflow and general principles still apply to macOS.

## Compiled languages

Let us have a look at a simple example:

```fortran
program hello
    write(*,*) 'Hello!'
end program hello
```

This is just about the simplest program you can write in Fortran and it
is certainly a variation on one of the most famous programs. Even though
it is simple to express in source code, a lot of things actually happen
when the executable that is built from this code runs:

* A process is started on the computer in such a way that it can write
to the console — the window (DOS-box, xterm, ...) at which you type the
program's name.
* It writes the text "Hello!" to the console. To do so it must properly
interact with the console.
* When done, it finishes, cleaning up all the resources (memory,
connection to the console etc.) it took.

Fortunately, as a programmer in a high-level language you do not need to
consider all these details. In fact, this is the sort of things that is
taken care of by the build process: the compiler and the linker.
