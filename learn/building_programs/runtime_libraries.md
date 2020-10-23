---
layout: book
title: Run-time libraries
permalink: /learn/building_programs/runtime_libraries
---

To illustrate that even a simple program depends on external run-time
libraries, here is the output from the `ldd` utility that reports such
dependencies:

```shell
$ ldd tabulate.exe
        ntdll.dll => /cygdrive/c/WINDOWS/SYSTEM32/ntdll.dll (0x7ff88f2b0000)
        KERNEL32.DLL => /cygdrive/c/WINDOWS/System32/KERNEL32.DLL (0x7ff88e450000)
        KERNELBASE.dll => /cygdrive/c/WINDOWS/System32/KERNELBASE.dll (0x7ff88b9e0000)
        cygwin1.dll => /usr/bin/cygwin1.dll (0x180040000)
        cyggfortran-5.dll => /usr/bin/cyggfortran-5.dll (0x3efd20000)
        cygquadmath-0.dll => /usr/bin/cygquadmath-0.dll (0x3ee0b0000)
        cyggcc_s-seh-1.dll => /usr/bin/cyggcc_s-seh-1.dll (0x3f7000000)
```

Other compilers or other versions of the same compiler will probably
require different dynamic libraries. As long as you run the program on
the same computer – or, more accurately, within the same environment –
there should be no problem. However, when such a library cannot be
found, you will get (hopefully) an error message and the program stops
immediately.

Therefore it is good to know what libraries are required. On Linux and
Linux-like environments, the `ldd` utility is a great help. On Windows,
you may want to use the `dependency walker` (the latest version, which
works very nicely with Windows 10, is found here: <https://github.com/lucasg/Dependencies>)

Another thing you should know is where the program tries to find these
libraries. That is a vast subject in its own right and full of
complications and history. Here we merely scratch the surface:

_On Linux:_

   * The environment variable `LD_LIBRARY_PATH` is used. It consists of a
list of directories to be searched, each directory separated via colons
(:) from the others. For instance: `/usr/lib:/usr/local/lib` – typical
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
system to the next. The above is merely an indication – use tools like
"ldd" or "dependency walker" to find out what libraries are loaded and
where they are found.

If you want to share your program with colleagues or clients or simply
users all over the world, you will have to take care that, besides the
program, you also distribute the libraries it depends on. For more
information: see below.

