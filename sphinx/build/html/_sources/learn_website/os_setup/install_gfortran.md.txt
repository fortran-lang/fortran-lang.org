---
layout: book
title: Installing GFortran
permalink: /learn/os_setup/install_gfortran
---

GFortran is the name of the [GNU Fortran project](https://gcc.gnu.org/fortran/). The main [wiki](https://gcc.gnu.org/wiki/GFortran) page offers many helpful links about GFortran, as well as Fortran in general. In this guide, the installation process for GFortran on Windows, Linux, macOS and OpenBSD is presented in a beginner-friendly format based on the information from [GFortranBinaries](https://gcc.gnu.org/wiki/GFortranBinaries).

## Windows


Three sources provide quick and easy way to install GFortran compiler on Windows:
1. [http://www.equation.com](http://www.equation.com/servlet/equation.cmd?fa=fortran), provides 32 and 64-bit x86
 executables of the latest (10.2) gcc-version.
2. [TDM GCC](https://jmeubank.github.io/tdm-gcc/articles/2020-03/9.2.0-release), provides 32 and 64-bit x86 executables of the 9.2 gcc-version.
3. [MinGW-w64](http://mingw-w64.org/doku.php/download/mingw-builds) provides a 64-bit x86 executable of the 8.1.0 gcc-version.

In all the above choices, the process is straightforward—just download the installer and follow the installation wizard.

### Unix-like development on Windows
For those familiar with a unix-like development environment, several emulation options are available on Windows each of which provide packages for gfortran:

* __Cygwin:__ A runtime environment that provides POSIX compatibility to Windows;
* __MSYS2:__ A collection of Unix-like development tools, based on modern Cygwin and MinGW-w64;
* __Windows Subsystem for Linux (WSL):__ An official compatibility layer for running Linux binary executables on Windows. With [Windows Subsystem for Linux GUI](https://github.com/microsoft/wslg) one can run text editors and other graphical programs.

All of the above approaches provide access to common shells such as bash and development tools including GNU coreutils, Make, CMake, autotools, git, grep, sed, awk, ssh, etc.

We recommend the WSL environment for those looking for a Unix-like development environment on Windows.

## Linux


### Debian-based (Debian, Ubuntu, Mint, etc...)
Check whether you have gfortran already installed
```bash
which gfortran
```
If nothing is returned then gfortran is not installed.
To install gfortran type:
```bash
sudo apt-get install gfortran
```
to check what version was installed type:
```bash
gfortran --version
```
You can install multiple versions up to version 9 by typing the version number immediately after "gfortran", e.g.:
```bash
sudo apt-get install gfortran-7 
```
To install the latest version 10 you need first to add / update the following repository and then install: 
```bash
sudo add-apt-repository ppa:ubuntu-toolchain-r/test
sudo apt update
sudo apt install gfortran-10
```
Finally, you can switch between different versions or set the default one with the **update-alternatives** ([see manpage](https://manpages.ubuntu.com/manpages/trusty/man8/update-alternatives.8.html#:~:text=update%2Dalternatives%20creates%2C%20removes%2C,system%20at%20the%20same%20time.)). There are many online tutorials on how to use this feature. A well structured one using as an example C and C++ can be found [here](https://linuxconfig.org/how-to-switch-between-multiple-gcc-and-g-compiler-versions-on-ubuntu-20-04-lts-focal-fossa), you can apply the same logic by replacing either `gcc` or `g++` with `gfortran`.

### RPM-based (Red Hat Linux, CentOS, Fedora, openSuse, Mandrake Linux)
```bash
sudo yum install gcc-gfortran
```

Since Fedora 22, `dnf` is the default package manager for Fedora:
```bash
sudo dnf install gcc-gfortran
```

### Arch-based (Arch Linux, Antergos, Manjaro, etc...)
```bash
sudo pacman -S gcc-fortran
```

## macOS
### Xcode
If you have Xcode installed, open a terminal window and type:
```bash
xcode-select --install
```
### Binaries
Go to [fxcoudert/gfortran-for-macOS](https://github.com/fxcoudert/gfortran-for-macOS/releases) to directly install binaries.
### Homebrew
```bash
brew install gcc
```
### Fink
GNU-gcc Package [link](https://pdb.finkproject.org/pdb/browse.php?summary=GNU+Compiler+Collection+Version)
### MacPorts
Search for available gcc versions:
```bash
port search gcc
```
Install a gcc version:
```bash
sudo port install gcc10
```

## OpenBSD
```bash
pkg_add g95
```

On OpenBSD, the GFortran executable is named `egfortran`. To test it, type:
```bash
egfortran -v
```

---
## OpenCoarrays

[OpenCoarrays](http://www.opencoarrays.org/) is an open-source software project that produces an application binary interface (ABI) used by the GNU Compiler Collection (GCC) Fortran front-end to build executable programs that leverage the parallel programming features of Fortran 2018. Since OpenCoarrays is not a separate compiler, we include it here, under gfortran.

While with gfortran you can compile perfectly valid code using coarrays, the generated binaries will only run in a single image (_image_ is a Fortran term for a parallel process), that is, in serial mode. OpenCoarrays allows running code in parallel on shared- and distributed-memory machines, similar to MPI:
```bash
cafrun -n <number_of_images> <executable_name>
```

The process of installation is provided in a clear and comprehensive manner on the official site. 

We emphasize that native installation on Windows is not possible. It is only possible through WSL.
