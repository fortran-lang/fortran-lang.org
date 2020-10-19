---
layout: book
title: Installing GFortran
permalink: /learn/os_setup/install_gfortran
---

GFortran is the name of the [GNU Fortran project](https://gcc.gnu.org/fortran/). The main [wiki](https://gcc.gnu.org/wiki/GFortran) page offers many helpful links about GFortran, as well as Fortran in general. In this guide, the installation process for GFortran on Windows, Linux, and macOS is presented in a beginner-friendly format based on the information from [GFortranBinaries](https://gcc.gnu.org/wiki/GFortranBinaries).

## Windows


Three sources provide quick and easy way to install GFortran compiler on Windows:
1. [http://www.equation.com](http://www.equation.com/servlet/equation.cmd?fa=fortran), provides x32 and x64 executables of the latest (10.2) gcc-version.
2. [TDM GCC](https://jmeubank.github.io/tdm-gcc/articles/2020-03/9.2.0-release), provides x32 and x64 executables of the 9.2 gcc-version.
3. [Mingw-w64](http://mingw-w64.org/doku.php/download/mingw-builds) provides x64 executable of the 7.2 gcc-version.

In all the above choices, the process is straightforward—just download the installer and follow the installation wizard.

### Unix-like development on Windows
For those familiar with a unix-like development environment, several emulation options are available on Windows each of which provide packages for gfortran:

* __Cygwin:__ A runtime environment that provides POSIX compatibility to Windows;
￼* __MSYS2:__ A collection of Unix-like development tools for compiling native Windows binaries;
￼* __Windows Subsystem for Linux (WSL):__ An official compatibility layer for running Linux binary executables on Windows.

All of the above approaches provide access to common shells such as bash and development tools including GNU coreutils, Make, CMake, autotools, git, grep, sed, awk, ssh, etc.

We recommend the WSL environment for those looking for a Unix-like development environment on Windows.

Note that only MSYS2 allows the native compilation of Windows binaries; this is important if you intend to distribute your binaries or if you prioritise execution performance.
Binaries compiled in Cygwin are 'Cygwin' binaries linked against `cygwin.dll` and will hence only execute within the Cygwin environment. Binaries compiled in WSL are Linux binaries and therefore require the WSL environment to run on Windows.

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
Finally, you can switch between different versions or set the default one with the **update-alternatives** ([see manpage](http://manpages.ubuntu.com/manpages/trusty/man8/update-alternatives.8.html#:~:text=update%2Dalternatives%20creates%2C%20removes%2C,system%20at%20the%20same%20time.)). There are plenty online tutorials on how to use this feature.

### RPM-based (Red Hat Linux, CentOS, Fedora, openSuse, Mandrake Linux)
```bash
sudo yum install gcc-gfortran
```

### Arch-based (Arch Linux, Antergos, Manjaro, etc...)
```bash
sudo pacman -S gcc-fortran
```

## MacOS
### Xcode
If you have Xcode installed, open a terminal window and type:
```bash
xcode-select --install
```
### Binaries
Check this github repo: [fxcoudert/gfortran-for-macOS](https://github.com/fxcoudert/gfortran-for-macOS/releases) to directly install binaries.
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
---
## OpenCoarrays

The best way to describe [OpenCoarrays](http://www.opencoarrays.org/) is to directly quote from the official site: *OpenCoarrays is an open-source software project that produces an application binary interface (ABI) used by the GNU Compiler Collection (GCC) Fortran front-end to build executable programs that leverage the parallel programming features of Fortran 2018*. Therefore, since OpenCoarrays is not a separate compiler, we include it here, under gfortran.

The difference with gfortran is that, while with gfortran one can write perfectly valid code involving coarrays features, the generated code will only run in a single thread. OpenCoarrays allows to run code in parallel in a similar way as with MPI:
```bash
$ cafrun -n [number_of_threads] [executable_name]
```

The process of installation is provided in a clear and comprehensive manner on the official site. 

We emphasize here, that native installation in Windows is not possible. It is only possible through WSL. 
