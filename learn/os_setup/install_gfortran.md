---
layout: book
title: Installing GFortran
permalink: /learn/os_setup/install_gfortran
---

GFortran is the name of the [GNU Fortran project](https://gcc.gnu.org/fortran/). The main [wiki](https://gcc.gnu.org/wiki/GFortran) page offers many helpful and informational links about GFortran, as well as Fortran in general. In this guide, the installation process for GFortran on Windows, Linux, and MacOS is presented in a beginner-friendly format based on information from [GFortranBinaries](https://gcc.gnu.org/wiki/GFortranBinaries).

## Windows

In Windows, there are two main *vendors* that offer builds of GFortran, namely the [MinGW](http://www.mingw.org/) and the [Cygwin](https://cygwin.com/index.html). Directly derived from MinGW is the [Mingw-w64](http://mingw-w64.org/) project that offers 64-bit executables for many platforms. 

As of this date, there are three sources that provide quick and easy to install GFortran compiler under Windows:
1. [http://www.equation.com](http://www.equation.com/servlet/equation.cmd?fa=fortran), provides x32 and x64 executables of the latest (10.2) gcc-version.
2. [TDM GCC](https://jmeubank.github.io/tdm-gcc/articles/2020-03/9.2.0-release), provides x32 and x64 executables of the 9.2 gcc-version.
3. [Mingw-w64](http://mingw-w64.org/doku.php/download/mingw-builds) provides x64 executable of the 7.2 gcc-version.

In all the above choices, the process is straightforward. One has to download the installer and follow the installation wizard. 

There are two other approaches, the [MSYS2](https://www.msys2.org/) and the [WSL](https://docs.microsoft.com/en-us/windows/wsl/), however, both are considered advanced and better suited for experienced developers, preferably familiar with Linux and the use of command-line terminals. WSL is arguably the best solution for Fortran developers under Windows. WSL stands for Windows Subsystem for Linux. It is an official Windows 10 feature that allows the installation of a Linux distribution, albeit without a GUI (yet), just the Bash, directly on Windows. To activate WSL and install a Linux distribution please follow the link above with the official Microsoft instructions. The installation process of gfortran in WSL is no different than in a pure Linux installation (check below).

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
You can install multiple versions up to version 9 just by typing immidiately after gfortran[-number] e.g.:
```bash
sudo apt-get install gfortran-7 
```
To install the latest version 10 you need first to add / update the following repository and then install: 
```bash
add-apt-repository ppa:ubuntu-toolchain-r/test

apt update

apt install gfortran-10
```
Finally, you can switch between different versions or set the default one with the **update-alternatives** ([see manpage](http://manpages.ubuntu.com/manpages/trusty/man8/update-alternatives.8.html#:~:text=update%2Dalternatives%20creates%2C%20removes%2C,system%20at%20the%20same%20time.)). There are plenty online tutorials on how to use this feature.

### RPM-based (Red Hat Linux, CentOS, Fedora, openSuse, Mandrake Linux)
```bash
yum install gcc-gfortran
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
