---
layout: book
title: Choosing a compiler
permalink: /learn/os_setup/choose_compiler
---

A comprehensive list of available compilers is provided in this website under [Compilers]({{site.baseurl}}/compilers). In this guide, we will focus only on those that fulfill the following criteria:
1. Free to install and use
2. Actively maintained/updated
3. Production-level ready

Out of those enlisted at the link above, and as of the time of writing this tutorial only two fully match our criteria, GFortran and OpenCoarrays, both members of the GNU Compiler Collection (GCC), while NVFortran and IFort do match under certain conditions. 

NVFortran is part of the [NVIDIA HPC SDK formerly known as PGI Compilers & Tools](https://www.pgroup.com/index.htm). It comes with both free and paid versions. The current version 20.7 is available only for Linux x86_64 systems, while the Windows version *will be available at a later date.* It worths mentioning that nvfortran comes with NVIDIA CUDA Fortran, *a small set of extensions to Fortran that supports and is built upon the CUDA computing architecture*. However, it should be noted that since version 17.1 support for compute capability 2.0 (Fermi) and lower has been removed. This is important since, at any time, only the latest compiler version is free to download. It means that many users with older GPUs will not be able to run CUDA Fortran with the current (20.7) or future versions. The readers can refer to this [wiki](https://www.wikiwand.com/en/CUDA#/GPUs_supported) to find their GPU's compute capability and cross-check with the latest CUDA SDK that supports it.

IFort (Intel's Fortran Compiler) is not free in general. However, if you are a student, educator, or open-source contributor, you may [qualify for free software](https://software.intel.com/content/www/us/en/develop/articles/qualify-for-free-software.html).

Both NVFortran(NVidia) and IFort(Intel) are very well documented and readers can find detailed information about installing and using them on their websites respectively. For that reason, in the next chapter, we will focus only on the open-source GCC compilers. In the future, when more compilers (e.g., the expected [Flang](https://github.com/llvm/llvm-project/tree/master/flang) and [LFortran](https://lfortran.org/)) fully meet our criteria, this guide will be updated accordingly.