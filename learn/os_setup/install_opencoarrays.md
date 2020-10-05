---
layout: book
title: Installing OpenCoarrays
permalink: /learn/os_setup/install_opencoarrays
---

The best way to describe [OpenCoarrays](http://www.opencoarrays.org/) is to directly quote from the official site: *OpenCoarrays is an open-source software project that produces an application binary interface (ABI) used by the GNU Compiler Collection (GCC) Fortran front-end to build executable programs that leverage the parallel programming features of Fortran 2018*.

The difference with gfortran is that, while with gfortran one can write perfectly valid code involving coarrays features, the generated code will only run in a single thread. OpenCoarrays allows to run code in parallel in a similar way as in MPI:
```bash
$ cafrun -n [number_of_threads] [executable_name]
```

The process of installation is provided in a clear and comprehensive manner on the official site. 

We emphasize here, that direct installation in Windows is not possible. It is only possible through WSL. 