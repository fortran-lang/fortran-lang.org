---
layout: book
title: Parallel programming
permalink: /learn/best_practices/parallel_programming
---

OpenMP
------

[OpenMP](http://www.openmp.org/) should be compatible with non-openMP
compilers. This can be enforced by prepending all OpenMP-specific calls
by `!$`. Regular compilers will consider these lines as comments and
ignore them. For OpenMP compilers, these lines will be considered as
regular Fortran code. The following code :

``` fortran
program test_openmpi
  !$ use omp_lib
  implicit none

  integer :: nthreads

  nthreads = -1
  !$ nthreads = omp_get_num_threads()

  ! will print the number of running threads when compiled with OpenMP, else will print -1
  print*, "nthreads=", nthreads
end program
```

will print the number of threads used when compiled with OpenMP. It will
print by default -1 if compiled without OpenMP.

MPI
---

There are three ways of including MPI in a fortran program:

| Fortran version | Method             | Comments                                                             |
|-----------------|--------------------|----------------------------------------------------------------------|
| Fortran 08      | `use mpi_f08`      | Consistent with F08 standard, good type-checking; highly recommended |
| Fortran 90      | `use mpi`          | Not consistent with standard, so-so type-checking; not recommended   |
| Fortran 77      | `include "mpif.h"` | Not consistent with standard, no type-checking; strongly discouraged |

On infrastructures where `use mpi_f08` is not available, one should
fallback to `use mpi`. The use of `include "mpif.h"` is strongly
discouraged, as it does not check at all the types of the argument or
that the function calls provide the good arguments. For example, you
don’t get any compiler warnings if you call a subroutine and forget a
parameter, add an extra parameter, or pass a parameter of the wrong
type. It may also lead to silent data corruption.
