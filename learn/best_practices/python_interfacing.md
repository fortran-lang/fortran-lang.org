---
layout: book
title: Interfacing with Python
permalink: /learn/best_practices/python_interfacing
---

Using Cython
------------

To wrap Fortran code in Python, export it to C first (see above) and
then write this Cython code:

``` cython
from numpy cimport ndarray
from numpy import empty

cdef extern:
    void c_mesh_exp(double *r_min, double *r_max, double *a, int *N,
            double *mesh)

def mesh_exp(double r_min, double r_max, double a, int N):
    cdef ndarray[double, mode="c"] mesh = empty(N, dtype=double)
    c_mesh_exp(&r_min, &r_max, &a, &N, &mesh[0])
    return mesh
```

The memory is allocated and owned (reference counted) by Python, and a
pointer is given to the Fortran code. Use this approach for both "in"
and "out" arrays.

Notice that we didn't write any C code --- we only told fortran to use
the C calling convention when producing the ".o" files, and then we
pretended in Cython, that the function is implemented in C, but in fact,
it is linked in from Fortran directly. So this is the most direct way of
calling Fortran from Python. There is no intermediate step, and no
unnecessary processing/wrapping involved.

Using ctypes
------------

Alternatively, you can assign C-callable names to your Fortran routines
like this:

``` fortran
subroutine mesh_exp(r_min, r_max, a, N, mesh) bind(c, name='mesh_exp')
  real(c_double), intent(in), value :: r_min
  real(c_double), intent(in), value :: r_max
  real(c_double), intent(in), value :: a
  integer(c_int), intent(in), value :: N
  real(c_double), intent(out) :: mesh(N)

  ! ...

end subroutine mesh_exp
```

and use the builtin [ctypes](http://docs.python.org/library/ctypes.html)
Python package to dynamically load shared object files containing your
C-callable Fortran routines and call them directly:

``` python
from ctypes import CDLL, POINTER, c_int, c_double
from numpy import empty

fortran = CDLL('./libmyfortranroutines.so')

mesh = empty(N, dtype="double")
fortran.mesh_exp(c_double(r_min), c_double(r_max), c_double(a), c_int(N),
                 mesh.ctypes.data_as(POINTER(c_double)))
```
