---
layout: book
title: Interfacing with C
permalink: /learn/best_practices/c_interfacing
---

Write a C wrapper using the `iso_c_binding` module:

``` fortran
module fmesh_wrapper

  use iso_c_binding, only: c_double, c_int
  use fmesh, only: mesh_exp

  implicit none

contains

subroutine c_mesh_exp(r_min, r_max, a, N, mesh) bind(c)
  real(c_double), intent(in) :: r_min
  real(c_double), intent(in) :: r_max
  real(c_double), intent(in) :: a
  integer(c_int), intent(in) :: N
  real(c_double), intent(out) :: mesh(N)
  call mesh_exp(r_min, r_max, a, N, mesh)
end subroutine

! wrap more functions here
! ...

end module
```

You need to declare the length of all arrays (`mesh(N)`) and pass it as
a parameter. The Fortran compiler will check that the C and Fortran
types match. If it compiles, you can then trust it, and call it from C
using the following declaration:

``` c
void c_mesh_exp(double *r_min, double *r_max, double *a, int *N,
        double *mesh);
```

use it as:

``` c
int N=5;
double r_min, r_max, a, mesh[N];
c_mesh_exp(&r_min, &r_max, &a, &N, mesh);
```

No matter if you are passing arrays in or out, always allocate them in C
first, and you are (in C) responsible for the memory management. Use
Fortran to fill (or use) your arrays (that you own in C).

If calling the Fortran `exp_mesh` subroutine from the `c_exp_mesh`
subroutine is a problem (CPU efficiency), you can simply implement
whatever the routine does directly in the `c_exp_mesh` subroutine. In
other words, use the `iso_c_binding` module as a direct way to call
Fortran code from C, and you can make it as fast as needed.
