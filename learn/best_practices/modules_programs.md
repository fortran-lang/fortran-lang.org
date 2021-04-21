---
layout: book
title: Modules and Programs
permalink: /learn/best_practices/modules_programs
---

Only use modules and programs. Always setup a module in the following
way:

``` fortran
module ode1d
use types, only: dp, pi
use utils, only: stop_error
implicit none
private
public integrate, normalize, parsefunction, get_val, rk4step, eulerstep, &
        rk4step2, get_midpoints, rk4_integrate, rk4_integrate_inward, &
        rk4_integrate_inward2, rk4_integrate3, rk4_integrate4, &
        rk4_integrate_inward4

contains

subroutine get_val(...)
...
end subroutine
...

end module
```

The `implicit none` statement works for the whole module (so you don't
need to worry about it). By keeping the `private` empty, all your
subroutines/data types will be private to the module by default. Then
you export things by putting it into the `public` clause.

Setup programs in the following way:

``` fortran
program uranium
use fmesh, only: mesh_exp
use utils, only: stop_error, dp
use dft, only: atom
implicit none

integer, parameter :: Z = 92
real(dp), parameter :: r_min = 8e-9_dp, r_max = 50.0_dp, a = 1e7_dp
...
print *, "I am running"
end program
```

Notice the "explicit imports" (using Python terminology) in the `use`
statements. You can also use "implicit imports" like:

``` fortran
use fmesh
```

But just like in Python, this should be avoided ("explicit is better
than implicit") in most cases.
