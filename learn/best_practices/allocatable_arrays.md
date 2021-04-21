---
layout: book
title: Allocatable Arrays
permalink: /learn/best_practices/allocatable_arrays
---

When using allocatable arrays (as opposed to pointers), Fortran manages
the memory automatically and it is not possible to create memory leaks.

For example you can allocate it inside a subroutine:

``` fortran
subroutine foo(lam)
real(dp), allocatable, intent(out) :: lam(:)
allocate(lam(5))
end subroutine
```

And use somewhere else:

``` fortran
real(dp), allocatable :: lam(:)
call foo(lam)
```

When the `lam` symbol goes out of scope, Fortran will deallocate it. If
`allocate` is called twice on the same array, Fortran will abort with a
runtime error. One can check if `lam` is already allocated and
deallocate it if needed (before another allocation):

``` fortran
if (allocated(lam)) deallocate(lam)
allocate(lam(10))
```
