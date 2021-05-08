---
layout: book
title: File Input/Output
permalink: /learn/best_practices/file_io
---

To read from a file:

``` fortran
integer :: u
open(newunit=u, file="log.txt", status="old")
read(u, *) a, b
close(u)
```

Write to a file:

``` fortran
integer :: u
open(newunit=u, file="log.txt", status="replace")
write(u, *) a, b
close(u)
```

To append to an existing file:

``` fortran
integer :: u
open(newunit=u, file="log.txt", position="append", status="old")
write(u, *) N, V(N)
close(u)
```

The `newunit` keyword argument to `open` is a Fortran 2008 standard, in
older compilers, just replace `open(newunit=u, ...)` by:

``` fortran
open(newunit(u), ...)
```

where the `newunit` function is defined by:

``` fortran
integer function newunit(unit) result(n)
  ! returns lowest i/o unit number not in use
  integer, intent(out), optional :: unit
  logical inuse
  integer, parameter :: nmin=10   ! avoid lower numbers which are sometimes reserved
  integer, parameter :: nmax=999  ! may be system-dependent
  do n = nmin, nmax
    inquire(unit=n, opened=inuse)
    if (.not. inuse) then
      if (present(unit)) unit=n
      return
    end if
  end do
  call stop_error("newunit ERROR: available unit not found.")
end function
```
