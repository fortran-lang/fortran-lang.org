---
layout: book
title: is_iostat_eor
permalink: /learn/intrinsics/IS_IOSTAT_EOR
---
## __Name__

__is\_iostat\_eor__(3) - \[STATE\] Test for end-of-record value


## __Syntax__
```fortran
result = is_iostat_eor(i)
```
## __Description__

is\_iostat\_eor tests whether an variable has the value of the I/O
status "end of record". The function is equivalent to comparing the
variable with the iostat\_eor parameter of the intrinsic module
__iso\_fortran\_env__.

## __Arguments__

  - __i__
    : Shall be of the type _integer_.

## __Returns__

Returns a _logical_ of the default kind, which .true. if __i__ has the value
which indicates an end of file condition for iostat= specifiers, and is
.false. otherwise.

## __Examples__

Sample program:

```fortran
program demo_is_iostat_eor
implicit none
integer :: stat, i(50)

  open(88, file='test.dat', form='unformatted')
  read(88, iostat=stat) i

  if(is_iostat_eor(stat)) stop 'end of record'

end program demo_is_iostat_eor
```

## __Standard__

Fortran 2003 and later

###### fortran-lang intrinsic descriptions
