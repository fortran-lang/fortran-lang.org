---
layout: book
title: is_iostat_eor
permalink: /learn/intrinsics/IS_IOSTAT_EOR
---
## __Name__

__is\_iostat\_eor__(3) - \[\] Test for end-of-record value
(GFDL)

## __Syntax__

result = __is\_iostat\_eor__(i)

## __Description__

is\_iostat\_eor tests whether an variable has the value of the I/O
status "end of record". The function is equivalent to comparing the
variable with the iostat\_eor parameter of the intrinsic module
\[\[iso\_fortran\_env\]\].

## __Arguments__

  - __I__
    Shall be of the type _integer_.

## __Returns__

Returns a _logical_ of the default kind, which .true. if I has the value
which indicates an end of file condition for iostat= specifiers, and is
.false. otherwise.

## __Examples__

Sample program:

```
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
