---
layout: book
title: new_line
permalink: /learn/intrinsics/NEW_LINE
---
## __Name__

__new\_line__(3) - \[CHARACTER\] New line character
(GFDL)

## __Syntax__
```fortran
result = new_line(c)
```
## __Description__

__new\_line(c)__ returns the new-line character.

## __Arguments__

  - __C__
    : The argument shall be a scalar or array of the type _character_.

## __Returns__

Returns a _character_ scalar of length one with the new-line character of
the same kind as parameter C.

## __Examples__

Sample program:

```fortran
program demo_new_line
implicit none

   write(*,'(A)') 'This is record 1.'//NEW_LINE('A')//'This is record 2.'

end program demo_new_line
```
## __Standard__

Fortran 2003 and later
