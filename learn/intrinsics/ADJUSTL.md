---
layout: book
title: adjustl
permalink: /learn/intrinsics/ADJUSTL
---
-------------------------------------------------------------------------------
## __Name__

__adjustl__(3) - \[CHARACTER\] Left-adjust a string

## __Syntax__


```fortran
    result = adjustl(string)
     character(len=*),intent(in) :: string
     character(len=len(string))  :: result
```

## __Description__

__adjustl__(STRING) will left adjust a string by removing leading
spaces. Spaces are inserted at the end of the string as needed.

## __Arguments__

  - __STRING__
    the type shall be CHARACTER.

## __Returns__

The return value is of type CHARACTER and of the same kind as STRING
where leading spaces are removed and the same number of spaces are
inserted on the end of STRING.

## __Examples__

Sample program:

```fortran
    program demo_adjustl
    implicit none
    character(len=20) :: str = '   sample string'
    character(len=:),allocatable :: astr
       !
       ! basic use
       str = adjustl(str)
       write(*,'("[",a,"]")') str, trim(str)
       !
       ! an allocatable string stays the same length
       ! and is not trimmed.
       astr='    allocatable string   '
       write(*,'("[",a,"]")') adjustl(astr)
       !
    end program demo_adjustl
```

Results:

```
   [sample string       ]
   [sample string]
   [allocatable string       ]
```

## __Standard__

Fortran 95 and later

## __See Also__

    __adjustr__(3)(ADJUSTR)

###### fortran-lang intrinsic descriptions
