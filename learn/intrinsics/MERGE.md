---
layout: book
title: merge
permalink: /learn/intrinsics/MERGE
---
### NAME

__merge__(3f) - \[ARRAY CONSTRUCTION\] Merge variables

### SYNTAX

result = __MERGE__(TSOURCE, FSOURCE, MASK)

### DESCRIPTION

The elemental function __MERGE__(3f) selects values from two arrays or
scalars according to a logical mask. The result is equal to an element
of TSOURCE where the corresponding element of MASK is .true., or an
element of FSOURCE when it is .false. .

Multi-dimensional arrays are supported.

Note that argument expressions to __MERGE__(3f) are not required to be
short-circuited so (as an example) if the array X contains zero values
in the statement below the standard does not prevent floating point
divide by zero being generated; as 1.0/x may be evaluated for all values
of x before the mask is used to select which value to retain:

```fortran
      y = merge( 1.0\/x, 0.0, x \/= 0.0 )
```

Note the compiler is also free to short-circuit or to generate an
infinity so this may work in many programming environments but is not
recommended.

For cases like this one may instead use masked assignment via the WHERE
construct:

```fortran
      where(x .ne. 0.0)
         y = 1.0\/x
      elsewhere
         y = 0.0
      endwhere
```

instead of the more obscure

```fortran
      merge(1.0\/merge(x,1.0,x \/= 0.0), 0.0, x \/= 0.0)
```

### ARGUMENTS

  - __TSOURCE__
    May be of any type, including user-defined.

  - __FSOURCE__
    Shall be of the same type and type parameters as TSOURCE.

  - __MASK__
    Shall be of type LOGICAL.

Note that (currently) CHARACTER values must be of the same length.

### RETURN VALUE

The result is of the same type and type parameters as TSOURCE. For any
element the result is TSOURCE if MASK is true and FSOURCE otherwise.

### EXAMPLES

The value of

```fortran
     merge (1.0, 0.0, k > 0)
```

is 1.0 for K=5 and 0.0 for K=__-2__.

```fortran
   program demo_merge
   implicit none
   integer :: tvals(2,3), fvals(2,3), answer(2,3)
   logical :: mask(2,3)
   integer :: i
   logical :: chooseleft

      tvals(1,:)=[  10, -60,  50 ]
      tvals(2,:)=[ -20,  40, -60 ]

      fvals(1,:)=[ 0, 3, 2 ]
      fvals(2,:)=[ 7, 4, 8 ]

      mask(1,:)=[ .true.,  .false., .true. ]
      mask(2,:)=[ .false., .false., .true. ]

      write(*,*)'mask of logicals'
      answer=merge( tvals, fvals, mask )
      call printme()

      write(*, *)'highest values'
      answer=merge( tvals, fvals, tvals > fvals )
      call printme()

      write(*, *)'lowest values'
      answer=merge( tvals, fvals, tvals < fvals )
      call printme()

      write(*, *)'zero out negative values'
      answer=merge( tvals, 0, tvals < 0)
      call printme()

      write(*, *)'binary choice'
      chooseleft=.false.
      write(*, '(3i4)')merge([1,2,3],[10,20,30],chooseleft)
      chooseleft=.true.
      write(*, '(3i4)')merge([1,2,3],[10,20,30],chooseleft)

      contains
      subroutine printme()
         write(*, '(3i4)')(answer(i, :), i=1, size(answer, dim=1))
      end subroutine printme
```

> end program demo\_merge
>
> ```
>     mask of logicals
>      10   3  50
>       7   4 -60
>     highest values
>      10   3  50
>       7  40   8
>     lowest values
>       0 -60   2
>     -20   4 -60
>     zero out negative values
>       0 -60   0
>     -20   0 -60
>     binary choice
>      10  20  30
>       1   2   3
> ```

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure\|Elemental function

##### @urbanjost
