---
layout: book
title: lle
permalink: /learn/intrinsics/LLE
---
### NAME

__lle__(3f) - \[CHARACTER\] Lexical less than or equal
(GFDL)

### SYNTAX

result = __lle__(STR\_A, STR\_B)

__character__(len=\*),intent(in) :: STR\_A, STR\_B or
__character__(len=\*),intent(in) :: STR\_A, __STR\_B__(\*) logical ::
result

### DESCRIPTION

Determines whether one string is lexically less than or equal to another
string, where the two strings are interpreted as containing ASCII
character codes. If the String A and String B are not the same length,
the shorter is compared as if spaces were appended to it to form a value
that has the same length as the longer. Leading spaces are significant.

In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT
differ from the corresponding intrinsic operators .ge., .gt., .le., and
.lt., in that the latter use the processor's character ordering (which
is not ASCII on some targets), whereas the former always use the ASCII
ordering.

### ARGUMENTS

  - __STR\_A__
    variable or array of default CHARACTER type.

  - __STR\_B__
    variable or array of default CHARACTER type.

<!-- end list -->

```
            if STR_A and STR_B are both arrays they must be of the
            same shape.
```

### RETURN VALUE

  - __RESULT__
    Returns .TRUE. if STR\_A \<= STR\_B, and .FALSE. otherwise, based on
    the ASCII ordering.

### EXAMPLE

Sample program:

```
   program demo_lle
   implicit none
   integer             :: i
      write(*,'(*(a))')(char(i),i=32,126)
        write(*,*) lle('abc','ABC')              ! F lowercase is > uppercase
        write(*,*) lle('abc','abc  ')            ! T trailing spaces
        ! If both strings are of zero length the result is true.
        write(*,*) lle('','')                    ! T
        write(*,*) lle('','a')                   ! T the null string is padded
        write(*,*) lle('a','')                   ! F
        write(*,*) lle('abc',['abc','123'])      ! [T,F] scalar and array
        write(*,*) lle(['cba', '123'],'abc')     ! [F,T]
        write(*,*) lle(['abc','123'],['cba','123']) ! [T,T] both arrays
   end program demo_lle
```

> Results:
>
> ```
>    > !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ
>    > [\]^_`abcdefghijklmnopqrstuvwxyz{|}~
>    > F
>    > T
>    > T
>    > T
>    > F
>    > T F
>    > F T
>    > T T
> ```

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

__lge__(3), __lgt__(3), __llt__(3)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    __adjustl__(3), __adjustr__(3), __index__(3), __len\_trim__(3),
    __scan__(3), __verify__(3)

  - __Nonelemental:__
    __repeat__(3), __trim__(3)
