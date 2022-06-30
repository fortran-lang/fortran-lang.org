---
layout: book
title: lle
permalink: /learn/intrinsics/LLE
---
## __Name__

__lle__(3) - \[CHARACTER:COMPARE\] Lexical less than or equal


## __Syntax__
```fortran
result = lle(str_a, str_b)

   character(len=*),intent(in) :: str_a, str_b

      or

   character(len=*),intent(in) :: str_a, str_b(*) logical :: result
```
## __Description__

Determines whether one string is lexically less than or equal to another
string, where the two strings are interpreted as containing ASCII
character codes. if the __string\_a__ and __string\_b__ are not the same length,
the shorter is compared as if spaces were appended to it to form a value
that has the same length as the longer. Leading spaces are significant.

In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT
differ from the corresponding intrinsic operators .ge., .gt., .le., and
.lt., in that the latter use the processor's character ordering (which
is not ASCII on some targets), whereas the former always use the ASCII
ordering.

## __Arguments__

  - __str\_a__
    : variable or array of default _character_ type.

  - __str\_b__
    : variable or array of default _character_ type.

    if __str_a__ and __str_b__ are both arrays they must be of the
    same shape.

## __Returns__

  - __result__
    Returns __.true.__ if __STR\_A \<= STR\_B__, and __.false.__ otherwise, based on
    the ASCII ordering.

## __Examples__

Sample program:

```fortran
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

Results:

```text
  !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ
  [\]^_`abcdefghijklmnopqrstuvwxyz{|}~
  F
  T
  T
  T
  F
  T F
  F T
  T T
```

## __Standard__

FORTRAN 77 and later

## __See Also__

[__lge__(3)](LGE),
[__lgt__(3),](LGT),
[__llt__(3)](LLT)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

 - __Elemental:__
 [__adjustl__(3)](ADJUSTL),
 [__adjustr__(3)](ADJUSTR),
 [__index__(3)](INDEX),

 [__scan__(3)](SCAN),
 [__verify__(3)](VERIFY)

 - __Nonelemental:__
 [__len\_trim__(3)](LEN_TRIM),
 [__len__(3)](LEN),
 [__repeat__(3)](REPEAT),
 [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
