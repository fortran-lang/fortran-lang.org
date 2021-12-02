---
layout: book
title: btest
permalink: /learn/intrinsics/BTEST
---
## __Name__

__btest__(3) - \[BIT MANIPULATION\] Tests a bit of an _integer_ value.

## __Syntax__
```fortran
   result = btest(i, pos)

    integer(kind=KIND) elemental function btest(i,pos)
    integer,intent(in)  :: i
    logical,intent(out) :: pos
```
 where __KIND__ is any _integer_ kind supported by the programming environment.

## __Description__

__btest(i,pos)__ returns logical __.true.__ if the bit at __pos__ in __i__ is set.

## __Arguments__

  - __i__
    The type shall be _integer_.

  - __pos__
    The bit position to query. it must be a valid position for the
    value __i__; ie.  __0 <= pos <= bit_size(i)__ .

    A value of zero refers to the least significant bit.

## __Returns__
    The result is a _logical_ that has the value __.true.__ if bit
    position __pos__ of __i__ has the value __1__ and the value
    __.false.__ if bit __pos__ of __i__ has the value __0__.

## __Examples__

Sample program:

```fortran
program demo_btest
implicit none
integer :: i, j, pos, a(2,2)
logical :: bool
character(len=*),parameter :: g='(*(g0))'

     i = 32768 + 1024 + 64
    write(*,'(a,i0,"=>",b32.32,/)')'Looking at the integer: ',i

    ! looking one bit at a time from LOW BIT TO HIGH BIT
    write(*,g)'from bit 0 to bit ',bit_size(i),'==>'
    do pos=0,bit_size(i)-1
        bool = btest(i, pos)
        write(*,'(l1)',advance='no')bool
    enddo
    write(*,*)

    ! a binary format the hard way. 
    ! Note going from bit_size(i) to zero.
    write(*,*)
    write(*,g)'so for ',i,' with a bit size of ',bit_size(i)
    write(*,'(b32.32)')i
    write(*,g)merge('^','_',[(btest(i,j),j=bit_size(i)-1,0,-1)])
    write(*,*)
    write(*,g)'and for ',-i,' with a bit size of ',bit_size(i)
    write(*,'(b32.32)')-i
    write(*,g)merge('^','_',[(btest(-i,j),j=bit_size(i)-1,0,-1)])

    ! elemental:
    !
    a(1,:)=[ 1, 2 ]
    a(2,:)=[ 3, 4 ]
    write(*,*)
    write(*,'(a,/,*(i2,1x,i2,/))')'given the array a ...',a
    ! the second bit of all the values in a
    write(*,'(a,/,*(l2,1x,l2,/))')'the value of btest (a, 2)',btest(a,2)
    ! bits 1,2,3,4 of the value 2
    write(*,'(a,/,*(l2,1x,l2,/))')'the value of btest (2, a)',btest(2,a)
end program demo_btest
```
Results
```text
Looking at the integer: 33856=>11111111111111110111101111000000

00000000000000001000010001000000
11111111111111110111101111000000
1000010001000000
11111111111111110111101111000000
from bit 0 to bit 32==>
FFFFFFTFFFTFFFFTFFFFFFFFFFFFFFFF
 
so for 33856 with a bit size of 32
00000000000000001000010001000000
________________^____^___^______
 
and for -33856 with a bit size of 32
11111111111111110111101111000000
^^^^^^^^^^^^^^^^_^^^^_^^^^______
 
given the array a ...
 1  3
 2  4

the value of btest (a, 2)
 F  F
 F  T

the value of btest (2, a)
 T  F
 F  F
```
## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR),
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)

###### fortran-lang intrinsic descriptions (@urbanjost)
