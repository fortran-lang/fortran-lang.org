---
layout: book
title: conjg
permalink: /learn/intrinsics/CONJG
---
## __Name__

__conjg__(3) - \[NUMERIC\] Complex conjugate of a complex value

## __Syntax__
```fortran
z = conjg(z)

   complex(kind=K) elemental function conjg(z)
   complex(kind=K),intent(in) :: z 
```
where __K__ is the kind of the parameter __z__

## __Description__

__conjg(z)__ returns the complex conjugate of the _complex_ value __z__. 

In mathematics, the complex conjugate of a complex_ number is the number
with an equal real part and an imaginary part equal in magnitude but
opposite in sign.  

That is, If __z__ is __(x, y)__ then the result is __(x, -y)__.

For matrices of complex numbers, __conjg(array)__ represents the
element-by-element conjugation of __array__; not the conjugate transpose
of __array__ .

## __Arguments__

  - __z__
    : The type shall be _complex_.

## __Returns__

The return value is of type _complex_.

## __Examples__

Sample program:

```fortran
program demo_conjg
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
complex :: z = (2.0, 3.0)
complex(kind=real64) :: dz = (   &
   &  1.2345678901234567_real64, &
   & -1.2345678901234567_real64)
complex :: arr(3,3)
integer :: i

    print *, z
    z= conjg(z)
    print *, z
    print *

    print *, dz
    dz = conjg(dz)
    print *, dz
    print *

    ! the function is elemental so it can take arrays
    arr(1,:)=[(-1.0, 2.0),( 3.0, 4.0),( 5.0,-6.0)]
    arr(2,:)=[( 7.0,-8.0),( 8.0, 9.0),( 9.0, 9.0)]
    arr(3,:)=[( 1.0, 9.0),( 2.0, 0.0),(-3.0,-7.0)]

    write(*,*)'original'
    write(*,'(3("(",g8.2,",",g8.2,")",1x))')(arr(i,:),i=1,3)
    arr = conjg(arr)
    write(*,*)'conjugate'
    write(*,'(3("(",g8.2,",",g8.2,")",1x))')(arr(i,:),i=1,3)

end program demo_conjg
```
  Results:
```fortran
 (2.000000,3.000000)
 (2.000000,-3.000000)
 
 (1.23456789012346,-1.23456789012346)
 (1.23456789012346,1.23456789012346)
 
 original
(-1.0    , 2.0    ) ( 3.0    , 4.0    ) ( 5.0    ,-6.0    )
( 7.0    ,-8.0    ) ( 8.0    , 9.0    ) ( 9.0    , 9.0    )
( 1.0    , 9.0    ) ( 2.0    , 0.0    ) (-3.0    ,-7.0    )

 conjugate
(-1.0    ,-2.0    ) ( 3.0    ,-4.0    ) ( 5.0    , 6.0    )
( 7.0    , 8.0    ) ( 8.0    ,-9.0    ) ( 9.0    ,-9.0    )
( 1.0    ,-9.0    ) ( 2.0    , 0.0    ) (-3.0    , 7.0    )
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions (license: MIT))
