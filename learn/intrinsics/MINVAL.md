---
layout: book
title: minval
permalink: /learn/intrinsics/MINVAL
---
## __Name__

__minval__(3) - \[ARRAY REDUCTION\] Minimum value of an array

## __Syntax__

result = __minval__(array, dim \[, mask\]) result = __minval__(array \[,
mask\])

## __Description__

Determines the minimum value of the elements in an array value, or, if
the __dim__ argument is supplied, determines the minimum value along each
row of the array in the __dim__ direction. 

If __mask__ is present, only the
elements for which __mask__ is __.true.__ are considered. 

If the array has zero size, or all of the elements of __mask__ are
.false., then the result is __huge(array)__ if __array__ is numeric, or a
string of __char(len=255)__ characters if __array__ is of character type.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_, _real_, or _character_.

  - __dim__
    : (Optional) Shall be a scalar of type _integer_, with a value between
    one and the rank of ARRAY, inclusive. It may not be an optional
    dummy argument.

  - __mask__
    : Shall be an array of type _logical_, and conformable with __array__.

## __Returns__

If __dim__ is absent, or if __array__ has a rank of one, the result is a scalar.

If __dim__ is present, the result is an array with a rank one less than the
rank of __array__, and a size corresponding to the size of __array__ with the
__dim__ dimension removed. In all cases, the result is of the same type and
kind as __array__.

## __Examples__

sample program:

```fortran
program demo_minval
implicit none
integer :: i
character(len=*),parameter :: g='(3x,*(g0,1x))'

integer,save :: ints(3,5)= reshape([&
       1,  -2,   3,   4,   5,  &
      10,  20, -30,  40,  50,  &
      11,  22,  33, -44,  55  &
],shape(ints),order=[2,1])

integer,save :: box(3,5,2)

   box(:,:,1)=ints
   box(:,:,2)=-ints

   write(*,*)'Given the array'
   write(*,'(1x,*(g4.4,1x))') &
   & (ints(i,:),new_line('a'),i=1,size(ints,dim=1))

   write(*,*)'What is the smallest element in the array?'
   write(*,g) minval(ints),'at <',minloc(ints),'>'

   write(*,*)'What is the smallest element in each column?'
   write(*,g) minval(ints,dim=1)

   write(*,*)'What is the smallest element in each row?'
   write(*,g) minval(ints,dim=2)

   ! notice the shape of the output has less columns 
   ! than the input in this case
   write(*,*)'What is the smallest element in each column,'
   write(*,*)'considering only those elements that are'
   write(*,*)'greater than zero?'
   write(*,g) minval(ints, dim=1, mask = ints > 0)

   write(*,*)&
   & 'if everything is false a zero-sized array is NOT returned'
   write(*,*) minval(ints, dim=1, mask = .false.)
   write(*,*)'even for a zero-sized input'
   write(*,g) minval([integer ::], dim=1, mask = .false.)

   write(*,*)'a scalar answer for everything false is huge()'
   write(*,g) minval(ints, mask = .false.)
   write(*,g) minval([integer ::], mask = .false.)

   write(*,*)'some calls with three dimensions'
   write(*,g) minval(box, mask = .true. )
   write(*,g) minval(box, dim=1, mask = .true. )

   write(*,g) minval(box, dim=2, mask = .true. )
   write(*,g) 'shape of answer is ', &
   & shape(minval(box, dim=2, mask = .true. ))

end program demo_minval
```

results:

```text
 Given the array
    1   -2    3    4    5    
   10   20  -30   40   50    
   11   22   33  -44   55    

 What is the smallest element in the array?
   -44 at < 3 4 >
 What is the smallest element in each column?
   1 -2 -30 -44 5
 What is the smallest element in each row?
   -2 -30 -44
 What is the smallest element in each column,
 considering only those elements that are
 greater than zero?
   1 20 3 4 5
 if everything is false a zero-sized array is NOT returned
  2147483647  2147483647  2147483647  2147483647  2147483647
 even for a zero-sized input
   2147483647
 a scalar answer for everything false is huge()
   2147483647
   2147483647
 some calls with three dimensions
   -55
   1 -2 -30 -44 5 -11 -22 -33 -40 -55
   -2 -30 -44 -5 -50 -55
   shape of answer is  3 2
```

## __Standard__

Fortran 95 and later

## __See Also__

[__min__(3)](MIN),
[__minloc__(3)](MINLOC)

###### fortran-lang intrinsic descriptions (@urbanjost)
