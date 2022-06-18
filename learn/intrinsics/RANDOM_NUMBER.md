---
layout: book
title: random_number
permalink: /learn/intrinsics/RANDOM_NUMBER
---
## __Name__

__random\_number__(3) - \[MATHEMATICS:RANDOM\] Pseudo-random number

## __Syntax__
```fortran
   call random_number(harvest)

     subroutine random_number(harvest)
     real(kind=KIND),intent(out) :: harvest(..)
```
## __Description__

Returns a single pseudorandom number or an array of pseudorandom numbers
from the uniform distribution over the range 0 \<= x \< 1.

## __Arguments__

  - __harvest__
    : a scalar or an array of type _real_ that will be set with
    pseudorandom values from the uniform distribution in the interval
    0.0 <= x < 1.0 .

## __Examples__

Sample program:

```fortran
program demo_random_number
use, intrinsic :: iso_fortran_env, only : dp=>real64
implicit none
integer              :: i, n, first,last, rand_int
integer,allocatable  :: count(:)
real(kind=dp)        :: rand_val
   call random_seed() ! initialize random number sequence
   ! generate a lot of random integers from 1 to 10 and count them.
   first=1
   last=10
   allocate(count(last-first+1))
   ! To have a discrete uniform distribution on the integers 
   ! [first, first+1, ..., last-1, last] carve the continuous
   ! distribution up into last+1-first equal sized chunks, 
   ! mapping each chunk to an integer. One way is:
   !   call random_number(rand_val)
   ! choose one from last-first+1 integers
   !   rand_int = first + FLOOR((last+1-first)*rand_val)
   count=0
   ! generate a lot of random integers from 1 to 10 and count them.
   ! with a large number of values you should get about the same
   ! number of each value
   do i=1,100000000
      call random_number(rand_val)
      rand_int=first+floor((last+1-first)*rand_val)
      if(rand_int.ge.first.and.rand_int.le.last)then
         count(rand_int)=count(rand_int)+1
      else
         write(*,*)rand_int,' is out of range'
      endif
   enddo
   write(*,'(i0.3,1x,i12)')(i,count(i),i=1,size(count))
end program demo_random_number
```
  Results:
```text
   001      9992201
   002      9998539
   003     10000985
   004     10001765
   005     10001004
   006     10003617
   007     10000626
   008      9997512
   009     10007993
   010      9995758
```
## __Standard__

Fortran 95 and later

## __See Also__

[__random\_seed__(3)](RANDOM_SEED)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
