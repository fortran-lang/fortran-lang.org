---
layout: book
title: random_seed
permalink: /learn/intrinsics/RANDOM_SEED
---
## __Name__

__random\_seed__(3) - \[MATHEMATICS:RANDOM\] Initialize a pseudo-random number sequence


## __Syntax__
```fortran
call random_seed(size, put, get)
```
## __Description__

Restarts or queries the state of the pseudorandom number generator used
by random\_number.

If random\_seed is called without arguments, it is seeded with random
data retrieved from the operating system.

## __Arguments__

  - __size__
    : (Optional) Shall be a scalar and of type default _integer_, with
    __intent(out)__. It specifies the minimum size of the arrays used
    with the __put__ and __get__ arguments.

  - __put__
    : (Optional) Shall be an array of type default _integer_ and rank one.
    It is __intent(in)__ and the size of the array must be larger than
    or equal to the number returned by the __size__ argument.

  - __get__
    : (Optional) Shall be an array of type default _integer_ and rank one.
    It is __intent(out)__ and the size of the array must be larger than
    or equal to the number returned by the __size__ argument.

## __Examples__

Sample program:

```fortran
program demo_random_seed
implicit none
integer, allocatable :: seed(:)
integer :: n

   call random_seed(size = n)
   allocate(seed(n))
   call random_seed(get=seed)
   write (*, *) seed

end program demo_random_seed
```
  Results:
```text
     -674862499 -1750483360  -183136071  -317862567   682500039
     349459   344020729 -1725483289
```

## __Standard__

Fortran 95 and later

## __See Also__

[__random\_number__(3)](RANDOM_NUMBER)

###### fortran-lang intrinsic descriptions
