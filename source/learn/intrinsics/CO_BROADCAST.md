---
layout: book
title: co_broadcast
permalink: /learn/intrinsics/CO_BROADCAST
---
## __Name__

__co\_broadcast__(3) - \[COLLECTIVE\] Copy a value to all images the current set of images


## __Syntax__
```fortran
call co_broadcast(a, source_image, stat, errmsg)
```
## __Description__

__co\_broadcast(3)__ copies the value of argument __a__ on the image with image
index source\_image to all images in the current team. __a__ becomes defined
as if by intrinsic assignment. If the execution was successful and __stat__
is present, it is assigned the value zero. If the execution failed, __stat__
gets assigned a nonzero value and, if present, __errmsg__ gets assigned a
value describing the occurred error.

## __Arguments__

  - __a__
    : __intent(inout)__ argument; shall have the same dynamic type and
    type parameters on all images of the current team. If it is an
    array, it shall have the same shape on all images.

  - __source\_image__
    : a scalar integer expression. It shall have the same the same value
    on all images and refer to an image of the current team.

  - __stat__
    : (optional) a scalar integer variable

  - __errmsg__
    : (optional) a scalar character variable

## __Examples__

Sample program:

```fortran
program demo_co_broadcast
implicit none
integer :: val(3)
   if (this_image() == 1) then
      val = [1, 5, 3]
   endif
   call co_broadcast (val, source_image=1)
   print *, this_image(), ":", val
end program demo_co_broadcast
```

## __See Also__

[__co\_max__(3)](CO_MAX),
[__co\_min__(3)](CO_MIN),
[__co\_sum__(3)](CO_SUM),
[__co\_reduce__(3)](CO_REDUCE)

###### fortran-lang intrinsic descriptions
