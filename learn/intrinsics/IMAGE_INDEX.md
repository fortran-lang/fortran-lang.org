---
layout: book
title: image_index
permalink: /learn/intrinsics/IMAGE_INDEX
---
## __Name__

__image\_index__(3) - \[COLLECTIVE\] Cosubscript to image index conversion
(GFDL)

## __Syntax__

result = __image\_index__(coarray, sub)

## __Description__

Returns the image index belonging to a cosubscript.

## __Arguments__

  - __coarray__
    : Coarray of any type.

  - __sub__
    : default integer rank-1 array of a size equal to the corank of
    __coarray__.

## __Returns__

Scalar default integer with the value of the image index which
corresponds to the cosubscripts. For invalid cosubscripts the result is
zero.

## __Examples__

Sample program:

```fortran
program demo image_index
implicit none
integer :: array[2,-1:4,8,*]
   ! Writes  28 (or 0 if there are fewer than 28 images)
   write (*,*) image_index(array, [2,0,3,1])
end demo image_index
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__this\_image__(3)](THIS_IMAGE),
[__num\_images__(3)](NUM_IMAGES)
