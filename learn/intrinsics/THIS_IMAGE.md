---
layout: book
title: this_image
permalink: /learn/intrinsics/THIS_IMAGE
---
## __Name__

__this\_image__(3) - \[COLLECTIVE\] Cosubscript index of this image
(GFDL)

## __Syntax__

result = __this\_image__() result = __this\_image__(distance) result =
__this\_image__(coarray \[, dim\])

## __Description__

Returns the cosubscript for this image.

## __Arguments__

  - __DISTANCE__
    (optional, __intent__(in)) Nonnegative scalar integer (not permitted
    together with COARRAY).

  - __COARRAY__
    Coarray of any type (optional; if DIM present, required).

  - __DIM__
    default integer scalar (optional). If present, DIM shall be between
    one and the corank of COARRAY.

## __Returns__

Default integer. If COARRAY is not present, it is scalar; if DISTANCE is
not present or has value 0, its value is the image index on the invoking
image for the current team, for values smaller or equal distance to the
initial team, it returns the image index on the ancestor team which has
a distance of DISTANCE from the invoking team. If DISTANCE is larger
than the distance to the initial team, the image index of the initial
team is returned. Otherwise when the COARRAY is present, if DIM is not
present, a rank-1 array with corank elements is returned, containing the
cosubscripts for COARRAY specifying the invoking image. If DIM is
present, a scalar is returned, with the value of the DIM element of
__this\_image__(coarray).

## __Examples__

Sample program:

```
   program demo_this_image
   implicit none
   integer :: value[*]
   integer :: i
      value = this_image()
      sync all
      if (this_image() == 1) then
        do i = 1, num_images()
          write(*,'(2(a,i0))') 'value[', i, '] is ', value[i]
        end do
      endif
   end program demo_this_image
   !
   ! Check whether the current image is the initial image
   if (this_image(huge(1)) /= this_image())
   error stop "something is rotten here"
```

## __Standard__

Fortran 2008 and later. With DISTANCE argument, TS 18508
or later

## __See Also__

__num\_images__(3), __image\_index__(3)
