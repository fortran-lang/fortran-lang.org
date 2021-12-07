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

  - __distance__
    : (optional, __intent__(in)) Nonnegative scalar integer (not permitted
    together with __coarray__).

  - __coarray__
    : Coarray of any type (optional; if __dim__ present, required).

  - __dim__
    : default integer scalar (optional). If present, __dim__ shall be between
    one and the corank of __coarray__.

## __Returns__

Default integer. If __coarray__ is not present, it is scalar; if __distance__ is
not present or has value __0__, its value is the image index on the invoking
image for the current team, for values smaller or equal distance to the
initial team, it returns the image index on the ancestor team which has
a distance of __distance__ from the invoking team. If __distance__ is larger
than the distance to the initial team, the image index of the initial
team is returned. Otherwise when the __coarray__ is present, if __dim__ is not
present, a rank-1 array with corank elements is returned, containing the
cosubscripts for __coarray__ specifying the invoking image. If __dim__ is
present, a scalar is returned, with the value of the __dim__ element of
__this\_image(coarray)__.

## __Examples__

Sample program:

```fortran
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

[__num\_images__(3)](NUM_IMAGES),
[__image\_index__(3)](IMAGE_INDEX)
