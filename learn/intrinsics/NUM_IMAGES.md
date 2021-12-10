---
layout: book
title: num_images
permalink: /learn/intrinsics/NUM_IMAGES
---
## __Name__

__num\_images__(3) - \[COLLECTIVE\] Number of images
(GFDL)

## __Syntax__
```fortran
result = num_images(distance, failed)
```
## __Description__

Returns the number of images.

## __Arguments__

  - __distance__
    : (optional, __intent(in)__) Nonnegative scalar integer

  - __failed__
    : (optional, __intent(in)__) Scalar logical expression

## __Returns__

Scalar default-kind _integer_. If __distance__ is not present or has value 0,
the number of images in the current team is returned. For values smaller
or equal distance to the initial team, it returns the number of images
index on the ancestor team which has a distance of __distance__ from the
invoking team. If __distance__ is larger than the distance to the initial
team, the number of images of the initial team is returned. If __failed__ is
not present the total number of images is returned; if it has the value
.true., the number of failed images is returned, otherwise, the number
of images which do have not the failed status.

## __Examples__

Sample program:

```fortran
program demo_num_images
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

end program demo_num_images
```

## __Standard__

Fortran 2008 and later. With DISTANCE or FAILED argument, TS 18508 or later

## __See Also__

[__this\_image__(3)](THIS_IMAGE),
[__image\_index__(3)](THIS_INDEX)
