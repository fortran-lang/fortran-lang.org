---
layout: book
title: this_image
permalink: /learn/intrinsics/THIS_IMAGE
---
### NAME

**this\_image**(3f) - \[COLLECTIVE\] Cosubscript index of this image
(GFDL)

### SYNTAX

result = **this\_image**() result = **this\_image**(distance) result =
**this\_image**(coarray \[, dim\])

### DESCRIPTION

Returns the cosubscript for this image.

### ARGUMENTS

  - **DISTANCE**
    (optional, **intent**(in)) Nonnegative scalar integer (not permitted
    together with COARRAY).

  - **COARRAY**
    Coarray of any type (optional; if DIM present, required).

  - **DIM**
    default integer scalar (optional). If present, DIM shall be between
    one and the corank of COARRAY.

### RETURN VALUE

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
**this\_image**(coarray).

### EXAMPLE

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

### STANDARD

Fortran 2008 and later. With DISTANCE argument, TS 18508
or later

### CLASS

Transformational function

### SEE ALSO

**num\_images**(3), **image\_index**(3)
