---
layout: book
title: image_index
permalink: /learn/intrinsics/IMAGE_INDEX
---
#### NAME

__image\_index__(3f) - \[COLLECTIVE\] Cosubscript to image index conversion
(GFDL)

#### SYNTAX

result = __image\_index__(coarray, sub)

#### DESCRIPTION

Returns the image index belonging to a cosubscript.

#### ARGUMENTS

  - __COARRAY__
    Coarray of any type.

  - __SUB__
    default integer rank-1 array of a size equal to the corank of
    COARRAY.

#### RETURN VALUE

Scalar default integer with the value of the image index which
corresponds to the cosubscripts. For invalid cosubscripts the result is
zero.

#### EXAMPLE

Sample program:

```
   program demo image_index
   implicit none
   integer :: array[2,-1:4,8,*]
      ! Writes  28 (or 0 if there are fewer than 28 images)
      write (*,*) image_index(array, [2,0,3,1])
   end demo image_index
```

#### STANDARD

Fortran 2008 and later

#### CLASS

Inquiry function.

#### SEE ALSO

__this\_image__(3), __num\_images__(3)
