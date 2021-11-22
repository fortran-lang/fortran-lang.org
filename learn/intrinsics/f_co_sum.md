---
layout: book
title: co_sum
permalink: /learn/intrinsics/f_co_sum
---
### NAME

**co\_sum**(3f) - \[COLLECTIVE\] Sum of values on the
current set of images

### SYNTAX

call **co\_sum**(a \[, result\_image, stat, errmsg\])

### DESCRIPTION

co\_sum sums up the values of each element of A on all images of the
current team. If result\_image is present, the summed-up values are
returned in A on the specified image only and the value of A on the
other images become undefined. If result\_image is not present, the
value is returned on all images. If the execution was successful and
STAT is present, it is assigned the value zero. If the execution failed,
STAT gets assigned a nonzero value and, if present, ERRMSG gets assigned
a value describing the occurred error.

### ARGUMENTS

  - **A**
    shall be an integer, real or complex variable, which has the same
    type and type parameters on all images of the team.

  - **result\_image**
    (optional) a scalar integer expression; if present, it shall have
    the same the same value on all images and refer to an image of the
    current team.

  - **STAT**
    (optional) a scalar integer variable

  - **ERRMSG**
    (optional) a scalar character variable

### EXAMPLE

Sample program:

```
   program demo_co_sum
   implicit none
   integer :: val
      val = this_image()
      call co_sum(val, result_image=1)
      if (this_image() == 1) then
         ! prints (n**2 + n)/2, with n = num_images()
         write(*,*) "The sum is ", val
      endif
   end program demo_co_sum
```

### STANDARD

TS 18508 or later

### CLASS

Collective subroutine

### SEE ALSO

**co\_max**(3), **co\_min**(3), **co\_reduce**(3), **co\_broadcast**(3)
