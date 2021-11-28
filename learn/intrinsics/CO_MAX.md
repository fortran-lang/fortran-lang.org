---
layout: book
title: co_max
permalink: /learn/intrinsics/CO_MAX
---
## __Name__

__co\_max__(3) - \[COLLECTIVE\] Maximal value on the current set of images
(GFDL)

## __Syntax__

call __co\_max__(a \[, result\_image, stat, errmsg\])

## __Description__

co\_max determines element-wise the maximal value of A on all images of
the current team. If result\_image is present, the maximum values are
returned in A on the specified image only and the value of A on the
other images become undefined. If result\_image is not present, the
value is returned on all images. If the execution was successful and
STAT is present, it is assigned the value zero. If the execution failed,
STAT gets assigned a nonzero value and, if present, ERRMSG gets assigned
a value describing the occurred error.

## __Arguments__

  - __A__
    shall be an integer, real or character variable, which has the same
    type and type parameters on all images of the team.

  - __result\_image__
    (optional) a scalar integer expression; if present, it shall have
    the same the same value on all images and refer to an image of the
    current team.

  - __STAT__
    (optional) a scalar integer variable

  - __ERRMSG__
    (optional) a scalar character variable

## __Examples__

Sample program:

```fortran
   program demo_co_max
   implicit none
   integer :: val
      val = this_image()
      call co_max(val, result_image=1)
      if (this_image() == 1) then
        write(*,*) "Maximal value", val  ! prints num_images()
      endif
   end program demo_co_max
```

## __Standard__

TS 18508 or later

## __See Also__

__co\_min__(3), __co\_sum__(3), __co\_reduce__(3), __co\_broadcast__(3)
