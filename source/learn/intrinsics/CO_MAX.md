---
layout: book
title: co_max
permalink: /learn/intrinsics/CO_MAX
---
# CO_MAX
## __Name__

__co\_max__(3) - \[COLLECTIVE\] Maximal value on the current set of images


## __Syntax__
```fortran
call co_max(a, result_image, stat, errmsg)
```
## __Description__

co\_max determines element-wise the maximal value of __a__ on all images of
the current team. If result\_image is present, the maximum values are
returned in __a__ on the specified image only and the value of __a__ on the
other images become undefined. If result\_image is not present, the
value is returned on all images. If the execution was successful and
__stat__ is present, it is assigned the value zero. If the execution failed,
__stat__ gets assigned a nonzero value and, if present, __errmsg__ gets assigned
a value describing the occurred error.

## __Arguments__

  - __a__
    : shall be an integer, real or character variable, which has the same
    type and type parameters on all images of the team.

  - __result\_image__
    : (optional) a scalar integer expression; if present, it shall have
    the same the same value on all images and refer to an image of the
    current team.

  - __stat__
    : (optional) a scalar integer variable

  - __errmsg__
    : (optional) a scalar character variable

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
  Results:
```text
    Maximal value           2
```

## __Standard__

TS 18508 or later

## __See Also__

[__co\_min__(3)](CO_MIN),
[__co\_sum__(3)](CO_SUM),
[__co\_reduce__(3)](CO_REDUCE),
[__co\_broadcast__(3)](CO_BROADCAST)

###### fortran-lang intrinsic descriptions
