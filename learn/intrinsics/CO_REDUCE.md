---
layout: book
title: co_reduce
permalink: /learn/intrinsics/CO_REDUCE
---
#### NAME

__co\_reduce__(3f) - \[COLLECTIVE\] Reduction of values on the current set of images
(GFDL)

#### SYNTAX

call __co\_reduce__(a, operation, \[, result\_image, stat, errmsg\])

#### DESCRIPTION

co\_reduce determines element-wise the reduction of the value of A on
all images of the current team. The pure function passed as OPERATION is
used to pairwise reduce the values of A by passing either the value of A
of different images or the result values of such a reduction as
argument. If A is an array, the reduction is done element wise. If
result\_image is present, the result values are returned in A on the
specified image only and the value of A on the other images become
undefined. If result\_image is not present, the value is returned on all
images. If the execution was successful and STAT is present, it is
assigned the value zero. If the execution failed, STAT gets assigned a
nonzero value and, if present, ERRMSG gets assigned a value describing
the occurred error.

#### ARGUMENTS

  - __A__
    is an __intent__(inout) argument and shall be nonpolymorphic. If it
    is allocatable, it shall be allocated; if it is a pointer, it shall
    be associated. A shall have the same type and type parameters on all
    images of the team; if it is an array, it shall have the same shape
    on all images.

  - __OPERATION__
    pure function with two scalar nonallocatable arguments, which shall
    be nonpolymorphic and have the same type and type parameters as A.
    The function shall return a nonallocatable scalar of the same type
    and type parameters as A. The function shall be the same on all
    images and with regards to the arguments mathematically commutative
    and associative. Note that OPERATION may not be an elemental

      - __function, unless it is an intrinsic function.__
        result\_image

    <!-- end list -->

      - (optional) a scalar integer expression; if present, it shall
        have the same the same value on all images and refer to an image
        of the current team.

  - __STAT__
    (optional) a scalar integer variable

  - __ERRMSG__
    (optional) a scalar character variable

#### EXAMPLE

Sample program:

```
   program demo_co_reduce
   implicit none
     integer :: val
     val = this_image()
     call co_reduce(val, myprod, 1)
        if (this_image() == 1) then
          write(*,*) "Product value", val  ! prints num_images() factorial
        endif
   contains
     pure function myprod(a, b)
       integer, value :: a, b
       integer :: myprod
       myprod = a * b
     end function myprod
   end program demo_co_reduce
```

#### NOTE

While the rules permit in principle an intrinsic function, none of the
intrinsics in the standard fulfill the criteria of having a specific
function, which takes two arguments of the same type and returning that
type as a result.

#### STANDARD

TS 18508 or later

#### CLASS

Collective subroutine

#### SEE ALSO

__co\_min__(3), __co\_max__(3), __co\_sum__(3), __co\_broadcast__(3)
