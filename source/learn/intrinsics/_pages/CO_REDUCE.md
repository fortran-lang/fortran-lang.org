## co\_reduce
### __Name__

__co\_reduce__(3) - \[COLLECTIVE\] Reduction of values on the current set of images


### __Syntax__
```fortran
call co_reduce(a, operation, result_image, stat, errmsg)
```
### __Description__

co\_reduce determines element-wise the reduction of the value of __a__ on
all images of the current team. The pure function passed as __operation__ is
used to pairwise reduce the values of __a__ by passing either the value of __a__
of different images or the result values of such a reduction as
argument. If __a__ is an array, the reduction is done element wise. If
result\_image is present, the result values are returned in __a__ on the
specified image only and the value of __a__ on the other images become
undefined. If result\_image is not present, the value is returned on all
images. If the execution was successful and __stat__ is present, it is
assigned the value zero. If the execution failed, __stat__ gets assigned a
nonzero value and, if present, __errmsg__ gets assigned a value describing
the occurred error.

### __Arguments__

  - __a__
    : is an __intent(inout)__ argument and shall be nonpolymorphic. If it
    is allocatable, it shall be allocated; if it is a pointer, it shall
    be associated. __a__ shall have the same type and type parameters on all
    images of the team; if it is an array, it shall have the same shape
    on all images.

  - __operation__
    : pure function with two scalar nonallocatable arguments, which shall
    be nonpolymorphic and have the same type and type parameters as __a__.
    The function shall return a nonallocatable scalar of the same type
    and type parameters as __a__. The function shall be the same on all
    images and with regards to the arguments mathematically commutative
    and associative. Note that OPERATION may not be an elemental

      - __function, unless it is an intrinsic function.__
        result\_image

      - (optional) a scalar integer expression; if present, it shall
        have the same the same value on all images and refer to an image
        of the current team.

  - __stat__
    : (optional) a scalar integer variable

  - __errmsg__
    : (optional) a scalar character variable

### __Examples__

Sample program:

```fortran
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

### __Note__

While the rules permit in principle an intrinsic function, none of the
intrinsics in the standard fulfill the criteria of having a specific
function, which takes two arguments of the same type and returning that
type as a result.

### __Standard__

TS 18508 or later

### __See Also__

[__co\_min__(3)](CO_MIN),
[__co\_max__(3)](CO_MAX),
[__co\_sum__(3)](CO_SUM),
[__co\_broadcast__(3)](CO_BROADCAST)

####### fortran-lang intrinsic descriptions
