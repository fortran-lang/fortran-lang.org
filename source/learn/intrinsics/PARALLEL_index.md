# PARALLEL_index
### These routines support parallel programming using co_arrays and co_indexed arrays.

# CO_BROADCAST
## __Name__

__co\_broadcast__(3) - \[COLLECTIVE\] Copy a value to all images the current set of images


## __Syntax__
```fortran
call co_broadcast(a, source_image, stat, errmsg)
```
## __Description__

__co\_broadcast(3)__ copies the value of argument __a__ on the image with image
index source\_image to all images in the current team. __a__ becomes defined
as if by intrinsic assignment. If the execution was successful and __stat__
is present, it is assigned the value zero. If the execution failed, __stat__
gets assigned a nonzero value and, if present, __errmsg__ gets assigned a
value describing the occurred error.

## __Arguments__

  - __a__
    : __intent(inout)__ argument; shall have the same dynamic type and
    type parameters on all images of the current team. If it is an
    array, it shall have the same shape on all images.

  - __source\_image__
    : a scalar integer expression. It shall have the same the same value
    on all images and refer to an image of the current team.

  - __stat__
    : (optional) a scalar integer variable

  - __errmsg__
    : (optional) a scalar character variable

## __Examples__

Sample program:

```fortran
program demo_co_broadcast
implicit none
integer :: val(3)
   if (this_image() == 1) then
      val = [1, 5, 3]
   endif
   call co_broadcast (val, source_image=1)
   print *, this_image(), ":", val
end program demo_co_broadcast
```

## __See Also__

[__co\_max__(3)](CO_MAX),
[__co\_min__(3)](CO_MIN),
[__co\_sum__(3)](CO_SUM),
[__co\_reduce__(3)](CO_REDUCE)

###### fortran-lang intrinsic descriptions
# CO_LBOUND
## __Name__

__co\_lbound__(3) - \[COLLECTIVE\] Lower codimension bounds of an array


## __Syntax__
```fortran
result = co_lbound(coarray, dim, kind)
```
## __Description__

Returns the lower bounds of a coarray, or a single lower cobound along
the __dim__ codimension.

## __Arguments__

  - __array__
    : Shall be an coarray, of any type.

  - __dim__
    : (Optional) Shall be a scalar _integer_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind. If __dim__ is absent, the
result is an array of the lower cobounds of __coarray__. If __dim__ is present,
the result is a scalar corresponding to the lower cobound of the array
along that codimension.

## __Standard__

Fortran 2008 and later

## __See Also__

[__co\_ubound__(3)](CO_UBOUND),
[__lbound__(3)](LBOUND)

###### fortran-lang intrinsic descriptions
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
# CO_MIN
## __Name__

__co\_min__(3) - \[COLLECTIVE\] Minimal value on the current set of images


## __Syntax__
```fortran
call co_min(a, result_image, stat, errmsg)
```
## __Description__

co\_min determines element-wise the minimal value of __a__ on all images of
the current team. If result\_image is present, the minimal values are
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
program demo_co_min
implicit none
integer :: val
   val = this_image()
   call co_min(val, result_image=1)
   if (this_image() == 1) then
     write(*,*) "Minimal value", val  ! prints 1
   endif
end program demo_co_min
```

## __Standard__

TS 18508 or later

## __See Also__

[__co\_max__(3)](CO_MAX),
[__co\_sum__(3)](CO_SUM),
[__co\_reduce__(3)](CO_REDUCE),
[__co\_broadcast__(3)](CO_BROADCAST)

###### fortran-lang intrinsic descriptions
# CO_REDUCE
## __Name__

__co\_reduce__(3) - \[COLLECTIVE\] Reduction of values on the current set of images


## __Syntax__
```fortran
call co_reduce(a, operation, result_image, stat, errmsg)
```
## __Description__

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

## __Arguments__

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

## __Examples__

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

## __Note__

While the rules permit in principle an intrinsic function, none of the
intrinsics in the standard fulfill the criteria of having a specific
function, which takes two arguments of the same type and returning that
type as a result.

## __Standard__

TS 18508 or later

## __See Also__

[__co\_min__(3)](CO_MIN),
[__co\_max__(3)](CO_MAX),
[__co\_sum__(3)](CO_SUM),
[__co\_broadcast__(3)](CO_BROADCAST)

###### fortran-lang intrinsic descriptions
# CO_SUM
## __Name__

__co\_sum__(3) - \[COLLECTIVE\] Sum of values on the current set of images


## __Syntax__
```fortran
call co_sum(a, result_image, stat, errmsg)
```
## __Description__

co\_sum sums up the values of each element of __a__ on all images of the
current team. If result\_image is present, the summed-up values are
returned in __a__ on the specified image only and the value of __a__ on the
other images become undefined. If result\_image is not present, the
value is returned on all images. If the execution was successful and
__stat__ is present, it is assigned the value zero. If the execution failed,
__stat__ gets assigned a nonzero value and, if present, __errmsg__ gets assigned
a value describing the occurred error.

## __Arguments__

  - __a__
    : shall be an integer, real or complex variable, which has the same
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
  Results:
```text
    The sum is            1
```

## __Standard__

TS 18508 or later

## __See Also__

[__co\_max__(3)](CO_MAX),
[__co\_min__(3)](CO_MIN),
[__co\_reduce__(3)](CO_REDUCE),
[__co\_broadcast__(3)](CO_BROADCAST)

###### fortran-lang intrinsic descriptions
# CO_UBOUND
## __Name__

__co\_ubound__(3) - \[COLLECTIVE\] Upper codimension bounds of an array


## __Syntax__
```fortran
result = co_ubound(coarray, dim, kind)
```
## __Description__

Returns the upper cobounds of a coarray, or a single upper cobound along
the __dim__ codimension.

## __Arguments__

  - __array__
    : Shall be an coarray, of any type.

  - __dim__
    : (Optional) Shall be a scalar _integer_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind. If __dim__ is absent, the
result is an array of the lower cobounds of __coarray__. If __dim__ is present,
the result is a scalar corresponding to the lower cobound of the array
along that codimension.

## __Standard__

Fortran 2008 and later

## __See Also__

[__co\_lbound__(3)](CO_LBOUND),
[__lbound__(3)](LBOUND),
[__ubound__(3)](UBOUND)

###### fortran-lang intrinsic descriptions
# EVENT_QUERY
## __Name__

__event\_query__(3) - \[COLLECTIVE\] Query whether a coarray event has occurred


## __Syntax__
```fortran
call event_query(event, count, stat)
```
## __Description__

__event\_query__ assigns the number of events to __count__ which have been
posted to the __event__ variable and not yet been removed by calling
__event\_wait__. When __stat__ is present and the invocation was successful, it
is assigned the value __0__. If it is present and the invocation has failed,
it is assigned a positive value and __count__ is assigned the value __-1__.

## __Arguments__

  - __event__
    : (intent(in)) Scalar of type event\_type, defined in
    iso\_fortran\_env; shall not be coindexed.

  - __count__
    : (intent(out))Scalar integer with at least the precision of default
    _integer_.

  - __stat__
    : (OPTIONAL) Scalar default-kind _integer_ variable.

## __Examples__

Sample program:

```fortran
program demo_event_query
use iso_fortran_env
implicit none
type(event_type) :: event_value_has_been_set[*]
integer :: cnt
   if (this_image() == 1) then
      call event_query(event_value_has_been_set, cnt)
      if (cnt > 0) write(*,*) "Value has been set"
   elseif (this_image() == 2) then
      event post(event_value_has_been_set[1])
   endif
end program demo_event_query
```

## __Standard__

TS 18508 or later

###### fortran-lang intrinsic descriptions
# IMAGE_INDEX
## __Name__

__image\_index__(3) - \[COLLECTIVE\] Cosubscript to image index conversion


## __Syntax__
```fortran
result = image_index(coarray, sub)
```
## __Description__

Returns the image index belonging to a cosubscript.

## __Arguments__

  - __coarray__
    : Coarray of any type.

  - __sub__
    : default integer rank-1 array of a size equal to the corank of
    __coarray__.

## __Returns__

Scalar default integer with the value of the image index which
corresponds to the cosubscripts. For invalid cosubscripts the result is
zero.

## __Examples__

Sample program:

```fortran
program demo image_index
implicit none
integer :: array[2,-1:4,8,*]
   ! Writes  28 (or 0 if there are fewer than 28 images)
   write (*,*) image_index(array, [2,0,3,1])
end demo image_index
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__this\_image__(3)](THIS_IMAGE),
[__num\_images__(3)](NUM_IMAGES)

###### fortran-lang intrinsic descriptions
# NUM_IMAGES
## __Name__

__num\_images__(3) - \[COLLECTIVE\] Number of images


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

###### fortran-lang intrinsic descriptions
# THIS_IMAGE
## __Name__

__this\_image__(3) - \[COLLECTIVE\] Cosubscript index of this image


## __Syntax__
```fortran
result = this_image() result = this_image(distance) &
         & result = this_image(coarray, dim)
```
## __Description__

Returns the cosubscript for this image.

## __Arguments__

  - __distance__
    : (optional, __intent(in)__) Nonnegative scalar integer (not permitted
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
```
  Results:
```text
   value[1] is 1
```
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

###### fortran-lang intrinsic descriptions
# ATOMIC_AND
## __Name__

__atomic\_and__(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise AND operation


## __Syntax__
```fortran
call atomic_and(atom, value, stat)
```
## __Description__

__atomic\_and(atom, value)__ atomically defines __atom__ with the bitwise
__and__ between the values of __atom__ and __value__. When __stat__ is present and the
invocation was successful, it is assigned the value 0. If it is present
and the invocation has failed, it is assigned a positive value; in
particular, for a coindexed __atom__, if the remote image has stopped, it is
assigned the value of iso\_fortran\_env's stat\_stopped\_image and if
the remote image has failed, the value stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_and
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*]
   call atomic_and(atom[1], int(b'10100011101'))
end program demo_atomic_and
```
## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_fetch\_and__(3)](ATOMIC_FETCH_AND),
[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_ref__(3)](ATOMIC_REF),
[__atomic\_cas__(3)](ATOMIC_CAS),
__iso\_fortran\_env__(3),
[__atomic\_add__(3)](ATOMIC_ADD),
[__atomic\_or__(3)](ATOMIC_OR),
[__atomic\_xor__(3)](ATOMIC_XOR)

###### fortran-lang intrinsic descriptions
# ATOMIC_FETCH_AND
## __Name__

__atomic\_fetch\_and__(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise AND operation with prior fetch


## __Syntax__
```fortran
call atomic_fetch_and(atom, value, old, stat)
```
## __Description__

__atomic\_fetch\_and(atom, value, old)__ atomically stores the value of
__atom__ in __old__ and defines __atom__ with the bitwise AND between the values of
__atom__ and __value__. When __stat__ is present and the invocation was successful,
it is assigned the value __0__. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
__atom__, if the remote image has stopped, it is assigned the value of
iso\_fortran\_env's stat\_stopped\_image and if the remote image has
failed, the value stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __old__
    : Scalar of the same type and kind as __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_fetch_and
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*], old
   call atomic_fetch_and (atom[1], int(b'10100011101'), old)
end program demo_atomic_fetch_and
```

## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_and__(3)](ATOMIC_AND),
[__iso\_fortran\_env__(3)](),

[__atomic\_fetch\_add__(3)](ATOMIC_FETCH_ADD),
[__atomic\_fetch\_or__(3)](ATOMIC_FETCH_OR),

[__atomic\_fetch\_xor__(3)](ATOMIC_FETCH_XOR)

###### fortran-lang intrinsic descriptions
# ATOMIC_FETCH_OR
## __Name__

__atomic\_fetch\_or__(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise OR operation with prior fetch


## __Syntax__
```fortran
call atomic_fetch_or(atom, value, old, stat)
```
## __Description__

__atomic\_fetch\_or(atom, value, old)__ atomically stores the value of
__atom__ in __old__ and defines __atom__ with the bitwise OR between the values of
__atom__ and __value__. When __stat__ is present and the invocation was successful,
it is assigned the value __0__. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
__atom__, if the remote image has stopped, it is assigned the value of
iso\_fortran\_env's stat\_stopped\_image and if the remote image has
failed, the value stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __old__
    : Scalar of the same type and kind as __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_fetch_or
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*], old
   call atomic_fetch_or(atom[1], int(b'10100011101'), old)
end program demo_atomic_fetch_or
```

## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_or__(3)](ATOMIC_OR),
[__iso\_fortran\_env__(3)](),

[__atomic\_fetch\_add__(3)](ATOMIC_FETCH_ADD),
[__atomic\_fetch\_and__(3)](ATOMIC_FETCH_AND),

[__atomic\_fetch\_xor__(3)](ATOMIC_FETCH_XOR)

###### fortran-lang intrinsic descriptions
# ATOMIC_FETCH_XOR
## __Name__

__atomic\_fetch\_xor__(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise XOR operation with prior fetch


## __Syntax__
```fortran
call atomic_fetch_xor (atom, value, old, stat)
```
## __Description__

__atomic\_fetch\_xor(atom, value, old)__ atomically stores the value of
__atom__ in __old__ and defines __atom__ with the bitwise __xor__ between the values of
__atom__ and __value__. When __stat__ is present and the invocation was successful,
it is assigned the value __0__. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
__atom__, if the remote image has stopped, it is assigned the value of
iso\_fortran\_env's stat\_stopped\_image and if the remote image has
failed, the value stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __old__
    : Scalar of the same type and kind as __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_fetch_xor
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*], old
   call atomic_fetch_xor (atom[1], int(b'10100011101'), old)
end program demo_atomic_fetch_xor
```

## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_xor__(3)](ATOMIC_XOR),
[__iso\_fortran\_env__(3)](),

[__atomic\_fetch\_add__(3)](ATOMIC_FETCH_ADD),
[__atomic\_fetch\_and__(3)](ATOMIC_FETCH_AND),

[__atomic\_fetch\_or__(3)](ATOMIC_FETCH_OR)

###### fortran-lang intrinsic descriptions
# ATOMIC_OR
## __Name__

__atomic\_or__(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise OR operation


## __Syntax__
```fortran
call atomic_or__(atom, value, stat)
```
## __Description__

__atomic\_or(atom, value)__ atomically defines __atom__ with the bitwise __or__
between the values of __atom__ and __value__. When __stat__ is present and the
invocation was successful, it is assigned the value __0__. If it is present
and the invocation has failed, it is assigned a positive value; in
particular, for a coindexed __atom__, if the remote image has stopped, it is
assigned the value of iso\_fortran\_env's stat\_stopped\_image and if
the remote image has failed, the value stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_or
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*]
   call atomic_or(atom[1], int(b'10100011101'))
end program demo_atomic_or
```

## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_fetch\_or__(3)](ATOMIC_FETCH),

[__iso\_fortran\_env__(3)](),
[__atomic\_add__(3)](ATOMIC_ADD),
[__atomic\_or__(3)](ATOMIC_OR),

[__atomic\_xor__(3)](ATOMIC_XOR)

###### fortran-lang intrinsic descriptions
# ATOMIC_XOR
## __Name__

__atomic\_xor__(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise OR operation


## __Syntax__
```fortran
call atomic_xor(atom, value, stat)
```
## __Description__

__atomic\_xor(atom, value)__ atomically defines __atom__ with the bitwise
__xor__ between the values of __atom__ and __value__. When __stat__ is present and the
invocation was successful, it is assigned the value __0__. If it is present
and the invocation has failed, it is assigned a positive value; in
particular, for a coindexed __atom__, if the remote image has stopped, it is
assigned the value of iso\_fortran\_env's stat\_stopped\_image and if
the remote image has failed, the value stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_xor
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*]
   call atomic_xor(atom[1], int(b'10100011101'))
end program demo_atomic_xor
```

## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_fetch\_xor__(3)](ATOMIC_FETCH),
[__iso\_fortran\_env__(3)](),
[__atomic\_add__(3)](ATOMIC_ADD),
[__atomic\_or__(3)](ATOMIC_OR),
[__atomic\_xor__(3)](ATOMIC_XOR)

###### fortran-lang intrinsic descriptions
# ATOMIC_ADD
## __Name__

__atomic\_add__(3) - \[ATOMIC\] Atomic ADD operation 

## __Syntax__
```fortran
call atomic_add (atom, value, stat)
```
## __Description__

__atomic\_ad(atom, value)__ atomically adds the value of VAR to the
variable __atom__. When __stat__ is present and the invocation was successful,
it is assigned the value 0. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
ATOM, if the remote image has stopped, it is assigned the value of
iso\_fortran\_env's stat\_stopped\_image and if the remote image has
failed, the value stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_add
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*]
   call atomic_add (atom[1], this_image())
end program demo_atomic_add
```

## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_fetch\_add__(3)](ATOMIC_FETCH),
[__atomic\_and__(3)](ATOMIC_AND),
[__atomic\_or__(3)](ATOMIC_OR),
[__atomic\_xor__(3)](ATOMIC_XOR)
__iso\_fortran\_env__(3),

###### fortran-lang intrinsic descriptions
# ATOMIC_CAS
## __Name__

__atomic\_cas__(3) - \[ATOMIC\] Atomic compare and swap


## __Syntax__
```fortran
call atomic_cas (atom, old, compare, new, stat)
```
## __Description__

atomic\_cas compares the variable __atom__ with the value of __compare__; if the
value is the same, __atom__ is set to the value of __new__. Additionally, __old__ is
set to the value of __atom__ that was used for the comparison. When __stat__ is
present and the invocation was successful, it is assigned the value 0.
If it is present and the invocation has failed, it is assigned a
positive value; in particular, for a coindexed __atom__, if the remote image
has stopped, it is assigned the value of iso\_fortran\_env's
stat\_stopped\_image and if the remote image has failed, the value
stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of either integer type with
    atomic\_int\_kind kind or logical type with atomic\_logical\_kind
    kind.

  - __old__
    : Scalar of the same type and kind as __atom__.

  - __compare__
    : Scalar variable of the same type and kind as __atom__.

  - __new__
    : Scalar variable of the same type as __atom__. If kind is different, the
    value is converted to the kind of __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_cas
use iso_fortran_env
implicit none
logical(atomic_logical_kind) :: atom[*], prev
   call atomic_cas(atom[1], prev, .false., .true.)
end program demo_atomic_cas
```

## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_ref__(3)](ATOMIC_REF),
[__iso\_fortran\_env__(3)]()

###### fortran-lang intrinsic descriptions
# ATOMIC_DEFINE
## __Name__

__atomic\_define__(3) - \[ATOMIC\] Setting a variable atomically


## __Syntax__
```fortran
call atomic_define (atom, value, stat)

   subroutine atomic_define(atom, value, stat)
   TYPE(kind=KIND) :: atom
   TYPE(kind=KIND) :: value
   integer,intent(out),optional :: stat
```
## __Description__

__atomic\_define(atom, value)__ defines the variable __atom__ with the value
__value__ atomically. When __stat__ is present and the invocation was
successful, it is assigned the value __0__. If it is present and the
invocation has failed, it is assigned a positive value; in particular,
for a coindexed __atom__, if the remote image has stopped, it is assigned
the value of iso\_fortran\_env's stat\_stopped\_image and if the remote
image has failed, the value stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of either integer type with
    atomic\_int\_kind kind or logical type with atomic\_logical\_kind
    kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_define
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*]
    call atomic_define(atom[1], this_image())
end program demo_atomic_define
```
## __Standard__

Fortran 2008 and later; with __stat__, TS 18508 or later

## __See Also__

[__atomic\_ref__(3)](ATOMIC_REF),
[__atomic\_cas__(3)](ATOMIC_CAS),
__iso\_fortran\_env__(3),
[__atomic\_add__(3)](ATOMIC_ADD),
[__atomic\_and__(3)](ATOMIC_AND),
[__atomic\_or__(3)](ATOMIC_OR),
[__atomic\_xor__(3)](ATOMIC_XOR)

###### fortran-lang intrinsic descriptions
# ATOMIC_FETCH_ADD
## __Name__

__atomic\_fetch\_add__(3) - \[ATOMIC\] Atomic ADD operation with prior fetch


## __Syntax__
```fortran
call atomic_fetch_add(atom, value, old, stat)
```
## __Description__

__atomic\_fetch\_add(atom, value, old)__ atomically stores the value of
__atom__ in __old__ and adds the value of __var__ to the variable __atom__. When __stat__ is
present and the invocation was successful, it is assigned the value __0__.
If it is present and the invocation has failed, it is assigned a
positive value; in particular, for a coindexed __atom__, if the remote image
has stopped, it is assigned the value of iso\_fortran\_env's
stat\_stopped\_image and if the remote image has failed, the value
stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind. atomic\_logical\_kind kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __old__
    : Scalar of the same type and kind as __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_fetch_add
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*], old
   call atomic_add(atom[1], this_image(), old)
end program demo_atomic_fetch_add
```

## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_add__(3)](ATOMIC_ADD),
__iso\_fortran\_env__(3),

[__atomic\_fetch\_and__(3)](ATOMIC_FETCH_AND),
[__atomic\_fetch\_or__(3)](ATOMIC_FETCH_OR),

[__atomic\_fetch\_xor__(3)](ATOMIC_FETCH_XOR)

###### fortran-lang intrinsic descriptions
# ATOMIC_REF
## __Name__

__atomic\_ref__(3) - \[ATOMIC\] Obtaining the value of a variable atomically


## __Syntax__
```fortran
call atomic_ref(value, atom, stat)
```
## __Description__

__atomic\_ref(value, atom)__ atomically assigns the value of the
variable __atom__ to __value__. When __stat__ is present and the invocation was
successful, it is assigned the value __0__. If it is present and the
invocation has failed, it is assigned a positive value; in particular,
for a coindexed __atom__, if the remote image has stopped, it is assigned
the value of iso\_fortran\_env's __stat\_stopped\_image__ and if the remote
image has failed, the value __stat\_failed\_image__.

## __Arguments__

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __atom__
    : Scalar coarray or coindexed variable of either integer type with
    atomic\_int\_kind kind or logical type with atomic\_logical\_kind
    kind.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_ref
use iso_fortran_env
implicit none
logical(atomic_logical_kind) :: atom[*]
logical :: val
   call atomic_ref( val, atom[1] )
   ! ```
   call atomic_ref( val, atom[1] )
   if (val) then
      print *, "Obtained"
   endif
end program demo_atomic_ref
```

## __Standard__

Fortran 2008 and later; with STAT, TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_cas__(3)](ATOMIC_CAS),
[__iso\_fortran\_env__(3)](),

[__atomic\_fetch\_add__(3)](ATOMIC_ADD),
[__atomic\_fetch\_and__(3)](ATOMIC_AND),

[__atomic\_fetch\_or__(3)](ATOMIC_OR),
[__atomic\_fetch\_xor__(3)](ATOMIC_XOR)

###### fortran-lang intrinsic descriptions
