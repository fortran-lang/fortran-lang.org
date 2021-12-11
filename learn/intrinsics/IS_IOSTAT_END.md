---
layout: book
title: is_iostat_end
permalink: /learn/intrinsics/IS_IOSTAT_END
---
## __Name__

__is\_iostat\_end__(3) - \[STATE\] Test for end-of-file value
(GFDL)

## __Syntax__
```fortran
function is_iostat_end(i)

    logical function   :: is_iostat_end (i) result(yesno)
    integer,intent(in) :: i
```
## __Description__

is\_iostat\_end(3) tests whether a variable (assumed returned as a status
from an I/O statement) has the "end of file" I/O status value. 

The function is equivalent to comparing the variable with the
__iostat\_end__ parameter of the intrinsic module __iso\_fortran\_env__.

## __Arguments__

  - __i__
    : An _integer_ status value to test if indicating end of file.

## __Returns__

Returns a _logical_ of the default kind, __.true.__ if __i__ has the value
which indicates an end of file condition for __iostat=__ specifiers, and is
__.false.__ otherwise.

## __Examples__

Sample program:

```fortran
program demo_iostat
implicit none
real               :: value
integer            :: ios
character(len=256) :: message
   write(*,*)'Begin entering numeric values, one per line'
   do
      read(*,*,iostat=ios,iomsg=message)value
      if(ios.eq.0)then
         write(*,*)'VALUE=',value
      elseif( is_iostat_end(ios) ) then
         stop 'end of file. Goodbye!'
      else
         write(*,*)'ERROR:',ios,trim(message)
      endif
      !
   enddo
end program demo_iostat
```

## __Standard__

Fortran 2003 and later

###### fortran-lang intrinsic descriptions (@urbanjost)
