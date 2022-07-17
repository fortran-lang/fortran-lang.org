---
layout: book
title: system_clock
permalink: /learn/intrinsics/SYSTEM_CLOCK
---
# SYSTEM_CLOCK
## __Name__

__system\_clock__(3) - \[SYSTEM:TIME\] Return numeric data from a real-time clock.


## __Syntax__
```fortran
subroutine system_clock(count, count_rate, count_max)

   integer,intent(out),optional  :: count
   integer,intent(out),optional  :: count_rate
    ! or !
   real,intent(out),optional     :: count_rate
   integer,intent(out),optional  :: count_max
```
## __Description__

__system\_clock__ lets you measure durations of time with the precision of
the smallest time increment generally available on a system by returning
processor-dependent values based on the current value of the processor
clock. The __clock__ value is incremented by one for each clock count until
the value __count\_max__ is reached and is then reset to zero at the next
count. __clock__ therefore is a modulo value that lies in the range __0 to
count\_max__. __count\_rate__ and __count\_max__ are assumed constant (even though
CPU rates can vary on a single platform).

__count\_rate__ is system dependent and can vary depending on the kind of
the arguments.

If there is no clock, or querying the clock fails, __count__ is set to
__-huge(count)__, and __count\_rate__ and __count\_max__ are set to zero.

__system\_clock__ is typically used to measure short time intervals (system
clocks may be 24-hour clocks or measure processor clock ticks since
boot, for example). It is most often used for measuring or tracking the
time spent in code blocks in lieu of using profiling tools.

## __Arguments__

  - __count__
    : (optional) shall be an _integer_ scalar. It is assigned a
    processor-dependent value based on the current value of the
    processor clock, or __-huge(count)__ if there is no clock. The
    processor-dependent value is incremented by one for each clock count
    until the value __count\_max__ is reached and is reset to zero at the
    next count. It lies in the range __0__ to __count\_max__ if there is a
    clock.

  - __count\_rate__
    : (optional) shall be an _integer_ or _real_ scalar. It is assigned a
    processor-dependent approximation to the number of processor clock
    counts per second, or zero if there is no clock.

  - __count\_max__
    : (optional) shall be an _integer_ scalar. It is assigned the maximum
    value that __COUNT__ can have, or zero if there is no clock.

## __Examples__

Sample program:
```fortran
program demo_system_clock
implicit none
integer, parameter :: wp = kind(1.0d0)
integer :: count, count_rate, count_max
integer :: start, finish
real    :: time_read

   call system_clock(count, count_rate, count_max)
   write(*,*) count, count_rate, count_max

   call system_clock(start, count_rate)
   ! <<<< code to time
   call system_clock(finish)
   time_read=(finish-start)/real(count_rate,wp)
   write(*,'(a30,1x,f7.4,1x,a)') 'time * : ', time_read, ' seconds'

end program demo_system_clock
```
If the processor clock is a 24-hour clock that registers time at
approximately 18.20648193 ticks per second, at 11:30 A.M. the reference
```fortran
      call system_clock (count = c, count_rate = r, count_max = m)
```
defines

```text
      C = (11*3600+30*60)*18.20648193 = 753748,
      R = 18.20648193, and
      M = 24*3600*18.20648193-1 = 1573039.
```
## __Standard__

Fortran 95 and later

## __See Also__

[__date\_and\_time__(3)](DATE_AND_TIME),
[__cpu\_time__(3)](CPU_TIME)

###### fortran-lang intrinsic descriptions
