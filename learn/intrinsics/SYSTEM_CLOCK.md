---
layout: book
title: system_clock
permalink: /learn/intrinsics/SYSTEM_CLOCK
---
#### NAME

__system\_clock__(3f) - \[SYSTEM ENVIRONMENT\] Return numeric data from a real-time clock.
(GFDL)

#### SYNTAX

subroutine __system\_clock__(\[count, count\_rate, count\_max\])

```
   integer,intent(out),optional  :: count
   integer,intent(out),optional  :: count_rate
      or
   real,intent(out),optional     :: count_rate
   integer,intent(out,optional   :: count_max
```

#### DESCRIPTION

system\_clock lets you measure durations of time with the precision of
the smallest time increment generally available on a system by returning
processor-dependent values based on the current value of the processor
clock. The CLOCK value is incremented by one for each clock count until
the value count\_max is reached and is then reset to zero at the next
count. CLOCK therefore is a modulo value that lies in the range 0 to
count\_max. count\_rate and count\_max are assumed constant (even though
CPU rates can vary on a single platform).

count\_rate is system dependent and can vary depending on the kind of
the arguments.

If there is no clock, or querying the clock fails, COUNT is set to
__-huge__(count), and count\_rate and count\_max are set to zero.

system\_clock is typically used to measure short time intervals (system
clocks may be 24-hour clocks or measure processor clock ticks since
boot, for example). It is most often used for measuring or tracking the
time spent in code blocks in lieu of using profiling tools.

#### ARGUMENTS

  - __COUNT__
    (optional) shall be an integer scalar. It is assigned a
    processor-dependent value based on the current value of the
    processor clock, or __-HUGE__ (COUNT) if there is no clock. The
    processor-dependent value is incremented by one for each clock count
    until the value COUNT\_MAX is reached and is reset to zero at the
    next count. It lies in the range 0 to COUNT\_MAX if there is a
    clock.

  - __COUNT\_RATE__
    (optional) shall be an integer or real scalar. It is assigned a
    processor-dependent approximation to the number of processor clock
    counts per second, or zero if there is no clock.

  - __COUNT\_MAX__
    (optional) shall be an integer scalar. It is assigned the maximum
    value that COUNT can have, or zero if there is no clock.

#### EXAMPLE

Sample program:

```
      program demo_system_clock
      implicit none
        integer :: count, count_rate, count_max
        call system_clock(count, count_rate, count_max)
        write(*,*) count, count_rate, count_max
      end program demo_system_clock
```

If the processor clock is a 24-hour clock that registers time at
approximately 18.20648193 ticks per second, at 11:30 A.M. the reference

```
      CALL SYSTEM_CLOCK (COUNT = C, COUNT_RATE = R, COUNT_MAX = M)
```

defines

```
      C = (11*3600+30*60)*18.20648193 = 753748,
      R = 18.20648193, and
      M = 24*3600*18.20648193-1 = 1573039.
```

#### STANDARD

Fortran 95 and later

#### CLASS

Subroutine

#### SEE ALSO

__date\_and\_time__(3), __cpu\_time__(3)
