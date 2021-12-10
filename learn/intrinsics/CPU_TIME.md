---
layout: book
title: cpu_time
permalink: /learn/intrinsics/CPU_TIME
---
## __Name__

__cpu\_time__(3) - \[SYSTEM:TIME\] return CPU processor time in seconds

## __Syntax__
```fortran
     call cpu_time(time)
     real,intent(out) :: time
```
## __Description__

Returns a _real_ value representing the elapsed CPU time in seconds. This
is useful for testing segments of code to determine execution time.

The exact definition of time is left imprecise because of the
variability in what different processors are able to provide.

If no time source is available, TIME is set to a negative value.

Note that TIME may contain a system dependent, arbitrary offset and may
not start with 0.0. For cpu\_time the absolute value is meaningless.
Only differences between subsequent calls, as shown in the example
below, should be used.

A processor for which a single result is inadequate (for example, a
parallel processor) might choose to provide an additional version for
which time is an array.

## __Returns__

  - __TIME__
    : The type shall be _real_ with __intent(out)__. It is assigned a
    processor-dependent approximation to the processor time in seconds.
    If the processor cannot return a meaningful time, a
    processor-dependent negative value

      - __is returned.__
        The start time is left imprecise because the purpose is to time
        sections of code, as in the example. This might or might not
        include system overhead time.

## __Examples__

Sample program:

```fortran
program demo_cpu_time
implicit none
real :: start, finish
   !
   call cpu_time(start)
   ! put code to test here
   call cpu_time(finish)
   !
   ! writes processor time taken by the piece of code.
   print '("Processor Time = ",f6.3," seconds.")',finish-start
end program demo_cpu_time
```

## __Standard__

Fortran 95 and later

## __See Also__

[__system\_clock__(3)](SYSTEM_CLOCK),
[__date\_and\_time__(3)](DATE_AND_TIME)

###### fortran-lang intrinsic descriptions (@urbanjost)
