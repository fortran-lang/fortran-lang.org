---
layout: book
title: cpu_time
permalink: /learn/intrinsics/f_cpu_time
---
### NAME

**cpu\_time**(3f) - \[SYSTEM ENVIRONMENT\] return CPU processor time in seconds

### SYNTAX


```fortran
     call cpu_time(time)
     real,intent(out) :: time
```

### DESCRIPTION

Returns a REAL value representing the elapsed CPU time in seconds. This
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

### RETURN VALUE

  - **TIME**
    The type shall be REAL with **intent**(out). It is assigned a
    processor-dependent approximation to the processor time in seconds.
    If the processor cannot return a meaningful time, a
    processor-dependent negative value

      - **is returned.**
        The start time is left imprecise because the purpose is to time
        sections of code, as in the example. This might or might not
        include system overhead time.

### EXAMPLE

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

### STANDARD

Fortran 95 and later

### CLASS

Subroutine

### SEE ALSO

**system\_clock**(3), **date\_and\_time**(3)

#### @urbanjost
