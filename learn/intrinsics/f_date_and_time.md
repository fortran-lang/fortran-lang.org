---
layout: book
title: date_and_time
permalink: /learn/intrinsics/f_date_and_time
---
### NAME

**date\_and\_time**(3f) - \[SYSTEM ENVIRONMENT\] gets
current time

### SYNTAX

```fortran
    subroutine date_and_time([date, time, zone, values])
     character(len=8),intent(out),optional :: date
     character(len=10),intent(out),optional :: time
     character(len=5),intent(out),optional :: zone
     integer,intent(out),optional :: values
```

### DESCRIPTION

**DATE\_AND\_TIME**(date, time, zone, values) gets the corresponding
date and time information from the real-time system clock.

Unavailable time and date CHARACTER parameters return blanks.

### ARGUMENTS

  - **DATE**
    The type shall be **CHARACTER**(len=8) or larger, and of default
    kind. DATE has the form ccyymmdd.

  - **TIME**
    The type shall be **CHARACTER**(len=10) or larger, and of default
    kind. TIME has the form hhmmss.sss.

  - **ZONE**
    The type shall be **CHARACTER**(len=5) or larger, and of default
    kind. ZONE has form (+-)hhmm, representing the difference with
    respect to Coordinated Universal Time (UTC).

  - **VALUES**
    An INTEGER array of eight elements. On return VALUES contains:

      - **value**(1): - The year

      - **value**(2): - The month

      - **value**(3): - The day of the month

      - **value**(4): - Time difference with UTC in minutes

      - **value**(5): - The hour of the day

      - **value**(6): - The minutes of the hour

      - **value**(7): - The seconds of the minute

      - **value**(8): - The milliseconds of the second

### EXAMPLE

Sample program:

```
    program demo_time_and_date
    implicit none
    character(len=8)     :: date
    character(len=10)    :: time
    character(len=5)     :: zone
    integer,dimension(8) :: values
        call date_and_time(date,time,zone,values)
        ! using keyword arguments
        call date_and_time(DATE=date,TIME=time,ZONE=zone)
        call date_and_time(VALUES=values)
        print '(*(g0))','DATE="',date,'" TIME="',time,'" ZONE="',zone,'"'
        write(*,'(i5,a)') &
         & values(1),' - The year', &
         & values(2),' - The month', &
         & values(3),' - The day of the month', &
         & values(4),' - Time difference with UTC in minutes', &
         & values(5),' - The hour of the day', &
         & values(6),' - The minutes of the hour', &
         & values(7),' - The seconds of the minute', &
         & values(8),' - The milliseconds of the second'
    end program demo_time_and_date
```

Results:

```
   DATE="20201222" TIME="165738.779" ZONE="-0500"
    2020 - The year
      12 - The month
      22 - The day of the month
    -300 - Time difference with UTC in minutes
      16 - The hour of the day
      57 - The minutes of the hour
      38 - The seconds of the minute
     779 - The milliseconds of the second
```

### STANDARD

Fortran 95 and later

### SEE ALSO

**cpu\_time**(3), **system\_clock**(3)

#### @urbanjost
