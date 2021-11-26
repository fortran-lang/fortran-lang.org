---
layout: book
title: date_and_time
permalink: /learn/intrinsics/DATE_AND_TIME
---
## __Name__

__date\_and\_time__(3) - \[SYSTEM ENVIRONMENT\] gets current time

## __Syntax__

```fortran
    subroutine date_and_time([date, time, zone, values])
     character(len=8),intent(out),optional :: date
     character(len=10),intent(out),optional :: time
     character(len=5),intent(out),optional :: zone
     integer,intent(out),optional :: values
```

## __Description__

__DATE\_AND\_TIME__(date, time, zone, values) gets the corresponding
date and time information from the real-time system clock.

Unavailable time and date CHARACTER parameters return blanks.

## __Arguments__

  - __DATE__
    The type shall be __CHARACTER__(len=8) or larger, and of default
    kind. DATE has the form ccyymmdd.

  - __TIME__
    The type shall be __CHARACTER__(len=10) or larger, and of default
    kind. TIME has the form hhmmss.sss.

  - __ZONE__
    The type shall be __CHARACTER__(len=5) or larger, and of default
    kind. ZONE has form (+-)hhmm, representing the difference with
    respect to Coordinated Universal Time (UTC).

  - __VALUES__
    An _integer_ array of eight elements. On return VALUES contains:

      - __value__(1): - The year

      - __value__(2): - The month

      - __value__(3): - The day of the month

      - __value__(4): - Time difference with UTC in minutes

      - __value__(5): - The hour of the day

      - __value__(6): - The minutes of the hour

      - __value__(7): - The seconds of the minute

      - __value__(8): - The milliseconds of the second

## __Examples__

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

## __Standard__

Fortran 95 and later

## __See Also__

__cpu\_time__(3), __system\_clock__(3)

##### fortran-lang intrinsic descriptions
