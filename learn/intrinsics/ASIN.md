---
layout: book
title: asin
permalink: /learn/intrinsics/ASIN
---
#### NAME

__asin__(3f) - \[MATHEMATICS:TRIGONOMETRIC\] Arcsine function
(MIT)

#### SYNTAX

```fortran
result = asin(X)
```

#### DESCRIPTION

__asin__(x) computes the arcsine of its argument X.

The arcsine is the inverse function of the sine function. It is commonly
used in trigonometry when trying to find the angle when the lengths of
the hypotenuse and the opposite side of a right triangle are known.

#### ARGUMENTS

  - __X__
    The type shall be either REAL and a magnitude that is less than or
    equal to one; or be COMPLEX.

#### RETURN VALUE

  - __RESULT__
    The return value is of the same type and kind as X. The real part of
    the result is in radians and lies in the range __-PI__/2 \<=
    __asin__(x) \<= PI/2.

#### EXAMPLE

The arcsine will allow you to find the measure of a right angle when you
know the ratio of the side opposite the angle to the hypotenuse.

So if you knew that a train track rose 1.25 vertical miles on a track
that was 50 miles long, you could determine the average angle of incline
of the track using the arcsine. Given

```fortran
    sin(theta) = 1.25 miles/50 miles (opposite/hypotenuse)

    program demo_asin
    use, intrinsic :: iso_fortran_env, only : dp=>real64
    implicit none
    ! value to convert degrees to radians
    real(kind=dp),parameter :: D2R=acos(-1.0_dp)/180.0_dp
    real(kind=dp)           :: angle, rise, run
      ! given sine(theta) = 1.25 miles/50 miles (opposite/hypotenuse)
      ! then taking the arcsine of both sides of the equality yields
      ! theta = arcsine(1.25 miles/50 miles) ie. arcsine(opposite/hypotenuse)
      rise=1.250_dp
      run=50.00_dp
      angle = asin(rise/run)
      write(*,*)'angle of incline(radians) = ', angle
      angle = angle/D2R
      write(*,*)'angle of incline(degrees) = ', angle

      write(*,*)'percent grade=',rise/run*100.0_dp
    end program demo_asin
```

Results:

```
    angle of incline(radians) =    2.5002604899361139E-002
    angle of incline(degrees) =    1.4325437375665075
    percent grade=   2.5000000000000000
```

The percentage grade is the slope, written as a percent. To calculate
the slope you divide the rise by the run. In the example the rise is
1.25 mile over a run of 50 miles so the slope is 1.25/50 = 0.025.
Written as a percent this is 2.5 %.

> for the US, two and 1/2 percent is generally thought of as the upper
> limit. This means a rise of 2.5 feet when going 100 feet forward. In
> the US this was the maximum grade on the first major US railroad, the
> Baltimore and Ohio. Note curves increase the frictional drag on a
> train reducing the allowable grade.

#### STANDARD

FORTRAN 77 and later, for a complex argument Fortran 2008 or later

#### CLASS

Elemental procedure\|Elemental function

#### SEE ALSO

Inverse function: __sin__(3)

##### @urbanjost
