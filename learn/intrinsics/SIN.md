---
layout: book
title: sin
permalink: /learn/intrinsics/SIN
---
### NAME

**sin**(3f) - \[MATHEMATICS:TRIGONOMETRIC\] Sine function
(MIT)

### SYNTAX

result = **sin**(x)

### DESCRIPTION

**sin**(3f) computes the sine of an angle given the size of the angle in
radians.

The sine of an angle in a right-angled triangle is the ratio of the
length of the side opposite the given angle divided by the length of the
hypotenuse.

### ARGUMENTS

  - **X**
    The type shall be REAL or COMPLEX in radians.

### RETURN VALUE

  - **RESULT**
    The return value has the same type and kind as X.

### EXAMPLE

Sample program:

```fortran
    program sample_sin
    implicit none
      real :: x = 0.0
      x = sin(x)
    end program sample_sin
```

### HAVERSINE FORMULA

From the article on "Haversine formula" in Wikipedia:

```
      The haversine formula is an equation important in navigation,
      giving great-circle distances between two points on a sphere from
      their longitudes and latitudes.
```

So to show the great-circle distance between the Nashville International
Airport (BNA) in TN, USA, and the Los Angeles International Airport
(LAX) in CA, USA you would start with their latitude and longitude,
commonly given as

```
     BNA: N 36 degrees 7.2',   W 86 degrees 40.2'
     LAX: N 33 degrees 56.4',  W 118 degrees 24.0'
```

which converted to floating-point values in degrees is:

```
          Latitude Longitude
```

>   - **BNA**
>     36.12, **-86.67**
>
>   - **LAX**
>     33.94, **-118.40**

And then use the haversine formula to roughly calculate the distance
along the surface of the Earth between the locations:

> Sample program:
>
> ```
>     program demo_sin
>     implicit none
>     real :: d
>         d = haversine(36.12,-86.67, 33.94,-118.40) ! BNA to LAX
>         print '(A,F9.4,A)', 'distance: ',d,' km'
>     contains
>     function haversine(latA,lonA,latB,lonB) result (dist)
>     !
>     ! calculate great circle distance in kilometers
>     ! given latitude and longitude in degrees
>     !
>     real,intent(in) :: latA,lonA,latB,lonB
>     real :: a,c,dist,delta_lat,delta_lon,lat1,lat2
>     real,parameter :: radius = 6371 ! mean earth radius in kilometers,
>     ! recommended by the International Union of Geodesy and Geophysics
>     real, parameter :: deg_to_rad = atan(1.0)/45.0 ! generate constant pi/180
>        delta_lat = deg_to_rad*(latB-latA)
>        delta_lon = deg_to_rad*(lonB-lonA)
>        lat1 = deg_to_rad*(latA)
>        lat2 = deg_to_rad*(latB)
>        a = (sin(delta_lat/2))**2 + cos(lat1)*cos(lat2)*(sin(delta_lon/2))**2
>        c = 2*asin(sqrt(a))
>        dist = radius*c
>     end function haversine
>     end program demo_sin
> ```
>
> Results:
>
> ```
>    distance: 2886.4446 km
> ```

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental function

### SEE ALSO

**asin**(3), **cos**(3), **tan**(3)

#### @urbanjost
