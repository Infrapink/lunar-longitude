# lunar-longitude
This is a simple Fortran module to calculate the exact geocentric position of the moon, accurate to 0.01 degrees. It is based on the algorithm described in Chapter 47 of *Astronomical Algorithms, 2nd Edition* by Jean Meeus (Willmann-Bell, 1998)

It uses the Mean Julian Day, which runs from noon to noon UTC. Thus, for example, 01 January 1970 lasts from 2440586.5 to 2440587.5. This must be taken into account when passing arguments to getlambda().
