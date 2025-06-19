# About

This package adds vector/list support to the Tcl expr command.

It's a proof-of-concept implementation: Use it on your own risk.

# Example

This example computes the median absolute deviation of a vector/list:

    set x {2.3 3.1 2.8 2.5 3.2}
    set mad [expr {median(abs(sub($x, median($x))))}]

It is possible to intermix scalar and vectors:

    set x {1.0 2.0 3.0 4.0 5.0}
    set y [expr {sub(mul($x,2),1)}]
    # y: 1.0 3.0 5.0 7.0 9.0

# Usage

Put package into auto_path and load it:

    package require expr++

The changes this package brings get into effect after loading it.

Beside replacing existing functions the package adds some new expr functions:

    nroot(x, n) - Compute the Nth root of X
    sgn(x) - Signum function
    fix(x) - Return integral part X
    trunc(x) - Truncate the fractional part of X
    asinh(x) - Inverse hyperbolic sine
    acosh(x) - Inverse hyperbolic cosine
    atanh(x) - Inverse hyperbolic tangens
    sat(x, z, a) - Saturate X in range [a, z]
    fit(x, type) - Fit value to range of given type
    mean(x) - Compute the mean value (average) of X
    median(x) - Compute the median value of X

Tcl does not allow to replace expr operators. Therefore new expr functions
are added that can handle vectors/lists:

    add(a, b)
    sub(a, b)
    mul(a, b)
    div(a, b)
    mod(a, b)

The capability of round() function is extended by optional new arguments
to specify the rounding mode and integral saturation:

    round(x, mode, sat)

By default the behavior of round() is unchanged.
Mode can be one of the following:

    "infinity" (round ties away from zero)
    "zero" (round ties towards zero)
    "ceil" (round ties towards +inf)
    "floor" (round ties towards -inf)

The sat argument corresponds to the type argument of the fix() function which are:

    "int" (integer of arbitrary size)
    "bool" (boolean value)
    "double" (floating point value)
    "byte" (8 bit unsigned integer)
    "int8" (8 bit signed integer)
    "int16" (16 bit signed integer)
    "int32" (32 bit signed integer)
    "int64" (64 bit signed integer)
    "uint8" (8 bit unsigned integer)
    "uint16" (16 bit unsigned integer)
    "uint32" (32 bit unsigned integer)
    "uint64" (64 bit unsigned integer)
