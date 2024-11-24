/*  File:    linux/i2c.pl
    Author:  Roy Ratcliffe, Northumberland, United Kingdom
    Created: Nov 17 2024
    Purpose: I2C
*/

:- module(linux_i2c,
          [ i2c_open/2,                         % +Dev,-I2C
            i2c_funcs/2,                        % +I2C,?Funcs
            i2c_slave/2,                        % +I2C,+Addr
            i2c_slave_force/2,                  % +I2C,+Addr
            i2c_write/3,                        % +I2C,++Bytes,-Actual
            i2c_write/2,                        % +I2C,++Bytes
            i2c_read/3,                         % +I2C,+Expected,?Bytes
            i2c_read/2                          % +I2C,?Bytes
          ]).
:- use_module(library(shlib)).

/** <module> linux_i2c
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

?- listing(linux_i2c:_).

%   Foreign: i2c_slave/2
true.

Open an I2C controller. Ask for its capabilities using the `I2C_FUNCS` call to
`ioctl` using the stream's file descriptor. See the followinmg snippet; some
details elided.

    ?- i2c_open('/dev/i2c-1', I2C).
    ?- linux_i2c:i2c_funcs_int($Stream, Funcs), format('~16r~n', [Funcs]).
    eff000d

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- load_foreign_library(foreign(linux_i2c)).

%!  i2c_open(+Dev:integer, -I2C) is semidet.

%!  i2c_funcs(+I2C, -Funcs:list) is det.

i2c_funcs(I2C, Funcs), var(Funcs) =>
    i2c_funcs_int(I2C, Int),
    i2c_funcs_terms(Int, Funcs).
i2c_funcs(I2C, Funcs) =>
    i2c_funcs(I2C, Funcs0),
    ord_subset(Funcs, Funcs0).

%!  i2c_slave(+I2C, +Addr:integer) is det.

%!  i2c_write(+I2C, ++Bytes:list, ?Actual:integer) is semidet.
%!  i2c_write(+I2C, ++Bytes:list) is semidet.
%
%   The 2-arity form fails if the Actual number of bytes written does
%   not match the number of Bytes.

i2c_write(I2C, Bytes) :-
    length(Bytes, Actual),
    i2c_write(I2C, Bytes, Actual).

%!  i2c_read(+I2C, +Expected:integer, ?Bytes:list) is semidet.
%!  i2c_read(+I2C, ?Bytes:list) is semidet.

i2c_read(I2C, Bytes) :-
    length(Bytes, Expected),
    i2c_read(I2C, Expected, Bytes).
