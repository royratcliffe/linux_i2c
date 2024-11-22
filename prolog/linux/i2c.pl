/*  File:    linux/i2c.pl
    Author:  Roy Ratcliffe, Northumberland, United Kingdom
    Created: Nov 17 2024
    Purpose: I2C
*/

:- module(linux_i2c,
          [ i2c_funcs/2                         % +Stream,-Funcs
          ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

?- listing(linux_i2c:_).

%   Foreign: i2c_slave/2
true.

Open an I2C controller. Ask for its capabilities using the `I2C_FUNCS` call to
`ioctl` using the stream's file descriptor. See the followinmg snippet; some
details elided.

    ?- open('/dev/i2c-1', update, Stream, [encoding(octet)]).
    ?- linux_i2c:i2c_funcs($Stream, Funcs), format('~16r~n', [Funcs]).
    eff000d

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- load_foreign_library(foreign(linux_i2c)).

%!  i2c_funcs(+Stream, -Funcs) is det.

i2c_funcs(Stream, Funcs) :-
    i2c_funcs_stream_to_int(Stream, Int),
    i2c_funcs_int_to_list(Int, Funcs).
