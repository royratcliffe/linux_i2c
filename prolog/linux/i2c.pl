/*  File:    linux/i2c.pl
    Author:  Roy Ratcliffe, Northumberland, United Kingdom
    Created: Nov 17 2024
    Purpose: I2C
*/

:- module(linux_i2c,
          [ i2c_funcs/2,                        % +Stream,-Funcs
            i2c_open/2,                         % +Pathname,-Dev
            i2c_write/3,                        % +Dev,++Bytes,-Actual
            i2c_write/2,                        % +Dev,++Bytes
            i2c_read/3,                         % +Dev,+Expected,-Bytes
            i2c_read/2                          % +Dev,+Bytes
          ]).
:- use_module(library(shlib)).

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
    i2c_funcs_dev_to_int(Stream, Int),
    i2c_funcs_int_to_list(Int, Funcs).

%!  i2c_open(+Pathname, -Dev) is semidet.

%!  i2c_write(+Dev, ++Bytes, -Actual) is semidet.
%!  i2c_write(+Dev, ++Bytes) is semidet.
%
%   The 2-arity form fails if the Actual number of bytes written does
%   not match the number of Bytes.

i2c_write(Dev, Bytes) :-
    i2c_write(Dev, Bytes, Actual),
    length(Bytes, Actual).

%!  i2c_read(+Dev, +Bytes) is semidet.

i2c_read(Dev, Bytes) :-
    length(Bytes, Expected),
    i2c_read(Dev, Expected, Bytes).
