/*  File:    linux/i2c.pl
    Author:  Roy Ratcliffe, Northumberland, United Kingdom
    Created: Nov 17 2024
    Purpose: I2C Device on Linux

Copyright (c) 2024, Roy Ratcliffe, Northumberland, United Kingdom

SPDX-License-Identifier: MIT

Permission is hereby granted, free of charge,  to any person obtaining a
copy  of  this  software  and    associated   documentation  files  (the
"Software"), to deal in  the   Software  without  restriction, including
without limitation the rights to  use,   copy,  modify,  merge, publish,
distribute, sublicense, and/or sell  copies  of   the  Software,  and to
permit persons to whom the Software is   furnished  to do so, subject to
the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT  WARRANTY OF ANY KIND, EXPRESS
OR  IMPLIED,  INCLUDING  BUT  NOT   LIMITED    TO   THE   WARRANTIES  OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR   PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS  OR   COPYRIGHT  HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY,  WHETHER   IN  AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM,  OUT  OF   OR  IN  CONNECTION  WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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

/** <module> Linux I2C Devices
@author Roy Ratcliffe
*/

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
