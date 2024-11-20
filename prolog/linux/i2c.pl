/*  File:    linux/i2c.pl
    Author:  Roy Ratcliffe, Northumberland, United Kingdom
    Created: Nov 17 2024
    Purpose: I2C
*/

:- module(linux_i2c,
          [
          ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

?- listing(linux_i2c:_).

%   Foreign: i2c_slave/2
true.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- load_foreign_library(foreign(linux_i2c)).
