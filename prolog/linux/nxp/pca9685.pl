/*  File:    linux/nxp/pca9685.pl
    Author:  Roy Ratcliffe, Northumberland, United Kingdom
    Created: Nov 24 2024
    Purpose: NXP PCA9685 for Linux

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

:- module(linux_nxp_pca9685,
          [ led_adr/3,                          % ?OnOff,?LH,?Adr0
            reg_adr/2,                          % ?Reg,?Adr
            wr/3,                               % +I2C,+Adr:integer,++Bytes:list
            rd/3,                               % +I2C,+Adr:integer,?Bytes:list
            rd/3,                               % +I2C,+Adr:integer,?Bytes:list
            ai/1                                % +I2C
          ]).
:- use_module(library(linux/i2c)).

/** <module> NXP PCA9685

The PCA9685 is a 16-channel LED controller that operates via I^2^C-bus,
specifically designed for Red/Green/Blue/Amber (RGBA) colour
back-lighting applications. Each LED output features its own 12-bit
resolution, equating to 4096 brightness levels, managed by a dedicated
PWM (Pulse Width Modulation) controller. This controller can be
programmed to operate at frequencies ranging from a typical 24 Hz to
1526 Hz, with the duty cycle adjustable between 0% and 100%, allowing
for precise control over brightness levels. Notably, all outputs
maintain the same PWM frequency, ensuring consistency in lighting
performance.

@see https://www.nxp.com/products/interfaces/ic-spi-i3c-interface-devices/i3c-interface-devices/led-drivers/16-channel-12-bit-pwm-fm-plus-ic-bus-led-driver:PCA9685

*/

on_off(on).
on_off(off).

l_h(l).
l_h(h).

led(OnOff, LH) :- on_off(OnOff), l_h(LH).

%!  led_adr(?OnOff, ?LH, ?Adr0) is nondet.
%
%   Adr0 is the PWM control's on-off low-high relative register address
%   offset, between 0 and 3 inclusive.

led_adr(on,  l, 16'00).
led_adr(on,  h, 16'01).
led_adr(off, l, 16'02).
led_adr(off, h, 16'03).

%!  reg_adr(?Reg, ?Adr) is nondet.
%
%   Maps the entire PCA9685 register file.
%
%       00: 31 mode(1)
%       01: 04 mode(2)
%       02: e2 subadr(1)
%       03: e4 subadr(2)
%       04: e8 subadr(3)
%       05: e0 allcalladr
%       06: 00 led(0,on,l)
%       07: 00 led(0,on,h)
%       08: 00 led(0,off,l)
%       09: 10 led(0,off,h)
%       0a: 00 led(1,on,l)
%       0b: 00 led(1,on,h)
%       0c: 00 led(1,off,l)
%       0d: 10 led(1,off,h)
%       0e: 00 led(2,on,l)
%       0f: 00 led(2,on,h)
%       10: 00 led(2,off,l)
%       11: 10 led(2,off,h)
%       12: 00 led(3,on,l)
%       13: 00 led(3,on,h)
%       14: 00 led(3,off,l)
%       15: 10 led(3,off,h)
%       16: 00 led(4,on,l)
%       17: 00 led(4,on,h)
%       18: 00 led(4,off,l)
%       19: 10 led(4,off,h)
%       1a: 00 led(5,on,l)
%       1b: 00 led(5,on,h)
%       1c: 00 led(5,off,l)
%       1d: 10 led(5,off,h)
%       1e: 00 led(6,on,l)
%       1f: 00 led(6,on,h)
%       20: 00 led(6,off,l)
%       21: 10 led(6,off,h)
%       22: 00 led(7,on,l)
%       23: 00 led(7,on,h)
%       24: 00 led(7,off,l)
%       25: 10 led(7,off,h)
%       26: 00 led(8,on,l)
%       27: 00 led(8,on,h)
%       28: 00 led(8,off,l)
%       29: 10 led(8,off,h)
%       2a: 00 led(9,on,l)
%       2b: 00 led(9,on,h)
%       2c: 00 led(9,off,l)
%       2d: 10 led(9,off,h)
%       2e: 00 led(10,on,l)
%       2f: 00 led(10,on,h)
%       30: 00 led(10,off,l)
%       31: 10 led(10,off,h)
%       32: 00 led(11,on,l)
%       33: 00 led(11,on,h)
%       34: 00 led(11,off,l)
%       35: 10 led(11,off,h)
%       36: 00 led(12,on,l)
%       37: 00 led(12,on,h)
%       38: 00 led(12,off,l)
%       39: 10 led(12,off,h)
%       3a: 00 led(13,on,l)
%       3b: 00 led(13,on,h)
%       3c: 00 led(13,off,l)
%       3d: 10 led(13,off,h)
%       3e: 00 led(14,on,l)
%       3f: 00 led(14,on,h)
%       40: 00 led(14,off,l)
%       41: 10 led(14,off,h)
%       42: 00 led(15,on,l)
%       43: 00 led(15,on,h)
%       44: 00 led(15,off,l)
%       45: 10 led(15,off,h)
%       fa: 00 led(all,on,l)
%       fb: 00 led(all,on,h)
%       fc: 00 led(all,off,l)
%       fd: 00 led(all,off,h)
%       fe: 1e pre(scale)
%       ff: 31 test(mode)

reg_adr(mode(Mode), Adr) :-
    between(1, 2, Mode),
    Adr is 16'00 + Mode - 1.
reg_adr(subadr(SubAdr), Adr) :-
    between(1, 3, SubAdr),
    Adr is 16'02 + SubAdr - 1.
reg_adr(allcalladr, 16'05).
reg_adr(led(LED, OnOff, LH), Adr) :-
    between(0, 15, LED),
    led_adr(OnOff, LH, Adr0),
    Adr is 16'06 + Adr0 + (16'04 * LED).
reg_adr(led(all, OnOff, LH), Adr) :-
    led_adr(OnOff, LH, Adr0),
    Adr is 16'fa + Adr0.
reg_adr(pre(scale), 16'fe).
reg_adr(test(mode), 16'ff).

%!  wr(+I2C, +Adr:integer, ++Bytes:list) is semidet.

wr(I2C, Adr, Bytes) :- i2c_write(I2C, [Adr|Bytes]).

%!  rd(+I2C, +Adr:integer, ?Bytes:list) is semidet.

rd(I2C, Adr, Bytes) :- i2c_write(I2C, [Adr]), i2c_read(I2C, Bytes).

%!  ai(+I2C) is semidet.

ai(I2C) :-
    reg_adr(mode(1), Adr),
    rd(I2C, Adr, [Mode1]),
    (   Mode1 /\ 2'0010_0000 =\= 16'00
    ->  true
    ;   Mode1_ is Mode1 \/ 2'0010_0000,
        wr(I2C, Adr, [Mode1_])
    ).
