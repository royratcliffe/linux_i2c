:- begin_tests(i2c_dev_1_pca9685_40).
:- use_module(i2c).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

These tests assume that an NXP PCA9685 exists on I2C channel 1 at
7-bit slave address 40 hexadecimal.

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

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

i2c_dev(1).

pca9685_addr(16'40).

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

rd(I2C, Adr, Bytes) :- i2c_write(I2C, [Adr]), i2c_read(I2C, Bytes).

wr(I2C, Adr, Bytes) :- i2c_write(I2C, [Adr|Bytes]).

ai(I2C) :-
    reg_adr(mode(1), Adr),
    rd(I2C, Adr, [Mode1]),
    (   Mode1 /\ 2'0010_0000 =\= 16'00
    ->  true
    ;   Mode1_ is Mode1 \/ 2'0010_0000,
        wr(I2C, Adr, [Mode1_])
    ).

pair(A, B, A-B).

zip(A, B, C) :- maplist(pair, A, B, C).

adr_bytes(Adr, Bytes, AdrBytes) :-
    length(Bytes, Bytes1),
    Adr1 is Adr + Bytes1 - 1,
    fdset_to_list(from_to(n(Adr), n(Adr1)), Adrs),
    zip(Adrs, Bytes, AdrBytes).

test(mode1, true(Byte /\ 2'0010_0000 =\= 16'00)) :-
    i2c_dev(Dev),
    i2c_open(Dev, I2C),
    pca9685_addr(Addr),
    i2c_slave(I2C, Addr),
    ai(I2C),
    % Write 00 to the Control Register.
    % It determines access to the other registers.
    i2c_write(I2C, [16'00]),
    i2c_read(I2C, [Byte]).

test(mode1_mode2, [Mode1, Mode2] == [16'31, 16'04]) :-
    i2c_dev(Dev),
    i2c_open(Dev, I2C),
    pca9685_addr(Addr),
    i2c_slave(I2C, Addr),
    ai(I2C),
    i2c_write(I2C, [16'00]),
    i2c_read(I2C, [Mode1, Mode2]).

test(dump_46) :-
    i2c_dev(Dev),
    i2c_open(Dev, I2C),
    pca9685_addr(Addr),
    i2c_slave(I2C, Addr),
    ai(I2C),
    length(Bytes, 16'46),
    rd(I2C, 16'00, Bytes),
    adr_bytes(16'00, Bytes, AdrBytes),
    forall(member(Adr-Byte, AdrBytes),
           (   reg_adr(Reg, Adr),
               format('~n~|~`0t~16r~2+: ~|~`0t~16r~2+ ~k', [Adr, Byte, Reg])
           )),
    nl.

test(dump_fa) :-
    i2c_dev(Dev),
    i2c_open(Dev, I2C),
    pca9685_addr(Addr),
    i2c_slave(I2C, Addr),
    ai(I2C),
    i2c_write(I2C, [16'fa]),
    length(Bytes, 6),
    i2c_read(I2C, Bytes),
    fdset_to_list(from_to(n(16'fa), n(16'ff)), Adrs),
    zip(Adrs, Bytes, AdrBytes),
    forall(member(Adr-Byte, AdrBytes),
           (   reg_adr(Reg, Adr),
               format('~n~|~`0t~16r~2+: ~|~`0t~16r~2+ ~k', [Adr, Byte, Reg])
           )),
    nl.

:- end_tests(i2c_dev_1_pca9685_40).
