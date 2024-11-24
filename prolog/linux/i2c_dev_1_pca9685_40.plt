:- begin_tests(i2c_dev_1_pca9685_40).
:- use_module(i2c).
:- use_module(pca9685).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

These tests assume that an NXP PCA9685 exists on I2C channel 1 at
7-bit slave address 40 hexadecimal.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

i2c_dev(1).

pca9685_addr(16'40).

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
