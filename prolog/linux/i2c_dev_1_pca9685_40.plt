:- begin_tests(i2c_dev_1_pca9685_40).
:- use_module(i2c).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

These tests assume that an NXP PCA9685 exists on I2C channel 1 at
7-bit slave address 40 hexadecimal.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

i2c_dev('/dev/i2c-1').

pca9685_addr(16'40).

test(it, Byte == 16'20) :-
    i2c_dev(Dev),
    i2c_open(Dev, I2C),
    pca9685_addr(Addr),
    i2c_slave(I2C, Addr),
    i2c_write(I2C, [16'00]),
    i2c_read(I2C, [Byte]).

:- end_tests(i2c_dev_1_pca9685_40).
