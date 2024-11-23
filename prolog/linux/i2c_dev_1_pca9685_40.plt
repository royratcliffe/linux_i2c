:- begin_tests(i2c_dev_1_pca9685_40).
:- use_module(i2c).

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
