:- begin_tests(i2c_dev_1).
:- use_module(i2c).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

These tests make some assumptions. They presume that an I2C device
exists at /dev/i2c-1.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

i2c_dev('/dev/i2c-1').

test(i2c_open) :-
    i2c_dev(Dev),
    i2c_open(Dev, _).

test(i2c_funcs,
     Funcs == [ i2c,
                protocol_mangling,
                smbus_pec,
                smbus_quick,
                smbus_read_byte,
                smbus_write_byte,
                smbus_read_byte_data,
                smbus_write_byte_data,
                smbus_read_word_data,
                smbus_write_word_data,
                smbus_proc_call,
                smbus_write_block_data,
                smbus_read_i2c_block,
                smbus_write_i2c_block
              ]) :-
    i2c_dev(Dev),
    i2c_open(Dev, I2C),
    i2c_funcs(I2C, Funcs).

:- end_tests(i2c_dev_1).
