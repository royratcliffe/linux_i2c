:- begin_tests(i2c_dev_1).
:- use_module(i2c).

test(i2c_open) :-
    i2c_open('/dev/i2c-1', _).

test(i2c_funcs,
     Funcs == [ i2c,
                protocol_mangling,
                smbus_pec,
                smbus_quick,
                smbus_read_byte,
                smbus_write_byte,
                bit(19),bit(20),bit(21),bit(22),bit(23),bit(25),bit(26),bit(27)
              ]) :-
    i2c_open('/dev/i2c-1', I2C),
    i2c_funcs(I2C, Funcs).

:- end_tests(i2c_dev_1).
