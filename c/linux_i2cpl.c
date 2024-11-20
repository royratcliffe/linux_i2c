#include <SWI-Prolog.h>
#include <SWI-Stream.h>

/*
 * This is why the pack has the name linux_i2c. I2C meets Prolog on the Linux
 * platform.
 */
#include <linux/i2c.h>
#include <linux/i2c-dev.h>

#include <sys/ioctl.h>

/*!
 * Throws an exception if the address is \e not an integer.
 */
foreign_t i2c_slave_2(term_t Stream, term_t Address)
{ IOSTREAM *stream;
  int address;
  if (!PL_get_stream(Stream, &stream, SIO_OUTPUT)) PL_fail;
  if (!PL_get_integer_ex(Address, &address)) PL_fail;
  if (0 > ioctl(Sfileno(stream), I2C_SLAVE, address)) PL_fail;
  PL_succeed;
}

install_t install_linux_i2c()
{ PL_register_foreign("i2c_slave", 2, i2c_slave_2, 0);
}

install_t uninstall_linux_i2c()
{ ;
}
