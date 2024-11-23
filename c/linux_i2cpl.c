#include <SWI-Prolog.h>
#include <SWI-Stream.h>

/*
 * This is why the pack has the name linux_i2c. I2C meets Prolog on the Linux
 * platform.
 */
#include <linux/i2c.h>
#include <linux/i2c-dev.h>

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/ioctl.h>

#include "linux_i2c_devpl.h"

#define I2C_BLOCK_MAX (I2C_SMBUS_BLOCK_MAX + 2)

foreign_t i2c_open_2(term_t Pathname, term_t Dev)
{ char *pathname;
  if (!PL_get_atom_chars(Pathname, &pathname)) PL_fail;
  int fd;
  if (0 > (fd = open(pathname, O_RDWR))) PL_instantiation_error(Pathname);
  return unify_i2c_dev(Dev, fd);
}

/*!
 * Makes an assumption about unsigned longs and 64-bit unsigned integers. It
 * assumes that their size matches on the target platform. The C `ioctl`
 * interface expects an unsigned long. The Prolog interface unifies the unsigned
 * long with an unsigned 64-bit integer. It relies on C to coerce the former to
 * the latter.
 */
foreign_t i2c_funcs_dev_to_int_2(term_t Dev, term_t Int)
{ struct linux_i2c_dev *blob;
  if (!get_i2c_dev(Dev, &blob)) PL_fail;
  unsigned long funcs;
  if (0 > ioctl(blob->fd, I2C_FUNCS, &funcs)) PL_fail;
  if (!PL_unify_uint64(Int, funcs)) PL_fail;
  PL_succeed;
}

/*
 * Turn a set of bits into a list of atoms. One atom, one bit; it makes a
 * simplifying assumption that all functionality bits in the mask represent
 * presence or absence only. Each item in the resulting list represents a set
 * bit in the integer.
 */
foreign_t i2c_funcs_int_to_list_2(term_t Int, term_t Funcs)
{ uint64_t funcs;
  if (!PL_get_uint64(Int, &funcs)) PL_fail;
  term_t Tail = PL_copy_term_ref(Funcs), Head = PL_new_term_ref();
  static const struct
  { unsigned long func;
    const char *chars;
  } i2c_funcs[] =
  {
#ifdef I2C_FUNC_I2C
    {I2C_FUNC_I2C, "i2c"},
#endif
#ifdef I2C_FUNC_10BIT_ADDR
    /*
     * The following becomes an atom that requires quoting in Prolog because it
     * begins with a digit. It could be reformatted, e.g. `tenbit_addr` or
     * perhaps `bit10_addr`. Nevertheless, ten-bit addressing is less common.
     */
    {I2C_FUNC_10BIT_ADDR, "10bit_addr"},
#endif
#ifdef I2C_FUNC_PROTOCOL_MANGLING
    {I2C_FUNC_PROTOCOL_MANGLING, "protocol_mangling"},
#endif
#ifdef I2C_FUNC_SMBUS_PEC
    {I2C_FUNC_SMBUS_PEC, "smbus_pec"},
#endif
#ifdef I2C_FUNC_NOSTART
    {I2C_FUNC_NOSTART, "nostart"},
#endif
#ifdef I2C_FUNC_SLAVE
    {I2C_FUNC_SLAVE, "slave"},
#endif
#ifdef I2C_FUNC_SMBUS_BLOCK_PROC_CALL
    {I2C_FUNC_SMBUS_BLOCK_PROC_CALL, "smbus_block_proc_call"},
#endif
#ifdef I2C_FUNC_SMBUS_QUICK
    {I2C_FUNC_SMBUS_QUICK, "smbus_quick"},
#endif
#ifdef I2C_FUNC_SMBUS_READ_BYTE
    {I2C_FUNC_SMBUS_READ_BYTE, "smbus_read_byte"},
#endif
#ifdef I2C_FUNC_SMBUS_WRITE_BYTE
    {I2C_FUNC_SMBUS_WRITE_BYTE, "smbus_write_byte"},
#endif
  };
  for (size_t i = 0; i < sizeof(i2c_funcs)/sizeof(i2c_funcs[0]); i++)
    if (funcs & i2c_funcs[i].func)
    { if (!PL_unify_list(Tail, Head, Tail) || !PL_unify_atom_chars(Head, i2c_funcs[i].chars)) PL_fail;
      /*
        * Clear the functionality bit so that it does not appear in the Funcs
        * list subsequently as a bit(Z) term.
        */
      funcs ^= i2c_funcs[i].func;
    }
  /*
   * Add bit(Z) terms for anything else where Z is the number of trailing bits.
   *
   *   for (int z = 0; z < 32; z++)
   *   { if (funcs & (1UL << z))
   *       if (!PL_unify_list(Tail, Head, Tail) || !PL_unify_term(Head, PL_FUNCTOR_CHARS, "bit", 1, PL_INT, z)) PL_fail;
   *   }
   */
  for (int z; (z = __builtin_ctzl(funcs)) < 32; funcs ^= 1UL << z)
    if (!PL_unify_list(Tail, Head, Tail) || !PL_unify_term(Head, PL_FUNCTOR_CHARS, "bit", 1, PL_INT, z)) PL_fail;
  return PL_unify_nil(Tail);
}

/*!
 * Throws an exception if the address is \e not an integer.
 */
foreign_t i2c_slave_2(term_t Dev, term_t Address)
{ struct linux_i2c_dev *blob;
  int address;
  if (!get_i2c_dev(Dev, &blob)) PL_fail;
  if (!PL_get_integer_ex(Address, &address)) PL_fail;
  if (0 > ioctl(blob->fd, I2C_SLAVE, address)) PL_fail;
  PL_succeed;
}

foreign_t i2c_slave_force_2(term_t Dev, term_t Address)
{ struct linux_i2c_dev *blob;
  int address;
  if (!get_i2c_dev(Dev, &blob)) PL_fail;
  if (!PL_get_integer_ex(Address, &address)) PL_fail;
  if (0 > ioctl(blob->fd, I2C_SLAVE_FORCE, address)) PL_fail;
  PL_succeed;
}

foreign_t i2c_write_3(term_t Dev, term_t Bytes, term_t Actual)
{ struct linux_i2c_dev *blob;
  if (!get_i2c_dev(Dev, &blob)) return PL_type_error(i2c_dev_blob_type.name, Dev);
  size_t len;
  char *bytes;
  if (!PL_get_list_nchars(Bytes, &len, &bytes, CVT_LIST)) return PL_type_error("list", Bytes);
  ssize_t actual;
  if (0 > (actual = write(blob->fd, bytes, len))) PL_fail;
  return PL_unify_integer(Actual, actual);
}

foreign_t i2c_read_3(term_t Dev, term_t Expected, term_t Bytes)
{ struct linux_i2c_dev *blob;
  if (!get_i2c_dev(Dev, &blob)) return PL_type_error(i2c_dev_blob_type.name, Dev);
  size_t expected;
  if (!PL_get_size_ex(Expected, &expected) || expected > I2C_BLOCK_MAX) PL_fail;
  char bytes[I2C_BLOCK_MAX];
  ssize_t actual = read(blob->fd, bytes, expected);
  if (0 > actual) PL_fail;
  return PL_unify_list_ncodes(Bytes, actual, bytes);
}

install_t install_linux_i2c()
{ PL_register_foreign("i2c_open", 2, i2c_open_2, 0);
  PL_register_foreign("i2c_funcs_dev_to_int", 2, i2c_funcs_dev_to_int_2, 0);
  PL_register_foreign("i2c_funcs_int_to_list", 2, i2c_funcs_int_to_list_2, 0);
  PL_register_foreign("i2c_slave", 2, i2c_slave_2, 0);
  PL_register_foreign("i2c_slave_force", 2, i2c_slave_force_2, 0);
  PL_register_foreign("i2c_write", 3, i2c_write_3, 0);
  PL_register_foreign("i2c_read", 3, i2c_read_3, 0);
}

install_t uninstall_linux_i2c()
{ ;
}
