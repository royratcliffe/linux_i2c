#include <SWI-Prolog.h>
#include <SWI-Stream.h>

/*
 * This is why the pack has the name linux_i2c. I2C meets Prolog on the Linux
 * platform.
 */
#include <linux/i2c.h>
#include <linux/i2c-dev.h>
#include <linux/limits.h>

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include <sys/ioctl.h>

#include "linux_i2c_devpl.h"

/*
 * Defines the maximum size of an I2C transfer. Used to allocate the stack-based
 * read buffer, and validate the number of expected bytes to read. This limits
 * auto-incrementing block reads, or writes, for devices with large register
 * files.
 */
#define I2C_BLOCK_MAX 256

/*!
 * \brief Raises an exception.
 * \details Exceptions have the compound form \c{i2c_errno(Culprit, ErrNo)}
 * where \c Culprit describes what caused the error and \c ErrNo is the standard
 * C library error number, a positive integer.
 */
static int i2c_errno(const char *culprit);

foreign_t i2c_open_2(term_t Dev, term_t I2C)
{ int dev;
  char pathname[PATH_MAX];
  int fd;
  /*
   * Does the atom-to-character convertion need to care about Windows multi-byte
   * characters? No because this pack does not work on Windows. It cannot build
   * on Windows, not without the Linux headers.
   */
  if (!PL_get_integer(Dev, &dev)) PL_fail;
  Ssnprintf(pathname, sizeof(pathname), "/dev/i2c-%d", dev);
  if (0 > (fd = open(pathname, O_RDWR))) return i2c_errno("open");
  return unify_i2c_dev(I2C, fd);
}

/*
 * Makes an assumption about unsigned longs and 64-bit unsigned integers. It
 * assumes that their size matches on the target platform. The C `ioctl`
 * interface expects an unsigned long. The Prolog interface unifies the unsigned
 * long with an unsigned 64-bit integer. It relies on C to coerce the former to
 * the latter.
 */
foreign_t i2c_funcs_dev_to_int_2(term_t I2C, term_t Int)
{ struct linux_i2c_dev *blob;
  unsigned long funcs;
  if (!get_i2c_dev(I2C, &blob)) PL_fail;
  if (0 > ioctl(blob->fd, I2C_FUNCS, &funcs)) return i2c_errno("ioctl");
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
#ifdef I2C_FUNC_SMBUS_READ_BYTE_DATA
    {I2C_FUNC_SMBUS_READ_BYTE_DATA, "smbus_read_byte_data"},
#endif
#ifdef I2C_FUNC_SMBUS_WRITE_BYTE_DATA
    {I2C_FUNC_SMBUS_WRITE_BYTE_DATA, "smbus_write_byte_data"},
#endif
#ifdef I2C_FUNC_SMBUS_READ_WORD_DATA
    {I2C_FUNC_SMBUS_READ_WORD_DATA, "smbus_read_word_data"},
#endif
#ifdef I2C_FUNC_SMBUS_WRITE_WORD_DATA
    {I2C_FUNC_SMBUS_WRITE_WORD_DATA, "smbus_write_word_data"},
#endif
#ifdef I2C_FUNC_SMBUS_PROC_CALL
    {I2C_FUNC_SMBUS_PROC_CALL, "smbus_proc_call"},
#endif
#ifdef I2C_FUNC_SMBUS_READ_BLOCK_DATA
    {I2C_FUNC_SMBUS_READ_BLOCK_DATA, "smbus_read_block_data"},
#endif
#ifdef I2C_FUNC_SMBUS_WRITE_BLOCK_DATA
    {I2C_FUNC_SMBUS_WRITE_BLOCK_DATA, "smbus_write_block_data"},
#endif
#ifdef I2C_FUNC_SMBUS_READ_I2C_BLOCK
    {I2C_FUNC_SMBUS_READ_I2C_BLOCK, "smbus_read_i2c_block"},
#endif
#ifdef I2C_FUNC_SMBUS_WRITE_I2C_BLOCK
    {I2C_FUNC_SMBUS_WRITE_I2C_BLOCK, "smbus_write_i2c_block"},
#endif
#ifdef I2C_FUNC_SMBUS_HOST_NOTIFY
    {I2C_FUNC_SMBUS_HOST_NOTIFY, "smbus_host_notify"},
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
foreign_t i2c_slave_2(term_t I2C, term_t Addr)
{ struct linux_i2c_dev *blob;
  int addr;
  if (!get_i2c_dev(I2C, &blob)) PL_fail;
  if (!PL_get_integer_ex(Addr, &addr)) PL_fail;
  if (0 > ioctl(blob->fd, I2C_SLAVE, addr)) return i2c_errno("ioctl");
  PL_succeed;
}

foreign_t i2c_slave_force_2(term_t I2C, term_t Addr)
{ struct linux_i2c_dev *blob;
  int addr;
  if (!get_i2c_dev(I2C, &blob)) PL_fail;
  if (!PL_get_integer_ex(Addr, &addr)) PL_fail;
  if (0 > ioctl(blob->fd, I2C_SLAVE_FORCE, addr)) return i2c_errno("ioctl");
  PL_succeed;
}

foreign_t i2c_write_3(term_t I2C, term_t Bytes, term_t Actual)
{ struct linux_i2c_dev *blob;
  size_t len;
  char *bytes;
  ssize_t actual;
  if (!get_i2c_dev(I2C, &blob)) return PL_type_error(i2c_dev_blob_type.name, I2C);
  if (!PL_get_list_nchars(Bytes, &len, &bytes, CVT_LIST)) return PL_type_error("list", Bytes);
  if (0 > (actual = write(blob->fd, bytes, len))) return i2c_errno("write");
  return PL_unify_integer(Actual, actual);
}

foreign_t i2c_read_3(term_t I2C, term_t Expected, term_t Bytes)
{ struct linux_i2c_dev *blob;
  size_t expected;
  ssize_t actual;
  if (!get_i2c_dev(I2C, &blob)) return PL_type_error(i2c_dev_blob_type.name, I2C);
  if (!PL_get_size_ex(Expected, &expected) || expected > I2C_BLOCK_MAX) PL_fail;
  /*
   * Use C99 variable-length arrays.
   */
  char bytes[expected];
  if (0 > (actual = read(blob->fd, bytes, expected))) return i2c_errno("read");
  /*
   * Unify as codes, not as characters. The result will appear as integers
   * rather than Unicode character atoms.
   */
  return PL_unify_list_ncodes(Bytes, actual, bytes);
}

int i2c_errno(const char *culprit)
{ term_t Except = PL_new_term_ref();
  if (!PL_unify_term(Except,
                     PL_FUNCTOR_CHARS, "i2c_errno", 2,
                       PL_CHARS, culprit,
                       PL_INT, errno)) PL_fail;
  return PL_raise_exception(Except);
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
