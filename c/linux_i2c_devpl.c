#include "linux_i2c_devpl.h"

#include <SWI-Stream.h>

#include <unistd.h>
#include <fcntl.h>
#include <memory.h>

#include <linux/i2c.h>
#include <linux/i2c-dev.h>

/*!
 * \brief Closes an open I2C device.
 * Closes the device's file descriptor. Closing can fail. Releasing fails if
 * closing fails.
 */
static int release_i2c_dev(atom_t Atom);

static int write_i2c_dev(IOSTREAM *stream, atom_t Atom, int flags);

PL_blob_t i2c_dev_blob_type =
{ .magic = PL_BLOB_MAGIC,
  .name = "i2c_dev",
  .release = release_i2c_dev,
  .write = write_i2c_dev,
};

int unify_i2c_dev(term_t Term, const char *pathname)
{ int fd;
  if ((fd = open(pathname, O_RDWR)) < 0) PL_fail;
  struct linux_i2c_dev *blob = PL_malloc(sizeof(*blob));
  (void)memset(blob, 0, sizeof(*blob));
  blob->fd = fd;
  return PL_unify_blob(Term, blob, sizeof(*blob), &i2c_dev_blob_type);
}

int get_i2c_dev(term_t Term, struct linux_i2c_dev **blob_ptr)
{ void *blob;
  PL_blob_t *blob_type;
  if (!PL_get_blob(Term, &blob, NULL, &blob_type) || blob_type != &i2c_dev_blob_type)
    return PL_type_error("i2c_dev", Term);
  *blob_ptr = blob;
  PL_succeed;
}

int release_i2c_dev(atom_t Atom)
{ struct linux_i2c_dev *blob = PL_blob_data(Atom, NULL, NULL);
  if (0 > close(blob->fd)) PL_fail;
  PL_free(blob);
  PL_succeed;
}

int write_i2c_dev(IOSTREAM *stream, atom_t Atom, int flags)
{ struct linux_i2c_dev *blob = PL_blob_data(Atom, NULL, NULL);
  (void)Sfprintf(stream, "<%s>(%p, fd=%d)", i2c_dev_blob_type.name, blob, blob->fd);
  PL_succeed;
}
