/*!
 * \file linux_i2c_devpl.c
 * \brief Linux I2C device for Prolog.
 * \copyright 2024, Roy Ratcliffe, Northumberland, United Kingdom
 *
 * SPDX-License-Identifier: MIT
 *
 * Permission is hereby granted, free of charge,  to any person obtaining a
 * copy  of  this  software  and    associated   documentation  files  (the
 * "Software"), to deal in  the   Software  without  restriction, including
 * without limitation the rights to  use,   copy,  modify,  merge, publish,
 * distribute, sublicense, and/or sell  copies  of   the  Software,  and to
 * permit persons to whom the Software is   furnished  to do so, subject to
 * the following conditions:
 *
 *     The above copyright notice and this permission notice shall be
 *     included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT  WARRANTY OF ANY KIND, EXPRESS
 * OR  IMPLIED,  INCLUDING  BUT  NOT   LIMITED    TO   THE   WARRANTIES  OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR   PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS  OR   COPYRIGHT  HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY,  WHETHER   IN  AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM,  OUT  OF   OR  IN  CONNECTION  WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include "linux_i2c_devpl.h"

#include <SWI-Stream.h>

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

int unify_i2c_dev(term_t Term, int fd)
{ struct linux_i2c_dev *blob = PL_malloc(sizeof(*blob));
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
