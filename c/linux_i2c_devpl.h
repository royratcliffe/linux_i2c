#pragma once

#include <SWI-Prolog.h>

/*!
 * \brief An open I2C device.
 */
struct linux_i2c_dev
{ int fd;
};

extern PL_blob_t i2c_dev_blob_type;

/*!
 * \brief Opens an I2C device.
 * Opens the device for read-write access by its path name first. Fails unless
 * opening succeeds.
 */
int unify_i2c_dev(term_t Term, const char *pathname);

int get_i2c_dev(term_t Term, struct linux_i2c_dev **blob_ptr);
