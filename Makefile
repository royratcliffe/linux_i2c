SOBJ = $(PACKSODIR)/linux_i2c.$(SOEXT)
OBJ = c/linux_i2cpl.o c/linux_i2c_devpl.o

CFLAGS += -O2 -fomit-frame-pointer

all: $(SOBJ)

$(SOBJ): $(OBJ)
	mkdir -p $(PACKSODIR)
	$(LD) $(LDSOFLAGS) -o $@ $(OBJ) $(SWISOLIB)

check::
install::
clean:
	rm -f $(OBJ)
distclean: clean
	rm -f $(SOBJ)

# Use the rebuild goal when installing directly from the source. This presumes a
# SWI-Prolog installation on Linux. Its goal rebuilds and activates the pack
# symbolically. Check out the `~/.local/share/swi-prolog/pack` folder. Find a
# symbolic link to the source pack.
rebuild:
	swipl -g "pack_install(., [rebuild(true), interactive(false)])" -t halt

installed:
	swipl -g "ignore(pack_list_installed)" -t halt

# Threaded testing on ARM A72 crashes: SWI-Prolog version 9.0.4 stable for
# `aarch64-linux` on Linux Bookworm.
test:
	swipl --threads=false -l prolog/linux/i2c_dev_1.plt -t run_tests
