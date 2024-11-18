SOBJ = $(PACKSODIR)/linux_i2c.$(SOEXT)
OBJ = c/linux_i2cpl.o

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
# SWI-Prolog installation on Linux. It goal rebuilds and installs symbolically.
# Check out the `~/.local/share/swi-prolog/pack` folder. Find a symbolic link to
# the source pack.
rebuild:
	swipl -g "pack_install(., [rebuild(true)])" -t halt

installed:
	swipl -g "ignore(pack_list_installed)" -t halt
