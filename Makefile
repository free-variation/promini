# Makefile for sampler - Prolog interface to miniaudio

# Detect operating system
UNAME_S := $(shell uname -s)

# SWI-Prolog compiler
SWIPL := swipl
SWIPL_LD := swipl-ld

# Compiler flags
CFLAGS := -O3 -ffast-math

# Platform-specific settings
ifeq ($(UNAME_S),Darwin)
    # macOS
    SOEXT := dylib
    LDFLAGS := -framework CoreAudio -framework AudioToolbox
else ifeq ($(UNAME_S),Linux)
    # Linux
    SOEXT := so
    LDFLAGS := -lpthread -lm -ldl
else
    # Windows or other
    SOEXT := dll
    LDFLAGS :=
endif

# Source and output
SRC := src/c/sampler.c src/c/synth.c src/c/effects.c src/c/init.c
HEADERS := src/c/sampler_internal.h
LIBDIR := lib
TARGET := $(LIBDIR)/sampler.$(SOEXT)

# Default target
all: $(TARGET)

# Build shared library
$(TARGET): $(SRC) $(HEADERS) | $(LIBDIR)
	$(SWIPL_LD) -shared -o $(TARGET) $(SRC) $(CFLAGS) $(LDFLAGS)

# Create lib directory
$(LIBDIR):
	mkdir -p $(LIBDIR)

# Clean build artifacts
clean:
	rm -f $(TARGET)
	rm -f *.o
	rmdir $(LIBDIR) 2>/dev/null || true

# Phony targets
.PHONY: all clean
