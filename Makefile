# Makefile for sampler - Prolog interface to miniaudio

# Detect operating system
UNAME_S := $(shell uname -s)

# SWI-Prolog compiler
SWIPL := swipl
SWIPL_LD := swipl-ld

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
SRC := src/c/sampler.c
LIBDIR := lib
TARGET := $(LIBDIR)/sampler.$(SOEXT)

# Default target
all: $(TARGET)

# Build shared library
$(TARGET): $(SRC) | $(LIBDIR)
	$(SWIPL_LD) -shared -o $(TARGET) $(SRC) $(LDFLAGS)

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
