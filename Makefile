# Makefile for promini - Prolog interface to miniaudio

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
    SDL3_PREFIX := $(shell brew --prefix sdl3)
    SDL3_TTF_PREFIX := $(shell brew --prefix sdl3_ttf)
    CFLAGS += -I$(SDL3_PREFIX)/include -I$(SDL3_TTF_PREFIX)/include
    LDFLAGS := -L$(SDL3_PREFIX)/lib -L$(SDL3_TTF_PREFIX)/lib -lSDL3 -lSDL3_ttf
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
SRC := src/c/promini.c src/c/synth.c src/c/effects.c src/c/init.c src/c/reverb.c src/c/mod.c src/c/mixer.c src/c/control.c src/c/capture.c src/c/granular.c src/c/clock.c src/c/visualizer.c
HEADERS := src/c/promini.h
LIBDIR := lib
TARGET := $(LIBDIR)/promini.$(SOEXT)

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
