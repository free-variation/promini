/*
 * promini.c - Prolog interface to miniaudio
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#include <SWI-Prolog.h>
#include <SWI-Stream.h>

#define MINIAUDIO_IMPLEMENTATION
#include "../../include/miniaudio.h"

/*
 * promini_version(-Version)
 * Unifies Version with the miniaudio version string.
 */
static foreign_t pl_promini_version(term_t version)
{
    return PL_unify_atom_chars(version, MA_VERSION_STRING);
}

/*
 * install()
 * Register foreign predicates with SWI-Prolog.
 */
install_t install(void)
{
    PL_register_foreign("promini_version", 1, pl_promini_version, 0);
}
