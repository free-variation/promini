/*
 * init.c - Entry point for promini shared library
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#include "promini.h"


/*
 * install()
 * Main entry point called by SWI-Prolog when loading the shared library.
 * Registers predicates from all modules.
 */
install_t install(void)
{
    promini_register_predicates();
    synth_register_predicates();
    effects_register_predicates();
    mod_register_predicates();
}

/*
 * uninstall()
 * Called by SWI-Prolog when unloading the shared library.
 * Cleanus up resources from all modules.
 */
install_t uninstall(void)
{
	uninstall_mod();
	uninstall_synth();
	uninstall_effects();
	uninstall_promini();
}
