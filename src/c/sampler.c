/*
 * sampler.c - Prolog interface to miniaudio (engine API)
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#include <stdlib.h>
#include <SWI-Prolog.h>
#include <SWI-Stream.h>

#define MINIAUDIO_IMPLEMENTATION
#include "../../include/miniaudio.h"

/*
 * Global engine - one engine for the library lifetime
 */
static ma_engine* g_engine = NULL;

/*
 * sampler_version(-Version)
 * Unifies Version with the miniaudio version string.
 */
static foreign_t pl_sampler_version(term_t version)
{
    return PL_unify_atom_chars(version, MA_VERSION_STRING);
}

/*
 * sampler_init
 * Initializes the global miniaudio engine.
 * Returns true if successful or already initialized.
 */
static foreign_t pl_sampler_init(void)
{
    ma_result result;

    /* Already initialized */
    if (g_engine != NULL) {
        return TRUE;
    }

    /* Allocate engine */
    g_engine = (ma_engine*)malloc(sizeof(ma_engine));
    if (g_engine == NULL) {
        return PL_resource_error("memory");
    }

    /* Initialize engine with default config */
    result = ma_engine_init(NULL, g_engine);
    if (result != MA_SUCCESS) {
        free(g_engine);
        g_engine = NULL;
        return FALSE;
    }

    return TRUE;
}

/*
 * sampler_devices(-Devices)
 * Unifies Devices with a list of available audio devices.
 * Each device is represented as device(Name, Type, IsDefault) where:
 *   - Name is the device name string
 *   - Type is 'playback' or 'capture'
 *   - IsDefault is 'true' or 'false'
 */
static foreign_t pl_sampler_devices(term_t devices)
{
    ma_context* pContext;
    ma_result result;
    ma_device_info* pPlaybackInfos;
    ma_uint32 playbackCount;
    ma_device_info* pCaptureInfos;
    ma_uint32 captureCount;
    term_t list = PL_new_term_ref();
    functor_t device_functor;
    ma_uint32 i;

    /* Auto-initialize engine if needed */
    if (g_engine == NULL) {
        if (!pl_sampler_init()) {
            return PL_unify_nil(devices);
        }
    }

    /* Get context from engine */
    pContext = ma_engine_get_device(g_engine)->pContext;

    /* Get device information */
    result = ma_context_get_devices(pContext, &pPlaybackInfos, &playbackCount,
                                     &pCaptureInfos, &captureCount);
    if (result != MA_SUCCESS) {
        return PL_unify_nil(devices);
    }

    /* Create device/3 functor */
    device_functor = PL_new_functor(PL_new_atom("device"), 3);

    /* Build the list (start with nil) */
    PL_put_nil(list);

    /* Add capture devices (reverse order for cons) */
    for (i = captureCount; i > 0; i--) {
        term_t device = PL_new_term_ref();
        term_t args = PL_new_term_refs(3);

        PL_put_atom_chars(args+0, pCaptureInfos[i-1].name);
        PL_put_atom_chars(args+1, "capture");
        PL_put_atom_chars(args+2, pCaptureInfos[i-1].isDefault ? "true" : "false");

        if (!PL_cons_functor_v(device, device_functor, args)) {
            return FALSE;
        }
        if (!PL_cons_list(list, device, list)) {
            return FALSE;
        }
    }

    /* Add playback devices */
    for (i = playbackCount; i > 0; i--) {
        term_t device = PL_new_term_ref();
        term_t args = PL_new_term_refs(3);

        PL_put_atom_chars(args+0, pPlaybackInfos[i-1].name);
        PL_put_atom_chars(args+1, "playback");
        PL_put_atom_chars(args+2, pPlaybackInfos[i-1].isDefault ? "true" : "false");

        if (!PL_cons_functor_v(device, device_functor, args)) {
            return FALSE;
        }
        if (!PL_cons_list(list, device, list)) {
            return FALSE;
        }
    }

    /* Unify with output */
    return PL_unify(devices, list);
}

/*
 * install()
 * Register foreign predicates with SWI-Prolog.
 */
install_t install(void)
{
    PL_register_foreign("sampler_version", 1, pl_sampler_version, 0);
    PL_register_foreign("sampler_init", 0, pl_sampler_init, 0);
    PL_register_foreign("sampler_devices", 1, pl_sampler_devices, 0);
}

/*
 * uninstall_sampler()
 * Called when the foreign library is unloaded.
 * Cleans up the engine.
 */
install_t uninstall_sampler(void)
{
    /* Clean up engine */
    if (g_engine != NULL) {
        ma_engine_uninit(g_engine);
        free(g_engine);
        g_engine = NULL;
    }
}
