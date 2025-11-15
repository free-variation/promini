/*
 * sampler.c - Prolog interface to miniaudio
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#include <stdlib.h>
#include <SWI-Prolog.h>
#include <SWI-Stream.h>

#define MINIAUDIO_IMPLEMENTATION
#include "../../include/miniaudio.h"

/*
 * Global context - one context for the library lifetime
 */
static ma_context* g_context = NULL;

/*
 * Device handle management
 */
#define MAX_DEVICES 32

typedef struct {
    ma_device* device;
    ma_bool32 in_use;
} device_slot_t;

static device_slot_t g_devices[MAX_DEVICES] = {{NULL, MA_FALSE}};

/*
 * allocate_device_slot()
 * Finds a free device slot and marks it as in use.
 * Returns slot index, or -1 if all slots are full.
 */
static int allocate_device_slot(void)
{
    int i;
    for (i = 0; i < MAX_DEVICES; i++) {
        if (!g_devices[i].in_use) {
            g_devices[i].in_use = MA_TRUE;
            g_devices[i].device = NULL;
            return i;
        }
    }
    return -1;
}

/*
 * free_device_slot()
 * Frees a device slot.
 */
static void free_device_slot(int index)
{
    if (index >= 0 && index < MAX_DEVICES) {
        if (g_devices[index].device != NULL) {
            ma_device_uninit(g_devices[index].device);
            free(g_devices[index].device);
            g_devices[index].device = NULL;
        }
        g_devices[index].in_use = MA_FALSE;
    }
}

/*
 * get_device()
 * Validates handle and returns device pointer.
 * Returns NULL if invalid handle.
 */
static ma_device* get_device(int index)
{
    if (index < 0 || index >= MAX_DEVICES) {
        return NULL;
    }
    if (!g_devices[index].in_use) {
        return NULL;
    }
    return g_devices[index].device;
}

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
 * Initializes the global miniaudio context.
 * Returns true if successful or already initialized.
 */
static foreign_t pl_sampler_init(void)
{
    ma_result result;

    /* Already initialized */
    if (g_context != NULL) {
        return TRUE;
    }

    /* Allocate context */
    g_context = (ma_context*)malloc(sizeof(ma_context));
    if (g_context == NULL) {
        return PL_resource_error("memory");
    }

    /* Initialize context with default backends */
    result = ma_context_init(NULL, 0, NULL, g_context);
    if (result != MA_SUCCESS) {
        free(g_context);
        g_context = NULL;
        return FALSE;
    }

    return TRUE;
}

/*
 * Stub callbacks for Phase 1 (will be implemented in Phase 2)
 */
static void capture_callback(ma_device* pDevice, void* pOutput,
                             const void* pInput, ma_uint32 frameCount)
{
    /* Phase 2: will write pInput to recording buffer */
    (void)pDevice;
    (void)pOutput;
    (void)pInput;
    (void)frameCount;
}

static void playback_callback(ma_device* pDevice, void* pOutput,
                              const void* pInput, ma_uint32 frameCount)
{
    /* Phase 2: will read from recording buffer to pOutput */
    /* For now, output silence */
    (void)pDevice;
    (void)pInput;
    ma_uint32 sampleCount = frameCount * pDevice->playback.channels;
    if (pDevice->playback.format == ma_format_f32) {
        float* pOutputF32 = (float*)pOutput;
        ma_uint32 i;
        for (i = 0; i < sampleCount; i++) {
            pOutputF32[i] = 0.0f;
        }
    } else if (pDevice->playback.format == ma_format_s16) {
        ma_int16* pOutputS16 = (ma_int16*)pOutput;
        ma_uint32 i;
        for (i = 0; i < sampleCount; i++) {
            pOutputS16[i] = 0;
        }
    }
}

/*
 * init_device()
 * Helper function to initialize a device with given type and callback.
 * Returns slot index on success, -1 on failure.
 */
static int init_device(ma_device_type deviceType, ma_device_data_proc callback)
{
    ma_device_config config;
    ma_device* device;
    ma_result result;
    int slot;

    /* Auto-initialize context if needed */
    if (g_context == NULL) {
        if (!pl_sampler_init()) {
            return -1;
        }
    }

    /* Allocate device slot */
    slot = allocate_device_slot();
    if (slot < 0) {
        return -1;
    }

    /* Allocate device */
    device = (ma_device*)malloc(sizeof(ma_device));
    if (device == NULL) {
        free_device_slot(slot);
        return -1;
    }

    /* Configure device */
    config = ma_device_config_init(deviceType);
    config.sampleRate = 48000;
    config.dataCallback = callback;

    if (deviceType == ma_device_type_capture) {
        config.capture.format = ma_format_f32;
        config.capture.channels = 2;
    } else {
        config.playback.format = ma_format_f32;
        config.playback.channels = 2;
    }

    /* Initialize device */
    result = ma_device_init(g_context, &config, device);
    if (result != MA_SUCCESS) {
        free(device);
        free_device_slot(slot);
        return -1;
    }

    /* Store device */
    g_devices[slot].device = device;
    return slot;
}

/*
 * sampler_device_init_capture(-Handle)
 * Initializes a capture device and returns a handle.
 */
static foreign_t pl_sampler_device_init_capture(term_t handle)
{
    int slot = init_device(ma_device_type_capture, capture_callback);
    if (slot < 0) {
        return PL_resource_error("device");
    }
    return PL_unify_integer(handle, slot);
}

/*
 * sampler_device_init_playback(-Handle)
 * Initializes a playback device and returns a handle.
 */
static foreign_t pl_sampler_device_init_playback(term_t handle)
{
    int slot = init_device(ma_device_type_playback, playback_callback);
    if (slot < 0) {
        return PL_resource_error("device");
    }
    return PL_unify_integer(handle, slot);
}

/*
 * sampler_device_start(+Handle)
 * Starts a device.
 */
static foreign_t pl_sampler_device_start(term_t handle)
{
    int slot;
    ma_device* device;
    ma_result result;

    /* Get handle */
    if (!PL_get_integer(handle, &slot)) {
        return PL_type_error("integer", handle);
    }

    /* Validate and get device */
    device = get_device(slot);
    if (device == NULL) {
        return PL_existence_error("device", handle);
    }

    /* Start device */
    result = ma_device_start(device);
    return (result == MA_SUCCESS) ? TRUE : FALSE;
}

/*
 * sampler_device_stop(+Handle)
 * Stops a device.
 */
static foreign_t pl_sampler_device_stop(term_t handle)
{
    int slot;
    ma_device* device;
    ma_result result;

    /* Get handle */
    if (!PL_get_integer(handle, &slot)) {
        return PL_type_error("integer", handle);
    }

    /* Validate and get device */
    device = get_device(slot);
    if (device == NULL) {
        return PL_existence_error("device", handle);
    }

    /* Stop device */
    result = ma_device_stop(device);
    return (result == MA_SUCCESS) ? TRUE : FALSE;
}

/*
 * sampler_device_uninit(+Handle)
 * Uninitializes and frees a device.
 */
static foreign_t pl_sampler_device_uninit(term_t handle)
{
    int slot;

    /* Get handle */
    if (!PL_get_integer(handle, &slot)) {
        return PL_type_error("integer", handle);
    }

    /* Validate handle */
    if (slot < 0 || slot >= MAX_DEVICES || !g_devices[slot].in_use) {
        return PL_existence_error("device", handle);
    }

    /* Free device slot (stops and uninits device) */
    free_device_slot(slot);
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
    ma_result result;
    ma_device_info* pPlaybackInfos;
    ma_uint32 playbackCount;
    ma_device_info* pCaptureInfos;
    ma_uint32 captureCount;
    term_t list = PL_new_term_ref();
    functor_t device_functor;
    ma_uint32 i;

    /* Auto-initialize context if needed */
    if (g_context == NULL) {
        if (!pl_sampler_init()) {
            return PL_unify_nil(devices);
        }
    }

    /* Get device information */
    result = ma_context_get_devices(g_context, &pPlaybackInfos, &playbackCount,
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
    PL_register_foreign("sampler_device_init_capture", 1, pl_sampler_device_init_capture, 0);
    PL_register_foreign("sampler_device_init_playback", 1, pl_sampler_device_init_playback, 0);
    PL_register_foreign("sampler_device_start", 1, pl_sampler_device_start, 0);
    PL_register_foreign("sampler_device_stop", 1, pl_sampler_device_stop, 0);
    PL_register_foreign("sampler_device_uninit", 1, pl_sampler_device_uninit, 0);
}

/*
 * uninstall_sampler()
 * Called when the foreign library is unloaded.
 * Cleans up all devices and the context.
 */
install_t uninstall_sampler(void)
{
    int i;

    /* Clean up all device slots */
    for (i = 0; i < MAX_DEVICES; i++) {
        if (g_devices[i].in_use) {
            free_device_slot(i);
        }
    }

    /* Clean up context */
    if (g_context != NULL) {
        ma_context_uninit(g_context);
        free(g_context);
        g_context = NULL;
    }
}
