/*
 * image.c - Image loading for promini
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#define STB_IMAGE_IMPLEMENTATION
#include "../../include/stb_image.h"
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "../../include/stb_image_write.h"
#include "promini.h"

#if defined(__ARM_NEON) || defined(__aarch64__)
#include <arm_neon.h>
#endif

#define GET_IMAGE_FROM_SLOT(slot, img_var) \
	do { \
		if (slot < 0 || slot >= MAX_IMAGES || !g_images[slot].in_use) { \
			return -1; \
		} \
		img_var = &g_images[slot]; \
	} while(0)

/******************************************************************************
 * GLOBAL VARIABLES
 *****************************************************************************/

image_slot_t g_images[MAX_IMAGES] = {{0}};
pthread_mutex_t g_images_mutex = PTHREAD_MUTEX_INITIALIZER;

/******************************************************************************
 * SLOT MANAGEMENT
 *****************************************************************************/

/*
 * allocate_image_slot()
 * Finds a free image slot and marks it as in use.
 * Returns slot index, or -1 if all slots are full.
 */
static int allocate_image_slot(void)
{
	int i;

	pthread_mutex_lock(&g_images_mutex);
	for (i = 0; i < MAX_IMAGES; i++) {
		if (!g_images[i].in_use) {
			g_images[i].in_use = MA_TRUE;
			g_images[i].pixels = NULL;
			g_images[i].width = 0;
			g_images[i].height = 0;
			pthread_mutex_unlock(&g_images_mutex);
			return i;
		}
	}
	pthread_mutex_unlock(&g_images_mutex);
	return -1;
}

/*
 * free_image_slot()
 * Frees an image slot and its pixel data.
 */
static void free_image_slot(int index);

/*
 * load_image()
 * Loads an image file into a slot.
 * Returns slot index, or -1 on failure.
 */
static int load_image(const char* filename)
{
	int slot;
	int width, height, channels;
	int num_bytes;
	unsigned char* pixels;
	unsigned char* buffer;

	slot = allocate_image_slot();
	if (slot < 0) {
		return -1;
	}

	pixels = stbi_load(filename, &width, &height, &channels, 0);
	if (pixels == NULL) {
		free_image_slot(slot);
		return -1;
	}

	num_bytes = width * height * channels;
	buffer = (unsigned char*)malloc(num_bytes);
	if (buffer == NULL) {
		stbi_image_free(pixels);
		free_image_slot(slot);
		return -1;
	}
	memcpy(buffer, pixels, num_bytes);

	g_images[slot].pixels = pixels;
	g_images[slot].buffer = buffer;
	g_images[slot].width = width;
	g_images[slot].height = height;
	g_images[slot].buf_width = width;
	g_images[slot].buf_height = height;
	g_images[slot].channels = channels;

	return slot;
}

/*
 * image_write_png()
 * Writes an image to tmp/images/<name>.png
 * Returns 0 on success, -1 on failure.
 */
static int image_write_png(int slot, const char* name)
{
	image_slot_t* img;
	char path[256];
	int result;

	GET_IMAGE_FROM_SLOT(slot, img);

	snprintf(path, sizeof(path), "tmp/images/%s.png", name);

	if (img->buf_width == img->width && img->buf_height == img->height) {
		/* no expansion needed */
		result = stbi_write_png(path, img->buf_width, img->buf_height,
		                        img->channels, img->buffer,
		                        img->buf_width * img->channels);
	} else {
		/* expand buffer to original size */
		int x, y, c;
		int block_w = (img->width + img->buf_width - 1) / img->buf_width;
		int block_h = (img->height + img->buf_height - 1) / img->buf_height;
		unsigned char* expanded;

		expanded = (unsigned char*)malloc(img->width * img->height * img->channels);
		if (expanded == NULL) {
			return -1;
		}

		for (y = 0; y < img->height; y++) {
			int buf_y = y / block_h;
			for (x = 0; x < img->width; x++) {
				int buf_x = x / block_w;
				for (c = 0; c < img->channels; c++) {
					int src_idx = (buf_y * img->buf_width + buf_x) * img->channels + c;
					int dst_idx = (y * img->width + x) * img->channels + c;
					expanded[dst_idx] = img->buffer[src_idx];
				}
			}
		}

		result = stbi_write_png(path, img->width, img->height, img->channels,
		                        expanded, img->width * img->channels);
		free(expanded);
	}

	return result ? 0 : -1;
}

/*
 * free_image_slot()
 * Frees an image slot and its pixel data.
 */
static void free_image_slot(int index)
{
	if (index >= 0 && index < MAX_IMAGES) {
		pthread_mutex_lock(&g_images_mutex);
		if (g_images[index].in_use) {
			if (g_images[index].pixels != NULL) {
				stbi_image_free(g_images[index].pixels);
				g_images[index].pixels = NULL;
			}
			if (g_images[index].buffer != NULL) {
				free(g_images[index].buffer);
				g_images[index].buffer = NULL;
			}
			g_images[index].width = 0;
			g_images[index].height = 0;
			g_images[index].buf_width = 0;
			g_images[index].buf_height = 0;
			g_images[index].in_use = MA_FALSE;
		}
		pthread_mutex_unlock(&g_images_mutex);
	}
}

/*
 * uninstall_image()
 * Cleans up all image resources.
 */
install_t uninstall_image(void)
{
	int i;

	for (i = 0; i < MAX_IMAGES; i++) {
		if (g_images[i].in_use) {
			free_image_slot(i);
		}
	}
}

/******************************************************************************
 * IMAGE MANIPULATORS
 *****************************************************************************/

/* image_to_grayscale()
 * Converts an image to grayscale in place (destructively)
 * Returns 0 on success, -1 on failure
 */
static int image_to_grayscale(int slot)
{
	image_slot_t* img;
	int num_pixels, i;
	unsigned char *dest;

	GET_IMAGE_FROM_SLOT(slot, img);

	if (img->channels == 1) {
		return 0;
	}

	num_pixels = img->width * img->height;

	dest = (unsigned char*)malloc(num_pixels);
	if (dest == NULL) {
		return -1;
	}

	if (img->channels == 2) {
		/* grayscale + alpha: take grayscale, ignore alpha */
		for (i = 0; i < num_pixels; i++) {
			dest[i] = img->pixels[i * 2];
		}
	} else if (img->channels == 3) {
#if defined(__ARM_NEON) || defined(__aarch64__)
		/* RGB: NEON accelerated ITU-R BT.601 luminance */
		int simd_pixels = num_pixels & ~7;
		uint8x8_t coeff_r = vdup_n_u8(77);
		uint8x8_t coeff_g = vdup_n_u8(150);
		uint8x8_t coeff_b = vdup_n_u8(29);

		for (i = 0; i < simd_pixels; i += 8) {
			uint8x8x3_t rgb = vld3_u8(img->pixels + i * 3);
			uint16x8_t sum = vmull_u8(rgb.val[0], coeff_r);
			sum = vmlal_u8(sum, rgb.val[1], coeff_g);
			sum = vmlal_u8(sum, rgb.val[2], coeff_b);
			vst1_u8(dest + i, vshrn_n_u16(sum, 8));
		}
		for (; i < num_pixels; i++) {
			dest[i] = (unsigned char)(0.299f * img->pixels[i * 3] +
			                          0.587f * img->pixels[i * 3 + 1] +
			                          0.114f * img->pixels[i * 3 + 2]);
		}
#else
		/* RGB: ITU-R BT.601 luminance */
		for (i = 0; i < num_pixels; i++) {
			dest[i] = (unsigned char)(0.299f * img->pixels[i * 3] +
			                          0.587f * img->pixels[i * 3 + 1] +
			                          0.114f * img->pixels[i * 3 + 2]);
		}
#endif
	} else {
		/* RGBA: ITU-R BT.601 luminance, ignore alpha */
		for (i = 0; i < num_pixels; i++) {
			dest[i] = (unsigned char)(0.299f * img->pixels[i * 4] +
			                          0.587f * img->pixels[i * 4 + 1] +
			                          0.114f * img->pixels[i * 4 + 2]);
		}
	}

	stbi_image_free(img->pixels);
	img->pixels = dest;
	img->channels = 1;

	/* reallocate buffer for grayscale */
	free(img->buffer);
	img->buffer = (unsigned char*)malloc(num_pixels);
	if (img->buffer == NULL) {
		return -1;
	}
	memcpy(img->buffer, img->pixels, num_pixels);
	img->buf_width = img->width;
	img->buf_height = img->height;

	return 0;
}

/*
 * image_reset()
 * Copies pixels back to buffer, restoring original dimensions.
 * Returns 0 on success, -1 on failure.
 */
static int image_reset(int slot)
{
	image_slot_t* img;
	int num_bytes;
	unsigned char* new_buf;

	GET_IMAGE_FROM_SLOT(slot, img);

	num_bytes = img->width * img->height * img->channels;
	new_buf = (unsigned char*)malloc(num_bytes);
	if (new_buf == NULL) {
		return -1;
	}

	memcpy(new_buf, img->pixels, num_bytes);
	free(img->buffer);
	img->buffer = new_buf;
	img->buf_width = img->width;
	img->buf_height = img->height;

	return 0;
}

/*
 * image_quantize()
 * Reduces bit depth of buffer pixels.
 * Returns 0 on success, -1 on failure.
 */
static int image_quantize(int slot, int bits)
{
	image_slot_t* img;
	int i, num_bytes;
	int levels, shift;

	GET_IMAGE_FROM_SLOT(slot, img);

	if (bits < 1 || bits > 8) {
		return -1;
	}

	num_bytes = img->buf_width * img->buf_height * img->channels;
	shift = 8 - bits;
	levels = (1 << bits) - 1;

	for (i = 0; i < num_bytes; i++) {
		int q = img->buffer[i] >> shift;
		img->buffer[i] = (unsigned char)(q * 255 / levels);
	}

	return 0;
}

/*
 * image_downsample()
 * Downsamples buffer by averaging BlockW x BlockH regions.
 * Edge blocks average fewer pixels if dimensions aren't divisible.
 * Buffer shrinks, original pixels unchanged.
 * Returns 0 on success, -1 on failure.
 */
static int image_downsample(int slot, int block_w, int block_h)
{
	image_slot_t* img;
	int new_w, new_h;
	int x, y, bx, by, c;
	int sum, count;
	unsigned char* new_buf;

	GET_IMAGE_FROM_SLOT(slot, img);

	/* round up to include partial edge blocks */
	new_w = (img->buf_width + block_w - 1) / block_w;
	new_h = (img->buf_height + block_h - 1) / block_h;

	if (new_w < 1 || new_h < 1) {
		return -1;
	}

	new_buf = (unsigned char*)malloc(new_w * new_h * img->channels);
	if (new_buf == NULL) {
		return -1;
	}

	for (y = 0; y < new_h; y++) {
		for (x = 0; x < new_w; x++) {
			for (c = 0; c < img->channels; c++) {
				sum = 0;
				count = 0;
				for (by = 0; by < block_h; by++) {
					int src_y = y * block_h + by;
					if (src_y >= img->buf_height) break;
					for (bx = 0; bx < block_w; bx++) {
						int src_x = x * block_w + bx;
						if (src_x >= img->buf_width) break;
						sum += img->buffer[(src_y * img->buf_width + src_x) * img->channels + c];
						count++;
					}
				}
				new_buf[(y * new_w + x) * img->channels + c] = (unsigned char)(sum / count);
			}
		}
	}

	free(img->buffer);
	img->buffer = new_buf;
	img->buf_width = new_w;
	img->buf_height = new_h;

	return 0;
}

/******************************************************************************
 * PROLOG PREDICATES
 *****************************************************************************/

/*
 * pl_image_load()
 * image_load(+Filename, -Image)
 * Loads an image file. Returns image(Handle).
 */
static foreign_t pl_image_load(term_t filename_term, term_t image_term)
{
	char* filename;
	int slot;
	term_t handle;
	functor_t image_functor;

	if (!PL_get_file_name(filename_term, &filename, PL_FILE_READ)) {
		return PL_type_error("filename", filename_term);
	}

	slot = load_image(filename);
	if (slot < 0) {
		return FALSE;
	}

	image_functor = PL_new_functor(PL_new_atom("image"), 1);
	handle = PL_new_term_ref();
	if (!PL_put_integer(handle, slot)) {
		free_image_slot(slot);
		return FALSE;
	}

	return PL_unify_term(image_term, PL_FUNCTOR, image_functor, PL_TERM, handle);
}

/*
 * pl_image_properties()
 * image_properties(+Image, -Width, -Height, -Channels)
 * Gets the dimensions and channel count of an image.
 */
static foreign_t pl_image_properties(term_t image_term, term_t width_term,
                                     term_t height_term, term_t channels_term)
{
	term_t handle;
	int slot;

	handle = PL_new_term_ref();
	if (!PL_get_arg(1, image_term, handle) || !PL_get_integer(handle, &slot)) {
		return PL_type_error("image", image_term);
	}

	if (slot < 0 || slot >= MAX_IMAGES || !g_images[slot].in_use) {
		return PL_existence_error("image", image_term);
	}

	return PL_unify_integer(width_term, g_images[slot].width) &&
	       PL_unify_integer(height_term, g_images[slot].height) &&
	       PL_unify_integer(channels_term, g_images[slot].channels);
}

/*
 * pl_image_buffer_properties()
 * image_buffer_properties(+Image, -BufWidth, -BufHeight)
 * Gets the buffer dimensions of an image.
 */
static foreign_t pl_image_buffer_properties(term_t image_term, term_t buf_width_term,
                                            term_t buf_height_term)
{
	term_t handle;
	int slot;

	handle = PL_new_term_ref();
	if (!PL_get_arg(1, image_term, handle) || !PL_get_integer(handle, &slot)) {
		return PL_type_error("image", image_term);
	}

	if (slot < 0 || slot >= MAX_IMAGES || !g_images[slot].in_use) {
		return PL_existence_error("image", image_term);
	}

	return PL_unify_integer(buf_width_term, g_images[slot].buf_width) &&
	       PL_unify_integer(buf_height_term, g_images[slot].buf_height);
}

/*
 * pl_image_unload()
 * image_unload(+Image)
 * Unloads an image and frees its memory.
 */
static foreign_t pl_image_unload(term_t image_term)
{
	term_t handle;
	int slot;

	handle = PL_new_term_ref();
	if (!PL_get_arg(1, image_term, handle) || !PL_get_integer(handle, &slot)) {
		return PL_type_error("image", image_term);
	}

	if (slot < 0 || slot >= MAX_IMAGES || !g_images[slot].in_use) {
		return PL_existence_error("image", image_term);
	}

	free_image_slot(slot);
	return TRUE;
}

/*
 * pl_image_to_grayscale()
 * image_to_grayscale(+Image)
 * Converts an image to grayscale in place.
 */
static foreign_t pl_image_to_grayscale(term_t image_term)
{
	term_t handle;
	int slot;

	handle = PL_new_term_ref();
	if (!PL_get_arg(1, image_term, handle) || !PL_get_integer(handle, &slot)) {
		return PL_type_error("image", image_term);
	}

	if (image_to_grayscale(slot) < 0) {
		return FALSE;
	}

	return TRUE;
}

/*
 * pl_image_downsample()
 * image_downsample(+Image, +BlockW, +BlockH)
 * Downsamples buffer by averaging BlockW x BlockH regions.
 */
static foreign_t pl_image_downsample(term_t image_term, term_t block_w_term, term_t block_h_term)
{
	term_t handle;
	int slot;
	int block_w, block_h;

	handle = PL_new_term_ref();
	if (!PL_get_arg(1, image_term, handle) || !PL_get_integer(handle, &slot)) {
		return PL_type_error("image", image_term);
	}

	if (!PL_get_integer(block_w_term, &block_w) || block_w < 1) {
		return PL_type_error("positive_integer", block_w_term);
	}

	if (!PL_get_integer(block_h_term, &block_h) || block_h < 1) {
		return PL_type_error("positive_integer", block_h_term);
	}

	if (image_downsample(slot, block_w, block_h) < 0) {
		return FALSE;
	}

	return TRUE;
}

/*
 * pl_image_reset()
 * image_reset(+Image)
 * Copies pixels back to buffer, restoring original dimensions.
 */
static foreign_t pl_image_reset(term_t image_term)
{
	term_t handle;
	int slot;

	handle = PL_new_term_ref();
	if (!PL_get_arg(1, image_term, handle) || !PL_get_integer(handle, &slot)) {
		return PL_type_error("image", image_term);
	}

	if (image_reset(slot) < 0) {
		return FALSE;
	}

	return TRUE;
}

/*
 * pl_image_quantize()
 * image_quantize(+Image, +Bits)
 * Reduces bit depth of buffer pixels.
 */
static foreign_t pl_image_quantize(term_t image_term, term_t bits_term)
{
	term_t handle;
	int slot;
	int bits;

	handle = PL_new_term_ref();
	if (!PL_get_arg(1, image_term, handle) || !PL_get_integer(handle, &slot)) {
		return PL_type_error("image", image_term);
	}

	if (!PL_get_integer(bits_term, &bits) || bits < 1 || bits > 8) {
		return PL_type_error("integer_1_to_8", bits_term);
	}

	if (image_quantize(slot, bits) < 0) {
		return FALSE;
	}

	return TRUE;
}

/*
 * pl_image_write_png()
 * image_write_png(+Image, +Name)
 * Writes an image to tmp/images/<Name>.png
 */
static foreign_t pl_image_write_png(term_t image_term, term_t name_term)
{
	term_t handle;
	int slot;
	char* name;

	handle = PL_new_term_ref();
	if (!PL_get_arg(1, image_term, handle) || !PL_get_integer(handle, &slot)) {
		return PL_type_error("image", image_term);
	}

	if (!PL_get_atom_chars(name_term, &name)) {
		return PL_type_error("atom", name_term);
	}

	if (image_write_png(slot, name) < 0) {
		return FALSE;
	}

	return TRUE;
}

/******************************************************************************
 * PROLOG INTERFACE
 *****************************************************************************/

/*
 * image_register_predicates()
 * Register image foreign predicates with SWI-Prolog.
 */
install_t image_register_predicates(void)
{
	PL_register_foreign("image_load", 2, pl_image_load, 0);
	PL_register_foreign("image_properties", 4, pl_image_properties, 0);
	PL_register_foreign("image_unload", 1, pl_image_unload, 0);
	PL_register_foreign("image_write_png", 2, pl_image_write_png, 0);
	PL_register_foreign("image_to_grayscale", 1, pl_image_to_grayscale, 0);
	PL_register_foreign("image_downsample", 3, pl_image_downsample, 0);
	PL_register_foreign("image_quantize", 2, pl_image_quantize, 0);
	PL_register_foreign("image_reset", 1, pl_image_reset, 0);
	PL_register_foreign("image_buffer_properties", 3, pl_image_buffer_properties, 0);
}
