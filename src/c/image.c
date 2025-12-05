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

image_synth_node_t g_image_synths[MAX_IMAGE_SYNTHS] = {{0}};
pthread_mutex_t g_image_synths_mutex = PTHREAD_MUTEX_INITIALIZER;

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
 * allocate_image_synth_slot()
 * Find and allocate a free image synth slot.
 */
static int allocate_image_synth_slot(void)
{
	int i;

	pthread_mutex_lock(&g_image_synths_mutex);
	for (i = 0; i < MAX_IMAGE_SYNTHS; i++) {
		if (!g_image_synths[i].in_use) {
			g_image_synths[i].in_use = MA_TRUE;
			pthread_mutex_unlock(&g_image_synths_mutex);
			return i;
		}
	}

	pthread_mutex_unlock(&g_image_synths_mutex);
	return -1;
}

/*
 * free_image_synth_slot()
 * Uninitialize and free an image synth slot.
 */
static void free_image_synth_slot(int index)
{
	if (index >= 0 && index < MAX_IMAGE_SYNTHS) {
		pthread_mutex_lock(&g_image_synths_mutex);

		image_synth_node_t* synth = &g_image_synths[index];
		if (synth->in_use) {
			free_effect_chain(synth->effect_chain);
			synth->effect_chain = NULL;
			ma_node_uninit(&synth->base, NULL);
			synth->in_use = MA_FALSE;
		}

		pthread_mutex_unlock(&g_image_synths_mutex);
	}
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

/*
 * image_transpose()
 * Rotates an image 90 dagrees. Direction: 1 = clockwise, -1 = counter-clockwise.
 * Returns 0 on success, -1 on failure.
 */
static int image_transpose(int slot, int direction)
{
	image_slot_t* img;
	unsigned char* new_buf;
	int new_w, new_h;
	int x, y, c;

	GET_IMAGE_FROM_SLOT(slot, img);

	new_w = img->buf_height;
	new_h = img->buf_width;

	new_buf = (unsigned char*)malloc(new_w * new_h * img->channels);
	if (new_buf == NULL) {
		return -1;
	}

	for (y = 0; y < new_h; y++) {
  		for (x = 0; x < new_w; x++) {
  			int src_x, src_y;
  			if (direction > 0) {
  				/* clockwise */
  				src_x = y;
  				src_y = img->buf_height - 1 - x;
  			} else {
  				/* counter-clockwise */
  				src_x = img->buf_width - 1 - y;
  				src_y = x;
  			}
  			for (c = 0; c < img->channels; c++) {
  				new_buf[(y * new_w + x) * img->channels + c] =
  					img->buffer[(src_y * img->buf_width + src_x) * img->channels + c];
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

/*
 * pl_image_transpose()
 * image_transpose(+Image, +Direction)
 * Rotates the image buffer 90 degrees. Direction is cw or ccw.
 */
static foreign_t pl_image_transpose(term_t image_term, term_t dir_term)
{
	term_t handle;
	int slot;
	char* dir_str;
	int direction;

	handle = PL_new_term_ref();
	if (!PL_get_arg(1, image_term, handle) || !PL_get_integer(handle, &slot)) {
		return PL_type_error("image", image_term);
	}

	if (!PL_get_atom_chars(dir_term, &dir_str)) {
		return PL_type_error("atom", dir_term);
	}

	if (strcmp(dir_str, "cw") == 0) {
		direction = 1;
	} else if (strcmp(dir_str, "ccw") == 0) {
		direction = -1;
	} else {
		return PL_domain_error("cw_or_ccw", dir_term);
	}

	if (image_transpose(slot, direction) < 0) {
		return FALSE;
	}

	return TRUE;
}

/******************************************************************************
 * IMAGE SYNTH NODE
 *****************************************************************************/

#define CLAMP(val, min, max) ((val) < (min) ? (min) : ((val) > (max) ? (max) : (val)))

/*
 * cubic_interpolate_row()
 * Get cubic-interpolated amplitude for a row at fractional column position
 */
static float cubic_interpolate_row(image_slot_t* img, int channel, int row, float x)
{
	int x0, x1, x2, x3;
	float t;
	float y0, y1, y2, y3;
	float a, b, c, d;

	x1 = (int)x;
	x0 = x1 - 1;
	x2 = x1 + 1;
	x3 = x1 + 2;
	t = x - x1;

	x0 = CLAMP(x0, 0, img->buf_width - 1);
	x1 = CLAMP(x1, 0, img->buf_width - 1);
	x2 = CLAMP(x2, 0, img->buf_width - 1);
	x3 = CLAMP(x3, 0, img->buf_width - 1);

	y0 = img->buffer[(row * img->buf_width + x0) * img->channels + channel] / 255.0f;
	y1 = img->buffer[(row * img->buf_width + x1) * img->channels + channel] / 255.0f;
	y2 = img->buffer[(row * img->buf_width + x2) * img->channels + channel] / 255.0f;
	y3 = img->buffer[(row * img->buf_width + x3) * img->channels + channel] / 255.0f;
	
	/* Catmull-Rom cubic interpolation */
	a = -0.5f*y0 + 1.5f*y1 - 1.5f*y2 + 0.5f*y3;
  	b = y0 - 2.5f*y1 + 2.0f*y2 - 0.5f*y3;
  	c = -0.5f*y0 + 0.5f*y2;
  	d = y1;

	return a*t*t*t + b*t*t + c*t + d;
}



/*
 * image_synth_process_pcm_frames()
 * Generate audio by scanning image and modulating oscillator volumes.
 */
static void image_synth_process_pcm_frames(
		ma_node* node,
		const float** frames_in,
		ma_uint32* frame_count_in,
		float** frames_out,
		ma_uint32* frame_count_out)
{
	image_synth_node_t* synth;
	image_slot_t* img;
	float* output;
	ma_uint32 channels;
	ma_uint32 frame_count;
	ma_uint32 sample_rate;
	float beats_per_sample;
	float columns_per_sample;
	ma_uint32 frame;
	ma_uint32 ch;
	int row;
	float sample;
	float amp;

	(void)frames_in;
	(void)frame_count_in;

	synth = (image_synth_node_t*)node;
	output = frames_out[0];
	channels = ma_node_get_output_channels(node, 0);
	frame_count = *frame_count_out;

	/* output silence if not playing */
	if (!synth->playing) {
		memset(output, 0, frame_count * channels * sizeof(float));
		return;
	}

	img = &g_images[synth->image_slot];
	if (!img->in_use || img->buffer == NULL) {
		memset(output, 0, frame_count * channels * sizeof(float));
		return;
	}

	sample_rate = ma_engine_get_sample_rate(g_engine);

	if (synth->mode == IMAGE_SYNTH_ADDITIVE) {
		/* calculate scan rate:
		 * - beats_per_sample: how many beats pass per audio sample
		 * - columns_per_sample: how many image columns to advance per sample
		 */
		beats_per_sample = synth->params.additive.bpm / 60.0f / sample_rate;
		columns_per_sample = (beats_per_sample / synth->params.additive.beats_per_scan) * img->buf_width;

		for (frame = 0; frame < frame_count; frame++) {
			sample = 0.0f;

			/*
			 * additive synthesis: sum sine waves for each row.
			 * each row is one oscillator; amplitude comes from image pixel brightness.
			 */
			for (row = 0; row < img->buf_height; row++) {
				amp = cubic_interpolate_row(img, synth->channel, row, synth->params.additive.scan_position);
				sample += sinf(synth->params.additive.phases[row] * 2.0f * (float)M_PI) * amp * synth->params.additive.amplitudes[row];

				/* advance oscillator phase, wrap to [0, 1]
				 * frequency/sample_rate gives the fraction of a full cycle (0-1) to advance per sample. */
				synth->params.additive.phases[row] += synth->params.additive.frequencies[row] / sample_rate;
				if (synth->params.additive.phases[row] >= 1.0f) {
					synth->params.additive.phases[row] -= 1.0f;
				}
			}

			/* write mono sample to all output channels */
			for (ch = 0; ch < channels; ch++) {
				output[frame * channels + ch] = sample;
			}

			/* advance scan position, handle looping or stop at end */
			synth->params.additive.scan_position += columns_per_sample;
			if (synth->params.additive.scan_position >= img->buf_width) {
				if (synth->params.additive.looping) {
					synth->params.additive.scan_position -= img->buf_width;
				} else {
					synth->params.additive.scan_position = (float)(img->buf_width - 1);
					synth->playing = MA_FALSE;
				}
			}
		}
	} else {
		/* waveform mode: read row as wavetable */
		float phase_inc = synth->params.waveform.frequency / sample_rate;

		for (frame = 0; frame < frame_count; frame++) {
			/* phase is 0-1, map to 0-buf_width for interpolation */
			float x = synth->params.waveform.phase * img->buf_width;
			sample = cubic_interpolate_row(img, synth->channel, synth->params.waveform.row, x);
			/* convert from 0-1 brightness to -1 to 1 audio range */
			sample = (sample * 2.0f - 1.0f) * synth->params.waveform.amplitude;

			for (ch = 0; ch < channels; ch++) {
				output[frame * channels + ch] = sample;
			}

			synth->params.waveform.phase += phase_inc;
			if (synth->params.waveform.phase >= 1.0f) {
				synth->params.waveform.phase -= 1.0f;
			}
		}
	}
}

static ma_node_vtable image_synth_vtable = {
	image_synth_process_pcm_frames,
	NULL,
	0,		/* 0 input buses (generator) */
	1,		/* 1 output bus */
	0
};

/******************************************************************************
 * IMAGE SYNTH PROLOG PREDICATES
 *****************************************************************************/

/*
 * pl_image_synth_create()
 * Creates an image synth node for a given image, channel, and mode.
 * image_synth_create(+Image, +Channel, +Mode, -SynthHandle)
 * Mode is 'additive' or 'waveform'.
 */
static foreign_t pl_image_synth_create(term_t image_term, term_t channel_term, term_t mode_term, term_t synth_term)
{
	term_t image_handle;
	int image_slot;
	int channel;
	int synth_slot;
	image_synth_node_t* synth;
	image_slot_t* img;
	ma_node_config config;
	ma_uint32 channels;
	ma_result result;
	char* mode_str;
	image_synth_mode_t mode;
	int i;

	ENSURE_ENGINE_INITIALIZED();

	/* Get image slot from image(Handle) term */
	image_handle = PL_new_term_ref();
	if (!PL_get_arg(1, image_term, image_handle) || !PL_get_integer(image_handle, &image_slot)) {
		return PL_type_error("image", image_term);
	}

	if (image_slot < 0 || image_slot >= MAX_IMAGES || !g_images[image_slot].in_use) {
		return PL_existence_error("image", image_term);
	}
	img = &g_images[image_slot];

	if (!PL_get_integer(channel_term, &channel)) {
		return PL_type_error("integer", channel_term);
	}
	if (channel < 0 || channel >= img->channels) {
		return PL_domain_error("valid_channel", channel_term);
	}

	if (!PL_get_atom_chars(mode_term, &mode_str)) {
		return PL_type_error("atom", mode_term);
	}
	if (strcmp(mode_str, "additive") == 0) {
		mode = IMAGE_SYNTH_ADDITIVE;
		if (img->buf_height > MAX_IMAGE_SYNTH_ROWS) {
			return PL_domain_error("image_128_rows_or_fewer", image_term);
		}
	} else if (strcmp(mode_str, "waveform") == 0) {
		mode = IMAGE_SYNTH_WAVEFORM;
	} else {
		return PL_domain_error("additive_or_waveform", mode_term);
	}

	synth_slot = allocate_image_synth_slot();
	if (synth_slot < 0) {
		return PL_resource_error("image_synth_slots");
	}

	synth = &g_image_synths[synth_slot];
	channels = ma_engine_get_channels(g_engine);

	config = ma_node_config_init();
	config.vtable = &image_synth_vtable;
	config.pInputChannels = NULL;
	config.pOutputChannels = &channels;

	result = ma_node_init(ma_engine_get_node_graph(g_engine), &config, NULL, &synth->base);
	if (result != MA_SUCCESS) {
		free_image_synth_slot(synth_slot);
		return FALSE;
	}

	/* Connect to endpoint by default */
	result = ma_node_attach_output_bus(&synth->base, 0,
			ma_node_graph_get_endpoint(ma_engine_get_node_graph(g_engine)), 0);
	if (result != MA_SUCCESS) {
		ma_node_uninit(&synth->base, NULL);
		free_image_synth_slot(synth_slot);
		return FALSE;
	}

	/* Initialize synth fields */
	synth->image_slot = image_slot;
	synth->channel = channel;
	synth->playing = MA_FALSE;
	synth->mode = mode;
	synth->effect_chain = NULL;

	if (mode == IMAGE_SYNTH_ADDITIVE) {
		synth->params.additive.bpm = 120.0f;
		synth->params.additive.beats_per_scan = 4.0f;
		synth->params.additive.looping = MA_TRUE;
		synth->params.additive.scan_position = 0.0f;

		for (i = 0; i < img->buf_height; i++) {
			synth->params.additive.phases[i] = 0.0f;
			synth->params.additive.frequencies[i] = 0.0f;
			synth->params.additive.amplitudes[i] = 1.0f;
		}
	} else {
		synth->params.waveform.frequency = 440.0f;
		synth->params.waveform.amplitude = 1.0f;
		synth->params.waveform.phase = 0.0f;
		synth->params.waveform.row = 0;
	}

	return PL_unify_integer(synth_term, synth_slot);
}

/*
 * pl_image_synth_unload()
 * Destroys an image synth node.
 * image_synth_unload(+SynthHandle)
 */
static foreign_t pl_image_synth_unload(term_t synth_term)
{
	int synth_slot;

	if (!PL_get_integer(synth_term, &synth_slot)) {
		return PL_type_error("integer", synth_term);
	}

	if (synth_slot < 0 || synth_slot >= MAX_IMAGE_SYNTHS) {
		return PL_existence_error("image_synth", synth_term);
	}

	if (!g_image_synths[synth_slot].in_use) {
		return PL_existence_error("image_synth", synth_term);
	}

	free_image_synth_slot(synth_slot);
	return TRUE;
}

/*
 * pl_image_synth_set_parameters()
 * Set parameters on an image synth.
 * image_synth_set_parameters(+SynthHandle, +ParamsList)
 */
static foreign_t pl_image_synth_set_parameters(term_t synth_term, term_t params_list)
{
	int synth_slot;
	image_synth_node_t* synth;
	image_slot_t* img;
	term_t list;
	term_t head;
	functor_t eq_functor;

	if (!PL_get_integer(synth_term, &synth_slot)) {
		return PL_type_error("integer", synth_term);
	}

	if (synth_slot < 0 || synth_slot >= MAX_IMAGE_SYNTHS || !g_image_synths[synth_slot].in_use) {
		return PL_existence_error("image_synth", synth_term);
	}

	synth = &g_image_synths[synth_slot];
	img = &g_images[synth->image_slot];
	list = PL_copy_term_ref(params_list);
	head = PL_new_term_ref();
	eq_functor = PL_new_functor(PL_new_atom("="), 2);

	while (PL_get_list(list, head, list)) {
		term_t key_term = PL_new_term_ref();
		term_t value_term = PL_new_term_ref();
		char* param_name;

		if (!PL_is_functor(head, eq_functor)) {
			return PL_type_error("key_value_pair", head);
		}
		if (!PL_get_arg(1, head, key_term)) {
			return FALSE;
		}
		if (!PL_get_arg(2, head, value_term)) {
			return FALSE;
		}
		if (!PL_get_atom_chars(key_term, &param_name)) {
			return PL_type_error("atom", key_term);
		}

		if (synth->mode == IMAGE_SYNTH_ADDITIVE) {
			/* additive mode parameters */
			if (strcmp(param_name, "bpm") == 0) {
				double val;
				if (!PL_get_float(value_term, &val)) {
					return PL_type_error("float", value_term);
				}
				if (val <= 0) {
					return PL_domain_error("positive_number", value_term);
				}
				synth->params.additive.bpm = (float)val;
			} else if (strcmp(param_name, "beats_per_scan") == 0) {
				double val;
				if (!PL_get_float(value_term, &val)) {
					return PL_type_error("float", value_term);
				}
				if (val <= 0) {
					return PL_domain_error("positive_number", value_term);
				}
				synth->params.additive.beats_per_scan = (float)val;
			} else if (strcmp(param_name, "looping") == 0) {
				int val;
				if (!PL_get_bool(value_term, &val)) {
					return PL_type_error("bool", value_term);
				}
				synth->params.additive.looping = val ? MA_TRUE : MA_FALSE;
			} else if (strcmp(param_name, "scan_position") == 0) {
				double val;
				if (!PL_get_float(value_term, &val)) {
					return PL_type_error("float", value_term);
				}
				if (val < 0 || val >= img->buf_width) {
					return PL_domain_error("valid_scan_position", value_term);
				}
				synth->params.additive.scan_position = (float)val;
			} else if (strcmp(param_name, "frequencies") == 0) {
				term_t freq_list = PL_copy_term_ref(value_term);
				term_t freq_head = PL_new_term_ref();
				int i = 0;
				while (PL_get_list(freq_list, freq_head, freq_list)) {
					double freq;
					if (i >= img->buf_height) {
						return PL_domain_error("frequencies_length", value_term);
					}
					if (!PL_get_float(freq_head, &freq)) {
						return PL_type_error("float", freq_head);
					}
					synth->params.additive.frequencies[i] = (float)freq;
					i++;
				}
				if (i != img->buf_height) {
					return PL_domain_error("frequencies_length", value_term);
				}
			} else if (strcmp(param_name, "phases") == 0) {
				term_t phase_list = PL_copy_term_ref(value_term);
				term_t phase_head = PL_new_term_ref();
				int i = 0;
				while (PL_get_list(phase_list, phase_head, phase_list)) {
					double phase;
					if (i >= img->buf_height) {
						return PL_domain_error("phases_length", value_term);
					}
					if (!PL_get_float(phase_head, &phase)) {
						return PL_type_error("float", phase_head);
					}
					synth->params.additive.phases[i] = (float)phase;
					i++;
				}
				if (i != img->buf_height) {
					return PL_domain_error("phases_length", value_term);
				}
			} else if (strcmp(param_name, "amplitudes") == 0) {
				term_t amp_list = PL_copy_term_ref(value_term);
				term_t amp_head = PL_new_term_ref();
				int i = 0;
				while (PL_get_list(amp_list, amp_head, amp_list)) {
					double amp;
					if (i >= img->buf_height) {
						return PL_domain_error("amplitudes_length", value_term);
					}
					if (!PL_get_float(amp_head, &amp)) {
						return PL_type_error("float", amp_head);
					}
					synth->params.additive.amplitudes[i] = (float)amp;
					i++;
				}
				if (i != img->buf_height) {
					return PL_domain_error("amplitudes_length", value_term);
				}
			} else {
				return PL_domain_error("additive_synth_parameter", key_term);
			}
		} else {
			/* waveform mode parameters */
			if (strcmp(param_name, "frequency") == 0) {
				double val;
				if (!PL_get_float(value_term, &val)) {
					return PL_type_error("float", value_term);
				}
				if (val <= 0) {
					return PL_domain_error("positive_number", value_term);
				}
				synth->params.waveform.frequency = (float)val;
			} else if (strcmp(param_name, "amplitude") == 0) {
				double val;
				if (!PL_get_float(value_term, &val)) {
					return PL_type_error("float", value_term);
				}
				synth->params.waveform.amplitude = (float)val;
			} else if (strcmp(param_name, "phase") == 0) {
				double val;
				if (!PL_get_float(value_term, &val)) {
					return PL_type_error("float", value_term);
				}
				synth->params.waveform.phase = (float)val;
			} else if (strcmp(param_name, "row") == 0) {
				int val;
				if (!PL_get_integer(value_term, &val)) {
					return PL_type_error("integer", value_term);
				}
				if (val < 0 || val >= img->buf_height) {
					return PL_domain_error("valid_row", value_term);
				}
				synth->params.waveform.row = val;
			} else {
				return PL_domain_error("waveform_synth_parameter", key_term);
			}
		}
	}

	if (!PL_get_nil(list)) {
		return PL_type_error("list", params_list);
	}

	return TRUE;
}

/*
 * pl_image_synth_get_parameters()
 * Query all parameters of an image synth.
 * image_synth_get_parameters(+SynthHandle, -ParamsList)
 */
static foreign_t pl_image_synth_get_parameters(term_t synth_term, term_t params_term)
{
	int synth_slot;
	image_synth_node_t* synth;
	image_slot_t* img;
	term_t params_list;
	term_t param_args;
	functor_t eq_functor;
	term_t param_term;
	int i;

	if (!PL_get_integer(synth_term, &synth_slot)) {
		return PL_type_error("integer", synth_term);
	}

	if (synth_slot < 0 || synth_slot >= MAX_IMAGE_SYNTHS || !g_image_synths[synth_slot].in_use) {
		return PL_existence_error("image_synth", synth_term);
	}

	synth = &g_image_synths[synth_slot];
	img = &g_images[synth->image_slot];
	params_list = PL_new_term_ref();
	param_args = PL_new_term_refs(2);
	eq_functor = PL_new_functor(PL_new_atom("="), 2);
	param_term = PL_new_term_ref();

	PL_put_nil(params_list);

	if (synth->mode == IMAGE_SYNTH_ADDITIVE) {
		/* phases */
		{
			term_t phases_list = PL_new_term_ref();
			PL_put_nil(phases_list);
			for (i = img->buf_height - 1; i >= 0; i--) {
				term_t pt = PL_new_term_ref();
				if (!PL_put_float(pt, synth->params.additive.phases[i])) {
					return FALSE;
				}
				if (!PL_cons_list(phases_list, pt, phases_list)) {
					return FALSE;
				}
			}
			PL_put_atom_chars(param_args+0, "phases");
			if (!PL_put_term(param_args+1, phases_list)) {
				return FALSE;
			}
			if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
				return FALSE;
			}
			if (!PL_cons_list(params_list, param_term, params_list)) {
				return FALSE;
			}
		}

		/* amplitudes */
		{
			term_t amps_list = PL_new_term_ref();
			PL_put_nil(amps_list);
			for (i = img->buf_height - 1; i >= 0; i--) {
				term_t at = PL_new_term_ref();
				if (!PL_put_float(at, synth->params.additive.amplitudes[i])) {
					return FALSE;
				}
				if (!PL_cons_list(amps_list, at, amps_list)) {
					return FALSE;
				}
			}
			PL_put_atom_chars(param_args+0, "amplitudes");
			if (!PL_put_term(param_args+1, amps_list)) {
				return FALSE;
			}
			if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
				return FALSE;
			}
			if (!PL_cons_list(params_list, param_term, params_list)) {
				return FALSE;
			}
		}

		/* frequencies */
		{
			term_t freqs_list = PL_new_term_ref();
			PL_put_nil(freqs_list);
			for (i = img->buf_height - 1; i >= 0; i--) {
				term_t ft = PL_new_term_ref();
				if (!PL_put_float(ft, synth->params.additive.frequencies[i])) {
					return FALSE;
				}
				if (!PL_cons_list(freqs_list, ft, freqs_list)) {
					return FALSE;
				}
			}
			PL_put_atom_chars(param_args+0, "frequencies");
			if (!PL_put_term(param_args+1, freqs_list)) {
				return FALSE;
			}
			if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
				return FALSE;
			}
			if (!PL_cons_list(params_list, param_term, params_list)) {
				return FALSE;
			}
		}

		/* scan_position */
		PL_put_atom_chars(param_args+0, "scan_position");
		if (!PL_put_float(param_args+1, synth->params.additive.scan_position)) {
			return FALSE;
		}
		if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
			return FALSE;
		}
		if (!PL_cons_list(params_list, param_term, params_list)) {
			return FALSE;
		}

		/* looping */
		PL_put_atom_chars(param_args+0, "looping");
		if (!PL_put_atom_chars(param_args+1, synth->params.additive.looping ? "true" : "false")) {
			return FALSE;
		}
		if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
			return FALSE;
		}
		if (!PL_cons_list(params_list, param_term, params_list)) {
			return FALSE;
		}

		/* beats_per_scan */
		PL_put_atom_chars(param_args+0, "beats_per_scan");
		if (!PL_put_float(param_args+1, synth->params.additive.beats_per_scan)) {
			return FALSE;
		}
		if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
			return FALSE;
		}
		if (!PL_cons_list(params_list, param_term, params_list)) {
			return FALSE;
		}

		/* bpm */
		PL_put_atom_chars(param_args+0, "bpm");
		if (!PL_put_float(param_args+1, synth->params.additive.bpm)) {
			return FALSE;
		}
		if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
			return FALSE;
		}
		if (!PL_cons_list(params_list, param_term, params_list)) {
			return FALSE;
		}
	} else {
		/* waveform mode */
		/* row */
		PL_put_atom_chars(param_args+0, "row");
		if (!PL_put_integer(param_args+1, synth->params.waveform.row)) {
			return FALSE;
		}
		if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
			return FALSE;
		}
		if (!PL_cons_list(params_list, param_term, params_list)) {
			return FALSE;
		}

		/* phase */
		PL_put_atom_chars(param_args+0, "phase");
		if (!PL_put_float(param_args+1, synth->params.waveform.phase)) {
			return FALSE;
		}
		if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
			return FALSE;
		}
		if (!PL_cons_list(params_list, param_term, params_list)) {
			return FALSE;
		}

		/* amplitude */
		PL_put_atom_chars(param_args+0, "amplitude");
		if (!PL_put_float(param_args+1, synth->params.waveform.amplitude)) {
			return FALSE;
		}
		if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
			return FALSE;
		}
		if (!PL_cons_list(params_list, param_term, params_list)) {
			return FALSE;
		}

		/* frequency */
		PL_put_atom_chars(param_args+0, "frequency");
		if (!PL_put_float(param_args+1, synth->params.waveform.frequency)) {
			return FALSE;
		}
		if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
			return FALSE;
		}
		if (!PL_cons_list(params_list, param_term, params_list)) {
			return FALSE;
		}
	}

	return PL_unify(params_term, params_list);
}

/*
 * pl_image_synth_start()
 * Start playback of an image synth.
 * image_synth_start(+SynthHandle)
 */
static foreign_t pl_image_synth_start(term_t synth_term)
{
	int synth_slot;

	if (!PL_get_integer(synth_term, &synth_slot)) {
		return PL_type_error("integer", synth_term);
	}

	if (synth_slot < 0 || synth_slot >= MAX_IMAGE_SYNTHS || !g_image_synths[synth_slot].in_use) {
		return PL_existence_error("image_synth", synth_term);
	}

	g_image_synths[synth_slot].playing = MA_TRUE;
	return TRUE;
}

/*
 * pl_image_synth_stop()
 * Stop playback of an image synth.
 * image_synth_stop(+SynthHandle)
 */
static foreign_t pl_image_synth_stop(term_t synth_term)
{
	int synth_slot;

	if (!PL_get_integer(synth_term, &synth_slot)) {
		return PL_type_error("integer", synth_term);
	}

	if (synth_slot < 0 || synth_slot >= MAX_IMAGE_SYNTHS || !g_image_synths[synth_slot].in_use) {
		return PL_existence_error("image_synth", synth_term);
	}

	g_image_synths[synth_slot].playing = MA_FALSE;
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
	PL_register_foreign("image_transpose", 2, pl_image_transpose, 0);

	/* Image synth predicates */
	PL_register_foreign("image_synth_create", 4, pl_image_synth_create, 0);
	PL_register_foreign("image_synth_unload", 1, pl_image_synth_unload, 0);
	PL_register_foreign("image_synth_set_parameters", 2, pl_image_synth_set_parameters, 0);
	PL_register_foreign("image_synth_get_parameters", 2, pl_image_synth_get_parameters, 0);
	PL_register_foreign("image_synth_start", 1, pl_image_synth_start, 0);
	PL_register_foreign("image_synth_stop", 1, pl_image_synth_stop, 0);
}
