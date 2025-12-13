/*
 * mixer.c - Summing nodes and crossfaders
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#include "promini.h"

/******************************************************************************
 * GLOBAL VARIABLES
 *****************************************************************************/

summing_node_t g_summing_nodes[MAX_SUMMING_NODES] = {{0}};
pthread_mutex_t g_summing_mutex = PTHREAD_MUTEX_INITIALIZER;

/******************************************************************************
 * SLOT MANAGEMENT
 *****************************************************************************/

static int allocate_summing_slot(void)
{
	int i;
	for (i = 0; i < MAX_SUMMING_NODES; i++) {
		if (!g_summing_nodes[i].in_use) {
			g_summing_nodes[i].in_use = MA_TRUE;
			return i;
		}
	}
	return -1;
}

static void free_summing_slot(int index)
{
	if (index >= 0 && index < MAX_SUMMING_NODES) {
		g_summing_nodes[index].in_use = MA_FALSE;
		g_summing_nodes[index].effect_chain = NULL;
	}
}

/******************************************************************************
 * SUMMING NODE
 *****************************************************************************/

static void summing_process_pcm_frames(
		ma_node *node,
		const float** frames_in,
		ma_uint32 *frame_count_in,
		float** frames_out,
		ma_uint32 *frame_count_out)
{
	(void)node;
	(void)frames_in;
	(void)frame_count_in;
	(void)frames_out;
	(void)frame_count_out;
}

/*
 * Passthrough vtable for summing node.
 * No processing callback - just passes input to output.
 */
static ma_node_vtable summing_vtable = {
	summing_process_pcm_frames,
	NULL,  /* no uninit callback */
	1,     /* 1 input bus */
	1,     /* 1 output bus */
	MA_NODE_FLAG_PASSTHROUGH
};

/*
 * pl_summing_node_init()
 * Creates a summing node. Multiple sources can connect to it.
 * summing_node_init(-SummingNode)
 * Returns summing_node(N).
 */
static foreign_t pl_summing_node_init(term_t handle_term)
{
	int slot;
	summing_node_t *node;
	ma_node_config config;
	ma_uint32 channels;
	ma_result result;

	ENSURE_ENGINE_INITIALIZED();

	pthread_mutex_lock(&g_summing_mutex);

	slot = allocate_summing_slot();
	if (slot < 0) {
		pthread_mutex_unlock(&g_summing_mutex);
		return PL_resource_error("summing_node_slots");
	}

	node = &g_summing_nodes[slot];
	channels = ma_engine_get_channels(g_engine);

	config = ma_node_config_init();
	config.vtable = &summing_vtable;
	config.pInputChannels = &channels;
	config.pOutputChannels = &channels;

	result = ma_node_init(ma_engine_get_node_graph(g_engine), &config, NULL, &node->base);
	if (result != MA_SUCCESS) {
		free_summing_slot(slot);
		pthread_mutex_unlock(&g_summing_mutex);
		return FALSE;
	}

	/* connect to endpoint by default */
	result = ma_node_attach_output_bus(&node->base, 0,
			ma_node_graph_get_endpoint(ma_engine_get_node_graph(g_engine)), 0);
	if (result != MA_SUCCESS) {
		ma_node_uninit(&node->base, NULL);
		free_summing_slot(slot);
		pthread_mutex_unlock(&g_summing_mutex);
		return FALSE;
	}

	node->effect_chain = NULL;

	pthread_mutex_unlock(&g_summing_mutex);
	return unify_typed_handle(handle_term, "summing_node", slot);
}

/*
 * pl_summing_node_uninit()
 * Destroys a summing node.
 * summing_node_uninit(+SummingNode)
 */
static foreign_t pl_summing_node_uninit(term_t handle_term)
{
	int slot;
	summing_node_t *node;

	if (!get_typed_handle(handle_term, "summing_node", &slot)) {
		return PL_type_error("summing_node", handle_term);
	}
	if (slot < 0 || slot >= MAX_SUMMING_NODES) {
		return PL_existence_error("summing_node", handle_term);
	}

	pthread_mutex_lock(&g_summing_mutex);

	node = &g_summing_nodes[slot];
	if (!node->in_use) {
		pthread_mutex_unlock(&g_summing_mutex);
		return PL_existence_error("summing_node", handle_term);
	}

	free_effect_chain(node->effect_chain);
	ma_node_uninit(&node->base, NULL);
	free_summing_slot(slot);

	pthread_mutex_unlock(&g_summing_mutex);
	return TRUE;
}

/* pl_summing_node_connect()
 * Connects a source to a summing node.
 * summing_node_connect(+SummingNode, +Source)
 * Source is sound(N), voice(N)
 */
static foreign_t pl_summing_node_connect(term_t handle_term, term_t source_term)
{
	int slot;
	summing_node_t *node;
	ma_node *source_node;
	ma_node *output_node;
	effect_node_t *chain;
	ma_result result;

	if (!get_typed_handle(handle_term, "summing_node", &slot)) {
		return PL_type_error("summing_node", handle_term);
	}
	if (slot < 0 || slot >= MAX_SUMMING_NODES) {
		return PL_existence_error("summing_node", handle_term);
	}

	if (!get_source_from_term(source_term, &source_node, &chain)) {
  		return PL_existence_error("source", source_term);
  	}

	pthread_mutex_lock(&g_summing_mutex);

	node = &g_summing_nodes[slot];
	if (!node->in_use) {
		pthread_mutex_unlock(&g_summing_mutex);
		return PL_existence_error("summing_node", handle_term);
	}

	/* find output node: end of effect chain, or source itself */
	output_node = get_effect_chain_tail(chain);
	if (output_node == NULL) {
		output_node = source_node;
	}

	/* attach to summing node (auto-detaches from previous) */
	result = ma_node_attach_output_bus(output_node, 0, (ma_node*)&node->base, 0);

	pthread_mutex_unlock(&g_summing_mutex);
	return result == MA_SUCCESS? TRUE : FALSE;
}

/*
 * pl_summing_node_disconnect()
 * Disconnects a source from a summing node, reconnecting it to the endpoint.
 * summing_node_disconnect(+Source)
 * Source is sound(N) or voice(N).
 */
static foreign_t pl_summing_node_disconnect(term_t source_term)
{
	ma_node *source_node;
	ma_node *output_node;
	effect_node_t *chain;
	ma_result result;

	if (!get_source_from_term(source_term, &source_node, &chain)) {
		return PL_existence_error("source", source_term);
	}

	/* find output node: end of effect chain, or source itself */
	output_node = get_effect_chain_tail(chain);
	if (output_node == NULL) {
		output_node = source_node;
	}

	/* reconnect to endpoint */
	result = ma_node_attach_output_bus(output_node, 0,
			ma_node_graph_get_endpoint(ma_engine_get_node_graph(g_engine)), 0);

	return result == MA_SUCCESS ? TRUE : FALSE;
}


/******************************************************************************
 * REGISTRATION
 *****************************************************************************/

install_t mixer_register_predicates(void)
{
	PL_register_foreign("summing_node_init", 1, pl_summing_node_init, 0);
	PL_register_foreign("summing_node_uninit", 1, pl_summing_node_uninit, 0);
	PL_register_foreign("summing_node_connect", 2, pl_summing_node_connect, 0);
  	PL_register_foreign("summing_node_disconnect", 1, pl_summing_node_disconnect, 0);
}

/*
 * uninstall_mixer()
 * Cleans up all mixer resources.
 */
install_t uninstall_mixer(void)
{
	int i;

	pthread_mutex_lock(&g_summing_mutex);

	for (i = 0; i < MAX_SUMMING_NODES; i++) {
		if (g_summing_nodes[i].in_use) {
			free_effect_chain(g_summing_nodes[i].effect_chain);
			ma_node_uninit(&g_summing_nodes[i].base, NULL);
			g_summing_nodes[i].in_use = MA_FALSE;
		}
	}

	pthread_mutex_unlock(&g_summing_mutex);
}
