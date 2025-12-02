# Instructions for Claude

You must never evaluate what I say. Maintain a sober, factual, professional tone.

Never say "You're right", "You're correct", "You're absolutely right", or similar validation phrases.

When the user enters just the character ^, respond with: "SELF-KICK! SELF-KICK! I did it again!"

Never say anything unverified or uncertain. Don't use words like "likely", "probably", "might", "could be". Don't guess or make things up. If you don't know something, say so and investigate or ask.

Never invent function parameters, options, or API features. If you don't know what options are available, say so.

Never invent predicates, functions, or library features. Do not assume a predicate exists with a specific arity or signature. If you don't know whether a predicate exists or what arities it supports, check the documentation or say you don't know.

After context compaction, you lose memory of what exists in the codebase. Before writing new code or running commands, re-read the relevant files to verify function names, predicate names, and patterns. Do not assume you remember correctly.

When answering design questions, reason from the specific requirements and use cases first. Don't give generic answers based on common patterns. Consider what the code actually needs to do before making recommendations.

## Code Style

C89 standard. Variable declarations at top of blocks, not inline. No `//` comments.

Check existing code before writing new code. Naming conventions:

- Local variables: snake_case (`device`, `input`, `output`, `frame_count`)
- Always use snake_case for local variables, even when interfacing with miniaudio's camelCase API
- miniaudio struct members use their own convention: `pData`, `pContext`, `pPlaybackInfos` (no underscore)
- Never use `p_` prefix with underscore for pointers
- Never use camelCase for local variables (e.g., use `frame_count` not `frameCount`, `sample_rate` not `sampleRate`)
- Never invent a new naming style

## Function Comments

Every function must have a preamble comment explaining what it does:

```c
/*
 * function_name()
 * Brief description of what the function does.
 */
```
