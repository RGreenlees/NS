#ifndef AVH_AI_PLAYER_H
#define AVH_AI_PLAYER_H

#include "AvHPlayer.h"
#include "AvHAIConstants.h"

// These define the bot's view frustum sides
#define FRUSTUM_PLANE_TOP 0
#define FRUSTUM_PLANE_BOTTOM 1
#define FRUSTUM_PLANE_LEFT 2
#define FRUSTUM_PLANE_RIGHT 3
#define FRUSTUM_PLANE_NEAR 4
#define FRUSTUM_PLANE_FAR 5

static const float BOT_FOV = 100.0f;  // Bot's field of view;
static const float BOT_MAX_VIEW = 9999.0f; // Bot's maximum view distance;
static const float BOT_MIN_VIEW = 5.0f; // Bot's minimum view distance;
static const float BOT_ASPECT_RATIO = 1.77778f; // Bot's view aspect ratio, 1.333333 for 4:3, 1.777778 for 16:9, 1.6 for 16:10;

static const float f_fnheight = 2.0f * tan((BOT_FOV * 0.0174532925f) * 0.5f) * BOT_MIN_VIEW;
static const float f_fnwidth = f_fnheight * BOT_ASPECT_RATIO;

static const float f_ffheight = 2.0f * tan((BOT_FOV * 0.0174532925f) * 0.5f) * BOT_MAX_VIEW;
static const float f_ffwidth = f_ffheight * BOT_ASPECT_RATIO;


Vector GetVisiblePointOnPlayerFromObserver(edict_t* Observer, edict_t* TargetPlayer);

#endif