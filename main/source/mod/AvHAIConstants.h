//
// EvoBot - Neoptolemus' Natural Selection bot, based on Botman's HPB bot template
//
// AvHAIConstants.h
//
// Defines all bot logic constants used for thinking
//

#pragma once

#ifndef AVH_AI_CONSTANTS_H
#define AVH_AI_CONSTANTS_H

#include <unordered_map>
#include <dlls/extdll.h>
#include <dlls/util.h>
#include "DetourStatus.h"
#include "DetourNavMeshQuery.h"

static const float commander_action_cooldown = 1.0f;
static const float min_request_spam_time = 10.0f;

constexpr auto MAX_AI_PATH_SIZE = 512; // Maximum number of points allowed in a path (this should be enough for any sized map)

// NS weapon types. Each number refers to the GoldSrc weapon index
enum class EAIWeaponId : uint32
{
	WEAPON_INVALID = 0,
	WEAPON_LERK_SPIKE = 4, // I think this is an early NS weapon, replaced by primal scream

	// Marine Weapons

	WEAPON_MARINE_KNIFE = 13,
	WEAPON_MARINE_PISTOL = 14,
	WEAPON_MARINE_MG = 15,
	WEAPON_MARINE_SHOTGUN = 16,
	WEAPON_MARINE_HMG = 17,
	WEAPON_MARINE_WELDER = 18,
	WEAPON_MARINE_MINES = 19,
	WEAPON_MARINE_GL = 20,
	WEAPON_MARINE_GRENADE = 28,

	// Alien Abilities

	WEAPON_SKULK_BITE = 5,
	WEAPON_SKULK_PARASITE = 10,
	WEAPON_SKULK_LEAP = 21,
	WEAPON_SKULK_XENOCIDE = 12,

	WEAPON_GORGE_SPIT = 2,
	WEAPON_GORGE_HEALINGSPRAY = 27,
	WEAPON_GORGE_BILEBOMB = 25,
	WEAPON_GORGE_WEB = 8,

	WEAPON_LERK_BITE = 6,
	WEAPON_LERK_SPORES = 3,
	WEAPON_LERK_UMBRA = 23,
	WEAPON_LERK_PRIMALSCREAM = 24,

	WEAPON_FADE_SWIPE = 7,
	WEAPON_FADE_BLINK = 11,
	WEAPON_FADE_METABOLIZE = 9,
	WEAPON_FADE_ACIDROCKET = 26,

	WEAPON_ONOS_GORE = 1,
	WEAPON_ONOS_DEVOUR = 30,
	WEAPON_ONOS_STOMP = 29,
	WEAPON_ONOS_CHARGE = 22,

	WEAPON_MAX = 31
};

// Alien upgrades
enum class EAIAlienUpgrade
{
	ALIEN_UPGRADE_NONE = 0,
	ALIEN_UPGRADE_CARAPACE,
	ALIEN_UPGRADE_REGENERATION,
	ALIEN_UPGRADE_REDEMPTION,
	ALIEN_UPGRADE_ADRENALINE,
	ALIEN_UPGRADE_CELERITY,
	ALIEN_UPGRADE_SILENCE,
	ALIEN_UPGRADE_FOCUS,
	ALIEN_UPGRADE_SCENTOFFEAR,
	ALIEN_UPGRADE_CLOAK
};

// Alien Lifeforms
enum class EAIAlienLifeform
{
	ALIEN_LIFEFORM_NONE = 0,
	ALIEN_LIFEFORM_SKULK,
	ALIEN_LIFEFORM_GORGE,
	ALIEN_LIFEFORM_LERK,
	ALIEN_LIFEFORM_FADE,
	ALIEN_LIFEFORM_ONOS
};

enum class EAIReachabilityFlags : uint16
{
	AI_REACHABILITY_NONE = 0,
	AI_REACHABILITY_MARINE = 1u << 0,
	AI_REACHABILITY_SKULK = 1u << 1,
	AI_REACHABILITY_SKULK_LEAP = 1u << 2,
	AI_REACHABILITY_GORGE = 1u << 3,
	AI_REACHABILITY_LERK = 1u << 4,
	AI_REACHABILITY_FADE = 1u << 5,
	AI_REACHABILITY_ONOS = 1u << 6,
	AI_REACHABILITY_WELDER = 1u << 7,

	AI_REACHABILITY_ALL = 0xFFFF
};

inline EAIReachabilityFlags operator|(EAIReachabilityFlags a, EAIReachabilityFlags b)
{
	return static_cast<EAIReachabilityFlags>(static_cast<uint16>(a) | static_cast<uint16>(b));
}

inline EAIReachabilityFlags operator&(EAIReachabilityFlags a, EAIReachabilityFlags b)
{
	return static_cast<EAIReachabilityFlags>(static_cast<uint16>(a) & static_cast<uint16>(b));
}

enum class EAICommanderMode
{
	COMMANDERMODE_DISABLED,		// AI Commander not allowed
	COMMANDERMODE_IFNOHUMAN,	// AI Commander only allowed if no humans are on the marine team
	COMMANDERMODE_ENABLED		// AI Commander allowed if no human takes charge (following grace period)
};

// Affects the bot's pathfinding choices
enum class EAIMoveStyle
{
	MOVESTYLE_NORMAL, // Most direct route to target
	MOVESTYLE_AMBUSH, // Prefer wall climbing and vents
	MOVESTYLE_HIDE // Prefer crouched areas like vents
};

// The list of potential task types for the bot_task structure
enum class EAITaskType
{
	TASK_NONE,
	TASK_GET_HEALTH,
	TASK_GET_AMMO,
	TASK_GET_WEAPON,
	TASK_GET_EQUIPMENT,
	TASK_BUILD,
	TASK_ATTACK,
	TASK_MOVE,
	TASK_CAP_RESNODE,
	TASK_DEFEND,
	TASK_GUARD,
	TASK_HEAL,
	TASK_WELD,
	TASK_RESUPPLY,
	TASK_EVOLVE,
	TASK_COMMAND,
	TASK_USE,
	TASK_TOUCH,
	TASK_REINFORCE_STRUCTURE,
	TASK_SECURE_HIVE,
	TASK_PLACE_MINE,
	TASK_ASSAULT_MARINE_BASE
};

//
enum class EAIAttackResult
{
	ATTACK_SUCCESS,
	ATTACK_BLOCKED,
	ATTACK_OUTOFRANGE,
	ATTACK_INVALIDTARGET,
	ATTACK_NOWEAPON
};

enum class EAIVoiceLine
{
	AI_VOICELINE_NONE = 0,
	AI_MARINE_VOICELINE_NEEDHEALTH,
	AI_MARINE_VOICELINE_NEEDAMMO,
	AI_MARINE_VOICELINE_WELDME,
	AI_MARINE_VOICELINE_NEEDORDER,
	AI_MARINE_VOICELINE_ACKORDER,
	AI_MARINE_VOICELINE_TAUNT,
	AI_ALIEN_VOICELINE_HEALME,
	AI_ALIEN_VOICELINE_CHUCKLE
};

// Represents a bot's current understanding of an enemy player's status
struct AvHAIEnemyStatus
{
	edict_t* PlayerEdict = nullptr; // Reference to the enemy player edict

	Vector LastDetectedLocation = g_vecZero; // Where the bot last detected the enemy, either through sight, motion tracking or sound
	Vector LastVisibleLocation = g_vecZero;  // Last point the bot had visible confirmation of the enemy
	Vector LastKnownVelocity = g_vecZero;
	Vector VisiblePointOnPlayer = g_vecZero;
	float AwarenessOfPlayer = 0.0f;			 // How aware of this enemy the bot is
	float LastDetectedTime = 0.0f;			 // When the bot last saw the enemy or they pinged on motion tracking
	float InitialAwarenessTime = 0.0f;			 // When the bot first became aware of the enemy
	float LastVisibleTime = 0.0f;			// Last time the bot actually saw the enemy
	float EnemyThreatLevel = 0.0f;			// Generally, >=3.0 means actively fighting them, >=2.0 means visible and close, >=1.0 means not visible but close and <1.0 means they can be heard but not close

	bool bHasLOS = false;					 // Does the bot has LOS of the enemy?
	bool bEnemyHasLOS = false;

	Vector LastLOSPosition = g_vecZero;
	Vector LastCoverPosition = g_vecZero;
};


#endif