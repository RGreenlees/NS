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

#include "DetourStatus.h"
#include "DetourNavMeshQuery.h"

#include "AvHHive.h"
#include "AvHEntities.h"
#include "AvHAIMath.h"
#include "AvHAINavConstants.h"
#include "AvHAIMapData.h"

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

// Hives can either be unbuilt ("ghost" hive), in progress or fully built (active)
enum class EAIHiveStatus
{
	HIVE_STATUS_UNBUILT = 0,
	HIVE_STATUS_BUILDING = 1,
	HIVE_STATUS_BUILT = 2
};

// All tech statuses that can be assigned to a hive
enum class EAIHiveTechStatus
{
	HIVE_TECH_NONE = 0, // Hive doesn't have any tech assigned to it yet (no chambers built for it)
	HIVE_TECH_DEFENSE = 1,
	HIVE_TECH_SENSORY = 2,
	HIVE_TECH_MOVEMENT = 3
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

enum class EAIStructureStatus : uint16
{
	STRUCTURE_STATUS_NONE = 0,				// No filters, all buildings will be returned
	STRUCTURE_STATUS_GHOST = 1u << 0,		// For marine structure, this is their "ghost" form before anyone has started building it
	STRUCTURE_STATUS_PARTIAL = 1u << 1,		// Partially finished, but not yet completed
	STRUCTURE_STATUS_COMPLETED = 1u << 2,	// Structure is fully built
	STRUCTURE_STATUS_ELECTRIFIED = 1u << 3,
	STRUCTURE_STATUS_RECYCLING = 1u << 4,
	STRUCTURE_STATUS_PARASITED = 1u << 5,
	STRUCTURE_STATUS_UNDERATTACK = 1u << 6,
	STRUCTURE_STATUS_RESEARCHING = 1u << 7,
	STRUCTURE_STATUS_DAMAGED = 1u << 8,		// When it's completed, but at less than 100% health
	STRUCTURE_STATUS_DISABLED = 1u << 9,		// For marine turrets when there's no TF

	STRUCTURE_STATUS_ALL = 0xFFFF
};

inline EAIStructureStatus operator|(EAIStructureStatus a, EAIStructureStatus b)
{
	return static_cast<EAIStructureStatus>(static_cast<uint16>(a) | static_cast<uint16>(b));
}

inline EAIStructureStatus operator&(EAIStructureStatus a, EAIStructureStatus b)
{
	return static_cast<EAIStructureStatus>(static_cast<uint16>(a) & static_cast<uint16>(b));
}

enum class EAIStructureType : uint32
{
	STRUCTURE_NONE = 0,
	STRUCTURE_MARINE_RESTOWER = 1u << 0,
	STRUCTURE_MARINE_INFANTRYPORTAL = 1u << 1,
	STRUCTURE_MARINE_TURRETFACTORY = 1u << 2,
	STRUCTURE_MARINE_ADVTURRETFACTORY = 1u << 3,
	STRUCTURE_MARINE_ARMORY = 1u << 4,
	STRUCTURE_MARINE_ADVARMORY = 1u << 5,
	STRUCTURE_MARINE_ARMSLAB = 1u << 6,
	STRUCTURE_MARINE_PROTOTYPELAB = 1u << 7,
	STRUCTURE_MARINE_OBSERVATORY = 1u << 8,
	STRUCTURE_MARINE_PHASEGATE = 1u << 9,
	STRUCTURE_MARINE_TURRET = 1u << 10,
	STRUCTURE_MARINE_SIEGETURRET = 1u << 11,
	STRUCTURE_MARINE_COMMCHAIR = 1u << 12,
	STRUCTURE_MARINE_DEPLOYEDMINE = 1u << 13,

	STRUCTURE_ALIEN_HIVE = 1u << 14,
	STRUCTURE_ALIEN_RESTOWER = 1u << 15,
	STRUCTURE_ALIEN_DEFENSECHAMBER = 1u << 16,
	STRUCTURE_ALIEN_SENSORYCHAMBER = 1u << 17,
	STRUCTURE_ALIEN_MOVEMENTCHAMBER = 1u << 18,
	STRUCTURE_ALIEN_OFFENSECHAMBER = 1u << 19,

	ALL_MARINE_STRUCTURES = 0xFFF,
	ALL_ALIEN_STRUCTURES = (STRUCTURE_ALIEN_HIVE | STRUCTURE_ALIEN_RESTOWER | STRUCTURE_ALIEN_DEFENSECHAMBER | STRUCTURE_ALIEN_SENSORYCHAMBER | STRUCTURE_ALIEN_MOVEMENTCHAMBER | STRUCTURE_ALIEN_OFFENSECHAMBER),
	ANY_RES_TOWER = (STRUCTURE_MARINE_RESTOWER | STRUCTURE_ALIEN_RESTOWER),

	ALL_STRUCTURES = ((uint32)-1 & ~(STRUCTURE_MARINE_DEPLOYEDMINE))
};

inline EAIStructureType operator|(EAIStructureType a, EAIStructureType b)
{
	return static_cast<EAIStructureType>(static_cast<uint32>(a) | static_cast<uint32>(b));
}

inline EAIStructureType operator&(EAIStructureType a, EAIStructureType b)
{
	return static_cast<EAIStructureType>(static_cast<uint32>(a) & static_cast<uint32>(b));
}

enum class EAIDeployableItemType : uint16
{
	DEPLOYABLE_ITEM_NONE = 0,
	DEPLOYABLE_ITEM_RESUPPLY = 1u, // For combat mode
	DEPLOYABLE_ITEM_HEAVYARMOUR = 1u << 1,
	DEPLOYABLE_ITEM_JETPACK = 1u << 2,
	DEPLOYABLE_ITEM_CATALYSTS = 1u << 3,
	DEPLOYABLE_ITEM_SCAN = 1u << 4,
	DEPLOYABLE_ITEM_HEALTHPACK = 1u << 5,
	DEPLOYABLE_ITEM_AMMO = 1u << 6,
	DEPLOYABLE_ITEM_MINES = 1u << 7,
	DEPLOYABLE_ITEM_WELDER = 1u << 8,
	DEPLOYABLE_ITEM_SHOTGUN = 1u << 9,
	DEPLOYABLE_ITEM_LMG = 1u << 10,
	DEPLOYABLE_ITEM_HMG = 1u << 11,
	DEPLOYABLE_ITEM_GRENADELAUNCHER = 1u << 12,

	DEPLOYABLE_ITEM_WEAPONS = 0xF80,
	DEPLOYABLE_ITEM_EQUIPMENT = 0x6,

	DEPLOYABLE_ITEM_ALL = 0xFFFF
};

inline EAIDeployableItemType operator|(EAIDeployableItemType a, EAIDeployableItemType b)
{
	return static_cast<EAIDeployableItemType>(static_cast<uint16>(a) | static_cast<uint16>(b));
}

inline EAIDeployableItemType operator&(EAIDeployableItemType a, EAIDeployableItemType b)
{
	return static_cast<EAIDeployableItemType>(static_cast<uint16>(a) & static_cast<uint16>(b));
}

// Type of goal the commander wants to achieve
enum class EAIStructurePurpose : uint16
{
	STRUCTURE_PURPOSE_NONE = 0,
	STRUCTURE_PURPOSE_GENERAL = 1u,
	STRUCTURE_PURPOSE_SIEGE = 1u << 1,
	STRUCTURE_PURPOSE_FORTIFY = 1u << 2,
	STRUCTURE_PURPOSE_BASE = 1u << 3,
	STRUCTURE_PURPOSE_ANY = 0xFFFF
};

inline EAIStructurePurpose operator|(EAIStructurePurpose a, EAIStructurePurpose b)
{
	return static_cast<EAIStructurePurpose>(static_cast<uint16>(a) | static_cast<uint16>(b));
}

inline EAIStructurePurpose operator&(EAIStructurePurpose a, EAIStructurePurpose b)
{
	return static_cast<EAIStructurePurpose>(static_cast<uint16>(a) & static_cast<uint16>(b));
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

enum class EAIBuildAttemptResult
{
	BUILD_ATTEMPT_NONE = 0,
	BUILD_ATTEMPT_PENDING,
	BUILD_ATTEMPT_SUCCESS,
	BUILD_ATTEMPT_FAILED
};

enum class EAIMovementTaskType
{
	MOVE_TASK_NONE = 0,
	MOVE_TASK_MOVE,
	MOVE_TASK_USE,
	MOVE_TASK_BREAK,
	MOVE_TASK_TOUCH,
	MOVE_TASK_PICKUP,
	MOVE_TASK_WELD
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
	AvHPlayer* PlayerRef = nullptr; // Reference to the enemy AvHPlayer
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