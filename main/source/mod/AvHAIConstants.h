#pragma once

#ifndef AVH_AI_CONSTANTS_H
#define AVH_AI_CONSTANTS_H

#include "DetourStatus.h"
#include "DetourNavMeshQuery.h"

#include "AvHHive.h"
#include "AvHEntities.h"
#include "AvHAIMath.h"
#include "AvHAINavConstants.h"

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
enum class EAIHiveStatusType
{
	HIVE_STATUS_UNBUILT = 0,
	HIVE_STATUS_BUILDING = 1,
	HIVE_STATUS_BUILT = 2
};

// All tech statuses that can be assigned to a hive
enum class EAIHiveTechStatus
{
	HIVE_TECH_NONE = 0, // Hive doesn't have any tech assigned to it yet (no chambers built for it)
	HIVE_TECH_DEFENCE = 1,
	HIVE_TECH_SENSORY = 2,
	HIVE_TECH_MOVEMENT = 3
};

enum class EAIReachabilityStatus : uint16
{
	AI_REACHABILITY_NONE = 0,
	AI_REACHABILITY_MARINE = 1u << 0,
	AI_REACHABILITY_SKULK = 1u << 1,
	AI_REACHABILITY_GORGE = 1u << 2,
	AI_REACHABILITY_ONOS = 1u << 3,
	AI_REACHABILITY_WELDER = 1u << 4,
	AI_REACHABILITY_UNREACHABLE = 1u << 5,

	AI_REACHABILITY_ALL = -1
};

inline EAIReachabilityStatus operator|(EAIReachabilityStatus a, EAIReachabilityStatus b)
{
	return static_cast<EAIReachabilityStatus>(static_cast<uint16>(a) | static_cast<uint16>(b));
}

inline EAIReachabilityStatus operator&(EAIReachabilityStatus a, EAIReachabilityStatus b)
{
	return static_cast<EAIReachabilityStatus>(static_cast<uint16>(a) & static_cast<uint16>(b));
}

enum class EAIStructureStatus : uint16
{
	STRUCTURE_STATUS_NONE = 0,					// No filters, all buildings will be returned
	STRUCTURE_STATUS_COMPLETED = 1,				// Structure is fully built
	STRUCTURE_STATUS_ELECTRIFIED = 1 << 1,
	STRUCTURE_STATUS_RECYCLING = 1 << 2,
	STRUCTURE_STATUS_PARASITED = 1 << 3,
	STRUCTURE_STATUS_UNDERATTACK = 1 << 4,
	STRUCTURE_STATUS_RESEARCHING = 1 << 5,
	STRUCTURE_STATUS_DAMAGED = 1 << 6,
	STRUCTURE_STATUS_DISABLED = 1 << 7,		// For marine turrets when there's no TF

	STRUCTURE_STATUS_ALL = -1
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
	STRUCTURE_MARINE_RESTOWER = 1u,
	STRUCTURE_MARINE_INFANTRYPORTAL = 1u << 1,
	STRUCTURE_MARINE_TURRETFACTORY = 1u << 2,
	STRUCTURE_MARINE_ADVTURRETFACTORY = 1u << 3,
	STRUCTURE_MARINE_ARMOURY = 1u << 4,
	STRUCTURE_MARINE_ADVARMOURY = 1u << 5,
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
	STRUCTURE_ALIEN_DEFENCECHAMBER = 1u << 16,
	STRUCTURE_ALIEN_SENSORYCHAMBER = 1u << 17,
	STRUCTURE_ALIEN_MOVEMENTCHAMBER = 1u << 18,
	STRUCTURE_ALIEN_OFFENCECHAMBER = 1u << 19,

	ALL_MARINE_STRUCTURES = 0xFFF,
	ALL_ALIEN_STRUCTURES = (STRUCTURE_ALIEN_HIVE | STRUCTURE_ALIEN_RESTOWER | STRUCTURE_ALIEN_DEFENCECHAMBER | STRUCTURE_ALIEN_SENSORYCHAMBER | STRUCTURE_ALIEN_MOVEMENTCHAMBER | STRUCTURE_ALIEN_OFFENCECHAMBER),
	ANY_RES_TOWER = (STRUCTURE_MARINE_RESTOWER | STRUCTURE_ALIEN_RESTOWER),

	ALL_STRUCTURES = ((unsigned int)-1 & ~(STRUCTURE_MARINE_DEPLOYEDMINE))
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
	DEPLOYABLE_ITEM_HMG = 1u << 10,
	DEPLOYABLE_ITEM_GRENADELAUNCHER = 1u << 11,

	DEPLOYABLE_ITEM_WEAPONS = 0xF80,
	DEPLOYABLE_ITEM_EQUIPMENT = 0x6,

	DEPLOYABLE_ITEM_ALL = -1
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
	STRUCTURE_PURPOSE_ANY = -1
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

// Bot's role on the team. For marines, this only governs what they do when left to their own devices.
// Marine bots will always listen to orders from the commander regardless of role.
enum class EAIPlayerRole
{
	BOT_ROLE_NONE,			 // No defined role

	// General Roles

	BOT_ROLE_FIND_RESOURCES, // Will hunt for uncapped resource nodes and cap them. Will attack enemy resource towers
	BOT_ROLE_SWEEPER,		 // Defensive role to protect infrastructure and build at base. Will patrol to keep outposts secure
	BOT_ROLE_ASSAULT,		 // Will go to attack the enemy base. In combat mode, used for Fade-focus aliens

	// Marine-only Roles

	BOT_ROLE_COMMAND,		 // Will attempt to take command
	BOT_ROLE_BOMBARDIER,	 // Bot is armed with a GL and wants to wreck your shit. In combat mode, used for Onos-focus aliens

	// Alien-only roles

	BOT_ROLE_BUILDER,		 // Will focus on building chambers and hives. Stays gorge most of the time
	BOT_ROLE_HARASS		 // Focuses on taking down enemy resource nodes and hunting the enemy
};

enum class EAICombatStrategy
{
	COMBAT_STRATEGY_IGNORE = 0, // Don't engage this enemy
	COMBAT_STRATEGY_AMBUSH,		// Set up an ambush for this enemy
	COMBAT_STRATEGY_RETREAT,	// Retreat and find health
	COMBAT_STRATEGY_SKIRMISH,	// Maintain distance, whittle down their health from range and generally be a pain the arse
	COMBAT_STRATEGY_ATTACK		// Attack the enemy
};

// Data structure used to track resource nodes in the map
struct AvHAIResourceNode
{
	AvHFuncResource* ResourceEntity = nullptr;						// The func_resource edict reference
	edict_t* ResourceEdict = nullptr;
	Vector Location = g_vecZero;									// origin of the func_resource edict (not the tower itself)
	bool bIsOccupied = false;										// True if there is any resource tower on it
	AvHTeamNumber OwningTeam = TEAM_IND;							// The team that has currently capped this node (TEAM_IND if none)
	edict_t* ActiveTowerEntity = nullptr;							// Reference to the resource tower edict (if capped)
	bool bIsBaseNode = false;										// Is this a node in the marine base or active alien hive?
	edict_t* ParentHive = nullptr;
	unsigned int TeamAReachabilityFlags = 0;		// Who on team A can reach this node?
	unsigned int TeamBReachabilityFlags = 0;		// Who on team B can reach this node?
	bool bReachabilityMarkedDirty = false;							// Reachability needs to be recalculated
	float NextReachabilityRefreshTime = 0.0f;
};

// Data structure to hold information about each hive in the map
struct AvHAIHiveDefinition
{
	AvHHive* HiveEntity = nullptr;					// Hive entity reference
	edict_t* HiveEdict = nullptr;					// Hive edict reference
	Vector Location = g_vecZero;					// Origin of the hive
	Vector FloorLocation = g_vecZero;				// Some hives are suspended in the air, this is the floor location directly beneath it
	EAIHiveStatusType Status = EAIHiveStatusType::HIVE_STATUS_UNBUILT;	// Can be unbuilt, in progress, or fully built
	AvHMessageID TechStatus = MESSAGE_NULL;			// What tech (if any) is assigned to this hive right now
	bool bIsUnderAttack = false;					// Is the hive currently under attack? Becomes false if not taken damage for more than 10 seconds
	float HealthPercent = 0.0f;						// If the hive is built and active, what its health currently is
	AvHAIResourceNode* HiveResNodeRef = nullptr;	// Which resource node (indexes into ResourceNodes array) belongs to this hive?
	std::vector<NavTempObstacle*> ObstacleRefs;		// When in progress or built, will place an obstacle so bots don't try to walk through it
	float NextFloorLocationCheck = 0.0f;			// When should the closest navigable point to the hive be calculated? Used to delay the check after a hive is built
	AvHTeamNumber OwningTeam = TEAM_IND;			// Which team owns this hive currently (TEAM_IND if empty)
	EAIReachabilityStatus TeamAReachabilityFlags = EAIReachabilityStatus::AI_REACHABILITY_NONE;		// Who on team A can reach this node?
	EAIReachabilityStatus TeamBReachabilityFlags = EAIReachabilityStatus::AI_REACHABILITY_NONE;		// Who on team B can reach this node?
	char HiveName[64] = {'\0'};
};

struct DeployableSearchFilter
{
	EAIStructureType DeployableTypes = EAIStructureType::ALL_STRUCTURES;
	EAIStructureStatus IncludeStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_NONE;
	EAIStructureStatus ExcludeStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_NONE;
	EAIReachabilityStatus ReachabilityFlags = EAIReachabilityStatus::AI_REACHABILITY_NONE;
	float MinSearchRadius = 0.0f;
	float MaxSearchRadius = 0.0f;
	bool bConsiderPhaseDistance = false;
	AvHTeamNumber DeployableTeam = TEAM_IND;
	AvHTeamNumber ReachabilityTeam = TEAM_IND;
	EAIStructurePurpose PurposeFlags = EAIStructurePurpose::STRUCTURE_PURPOSE_ANY;
};

// Pending message a bot wants to say. Allows for a delay in sending a message to simulate typing, or prevent too many messages on the same frame
struct AvHAIBotMsg
{
	char msg[64]; // Message to send
	float SendTime = 0.0f; // When the bot should send this message
	bool bIsPending = false; // Represents a valid pending message
	bool bIsTeamSay = false; // Is this a team-only message?
};

struct AvHAIGuardInfo
{
	Vector GuardLocation = g_vecZero; // What position are we guarding?
	Vector GuardStandPosition = g_vecZero; // Where the bot should stand to guard position (moves around a bit)
	std::vector<Vector> GuardPoints; // All potential areas to watch that an enemy could approach from
	int NumGuardPoints = 0; // How many watch areas there are for the current location
	Vector GuardLookLocation = g_vecZero; // Which area are we currently watching?
	float GuardStartLookTime = 0.0f; // When did we start watching the current area?
	float ThisGuardLookTime = 0.0f; // How long should we watch this area for?
	float ThisGuardStandTime = 0.0f; // How long should we watch this area for?
	float GuardStartStandTime = 0.0f; // How long should we watch this area for?
};

// Data structure to hold information on any kind of buildable structure (hive, resource tower, chamber, marine building etc)
struct AvHAIBuildableStructure
{
	AvHBaseBuildable* EntityRef = nullptr;
	int EntIndex = -1;
	edict_t* edict = nullptr; // Reference to structure edict
	Vector Location = g_vecZero; // origin of the structure edict
	float healthPercent = 0.0f; // Current health of the building
	float lastDamagedTime = 0.0f; // When it was last damaged by something. Used by bots to determine if still needs defending
	EAIStructureType StructureType = EAIStructureType::STRUCTURE_NONE; // Type of structure it is (e.g. hive, comm chair, infantry portal, defence chamber etc.)
	EAIStructureStatus StructureStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_NONE;
	EAIReachabilityStatus TeamAReachabilityFlags = EAIReachabilityStatus::AI_REACHABILITY_NONE;
	EAIReachabilityStatus TeamBReachabilityFlags = EAIReachabilityStatus::AI_REACHABILITY_NONE;
	int LastSeen = 0; // Which refresh cycle was this last seen on? Used to determine if the building has been removed from play
	std::vector<NavTempObstacle*> TempObstacles;
	vector<NavOffMeshConnection*> OffMeshConnections; // References to any off-mesh connections this structure is associated with
	Vector LastSuccessfulCommanderLocation = g_vecZero; // Tracks the last commander view location where it successfully placed or selected the building
	Vector LastSuccessfulCommanderAngle = g_vecZero; // Tracks the last commander input angle ("click" location) used to successfully place or select building
	EAIStructurePurpose Purpose = EAIStructurePurpose::STRUCTURE_PURPOSE_NONE;
	bool bReachabilityMarkedDirty = false; // If true, reachability flags will be recalculated for this structure
	bool bPlacedByHuman = true; // This structure was placed by a human: AI commander will not recycle these unless it absolutely makes sense to

	bool IsValid() { return !FNullEnt(edict) && !edict->free && !(edict->v.flags & EF_NODRAW) && edict->v.deadflag == DEAD_NO; }
	bool IsCompleted() { return (StructureStatusFlags & EAIStructureStatus::STRUCTURE_STATUS_COMPLETED) != EAIStructureStatus::STRUCTURE_STATUS_NONE; }
	bool IsIdle() { return (StructureStatusFlags & EAIStructureStatus::STRUCTURE_STATUS_RESEARCHING) == EAIStructureStatus::STRUCTURE_STATUS_NONE; }

};

// Any kind of pickup that has been dropped either by the commander or by a player
struct AvHAIDroppedItem
{
	edict_t* edict = nullptr; // Reference to the item edict
	Vector Location = g_vecZero; // Origin of the entity
	EAIDeployableItemType ItemType = EAIDeployableItemType::DEPLOYABLE_ITEM_NONE; // Is it a weapon, health pack, ammo pack etc?
	EAIReachabilityStatus TeamAReachabilityFlags = EAIReachabilityStatus::AI_REACHABILITY_NONE;
	EAIReachabilityStatus TeamBReachabilityFlags = EAIReachabilityStatus::AI_REACHABILITY_NONE;
	bool bReachabilityMarkedDirty = false; // Reachability needs to be recalculated
	int LastSeen = 0; // Which refresh cycle was this last seen on? Used to determine if the item has been removed from play

	bool IsValid() { return !FNullEnt(edict) && !edict->free && !(edict->v.flags & EF_NODRAW) && edict->v.deadflag == DEAD_NO; }
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

// The type of base a marine outpost could be. Used to help the AI establish and expand outposts across the map
enum class EAIMarineBaseType
{
	MARINE_BASE_MAINBASE,   // The main marine base, where the CC, infantry portals and stuff like arms labs go
	MARINE_BASE_OUTPOST,    // A permanent outpost designed to control an area of the map, but not the main marine base
	MARINE_BASE_SIEGE,		// A siege base designed to take down an enemy base
	MARINE_BASE_GUARDPOST	// A cut-down version of an outpost with just sentry turrets and an observatory
};

struct AvHAIMarineBase
{
	AvHTeamNumber BaseTeam = TEAM_IND;
	EAIMarineBaseType BaseType = EAIMarineBaseType::MARINE_BASE_OUTPOST; // The purpose of the base. Determines what structures the commander will place
	Vector BaseLocation = ZERO_VECTOR; // Where the base should be located. The base will be grown around this location
	Vector SiegeTarget = ZERO_VECTOR; // For siege bases, this is where the siege base wants to blast stuff
	vector<int> PlacedStructures; // Which structures are part of this base.
	int NumBuilders = 0; // How many potential builders are there, able to construct stuff?
	int NumEnemies = 0; // How many enemies are in and around the base?
	bool bRecycleBase = false;  // Should the commander pack up and remove this base?
	bool bIsActive = true;  // Should the commander actively build and maintain this base?
	bool bBaseInitialised = false; // Has the commander started building this base? Will be true once a structure has been placed
	bool bCanBeBuiltOut = false; // Can this base be built out currently?
	bool bIsBaseEstablished = false; // Have enough key structures been placed to consider this "established", even if it's not finished yet?
};

// Bot path node. A path will be several of these strung together to lead the bot to its destination
struct AvHAIPathNode
{
	Vector FromLocation = ZERO_VECTOR; // Location to move from
	Vector ToLocation = ZERO_VECTOR; // Location to move to
	float requiredZ = 0.0f; // If climbing a up ladder or wall, how high should they aim to get before dismounting.
	unsigned int flag = NAV_FLAG_DISABLED; // Is this a ladder movement, wall climb, walk etc
	unsigned char area = NAV_AREA_UNWALKABLE; // Is this a crouch area, normal walking area etc
	unsigned int poly = 0; // The nav mesh poly this point resides on
	edict_t* Platform = nullptr;
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

// Tracks what orders have been given to which players
struct AvHAISkillLevel
{
	float marine_bot_reaction_time = 0.2f; // How quickly the bot will react to seeing an enemy
	float marine_bot_aim_skill = 0.5f; // How quickly the bot can lock on to an enemy
	float marine_bot_motion_tracking_skill = 0.5f; // How well the bot can follow an enemy target's motion
	float marine_bot_view_speed = 1.0f; // How fast a bot can spin its view to aim in a given direction
	float alien_bot_reaction_time = 0.2f; // How quickly the bot will react to seeing an enemy
	float alien_bot_aim_skill = 0.5f; // How quickly the bot can lock on to an enemy
	float alien_bot_motion_tracking_skill = 0.5f; // How well the bot can follow an enemy target's motion
	float alien_bot_view_speed = 0.5f; // How fast a bot can spin its view to aim in a given direction

};

struct AvHAIBuildAttempt
{
	EAIStructureType AttemptedStructureType = EAIStructureType::STRUCTURE_NONE;
	Vector AttemptedLocation = g_vecZero;
	int NumAttempts = 0;
	EAIBuildAttemptResult BuildStatus = EAIBuildAttemptResult::BUILD_ATTEMPT_NONE;
	float BuildAttemptTime = 0.0f;
	AvHAIBuildableStructure* LinkedStructure = nullptr;
};

// A bot task is a goal the bot wants to perform, such as attacking a structure, placing a structure etc. NOT USED BY COMMANDER
struct AvHAIPlayerTask
{
	EAITaskType TaskType = EAITaskType::TASK_NONE; // Task Type (e.g. build, attack, defend, heal etc)
	Vector TaskLocation = g_vecZero; // Task location, if task needs one (e.g. where to place structure for TASK_BUILD)
	edict_t* TaskTarget = nullptr; // Reference to a target, if task needs one (e.g. TASK_ATTACK)
	edict_t* TaskSecondaryTarget = nullptr; // Secondary target, if task needs one (e.g. TASK_REINFORCE)
	EAIStructureType StructureType = EAIStructureType::STRUCTURE_NONE; // For Gorges, what structure to build (if TASK_BUILD)
	float TaskStartedTime = 0.0f; // When the bot started this task. Helps time-out if the bot gets stuck trying to complete it
	bool bIssuedByCommander = false; // Was this task issued by the commander? Top priority if so
	bool bTargetIsPlayer = false; // Is the TaskTarget a player?
	bool bTaskIsUrgent = false; // Determines whether this task is prioritised over others if bot has multiple
	bool bIsWaitingForBuildLink = false; // If true, Gorge has sent the build impulse and is waiting to see if the building materialised
	float LastBuildAttemptTime = 0.0f; // When did the Gorge last try to place a structure?
	int BuildAttempts = 0; // How many attempts the Gorge has tried to place it, so it doesn't keep trying forever
	AvHMessageID Evolution = MESSAGE_NULL; // Used by TASK_EVOLVE to determine what to evolve into
	float TaskLength = 0.0f; // If a task has gone on longer than this time, it will be considered completed
	AvHAIBuildAttempt ActiveBuildInfo; // If gorge, the current status of any recent attempt to place a structure
};

struct AvHAIMoveTask
{
	EAIMovementTaskType TaskType = EAIMovementTaskType::MOVE_TASK_NONE;
	Vector TaskLocation = ZERO_VECTOR;
	edict_t* TaskTarget = nullptr;
	edict_t* TriggerToActivate = nullptr;
	bool bPathGenerated = false;
};

struct AvHAIStuckTracker
{
	Vector LastBotPosition = g_vecZero;
	Vector MoveDestination = g_vecZero;
	float TotalStuckTime = 0.0f; // Total time the bot has spent stuck
	bool bPathFollowFailed = false;

};

// Contains the bot's current navigation info, such as current path
struct AvHAINavStatus
{
	std::vector<AvHAIPathNode> CurrentPath; // Bot's path nodes
	unsigned int CurrentPathPoint = 0;

	Vector TargetDestination = ZERO_VECTOR; // Desired destination
	Vector ActualMoveDestination = ZERO_VECTOR; // Actual destination on nav mesh
	Vector PathDestination = ZERO_VECTOR; // Where the path is currently headed to

	Vector LastNavMeshCheckPosition = ZERO_VECTOR;
	Vector LastNavMeshPosition = ZERO_VECTOR; // Tracks the last place the bot was on the nav mesh. Useful if accidentally straying off it
	Vector LastOpenLocation = ZERO_VECTOR; // Tracks the last place the bot had enough room to move around people. Useful if in a vent and need to back up somewhere to let another player past.

	int CurrentMoveType = MOVETYPE_NONE; // Tracks the edict's current movement type

	unsigned int CurrentPoly = 0; // Which nav mesh poly the bot is currently on

	float LastStuckCheckTime = 0.0f; // Last time the bot checked if it had successfully moved
	float TotalStuckTime = 0.0f; // Total time the bot has spent stuck
	float LastDistanceFromDestination = 0.0f; // How far from its destination was it last stuck check

	Vector StuckCheckMoveLocation = ZERO_VECTOR; // Where is the bot trying to go that we're checking if they're stuck?
	Vector UnstuckMoveLocation = ZERO_VECTOR; // If the bot is unable to find a path, blindly move here to try and fix the problem

	float LandedTime = 0.0f; // When the bot last landed after a fall/jump.
	float AirStartedTime = 0.0f; // When the bot left the ground if in the air
	float LeapAttemptedTime = 0.0f; // When the bot last attempted to leap/blink. Avoid spam that sends it flying around too fast
	bool bIsJumping = false; // Is the bot in the air from a jump? Will duck so it can duck-jump
	bool IsOnGround = true; // Is the bot currently on the ground, or on a ladder?
	bool bHasAttemptedJump = false; // Last frame, the bot tried a jump. If the bot is still on the ground, it probably tried to jump in a vent or something
	float LastFlapTime = 0.0f; // When the bot last flapped its wings (if Lerk). Prevents per-frame spam draining adrenaline

	bool bShouldWalk = false; // Should the bot walk at this point?

	EAIMoveStyle PreviousMoveStyle = EAIMoveStyle::MOVESTYLE_NORMAL; // Previous desired move style (e.g. normal, ambush, hide). Will trigger new path calculations if this changes
	EAIMoveStyle MoveStyle = EAIMoveStyle::MOVESTYLE_NORMAL; // Current desired move style (e.g. normal, ambush, hide). Will trigger new path calculations if this changes
	float LastPathCalcTime = 0.0f; // When the bot last calculated a path, to limit how frequently it can recalculate

	float NextForceRecalc = 0.0f; // If set, then the bot will force-recalc its current path

	NavAgentProfile NavProfile;
	bool bNavProfileChanged = false;

	AvHAIStuckTracker StuckInfo;

	unsigned int SpecialMovementFlags = 0; // Any special movement flags required for the current path (e.g. needs to pick up an item)

	std::vector<AvHAIMoveTask> MovementTasks;
	AvHAIMoveTask UnstuckTask;
};

enum class EAIOrderPurpose
{
	ORDERPURPOSE_NONE,
	ORDERPURPOSE_SECURE_HIVE,
	ORDERPURPOSE_SIEGE_HIVE,
	ORDERPURPOSE_SECURE_RESNODE,
	ORDERPURPOSE_BUILD_MAINBASE,
	ORDERPURPOSE_BUILD_SIEGE,
	ORDERPURPOSE_BUILD_OUTPOST,
	ORDERPURPOSE_BUILD_GUARDPOST
};

struct AvHAICommanderOrder
{
	edict_t* Assignee = nullptr;
	EAIOrderPurpose OrderPurpose = EAIOrderPurpose::ORDERPURPOSE_NONE;
	edict_t* OrderTarget = nullptr;
	Vector OrderLocation = g_vecZero;
	float LastReminderTime = 0.0f;
	float LastPlayerDistance = 0.0f;
};

struct AvHAICommanderRequest
{
	bool bNewRequest = false; // Is this a new request just come in?
	edict_t* Requestor = nullptr; // Who sent the request?
	AvHMessageID RequestType = MESSAGE_NULL; // What did they request?
	bool bAcknowledged = false; // If we can't satisfy the request right now, have we at least acknowledged it?
	bool bResponded = false; // Have we already responded to this request?
	float RequestTime = 0.0f; // When the request came in
	int ResponseAttempts = 0; // How many times have we tried to respond to this request?
	Vector RequestLocation = g_vecZero; // Where was the request raised? Ideal drop location for stuff
};

struct AvHAIPlayer
{
	AvHPlayer* Player = nullptr;
	edict_t* Edict = nullptr;
	AvHTeamNumber	Team = TEAM_IND;
	float			ForwardMove = 0.0f;
	float			SideMove = 0.0f;
	float			UpMove = 0.0f;
	int				Button = 0.0f;
	int				Impulse = 0.0f;
	byte			AdjustedMsec = 0;

	bool bIsPendingKill = false;
	bool bIsInactive = false;

	float LastUseTime = 0.0f;

	Vector SpawnLocation = g_vecZero;
	Vector DesiredMovementDir = g_vecZero;
	Vector CurrentEyePosition = g_vecZero;
	Vector CurrentFloorPosition = g_vecZero;

	Vector CollisionHullBottomLocation = g_vecZero;
	Vector CollisionHullTopLocation = g_vecZero;

	EAIWeaponId DesiredMoveWeapon = EAIWeaponId::WEAPON_INVALID;
	EAIWeaponId DesiredCombatWeapon = EAIWeaponId::WEAPON_INVALID;

	AvHBotViewFrustumPlane viewFrustum[6]; // Bot's view frustum. Essentially, their "screen" for determining visibility of stuff

	AvHAIEnemyStatus TrackedEnemies[32];
	int CurrentEnemy = -1;
	EAICombatStrategy CurrentCombatStrategy = EAICombatStrategy::COMBAT_STRATEGY_ATTACK;
	edict_t* CurrentEnemyRef = nullptr;

	vector<AvHAIBuildableStructure> DangerTurrets;

	AvHAIPlayerTask PrimaryBotTask;
	AvHAIPlayerTask SecondaryBotTask;
	AvHAIPlayerTask WantsAndNeedsTask;
	AvHAIPlayerTask CommanderTask; // Task assigned by the commander
	AvHAIPlayerTask* CurrentTask = &PrimaryBotTask; // Bot's current task they're performing

	float BotNextTaskEvaluationTime = 0.0f;

	AvHAISkillLevel BotSkillSettings;

	char PathStatus[128]; // Debug used to help figure out what's going on with a bot's path finding
	char MoveStatus[128]; // Debug used to help figure out what's going on with a bot's steering

	AvHAINavStatus BotNavInfo; // Bot's movement information, their current path, where in the path they are etc.

	vector<AvHAICommanderRequest> ActiveRequests;
	vector<AvHAICommanderOrder> ActiveOrders;

	float next_commander_action_time = 0.0f;

	AvHAIBotMsg ChatMessages[5]; // Bot can have up to 5 chat messages pending

	float LastCombatTime = 0.0f;

	AvHAIGuardInfo GuardInfo;

	float LastRequestTime = 0.0f; // When bot last used a voice line to request something. Prevents spam

	float LastTeleportTime = 0.0f; // Last time the bot teleported somewhere

	Vector DesiredLookDirection = g_vecZero; // What view angle is the bot currently turning towards
	Vector InterpolatedLookDirection = g_vecZero; // Used to smoothly interpolate the bot's view rather than snap instantly like an aimbot
	edict_t* LookTarget = nullptr; // Used to work out what view angle is needed to look at the desired entity
	Vector LookTargetLocation = g_vecZero; // This is the bot's current desired look target. Could be an enemy (see LookTarget), or point of interest
	Vector MoveLookLocation = g_vecZero; // If the bot has to look somewhere specific for movement (e.g. up for a ladder or wall-climb), this will override LookTargetLocation so the bot doesn't get distracted and mess the move up
	bool bSnapView = false; // Use for rapid, precise snapping of the bot's view to the target. Useful if the bot requires more precise view angles for movement or other reasons
	float LastTargetTrackUpdate = 0.0f; // Add a delay to how frequently a bot can track a target's movements
	float ViewInterpolationSpeed = 0.0f; // How fast should the bot turn its view? Depends on distance to turn
	float ViewInterpStartedTime = 0.0f; // Used for interpolation

	float ViewUpdateRate = 0.2f; // How frequently the bot can react to new sightings of enemies etc.
	float LastViewUpdateTime = 0.0f; // Used to throttle view updates based on ViewUpdateRate

	Vector ViewForwardVector = g_vecZero; // Bot's current forward unit vector
	Vector LastSafeLocation = g_vecZero;

	EAIPlayerRole BotRole = EAIPlayerRole::BOT_ROLE_NONE;

	int ExperiencePointsAvailable = 0; // How much experience the bot has to spend
	AvHMessageID NextCombatModeUpgrade = MESSAGE_NULL;

	float ThinkDelta = 0.0f; // How long since this bot last ran AIPlayerThink
	float LastThinkTime = 0.0f; // When the bot last ran AIPlayerThink

	float ServerUpdateDelta = 0.0f; // How long since we last called RunPlayerMove
	float LastServerUpdateTime = 0.0f; // When we last called RunPlayerMove

	float HearingThreshold = 0.0f; // How loud does a sound need to be before the bot detects it? This is set when hearing a sound so that louder sounds drown out quieter ones, and decrements quickly

	int DebugValue = 0; // Used for debugging the bot

	Vector RelocationSpot = ZERO_VECTOR; // If the bot is commanding and wants to relocate, then this is where they plan to go

	vector<AvHAIMarineBase> Bases;

};

struct AvHAISquad
{
	AvHTeamNumber SquadTeam = TEAM_IND; // Which team this squad is for
	vector<AvHAIPlayer*> SquadMembers; // Which bots are assigned to this
	Vector SquadGatherLocation = g_vecZero; // Where should the squad gather before attempting the objective?
	edict_t* SquadTarget = nullptr; // The target of the objective
	Vector ObjectiveLocation = g_vecZero;
	EAITaskType SquadObjective = EAITaskType::TASK_NONE; // What to do with the objective
	bool bExecuteObjective = false; // Are we at the gather or execute phase?

	bool IsValid()
	{
		return (SquadMembers.size() > 0 && !FNullEnt(SquadTarget));
	}
};


#endif