//
// EvoBot - Neoptolemus' Natural Selection bot, based on Botman's HPB bot template
//
// AvHAINavConstants.h
//
// Contains global definitions for navmesh and navigation
//

#pragma once

#ifndef AVH_AI_NAV_CONSTANTS_H
#define AVH_AI_NAV_CONSTANTS_H

#include <vector>
#include <dlls/extdll.h>
#include <dlls/util.h>
#include "DetourNavMeshQuery.h"
#include "AvHAIHelper.h"

// How far a bot can be from a useable object when trying to interact with it. Used also for melee attacks. We make it slightly less than actual to avoid edge cases
constexpr float max_ai_use_reach = 55.0f;

// Minimum time a bot can wait between attempts to use something in seconds (when not holding the use key down)
constexpr float min_ai_use_interval = 0.5f;

// Minimum time a bot can wait between attempts to use something in seconds (when not holding the use key down)
constexpr float max_ai_jump_height = 62.0f;

// Max nav mesh polys that can be traversed in a path. This should be sufficient for any sized map.
constexpr auto MAX_PATH_POLY = 512;

constexpr int NAVMESHSET_MAGIC = 'M' << 24 | 'S' << 16 | 'E' << 8 | 'T'; //'MSET', used to confirm the nav mesh we're loading is compatible;
constexpr int NAVMESHSET_VERSION = 1;

constexpr int TILECACHESET_MAGIC = 'T' << 24 | 'S' << 16 | 'E' << 8 | 'T'; //'TSET', used to confirm the tile cache we're loading is compatible;
constexpr int TILECACHESET_VERSION = 4;

constexpr int DT_AREA_NULL = 0; // Represents a null area on the nav mesh. Not traversable and considered not on the nav mesh
constexpr int DT_AREA_BLOCKED = 3; // Area occupied by an obstruction (e.g. building). Not traversable, but considered to be on the nav mesh

constexpr float dtDefaultProjectionExtents[3] = { 400.0f, 50.0f, 400.0f }; // Default extents (in GoldSrc units) to find the nearest spot on the nav mesh
constexpr float dtDefaultReachableExtents[3] = { max_ai_use_reach, max_ai_use_reach, max_ai_use_reach }; // Extents (in GoldSrc units) to determine if something is on the nav mesh
static const Vector DefaultReachableExtents = Vector(max_ai_use_reach, max_ai_use_reach, max_ai_use_reach); // Extents (in GoldSrc units) to determine if something is on the nav mesh

// Possible movement types. Defines the actions the bot needs to take to traverse this node
enum class EAINavMovementFlag : uint32
{
	NAV_FLAG_NONE = 0,
	NAV_FLAG_DISABLED = 1u << 31,		// Disabled
	NAV_FLAG_WALK = 1u << 0,		// Walk
	NAV_FLAG_CROUCH = 1u << 1,		// Crouch
	NAV_FLAG_JUMP = 1u << 2,		// Jump
	NAV_FLAG_LADDER = 1u << 3,		// Ladder
	NAV_FLAG_FALL = 1u << 4,		// Fall
	NAV_FLAG_PLATFORM = 1u << 5,		// Platform
	NAV_FLAG_TELEPORT = 1u << 6,		// Teleport
	NAV_FLAG_WALLCLIMB = 1u << 7,		// Wall Climb
	NAV_FLAG_LEAP = 1u << 8,		// Leap
	NAV_FLAG_BLOCKAGE_TEAM1 = 1u << 9,		// Destroy Team 1 Blockage
	NAV_FLAG_BLOCKAGE_TEAM2 = 1u << 10,		// Destroy Team 2 Blockage
	NAV_FLAG_WELD = 1u << 11,		// Weld
	NAV_FLAG_PHASEGATE_TEAM1 = 1u << 12,		// Team 1 Phase Gate
	NAV_FLAG_PHASEGATE_TEAM2 = 1u << 13,		// Team 2 Phase Gate
	NAV_FLAG_FLY = 1u << 14,		// Fly
	NAV_FLAG_ALL = 0xFFFFFFFF		// All flags
};

inline EAINavMovementFlag operator|(EAINavMovementFlag a, EAINavMovementFlag b)
{
	return static_cast<EAINavMovementFlag>(static_cast<uint16>(a) | static_cast<uint16>(b));
}

inline EAINavMovementFlag operator&(EAINavMovementFlag a, EAINavMovementFlag b)
{
	return static_cast<EAINavMovementFlag>(static_cast<uint16>(a) & static_cast<uint16>(b));
}

// Nav hint types
enum class EAINavHintType : uint16
{
	NAV_HINT_BUILD_COMMCHAIR = 1u << 0,		// Place Command Chair
	NAV_HINT_BUILD_INFPORTAL = 1u << 1,		// Place Infantry Portal
	NAV_HINT_BUILD_ARMORY = 1u << 2,		// Place Armory
	NAV_HINT_BUILD_TURRETFACTORY = 1u << 3,		// Place Turret Factory
	NAV_HINT_BUILD_OBSERVATORY = 1u << 4,		// Place Observatory
	NAV_HINT_BUILD_ARMSLAB = 1u << 5,		// Place Arms Lab
	NAV_HINT_BUILD_PROTOTYPELAB = 1u << 6,		// Place Prototype Lab
	NAV_HINT_BUILD_SENTRY = 1u << 7,		// Place Sentry Turret
	NAV_HINT_BUILD_SIEGETURRET = 1u << 8,		// Place Siege Turret
	NAV_HINT_BUILD_PHASEGATE = 1u << 9,		// Place Phase Gate
	NAV_HINT_BUILD_OFFENSE_CHAMBER = 1u << 10,		// Place Offense Chamber
	NAV_HINT_BUILD_DEFENSE_CHAMBER = 1u << 11,		// Place Defense Chamber
	NAV_HINT_BUILD_MOVEMENT_CHAMBER = 1u << 12,		// Place Movement Chamber
	NAV_HINT_SENSORY_CHAMBER = 1u << 13,		// Place Sensory Chamber
	NAV_HINT_ANY = 0xFFFF		// Any hint type
};

inline EAINavHintType operator|(EAINavHintType a, EAINavHintType b)
{
	return static_cast<EAINavHintType>(static_cast<uint16>(a) | static_cast<uint16>(b));
}

inline EAINavHintType operator&(EAINavHintType a, EAINavHintType b)
{
	return static_cast<EAINavHintType>(static_cast<uint16>(a) & static_cast<uint16>(b));
}

// Area types. Defines the cost of movement through an area and which flag to use
enum class EAINavArea : uint16
{
	NAV_AREA_NULL = 0,		// Null area, cuts a hole in the mesh
	NAV_AREA_UNWALKABLE = 60,		// Unwalkable
	NAV_AREA_WALK = 1,		// Walk
	NAV_AREA_CROUCH = 2,		// Crouch
	NAV_AREA_OBSTRUCTED = 3,		// Obstructed
	NAV_AREA_HAZARD = 4,		// Hazard
	NAV_AREA_TELEPORT = 5,		// Teleport
	NAV_AREA_BLOCKAGE_TEAM1 = 6,		// Team 1 Structure Blockage
	NAV_AREA_BLOCKAGE_TEAM2 = 7,		// Team 2 Structure Blockage
	NAV_AREA_WELDABLE = 8,		// Weldable
	NAV_AREA_WALLCLIMB = 9,		// Wall Climb
	NAV_AREA_LADDER = 10,		// Ladder
};

// Area types. Defines the cost of movement through an area and which flag to use
enum class EAINavMoveResult : uint8
{
	NAV_MOVE_SUCCESS = 0,  // Succeeded in moving this tick
	NAV_MOVE_NOPATH,       // Could not generate a path to the destination
	NAV_MOVE_OFFPATH,      // Bot has unfortunately fallen off the path somehow
	NAV_MOVE_STUCK,        // Has a path but is blocked by something
	NAV_MOVE_NOTASK,       // No task to pursue
	NAV_MOVE_INVALIDTASK,       // No task to pursue
	NAV_MOVE_PATH_COMPLETE // Path is fully completed, no more to do
};

// Profile indices. Use these when retrieving base agent profile information
enum class EAINavProfileIndex : uint16
{
	NAV_PROFILE_MARINE = 0,		// Marine
	NAV_PROFILE_SKULK = 1,		// Skulk
	NAV_PROFILE_GORGE = 2,		// Gorge
	NAV_PROFILE_LERK = 3,		// Lerk
	NAV_PROFILE_FADE = 4,		// Fade
	NAV_PROFILE_ONOS = 5,		// Onos
	NAV_PROFILE_CONSTRUCTION = 6, // Profile for determining structure placement
	NAV_PROFILE_DEFAULT = 7,		// Default profile which has all capabilities except disabled flags, and 1.0 area costs for everything
};

// Profile indices. Use these when retrieving base agent profile information
enum class EAINavMeshIndex : uint8
{
	NAV_MESH_REGULAR = 0,		// Regular Nav Mesh
	NAV_MESH_ONOS = 1,		// Onos Nav Mesh
	NAV_MESH_CONSTRUCTION = 2,		// Construction Nav Mesh

	NAV_MESH_INVALID = 3
};

// Agent profile definition. Holds all information an agent needs when querying the nav mesh
struct NavAgentProfile
{
	EAINavMeshIndex MeshIndex = EAINavMeshIndex::NAV_MESH_INVALID;
	class dtQueryFilter Filters;
	bool bFlyingProfile = false;
	enum_hull PlayerHullIndex = human_hull;

	NavAgentProfile() = default;

	NavAgentProfile(EAINavMeshIndex InMeshIndex, EAINavMovementFlag InFlags, bool bInFlyingProfile)
		: MeshIndex(InMeshIndex)
		, bFlyingProfile(bInFlyingProfile)
	{
		Filters.setExcludeFlags(0);
		Filters.setIncludeFlags(static_cast<unsigned int>(InFlags));
	}

	bool IsValid() const
	{
		return MeshIndex != EAINavMeshIndex::NAV_MESH_INVALID;
	}
};

inline bool IsValidNavMeshIndex(int CheckIndex)
{
	return CheckIndex >= static_cast<int>(EAINavMeshIndex::NAV_MESH_REGULAR)
		&& CheckIndex < static_cast<int>(EAINavMeshIndex::NAV_MESH_INVALID);
}

// Hints are locations placed on the nav mesh to influence and guide the bot. For example, "good ambush point".
// See the nav constants header for all nav hint types.
struct NavHint
{
	EAINavMeshIndex NavMeshIndex = EAINavMeshIndex::NAV_MESH_INVALID;
	unsigned int HintTypes = 0;
	Vector Position;
};
typedef std::vector<NavHint> NavHintList;

struct NavOffMeshConnection
{
	EAINavMeshIndex NavMeshIndex = EAINavMeshIndex::NAV_MESH_INVALID;
	Vector FromLocation = g_vecZero; // The start point of the connection
	Vector ToLocation = g_vecZero; // The end point of the connection
	EAINavMovementFlag ConnectionFlags = EAINavMovementFlag::NAV_FLAG_DISABLED; // The type of connection it is
	EAINavMovementFlag DefaultConnectionFlags = EAINavMovementFlag::NAV_FLAG_DISABLED; // If this connection is being temporarily modified, what it should normally be
	unsigned int ConnectionRef = 0; // References to this connection on all defined nav meshes
	edict_t* LinkedObject = nullptr;

	bool IsValid() const
	{
		return ConnectionRef > 0 && IsValidNavMeshIndex(static_cast<int>(NavMeshIndex));
	}
};
typedef std::vector<NavOffMeshConnection> OffMeshConnectionList;

// A temporary obstacle is a shape placed on the map during play which affects the area it covers, changing the movement flags on it.
// For example, a temporary obstacle with an area type of NULL would cut a hole in the nav mesh, e.g. a door is permanently welded shut.
// Can also be later removed to undo the change, hence "temporary" obstacle.
struct NavTempObstacle
{
	EAINavMeshIndex NavMeshIndex = EAINavMeshIndex::NAV_MESH_INVALID; // Which nav mesh this obstacle belongs to
	Vector Location = g_vecZero; // The location of the obstacle. This will be at the BASE of the cylinder
	float Radius = 0.0f; // How wide the cylindrical obstacle is
	float Height = 0.0f; // How tall the cylinder is
	EAINavArea Area = EAINavArea::NAV_AREA_NULL; // The area to mark on the nav mesh
	unsigned int ObstacleRef = 0; // The reference to the obstacle within Detour

	bool IsValid()
	{
		return IsValidNavMeshIndex(static_cast<int>(NavMeshIndex)) && ObstacleRef > 0;
	}

	void Clear()
	{
		NavMeshIndex = EAINavMeshIndex::NAV_MESH_INVALID;
		Area = EAINavArea::NAV_AREA_NULL;
		ObstacleRef = 0;
	}
};
typedef std::vector<NavTempObstacle> NavTempObstacleList;

// List of base agent profiles
std::vector<NavAgentProfile> BaseAgentProfiles;

// Retrieve appropriate flag for area (See process() in the MeshProcess struct)
inline EAINavMovementFlag GetFlagForArea(EAINavArea Area)
{
	switch (Area)
	{
		case EAINavArea::NAV_AREA_UNWALKABLE:
			return EAINavMovementFlag::NAV_FLAG_DISABLED;
		case EAINavArea::NAV_AREA_WALK:
			return EAINavMovementFlag::NAV_FLAG_WALK;
		case EAINavArea::NAV_AREA_CROUCH:
			return EAINavMovementFlag::NAV_FLAG_CROUCH;
		case EAINavArea::NAV_AREA_OBSTRUCTED:
			return EAINavMovementFlag::NAV_FLAG_JUMP;
		case EAINavArea::NAV_AREA_HAZARD:
			return EAINavMovementFlag::NAV_FLAG_WALK;
		case EAINavArea::NAV_AREA_TELEPORT:
			return EAINavMovementFlag::NAV_FLAG_TELEPORT;
		case EAINavArea::NAV_AREA_BLOCKAGE_TEAM1:
			return EAINavMovementFlag::NAV_FLAG_BLOCKAGE_TEAM1;
		case EAINavArea::NAV_AREA_BLOCKAGE_TEAM2:
			return EAINavMovementFlag::NAV_FLAG_BLOCKAGE_TEAM2;
		case EAINavArea::NAV_AREA_WELDABLE:
			return EAINavMovementFlag::NAV_FLAG_WELD;
		case EAINavArea::NAV_AREA_WALLCLIMB:
			return EAINavMovementFlag::NAV_FLAG_WALLCLIMB;
		case EAINavArea::NAV_AREA_LADDER:
			return EAINavMovementFlag::NAV_FLAG_LADDER;
		default:
			return EAINavMovementFlag::NAV_FLAG_DISABLED;
	}
}

// Get appropriate debug colour for the area. Returns RGB as 3 unsigned chars encoded into a single unsigned int
inline void GetDebugColorForArea(EAINavArea Area, unsigned char& R, unsigned char& G, unsigned char& B)
{
	switch (Area)
	{
		case EAINavArea::NAV_AREA_NULL:
			R = 128;
			G = 128;
			B = 128;
			break;
		case EAINavArea::NAV_AREA_UNWALKABLE:
			R = 10;
			G = 10;
			B = 10;
			break;
		case EAINavArea::NAV_AREA_WALK:
			R = 0;
			G = 192;
			B = 255;
			break;
		case EAINavArea::NAV_AREA_CROUCH:
			R = 9;
			G = 130;
			B = 150;
			break;
		case EAINavArea::NAV_AREA_OBSTRUCTED:
			R = 255;
			G = 64;
			B = 64;
			break;
		case EAINavArea::NAV_AREA_HAZARD:
			R = 192;
			G = 32;
			B = 32;
			break;
		case EAINavArea::NAV_AREA_TELEPORT:
			R = 255;
			G = 255;
			B = 255;
			break;
		case EAINavArea::NAV_AREA_BLOCKAGE_TEAM1:
			R = 255;
			G = 0;
			B = 0;
			break;
		case EAINavArea::NAV_AREA_BLOCKAGE_TEAM2:
			R = 165;
			G = 0;
			B = 52;
			break;
		case EAINavArea::NAV_AREA_WELDABLE:
			R = 205;
			G = 96;
			B = 0;
			break;
		case EAINavArea::NAV_AREA_WALLCLIMB:
			R = 0;
			G = 63;
			B = 0;
			break;
		case EAINavArea::NAV_AREA_LADDER:
			R = 0;
			G = 0;
			B = 159;
			break;
		default:
			R = 255;
			G = 255;
			B = 255;
			break;
	}
}

// Get appropriate debug colour for the movement flag. Returns RGB as 3 unsigned chars encoded into a single unsigned int
inline void GetDebugColorForFlag(EAINavMovementFlag Flag, unsigned char& R, unsigned char& G, unsigned char& B)
{
	switch (Flag)
	{
		case EAINavMovementFlag::NAV_FLAG_DISABLED:
			R = 8;
			G = 8;
			B = 8;
			break;
		case EAINavMovementFlag::NAV_FLAG_WALK:
			R = 255;
			G = 255;
			B = 255;
			break;
		case EAINavMovementFlag::NAV_FLAG_CROUCH:
			R = 9;
			G = 130;
			B = 150;
			break;
		case EAINavMovementFlag::NAV_FLAG_JUMP:
			R = 200;
			G = 200;
			B = 0;
			break;
		case EAINavMovementFlag::NAV_FLAG_LADDER:
			R = 64;
			G = 64;
			B = 255;
			break;
		case EAINavMovementFlag::NAV_FLAG_FALL:
			R = 149;
			G = 0;
			B = 0;
			break;
		case EAINavMovementFlag::NAV_FLAG_PLATFORM:
			R = 255;
			G = 32;
			B = 255;
			break;
		case EAINavMovementFlag::NAV_FLAG_TELEPORT:
			R = 255;
			G = 76;
			B = 68;
			break;
		case EAINavMovementFlag::NAV_FLAG_WALLCLIMB:
			R = 0;
			G = 66;
			B = 0;
			break;
		case EAINavMovementFlag::NAV_FLAG_LEAP:
			R = 255;
			G = 255;
			B = 0;
			break;
		case EAINavMovementFlag::NAV_FLAG_BLOCKAGE_TEAM1:
			R = 209;
			G = 0;
			B = 0;
			break;
		case EAINavMovementFlag::NAV_FLAG_BLOCKAGE_TEAM2:
			R = 227;
			G = 0;
			B = 0;
			break;
		case EAINavMovementFlag::NAV_FLAG_WELD:
			R = 255;
			G = 121;
			B = 0;
			break;
		case EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM1:
			R = 107;
			G = 0;
			B = 85;
			break;
		case EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM2:
			R = 93;
			G = 0;
			B = 80;
			break;
		case EAINavMovementFlag::NAV_FLAG_FLY:
			R = 0;
			G = 156;
			B = 255;
			break;
		default:
			R = 255;
			G = 255;
			B = 255;
			break;
	}
}

// Return name of a flag for debugging purposes
inline void GetFlagName(EAINavMovementFlag Flag, char* outName)
{
	if (!outName) { return; }

	switch (Flag)
	{
		case EAINavMovementFlag::NAV_FLAG_DISABLED:
			sprintf(outName, "Disabled");
			break;
		case EAINavMovementFlag::NAV_FLAG_WALK:
			sprintf(outName, "Walk");
			break;
		case EAINavMovementFlag::NAV_FLAG_CROUCH:
			sprintf(outName, "Crouch");
			break;
		case EAINavMovementFlag::NAV_FLAG_JUMP:
			sprintf(outName, "Jump");
			break;
		case EAINavMovementFlag::NAV_FLAG_LADDER:
			sprintf(outName, "Ladder");
			break;
		case EAINavMovementFlag::NAV_FLAG_FALL:
			sprintf(outName, "Fall");
			break;
		case EAINavMovementFlag::NAV_FLAG_PLATFORM:
			sprintf(outName, "Platform");
			break;
		case EAINavMovementFlag::NAV_FLAG_TELEPORT:
			sprintf(outName, "Teleport");
			break;
		case EAINavMovementFlag::NAV_FLAG_WALLCLIMB:
			sprintf(outName, "Wall Climb");
			break;
		case EAINavMovementFlag::NAV_FLAG_LEAP:
			sprintf(outName, "Leap");
			break;
		case EAINavMovementFlag::NAV_FLAG_BLOCKAGE_TEAM1:
			sprintf(outName, "Destroy Team 1 Blockage");
			break;
		case EAINavMovementFlag::NAV_FLAG_BLOCKAGE_TEAM2:
			sprintf(outName, "Destroy Team 2 Blockage");
			break;
		case EAINavMovementFlag::NAV_FLAG_WELD:
			sprintf(outName, "Weld");
			break;
		case EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM1:
			sprintf(outName, "Team 1 Phase Gate");
			break;
		case EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM2:
			sprintf(outName, "Team 2 Phase Gate");
			break;
		case EAINavMovementFlag::NAV_FLAG_FLY:
			sprintf(outName, "Fly");
			break;
		default:
			sprintf(outName, "Undefined");
			break;
	}
}

// Returns true if this flag is a teleport move (i.e. not affected by doors or other obstacles)
inline bool IsFlagTeleportType(EAINavMovementFlag Flag)
{
	switch (Flag)
	{
		case EAINavMovementFlag::NAV_FLAG_TELEPORT:
		case EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM1:
		case EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM2:
			return true;
		default:
			return false;
	}
}

// Return name of a flag for debugging purposes
inline void GetAreaName(EAINavArea Area, char* outName)
{
	if (!outName) { return; }

	switch (Area)
	{
		case EAINavArea::NAV_AREA_UNWALKABLE:
			sprintf(outName, "Unwalkable");
			break;
		case EAINavArea::NAV_AREA_WALK:
			sprintf(outName, "Walk");
			break;
		case EAINavArea::NAV_AREA_CROUCH:
			sprintf(outName, "Crouch");
			break;
		case EAINavArea::NAV_AREA_OBSTRUCTED:
			sprintf(outName, "Obstructed");
			break;
		case EAINavArea::NAV_AREA_HAZARD:
			sprintf(outName, "Hazard");
			break;
		case EAINavArea::NAV_AREA_TELEPORT:
			sprintf(outName, "Teleport");
			break;
		case EAINavArea::NAV_AREA_BLOCKAGE_TEAM1:
			sprintf(outName, "Team 1 Structure Blockage");
			break;
		case EAINavArea::NAV_AREA_BLOCKAGE_TEAM2:
			sprintf(outName, "Team 2 Structure Blockage");
			break;
		case EAINavArea::NAV_AREA_WELDABLE:
			sprintf(outName, "Weldable");
			break;
		case EAINavArea::NAV_AREA_WALLCLIMB:
			sprintf(outName, "Wall Climb");
			break;
		case EAINavArea::NAV_AREA_LADDER:
			sprintf(outName, "Ladder");
			break;
		default:
			sprintf(outName, "Undefined");
			break;
	}
}

// Populate the base nav profiles. Should be called once after loading the navigation data
inline void PopulateBaseAgentProfiles()
{
	BaseAgentProfiles.clear();

	NavAgentProfile NewProfile0;
	NewProfile0.MeshIndex = EAINavMeshIndex::NAV_MESH_REGULAR;
	NewProfile0.Filters.setIncludeFlags(127);
	NewProfile0.Filters.setExcludeFlags(static_cast<unsigned int>(EAINavMovementFlag::NAV_FLAG_DISABLED));
	NewProfile0.Filters.setAreaCost(0, 0.0);
	NewProfile0.Filters.setAreaCost(1, 1.0);
	NewProfile0.Filters.setAreaCost(2, 2.0);
	NewProfile0.Filters.setAreaCost(3, 2.0);
	NewProfile0.Filters.setAreaCost(4, 5.0);
	NewProfile0.Filters.setAreaCost(5, 0.1);
	NewProfile0.Filters.setAreaCost(6, 20.0);
	NewProfile0.Filters.setAreaCost(7, 20.0);
	NewProfile0.Filters.setAreaCost(8, 2.0);
	NewProfile0.Filters.setAreaCost(9, 1.0);
	NewProfile0.Filters.setAreaCost(10, 1.0);
	BaseAgentProfiles.push_back(NewProfile0);

	NavAgentProfile NewProfile1;
	NewProfile1.MeshIndex = EAINavMeshIndex::NAV_MESH_REGULAR;
	NewProfile1.Filters.setIncludeFlags(255);
	NewProfile1.Filters.setExcludeFlags(static_cast<unsigned int>(EAINavMovementFlag::NAV_FLAG_DISABLED));
	NewProfile1.Filters.setAreaCost(0, 0.0);
	NewProfile1.Filters.setAreaCost(1, 1.0);
	NewProfile1.Filters.setAreaCost(2, 1.0);
	NewProfile1.Filters.setAreaCost(3, 2.0);
	NewProfile1.Filters.setAreaCost(4, 5.0);
	NewProfile1.Filters.setAreaCost(5, 0.1);
	NewProfile1.Filters.setAreaCost(6, 20.0);
	NewProfile1.Filters.setAreaCost(7, 20.0);
	NewProfile1.Filters.setAreaCost(8, 1.0);
	NewProfile1.Filters.setAreaCost(9, 1.0);
	NewProfile1.Filters.setAreaCost(10, 1.0);
	BaseAgentProfiles.push_back(NewProfile1);

	NavAgentProfile NewProfile2;
	NewProfile2.MeshIndex = EAINavMeshIndex::NAV_MESH_REGULAR;
	NewProfile2.Filters.setIncludeFlags(127);
	NewProfile2.Filters.setExcludeFlags(static_cast<unsigned int>(EAINavMovementFlag::NAV_FLAG_DISABLED));
	NewProfile2.Filters.setAreaCost(0, 0.0);
	NewProfile2.Filters.setAreaCost(1, 1.0);
	NewProfile2.Filters.setAreaCost(2, 1.0);
	NewProfile2.Filters.setAreaCost(3, 3.0);
	NewProfile2.Filters.setAreaCost(4, 5.0);
	NewProfile2.Filters.setAreaCost(5, 0.1);
	NewProfile2.Filters.setAreaCost(6, 20.0);
	NewProfile2.Filters.setAreaCost(7, 20.0);
	NewProfile2.Filters.setAreaCost(8, 1.0);
	NewProfile2.Filters.setAreaCost(9, 1.0);
	NewProfile2.Filters.setAreaCost(10, 1.0);
	BaseAgentProfiles.push_back(NewProfile2);

	NavAgentProfile NewProfile3;
	NewProfile3.MeshIndex = EAINavMeshIndex::NAV_MESH_REGULAR;
	NewProfile3.Filters.setIncludeFlags(16895);
	NewProfile3.Filters.setExcludeFlags(static_cast<unsigned int>(EAINavMovementFlag::NAV_FLAG_DISABLED));
	NewProfile3.Filters.setAreaCost(0, 0.0);
	NewProfile3.Filters.setAreaCost(1, 1.0);
	NewProfile3.Filters.setAreaCost(2, 1.0);
	NewProfile3.Filters.setAreaCost(3, 2.0);
	NewProfile3.Filters.setAreaCost(4, 5.0);
	NewProfile3.Filters.setAreaCost(5, 0.1);
	NewProfile3.Filters.setAreaCost(6, 20.0);
	NewProfile3.Filters.setAreaCost(7, 20.0);
	NewProfile3.Filters.setAreaCost(8, 1.0);
	NewProfile3.Filters.setAreaCost(9, 1.0);
	NewProfile3.Filters.setAreaCost(10, 1.0);
	BaseAgentProfiles.push_back(NewProfile3);

	NavAgentProfile NewProfile4;
	NewProfile4.MeshIndex = EAINavMeshIndex::NAV_MESH_REGULAR;
	NewProfile4.Filters.setIncludeFlags(16767);
	NewProfile4.Filters.setExcludeFlags(static_cast<unsigned int>(EAINavMovementFlag::NAV_FLAG_DISABLED));
	NewProfile4.Filters.setAreaCost(0, 0.0);
	NewProfile4.Filters.setAreaCost(1, 1.0);
	NewProfile4.Filters.setAreaCost(2, 2.0);
	NewProfile4.Filters.setAreaCost(3, 3.0);
	NewProfile4.Filters.setAreaCost(4, 5.0);
	NewProfile4.Filters.setAreaCost(5, 0.1);
	NewProfile4.Filters.setAreaCost(6, 20.0);
	NewProfile4.Filters.setAreaCost(7, 20.0);
	NewProfile4.Filters.setAreaCost(8, 1.0);
	NewProfile4.Filters.setAreaCost(9, 1.0);
	NewProfile4.Filters.setAreaCost(10, 1.0);
	BaseAgentProfiles.push_back(NewProfile4);

	NavAgentProfile NewProfile5;
	NewProfile5.MeshIndex = EAINavMeshIndex::NAV_MESH_ONOS;
	NewProfile5.Filters.setIncludeFlags(127);
	NewProfile5.Filters.setExcludeFlags(static_cast<unsigned int>(EAINavMovementFlag::NAV_FLAG_DISABLED));
	NewProfile5.Filters.setAreaCost(0, 0.0);
	NewProfile5.Filters.setAreaCost(1, 1.0);
	NewProfile5.Filters.setAreaCost(2, 2.0);
	NewProfile5.Filters.setAreaCost(3, 3.0);
	NewProfile5.Filters.setAreaCost(4, 5.0);
	NewProfile5.Filters.setAreaCost(5, 0.1);
	NewProfile5.Filters.setAreaCost(6, 5.0);
	NewProfile5.Filters.setAreaCost(7, 5.0);
	NewProfile5.Filters.setAreaCost(8, 1.0);
	NewProfile5.Filters.setAreaCost(9, 1.0);
	NewProfile5.Filters.setAreaCost(10, 1.0);
	BaseAgentProfiles.push_back(NewProfile5);

	NavAgentProfile NewProfile6;
	NewProfile6.MeshIndex = EAINavMeshIndex::NAV_MESH_CONSTRUCTION;
	NewProfile6.Filters.setIncludeFlags(static_cast<unsigned int>(EAINavMovementFlag::NAV_FLAG_ALL));
	NewProfile6.Filters.setExcludeFlags(static_cast<unsigned int>(EAINavMovementFlag::NAV_FLAG_DISABLED));
	NewProfile6.Filters.setAreaCost(0, 1.0);
	NewProfile6.Filters.setAreaCost(1, 1.0);
	NewProfile6.Filters.setAreaCost(2, 1.0);
	NewProfile6.Filters.setAreaCost(3, 1.0);
	NewProfile6.Filters.setAreaCost(4, 1.0);
	NewProfile6.Filters.setAreaCost(5, 1.0);
	NewProfile6.Filters.setAreaCost(6, 1.0);
	NewProfile6.Filters.setAreaCost(7, 1.0);
	NewProfile6.Filters.setAreaCost(8, 1.0);
	NewProfile6.Filters.setAreaCost(9, 1.0);
	NewProfile6.Filters.setAreaCost(10, 1.0);
	BaseAgentProfiles.push_back(NewProfile6);

	NavAgentProfile DefaultProfile;
	DefaultProfile.MeshIndex = EAINavMeshIndex::NAV_MESH_REGULAR;
	DefaultProfile.Filters.setIncludeFlags(static_cast<unsigned int>(EAINavMovementFlag::NAV_FLAG_ALL));
	DefaultProfile.Filters.setExcludeFlags(static_cast<unsigned int>(EAINavMovementFlag::NAV_FLAG_DISABLED));
	DefaultProfile.Filters.setAreaCost(0, 1.0);
	DefaultProfile.Filters.setAreaCost(1, 1.0);
	DefaultProfile.Filters.setAreaCost(2, 1.0);
	DefaultProfile.Filters.setAreaCost(3, 1.0);
	DefaultProfile.Filters.setAreaCost(4, 1.0);
	DefaultProfile.Filters.setAreaCost(5, 1.0);
	DefaultProfile.Filters.setAreaCost(6, 1.0);
	DefaultProfile.Filters.setAreaCost(7, 1.0);
	DefaultProfile.Filters.setAreaCost(8, 1.0);
	DefaultProfile.Filters.setAreaCost(9, 1.0);
	DefaultProfile.Filters.setAreaCost(10, 1.0);
	BaseAgentProfiles.push_back(DefaultProfile);
}

// Used by Detour for the FindRandomPointInCircle type functions
inline float frand()
{
	return (float)rand() / (float)RAND_MAX;
}

// Converts the input GoldSrc Vector to Detour coordinates and outputs the result in the float[3] OutDetour.
inline void UTIL_VecGoldSrcToDetour(const Vector& GoldSrcVector, float* OutDetour)
{
	if (!OutDetour) { return; }

	OutDetour[0] = GoldSrcVector.x;
	OutDetour[1] = GoldSrcVector.z;
	OutDetour[2] = -GoldSrcVector.y;
}

// Returns a GoldSrc Vector from the supplied Detour float[3] coordinates.
inline Vector UTIL_VecDetourToGoldSrc(const float* DetourVector)
{
	if (!DetourVector) { return Vector(0.0f, 0.0f, 0.0f); }

	return Vector(DetourVector[0], -DetourVector[2], DetourVector[1]);
}

// Return the appropriate base nav profile information
inline const NavAgentProfile* GetBaseAgentProfile(const EAINavProfileIndex Index)
{
	unsigned int NavIndex = static_cast<unsigned int>(Index);

	if (NavIndex > static_cast<unsigned int>(EAINavProfileIndex::NAV_PROFILE_DEFAULT)) { return nullptr; }

	return &BaseAgentProfiles[NavIndex];
}

#endif