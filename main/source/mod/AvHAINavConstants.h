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
#include "DetourNavMeshQuery.h"

// How far a bot can be from a useable object when trying to interact with it. Used also for melee attacks. We make it slightly less than actual to avoid edge cases
static const float max_ai_use_reach = 55.0f;

// Minimum time a bot can wait between attempts to use something in seconds (when not holding the use key down)
static const float min_ai_use_interval = 0.5f;

// Minimum time a bot can wait between attempts to use something in seconds (when not holding the use key down)
static const float max_ai_jump_height = 62.0f;

// Possible movement types. Defines the actions the bot needs to take to traverse this node
enum NavMovementFlag
{
	NAV_FLAG_DISABLED = 1 << 31,		// Disabled
	NAV_FLAG_WALK = 1 << 0,		// Walk
	NAV_FLAG_CROUCH = 1 << 1,		// Crouch
	NAV_FLAG_JUMP = 1 << 2,		// Jump
	NAV_FLAG_LADDER = 1 << 3,		// Ladder
	NAV_FLAG_FALL = 1 << 4,		// Fall
	NAV_FLAG_PLATFORM = 1 << 5,		// Platform
	NAV_FLAG_TELEPORT = 1 << 6,		// Teleport
	NAV_FLAG_WALLCLIMB = 1 << 7,		// Wall Climb
	NAV_FLAG_LEAP = 1 << 8,		// Leap
	NAV_FLAG_BLOCKAGE_TEAM1 = 1 << 9,		// Destroy Team 1 Blockage
	NAV_FLAG_BLOCKAGE_TEAM2 = 1 << 10,		// Destroy Team 2 Blockage
	NAV_FLAG_WELD = 1 << 11,		// Weld
	NAV_FLAG_PHASEGATE_TEAM1 = 1 << 12,		// Team 1 Phase Gate
	NAV_FLAG_PHASEGATE_TEAM2 = 1 << 13,		// Team 2 Phase Gate
	NAV_FLAG_FLY = 1 << 14,		// Fly
	NAV_FLAG_ALL = -1		// All flags
};

// Nav hint types
enum NavHintType
{
	NAV_HINT_BUILD_COMMCHAIR = 1 << 0,		// Place Command Chair
	NAV_HINT_BUILD_INFPORTAL = 1 << 1,		// Place Infantry Portal
	NAV_HINT_BUILD_ARMORY = 1 << 2,		// Place Armory
	NAV_HINT_BUILD_TURRETFACTORY = 1 << 3,		// Place Turret Factory
	NAV_HINT_BUILD_OBSERVATORY = 1 << 4,		// Place Observatory
	NAV_HINT_BUILD_ARMSLAB = 1 << 5,		// Place Arms Lab
	NAV_HINT_BUILD_PROTOTYPELAB = 1 << 6,		// Place Prototype Lab
	NAV_HINT_BUILD_SENTRY = 1 << 7,		// Place Sentry Turret
	NAV_HINT_BUILD_SIEGETURRET = 1 << 8,		// Place Siege Turret
	NAV_HINT_BUILD_PHASEGATE = 1 << 9,		// Place Phase Gate
	NAV_HINT_BUILD_OFFENSE_CHAMBER = 1 << 10,		// Place Offense Chamber
	NAV_HINT_BUILD_DEFENSE_CHAMBER = 1 << 11,		// Place Defense Chamber
	NAV_HINT_BUILD_MOVEMENT_CHAMBER = 1 << 12,		// Place Movement Chamber
	NAV_HINT_SENSORY_CHAMBER = 1 << 13,		// Place Sensory Chamber
	NAV_HINT_ANY = -1		// Any hint type
};

// Area types. Defines the cost of movement through an area and which flag to use
enum NavArea
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

// Profile indices. Use these when retrieving base agent profile information
enum NavProfileIndex
{
	NAV_PROFILE_MARINE = 0,		// Marine
	NAV_PROFILE_SKULK = 1,		// Skulk
	NAV_PROFILE_GORGE = 2,		// Gorge
	NAV_PROFILE_LERK = 3,		// Lerk
	NAV_PROFILE_FADE = 4,		// Fade
	NAV_PROFILE_ONOS = 5,		// Onos
	NAV_PROFILE_DEFAULT = 6,		// Default profile which has all capabilities except disabled flags, and 1.0 area costs for everything
};

// Profile indices. Use these when retrieving base agent profile information
enum NavMeshIndex
{
	NAV_MESH_REGULAR = 0,		// Regular Nav Mesh
	NAV_MESH_ONOS = 1,		// Onos Nav Mesh
	NAV_MESH_CONSTRUCTION = 2,		// Construction Nav Mesh

	NAV_MESH_INVALID = 3
};

// Agent profile definition. Holds all information an agent needs when querying the nav mesh
typedef struct _NAV_AGENT_PROFILE
{
	NavMeshIndex MeshIndex = NAV_MESH_INVALID;
	class dtQueryFilter Filters;
	bool bFlyingProfile = false;
} NavAgentProfile;

// Declared in DTNavigation.cpp
// List of base agent profiles
extern std::vector<NavAgentProfile> BaseAgentProfiles;

// Agent profile definition. Holds all information an agent needs when querying the nav mesh
typedef struct _NAV_HINT
{
	unsigned int NavMeshIndex = 0;
	unsigned int HintTypes = 0;
	Vector Position;
} NavHint;

inline bool IsValidNavMeshIndex(int CheckIndex)
{
	return CheckIndex >= static_cast<int>(NAV_MESH_REGULAR)
		&& CheckIndex < static_cast<int>(NAV_MESH_INVALID);
}

// Retrieve appropriate flag for area (See process() in the MeshProcess struct)
inline NavMovementFlag GetFlagForArea(NavArea Area)
{
	switch (Area)
	{
		case NAV_AREA_UNWALKABLE:
			return NAV_FLAG_DISABLED;
		case NAV_AREA_WALK:
			return NAV_FLAG_WALK;
		case NAV_AREA_CROUCH:
			return NAV_FLAG_CROUCH;
		case NAV_AREA_OBSTRUCTED:
			return NAV_FLAG_JUMP;
		case NAV_AREA_HAZARD:
			return NAV_FLAG_WALK;
		case NAV_AREA_TELEPORT:
			return NAV_FLAG_TELEPORT;
		case NAV_AREA_BLOCKAGE_TEAM1:
			return NAV_FLAG_BLOCKAGE_TEAM1;
		case NAV_AREA_BLOCKAGE_TEAM2:
			return NAV_FLAG_BLOCKAGE_TEAM2;
		case NAV_AREA_WELDABLE:
			return NAV_FLAG_WELD;
		case NAV_AREA_WALLCLIMB:
			return NAV_FLAG_WALLCLIMB;
		case NAV_AREA_LADDER:
			return NAV_FLAG_LADDER;
		default:
			return NAV_FLAG_DISABLED;
	}
}

// Get appropriate debug colour for the area. Returns RGB as 3 unsigned chars encoded into a single unsigned int
inline void GetDebugColorForArea(NavArea Area, unsigned char& R, unsigned char& G, unsigned char& B)
{
	switch (Area)
	{
	case NAV_AREA_NULL:
		R = 128;
		G = 128;
		B = 128;
		break;
	case NAV_AREA_UNWALKABLE:
		R = 10;
		G = 10;
		B = 10;
		break;
	case NAV_AREA_WALK:
		R = 0;
		G = 192;
		B = 255;
		break;
	case NAV_AREA_CROUCH:
		R = 9;
		G = 130;
		B = 150;
		break;
	case NAV_AREA_OBSTRUCTED:
		R = 255;
		G = 64;
		B = 64;
		break;
	case NAV_AREA_HAZARD:
		R = 192;
		G = 32;
		B = 32;
		break;
	case NAV_AREA_TELEPORT:
		R = 255;
		G = 255;
		B = 255;
		break;
	case NAV_AREA_BLOCKAGE_TEAM1:
		R = 255;
		G = 0;
		B = 0;
		break;
	case NAV_AREA_BLOCKAGE_TEAM2:
		R = 165;
		G = 0;
		B = 52;
		break;
	case NAV_AREA_WELDABLE:
		R = 205;
		G = 96;
		B = 0;
		break;
	case NAV_AREA_WALLCLIMB:
		R = 0;
		G = 63;
		B = 0;
		break;
	case NAV_AREA_LADDER:
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
inline void GetDebugColorForFlag(NavMovementFlag Flag, unsigned char& R, unsigned char& G, unsigned char& B)
{
	switch (Flag)
	{
	case NAV_FLAG_DISABLED:
		R = 8;
		G = 8;
		B = 8;
		break;
	case NAV_FLAG_WALK:
		R = 255;
		G = 255;
		B = 255;
		break;
	case NAV_FLAG_CROUCH:
		R = 9;
		G = 130;
		B = 150;
		break;
	case NAV_FLAG_JUMP:
		R = 200;
		G = 200;
		B = 0;
		break;
	case NAV_FLAG_LADDER:
		R = 64;
		G = 64;
		B = 255;
		break;
	case NAV_FLAG_FALL:
		R = 149;
		G = 0;
		B = 0;
		break;
	case NAV_FLAG_PLATFORM:
		R = 255;
		G = 32;
		B = 255;
		break;
	case NAV_FLAG_TELEPORT:
		R = 255;
		G = 76;
		B = 68;
		break;
	case NAV_FLAG_WALLCLIMB:
		R = 0;
		G = 66;
		B = 0;
		break;
	case NAV_FLAG_LEAP:
		R = 255;
		G = 255;
		B = 0;
		break;
	case NAV_FLAG_BLOCKAGE_TEAM1:
		R = 209;
		G = 0;
		B = 0;
		break;
	case NAV_FLAG_BLOCKAGE_TEAM2:
		R = 227;
		G = 0;
		B = 0;
		break;
	case NAV_FLAG_WELD:
		R = 255;
		G = 121;
		B = 0;
		break;
	case NAV_FLAG_PHASEGATE_TEAM1:
		R = 107;
		G = 0;
		B = 85;
		break;
	case NAV_FLAG_PHASEGATE_TEAM2:
		R = 93;
		G = 0;
		B = 80;
		break;
	case NAV_FLAG_FLY:
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
inline void GetFlagName(NavMovementFlag Flag, char* outName)
{
	if (!outName) { return; }

	switch (Flag)
	{
		case NAV_FLAG_DISABLED:
			sprintf(outName, "Disabled");
			break;
		case NAV_FLAG_WALK:
			sprintf(outName, "Walk");
			break;
		case NAV_FLAG_CROUCH:
			sprintf(outName, "Crouch");
			break;
		case NAV_FLAG_JUMP:
			sprintf(outName, "Jump");
			break;
		case NAV_FLAG_LADDER:
			sprintf(outName, "Ladder");
			break;
		case NAV_FLAG_FALL:
			sprintf(outName, "Fall");
			break;
		case NAV_FLAG_PLATFORM:
			sprintf(outName, "Platform");
			break;
		case NAV_FLAG_TELEPORT:
			sprintf(outName, "Teleport");
			break;
		case NAV_FLAG_WALLCLIMB:
			sprintf(outName, "Wall Climb");
			break;
		case NAV_FLAG_LEAP:
			sprintf(outName, "Leap");
			break;
		case NAV_FLAG_BLOCKAGE_TEAM1:
			sprintf(outName, "Destroy Team 1 Blockage");
			break;
		case NAV_FLAG_BLOCKAGE_TEAM2:
			sprintf(outName, "Destroy Team 2 Blockage");
			break;
		case NAV_FLAG_WELD:
			sprintf(outName, "Weld");
			break;
		case NAV_FLAG_PHASEGATE_TEAM1:
			sprintf(outName, "Team 1 Phase Gate");
			break;
		case NAV_FLAG_PHASEGATE_TEAM2:
			sprintf(outName, "Team 2 Phase Gate");
			break;
		case NAV_FLAG_FLY:
			sprintf(outName, "Fly");
			break;
		default:
			sprintf(outName, "Undefined");
			break;
	}
}

// Returns true if this flag is a teleport move (i.e. not affected by doors or other obstacles)
inline bool IsFlagTeleportType(NavMovementFlag Flag)
{
	switch (Flag)
	{
	case NAV_FLAG_DISABLED:
		return false;
	case NAV_FLAG_WALK:
		return false;
	case NAV_FLAG_CROUCH:
		return false;
	case NAV_FLAG_JUMP:
		return false;
	case NAV_FLAG_LADDER:
		return false;
	case NAV_FLAG_FALL:
		return false;
	case NAV_FLAG_PLATFORM:
		return false;
	case NAV_FLAG_TELEPORT:
		return true;
	case NAV_FLAG_WALLCLIMB:
		return false;
	case NAV_FLAG_LEAP:
		return false;
	case NAV_FLAG_BLOCKAGE_TEAM1:
		return false;
	case NAV_FLAG_BLOCKAGE_TEAM2:
		return false;
	case NAV_FLAG_WELD:
		return false;
	case NAV_FLAG_PHASEGATE_TEAM1:
		return true;
	case NAV_FLAG_PHASEGATE_TEAM2:
		return true;
	case NAV_FLAG_FLY:
		return false;
	default:
		return false;
	}
}

// Return name of a flag for debugging purposes
inline void GetAreaName(NavArea Area, char* outName)
{
	if (!outName) { return; }

	switch (Area)
	{
	case NAV_AREA_UNWALKABLE:
		sprintf(outName, "Unwalkable");
		break;
	case NAV_AREA_WALK:
		sprintf(outName, "Walk");
		break;
	case NAV_AREA_CROUCH:
		sprintf(outName, "Crouch");
		break;
	case NAV_AREA_OBSTRUCTED:
		sprintf(outName, "Obstructed");
		break;
	case NAV_AREA_HAZARD:
		sprintf(outName, "Hazard");
		break;
	case NAV_AREA_TELEPORT:
		sprintf(outName, "Teleport");
		break;
	case NAV_AREA_BLOCKAGE_TEAM1:
		sprintf(outName, "Team 1 Structure Blockage");
		break;
	case NAV_AREA_BLOCKAGE_TEAM2:
		sprintf(outName, "Team 2 Structure Blockage");
		break;
	case NAV_AREA_WELDABLE:
		sprintf(outName, "Weldable");
		break;
	case NAV_AREA_WALLCLIMB:
		sprintf(outName, "Wall Climb");
		break;
	case NAV_AREA_LADDER:
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
	NewProfile0.MeshIndex = NAV_MESH_REGULAR;
	NewProfile0.Filters.setIncludeFlags(127);
	NewProfile0.Filters.setExcludeFlags(NAV_FLAG_DISABLED);
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
	NewProfile1.MeshIndex = NAV_MESH_REGULAR;
	NewProfile1.Filters.setIncludeFlags(255);
	NewProfile1.Filters.setExcludeFlags(NAV_FLAG_DISABLED);
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
	NewProfile2.MeshIndex = NAV_MESH_REGULAR;
	NewProfile2.Filters.setIncludeFlags(127);
	NewProfile2.Filters.setExcludeFlags(NAV_FLAG_DISABLED);
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
	NewProfile3.MeshIndex = NAV_MESH_REGULAR;
	NewProfile3.Filters.setIncludeFlags(16895);
	NewProfile3.Filters.setExcludeFlags(NAV_FLAG_DISABLED);
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
	NewProfile4.MeshIndex = NAV_MESH_REGULAR;
	NewProfile4.Filters.setIncludeFlags(16767);
	NewProfile4.Filters.setExcludeFlags(NAV_FLAG_DISABLED);
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
	NewProfile5.MeshIndex = NAV_MESH_ONOS;
	NewProfile5.Filters.setIncludeFlags(127);
	NewProfile5.Filters.setExcludeFlags(NAV_FLAG_DISABLED);
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

	NavAgentProfile DefaultProfile;
	DefaultProfile.MeshIndex = NAV_MESH_REGULAR;
	DefaultProfile.Filters.setIncludeFlags(0x7fffffff);
	DefaultProfile.Filters.setExcludeFlags(NAV_FLAG_DISABLED);
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

// Return the appropriate base nav profile information
inline const NavAgentProfile GetBaseAgentProfile(const NavProfileIndex Index)
{
	return BaseAgentProfiles[Index];
}

#endif