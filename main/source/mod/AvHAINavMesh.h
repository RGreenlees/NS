//
// EvoBot - Neoptolemus' Natural Selection bot, based on Botman's HPB bot template
//
// AvHAINavMesh.h
//
// Handles the loading and maintaining of the navmesh data for bot navigation
//

#pragma once
#ifndef AVH_AI_NAVMESH_H
#define AVH_AI_NAVMESH_H

#include <vector>

#include "AvHAIMath.h"
#include "DetourStatus.h"
#include "DetourNavMeshQuery.h"
#include "DetourTileCache.h"
#include "AvHAINavConstants.h"

constexpr int MAX_PATH_POLY = 512; // Max nav mesh polys that can be traversed in a path. This should be sufficient for any sized map.

constexpr int NAVMESHSET_MAGIC = 'M' << 24 | 'S' << 16 | 'E' << 8 | 'T'; //'MSET', used to confirm the nav mesh we're loading is compatible;
constexpr int NAVMESHSET_VERSION = 1;

constexpr int TILECACHESET_MAGIC = 'T' << 24 | 'S' << 16 | 'E' << 8 | 'T'; //'TSET', used to confirm the tile cache we're loading is compatible;
constexpr int TILECACHESET_VERSION = 4;

constexpr int DT_AREA_NULL = 0; // Represents a null area on the nav mesh. Not traversable and considered not on the nav mesh
constexpr int DT_AREA_BLOCKED = 3; // Area occupied by an obstruction (e.g. building). Not traversable, but considered to be on the nav mesh

constexpr float pExtents[3] = { 400.0f, 50.0f, 400.0f }; // Default extents (in GoldSrc units) to find the nearest spot on the nav mesh
constexpr float dtDefaultReachableExtents[3] = { max_ai_use_reach, max_ai_use_reach, max_ai_use_reach }; // Extents (in GoldSrc units) to determine if something is on the nav mesh
static const Vector DefaultReachableExtents = Vector(max_ai_use_reach, max_ai_use_reach, max_ai_use_reach); // Extents (in GoldSrc units) to determine if something is on the nav mesh


// The current state of the nav mesh.
enum class EAINavMeshStatus
{
	NAVMESH_STATUS_UNLOADED = 0, // Waiting to try loading the navmesh
	NAVMESH_STATUS_FAILED,		 // Failed to load the navmesh
	NAVMESH_STATUS_SUCCESS		 // Successfully loaded the navmesh
};

// The result of trying to load a navmesh.
enum class EAINavMeshLoadResult
{
	NAVMESH_LOAD_SUCCESS = 0, // Successfully loaded the navmesh
	NAVMESH_LOAD_NOTFOUND,		 // The requested .nav file does not exist
	NAVMESH_LOAD_INVALID,		// The .nav file is invalid or corrupted
	NAVMESH_LOAD_WRONGVERSION,  // The .nav file is using a different version of the format
	NAVMESH_STATUS_ALLOCFAIL,		 // Failed to allocate memory for the nav data
	NAVMESH_STATUS_MESHINITFAIL,		 // Failed to initialize the navmesh, possibly due to bad data
	NAVMESH_STATUS_CACHEINITFAIL,		 // Failed to initialize the tile cache, possibly due to bad data
	NAVMESH_STATUS_QUERYINITFAIL,		 // Failed to initialize the nav query, possibly due to bad data
	NAVMESH_STATUS_MISSINGTILE		 // Initialized the navmesh, but one or more tiles could not be loaded
};

struct NavOffMeshConnection
{
	EAINavMeshIndex NavMeshIndex = EAINavMeshIndex::NAV_MESH_INVALID;
	Vector FromLocation = ZERO_VECTOR; // The start point of the connection
	Vector ToLocation = ZERO_VECTOR; // The end point of the connection
	unsigned int ConnectionFlags = 0; // The type of connection it is
	unsigned int DefaultConnectionFlags = 0; // If this connection is being temporarily modified, what it should normally be
	unsigned int ConnectionRef = 0; // References to this connection on all defined nav meshes
	edict_t* LinkedObject = nullptr;

	bool IsValid()
	{
		return ConnectionRef > 0 && IsValidNavMeshIndex(NavMeshIndex) && !vEquals(FromLocation, ToLocation);
	}
};

// Hints are locations placed on the nav mesh to influence and guide the bot. For example, "good ambush point".
// See the nav constants header for all nav hint types.
struct NavHint
{
	unsigned int NavMeshIndex = 0;
	unsigned int HintTypes = 0;
	Vector Position;
};

// A temporary obstacle is a shape placed on the map during play which affects the area it covers, changing the movement flags on it.
// For example, a temporary obstacle with an area type of NULL would cut a hole in the nav mesh, e.g. a door is permanently welded shut.
// Can also be later removed to undo the change, hence "temporary" obstacle.
struct NavTempObstacle
{
	EAINavMeshIndex NavMeshIndex = NAV_MESH_INVALID; // Which nav mesh this obstacle belongs to
	Vector Location = ZERO_VECTOR; // The location of the obstacle. This will be at the BASE of the cylinder
	float Radius = 0.0f; // How wide the cylindrical obstacle is
	float Height = 0.0f; // How tall the cylinder is
	unsigned char Area = 0; // The area to mark on the nav mesh
	unsigned int ObstacleRef = 0; // The reference to the obstacle within Detour

	bool IsValid()
	{
		return IsValidNavMeshIndex(NavMeshIndex) && ObstacleRef > 0;
	}
};

// Works like a TraceResult, but specifically for running traces on the nav mesh
struct NavHitResult
{
	float flFraction = 0.0f;
	bool bStartOffMesh = false;
	Vector TraceEndPoint = ZERO_VECTOR;
};

// Links together a tile cache, nav query and the nav mesh into one handy structure for all your querying needs
struct NavMesh
{
	EAINavMeshIndex MeshIndex = NAV_MESH_INVALID;
	class dtTileCache* tileCache = nullptr;
	class dtNavMeshQuery* navQuery = nullptr;
	class dtNavMesh* navMesh = nullptr;
	std::vector<NavOffMeshConnection> MeshConnections;
	std::vector<NavHint> MeshHints;
	std::vector<NavTempObstacle> TempObstacles;
	bool bIsMeshUpToDate = true;

	void Clear()
	{
		MeshIndex = NAV_MESH_INVALID;
		dtFreeNavMesh(navMesh);
		dtFreeNavMeshQuery(navQuery);
		dtFreeTileCache(tileCache);

		MeshConnections.clear();
		MeshHints.clear();
		TempObstacles.clear();
	}

	bool IsValid()
	{
		return MeshIndex < NAV_MESH_INVALID
			&& tileCache != nullptr
			&& navQuery != nullptr
			&& navMesh != nullptr;
	}

	bool IsUpToDate() { return bIsMeshUpToDate; }

	void RemoveOffMeshConnectionFromList(NavOffMeshConnection* ConnectionToRemove);
	void RemoveTempObstacleFromList(NavTempObstacle* ObstacleToRemove);
};

struct NavMeshSetHeader
{
	int magic;
	int version;
	int numTiles;
	dtNavMeshParams params;
	int MeshBuildOffset;
};

struct TileCacheSetHeader
{
	int magic = 0;
	int version = 0;
	int numTiles = 0;
	dtNavMeshParams meshParams;
	dtTileCacheParams cacheParams;

	int NumOffMeshCons = 0;
	int OffMeshConsOffset = 0;

	int NumConvexVols = 0;
	int ConvexVolsOffset = 0;

	int NumNavHints = 0;
	int NavHintsOffset = 0;
};

struct TileCacheExportHeader
{
	int magic;
	int version;

	int numTileCaches;
	int tileCacheDataOffset = 0;

	int tileCacheOffsets[8];

	int NumSurfTypes;
	int SurfTypesOffset;
};

struct TileCacheTileHeader
{
	dtCompressedTileRef tileRef;
	int dataSize;
};

struct NavMeshTileHeader
{
	dtTileRef tileRef;
	int dataSize;
};

struct OffMeshConnectionDef
{
	unsigned int UserID = 0;
	float spos[3] = { 0.0f, 0.0f, 0.0f };
	float epos[3] = { 0.0f, 0.0f, 0.0f };
	bool bBiDir = false;
	float Rad = 0.0f;
	unsigned char Area = 0;
	unsigned int Flag = 0;
	bool bPendingDelete = false;
	bool bDirty = false;
};

// Looks for a .nav file in the appropriate directory for the corresponding map name.
// Returns true if the load was successful. Will back out and clean up if the load is not fully completed.
EAINavMeshLoadResult AIMESH_LoadNavMesh(const char* mapname);

// Will pick up any pending off-mesh obstacles or off-mesh connections waiting to be added/removed/modified
// on the desired navmesh, and will apply the changes. Returns true if the mesh was fully up to date at the end.
bool AIMESH_UpdateTileCache(EAINavMeshIndex MeshIndex);

// Returns true if the requested navmesh is fully up to date and has no pending changes to be applied.
bool AIMESH_IsNavMeshUpToDate(EAINavMeshIndex MeshIndex);

// Returns the current state of the nav mesh, whether it is loaded or not.
EAINavMeshStatus AIMESH_GetNavMeshStatus();

// Will unload all nav mesh data if any is present.
void AIMESH_UnloadNavMesh();

// Does a thorough check to confirm valid navmesh data is loaded.
bool AIMESH_IsNavMeshLoaded();

// Returns the appropriate path to the .nav file for the corresponding map name. Nav files are expected to be in mod_root/navmeshes.
void AIMESH_GetNavMeshFilePath(const char* mapname, char* buffer);

// Guarantees that a non-null pointer return will be a valid, fully-initialized NavMesh
NavMesh* AIMESH_GetNavMeshAtIndex(EAINavMeshIndex DesiredIndex);

std::vector<NavMesh*> AIMESH_GetAllNavMeshes();

/* Adds a new off-mesh connection to the specified navmesh at runtime. Bots using this nav mesh will immediately start using this connection if they're allowed to */
NavOffMeshConnection* AIMESH_AddOffMeshConnection(EAINavMeshIndex TargetNavMesh, Vector StartLoc, Vector EndLoc, unsigned char area, unsigned int flags, bool bBiDirectional);

// Changes the flags on an existing off-mesh connection
void AIMESH_ModifyOffMeshConnectionFlag(NavOffMeshConnection* Connection, const unsigned int NewFlag);

/* Removes the off-mesh connection from all nav meshes which contain it */
bool AIMESH_RemoveOffMeshConnection(NavOffMeshConnection* RemoveConnectionDef);


// Applies a temporary obstacle to the navmesh. Returns a pointer to the temp obstacle created if successful.
NavTempObstacle* AIMESH_AddTemporaryObstacle(EAINavMeshIndex TargetNavMesh, Vector Position, float Radius, float Height, unsigned char Area);

// Will remove the temporary obstacle from the navmesh completely.
// NOTE: This will also null the supplied pointer if successful, as the pointer will be gone from the navmesh.
bool AIMESH_RemoveTemporaryObstacle(NavTempObstacle* ObstacleToRemove);

// Add a hint to the requested nav mesh
NavHint* AIMESH_AddHintToNavmesh(EAINavMeshIndex TargetNavMesh, Vector Location, unsigned int HintFlags);

/*
	Project point to navmesh:
	Takes the supplied location in the game world, and returns the nearest point on the nav mesh within the supplied extents.
	Uses pExtents by default if not supplying one.
	Returns ZERO_VECTOR if not projected successfully
*/
Vector AIMESH_ProjectPointToNavmesh(EAINavMeshIndex TargetNavMesh, const Vector Location, const NavAgentProfile& NavProfile = GetBaseAgentProfile(NAV_PROFILE_DEFAULT), const Vector Extents = Vector(400.0f, 400.0f, 400.0f));

// Finds any random point on the navmesh that is relevant for the bot. Returns ZERO_VECTOR if none found
Vector AIMESH_GetRandomPointOnNavmesh(const NavAgentProfile& NavProfile, const Vector& SearchPoint = ZERO_VECTOR, bool bIgnoreReachability = true);

/*	Finds any random point on the navmesh that is relevant for the bot within a given radius of the origin point,
	taking reachability into account(will not return impossible to reach location).

	Returns ZERO_VECTOR if none found
*/
Vector AIMESH_GetRandomPointOnNavmeshInRadius(const NavAgentProfile& NavProfile, const Vector SearchOrigin, const float MaxRadius, bool bIgnoreReachability, EAINavMovementFlag FlagFilter = NAV_FLAG_NONE);

/*	Finds any random point on the navmesh of the area type (e.g. crouch area) that is relevant for the bot within the min and max radius of the origin point,
	taking reachability into account(will not return impossible to reach location).

	Returns ZERO_VECTOR if none found
*/
Vector AIMESH_GetRandomPointOnNavmeshInDonut(const NavAgentProfile& NavProfile, const Vector origin, const float MinRadius, const float MaxRadius, bool bIgnoreReachability, EAINavMovementFlag FlagFilter = NAV_FLAG_NONE);


void AIMESH_DEBUG_DrawTemporaryObstacles(EAINavMeshIndex MeshIndex, float DrawTime);
void AIMESH_DEBUG_DrawOffMeshConnections(EAINavMeshIndex MeshIndex, float DrawTime);

#endif // AVH_AI_NAVMESH_H