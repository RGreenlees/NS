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
	NAVMESH_LOAD_NOTFOUND = 0,		 // The requested .nav file does not exist
	NAVMESH_LOAD_INVALID,		// The .nav file is invalid or corrupted
	NAVMESH_LOAD_WRONGVERSION,  // The .nav file is using a different version of the format
	NAVMESH_STATUS_ALLOCFAIL,		 // Failed to allocate memory for the nav data
	NAVMESH_STATUS_MESHINITFAIL,		 // Failed to initialize the navmesh, possibly due to bad data
	NAVMESH_STATUS_CACHEINITFAIL,		 // Failed to initialize the tile cache, possibly due to bad data
	NAVMESH_STATUS_QUERYINITFAIL,		 // Failed to initialize the nav query, possibly due to bad data
	NAVMESH_LOAD_SUCCESS // Successfully loaded the navmesh
};

// Works like a TraceResult, but specifically for running traces on the nav mesh
struct NavHitResult
{
	float flFraction = 0.0f;
	bool bStartOffMesh = false;
	Vector TraceEndPoint = ZERO_VECTOR;
	Vector HitNormal = ZERO_VECTOR;

	void Clear()
	{
		flFraction = 0.0f;
		bStartOffMesh = false;
		TraceEndPoint = ZERO_VECTOR;
		HitNormal = ZERO_VECTOR;
	}
};

// Links together a tile cache, nav query and the nav mesh into one handy structure for all your querying needs
struct NavMesh
{
	EAINavMeshIndex MeshIndex = EAINavMeshIndex::NAV_MESH_INVALID;
	class dtTileCache* dtTileCache = nullptr;
	class dtNavMeshQuery* dtNavQuery = nullptr;
	class dtNavMesh* dtNavMesh = nullptr;
	OffMeshConnectionList MeshConnections;
	NavHintList MeshHints;
	NavTempObstacleList TempObstacles;
	bool bIsMeshUpToDate = true;

	void Clear()
	{
		MeshIndex = EAINavMeshIndex::NAV_MESH_INVALID;
		dtFreeNavMesh(dtNavMesh);
		dtFreeNavMeshQuery(dtNavQuery);
		dtFreeTileCache(dtTileCache);

		MeshConnections.clear();
		MeshHints.clear();
		TempObstacles.clear();
	}

	bool IsValid()
	{
		return MeshIndex < EAINavMeshIndex::NAV_MESH_INVALID
			&& dtTileCache != nullptr
			&& dtNavQuery != nullptr
			&& dtNavMesh != nullptr;
	}

	bool IsUpToDate() { return bIsMeshUpToDate; }

	void RemoveOffMeshConnectionFromList(NavOffMeshConnection* ConnectionToRemove);
	void RemoveTempObstacleFromList(NavTempObstacle* ObstacleToRemove);
};
typedef std::vector<NavMesh*> NavMeshList;

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
EAINavMeshLoadResult AIMESH_LoadNavMesh(const char* MapName);

// Will pick up any pending off-mesh obstacles or off-mesh connections waiting to be added/removed/modified
// on the desired navmesh, and will apply the changes. Returns true if the mesh was modified in some way AND is fully up to date.
bool AIMESH_UpdateTileCache(EAINavMeshIndex MeshIndex);

void AIMESH_UpdateTileCaches(std::vector<EAINavMeshIndex>& ModifiedMeshes);

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

NavMeshList AIMESH_GetAllNavMeshes();

/* Adds a new off-mesh connection to the specified navmesh at runtime. Bots using this nav mesh will immediately start using this connection if they're allowed to */
NavOffMeshConnection* AIMESH_AddOffMeshConnection(EAINavMeshIndex TargetNavMesh, Vector StartLoc, Vector EndLoc, EAINavArea area, EAINavMovementFlag flags, bool bBiDirectional);

// Changes the flags on an existing off-mesh connection
void AIMESH_ModifyOffMeshConnectionFlag(NavOffMeshConnection* Connection, const EAINavMovementFlag NewFlag);

/* Removes the off-mesh connection from all nav meshes which contain it */
bool AIMESH_RemoveOffMeshConnection(NavOffMeshConnection* RemoveConnectionDef);

// Returns true if the trace along the nav mesh from start to end made it within the acceptable distance range
bool AIMESH_QuickTraceNavLine(const NavAgentProfile* NavProfile, const Vector StartLocation, const Vector EndLocation, float MaxAcceptableDistance = 0.1f);

// Returns detailed information on a nav mesh trace. Will return the end location of the trace, as well as populating the details in HitResult
Vector AIMESH_TraceNavLine(const NavAgentProfile* NavProfile, const Vector StartLocation, const Vector EndLocation, NavHitResult* HitResult = nullptr);
EAINavArea AIMESH_GetNavAreaAtLocation(const NavAgentProfile* NavProfile, const Vector Location);
dtPolyRef AIMESH_GetNearestPolyRefForLocation(const NavAgentProfile* NavProfile, const Vector Location);
Vector AIMESH_AdjustPointAwayFromNavWall(const NavAgentProfile* NavProfile, const Vector& Location, const float MaxDistanceFromWall);

// Applies a temporary obstacle to the navmesh. Returns a pointer to the temp obstacle created if successful.
NavTempObstacle* AIMESH_AddTemporaryObstacle(EAINavMeshIndex TargetNavMesh, Vector Position, float Radius, float Height, EAINavArea Area);

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
Vector AIMESH_ProjectPointToNavmesh(const NavAgentProfile* NavProfile, const Vector Location, const Vector Extents = Vector(400.0f, 400.0f, 400.0f));

// Finds any random point on the navmesh that is relevant for the bot. Returns ZERO_VECTOR if none found
Vector AIMESH_GetRandomPointOnNavmesh(const NavAgentProfile& NavProfile, const Vector& SearchPoint = ZERO_VECTOR, bool bIgnoreReachability = true);

/*	Finds any random point on the navmesh that is relevant for the bot within a given radius of the origin point,
	taking reachability into account(will not return impossible to reach location).

	Returns ZERO_VECTOR if none found
*/
Vector AIMESH_GetRandomPointOnNavmeshInRadius(const NavAgentProfile& NavProfile, const Vector SearchOrigin, const float MaxRadius, bool bIgnoreReachability, EAINavMovementFlag FlagFilter = EAINavMovementFlag::NAV_FLAG_NONE);

/*	Finds any random point on the navmesh of the area type (e.g. crouch area) that is relevant for the bot within the min and max radius of the origin point,
	taking reachability into account(will not return impossible to reach location).

	Returns ZERO_VECTOR if none found
*/
Vector AIMESH_GetRandomPointOnNavmeshInDonut(const NavAgentProfile& NavProfile, const Vector origin, const float MinRadius, const float MaxRadius, bool bIgnoreReachability, EAINavMovementFlag FlagFilter = EAINavMovementFlag::NAV_FLAG_NONE);


bool AIMESH_IsPointOnNavmesh(const EAINavMeshIndex MeshIndex, const Vector Location, const NavAgentProfile* NavProfile = GetBaseAgentProfile(EAINavProfileIndex::NAV_PROFILE_DEFAULT), const Vector SearchExtents = DefaultReachableExtents);

void AIMESH_DEBUG_DrawTemporaryObstacles(EAINavMeshIndex MeshIndex, float DrawTime);
void AIMESH_DEBUG_DrawOffMeshConnections(EAINavMeshIndex MeshIndex, float DrawTime);

#endif // AVH_AI_NAVMESH_H