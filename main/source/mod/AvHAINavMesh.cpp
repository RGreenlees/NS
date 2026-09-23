//
// EvoBot - Neoptolemus' Natural Selection bot, based on Botman's HPB bot template
//
// AvHAINavMesh.h
//
// Handles the loading and maintaining of the navmesh data for bot navigation
//

#include "AvHAINavMesh.h"
#include "AvHAIHelper.h"
#include "AvHConstants.h"
#include <string>
#include <dlls/enginecallback.h>

#include "DetourNavMesh.h"
#include "DetourCommon.h"
#include "DetourTileCache.h"
#include "DetourTileCacheBuilder.h"
#include "DetourNavMeshBuilder.h"
#include "fastlz/fastlz.c"
#include "DetourAlloc.h"
#include <dlls/util.h>

EAINavMeshStatus CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_UNLOADED;

std::vector<NavMesh> NavMeshList;

struct FastLZCompressor : public dtTileCacheCompressor
{
	virtual int maxCompressedSize(const int bufferSize)
	{
		return (int)(bufferSize * 1.05f);
	}

	virtual dtStatus compress(const unsigned char* buffer, const int bufferSize,
		unsigned char* compressed, const int /*maxCompressedSize*/, int* compressedSize)
	{
		*compressedSize = fastlz_compress((const void* const)buffer, bufferSize, compressed);
		return DT_SUCCESS;
	}

	virtual dtStatus decompress(const unsigned char* compressed, const int compressedSize,
		unsigned char* buffer, const int maxBufferSize, int* bufferSize)
	{
		*bufferSize = fastlz_decompress(compressed, compressedSize, buffer, maxBufferSize);
		return *bufferSize < 0 ? DT_FAILURE : DT_SUCCESS;
	}
};

struct LinearAllocator : public dtTileCacheAlloc
{
	unsigned char* buffer;
	size_t capacity;
	size_t top;
	size_t high;

	LinearAllocator(const size_t cap) : buffer(0), capacity(0), top(0), high(0)
	{
		resize(cap);
	}

	~LinearAllocator()
	{
		dtFree(buffer);
	}

	void resize(const size_t cap)
	{
		if (buffer) dtFree(buffer);
		buffer = (unsigned char*)dtAlloc(cap, DT_ALLOC_PERM);
		capacity = cap;
	}

	virtual void reset()
	{
		high = dtMax(high, top);
		top = 0;
	}

	virtual void* alloc(const size_t size)
	{
		if (!buffer)
			return 0;
		if (top + size > capacity)
			return 0;
		unsigned char* mem = &buffer[top];
		top += size;
		return mem;
	}

	virtual void free(void* /*ptr*/)
	{}
};

struct MeshProcess : public dtTileCacheMeshProcess
{
	inline MeshProcess()
	{}

	inline void init(OffMeshConnectionDef* OffMeshConnData, int NumConns)
	{}

	virtual void process(struct dtNavMeshCreateParams* params,
		unsigned char* polyAreas, unsigned int* polyFlags)
	{
		// Update poly flags from areas.
		for (int i = 0; i < params->polyCount; ++i)
		{
			polyFlags[i] = GetFlagForArea((EAINavArea)polyAreas[i]);
		}
	}
};

EAINavMeshStatus AIMESH_GetNavMeshStatus()
{
	return CurrentNavMeshStatus;
}

void AIMESH_UnloadNavMesh()
{
	for (auto it = NavMeshList.begin(); it != NavMeshList.end(); it++)
	{
		it->Clear();
	}

	NavMeshList.clear();

	CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_UNLOADED;
}

bool AIMESH_IsNavMeshLoaded()
{
	if (CurrentNavMeshStatus != EAINavMeshStatus::NAVMESH_STATUS_SUCCESS) { return false; }

	if (NavMeshList.size() < 1) { return false; }

	return NavMeshList[0].navMesh != nullptr;
}

void AIMESH_GetNavMeshFilePath(const char* mapname, char* buffer)
{
	if (!mapname || !buffer) { return; }

	std::string theMapName = mapname;
	std::string navPath = std::string(getModDirectory()) + "/navmeshes/" + mapname + ".nav";

	strcpy(buffer, navPath.c_str());
}

NavMesh* AIMESH_GetNavMeshAtIndex(EAINavMeshIndex DesiredIndex)
{
	if (!IsValidNavMeshIndex(DesiredIndex)) { return nullptr; }

	if (DesiredIndex >= NavMeshList.size()) { return nullptr; }

	NavMesh* NavRef = &NavMeshList[static_cast<int>(DesiredIndex)];

	if (NavRef && NavRef->IsValid())
	{
		return NavRef;
	}

	return nullptr;
}

std::vector<NavMesh*> AIMESH_GetAllNavMeshes()
{
	std::vector<NavMesh*> Result;

	const int InvalidIndex = static_cast<int>(NAV_MESH_INVALID);

	for (int i = 0; i < InvalidIndex; i++)
	{
		const EAINavMeshIndex NavIndex = static_cast<EAINavMeshIndex>(i);

		NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(NavIndex);

		if (FoundMesh)
		{
			Result.push_back(FoundMesh);
		}
	}

	return Result;
}

bool AIMESH_UpdateTileCache(EAINavMeshIndex MeshIndex)
{
	NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(MeshIndex);

	if (!FoundMesh) { return true; }

	FoundMesh->tileCache->update(0.0f, FoundMesh->navMesh, &FoundMesh->bIsMeshUpToDate);

	return FoundMesh->IsUpToDate();
}

bool AIMESH_IsNavMeshUpToDate(EAINavMeshIndex MeshIndex)
{
	NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(MeshIndex);

	if (!FoundMesh) { return true; }

	return FoundMesh->IsUpToDate();
}

EAINavMeshLoadResult AIMESH_LoadNavMesh(const char* mapname)
{
	// Clear out any existing nav mesh data first before we crack on.
	AIMESH_UnloadNavMesh();

	char filename[256]; // Full path to .nav file

	AIMESH_GetNavMeshFilePath(mapname, filename);

	FILE* OpenedNavFile = fopen(filename, "rb");

	if (!OpenedNavFile)
	{
		return EAINavMeshLoadResult::NAVMESH_LOAD_NOTFOUND;
	}

	LinearAllocator* m_talloc = new LinearAllocator(32000);
	FastLZCompressor* m_tcomp = new FastLZCompressor;
	MeshProcess* m_tmproc = new MeshProcess;

	// Read header.
	TileCacheExportHeader fileHeader;
	size_t headerReadReturnCode = fread(&fileHeader, sizeof(TileCacheExportHeader), 1, OpenedNavFile);

	// No header to be found, corrupted or invalid .nav file
	if (headerReadReturnCode != 1)
	{
		AIMESH_UnloadNavMesh();
		fclose(OpenedNavFile);
		return EAINavMeshLoadResult::NAVMESH_LOAD_INVALID;
	}

	// Incompatible version of the nav data
	if (fileHeader.magic != TILECACHESET_MAGIC)
	{
		AIMESH_UnloadNavMesh();
		fclose(OpenedNavFile);
		return EAINavMeshLoadResult::NAVMESH_LOAD_WRONGVERSION;
	}

	// Incompatible version of the nav data
	if (fileHeader.version != TILECACHESET_VERSION)
	{
		AIMESH_UnloadNavMesh();
		fclose(OpenedNavFile);
		return EAINavMeshLoadResult::NAVMESH_LOAD_WRONGVERSION;
	}

	fseek(OpenedNavFile, fileHeader.tileCacheDataOffset, SEEK_SET);

	for (int i = 0; i < fileHeader.numTileCaches; i++)
	{
		fseek(OpenedNavFile, fileHeader.tileCacheOffsets[i], SEEK_SET);

		NavMesh NewNavMesh;
		EAINavMeshIndex NewIndex = static_cast<EAINavMeshIndex>(i);

		// More nav meshes than we were expecting.
		if (!IsValidNavMeshIndex(NewIndex))
		{
			NewNavMesh.Clear();
			AIMESH_UnloadNavMesh();
			fclose(OpenedNavFile);
			return EAINavMeshLoadResult::NAVMESH_LOAD_INVALID;
		}

		TileCacheSetHeader tcHeader;

		size_t headerReadReturnCode = fread(&tcHeader, sizeof(TileCacheSetHeader), 1, OpenedNavFile);

		// The file is corrupted in some way, invalid tile data.
		if (headerReadReturnCode != 1)
		{
			NewNavMesh.Clear();
			AIMESH_UnloadNavMesh();
			fclose(OpenedNavFile);
			return EAINavMeshLoadResult::NAVMESH_LOAD_INVALID;
		}

		NewNavMesh.navMesh = dtAllocNavMesh();

		// Could not allocate memory for the nav mesh for some reason.
		if (!NewNavMesh.navMesh)
		{
			NewNavMesh.Clear();
			AIMESH_UnloadNavMesh();
			fclose(OpenedNavFile);
			return EAINavMeshLoadResult::NAVMESH_STATUS_ALLOCFAIL;
		}

		NewNavMesh.tileCache = dtAllocTileCache();

		// Could not allocate memory for the tile cache for some reason.
		if (!NewNavMesh.tileCache)
		{
			NewNavMesh.Clear();
			AIMESH_UnloadNavMesh();
			fclose(OpenedNavFile);
			return EAINavMeshLoadResult::NAVMESH_STATUS_ALLOCFAIL;
		}

		NewNavMesh.navQuery = dtAllocNavMeshQuery();

		// Could not allocate memory for the nav query for some reason.
		if (!NewNavMesh.navQuery)
		{
			NewNavMesh.Clear();
			AIMESH_UnloadNavMesh();
			fclose(OpenedNavFile);
			return EAINavMeshLoadResult::NAVMESH_STATUS_ALLOCFAIL;
		}

		dtStatus MeshInitStatus = NewNavMesh.navMesh->init(&tcHeader.meshParams);

		// Could not initialize the nav mesh for some reason (possibly bad data?)
		if (dtStatusFailed(MeshInitStatus))
		{
			NewNavMesh.Clear();
			AIMESH_UnloadNavMesh();
			fclose(OpenedNavFile);
			return EAINavMeshLoadResult::NAVMESH_STATUS_MESHINITFAIL;
		}

		dtStatus TileCacheInitStatus = NewNavMesh.tileCache->init(&tcHeader.cacheParams, m_talloc, m_tcomp, m_tmproc);

		// Could not initialize the tile cache for some reason (possibly bad data?)
		if (dtStatusFailed(TileCacheInitStatus))
		{
			NewNavMesh.Clear();
			AIMESH_UnloadNavMesh();
			fclose(OpenedNavFile);
			return EAINavMeshLoadResult::NAVMESH_STATUS_CACHEINITFAIL;
		}

		// Read tiles.
		for (int ii = 0; ii < tcHeader.numTiles; ++ii)
		{
			TileCacheTileHeader tileHeader;
			size_t tileHeaderReadReturnCode = fread(&tileHeader, sizeof(tileHeader), 1, OpenedNavFile);
			if (tileHeaderReadReturnCode != 1) { continue; }

			if (!tileHeader.tileRef || !tileHeader.dataSize)
				break;

			unsigned char* data = (unsigned char*)dtAlloc(tileHeader.dataSize, DT_ALLOC_PERM);
			if (!data) break;
			memset(data, 0, tileHeader.dataSize);
			size_t tileDataReadReturnCode = fread(data, tileHeader.dataSize, 1, OpenedNavFile);

			// Invalid tile data
			if (tileDataReadReturnCode != 1)
			{
				dtFree(data);
				NewNavMesh.Clear();
				AIMESH_UnloadNavMesh();
				fclose(OpenedNavFile);
				return EAINavMeshLoadResult::NAVMESH_LOAD_INVALID;
			}

			dtCompressedTileRef tile = 0;
			dtStatus addTileStatus = NewNavMesh.tileCache->addTile(data, tileHeader.dataSize, DT_COMPRESSEDTILE_FREE_DATA, &tile);

			// Invalid tile data
			if (dtStatusFailed(addTileStatus))
			{
				dtFree(data);
				NewNavMesh.Clear();
				AIMESH_UnloadNavMesh();
				fclose(OpenedNavFile);
				return EAINavMeshLoadResult::NAVMESH_LOAD_INVALID;
			}

			if (tile)
				NewNavMesh.tileCache->buildNavMeshTile(tile, NewNavMesh.navMesh);
		}

		dtStatus QueryInitStatus = NewNavMesh.navQuery->init(NewNavMesh.navMesh, 2048);

		if (dtStatusFailed(QueryInitStatus))
		{
			NewNavMesh.Clear();
			AIMESH_UnloadNavMesh();
			fclose(OpenedNavFile);
			return EAINavMeshLoadResult::NAVMESH_STATUS_QUERYINITFAIL;
		}

		NavMeshList.push_back(NewNavMesh);

		NavMesh* NewNavMeshRef = &NavMeshList.back();

		fseek(OpenedNavFile, tcHeader.OffMeshConsOffset, SEEK_SET);

		for (int ii = 0; ii < tcHeader.NumOffMeshCons; ii++)
		{
			dtOffMeshConnection def;
			fread(&def, sizeof(dtOffMeshConnection), 1, OpenedNavFile);

			Vector Start = UTIL_VecDetourToGoldSrc(def.pos);
			Vector End = UTIL_VecDetourToGoldSrc(&def.pos[3]);

			AIMESH_AddOffMeshConnection(NewIndex, Start, End, def.area, def.flags, def.bBiDir);
		}

		fseek(OpenedNavFile, tcHeader.NavHintsOffset, SEEK_SET);

		for (int ii = 0; ii < tcHeader.NumNavHints; ii++)
		{
			NavHint def;

			fread(&def, sizeof(NavHint), 1, OpenedNavFile);

			Vector Position = UTIL_VecDetourToGoldSrc(def.Position);

			AIMESH_AddHintToNavmesh(NewIndex, Position, def.HintTypes);
		}
	}

	fclose(OpenedNavFile);

	return EAINavMeshLoadResult::NAVMESH_LOAD_SUCCESS;
}

NavHint* AIMESH_AddHintToNavmesh(EAINavMeshIndex TargetNavMesh, Vector Location, unsigned int HintFlags)
{
	if (!IsValidNavMeshIndex(TargetNavMesh)) { return nullptr; }

	NavMesh* FoundNavMesh = AIMESH_GetNavMeshAtIndex(TargetNavMesh);

	if (!FoundNavMesh) { return; }

	NavHint NewHint;
	NewHint.Position = Location;
	NewHint.HintTypes = HintFlags;

	FoundNavMesh->MeshHints.push_back(NewHint);

	return &(*prev(FoundNavMesh->MeshHints.end()));
}

NavOffMeshConnection* AIMESH_AddOffMeshConnection(EAINavMeshIndex TargetNavMesh, Vector StartLoc, Vector EndLoc, unsigned char area, unsigned int flags, bool bBiDirectional)
{
	NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(TargetNavMesh);

	if (!FoundMesh) { return nullptr; }

	Vector ProjectedStart = AIMESH_ProjectPointToNavmesh(TargetNavMesh, StartLoc);
	Vector ProjectedEnd = AIMESH_ProjectPointToNavmesh(TargetNavMesh, EndLoc);

	if (vIsZero(ProjectedStart) || vIsZero(ProjectedEnd)) { return nullptr; }

	NavOffMeshConnection NewConnectionDef;
	NewConnectionDef.NavMeshIndex = TargetNavMesh;
	NewConnectionDef.FromLocation = ProjectedStart;
	NewConnectionDef.ToLocation = ProjectedEnd;
	NewConnectionDef.DefaultConnectionFlags = flags;
	NewConnectionDef.ConnectionFlags = flags;

	// Now flip the coordinates for Detour

	float ConvProjectedStart[3];
	float ConvProjectedEnd[3];

	UTIL_VecGoldSrcToDetour(ProjectedStart, ConvProjectedStart);
	UTIL_VecGoldSrcToDetour(ProjectedEnd, ConvProjectedEnd);

	dtOffMeshConnectionRef ref = 0;
	NewConnectionDef.ConnectionRef = 0;

	dtStatus AddStatus = FoundMesh->tileCache->addOffMeshConnection(ConvProjectedStart, ConvProjectedEnd, 18.0f, area, flags, bBiDirectional, &ref);

	if (dtStatusSucceed(AddStatus))
	{
		NewConnectionDef.ConnectionRef = (unsigned int)ref;

		FoundMesh->MeshConnections.push_back(NewConnectionDef);

		return &(*prev(FoundMesh->MeshConnections.end()));
	}

	return nullptr;
}

void AIMESH_ModifyOffMeshConnectionFlag(NavOffMeshConnection* Connection, const unsigned int NewFlag)
{
	// Don't do anything if the connection is invalid, or already has the desired flags set
	if (!Connection || !Connection->IsValid() || Connection->ConnectionFlags == NewFlag) { return; }

	NavMesh* ParentMesh = AIMESH_GetNavMeshAtIndex(Connection->NavMeshIndex);

	if (!ParentMesh) { return; }

	Connection->ConnectionFlags = NewFlag;
	ParentMesh->tileCache->modifyOffMeshConnection(Connection->ConnectionRef, NewFlag);
}

bool AIMESH_RemoveOffMeshConnection(NavOffMeshConnection* RemoveConnectionDef)
{
	if (!RemoveConnectionDef || !RemoveConnectionDef->IsValid()) { return false; }

	NavMesh* ParentNavMesh = AIMESH_GetNavMeshAtIndex(RemoveConnectionDef->NavMeshIndex);

	if (!ParentNavMesh) { return false; }

	dtStatus RemoveStatus = ParentNavMesh->tileCache->removeOffMeshConnection(RemoveConnectionDef->ConnectionRef);

	if (dtStatusSucceed(RemoveStatus))
	{
		ParentNavMesh->RemoveOffMeshConnectionFromList(RemoveConnectionDef);
		RemoveConnectionDef = nullptr;
		return true;
	}
	else
	{
		return false;
	}
}

NavTempObstacle* AIMESH_AddTemporaryObstacle(EAINavMeshIndex TargetNavMesh, Vector Position, float Radius, float Height, unsigned char Area)
{
	NavMesh* ParentNavMesh = AIMESH_GetNavMeshAtIndex(TargetNavMesh);

	if (!ParentNavMesh) { return; }

	// Convert to Detour coordinate system, and adjust so position is the centre of the obstacle rather than bottom
	float Pos[3];

	UTIL_VecGoldSrcToDetour(Position, Pos);

	dtObstacleRef ObsRef = 0;
	dtStatus status = ParentNavMesh->tileCache->addObstacle(Pos, Radius, Height, Area, &ObsRef);

	if (!dtStatusSucceed(status)) { return nullptr; }

	NavTempObstacle NewObstacle;
	NewObstacle.NavMeshIndex = TargetNavMesh;
	NewObstacle.Location = Position;
	NewObstacle.Radius = Radius;
	NewObstacle.Height = Height;
	NewObstacle.Area = Area;
	NewObstacle.ObstacleRef = (unsigned int)ObsRef;

	ParentNavMesh->TempObstacles.push_back(NewObstacle);

	return &(*prev(ParentNavMesh->TempObstacles.end()));
}

bool AIMESH_RemoveTemporaryObstacle(NavTempObstacle* ObstacleToRemove)
{
	if (!ObstacleToRemove) { return false; }

	NavMesh* ParentMesh = AIMESH_GetNavMeshAtIndex(ObstacleToRemove->NavMeshIndex);

	if (!ParentMesh)
	{
		ObstacleToRemove = nullptr;
		return false;
	}

	dtStatus status = ParentMesh->tileCache->removeObstacle(ObstacleToRemove->ObstacleRef);

	const bool bSuccessful = dtStatusSucceed(status);

	if (bSuccessful)
	{
		ParentMesh->RemoveTempObstacleFromList(ObstacleToRemove);
		ObstacleToRemove = nullptr;
	}

	return bSuccessful;
}

Vector AIMESH_ProjectPointToNavmesh(EAINavMeshIndex TargetNavMesh, const Vector Location, const NavAgentProfile& NavProfile, const Vector Extents)
{
	NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(TargetNavMesh);

	if (!FoundMesh) { return ZERO_VECTOR; }

	const dtNavMeshQuery* m_navQuery = FoundMesh->navQuery;
	const dtNavMesh* m_navMesh = FoundMesh->navMesh;
	const dtQueryFilter* m_navFilter = &NavProfile.Filters;

	if (!m_navQuery || !m_navMesh) { return ZERO_VECTOR; }

	Vector PointToProject = Location;

	float pCheckLoc[3];
	UTIL_VecGoldSrcToDetour(Location, pCheckLoc);

	dtPolyRef FoundPoly;
	float NavNearest[3];

	dtStatus success = m_navQuery->findNearestPoly(pCheckLoc, Extents, m_navFilter, &FoundPoly, NavNearest);

	if (FoundPoly > 0 && dtStatusSucceed(success))
	{
		return UTIL_VecDetourToGoldSrc(NavNearest);
	}
	else
	{
		int PointContents = UTIL_PointContents(PointToProject);

		if (PointContents != CONTENTS_SOLID && PointContents != CONTENTS_LADDER)
		{
			Vector TraceHit = UTIL_GetTraceHitLocation(PointToProject + Vector(0.0f, 0.0f, 1.0f), PointToProject - Vector(0.0f, 0.0f, 1000.0f));

			PointToProject = (!vIsZero(TraceHit)) ? TraceHit : Location;
		}

		float pRecheckLoc[3];

		UTIL_VecGoldSrcToDetour(PointToProject, pRecheckLoc);

		dtStatus successRetry = m_navQuery->findNearestPoly(pRecheckLoc, Extents, m_navFilter, &FoundPoly, NavNearest);

		if (FoundPoly > 0 && dtStatusSucceed(success))
		{
			return UTIL_VecDetourToGoldSrc(NavNearest);
		}
		else
		{
			return ZERO_VECTOR;
		}
	}

	return ZERO_VECTOR;
}

Vector AIMESH_GetRandomPointOnNavmesh(const NavAgentProfile& NavProfile, const Vector& SearchPoint, bool bIgnoreReachability)
{
	NavMesh* QueriedMesh = AIMESH_GetNavMeshAtIndex(NavProfile.MeshIndex);

	if (!QueriedMesh) { return g_vecZero; }

	const dtNavMeshQuery* m_navQuery = QueriedMesh->navQuery;
	const dtQueryFilter* m_navFilter = &NavProfile.Filters;

	if (!m_navQuery) { return g_vecZero; }

	float DetourResult[3];
	memset(DetourResult, 0, sizeof(DetourResult));

	dtStatus FindStatus;

	if (bIgnoreReachability)
	{
		dtPolyRef RefPoly = 0;

		FindStatus = m_navQuery->findRandomPoint(m_navFilter, frand, &RefPoly, DetourResult);
	}
	else
	{
		dtPolyRef RefPoly = 0;
		float SearchOrigin[3];
		float NavNearest[3];

		UTIL_VecGoldSrcToDetour(SearchPoint, SearchOrigin);

		dtStatus FoundPolyResult = m_navQuery->findNearestPoly(SearchOrigin, pExtents, m_navFilter, &RefPoly, NavNearest);

		if (dtStatusFailed(FoundPolyResult))
		{
			return g_vecZero;
		}

		dtPolyRef RandomPoly;
		float RandomPoint[3];
		const float DefaultSearchRadius = 8192.0f;

		dtStatus FindStatus = m_navQuery->findRandomPointAroundCircle(RefPoly, NavNearest, DefaultSearchRadius, m_navFilter, frand, &RandomPoly, RandomPoint);
	}

	if (dtStatusFailed(FindStatus))
	{
		return g_vecZero;
	}

	const Vector ReturnValue = UTIL_VecDetourToGoldSrc(DetourResult);

	return ReturnValue;
}

Vector AIMESH_GetRandomPointOnNavmeshInRadius(const NavAgentProfile& NavProfile, const Vector SearchOrigin, const float MaxRadius, bool bIgnoreReachability, EAINavMovementFlag FlagFilter)
{
	NavMesh* QueriedMesh = AIMESH_GetNavMeshAtIndex(NavProfile.MeshIndex);

	if (!QueriedMesh) { return g_vecZero; }

	const NavAgentProfile SearchProfile = (FlagFilter != NAV_FLAG_NONE)
		? NavAgentProfile(NavProfile.MeshIndex, FlagFilter, NavProfile.bFlyingProfile)
		: NavProfile;

	const dtNavMeshQuery* m_navQuery = QueriedMesh->navQuery;
	const dtQueryFilter* m_navFilter = &SearchProfile.Filters;

	if (!m_navQuery) { return g_vecZero; }

	float DetourSearchOrigin[3];

	UTIL_VecGoldSrcToDetour(SearchOrigin, DetourSearchOrigin);

	dtPolyRef FoundPoly;
	float NavNearest[3];

	dtStatus foundPolyResult = m_navQuery->findNearestPoly(DetourSearchOrigin, pExtents, m_navFilter, &FoundPoly, NavNearest);

	if (dtStatusFailed(foundPolyResult))
	{
		return g_vecZero;
	}

	dtPolyRef RandomPoly;
	float RandomPoint[3];
	dtStatus FoundRandomPointResult;

	if (bIgnoreReachability)
	{
		FoundRandomPointResult = m_navQuery->findRandomPointAroundCircleIgnoreReachability(FoundPoly, NavNearest, MaxRadius, m_navFilter, frand, &RandomPoly, RandomPoint);
	}
	else
	{
		FoundRandomPointResult = m_navQuery->findRandomPointAroundCircle(FoundPoly, NavNearest, MaxRadius, m_navFilter, frand, &RandomPoly, RandomPoint);
	}

	if (dtStatusFailed(FoundRandomPointResult))
	{
		return g_vecZero;
	}

	const Vector Result = UTIL_VecDetourToGoldSrc(RandomPoint);

	return Result;
}

Vector AIMESH_GetRandomPointOnNavmeshInDonut(const NavAgentProfile& NavProfile, const Vector origin, const float MinRadius, const float MaxRadius, bool bIgnoreReachability, EAINavMovementFlag FlagFilter = NAV_FLAG_NONE)
{
	int maxIterations = 0;
	float MinRadiusSq = sqrf(MinRadius);

	while (maxIterations < 100)
	{
		Vector StartPoint = AIMESH_GetRandomPointOnNavmeshInRadius(NavProfile, origin, MaxRadius, bIgnoreReachability, FlagFilter);

		if (vDist2DSq(StartPoint, origin) > MinRadiusSq)
		{
			return StartPoint;
		}

		maxIterations++;
	}

	return ZERO_VECTOR;
}

void AIMESH_DEBUG_DrawOffMeshConnections(EAINavMeshIndex MeshIndex, float DrawTime)
{
	NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(MeshIndex);

	if (!FoundMesh) { return; }

	int NumDrawn = 0;

	for (int i = 0; i < FoundMesh->tileCache->getOffMeshCount(); i++)
	{
		const dtOffMeshConnection* con = FoundMesh->tileCache->getOffMeshConnection(i);

		if (con->state == DT_OFFMESH_EMPTY || con->state == DT_OFFMESH_REMOVING) { continue; }

		Vector StartLine = Vector(con->pos[0], -con->pos[2], con->pos[1]);
		Vector EndLine = Vector(con->pos[3], -con->pos[5], con->pos[4]);

		unsigned char r, g, b;

		GetDebugColorForFlag(static_cast<EAINavMovementFlag>(con->flags), r, g, b);

		UTIL_DrawLine(INDEXENT(1), StartLine, EndLine, DrawTime, r, g, b);

		NumDrawn++;

		if (NumDrawn > 30)
		{
			break;
		}
	}
}

void AIMESH_DEBUG_DrawTemporaryObstacles(EAINavMeshIndex MeshIndex, float DrawTime)
{
	NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(MeshIndex);

	if (!FoundMesh) { return; }

		int NumObstacles = FoundMesh->tileCache->getObstacleCount();

		for (int i = 0; i < NumObstacles; i++)
		{
			const dtTileCacheObstacle* ObstacleRef = FoundMesh->tileCache->getObstacle(i);

			// TODO: Add support for AABB and orientated box types
			if (!ObstacleRef || ObstacleRef->state != DT_OBSTACLE_PROCESSED || ObstacleRef->type != ObstacleType::DT_OBSTACLE_CYLINDER) { continue; }

			unsigned char r, g, b;

			const EAINavArea NavArea = static_cast<EAINavArea>(ObstacleRef->cylinder.area);

			GetDebugColorForArea(NavArea, r, g, b);

			float Radius = ObstacleRef->cylinder.radius;
			float Height = ObstacleRef->cylinder.height;

			// The location of obstacles in Recast are at the bottom of the shape, not the centre
			Vector Centre = Vector(ObstacleRef->cylinder.pos[0], -ObstacleRef->cylinder.pos[2], ObstacleRef->cylinder.pos[1] + (Height * 0.5f));

			if (vDist2DSq(INDEXENT(1)->v.origin, Centre) > sqrf(UTIL_MetresToGoldSrcUnits(10.0f))) { continue; }

			Vector bMin = Centre - Vector(Radius, Radius, Height * 0.5f);
			Vector bMax = Centre + Vector(Radius, Radius, (Height * 0.5f));

			UTIL_DrawBox(INDEXENT(1), bMin, bMax, DrawTime, r, g, b);
		}
}

void NavMesh::RemoveOffMeshConnectionFromList(NavOffMeshConnection* ConnectionToRemove)
{
	if (!ConnectionToRemove) { return; }

	for (auto it = MeshConnections.begin(); it != MeshConnections.end();)
	{
		NavOffMeshConnection* CheckRef = &(*it);

		if (CheckRef->ConnectionRef == ConnectionToRemove->ConnectionRef)
		{
			it = MeshConnections.erase(it);
		}
		else
		{
			it++;
		}
	}
}

void NavMesh::RemoveTempObstacleFromList(NavTempObstacle* ObstacleToRemove)
{
	if (!ObstacleToRemove) { return; }

	for (auto it = TempObstacles.begin(); it != TempObstacles.end();)
	{
		NavTempObstacle* CheckRef = &(*it);

		if (CheckRef->ObstacleRef == ObstacleToRemove->ObstacleRef)
		{
			it = TempObstacles.erase(it);
		}
		else
		{
			it++;
		}
	}
}
