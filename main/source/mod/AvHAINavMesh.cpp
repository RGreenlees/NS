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
#include "DetourNavMeshBuilder.h"
#include "fastlz/fastlz.c"
#include "DetourAlloc.h"
#include <dlls/util.h>

EAINavMeshStatus CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_UNLOADED;

std::vector<NavMesh> ActiveNavMeshes;

int ConnectionDrawIndex = 0;
uint32_t CurrentNavMeshGeneration = 1;
uint32_t NextNavHintIndex = 1;

static bool AIMESH_AddOffMeshConnectionInternal(EAINavMeshIndex TargetNavMesh, Vector StartLoc, Vector EndLoc, EAINavArea area, EAINavMovementFlag flags, bool bBiDirectional, OffMeshConnectionHandle& OutHandle, bool bProjectEndpoints);

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
		return *bufferSize <= 0 ? DT_FAILURE : DT_SUCCESS;
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
			polyFlags[i] = static_cast<unsigned int>(GetFlagForArea((EAINavArea)polyAreas[i]));
		}
	}
};

void NavMesh::Clear()
{
	MeshIndex = EAINavMeshIndex::NAV_MESH_INVALID;
	dtFreeNavMesh(NavMeshRef);
	dtFreeNavMeshQuery(NavQueryRef);
	dtFreeTileCache(TileCacheRef);

	NavMeshRef = nullptr;
	NavQueryRef = nullptr;
	TileCacheRef = nullptr;

	delete TileCacheAllocator;
	delete TileCacheCompressor;
	delete TileCacheMeshProcess;

	TileCacheAllocator = nullptr;
	TileCacheCompressor = nullptr;
	TileCacheMeshProcess = nullptr;

	MeshConnections.clear();
	MeshConnectionHandles.clear();
	MeshHints.clear();
	TempObstacles.clear();
}

EAINavMeshStatus AIMESH_GetNavMeshStatus()
{
	return CurrentNavMeshStatus;
}

void AIMESH_UnloadNavMesh()
{
	for (auto it = ActiveNavMeshes.begin(); it != ActiveNavMeshes.end(); it++)
	{
		NavMesh* ThisMesh = &(*it);

		if (!ThisMesh) { continue; }

		ThisMesh->Clear();
	}

	ActiveNavMeshes.clear();

	CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_UNLOADED;
	ConnectionDrawIndex = 0;
	CurrentNavMeshGeneration++;
}

bool AIMESH_IsNavMeshLoaded()
{
	if (CurrentNavMeshStatus != EAINavMeshStatus::NAVMESH_STATUS_SUCCESS) { return false; }

	if (ActiveNavMeshes.size() < 1) { return false; }

	return ActiveNavMeshes[0].NavMeshRef != nullptr;
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
	const int DesiredIndexInt = static_cast<int>(DesiredIndex);

	if (!IsValidNavMeshIndex(DesiredIndexInt)) { return nullptr; }

	if (DesiredIndexInt >= ActiveNavMeshes.size()) { return nullptr; }

	NavMesh* NavRef = &ActiveNavMeshes[DesiredIndexInt];

	if (NavRef && NavRef->IsValid())
	{
		return NavRef;
	}

	return nullptr;
}

NavMeshList AIMESH_GetAllNavMeshes()
{
	NavMeshList Result;

	const int InvalidIndex = static_cast<int>(EAINavMeshIndex::NAV_MESH_INVALID);

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

void AIMESH_UpdateTileCaches(std::vector<EAINavMeshIndex>& ModifiedMeshes)
{
	ModifiedMeshes.clear();

	for (auto NavMeshIt = ActiveNavMeshes.begin(); NavMeshIt != ActiveNavMeshes.end(); NavMeshIt++)
	{
		NavMesh* Mesh = &(*NavMeshIt);

		if (!Mesh || !Mesh->IsValid()) { continue; }

		const bool bWasModified = AIMESH_UpdateTileCache(Mesh->MeshIndex);

		if (bWasModified)
		{
			ModifiedMeshes.push_back(Mesh->MeshIndex);
		}
	}
}

bool AIMESH_UpdateTileCache(EAINavMeshIndex MeshIndex)
{
	NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(MeshIndex);

	if (!FoundMesh) { return false; }

	bool bWasNavMeshModified = false;
	const dtStatus UpdateStatus = FoundMesh->TileCacheRef->update(0.0f, FoundMesh->NavMeshRef, &FoundMesh->bIsMeshUpToDate, &bWasNavMeshModified);

	if (dtStatusFailed(UpdateStatus))
	{
		FoundMesh->bIsMeshUpToDate = false;
		return false;
	}

	return FoundMesh->IsUpToDate() && bWasNavMeshModified;
}

bool AIMESH_IsNavMeshUpToDate(EAINavMeshIndex MeshIndex)
{
	NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(MeshIndex);

	if (!FoundMesh) { return true; }

	return FoundMesh->IsUpToDate();
}

EAINavMeshLoadResult AIMESH_LoadNavMesh(const char* MapName)
{
	// Clear out any existing nav mesh data first before we crack on.
	AIMESH_UnloadNavMesh();

	if (!MapName) { return EAINavMeshLoadResult::NAVMESH_LOAD_NOTFOUND; }

	char filename[256]; // Full path to .nav file

	AIMESH_GetNavMeshFilePath(MapName, filename);

	FILE* OpenedNavFile = fopen(filename, "rb");

	if (!OpenedNavFile)
	{
		CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_FAILED;
		return EAINavMeshLoadResult::NAVMESH_LOAD_NOTFOUND;
	}

	// Read header.
	TileCacheExportHeader fileHeader;
	size_t headerReadReturnCode = fread(&fileHeader, sizeof(TileCacheExportHeader), 1, OpenedNavFile);

	// No header to be found, corrupted or invalid .nav file
	if (headerReadReturnCode != 1)
	{
		AIMESH_UnloadNavMesh();
		fclose(OpenedNavFile);
		CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_FAILED;
		return EAINavMeshLoadResult::NAVMESH_LOAD_INVALID;
	}

	// Incompatible version of the nav data
	if (fileHeader.magic != TILECACHESET_MAGIC || fileHeader.version != TILECACHESET_VERSION)
	{
		AIMESH_UnloadNavMesh();
		fclose(OpenedNavFile);
		CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_FAILED;
		return EAINavMeshLoadResult::NAVMESH_LOAD_WRONGVERSION;
	}

	if (fileHeader.numTileCaches < 0 || fileHeader.numTileCaches >= 8)
	{
		AIMESH_UnloadNavMesh();
		fclose(OpenedNavFile);
		CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_FAILED;
		return EAINavMeshLoadResult::NAVMESH_LOAD_INVALID;
	}

	fseek(OpenedNavFile, fileHeader.tileCacheDataOffset, SEEK_SET);

	for (int i = 0; i < fileHeader.numTileCaches; i++)
	{
		fseek(OpenedNavFile, fileHeader.tileCacheOffsets[i], SEEK_SET);

		ActiveNavMeshes.emplace_back();

		NavMesh* NewNavMesh = &ActiveNavMeshes.back();

		// More nav meshes than we were expecting.
		if (!IsValidNavMeshIndex(i))
		{
			AIMESH_UnloadNavMesh();
			fclose(OpenedNavFile);
			CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_FAILED;
			return EAINavMeshLoadResult::NAVMESH_LOAD_INVALID;
		}

		NewNavMesh->MeshIndex = static_cast<EAINavMeshIndex>(i);

		TileCacheSetHeader tcHeader;

		size_t headerReadReturnCode = fread(&tcHeader, sizeof(TileCacheSetHeader), 1, OpenedNavFile);

		// The file is corrupted in some way, invalid tile data.
		if (headerReadReturnCode != 1 || !tcHeader.IsValid())
		{
			AIMESH_UnloadNavMesh();
			fclose(OpenedNavFile);
			CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_FAILED;
			return EAINavMeshLoadResult::NAVMESH_LOAD_INVALID;
		}

		const int HeaderTileLimit = tcHeader.cacheParams.maxTiles;

		NewNavMesh->NavMeshRef = dtAllocNavMesh();

		// Could not allocate memory for the nav mesh for some reason.
		if (!NewNavMesh->NavMeshRef)
		{
			AIMESH_UnloadNavMesh();
			fclose(OpenedNavFile);
			CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_FAILED;
			return EAINavMeshLoadResult::NAVMESH_STATUS_ALLOCFAIL;
		}

		NewNavMesh->TileCacheRef = dtAllocTileCache();

		// Could not allocate memory for the tile cache for some reason.
		if (!NewNavMesh->TileCacheRef)
		{
			AIMESH_UnloadNavMesh();
			fclose(OpenedNavFile);
			CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_FAILED;
			return EAINavMeshLoadResult::NAVMESH_STATUS_ALLOCFAIL;
		}

		NewNavMesh->NavQueryRef = dtAllocNavMeshQuery();

		// Could not allocate memory for the nav query for some reason.
		if (!NewNavMesh->NavQueryRef)
		{
			AIMESH_UnloadNavMesh();
			fclose(OpenedNavFile);
			CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_FAILED;
			return EAINavMeshLoadResult::NAVMESH_STATUS_ALLOCFAIL;
		}

		dtStatus MeshInitStatus = NewNavMesh->NavMeshRef->init(&tcHeader.meshParams);

		// Could not initialize the nav mesh for some reason (possibly bad data?)
		if (dtStatusFailed(MeshInitStatus))
		{
			AIMESH_UnloadNavMesh();
			fclose(OpenedNavFile);
			CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_FAILED;
			return EAINavMeshLoadResult::NAVMESH_STATUS_MESHINITFAIL;
		}

		NewNavMesh->TileCacheAllocator = new LinearAllocator(32000);
		NewNavMesh->TileCacheCompressor = new FastLZCompressor;
		NewNavMesh->TileCacheMeshProcess = new MeshProcess;

		LinearAllocator* Allocator = static_cast<LinearAllocator*>(NewNavMesh->TileCacheAllocator);

		if (!Allocator || !Allocator->buffer || !NewNavMesh->TileCacheCompressor || !NewNavMesh->TileCacheMeshProcess)
		{
			AIMESH_UnloadNavMesh();
			fclose(OpenedNavFile);
			CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_FAILED;
			return EAINavMeshLoadResult::NAVMESH_STATUS_ALLOCFAIL;
		}

		dtStatus TileCacheInitStatus = NewNavMesh->TileCacheRef->init(&tcHeader.cacheParams, NewNavMesh->TileCacheAllocator, NewNavMesh->TileCacheCompressor, NewNavMesh->TileCacheMeshProcess);

		// Could not initialize the tile cache for some reason (possibly bad data?)
		if (dtStatusFailed(TileCacheInitStatus))
		{
			AIMESH_UnloadNavMesh();
			fclose(OpenedNavFile);
			CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_FAILED;
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
				AIMESH_UnloadNavMesh();
				fclose(OpenedNavFile);
				CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_FAILED;
				return EAINavMeshLoadResult::NAVMESH_LOAD_INVALID;
			}

			dtCompressedTileRef tile = 0;
			dtStatus addTileStatus = NewNavMesh->TileCacheRef->addTile(data, tileHeader.dataSize, DT_COMPRESSEDTILE_FREE_DATA, &tile);

			// Invalid tile data
			if (dtStatusFailed(addTileStatus))
			{
				dtFree(data);
				AIMESH_UnloadNavMesh();
				fclose(OpenedNavFile);
				CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_FAILED;
				return EAINavMeshLoadResult::NAVMESH_LOAD_INVALID;
			}

			if (tile)
			{
				NewNavMesh->TileCacheRef->buildNavMeshTile(tile, NewNavMesh->NavMeshRef);
			}
		}

		const dtStatus QueryInitStatus = NewNavMesh->NavQueryRef->init(NewNavMesh->NavMeshRef, MAX_NAV_QUERY_NODES);

		if (dtStatusFailed(QueryInitStatus))
		{
			AIMESH_UnloadNavMesh();
			fclose(OpenedNavFile);
			CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_FAILED;
			return EAINavMeshLoadResult::NAVMESH_STATUS_QUERYINITFAIL;
		}

		const EAINavMeshIndex NewIndex = static_cast<EAINavMeshIndex>(i);

		fseek(OpenedNavFile, tcHeader.OffMeshConsOffset, SEEK_SET);

		OffMeshConnectionHandle NewConnectionHandle;

		for (int ii = 0; ii < tcHeader.NumOffMeshCons; ii++)
		{
			dtOffMeshConnection def;

			size_t ConnectionReadCode = fread(&def, sizeof(dtOffMeshConnection), 1, OpenedNavFile);

			if (ConnectionReadCode != 1) { break; }

			Vector Start = UTIL_VecDetourToGoldSrc(def.pos);
			Vector End = UTIL_VecDetourToGoldSrc(&def.pos[3]);

			AIMESH_AddOffMeshConnectionInternal(NewIndex, Start, End, static_cast<EAINavArea>(def.area), static_cast<EAINavMovementFlag>(def.flags), def.bBiDir, NewConnectionHandle, false);
		}

		fseek(OpenedNavFile, tcHeader.NavHintsOffset, SEEK_SET);

		for (int ii = 0; ii < tcHeader.NumNavHints; ii++)
		{
			NavHint def;

			size_t HintReadCode = fread(&def, sizeof(NavHint), 1, OpenedNavFile);

			if (HintReadCode != 1) { break; }

			Vector Position = UTIL_VecDetourToGoldSrc(def.Position);

			AIMESH_AddHintToNavmesh(NewIndex, Position, def.HintTypes);
		}
	}

	fclose(OpenedNavFile);

	CurrentNavMeshStatus = EAINavMeshStatus::NAVMESH_STATUS_SUCCESS;
	return EAINavMeshLoadResult::NAVMESH_LOAD_SUCCESS;
}

void AIMESH_AddHintToNavmesh(EAINavMeshIndex TargetNavMesh, Vector Location, unsigned int HintFlags)
{
	if (!IsValidNavMeshIndex(static_cast<int>(TargetNavMesh))) { return; }

	NavMesh* FoundNavMesh = AIMESH_GetNavMeshAtIndex(TargetNavMesh);

	if (!FoundNavMesh) { return; }

	NavHint NewHint;
	NewHint.Position = Location;
	NewHint.HintTypes = HintFlags;

	FoundNavMesh->MeshHints.push_back(NewHint);
}

static bool AIMESH_AddOffMeshConnectionInternal(EAINavMeshIndex TargetNavMesh, Vector StartLoc, Vector EndLoc, EAINavArea area, EAINavMovementFlag flags, bool bBiDirectional, OffMeshConnectionHandle& OutHandle, bool bProjectEndpoints)
{
	OutHandle.Invalidate();

	NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(TargetNavMesh);

	if (!FoundMesh) { return false; }

	Vector ProjectedStart = bProjectEndpoints ? AIMESH_ProjectPointToNavmesh(TargetNavMesh, StartLoc) : StartLoc;
	Vector ProjectedEnd = bProjectEndpoints ? AIMESH_ProjectPointToNavmesh(TargetNavMesh, EndLoc) : EndLoc;

	if (vIsZero(ProjectedStart) || vIsZero(ProjectedEnd)) { return false; }

	NavOffMeshConnection NewConnectionDef;
	NewConnectionDef.NavMeshIndex = TargetNavMesh;
	NewConnectionDef.FromLocation = ProjectedStart;
	NewConnectionDef.ToLocation = ProjectedEnd;
	NewConnectionDef.DefaultConnectionFlags = flags;
	NewConnectionDef.ConnectionFlags = flags;
	NewConnectionDef.bIsBiDirectional = bBiDirectional;

	// Now flip the coordinates for Detour

	float ConvProjectedStart[3];
	float ConvProjectedEnd[3];

	UTIL_VecGoldSrcToDetour(ProjectedStart, ConvProjectedStart);
	UTIL_VecGoldSrcToDetour(ProjectedEnd, ConvProjectedEnd);

	dtOffMeshConnectionRef dtRef = 0;
	NewConnectionDef.ConnectionRef = 0;

	dtStatus AddStatus = FoundMesh->TileCacheRef->addOffMeshConnection(ConvProjectedStart, ConvProjectedEnd, 18.0f, static_cast<unsigned char>(area), static_cast<unsigned int>(flags), bBiDirectional, &dtRef);

	if (dtStatusSucceed(AddStatus))
	{
		const uint32_t NewConnectionId = static_cast<uint32_t>(dtRef);

		NewConnectionDef.ConnectionRef = NewConnectionId;

		auto InsertResult = FoundMesh->MeshConnections.try_emplace(NewConnectionId, NewConnectionDef);

		if (!InsertResult.second)
		{
			return false;
		}

		OutHandle.ConnectionId = NewConnectionId;
		OutHandle.Generation = CurrentNavMeshGeneration;
		OutHandle.MeshIndex = TargetNavMesh;

		FoundMesh->RefreshOffMeshConnectionHandles();

		return true;
	}

	return false;
}

bool AIMESH_AddOffMeshConnection(EAINavMeshIndex TargetNavMesh, Vector StartLoc, Vector EndLoc, EAINavArea area, EAINavMovementFlag flags, bool bBiDirectional, OffMeshConnectionHandle& OutHandle)
{
	return AIMESH_AddOffMeshConnectionInternal(TargetNavMesh, StartLoc, EndLoc, area, flags, bBiDirectional, OutHandle, true);
}

bool AIMESH_ModifyOffMeshConnectionFlag(OffMeshConnectionHandle& ConnectionHandle, const EAINavMovementFlag NewFlag)
{
	// Don't do anything if the connection is invalid, or already has the desired flags set
	if (!ConnectionHandle.IsValid())
	{
		ConnectionHandle.Invalidate();

		return false;
	}

	NavOffMeshConnection* Connection = ConnectionHandle.GetConnectionRef_Mutable();

	if (!Connection)
	{
		ConnectionHandle.Invalidate();

		return false;
	}

	NavMesh* ParentMesh = AIMESH_GetNavMeshAtIndex(Connection->NavMeshIndex);

	if (!ParentMesh)
	{
		ConnectionHandle.Invalidate();
		return false;
	}

	if (Connection->ConnectionFlags == NewFlag) { return true; }

	const dtStatus ModifyStatus = ParentMesh->TileCacheRef->modifyOffMeshConnection(Connection->ConnectionRef, static_cast<unsigned int>(NewFlag), ParentMesh->NavMeshRef);

	if (dtStatusFailed(ModifyStatus)) { return false; }

	Connection->ConnectionFlags = NewFlag;
	return true;
}

bool AIMESH_RestoreDefaultOffMeshConnectionFlags(OffMeshConnectionHandle& ConnectionHandle)
{
	// Don't do anything if the connection is invalid, or already has the desired flags set
	if (!ConnectionHandle.IsValid())
	{
		ConnectionHandle.Invalidate();
		return false;
	}

	const NavOffMeshConnection* Connection = ConnectionHandle.GetConnectionRef();

	if (!Connection)
	{
		ConnectionHandle.Invalidate();
		return false;
	}

	return AIMESH_ModifyOffMeshConnectionFlag(ConnectionHandle, Connection->DefaultConnectionFlags);
}

bool AIMESH_RemoveOffMeshConnection(OffMeshConnectionHandle& ConnectionHandle)
{
	if (!ConnectionHandle.IsValid())
	{
		ConnectionHandle.Invalidate();

		return false;
	}

	NavMesh* ParentNavMesh = AIMESH_GetNavMeshAtIndex(ConnectionHandle.MeshIndex);

	if (!ParentNavMesh)
	{
		ConnectionHandle.Invalidate();

		return false;
	}

	const NavOffMeshConnection* RemoveConnectionDef = ConnectionHandle.GetConnectionRef();

	if (!RemoveConnectionDef)
	{
		ConnectionHandle.Invalidate();

		return false;
	}

	dtStatus RemoveStatus = ParentNavMesh->TileCacheRef->removeOffMeshConnection(RemoveConnectionDef->ConnectionRef);

	if (dtStatusSucceed(RemoveStatus))
	{
		ParentNavMesh->RemoveOffMeshConnectionFromList(ConnectionHandle.ConnectionId);
		ParentNavMesh->RefreshOffMeshConnectionHandles();

		ConnectionHandle.Invalidate();

		return true;
	}
	else
	{
		return false;
	}
}

EAINavArea AIMESH_GetNavAreaAtLocation(const NavAgentProfile* NavProfile, const Vector Location)
{
	if (!NavProfile) { return EAINavArea::NAV_AREA_NULL; }

	const NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(NavProfile->MeshIndex);

	if (!FoundMesh) { return EAINavArea::NAV_AREA_NULL; }

	const dtQueryFilter* m_navFilter = &NavProfile->Filters;

	const Vector TraceHit = UTIL_GetTraceHitLocation(Location + Vector(0.0f, 0.0f, 10.0f), Location - Vector(0.0f, 0.0f, 500.0f));

	const Vector PointToProject = (TraceHit != g_vecZero) ? TraceHit : Location;

	float dtLocation[3];
	UTIL_VecGoldSrcToDetour(PointToProject, dtLocation);

	dtPolyRef FoundPoly;
	float NavNearest[3];

	const dtStatus success = FoundMesh->NavQueryRef->findNearestPoly(PointToProject, dtDefaultReachableExtents, m_navFilter, &FoundPoly, NavNearest);

	if (dtStatusSucceed(success))
	{
		unsigned char area = 0;
		FoundMesh->NavMeshRef->getPolyArea(FoundPoly, &area);
		return static_cast<EAINavArea>(area);
	}

	return EAINavArea::NAV_AREA_NULL;
}

Vector AIMESH_AdjustPointAwayFromNavWall(const NavAgentProfile* NavProfile, const Vector& Location, const float MaxDistanceFromWall)
{
	if (!NavProfile) { return Location; }

	const NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(NavProfile->MeshIndex);

	if (!FoundMesh) { return Location; }

	const dtQueryFilter* m_navFilter = &NavProfile->Filters;

	float dtLocation[3] = { 0.0f, 0.0f, 0.0f };
	UTIL_VecGoldSrcToDetour(Location, dtLocation);

	float HitDist = 0.0f;
	float HitPos[3] = { 0.0f, 0.0f, 0.0f };
	float HitNorm[3] = { 0.0f, 0.0f, 0.0f };
	float dtSearchExtents[3] = { 16.0f, 16.0f, 16.0f };
	float dtNearestPoint[3] = { 0.0f, 0.0f, 0.0f };

	dtPolyRef StartPoly = 0;

	dtStatus FindResult = FoundMesh->NavQueryRef->findNearestPoly(dtLocation, dtSearchExtents, m_navFilter, &StartPoly, dtNearestPoint);

	if (!dtStatusSucceed(FindResult))
	{
		return Location;
	}

	dtStatus Result = FoundMesh->NavQueryRef->findDistanceToWall(StartPoly, dtLocation, MaxDistanceFromWall, m_navFilter, &HitDist, HitPos, HitNorm);

	if (dtStatusSucceed(Result) && HitDist < MaxDistanceFromWall)
	{
		const float AdjustDistance = MaxDistanceFromWall - HitDist;

		const Vector HitPosVector = UTIL_VecDetourToGoldSrc(HitPos);
		const Vector HitNormVector = UTIL_VecDetourToGoldSrc(HitNorm);

		const Vector AdjustLocation = Location + (HitNormVector * AdjustDistance);

		if (AIMESH_QuickTraceNavLine(NavProfile, Location, AdjustLocation, Vector(5.0f, 5.0f, 50.0f)))
		{
			return AdjustLocation;
		}
		else
		{
			return Location;
		}
	}

	return Location;
}

bool AIMESH_QuickTraceNavLine(const NavAgentProfile* NavProfile, const Vector StartLocation, const Vector EndLocation, Vector MaxAcceptableExtents)
{
	if (!NavProfile) { return false; }

	const Vector HitEndPoint = AIMESH_TraceNavLine(NavProfile, StartLocation, EndLocation, nullptr, MaxAcceptableExtents);

	if (vIsZero(HitEndPoint)) { return false; }

	return vDist2DSq(HitEndPoint, EndLocation) <= sqrf(fmaxf(MaxAcceptableExtents.x, MaxAcceptableExtents.y))
		&& fabsf(HitEndPoint.z - EndLocation.z) <= MaxAcceptableExtents.z;
}

Vector AIMESH_TraceNavLine(const NavAgentProfile* NavProfile, const Vector StartLocation, const Vector EndLocation, NavHitResult* HitResult, Vector MaxAcceptableExtents)
{
	if (HitResult)
	{
		HitResult->Clear();
	}

	if (!NavProfile) { return ZERO_VECTOR; }

	const NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(NavProfile->MeshIndex);

	if (!FoundMesh) { return ZERO_VECTOR; }

	const dtQueryFilter* m_Filter = &NavProfile->Filters;

	float dtStartLocation[3];
	UTIL_VecGoldSrcToDetour(StartLocation, dtStartLocation);

	float dtEndLocation[3];
	UTIL_VecGoldSrcToDetour(EndLocation, dtEndLocation);

	dtPolyRef StartPoly;
	float dtStartNearest[3];

	const float MaxReachableExtents[3] = { MaxAcceptableExtents.x, MaxAcceptableExtents.z, MaxAcceptableExtents.y };

	const dtStatus FoundStartPoly = FoundMesh->NavQueryRef->findNearestPoly(dtStartLocation, MaxReachableExtents, m_Filter, &StartPoly, dtStartNearest);

	if (!dtStatusSucceed(FoundStartPoly))
	{
		if (HitResult)
		{
			HitResult->bStartOffMesh = true;
		}

		return ZERO_VECTOR;
	}

	dtPolyRef PolyPath[MAX_PATH_POLY];
	int PathCount = 0;

	float HitDist = 0.0f;
	float HitNormal[3] = { 0.0f, 0.0f, 0.0f };

	const dtStatus RaycastResult = FoundMesh->NavQueryRef->raycast(StartPoly, dtStartNearest, dtEndLocation, m_Filter, &HitDist, HitNormal, PolyPath, &PathCount, MAX_AI_PATH_SIZE);

	HitDist = clampf(HitDist, 0.0f, 1.0f);

	if (dtStatusFailed(RaycastResult))
	{
		return ZERO_VECTOR;
	}

	if (HitResult)
	{
		HitResult->flFraction = HitDist;
		HitResult->bStartOffMesh = false;
	}

	float dtEndNearest[3] = { 0.0f, 0.0f, 0.0f };

	dtVlerp(dtEndNearest, dtStartNearest, dtEndLocation, HitDist);

	float Height = 0.0f;

	dtStatus PolyHeightStatus = FoundMesh->NavQueryRef->getPolyHeight(PolyPath[PathCount - 1], dtEndNearest, &Height);

	if (dtStatusSucceed(PolyHeightStatus))
	{
		dtEndNearest[1] = Height;
	}

	const Vector TraceEndPoint = UTIL_VecDetourToGoldSrc(dtEndNearest);
	const Vector HitNormalVector = UTIL_VecDetourToGoldSrc(HitNormal);

	if (HitResult)
	{
		HitResult->TraceEndPoint = TraceEndPoint;
		HitResult->HitNormal = HitNormalVector;
	}

	return TraceEndPoint;
}

NavTempObstacleHandle AIMESH_AddTemporaryObstacle(EAINavMeshIndex TargetNavMesh, Vector Position, float Radius, float Height, EAINavArea Area)
{
	NavTempObstacleHandle Result = NavTempObstacleHandle();

	NavMesh* ParentNavMesh = AIMESH_GetNavMeshAtIndex(TargetNavMesh);

	if (!ParentNavMesh) { return Result; }

	// Convert to Detour coordinate system, and adjust so position is the centre of the obstacle rather than bottom
	float Pos[3] = { 0.0f, 0.0f, 0.0f };
	UTIL_VecGoldSrcToDetour(Position, Pos);

	// In Detour, an obstacle's origin is at the bottom centre, whereas we pass in the centre of the desired obstacle. Adjust accordingly.
	Pos[1] -= Height * 0.5f;

	dtObstacleRef ObsRef = 0;
	const dtStatus status = ParentNavMesh->TileCacheRef->addObstacle(Pos, Radius, Height, static_cast<int>(Area), &ObsRef);

	if (!dtStatusSucceed(status) || ObsRef == 0) { return Result; }

	NavTempObstacle NewObstacle;
	NewObstacle.NavMeshIndex = TargetNavMesh;
	NewObstacle.Location = Position;
	NewObstacle.Radius = Radius;
	NewObstacle.Height = Height;
	NewObstacle.Area = Area;
	NewObstacle.ObstacleRef = (unsigned int)ObsRef;

	ParentNavMesh->TempObstacles.try_emplace(ObsRef, NewObstacle);

	Result.ObstacleId = (uint32_t)ObsRef;
	Result.MeshIndex = TargetNavMesh;
	Result.Generation = CurrentNavMeshGeneration;

	return Result;
}

bool AIMESH_RemoveTemporaryObstacle(NavTempObstacleHandle& ObstacleToRemove)
{
	if (!ObstacleToRemove.IsValid()) { return false; }

	NavMesh* ParentMesh = AIMESH_GetNavMeshAtIndex(ObstacleToRemove.MeshIndex);

	if (!ParentMesh)
	{
		ObstacleToRemove.Invalidate();
		return false;
	}

	dtStatus status = ParentMesh->TileCacheRef->removeObstacle(ObstacleToRemove.ObstacleId);

	const bool bSuccessful = dtStatusSucceed(status);

	if (bSuccessful)
	{
		ParentMesh->RemoveTempObstacleFromList(ObstacleToRemove);
	}

	return bSuccessful;
}

Vector AIMESH_ProjectPointToNavmesh(const EAINavMeshIndex MeshIndex, const Vector Location, const Vector Extents)
{
	NavAgentProfile DummyProfile;
	DummyProfile.MeshIndex = MeshIndex;
	DummyProfile.Filters.setIncludeFlags(static_cast<unsigned int>(EAINavMovementFlag::NAV_FLAG_ALL));
	DummyProfile.Filters.setExcludeFlags(static_cast<unsigned int>(EAINavMovementFlag::NAV_FLAG_DISABLED));

	return AIMESH_ProjectPointToNavmesh(&DummyProfile, Location, Extents);
}

Vector AIMESH_ProjectPointToNavmesh(const NavAgentProfile* NavProfile, const Vector Location, const Vector Extents)
{
	if (!NavProfile) { return ZERO_VECTOR; }

	NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(NavProfile->MeshIndex);

	if (!FoundMesh) { return ZERO_VECTOR; }

	const dtQueryFilter* m_navFilter = &NavProfile->Filters;

	Vector PointToProject = Location;

	float dtCheckLoc[3];
	UTIL_VecGoldSrcToDetour(Location, dtCheckLoc);

	dtPolyRef FoundPoly;
	float dtNavNearest[3];

	dtStatus success = FoundMesh->NavQueryRef->findNearestPoly(dtCheckLoc, Extents, m_navFilter, &FoundPoly, dtNavNearest);

	if (FoundPoly > 0 && dtStatusSucceed(success))
	{
		return UTIL_VecDetourToGoldSrc(dtNavNearest);
	}
	else
	{
		int PointContents = UTIL_PointContents(PointToProject);

		if (PointContents != CONTENTS_SOLID && PointContents != CONTENTS_LADDER)
		{
			Vector TraceHit = UTIL_GetTraceHitLocation(PointToProject + Vector(0.0f, 0.0f, 1.0f), PointToProject - Vector(0.0f, 0.0f, 1000.0f));

			PointToProject = (!vIsZero(TraceHit)) ? TraceHit : Location;
		}

		float dtRecheckLoc[3];

		UTIL_VecGoldSrcToDetour(PointToProject, dtRecheckLoc);

		dtStatus successRetry = FoundMesh->NavQueryRef->findNearestPoly(dtRecheckLoc, Extents, m_navFilter, &FoundPoly, dtNavNearest);

		if (FoundPoly > 0 && dtStatusSucceed(success))
		{
			return UTIL_VecDetourToGoldSrc(dtNavNearest);
		}
		else
		{
			return ZERO_VECTOR;
		}
	}

	return ZERO_VECTOR;
}

Vector AIMESH_GetRandomPointOnNavmesh(const NavAgentProfile* NavProfile, const Vector& SearchPoint, bool bIgnoreReachability)
{
	if (!NavProfile || !NavProfile->IsValid()) { return ZERO_VECTOR; }

	NavMesh* QueriedMesh = AIMESH_GetNavMeshAtIndex(NavProfile->MeshIndex);

	if (!QueriedMesh) { return g_vecZero; }

	const dtNavMeshQuery* m_navQuery = QueriedMesh->NavQueryRef;
	const dtQueryFilter* m_navFilter = &NavProfile->Filters;

	if (!m_navQuery) { return g_vecZero; }

	float DetourResult[3];
	memset(DetourResult, 0, sizeof(DetourResult));

	dtStatus FindStatus = 0;

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

		dtStatus FoundPolyResult = m_navQuery->findNearestPoly(SearchOrigin, dtDefaultProjectionExtents, m_navFilter, &RefPoly, NavNearest);

		if (dtStatusFailed(FoundPolyResult))
		{
			return ZERO_VECTOR;
		}

		dtPolyRef RandomPoly;
		const float DefaultSearchRadius = 8192.0f;

		dtStatus FindStatus = m_navQuery->findRandomPointAroundCircle(RefPoly, NavNearest, DefaultSearchRadius, m_navFilter, frand, &RandomPoly, DetourResult);
	}

	if (dtStatusFailed(FindStatus))
	{
		return ZERO_VECTOR;
	}

	return UTIL_VecDetourToGoldSrc(DetourResult);
}

Vector AIMESH_GetRandomPointOnNavmeshInRadius(const NavAgentProfile& NavProfile, const Vector SearchOrigin, const float MaxRadius, bool bIgnoreReachability, EAINavMovementFlag FlagFilter)
{
	NavMesh* QueriedMesh = AIMESH_GetNavMeshAtIndex(NavProfile.MeshIndex);

	if (!QueriedMesh) { return g_vecZero; }

	const NavAgentProfile SearchProfile = (FlagFilter != EAINavMovementFlag::NAV_FLAG_NONE)
		? NavAgentProfile(NavProfile.MeshIndex, FlagFilter, NavProfile.bFlyingProfile)
		: NavProfile;

	const dtNavMeshQuery* m_navQuery = QueriedMesh->NavQueryRef;
	const dtQueryFilter* m_navFilter = &SearchProfile.Filters;

	if (!m_navQuery) { return g_vecZero; }

	float DetourSearchOrigin[3];

	UTIL_VecGoldSrcToDetour(SearchOrigin, DetourSearchOrigin);

	dtPolyRef FoundPoly;
	float NavNearest[3];

	dtStatus foundPolyResult = m_navQuery->findNearestPoly(DetourSearchOrigin, dtDefaultProjectionExtents, m_navFilter, &FoundPoly, NavNearest);

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

Vector AIMESH_GetRandomPointOnNavmeshInDonut(const NavAgentProfile& NavProfile, const Vector origin, const float MinRadius, const float MaxRadius, bool bIgnoreReachability, EAINavMovementFlag FlagFilter)
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

bool AIMESH_IsPointOnNavmesh(const EAINavMeshIndex MeshIndex, const Vector Location, const NavAgentProfile* NavProfile, const Vector SearchExtents)
{
	NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(MeshIndex);

	if (!FoundMesh) { return false; }

	if (!NavProfile) { return false; }

	float dtCheckLoc[3];
	UTIL_VecGoldSrcToDetour(Location, dtCheckLoc);

	float dtCheckExtents[3];
	UTIL_VecGoldSrcToDetour(SearchExtents, dtCheckExtents);

	dtPolyRef FoundPoly;
	float NavNearest[3];

	dtStatus success = FoundMesh->NavQueryRef->findNearestPoly(dtCheckLoc, dtCheckExtents, &NavProfile->Filters, &FoundPoly, NavNearest);

	return dtStatusSucceed(success) && FoundPoly > 0;
}

void AIMESH_DEBUG_DrawOffMeshConnections(EAINavMeshIndex MeshIndex, float DrawTime)
{
	NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(MeshIndex);

	if (!FoundMesh) { return; }

	const edict_t* ListenServerEdict = INDEXENT(1);

	if (FNullEnt(ListenServerEdict)) { return; }

	int NumDrawn = 0;

	const int32 TotalConnections = FoundMesh->TileCacheRef->getOffMeshCount();

	if (TotalConnections == 0) { return; }

	const int32 StartingConnectionIndex = ConnectionDrawIndex;

	while (NumDrawn < 10)
	{
		const dtOffMeshConnection* con = FoundMesh->TileCacheRef->getOffMeshConnection(ConnectionDrawIndex);

		if (con->state != DT_OFFMESH_EMPTY && con->state != DT_OFFMESH_REMOVING)
		{
			Vector StartLine = UTIL_VecDetourToGoldSrc(con->pos);
			Vector EndLine = UTIL_VecDetourToGoldSrc(&con->pos[3]);

			unsigned char r, g, b;

			GetDebugColorForFlag(static_cast<EAINavMovementFlag>(con->flags), r, g, b);

			UTIL_DrawLine(INDEXENT(1), StartLine, EndLine, DrawTime, r, g, b);

			NumDrawn++;
		}

		ConnectionDrawIndex++;

		if (ConnectionDrawIndex > TotalConnections - 1)
		{
			ConnectionDrawIndex = 0;
		}

		if (ConnectionDrawIndex == StartingConnectionIndex) { break; }
	}
}

void AIMESH_DEBUG_DrawTemporaryObstacles(EAINavMeshIndex MeshIndex, float DrawTime)
{
	NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(MeshIndex);

	if (!FoundMesh) { return; }

		int NumObstacles = FoundMesh->TileCacheRef->getObstacleCount();

		for (int i = 0; i < NumObstacles; i++)
		{
			const dtTileCacheObstacle* ObstacleRef = FoundMesh->TileCacheRef->getObstacle(i);

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

void NavMesh::RefreshOffMeshConnectionHandles()
{
	MeshConnectionHandles.clear();

	for (auto it = MeshConnections.begin(); it != MeshConnections.end(); it++)
	{
		const uint32_t ThisConnectionId = it->first;
		NavOffMeshConnection* ThisConnection = &it->second;

		if (!ThisConnection || !ThisConnection->IsValid()) { continue; }

		OffMeshConnectionHandle NewHandle;
		NewHandle.ConnectionId = ThisConnectionId;
		NewHandle.Generation = CurrentNavMeshGeneration;
		NewHandle.MeshIndex = MeshIndex;

		MeshConnectionHandles.push_back(NewHandle);
	}
}

void NavMesh::RemoveOffMeshConnectionFromList(uint32_t ConnectionId)
{
	if (ConnectionId == 0) { return; }

	MeshConnections.erase(ConnectionId);
}

void NavMesh::RemoveTempObstacleFromList(NavTempObstacleHandle& ObstacleToRemove)
{
	if (!ObstacleToRemove.IsValid())
	{
		ObstacleToRemove.Invalidate();
		return;
	}

	if (MeshIndex != ObstacleToRemove.MeshIndex) { return; }

	const uint32_t ObstacleIndex = ObstacleToRemove.ObstacleId;

	ObstacleToRemove.Invalidate();

	auto FoundObstacleIt = TempObstacles.find(ObstacleIndex);

	if (FoundObstacleIt == TempObstacles.end()) { return; }

	TempObstacles.erase(FoundObstacleIt);
}

bool OffMeshConnectionHandle::IsValid() const
{
	return ConnectionId > 0 && Generation > 0 && Generation == CurrentNavMeshGeneration && IsValidNavMeshIndex(static_cast<int>(MeshIndex));
}

const NavOffMeshConnection* OffMeshConnectionHandle::GetConnectionRef() const
{
	if (!IsValid()) { return nullptr; }

	NavMesh* ParentMesh = AIMESH_GetNavMeshAtIndex(MeshIndex);

	if (!ParentMesh || !ParentMesh->IsValid()) { return nullptr; }

	auto FoundConnectionIt = ParentMesh->MeshConnections.find(ConnectionId);

	if (FoundConnectionIt == ParentMesh->MeshConnections.end()) { return nullptr; }

	const NavOffMeshConnection* FoundConnection = &FoundConnectionIt->second;

	if (!FoundConnection || !FoundConnection->IsValid()) { return nullptr; }

	return FoundConnection;
}

NavOffMeshConnection* OffMeshConnectionHandle::GetConnectionRef_Mutable() const
{
	if (!IsValid()) { return nullptr; }

	NavMesh* ParentMesh = AIMESH_GetNavMeshAtIndex(MeshIndex);

	if (!ParentMesh || !ParentMesh->IsValid()) { return nullptr; }

	auto FoundConnectionIt = ParentMesh->MeshConnections.find(ConnectionId);

	if (FoundConnectionIt == ParentMesh->MeshConnections.end()) { return nullptr; }

	NavOffMeshConnection* FoundConnection = &FoundConnectionIt->second;

	if (!FoundConnection || !FoundConnection->IsValid()) { return nullptr; }

	return FoundConnection;
}

bool NavTempObstacleHandle::IsValid() const
{
	return ObstacleId > 0 && Generation > 0 && Generation == CurrentNavMeshGeneration && IsValidNavMeshIndex(static_cast<int>(MeshIndex));
}

const NavTempObstacle* NavTempObstacleHandle::GetObstacleRef() const
{
	if (!IsValid()) { return nullptr; }

	NavMesh* ParentMesh = AIMESH_GetNavMeshAtIndex(MeshIndex);

	if (!ParentMesh || !ParentMesh->IsValid()) { return nullptr; }

	auto FoundObstacleIt = ParentMesh->TempObstacles.find(ObstacleId);

	if (FoundObstacleIt == ParentMesh->TempObstacles.end()) { return nullptr; }

	const NavTempObstacle* FoundObstacle = &FoundObstacleIt->second;

	if (!FoundObstacle || !FoundObstacle->IsValid()) { return nullptr; }

	return FoundObstacle;
}

NavTempObstacle* NavTempObstacleHandle::GetObstacleRef_Mutable() const
{
	if (!IsValid()) { return nullptr; }

	NavMesh* ParentMesh = AIMESH_GetNavMeshAtIndex(MeshIndex);

	if (!ParentMesh || !ParentMesh->IsValid()) { return nullptr; }

	auto FoundObstacleIt = ParentMesh->TempObstacles.find(ObstacleId);

	if (FoundObstacleIt == ParentMesh->TempObstacles.end()) { return nullptr; }

	NavTempObstacle* FoundObstacle = &FoundObstacleIt->second;

	if (!FoundObstacle || !FoundObstacle->IsValid()) { return nullptr; }

	return FoundObstacle;
}
