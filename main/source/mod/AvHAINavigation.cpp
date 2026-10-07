//
// EvoBot - Neoptolemus' Natural Selection bot, based on Botman's HPB bot template
//
// bot_navigation.cpp
//
// Handles all bot path finding and movement
//

#include "AvHAINavigation.h"
#include "AvHAIMath.h"
#include "AvHAIPlayerUtil.h"
#include "AvHAIHelper.h"
#include "AvHAIPlayerManager.h"
#include "AvHAITactical.h"
#include "AvHAITask.h"
#include "AvHAIWeaponHelper.h"
#include "AvHAIConfig.h"

#include "AvHWeldable.h"
#include "AvHServerUtil.h"
#include "AvHGamerules.h"

#include "../../dlls/triggers.h"

#include <stdlib.h>
#include <math.h>

#include "../../dlls/plats.h"

#include "DetourNavMesh.h"
#include "DetourCommon.h"
#include "DetourTileCache.h"
#include "DetourTileCacheBuilder.h"
#include "DetourNavMeshBuilder.h"
#include "fastlz/fastlz.h"
#include "DetourAlloc.h"

#include <cfloat>

std::vector<NavAgentProfile> BaseAgentProfiles;

bool AINAV_IsPointReachable(const NavAgentProfile* NavProfile, const Vector& FromLocation, const Vector& ToLocation, float MaxAcceptableDistance)
{
	if (!NavProfile) { return false; }

	NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(NavProfile->MeshIndex);

	if (!FoundMesh) { return false; }

	const dtQueryFilter* m_navFilter = &NavProfile->Filters;

	bool bStartInWater = UTIL_IsPointInSwimArea(FromLocation);
	bool bEndInWater = UTIL_IsPointInSwimArea(ToLocation);

	if (bStartInWater && bEndInWater)
	{
		if (UTIL_QuickHullTrace(nullptr, FromLocation, ToLocation)) { return true; }
	}

	float dtStartPos[3];
	float dtEndPos[3];

	UTIL_VecGoldSrcToDetour(FromLocation, dtStartPos);
	UTIL_VecGoldSrcToDetour(ToLocation, dtEndPos);

	if (bStartInWater)
	{
		TraceResult Hit;
		UTIL_TraceLine(FromLocation, FromLocation - Vector(0.0f, 0.0f, 1000.0f), ignore_monsters, nullptr, &Hit);

		if (Hit.flFraction < 1.0f)
		{
			UTIL_VecGoldSrcToDetour(Hit.vecEndPos, dtStartPos);
		}
	}

	if (bEndInWater)
	{
		TraceResult Hit;
		UTIL_TraceLine(ToLocation, ToLocation - Vector(0.0f, 0.0f, 1000.0f), ignore_monsters, nullptr, &Hit);

		if (Hit.flFraction < 1.0f)
		{
			UTIL_VecGoldSrcToDetour(Hit.vecEndPos, dtEndPos);
		}
	}

	dtStatus status;
	dtPolyRef StartPoly;
	float dtStartNearest[3];
	dtPolyRef EndPoly;
	float dtEndNearest[3];
	dtPolyRef PolyPath[MAX_PATH_POLY];
	int nPathCount = 0;

	float searchExtents[3] = { MaxAcceptableDistance, MaxAcceptableDistance, MaxAcceptableDistance };

	// find the start polygon
	status = FoundMesh->NavQueryRef->findNearestPoly(dtStartPos, searchExtents, m_navFilter, &StartPoly, dtStartNearest);
	if (!dtStatusSucceed(status))
	{
		return false; // couldn't find a polygon
	}

	// find the end polygon
	status = FoundMesh->NavQueryRef->findNearestPoly(dtEndPos, searchExtents, m_navFilter, &EndPoly, dtEndNearest);
	if (!dtStatusSucceed(status))
	{
		return false; // couldn't find a polygon
	}

	status = FoundMesh->NavQueryRef->findPath(StartPoly, EndPoly, dtStartNearest, dtEndNearest, m_navFilter, PolyPath, &nPathCount, MAX_PATH_POLY);

	if (nPathCount == 0)
	{
		return false; // couldn't find a path
	}

	if (PolyPath[nPathCount - 1] != EndPoly)
	{
		float dtEndPoint[3];
		dtVcopy(dtEndPoint, dtEndNearest);

		FoundMesh->NavQueryRef->closestPointOnPoly(PolyPath[nPathCount - 1], dtEndNearest, dtEndPoint, 0);

		if (dtVdistSqr(dtEndNearest, dtEndPoint) <= sqrf(MaxAcceptableDistance))
		{
			return true;
		}
		else
		{
			Vector FinalEndPosition = UTIL_VecDetourToGoldSrc(dtEndPoint);

			if (UTIL_IsPointInSwimArea(FinalEndPosition) && UTIL_IsPointInSwimArea(ToLocation))
			{
				return UTIL_QuickHullTrace(nullptr, FinalEndPosition, ToLocation);
			}
		}
	}

	return true;
}

bool AINAV_IsPointDirectlyReachable(const NavAgentProfile* NavProfile, const Vector& FromLocation, const Vector& ToLocation, float MaxAcceptableDistance)
{
	return AIMESH_QuickTraceNavLine(NavProfile, FromLocation, ToLocation, MaxAcceptableDistance);
}

Vector AINAV_FindClosestNavigablePointTo(const NavAgentProfile* NavProfile, const Vector& FromLocation, const Vector& ToLocation)
{
	if (!NavProfile) { return FromLocation; }

	NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(NavProfile->MeshIndex);

	if (!FoundMesh) { return FromLocation; }

	const dtQueryFilter* m_navFilter = &NavProfile->Filters;

	bool bStartInWater = UTIL_IsPointInSwimArea(FromLocation);
	bool bEndInWater = UTIL_IsPointInSwimArea(ToLocation);

	if (bStartInWater && bEndInWater)
	{
		if (UTIL_QuickHullTrace(nullptr, FromLocation, ToLocation)) { return ToLocation; }
	}

	float dtStartPos[3];
	float dtEndPos[3];

	UTIL_VecGoldSrcToDetour(FromLocation, dtStartPos);
	UTIL_VecGoldSrcToDetour(ToLocation, dtEndPos);

	if (bStartInWater)
	{
		TraceResult Hit;
		UTIL_TraceLine(FromLocation, FromLocation - Vector(0.0f, 0.0f, 1000.0f), ignore_monsters, nullptr, &Hit);

		if (Hit.flFraction < 1.0f)
		{
			UTIL_VecGoldSrcToDetour(Hit.vecEndPos, dtStartPos);
		}
	}

	if (bEndInWater)
	{
		TraceResult Hit;
		UTIL_TraceLine(ToLocation, ToLocation - Vector(0.0f, 0.0f, 1000.0f), ignore_monsters, nullptr, &Hit);

		if (Hit.flFraction < 1.0f)
		{
			UTIL_VecGoldSrcToDetour(Hit.vecEndPos, dtEndPos);
		}
	}

	dtStatus status;
	dtPolyRef StartPoly;
	float dtStartNearest[3];
	dtPolyRef EndPoly;
	float dtEndNearest[3];
	dtPolyRef PolyPath[MAX_PATH_POLY];
	int nPathCount = 0;

	float dtSearchExtents[3] = { 400.0f, 400.0f, 400.0f };

	// find the start polygon
	status = FoundMesh->NavQueryRef->findNearestPoly(dtStartPos, dtSearchExtents, m_navFilter, &StartPoly, dtStartNearest);
	if (!dtStatusSucceed(status))
	{
		return FromLocation; // couldn't find a polygon
	}

	// find the end polygon
	status = FoundMesh->NavQueryRef->findNearestPoly(dtEndPos, dtSearchExtents, m_navFilter, &EndPoly, dtEndNearest);
	if (!dtStatusSucceed(status))
	{
		return FromLocation; // couldn't find a polygon
	}

	status = FoundMesh->NavQueryRef->findPath(StartPoly, EndPoly, dtStartNearest, dtEndNearest, m_navFilter, PolyPath, &nPathCount, MAX_PATH_POLY);

	if (nPathCount == 0)
	{
		return FromLocation; // couldn't find a path
	}

	return UTIL_VecDetourToGoldSrc(dtEndNearest);
}

Vector AINAV_AdjustPointForPathfinding(const NavAgentProfile* NavProfile, const Vector& Point)
{
	const Vector ProjectedPoint = AIMESH_ProjectPointToNavmesh(NavProfile, Point);

	if (vIsZero(ProjectedPoint)) { return Point; }

	int PointContents = UTIL_PointContents(ProjectedPoint);

	if (PointContents == CONTENTS_SOLID)
	{
		int PointContents = UTIL_PointContents(ProjectedPoint + Vector(0.0f, 0.0f, 32.0f));

		if (PointContents != CONTENTS_SOLID && PointContents != CONTENTS_LADDER)
		{
			Vector TraceStart = ProjectedPoint + Vector(0.0f, 0.0f, 32.0f);
			Vector TraceEnd = TraceStart - Vector(0.0f, 0.0f, 50.0f);
			Vector NewPoint = UTIL_GetHullTraceHitLocation(TraceStart, TraceEnd, point_hull);

			if (!vIsZero(NewPoint)) { return NewPoint; }
		}
	}
	else
	{
		Vector TraceStart = ProjectedPoint + Vector(0.0f, 0.0f, 5.0f);
		Vector TraceEnd = TraceStart - Vector(0.0f, 0.0f, 32.0f);
		Vector NewPoint = UTIL_GetHullTraceHitLocation(TraceStart, TraceEnd, point_hull);

		if (!vIsZero(NewPoint)) { return NewPoint; }
	}

	return ProjectedPoint;
}

bool AINAV_FindPathClosestToPoint(const NavAgentProfile* NavProfile, const Vector FromLocation, const Vector ToLocation, AvHAIPath* ResultPath, float MaxAcceptableDistance)
{
	ResultPath->Clear();

	if (!NavProfile || !NavProfile->IsValid()) { return false; }

	if (vEquals(FromLocation, ToLocation)) { return false; }

	// First check: if we're swimming, see if we can just swim directly to it!
	if (UTIL_IsPointInSwimArea(FromLocation) && UTIL_IsPointInSwimArea(ToLocation))
	{
		TraceResult Hit;

		UTIL_TraceHull(FromLocation, ToLocation, ignore_monsters, head_hull, nullptr, &Hit);

		if (!Hit.fStartSolid && !Hit.fAllSolid)
		{
			if (Hit.flFraction >= 1.0f || vDist3DSq(Hit.vecEndPos, ToLocation) < sqrf(MaxAcceptableDistance))
			{
				AvHAIPathNode StartPoint;
				StartPoint.FromLocation = FromLocation;
				StartPoint.ToLocation = ToLocation;
				StartPoint.MovementArea = EAINavArea::NAV_AREA_WALK;
				StartPoint.MovementFlag = EAINavMovementFlag::NAV_FLAG_WALK;

				ResultPath->PathNodes.push_back(StartPoint);

				return true;
			}
		}
	}

	NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(NavProfile->MeshIndex);

	if (!FoundMesh) { return false; }

	const dtQueryFilter* m_navFilter = &NavProfile->Filters;

	Vector FromFloorLocation = AINAV_FindNewPathStartPoint(NavProfile, ResultPath, FromLocation, ToLocation);

	const DynamicMapObject* CurrentPlatform = AIMAP_TraceForDynamicObject(FromLocation, FromLocation - Vector(0.0f, 0.0f, 1000.0f));
	bool bMustDisembarkLiftFirst = false;
	Vector LiftStart = ZERO_VECTOR;
	Vector LiftEnd = ZERO_VECTOR;

	if (CurrentPlatform)
	{
		LiftEnd = AIMAP_GetNearestPlatformDisembarkPoint(NavProfile, FromLocation, CurrentPlatform);

		if (!vIsZero(LiftEnd))
		{
			FromFloorLocation = LiftEnd;

			const NavOffMeshConnection* LiftOffMesh = AIMAP_GetOffMeshConnectionForPlatform(NavProfile, CurrentPlatform);

			if (LiftOffMesh)
			{
				LiftStart = (vEquals(LiftEnd, LiftOffMesh->ToLocation, 5.0f)) ? LiftOffMesh->FromLocation : LiftOffMesh->ToLocation;
				bMustDisembarkLiftFirst = true;
				FromFloorLocation = LiftEnd;
			}
		}
	}

	Vector ToFloorLocation = AINAV_AdjustPointForPathfinding(NavProfile, ToLocation);

	if (UTIL_IsPointInSwimArea(ToLocation))
	{
		TraceResult Hit;

		UTIL_TraceLine(ToLocation, ToLocation - Vector(0.0f, 0.0f, 1000.0f), ignore_monsters, nullptr, &Hit);

		if (Hit.flFraction < 1.0f)
		{
			ToFloorLocation = Hit.vecEndPos;
		}
	}

	float dtStartPos[3];
	UTIL_VecGoldSrcToDetour(FromFloorLocation, dtStartPos);

	float dtEndPos[3];
	UTIL_VecGoldSrcToDetour(ToFloorLocation, dtStartPos);

	dtStatus status;
	dtPolyRef dtStartPoly;
	float dtStartNearest[3];
	dtPolyRef dtEndPoly;
	float dtEndNearest[3];
	dtPolyRef dtPolyPath[MAX_PATH_POLY];
	dtPolyRef dtStraightPolyPath[MAX_AI_PATH_SIZE];
	int nPathCount = 0;
	float dtStraightPath[MAX_AI_PATH_SIZE * 3];
	unsigned char dtStraightPathFlags[MAX_AI_PATH_SIZE];
	std::memset(dtStraightPathFlags, 0, sizeof(dtStraightPathFlags));
	int nVertCount = 0;

	// find the start polygon
	status = FoundMesh->NavQueryRef->findNearestPoly(dtStartPos, dtDefaultProjectionExtents, m_navFilter, &dtStartPoly, dtStartNearest);
	if ((status & DT_FAILURE) || (status & DT_STATUS_DETAIL_MASK))
	{
		return false; // couldn't find a polygon
	}

	// find the end polygon
	status = FoundMesh->NavQueryRef->findNearestPoly(dtEndPos, dtDefaultProjectionExtents, m_navFilter, &dtEndPoly, dtEndNearest);
	if ((status & DT_FAILURE) || (status & DT_STATUS_DETAIL_MASK))
	{
		return false; // couldn't find a polygon
	}

	status = FoundMesh->NavQueryRef->findPath(dtStartPoly, dtEndPoly, dtStartNearest, dtEndNearest, m_navFilter, dtPolyPath, &nPathCount, MAX_PATH_POLY);

	if (dtPolyPath[nPathCount - 1] != dtEndPoly)
	{
		float epos[3];
		dtVcopy(epos, dtEndNearest);

		FoundMesh->NavQueryRef->closestPointOnPoly(dtPolyPath[nPathCount - 1], dtEndNearest, epos, 0);

		if (dtVdistSqr(dtEndNearest, epos) > sqrf(MaxAcceptableDistance))
		{
			if (!UTIL_IsPointInSwimArea(ToLocation))
			{
				return false;
			}
			else
			{
				TraceResult Hit;
				Vector StartTrace = Vector(epos[0], -epos[2], epos[1] + 5.0f);
				UTIL_TraceLine(StartTrace, ToLocation, ignore_monsters, nullptr, &Hit);

				if (Hit.fAllSolid || (Hit.flFraction < 1.0f && vDist3DSq(Hit.vecEndPos, ToLocation) > sqrf(MaxAcceptableDistance)))
				{
					return DT_FAILURE;
				}
				else
				{
					dtVcopy(dtEndNearest, epos);
				}
			}
		}
		else
		{
			dtVcopy(dtEndNearest, epos);
		}
	}

	status = FoundMesh->NavQueryRef->findStraightPath(dtStartNearest, dtEndNearest, dtPolyPath, nPathCount, dtStraightPath, dtStraightPathFlags, dtStraightPolyPath, &nVertCount, MAX_AI_PATH_SIZE, DT_STRAIGHTPATH_AREA_CROSSINGS);

	if ((status & DT_FAILURE) || (status & DT_STATUS_DETAIL_MASK))
	{
		return false; // couldn't create a path
	}

	if (nVertCount == 0)
	{
		return false; // couldn't find a path
	}

	unsigned int dtCurrFlags;
	unsigned char dtCurrArea;

	FoundMesh->NavMeshRef->getPolyFlags(dtStraightPolyPath[0], &dtCurrFlags);
	FoundMesh->NavMeshRef->getPolyArea(dtStraightPolyPath[0], &dtCurrArea);

	EAINavMovementFlag CurrFlags = static_cast<EAINavMovementFlag>(dtCurrFlags);
	EAINavArea CurrArea = static_cast<EAINavArea>(dtCurrArea);

	// At this point we have our path.  Copy it to the path store
	int nIndex = 0;
	TraceResult hit;

	ResultPath->RequiredMoveFlags = EAINavMovementFlag::NAV_FLAG_NONE;

	Vector NodeFromLocation = FromFloorLocation;

	if (bMustDisembarkLiftFirst)
	{
		AvHAIPathNode StartPathNode;
		StartPathNode.FromLocation = LiftStart;
		StartPathNode.ToLocation = LiftEnd;
		StartPathNode.MovementFlag = EAINavMovementFlag::NAV_FLAG_PLATFORM;
		StartPathNode.MovementArea = EAINavArea::NAV_AREA_WALK;

		ResultPath->PathNodes.push_back(StartPathNode);

		NodeFromLocation = LiftEnd;
	}

	for (int nVert = 0; nVert < nVertCount; nVert++)
	{
		AvHAIPathNode NextPathNode;

		NextPathNode.FromLocation = NodeFromLocation;

		// The nav mesh doesn't always align perfectly with the floor, so align each nav point with the floor after generation
		NextPathNode.ToLocation.x = dtStraightPath[nIndex++];
		NextPathNode.ToLocation.z = dtStraightPath[nIndex++];
		NextPathNode.ToLocation.y = -dtStraightPath[nIndex++];

		NextPathNode.ToLocation = AIMESH_AdjustPointAwayFromNavWall(NavProfile, NextPathNode.ToLocation, 16.0f);

		NextPathNode.ToLocation = AINAV_AdjustPointForPathfinding(NavProfile, NextPathNode.ToLocation);

		if (CurrFlags != EAINavMovementFlag::NAV_FLAG_JUMP || NextPathNode.FromLocation.z > NextPathNode.ToLocation.z)
		{
			float Offset = UTIL_GetHullOffsetFromFloor(NavProfile->PlayerHullIndex).z;

			if (CurrFlags == EAINavMovementFlag::NAV_FLAG_CROUCH)
			{
				Offset *= 0.5f;
			}

			NextPathNode.ToLocation.z += Offset;
		}

		if (CurrFlags == EAINavMovementFlag::NAV_FLAG_PLATFORM)
		{
			const DynamicMapObject* PlatformRef = AIMAP_GetClosestPlatformToPoints(NextPathNode.FromLocation, NextPathNode.ToLocation);

			if (PlatformRef)
			{
				NextPathNode.MovementObject = PlatformRef->Edict;
			}
		}
		else if (CurrFlags == EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM1 || CurrFlags == EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM2)
		{
			const bool bIsTeamOne = (CurrFlags == EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM1);

			StructureSearchFilter PGFilter;
			PGFilter.DeployableTeam = (bIsTeamOne) ? GetGameRules()->GetTeamA()->GetTeamNumber() : GetGameRules()->GetTeamB()->GetTeamNumber();
			PGFilter.IncludeStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_COMPLETED;
			PGFilter.ExcludeStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_RECYCLING;
			PGFilter.MaxSearchRadius = UTIL_MetresToGoldSrcUnits(2.0f);

			if (const AvHAIBuildableStructure* MatchingPhaseGate = AITAC_FindSingleMatchingStructure(NextPathNode.FromLocation, &PGFilter, EAIStructureSortType::FIND_STRUCTURE_NEAREST))
			{
				NextPathNode.MovementObject = MatchingPhaseGate->Edict;
			}
		}

		EnumAddFlags(ResultPath->RequiredMoveFlags, CurrFlags);

		// End alignment to floor

		// For ladders and wall climbing, calculate the climb height needed to complete the move.
		// This what allows bots to climb over railings without having to explicitly place nav points on the railing itself
		NextPathNode.RequiredClimbZ = NextPathNode.ToLocation.z;

		if (CurrFlags == EAINavMovementFlag::NAV_FLAG_LADDER || CurrFlags == EAINavMovementFlag::NAV_FLAG_WALLCLIMB)
		{
			Vector FromLocation = (ResultPath->PathNodes.size() > 0) ? ResultPath->PathNodes.back().ToLocation : FromFloorLocation;
			float NewRequiredZ = AINAV_FindZHeightForClimb(FromLocation, NextPathNode.ToLocation, head_hull);
			NextPathNode.RequiredClimbZ = fmaxf(NewRequiredZ, NextPathNode.ToLocation.z);

			if (CurrFlags == EAINavMovementFlag::NAV_FLAG_LADDER)
			{
				NextPathNode.RequiredClimbZ += 5.0f;
			}

		}
		else
		{
			NextPathNode.RequiredClimbZ = NextPathNode.ToLocation.z;
		}

		NextPathNode.MovementFlag = CurrFlags;
		NextPathNode.MovementArea = CurrArea;
		NextPathNode.MeshPoly = dtStraightPolyPath[nVert];

		FoundMesh->NavMeshRef->getPolyFlags(dtStraightPolyPath[nVert], &dtCurrFlags);
		FoundMesh->NavMeshRef->getPolyArea(dtStraightPolyPath[nVert], &dtCurrArea);

		CurrFlags = static_cast<EAINavMovementFlag>(dtCurrFlags);
		CurrArea = static_cast<EAINavArea>(dtCurrArea);

		NodeFromLocation = NextPathNode.ToLocation;

		ResultPath->PathNodes.push_back(NextPathNode);
	}

	if (UTIL_IsPointInSwimArea(ToLocation))
	{
		AvHAIPathNode FinalSwimBit;
		FinalSwimBit.MovementArea = EAINavArea::NAV_AREA_WALK;
		FinalSwimBit.MovementFlag = EAINavMovementFlag::NAV_FLAG_WALK;
		FinalSwimBit.FromLocation = ResultPath->PathNodes.back().ToLocation;
		FinalSwimBit.ToLocation = ToLocation;

		ResultPath->PathNodes.push_back(FinalSwimBit);
	}

	return true;
}

Vector AINAV_FindNewPathStartPoint(const NavAgentProfile* NavProfile, const AvHAIPath* ExistingPath, const Vector& DesiredStartPoint, const Vector& Destination)
{
	if (!NavProfile || !NavProfile->IsValid()) { return ZERO_VECTOR; }

	const Vector FloorLocation = UTIL_FindFloor(DesiredStartPoint);

	if (UTIL_IsPointInSwimArea(DesiredStartPoint))
	{
		return FloorLocation;
	}

	Vector Result = AINAV_AdjustPointForPathfinding(NavProfile, FloorLocation);

	// If the bot currently has a path, then let's calculate the navigation from the "from" point rather than our exact position right now
	if (ExistingPath && ExistingPath->IsValidPath())
	{
		const AvHAIPathNode* CurrentPathNode = ExistingPath->GetCurrentPathNode();

		if (CurrentPathNode->MovementFlag == EAINavMovementFlag::NAV_FLAG_WALK)
		{
			bool bFromReachable = AINAV_IsPointDirectlyReachable(NavProfile, FloorLocation, CurrentPathNode->ToLocation);
			bool bToReachable = AINAV_IsPointDirectlyReachable(NavProfile, FloorLocation, CurrentPathNode->FromLocation);

			if (bFromReachable && bToReachable)
			{
				return FloorLocation;
			}
			else if (bFromReachable)
			{
				return CurrentPathNode->FromLocation;
			}
			else if (bToReachable)
			{
				return CurrentPathNode->ToLocation;
			}
		}
	}

	// Add a slight bias towards trying to move forward if on a railing or other narrow bit of navigable terrain
	// rather than potentially dropping back off it the wrong way
	Vector GeneralDir = UTIL_GetVectorNormal2D(Destination - FloorLocation);
	Vector CheckLocation = Result + (GeneralDir * 16.0f);

	Vector AdjustedCheckLocation = AINAV_AdjustPointForPathfinding(NavProfile, CheckLocation);

	if (!vIsZero(AdjustedCheckLocation))
	{
		Result = AdjustedCheckLocation;
	}

	return Result;
}

float AINAV_FindZHeightForClimb(const Vector ClimbStart, const Vector ClimbEnd, const int HullNum)
{
	TraceResult hit;

	Vector StartTrace = ClimbEnd;

	UTIL_TraceLine(ClimbEnd, ClimbEnd - Vector(0.0f, 0.0f, 50.0f), ignore_monsters, nullptr, &hit);

	if (hit.fAllSolid || hit.fStartSolid || hit.flFraction < 1.0f)
	{
		StartTrace.z = hit.vecEndPos.z + 18.0f;
	}

	Vector EndTrace = ClimbStart;
	EndTrace.z = StartTrace.z;

	Vector CurrTraceStart = StartTrace;

	UTIL_TraceHull(StartTrace, EndTrace, ignore_monsters, HullNum, nullptr, &hit);

	if (hit.flFraction >= 1.0f && !hit.fAllSolid && !hit.fStartSolid)
	{
		return StartTrace.z;
	}
	else
	{
		int maxTests = 100;
		int testCount = 0;

		while ((hit.flFraction < 1.0f || hit.fStartSolid || hit.fAllSolid) && testCount < maxTests)
		{
			CurrTraceStart.z += 1.0f;
			EndTrace.z = CurrTraceStart.z;
			UTIL_TraceHull(CurrTraceStart, EndTrace, ignore_monsters, HullNum, nullptr, &hit);
			testCount++;
		}

		if (hit.flFraction >= 1.0f && !hit.fStartSolid)
		{
			return CurrTraceStart.z;
		}
		else
		{
			return StartTrace.z;
		}
	}

	return StartTrace.z;
}

Vector AINAV_GetNearestPlatformDisembarkPoint(const NavAgentProfile* NavProfile, const edict_t* Rider, const edict_t* LiftReference)
{
	if (!NavProfile || !UTIL_IsEdictActive(LiftReference) || !UTIL_IsEdictActive(Rider)) { return ZERO_VECTOR; }

	const NavOffMeshConnection* NearestConnection = nullptr;
	float MinDist = 0.0f;

	NavMesh* ChosenNavMesh = AIMESH_GetNavMeshAtIndex(NavProfile->MeshIndex);

	if (!ChosenNavMesh) { return ZERO_VECTOR; }

	for (auto it = ChosenNavMesh->MeshConnections.begin(); it != ChosenNavMesh->MeshConnections.end(); it++)
	{
		const NavOffMeshConnection* ThisConnection = &(*it);

		if (!ThisConnection || !ThisConnection->IsValid()) { continue; }

		if (!EnumHasAnyFlags(ThisConnection->ConnectionFlags, EAINavMovementFlag::NAV_FLAG_PLATFORM)) { continue; }

		if (ThisConnection->LinkedObject == LiftReference)
		{
			float ThisDist = fminf(vDist3DSq(ThisConnection->FromLocation, UTIL_GetClosestPointOnEntityToLocation(ThisConnection->FromLocation, LiftReference)),
				vDist3DSq(ThisConnection->ToLocation, UTIL_GetClosestPointOnEntityToLocation(ThisConnection->ToLocation, LiftReference)));

			if (ThisDist < sqrf(100.0f) && (!NearestConnection || ThisDist < MinDist))
			{
				NearestConnection = ThisConnection;
				MinDist = ThisDist;
			}
		}
	}

	if (NearestConnection)
	{
		Vector NearestPointFromLocation = UTIL_GetClosestPointOnEntityToLocation(NearestConnection->FromLocation, LiftReference);
		NearestPointFromLocation.z = Rider->v.origin.z;

		Vector NearestPointToLocation = UTIL_GetClosestPointOnEntityToLocation(NearestConnection->ToLocation, LiftReference);
		NearestPointToLocation.z = Rider->v.origin.z;

		float DistFromLocation = vDist3DSq(NearestConnection->FromLocation, NearestPointFromLocation);
		float DistToLocation = vDist3DSq(NearestConnection->ToLocation, NearestPointToLocation);
		return (DistFromLocation < DistToLocation) ? NearestConnection->FromLocation : NearestConnection->ToLocation;
	}

	Vector NearestProjectedPoint = ZERO_VECTOR;
	Vector LiftCentre = UTIL_GetCentreOfEntity(LiftReference);
	float DisembarkHeight = (!FNullEnt(Rider)) ? GetPlayerBottomOfCollisionHull(Rider).z : LiftReference->v.absmax.z;

	Vector FrontLocation = Vector(LiftReference->v.absmax.x, LiftCentre.y, DisembarkHeight);
	Vector RearLocation = Vector(LiftReference->v.absmin.x, LiftCentre.y, DisembarkHeight);
	Vector LeftLocation = Vector(LiftCentre.x, LiftReference->v.absmin.y, DisembarkHeight);
	Vector RightLocation = Vector(LiftCentre.x, LiftReference->v.absmax.y, DisembarkHeight);

	float ProjectWidth = fmaxf((LiftReference->v.absmax.x - LiftReference->v.absmin.x) * 0.5f, (LiftReference->v.absmax.y - LiftReference->v.absmin.y) * 0.5f);
	ProjectWidth += 100.0f;

	Vector ProjectedLoc = AIMESH_ProjectPointToNavmesh(NavProfile, FrontLocation, Vector(ProjectWidth, ProjectWidth, 50.0f));

	if (!vIsZero(ProjectedLoc) && !vPointOverlaps2D(ProjectedLoc, LiftReference->v.absmin, LiftReference->v.absmax))
	{
		return ProjectedLoc;
	}

	ProjectedLoc = AIMESH_ProjectPointToNavmesh(NavProfile, RearLocation, Vector(ProjectWidth, ProjectWidth, 50.0f));

	if (!vIsZero(ProjectedLoc) && !vPointOverlaps2D(ProjectedLoc, LiftReference->v.absmin, LiftReference->v.absmax))
	{
		return ProjectedLoc;
	}

	ProjectedLoc = AIMESH_ProjectPointToNavmesh(NavProfile, LeftLocation, Vector(ProjectWidth, ProjectWidth, 50.0f));

	if (!vIsZero(ProjectedLoc) && !vPointOverlaps2D(ProjectedLoc, LiftReference->v.absmin, LiftReference->v.absmax))
	{
		return ProjectedLoc;
	}

	ProjectedLoc = AIMESH_ProjectPointToNavmesh(NavProfile, RightLocation, Vector(ProjectWidth, ProjectWidth, 50.0f));

	if (!vIsZero(ProjectedLoc) && !vPointOverlaps2D(ProjectedLoc, LiftReference->v.absmin, LiftReference->v.absmax))
	{
		return ProjectedLoc;
	}

	return ZERO_VECTOR;
}

bool AINAV_IsOffPathNode(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* PathNode)
{
	if (!NavProfile || !UTIL_IsEdictActive(AIPlayer) || !PathNode || !PathNode->IsValidMove()) { return false; }

	switch (PathNode->MovementFlag)
	{
		case EAINavMovementFlag::NAV_FLAG_WALK:
			return AINAV_IsOffWalkNode(NavProfile, AIPlayer, PathNode);
		case EAINavMovementFlag::NAV_FLAG_LADDER:
			return AINAV_IsOffLadderNode(NavProfile, AIPlayer, PathNode);
		case EAINavMovementFlag::NAV_FLAG_FALL:
			return AINAV_IsOffFallNode(NavProfile, AIPlayer, PathNode);
		case EAINavMovementFlag::NAV_FLAG_JUMP:
			return AINAV_IsOffJumpNode(NavProfile, AIPlayer, PathNode);
		case EAINavMovementFlag::NAV_FLAG_PLATFORM:
			return AINAV_IsOffPlatformNode(NavProfile, AIPlayer, PathNode);
		case EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM1:
		case EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM2:
			return AINAV_IsOffPhaseGateNode(NavProfile, AIPlayer, PathNode);
		default:
			return AINAV_IsOffWalkNode(NavProfile, AIPlayer, PathNode);
	}
}

bool AINAV_IsOffWalkNode(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* PathNode)
{
	if (!NavProfile || !UTIL_IsEdictActive(AIPlayer) || !PathNode || !PathNode->IsValidMove()) { return true; }

	if (!IsPlayerOnGround(AIPlayer)) { return false; }

	Vector NearestPointOnLine = vClosestPointOnLine(PathNode->FromLocation, PathNode->ToLocation, AIPlayer->v.origin);

	if (vPointOverlaps3D(NearestPointOnLine, AIPlayer->v.absmin, AIPlayer->v.absmax)) { return false; }

	if (vDist2DSq(AIPlayer->v.origin, NearestPointOnLine) > sqrf(GetPlayerRadius(AIPlayer) * 3.0f)) { return true; }

	const Vector FloorLocation = UTIL_GetFloorUnderEntity(AIPlayer);

	if (vEquals2D(NearestPointOnLine, PathNode->FromLocation) && !AINAV_IsPointDirectlyReachable(NavProfile, FloorLocation, PathNode->FromLocation)) { return true; }
	if (vEquals2D(NearestPointOnLine, PathNode->ToLocation) && !AINAV_IsPointDirectlyReachable(NavProfile, FloorLocation, PathNode->ToLocation)) { return true; }

	return false;
}

bool AINAV_IsOffLadderNode(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* PathNode)
{
	if (!NavProfile || !UTIL_IsEdictActive(AIPlayer) || !PathNode || !PathNode->IsValidMove()) { return true; }

	if (IsPlayerOnLadder(AIPlayer)) { return false; }

	if (IsPlayerOnGround(AIPlayer))
	{
		const Vector BotFloorPosition = GetPlayerBottomOfCollisionHull(AIPlayer);

		if (!AINAV_IsPointDirectlyReachable(NavProfile, BotFloorPosition, PathNode->FromLocation)
			&& !AINAV_IsPointDirectlyReachable(NavProfile, BotFloorPosition, PathNode->ToLocation))
		{
			return true;
		}
	}

	return false;
}

bool AINAV_IsOffFallNode(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* PathNode)
{
	if (!NavProfile || !UTIL_IsEdictActive(AIPlayer) || !PathNode || !PathNode->IsValidMove()) { return true; }

	if (!IsPlayerOnGround(AIPlayer)) { return false; }

	Vector NearestPointOnLine = vClosestPointOnLine2D(PathNode->FromLocation, PathNode->ToLocation, AIPlayer->v.origin);

	const Vector FloorPosition = UTIL_GetFloorUnderEntity(AIPlayer);

	if (!AINAV_IsPointDirectlyReachable(NavProfile, FloorPosition, PathNode->FromLocation)
		&& !AINAV_IsPointDirectlyReachable(NavProfile, FloorPosition, PathNode->ToLocation))
	{
		return true;
	}

	return false;
}

bool AINAV_IsOffJumpNode(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* PathNode)
{
	if (!NavProfile || !UTIL_IsEdictActive(AIPlayer) || !PathNode || !PathNode->IsValidMove()) { return true; }

	if (!IsPlayerOnGround(AIPlayer)) { return false; }

	Vector ClosestPointOnLine = vClosestPointOnLine2D(PathNode->FromLocation, PathNode->ToLocation, AIPlayer->v.origin);

	const Vector BottomOfHull = GetPlayerBottomOfCollisionHull(AIPlayer);

	if (vEquals2D(ClosestPointOnLine, PathNode->FromLocation) || vEquals2D(ClosestPointOnLine, PathNode->ToLocation))
	{
		return (!AINAV_IsPointDirectlyReachable(NavProfile, BottomOfHull, PathNode->FromLocation, 5.0f)
			&& !AINAV_IsPointDirectlyReachable(NavProfile, BottomOfHull, PathNode->ToLocation, 5.0f));
	}

	if (vDist2DSq(AIPlayer->v.origin, ClosestPointOnLine) > sqrf(GetPlayerRadius(AIPlayer) * 2.0f)) { return true; }

	if ((PathNode->ToLocation.z - AIPlayer->v.origin.z) < max_ai_jump_height) { return false; }

	const Vector MoveDir3D = (PathNode->ToLocation - BottomOfHull);

	const Vector MoveDir = Vector(MoveDir3D.x, MoveDir3D.y, 0.0f).Normalize();
	const Vector JustInFrontOfBot = BottomOfHull + (MoveDir * 16.0f);

	if (AINAV_IsPointDirectlyReachable(NavProfile, BottomOfHull, JustInFrontOfBot)) { return true; }

	// TODO: Add a check to see if they are up against a wall which cannot be jumped over
	const Vector NavMeshCheckPoint = Vector(PathNode->ToLocation.x, PathNode->ToLocation.y, AIPlayer->v.origin.z + max_ai_jump_height);

	const Vector ProjectedPoint = AIMESH_ProjectPointToNavmesh(NavProfile, NavMeshCheckPoint, Vector(16.0f, 16.0f, max_ai_jump_height));

	if (vIsZero(ProjectedPoint)) { return false; }

	return (ProjectedPoint.z - AIPlayer->v.origin.z) <= max_ai_jump_height;
}

bool AINAV_IsOffPlatformNode(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* PathNode)
{
	if (!NavProfile || !UTIL_IsEdictActive(AIPlayer) || !PathNode || !PathNode->IsValidMove()) { return true; }

	// TODO: Fill this in
	return false;
}

bool AINAV_IsOffPhaseGateNode(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* PathNode)
{
	if (!NavProfile || !UTIL_IsEdictActive(AIPlayer) || !PathNode || !PathNode->IsValidMove()) { return true; }

	if (FNullEnt(PathNode->MovementObject)) { return true; }

	const AvHAIBuildableStructure* StartingPhaseGate = AITAC_GetStructureFromEdict(PathNode->MovementObject);

	if (!StartingPhaseGate || !StartingPhaseGate->IsValid() || StartingPhaseGate->StructureType != EAIStructureType::STRUCTURE_MARINE_PHASEGATE || !StartingPhaseGate->IsCompleted()) { return true; }

	StructureSearchFilter PGFilter;
	PGFilter.DeployableTeam = (AvHTeamNumber)AIPlayer->v.team;
	PGFilter.IncludeStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_COMPLETED;
	PGFilter.ExcludeStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_RECYCLING;
	PGFilter.MaxSearchRadius = UTIL_MetresToGoldSrcUnits(2.0f);

	return (AITAC_FindSingleMatchingStructure(PathNode->ToLocation, &PGFilter, EAIStructureSortType::FIND_STRUCTURE_NEAREST) != nullptr);
}

bool AINAV_CheckAndAddRequiredMovementTasks(const NavAgentProfile* NavProfile, const AvHAIPath* Path, AvHAIMoveTask& NewMoveTask)
{
	if (!NavProfile || !NavProfile->IsValid() || !Path->IsValidPath()) { return false; }

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();

	// If we are currently navigating a platform, don't run the checks in case we screw up our current move
	if (CurrentPathNode->MovementFlag == EAINavMovementFlag::NAV_FLAG_PLATFORM) { return false; }

	AvHAIPathNodeList FuturePathNodes = Path->GetFuturePathNodeList();

	for (const AvHAIPathNode* FutureNode : FuturePathNodes)
	{
		if (!FutureNode || !FutureNode->IsValidMove()) { return false; }

		if (FutureNode->MovementFlag == EAINavMovementFlag::NAV_FLAG_PLATFORM)
		{
			const DynamicMapObject* PlatformObject = AIMAP_GetDynamicObjectByEdict(FutureNode->MovementObject);

			if (!PlatformObject || !PlatformObject->IsValid()) { return false; }

			return AINAV_CheckPlatformForMovementTasks(NavProfile, FutureNode, PlatformObject->Edict, NewMoveTask);
		}

		const DynamicMapObject* BlockingObject = AIMAP_FindObjectBlockingPathPoint(FutureNode, nullptr);

		if (!BlockingObject || !BlockingObject->IsValid()) { continue; }

		return AINAV_CheckMapObjectForMovementTasks(NavProfile, FutureNode, BlockingObject->Edict, NewMoveTask);
	}

	return false;
}

bool AINAV_CheckMapObjectForMovementTasks(const NavAgentProfile* NavProfile, const AvHAIPathNode* ImpactedPathNode, const edict_t* ImpactingObject, AvHAIMoveTask& NewMoveTask)
{
	if (!NavProfile || !NavProfile->IsValid()) { return false; }
	if (!ImpactedPathNode || !ImpactedPathNode->IsValidMove()) { return false; }
	if (!UTIL_IsEdictActive(ImpactingObject)) { return false; }

	const DynamicMapObject* ImpactingObjectRef = AIMAP_GetDynamicObjectByEdict(ImpactingObject);

	if (!ImpactingObjectRef || !ImpactingObjectRef->IsValid()) { return false; }

	switch (ImpactingObjectRef->Type)
	{
		case EAIDynamicMapObjectType::MAPOBJECT_PLATFORM:
		case EAIDynamicMapObjectType::MAPOBJECT_TRAIN:
			return AINAV_CheckPlatformForMovementTasks(NavProfile, ImpactedPathNode, ImpactingObject, NewMoveTask);
		case EAIDynamicMapObjectType::TRIGGER_BREAK:
		case EAIDynamicMapObjectType::TRIGGER_SHOOT:
			return AINAV_AddBreakMovementTask(NavProfile, ImpactedPathNode->FromLocation, ImpactingObject, ImpactingObject, NewMoveTask);
		case EAIDynamicMapObjectType::TRIGGER_WELD:
			return AINAV_AddWeldMovementTask(NavProfile, ImpactedPathNode->FromLocation, ImpactingObject, ImpactingObject, NewMoveTask);
		default:
			break;
	}

	if (ImpactingObjectRef->State != EAIDynamicMapObjectState::OBJECTSTATE_IDLE) { return false; }

	const DynamicMapObject* Trigger = AIMAP_GetBestTriggerForObject(NavProfile, ImpactingObjectRef, ImpactedPathNode->FromLocation);

	if (!Trigger || !Trigger->IsValid()) { return false; }

	return AINAV_AddTriggerMovementTask(NavProfile, ImpactedPathNode->FromLocation, Trigger->Edict, ImpactingObject, NewMoveTask);
}

bool AINAV_CheckPlatformForMovementTasks(const NavAgentProfile* NavProfile, const AvHAIPathNode* ImpactedPathNode, const edict_t* Platform, AvHAIMoveTask& NewMoveTask)
{
	if (!NavProfile || !ImpactedPathNode || !UTIL_IsEdictActive(Platform)) { return false; }

	const DynamicMapObject* PlatformRef = AIMAP_GetDynamicObjectByEdict(Platform);

	if (!PlatformRef || !PlatformRef->IsValid()) { return false; }

	if (!AIMAP_PlatformNeedsActivating(NavProfile, PlatformRef, ImpactedPathNode->FromLocation, ImpactedPathNode->ToLocation)) { return false; }

	const DynamicMapObjectStop* DesiredEmbarkStop = nullptr;
	const DynamicMapObjectStop* DesiredDisembarkStop = nullptr;

	AIMAP_GetDesiredPlatformStops(PlatformRef, ImpactedPathNode->FromLocation, ImpactedPathNode->ToLocation, DesiredEmbarkStop, DesiredDisembarkStop);

	const DynamicMapObject* Trigger = nullptr;

	if (vEquals(UTIL_GetCentreOfEntity(Platform), DesiredEmbarkStop->StopLocation, 5.0f))
	{
		Trigger = AIMAP_GetTriggerReachableFromPlatform(PlatformRef, ImpactedPathNode->FromLocation.z + 32.0f);
	}

	if (!Trigger)
	{
		Trigger = AIMAP_GetBestTriggerForObject(NavProfile, PlatformRef, ImpactedPathNode->FromLocation);

		if (Trigger)
		{
			if (PlatformRef->State == EAIDynamicMapObjectState::OBJECTSTATE_IDLE)
			{
				return AINAV_AddUseMovementTask(NavProfile, ImpactedPathNode->FromLocation, Trigger->Edict, Trigger->Edict, NewMoveTask);
			}
			else
			{
				return AINAV_AddMoveMovementTask(NavProfile, ImpactedPathNode->FromLocation, AIMAP_GetButtonFloorLocation(NavProfile, ImpactedPathNode->FromLocation, Trigger->Edict), nullptr, NewMoveTask);
			}
		}
	}

	return false;
}

bool AINAV_AddTriggerMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* Trigger, const edict_t* TriggerTarget, AvHAIMoveTask& NewTask)
{
	NewTask.Clear();

	if (!NavProfile || !NavProfile->IsValid()) { return false; }
	if (!UTIL_IsEdictActive(Trigger)) { return false; }
	if (!UTIL_IsEdictActive(TriggerTarget)) { return false; }

	const DynamicMapObject* TriggerRef = AIMAP_GetDynamicObjectByEdict(Trigger);

	if (!TriggerRef || !TriggerRef->IsValid()) { return false; }

	switch (TriggerRef->Type)
	{
		case EAIDynamicMapObjectType::TRIGGER_SHOOT:
		case EAIDynamicMapObjectType::TRIGGER_BREAK:
			return AINAV_AddBreakMovementTask(NavProfile, StartPoint, Trigger, TriggerTarget, NewTask);
			break;
		case EAIDynamicMapObjectType::TRIGGER_TOUCH:
			return AINAV_AddTouchMovementTask(NavProfile, StartPoint, Trigger, TriggerTarget, NewTask);
			break;
		case EAIDynamicMapObjectType::TRIGGER_USE:
			return AINAV_AddUseMovementTask(NavProfile, StartPoint, Trigger, TriggerTarget, NewTask);
			break;
		default:
			return AINAV_AddUseMovementTask(NavProfile, StartPoint, Trigger, TriggerTarget, NewTask);
			break;
	}
}

bool AINAV_AddPickupMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* ThingToPickup, const edict_t* TriggerToActivate, AvHAIMoveTask& NewTask)
{
	NewTask.Clear();

	NewTask.TaskType = EAIMovementTaskType::MOVE_TASK_PICKUP;
	NewTask.TaskTarget = ThingToPickup;
	NewTask.TriggerToActivate = (UTIL_IsEdictActive(TriggerToActivate)) ? TriggerToActivate : nullptr;
	NewTask.TaskLocation = ThingToPickup->v.origin;

	return true;
}

bool AINAV_AddTouchMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* EntityToTouch, const edict_t* TriggerToActivate, AvHAIMoveTask& NewTask)
{
	NewTask.Clear();

	AvHAIPath TestPath;
	bool bFoundPath = AINAV_FindPathClosestToPoint(NavProfile, StartPoint, UTIL_GetCentreOfEntity(EntityToTouch), &TestPath, 200.0f);

	if (!bFoundPath) { return false; }

	NewTask.TaskType = EAIMovementTaskType::MOVE_TASK_TOUCH;
	NewTask.TaskTarget = EntityToTouch;
	NewTask.TriggerToActivate = (UTIL_IsEdictActive(TriggerToActivate)) ? TriggerToActivate : nullptr;
	NewTask.TaskLocation = TestPath.GetFinalDestination();

	return true;
}

bool AINAV_AddBreakMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* EntityToBreak, const edict_t* TriggerToActivate, AvHAIMoveTask& NewTask)
{
	NewTask.Clear();

	NewTask.TaskType = EAIMovementTaskType::MOVE_TASK_BREAK;
	NewTask.TaskTarget = EntityToBreak;
	NewTask.TriggerToActivate = (UTIL_IsEdictActive(TriggerToActivate)) ? TriggerToActivate : nullptr;
	NewTask.TaskLocation = AIMAP_GetButtonFloorLocation(NavProfile, StartPoint, EntityToBreak);

	return true;
}

bool AINAV_AddWeldMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* EntityToWeld, const edict_t* TriggerToActivate, AvHAIMoveTask& NewTask)
{
	NewTask.Clear();

	NewTask.TaskType = EAIMovementTaskType::MOVE_TASK_BREAK;
	NewTask.TaskTarget = EntityToWeld;
	NewTask.TriggerToActivate = (UTIL_IsEdictActive(TriggerToActivate)) ? TriggerToActivate : nullptr;
	NewTask.TaskLocation = AIMAP_GetButtonFloorLocation(NavProfile, StartPoint, EntityToWeld);

	return true;
}

bool AINAV_AddUseMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* EntityToUse, const edict_t* TriggerToActivate, AvHAIMoveTask& NewTask)
{
	NewTask.Clear();

	NewTask.TaskType = EAIMovementTaskType::MOVE_TASK_USE;
	NewTask.TaskTarget = EntityToUse;
	NewTask.TriggerToActivate = (UTIL_IsEdictActive(TriggerToActivate)) ? TriggerToActivate : nullptr;
	NewTask.TaskLocation = AIMAP_GetButtonFloorLocation(NavProfile, StartPoint, EntityToUse);

	return true;
}

bool AINAV_AddMoveMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const Vector& MoveLocation, const edict_t* TriggerToActivate, AvHAIMoveTask& NewTask)
{
	NewTask.Clear();

	AvHAIPath TestPath;
	const bool bFoundPath = AINAV_FindPathClosestToPoint(NavProfile, StartPoint, MoveLocation, &TestPath, 200.0f);

	if (!bFoundPath) { return false; }

	NewTask.TaskType = EAIMovementTaskType::MOVE_TASK_MOVE;
	NewTask.TaskLocation = MoveLocation;
	NewTask.TaskLocation = TestPath.GetFinalDestination();

	return true;
}

bool AINAV_IsPathPointComplete(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!UTIL_IsEdictActive(AIPlayer) || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return false; }

	EAINavMovementFlag CurrentNavFlag = CurrentPathNode->MovementFlag;
	Vector MoveFrom = CurrentPathNode->FromLocation;
	Vector MoveTo = CurrentPathNode->ToLocation;

	if (UTIL_IsPointInSwimArea(MoveTo))
	{
		Vector ClosestPointToPath = vClosestPointOnLine(MoveFrom, MoveTo, AIPlayer->v.origin);
		bool bAtOrPastDestination = vEquals(ClosestPointToPath, MoveTo, 32.0f);

		return vPointOverlaps3D(MoveTo, AIPlayer->v.absmin, AIPlayer->v.absmax) || bAtOrPastDestination;
	}

	switch (CurrentNavFlag)
	{
	case EAINavMovementFlag::NAV_FLAG_WALK:
		return AINAV_IsWalkMoveComplete(NavProfile, AIPlayer, CurrentPathNode, NextPathNode);
	case EAINavMovementFlag::NAV_FLAG_LADDER:
		return AINAV_IsLadderMoveComplete(NavProfile, AIPlayer, CurrentPathNode, NextPathNode);
	case EAINavMovementFlag::NAV_FLAG_FALL:
		return AINAV_IsFallMoveComplete(NavProfile, AIPlayer, CurrentPathNode, NextPathNode);
	case EAINavMovementFlag::NAV_FLAG_JUMP:
		return AINAV_IsJumpMoveComplete(NavProfile, AIPlayer, CurrentPathNode, NextPathNode);
	case EAINavMovementFlag::NAV_FLAG_PLATFORM:
		return AINAV_IsLiftMoveComplete(NavProfile, AIPlayer, CurrentPathNode, NextPathNode);
	case EAINavMovementFlag::NAV_FLAG_WALLCLIMB:
		return AINAV_IsWallClimbMoveComplete(NavProfile, AIPlayer, CurrentPathNode, NextPathNode);
	case EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM1:
	case EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM2:
		return AINAV_IsPhaseGateMoveComplete(NavProfile, AIPlayer, CurrentPathNode, NextPathNode);
	default:
		return AINAV_IsWalkMoveComplete(NavProfile, AIPlayer, CurrentPathNode, NextPathNode);
	}

	return AINAV_IsWalkMoveComplete(NavProfile, AIPlayer, CurrentPathNode, NextPathNode);
}

bool AINAV_IsWalkMoveComplete(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!NavProfile || !UTIL_IsEdictActive(AIPlayer) || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return true; }

	if (NextPathNode && !NextPathNode->IsPrecisionMove())
	{
		const Vector CurrentPosition = UTIL_GetFloorUnderEntity(AIPlayer);
		if (AINAV_IsPointDirectlyReachable(NavProfile, CurrentPosition, NextPathNode->ToLocation, 5.0f))
		{
			if (UTIL_QuickHullTrace(nullptr, AIPlayer->v.origin, NextPathNode->ToLocation, head_hull))
			{
				return true;
			}
		}
	}

	return vPointOverlaps3D(CurrentPathNode->ToLocation, AIPlayer->v.absmin, AIPlayer->v.absmax)
		|| (vDist2DSq(AIPlayer->v.origin, CurrentPathNode->ToLocation) < sqrf(GetPlayerRadius(AIPlayer) * 2.0f));
}

bool AINAV_IsLadderMoveComplete(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!NavProfile || !UTIL_IsEdictActive(AIPlayer) || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return true; }

	if (IsPlayerOnLadder(AIPlayer)) { return false; }

	return vPointOverlaps3D(CurrentPathNode->ToLocation, AIPlayer->v.absmin, AIPlayer->v.absmax);
}

bool AINAV_IsFallMoveComplete(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!NavProfile || !UTIL_IsEdictActive(AIPlayer) || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return true; }

	if (NextPathNode && !NextPathNode->IsPrecisionMove())
	{
		Vector ThisMoveDir = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - CurrentPathNode->FromLocation);
		Vector NextMoveDir = UTIL_GetVectorNormal2D(NextPathNode->ToLocation - NextPathNode->FromLocation);

		float MoveDot = UTIL_GetDotProduct2D(ThisMoveDir, NextMoveDir);

		if (MoveDot > 0.0f)
		{
			const Vector FloorPosition = UTIL_GetFloorUnderEntity(AIPlayer);

			if (AINAV_IsPointDirectlyReachable(NavProfile, FloorPosition, NextPathNode->ToLocation, 5.0f)
				&& UTIL_QuickTrace(AIPlayer, AIPlayer->v.origin, NextPathNode->ToLocation)
				&& fabsf(GetPlayerBottomOfCollisionHull(AIPlayer).z - CurrentPathNode->ToLocation.z) < 100.0f)
			{
				return true;
			}
		}
	}

	return vPointOverlaps3D(CurrentPathNode->ToLocation, AIPlayer->v.absmin, AIPlayer->v.absmax);
}

bool AINAV_IsJumpMoveComplete(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!NavProfile || !UTIL_IsEdictActive(AIPlayer) || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return true; }

	const Vector PositionInMove = vClosestPointOnLine2D(CurrentPathNode->FromLocation, CurrentPathNode->ToLocation, AIPlayer->v.origin);

	if (!vEquals2D(PositionInMove, CurrentPathNode->ToLocation, 2.0f)) { return false; }

	if (NextPathNode && !NextPathNode->IsPrecisionMove())
	{
		Vector ThisMoveDir = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - CurrentPathNode->FromLocation);
		Vector NextMoveDir = UTIL_GetVectorNormal2D(NextPathNode->ToLocation - NextPathNode->FromLocation);

		float MoveDot = UTIL_GetDotProduct2D(ThisMoveDir, NextMoveDir);

		if (MoveDot >= 0.0f)
		{
			const Vector FloorPosition = UTIL_GetFloorUnderEntity(AIPlayer);
			Vector HullTraceEnd = CurrentPathNode->ToLocation;
			HullTraceEnd.z = AIPlayer->v.origin.z;

			if (AINAV_IsPointDirectlyReachable(NavProfile, FloorPosition, NextPathNode->ToLocation, 5.0f)
				&& UTIL_QuickHullTrace(AIPlayer, AIPlayer->v.origin, HullTraceEnd, head_hull, false)
				&& fabsf(GetPlayerBottomOfCollisionHull(AIPlayer).z - NextPathNode->ToLocation.z) < 100.0f)
			{
				return true;
			}
		}
	}

	return vPointOverlaps3D(CurrentPathNode->ToLocation, AIPlayer->v.absmin, AIPlayer->v.absmax);
}

bool AINAV_IsLiftMoveComplete(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!NavProfile || !UTIL_IsEdictActive(AIPlayer) || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return true; }

	return vPointOverlaps3D(CurrentPathNode->ToLocation, AIPlayer->v.absmin, AIPlayer->v.absmax);
}

bool AINAV_IsWallClimbMoveComplete(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!NavProfile || !UTIL_IsEdictActive(AIPlayer) || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return true; }

	if (NextPathNode && !NextPathNode->IsPrecisionMove())
	{
		if (!IsPlayerClimbingWall(AIPlayer))
		{
			const Vector FloorLocation = UTIL_GetFloorUnderEntity(AIPlayer);

			if (AINAV_IsPointDirectlyReachable(NavProfile, FloorLocation, NextPathNode->ToLocation)) { return true; }
		}
	}

	Vector PositionInMove = vClosestPointOnLine2D(CurrentPathNode->FromLocation, CurrentPathNode->ToLocation, AIPlayer->v.origin);

	return vEquals2D(PositionInMove, CurrentPathNode->ToLocation, 4.0f) && IsPlayerOnGround(AIPlayer);
}

bool AINAV_IsObstacleMoveComplete(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!AIPlayer || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return true; }

	return vPointOverlaps3D(CurrentPathNode->ToLocation, AIPlayer->v.absmin, AIPlayer->v.absmax);
}

bool AINAV_IsPhaseGateMoveComplete(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!AIPlayer || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return true; }

	return vPointOverlaps3D(CurrentPathNode->ToLocation, AIPlayer->v.absmin, AIPlayer->v.absmax) || vDist2DSq(AIPlayer->v.origin, CurrentPathNode->ToLocation) < sqrf(32.0f);
}

AvHPlayer* AINAV_GetPlayerRidingOnBot(const edict_t* AIPlayer)
{
	if (!UTIL_IsEdictActive(AIPlayer)) { return nullptr; }

	PlayerListType PotentialRiders = AIMGR_GetAllActivePlayers();

	for (auto it = PotentialRiders.begin(); it != PotentialRiders.end(); it++)
	{
		AvHPlayer* PotentiallyRidingPlayer = (*it);

		if (!PotentiallyRidingPlayer) { continue; }

		edict_t* PotentialRidingEdict = PotentiallyRidingPlayer->edict();

		if (FNullEnt(PotentialRidingEdict)) { continue; }

		if (PotentialRidingEdict->v.groundentity == AIPlayer)
		{
			return PotentiallyRidingPlayer;
		}
	}

	return nullptr;
}

Vector AINAV_GetLadderMountPoint(const edict_t* MountLadder, const Vector StartPoint)
{
	if (FNullEnt(MountLadder)) { return ZERO_VECTOR; }

	Vector LadderCentre = UTIL_GetCentreOfEntity(MountLadder);

	Vector LadderTop = Vector(LadderCentre.x, LadderCentre.y, MountLadder->v.absmax.z);
	Vector LadderBottom = Vector(LadderCentre.x, LadderCentre.y, MountLadder->v.absmin.z);

	Vector MountPoint = LadderCentre;
	MountPoint.z = clampf(MountPoint.z, LadderBottom.z + 10.0f, LadderTop.z - 10.0f);

	bool bUseXAxis = (MountLadder->v.size.x < MountLadder->v.size.y);

	if (bUseXAxis)
	{
		Vector FirstSamplePoint = LadderCentre;
		FirstSamplePoint.z = clampf(StartPoint.z, LadderBottom.z + 5.0f, LadderTop.z - 5.0f);
		FirstSamplePoint.x = (StartPoint.x > LadderCentre.x) ? MountLadder->v.absmax.x + 17.0f : MountLadder->v.absmin.x - 17.0f;

		Vector SecondSamplePoint = LadderCentre;
		SecondSamplePoint.x = (StartPoint.x > LadderCentre.x) ? MountLadder->v.absmin.x - 17.0f : MountLadder->v.absmax.x + 17.0f;

		float Modifier = (StartPoint.x > LadderCentre.x) ? 1.0f : -1.0f;

		if (UTIL_PointContents(FirstSamplePoint) != CONTENTS_SOLID)
		{
			MountPoint = FirstSamplePoint;
		}
		else if (UTIL_PointContents(SecondSamplePoint) != CONTENTS_SOLID)
		{
			MountPoint = SecondSamplePoint;
		}
	}
	else
	{
		Vector FirstSamplePoint = LadderCentre;
		FirstSamplePoint.z = clampf(StartPoint.z, LadderBottom.z + 5.0f, LadderTop.z - 5.0f);
		FirstSamplePoint.y = (StartPoint.y > LadderCentre.y) ? MountLadder->v.absmax.y + 17.0f : MountLadder->v.absmin.y - 17.0f;

		Vector SecondSamplePoint = LadderCentre;
		SecondSamplePoint.y = (StartPoint.y > LadderCentre.y) ? MountLadder->v.absmin.y - 17.0f : MountLadder->v.absmax.y + 17.0f;

		if (UTIL_PointContents(FirstSamplePoint) != CONTENTS_SOLID)
		{
			MountPoint = FirstSamplePoint;
		}
		else if (UTIL_PointContents(SecondSamplePoint) != CONTENTS_SOLID)
		{
			MountPoint = SecondSamplePoint;
		}
	}

	return MountPoint;
}

Vector AINAV_GetFurthestVisiblePointOnPath(const Vector& ViewerLocation, const AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return ZERO_VECTOR; }

	const int32 MaxScanAhead = imini(static_cast<int32>(Path->CurrentNodeIndex) + 10, Path->GetPathSize() - 1);

	for (int32 i = MaxScanAhead; i >= Path->CurrentNodeIndex; i--)
	{
		const AvHAIPathNode* CheckNode = Path->GetNodeAtIndex(i);

		if (!CheckNode || !CheckNode->IsValidMove()) { continue; }

		if (UTIL_QuickTrace(NULL, ViewerLocation, CheckNode->ToLocation))
		{
			return CheckNode->ToLocation;
		}

		const Vector Dir = UTIL_GetVectorNormal(CheckNode->FromLocation - CheckNode->ToLocation);

		const float Dist = vDist3D(CheckNode->FromLocation, CheckNode->ToLocation);
		const int Steps = (int)floorf(Dist / 50.0f);

		Vector ThisView = CheckNode->ToLocation + (Dir * 50.0f);

		for (int i = 0; i < Steps; i++)
		{
			if (UTIL_QuickTrace(NULL, ViewerLocation, ThisView))
			{
				return ThisView;
			}

			ThisView = ThisView + (Dir * 50.0f);
		}
	}

	return ZERO_VECTOR;
}
