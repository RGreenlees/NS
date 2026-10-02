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
#include "fastlz/fastlz.c"
#include "DetourAlloc.h"

#include <cfloat>

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
	status = FoundMesh->NavQuery->findNearestPoly(dtStartPos, searchExtents, m_navFilter, &StartPoly, dtStartNearest);
	if (!dtStatusSucceed(status))
	{
		return false; // couldn't find a polygon
	}

	// find the end polygon
	status = FoundMesh->NavQuery->findNearestPoly(dtEndPos, searchExtents, m_navFilter, &EndPoly, dtEndNearest);
	if (!dtStatusSucceed(status))
	{
		return false; // couldn't find a polygon
	}

	status = FoundMesh->NavQuery->findPath(StartPoly, EndPoly, dtStartNearest, dtEndNearest, m_navFilter, PolyPath, &nPathCount, MAX_PATH_POLY);

	if (nPathCount == 0)
	{
		return false; // couldn't find a path
	}

	if (PolyPath[nPathCount - 1] != EndPoly)
	{
		float dtEndPoint[3];
		dtVcopy(dtEndPoint, dtEndNearest);

		FoundMesh->NavQuery->closestPointOnPoly(PolyPath[nPathCount - 1], dtEndNearest, dtEndPoint, 0);

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
	status = FoundMesh->NavQuery->findNearestPoly(dtStartPos, dtSearchExtents, m_navFilter, &StartPoly, dtStartNearest);
	if (!dtStatusSucceed(status))
	{
		return FromLocation; // couldn't find a polygon
	}

	// find the end polygon
	status = FoundMesh->NavQuery->findNearestPoly(dtEndPos, dtSearchExtents, m_navFilter, &EndPoly, dtEndNearest);
	if (!dtStatusSucceed(status))
	{
		return FromLocation; // couldn't find a polygon
	}

	status = FoundMesh->NavQuery->findPath(StartPoly, EndPoly, dtStartNearest, dtEndNearest, m_navFilter, PolyPath, &nPathCount, MAX_PATH_POLY);

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
	status = FoundMesh->NavQuery->findNearestPoly(dtStartPos, dtDefaultProjectionExtents, m_navFilter, &dtStartPoly, dtStartNearest);
	if ((status & DT_FAILURE) || (status & DT_STATUS_DETAIL_MASK))
	{
		return false; // couldn't find a polygon
	}

	// find the end polygon
	status = FoundMesh->NavQuery->findNearestPoly(dtEndPos, dtDefaultProjectionExtents, m_navFilter, &dtEndPoly, dtEndNearest);
	if ((status & DT_FAILURE) || (status & DT_STATUS_DETAIL_MASK))
	{
		return false; // couldn't find a polygon
	}

	status = FoundMesh->NavQuery->findPath(dtStartPoly, dtEndPoly, dtStartNearest, dtEndNearest, m_navFilter, dtPolyPath, &nPathCount, MAX_PATH_POLY);

	if (dtPolyPath[nPathCount - 1] != dtEndPoly)
	{
		float epos[3];
		dtVcopy(epos, dtEndNearest);

		FoundMesh->NavQuery->closestPointOnPoly(dtPolyPath[nPathCount - 1], dtEndNearest, epos, 0);

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

	status = FoundMesh->NavQuery->findStraightPath(dtStartNearest, dtEndNearest, dtPolyPath, nPathCount, dtStraightPath, dtStraightPathFlags, dtStraightPolyPath, &nVertCount, MAX_AI_PATH_SIZE, DT_STRAIGHTPATH_AREA_CROSSINGS);

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

	FoundMesh->NavMesh->getPolyFlags(dtStraightPolyPath[0], &dtCurrFlags);
	FoundMesh->NavMesh->getPolyArea(dtStraightPolyPath[0], &dtCurrArea);

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
				NextPathNode.Platform = PlatformRef->Edict;
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

		FoundMesh->NavMesh->getPolyFlags(dtStraightPolyPath[nVert], &dtCurrFlags);
		FoundMesh->NavMesh->getPolyArea(dtStraightPolyPath[nVert], &dtCurrArea);

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

Vector AINAV_GetNearestPlatformDisembarkPoint(const NavAgentProfile* NavProfile, edict_t* Rider, DynamicMapObject* LiftReference)
{
	if (!NavProfile || !LiftReference || FNullEnt(Rider)) { return ZERO_VECTOR; }

	const NavOffMeshConnection* NearestConnection = nullptr;
	float MinDist = 0.0f;

	NavMesh* ChosenNavMesh = AIMESH_GetNavMeshAtIndex(NavProfile->MeshIndex);

	if (!ChosenNavMesh) { return ZERO_VECTOR; }

	for (auto it = ChosenNavMesh->MeshConnections.begin(); it != ChosenNavMesh->MeshConnections.end(); it++)
	{
		const NavOffMeshConnection* ThisConnection = &(*it);

		if (!ThisConnection || !ThisConnection->IsValid()) { continue; }

		if (!EnumHasAnyFlags(ThisConnection->ConnectionFlags, EAINavMovementFlag::NAV_FLAG_PLATFORM)) { continue; }

		if (ThisConnection->LinkedObject == LiftReference->Edict)
		{
			float ThisDist = fminf(vDist3DSq(ThisConnection->FromLocation, UTIL_GetClosestPointOnEntityToLocation(ThisConnection->FromLocation, LiftReference->Edict)),
				vDist3DSq(ThisConnection->ToLocation, UTIL_GetClosestPointOnEntityToLocation(ThisConnection->ToLocation, LiftReference->Edict)));

			if (ThisDist < sqrf(100.0f) && (!NearestConnection || ThisDist < MinDist))
			{
				NearestConnection = ThisConnection;
				MinDist = ThisDist;
			}
		}
	}

	if (NearestConnection)
	{
		Vector NearestPointFromLocation = UTIL_GetClosestPointOnEntityToLocation(NearestConnection->FromLocation, LiftReference->Edict);
		NearestPointFromLocation.z = Rider->v.origin.z;

		Vector NearestPointToLocation = UTIL_GetClosestPointOnEntityToLocation(NearestConnection->ToLocation, LiftReference->Edict);
		NearestPointToLocation.z = Rider->v.origin.z;

		float DistFromLocation = vDist3DSq(NearestConnection->FromLocation, NearestPointFromLocation);
		float DistToLocation = vDist3DSq(NearestConnection->ToLocation, NearestPointToLocation);
		return (DistFromLocation < DistToLocation) ? NearestConnection->FromLocation : NearestConnection->ToLocation;
	}

	Vector NearestProjectedPoint = ZERO_VECTOR;
	Vector LiftCentre = UTIL_GetCentreOfEntity(LiftReference->Edict);
	float DisembarkHeight = (!FNullEnt(Rider)) ? GetPlayerBottomOfCollisionHull(Rider).z : LiftReference->Edict->v.absmax.z;

	Vector FrontLocation = Vector(LiftReference->Edict->v.absmax.x, LiftCentre.y, DisembarkHeight);
	Vector RearLocation = Vector(LiftReference->Edict->v.absmin.x, LiftCentre.y, DisembarkHeight);
	Vector LeftLocation = Vector(LiftCentre.x, LiftReference->Edict->v.absmin.y, DisembarkHeight);
	Vector RightLocation = Vector(LiftCentre.x, LiftReference->Edict->v.absmax.y, DisembarkHeight);

	float ProjectWidth = fmaxf((LiftReference->Edict->v.absmax.x - LiftReference->Edict->v.absmin.x) * 0.5f, (LiftReference->Edict->v.absmax.y - LiftReference->Edict->v.absmin.y) * 0.5f);
	ProjectWidth += 100.0f;

	Vector ProjectedLoc = AIMESH_ProjectPointToNavmesh(NavProfile, FrontLocation, Vector(ProjectWidth, ProjectWidth, 50.0f));

	if (!vIsZero(ProjectedLoc) && !vPointOverlaps2D(ProjectedLoc, LiftReference->Edict->v.absmin, LiftReference->Edict->v.absmax))
	{
		return ProjectedLoc;
	}

	ProjectedLoc = AIMESH_ProjectPointToNavmesh(NavProfile, RearLocation, Vector(ProjectWidth, ProjectWidth, 50.0f));

	if (!vIsZero(ProjectedLoc) && !vPointOverlaps2D(ProjectedLoc, LiftReference->Edict->v.absmin, LiftReference->Edict->v.absmax))
	{
		return ProjectedLoc;
	}

	ProjectedLoc = AIMESH_ProjectPointToNavmesh(NavProfile, LeftLocation, Vector(ProjectWidth, ProjectWidth, 50.0f));

	if (!vIsZero(ProjectedLoc) && !vPointOverlaps2D(ProjectedLoc, LiftReference->Edict->v.absmin, LiftReference->Edict->v.absmax))
	{
		return ProjectedLoc;
	}

	ProjectedLoc = AIMESH_ProjectPointToNavmesh(NavProfile, RightLocation, Vector(ProjectWidth, ProjectWidth, 50.0f));

	if (!vIsZero(ProjectedLoc) && !vPointOverlaps2D(ProjectedLoc, LiftReference->Edict->v.absmin, LiftReference->Edict->v.absmax))
	{
		return ProjectedLoc;
	}

	return ZERO_VECTOR;
}

bool AINAV_HasBotCompletedPathPoint(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!AIPlayer || !AIPlayer->IsValid() || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return false; }

	EAINavMovementFlag CurrentNavFlag = CurrentPathNode->MovementFlag;
	Vector MoveFrom = CurrentPathNode->FromLocation;
	Vector MoveTo = CurrentPathNode->ToLocation;

	if (UTIL_IsPointInSwimArea(MoveTo))
	{
		Vector ClosestPointToPath = vClosestPointOnLine(MoveFrom, MoveTo, AIPlayer->Edict->v.origin);
		bool bAtOrPastDestination = vEquals(ClosestPointToPath, MoveTo, 32.0f);

		return vPointOverlaps3D(MoveTo, AIPlayer->Edict->v.absmin, AIPlayer->Edict->v.absmax) || bAtOrPastDestination;
	}

	switch (CurrentNavFlag)
	{
		case EAINavMovementFlag::NAV_FLAG_WALK:
			return AINAV_HasBotCompletedWalkMove(AIPlayer, CurrentPathNode, NextPathNode);
		case EAINavMovementFlag::NAV_FLAG_LADDER:
			return AINAV_HasBotCompletedLadderMove(AIPlayer, CurrentPathNode, NextPathNode);
		case EAINavMovementFlag::NAV_FLAG_FALL:
			return AINAV_HasBotCompletedFallMove(AIPlayer, CurrentPathNode, NextPathNode);
		case EAINavMovementFlag::NAV_FLAG_JUMP:
			return AINAV_HasBotCompletedJumpMove(AIPlayer, CurrentPathNode, NextPathNode);
		case EAINavMovementFlag::NAV_FLAG_PLATFORM:
			return AINAV_HasBotCompletedLiftMove(AIPlayer, CurrentPathNode, NextPathNode);
		case EAINavMovementFlag::NAV_FLAG_WALLCLIMB:
			return AINAV_HasBotCompletedWallClimbMove(AIPlayer, CurrentPathNode, NextPathNode);
		case EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM1:
		case EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM2:
			return AINAV_HasBotCompletedPhaseGateMove(AIPlayer, CurrentPathNode, NextPathNode);
		default:
			return AINAV_HasBotCompletedWalkMove(AIPlayer, CurrentPathNode, NextPathNode);
	}

	return AINAV_HasBotCompletedWalkMove(AIPlayer, CurrentPathNode, NextPathNode);
}

bool AINAV_HasBotCompletedWalkMove(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!AIPlayer || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return true; }

	if (NextPathNode && !NextPathNode->IsPrecisionMove())
	{
		const Vector CurrentPosition = UTIL_GetFloorUnderEntity(AIPlayer->Edict);
		if (AINAV_IsPointDirectlyReachable(AIPlayer->GetNavProfile(), CurrentPosition, NextPathNode->ToLocation, 5.0f))
		{
			if (UTIL_QuickHullTrace(nullptr, AIPlayer->Edict->v.origin, NextPathNode->ToLocation, head_hull))
			{
				return true;
			}
		}
	}

	return vPointOverlaps3D(CurrentPathNode->ToLocation, AIPlayer->Edict->v.absmin, AIPlayer->Edict->v.absmax)
		|| (vDist2DSq(AIPlayer->Edict->v.origin, CurrentPathNode->ToLocation) < sqrf(GetPlayerRadius(AIPlayer->Edict) * 2.0f));
}

bool AINAV_HasBotCompletedLadderMove(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!AIPlayer || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return true; }

	if (IsPlayerOnLadder(AIPlayer->Edict)) { return false; }

	return vPointOverlaps3D(CurrentPathNode->ToLocation, AIPlayer->Edict->v.absmin, AIPlayer->Edict->v.absmax);
}

bool AINAV_HasBotCompletedFallMove(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!AIPlayer || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return true; }

	if (NextPathNode && !NextPathNode->IsPrecisionMove())
	{
		Vector ThisMoveDir = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - CurrentPathNode->FromLocation);
		Vector NextMoveDir = UTIL_GetVectorNormal2D(NextPathNode->ToLocation - NextPathNode->FromLocation);

		float MoveDot = UTIL_GetDotProduct2D(ThisMoveDir, NextMoveDir);

		if (MoveDot > 0.0f)
		{
			const Vector FloorPosition = UTIL_GetFloorUnderEntity(AIPlayer->Edict);

			if (AINAV_IsPointDirectlyReachable(AIPlayer->GetNavProfile(), FloorPosition, NextPathNode->ToLocation, 5.0f)
				&& UTIL_QuickTrace(AIPlayer->Edict, AIPlayer->Edict->v.origin, NextPathNode->ToLocation)
				&& fabsf(AIPlayer->CollisionHullBottomLocation.z - CurrentPathNode->ToLocation.z) < 100.0f)
			{
				return true;
			}
		}
	}

	return vPointOverlaps3D(CurrentPathNode->ToLocation, AIPlayer->Edict->v.absmin, AIPlayer->Edict->v.absmax);
}

bool AINAV_HasBotCompletedJumpMove(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!AIPlayer || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return true; }

	const Vector PositionInMove = vClosestPointOnLine2D(CurrentPathNode->FromLocation, CurrentPathNode->ToLocation, AIPlayer->Edict->v.origin);

	if (!vEquals2D(PositionInMove, CurrentPathNode->ToLocation, 2.0f)) { return false; }

	if (NextPathNode && !NextPathNode->IsPrecisionMove())
	{
		Vector ThisMoveDir = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - CurrentPathNode->FromLocation);
		Vector NextMoveDir = UTIL_GetVectorNormal2D(NextPathNode->ToLocation - NextPathNode->FromLocation);

		float MoveDot = UTIL_GetDotProduct2D(ThisMoveDir, NextMoveDir);

		if (MoveDot >= 0.0f)
		{
			const Vector FloorPosition = UTIL_GetFloorUnderEntity(AIPlayer->Edict);
			Vector HullTraceEnd = CurrentPathNode->ToLocation;
			HullTraceEnd.z = AIPlayer->Edict->v.origin.z;

			if (AINAV_IsPointDirectlyReachable(AIPlayer->GetNavProfile(), FloorPosition, NextPathNode->ToLocation, 5.0f)
				&& UTIL_QuickHullTrace(AIPlayer->Edict, AIPlayer->Edict->v.origin, HullTraceEnd, head_hull, false)
				&& fabsf(AIPlayer->CollisionHullBottomLocation.z - NextPathNode->ToLocation.z) < 100.0f)
			{
				return true;
			}
		}
	}

	return vPointOverlaps3D(CurrentPathNode->ToLocation, AIPlayer->Edict->v.absmin, AIPlayer->Edict->v.absmax);
}

bool AINAV_HasBotCompletedLiftMove(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!AIPlayer || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return true; }

	return vPointOverlaps3D(CurrentPathNode->ToLocation, AIPlayer->Edict->v.absmin, AIPlayer->Edict->v.absmax);
}

bool AINAV_HasBotCompletedWallClimbMove(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!AIPlayer || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return true; }

	if (NextPathNode && !NextPathNode->IsPrecisionMove())
	{
		if (!IsPlayerClimbingWall(AIPlayer->Edict))
		{
			const Vector FloorLocation = UTIL_GetFloorUnderEntity(AIPlayer->Edict);

			if (AINAV_IsPointDirectlyReachable(AIPlayer->GetNavProfile(), FloorLocation, NextPathNode->ToLocation)) { return true; }
		}
	}

	Vector PositionInMove = vClosestPointOnLine2D(CurrentPathNode->FromLocation, CurrentPathNode->ToLocation, AIPlayer->Edict->v.origin);

	return vEquals2D(PositionInMove, CurrentPathNode->ToLocation, 4.0f) && AIPlayer->IsOnGround();
}

bool AINAV_HasBotCompletedObstacleMove(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!AIPlayer || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return true; }

	return vPointOverlaps3D(CurrentPathNode->ToLocation, AIPlayer->Edict->v.absmin, AIPlayer->Edict->v.absmax);
}

bool AINAV_HasBotCompletedPhaseGateMove(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!AIPlayer || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return true; }

	return vPointOverlaps3D(CurrentPathNode->ToLocation, AIPlayer->Edict->v.absmin, AIPlayer->Edict->v.absmax) || vDist2DSq(AIPlayer->Edict->v.origin, CurrentPathNode->ToLocation) < sqrf(32.0f);
}

bool AINAV_IsBotOffPathNode(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* PathNode)
{
	if (!AIPlayer || !AIPlayer->IsValid() || !PathNode || !PathNode->IsValidMove()) { return false; }

	switch (PathNode->MovementFlag)
	{
	case EAINavMovementFlag::NAV_FLAG_WALK:
		return AINAV_IsBotOffWalkNode(AIPlayer, PathNode);
	case EAINavMovementFlag::NAV_FLAG_LADDER:
		return AINAV_IsBotOffLadderNode(AIPlayer, PathNode);
	case EAINavMovementFlag::NAV_FLAG_FALL:
		return AINAV_IsBotOffFallNode(AIPlayer, PathNode);
	case EAINavMovementFlag::NAV_FLAG_JUMP:
		return AINAV_IsBotOffJumpNode(AIPlayer, PathNode);
	case EAINavMovementFlag::NAV_FLAG_PLATFORM:
		return AINAV_IsBotOffPlatformNode(AIPlayer, PathNode);
	default:
		return AINAV_IsBotOffWalkNode(AIPlayer, PathNode);
	}

	return AINAV_IsBotOffWalkNode(AIPlayer, PathNode);
}

bool AINAV_IsBotOffWalkNode(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* PathNode)
{
	if (!AIPlayer->IsOnGround()) { return false; }

	Vector NearestPointOnLine = vClosestPointOnLine(PathNode->FromLocation, PathNode->ToLocation, AIPlayer->Edict->v.origin);

	if (vPointOverlaps3D(NearestPointOnLine, AIPlayer->Edict->v.absmin, AIPlayer->Edict->v.absmax)) { return false; }

	if (vDist2DSq(AIPlayer->Edict->v.origin, NearestPointOnLine) > sqrf(GetPlayerRadius(AIPlayer->Edict) * 3.0f)) { return true; }

	const Vector FloorLocation = UTIL_GetFloorUnderEntity(AIPlayer->Edict);

	if (vEquals2D(NearestPointOnLine, PathNode->FromLocation) && !AINAV_IsPointDirectlyReachable(AIPlayer->GetNavProfile(), FloorLocation, PathNode->FromLocation)) { return true; }
	if (vEquals2D(NearestPointOnLine, PathNode->ToLocation) && !AINAV_IsPointDirectlyReachable(AIPlayer->GetNavProfile(), FloorLocation, PathNode->ToLocation)) { return true; }

	return false;
}

bool AINAV_IsBotOffLadderNode(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* PathNode)
{
	if (IsPlayerOnLadder(AIPlayer->Edict)) { return false; }

	if (AIPlayer->IsOnGround())
	{
		const Vector BotFloorPosition = GetPlayerBottomOfCollisionHull(AIPlayer->Edict);

		if (!AINAV_IsPointDirectlyReachable(AIPlayer->GetNavProfile(), BotFloorPosition, PathNode->FromLocation)
			&& !AINAV_IsPointDirectlyReachable(AIPlayer->GetNavProfile(), BotFloorPosition, PathNode->ToLocation))
		{
			return true;
		}
	}

	return false;
}

bool AINAV_IsBotOffFallNode(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* PathNode)
{
	if (!AIPlayer->IsOnGround()) { return false; }

	Vector NearestPointOnLine = vClosestPointOnLine2D(PathNode->FromLocation, PathNode->ToLocation, AIPlayer->Edict->v.origin);

	const Vector FloorPosition = UTIL_GetFloorUnderEntity(AIPlayer->Edict);

	if (!AINAV_IsPointDirectlyReachable(AIPlayer->GetNavProfile(), FloorPosition, PathNode->FromLocation)
		&& !AINAV_IsPointDirectlyReachable(AIPlayer->GetNavProfile(), FloorPosition, PathNode->ToLocation))
	{
		return true;
	}

	return false;
}

bool AINAV_IsBotOffJumpNode(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* PathNode)
{
	if (!AIPlayer->IsOnGround()) { return false; }

	Vector ClosestPointOnLine = vClosestPointOnLine2D(PathNode->FromLocation, PathNode->ToLocation, AIPlayer->Edict->v.origin);

	const Vector BottomOfHull = GetPlayerBottomOfCollisionHull(AIPlayer->Edict);

	if (vEquals2D(ClosestPointOnLine, PathNode->FromLocation) || vEquals2D(ClosestPointOnLine, PathNode->ToLocation))
	{
		return (!AINAV_IsPointDirectlyReachable(AIPlayer->GetNavProfile(), BottomOfHull, PathNode->FromLocation, 5.0f)
			&& !AINAV_IsPointDirectlyReachable(AIPlayer->GetNavProfile(), BottomOfHull, PathNode->ToLocation, 5.0f));
	}

	if (vDist2DSq(AIPlayer->Edict->v.origin, ClosestPointOnLine) > sqrf(GetPlayerRadius(AIPlayer->Edict) * 2.0f)) { return true; }

	if ((PathNode->ToLocation.z - AIPlayer->Edict->v.origin.z) < max_ai_jump_height) { return false; }

	const Vector MoveDir3D = (PathNode->ToLocation - BottomOfHull);

	const Vector MoveDir = Vector(MoveDir3D.x, MoveDir3D.y, 0.0f).Normalize();
	const Vector JustInFrontOfBot = BottomOfHull + (MoveDir * 16.0f);

	if (AINAV_IsPointDirectlyReachable(AIPlayer->GetNavProfile(), BottomOfHull, JustInFrontOfBot)) { return true; }

	// TODO: Add a check to see if they are up against a wall which cannot be jumped over
	const Vector NavMeshCheckPoint = Vector(PathNode->ToLocation.x, PathNode->ToLocation.y, AIPlayer->Edict->v.origin.z + max_ai_jump_height);

	const Vector ProjectedPoint = AIMESH_ProjectPointToNavmesh(AIPlayer->GetNavProfile(), NavMeshCheckPoint, Vector(16.0f, 16.0f, max_ai_jump_height));

	if (vIsZero(ProjectedPoint)) { return false; }

	return (ProjectedPoint.z - AIPlayer->Edict->v.origin.z) <= max_ai_jump_height;
}

bool AINAV_IsBotOffPlatformNode(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* PathNode)
{
	// TODO: Fill this in
	return false;
}

EAINavMoveResult AINAV_FollowPath(AvHAIPlayer* AIPlayer, AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return EAINavMoveResult::NAV_MOVE_NOPATH; }

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();
	const AvHAIPathNode* NextPathNode = Path->GetNextPathNode();

	if (!CurrentPathNode || !CurrentPathNode->IsValidMove()) { return EAINavMoveResult::NAV_MOVE_NOPATH; }

	if (AINAV_HasBotCompletedPathPoint(AIPlayer, CurrentPathNode, NextPathNode))
	{
		// We have reached the end of our path. Job done.
		if (!NextPathNode)
		{
			return EAINavMoveResult::NAV_MOVE_PATH_COMPLETE;
		}

		Path->OnPathNodeComplete();

		CurrentPathNode = Path->GetCurrentPathNode();
		NextPathNode = Path->GetNextPathNode();
	}

	if (AIPlayer->IsInWater())
	{
		TraceResult Hit;

		AvHAIMutablePathNodeList FutureNodeList = Path->GetMutableFuturePathNodeList();

		for (AvHAIPathNode* ThisNode : FutureNodeList)
		{
			if (!UTIL_IsPointInSwimArea(ThisNode->ToLocation)) { break; }

			UTIL_TraceHull(AIPlayer->GetLocation(), ThisNode->ToLocation, ignore_monsters, head_hull, nullptr, &Hit);

			if (!Hit.fAllSolid && !Hit.fStartSolid && Hit.flFraction >= 1.0f)
			{
				Path->JumpToPathNode(ThisNode);
				ThisNode->FromLocation = AIPlayer->GetLocation();
			}
		}

		CurrentPathNode = Path->GetCurrentPathNode();
		NextPathNode = Path->GetNextPathNode();
	}

	if (IsPlayerStandingOnPlayer(AIPlayer->Edict) && CurrentPathNode->MovementFlag != EAINavMovementFlag::NAV_FLAG_LADDER)
	{
		if (AIPlayer->GetVelocity().Length2D() > 10.0f)
		{
			AIPlayer->NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(-AIPlayer->Edict->v.groundentity->v.velocity);
			return EAINavMoveResult::NAV_MOVE_SUCCESS;
		}

		MoveToWithoutNav(AIPlayer, CurrentPathNode->ToLocation);

		return;
	}

	AvHPlayer* RidingPlayer = AINAV_GetPlayerRidingOnBot(AIPlayer);

	if (RidingPlayer)
	{
		// TODO: Something here
	}

	if (AINAV_IsBotOffPathNode(AIPlayer, CurrentPathNode))
	{
		const bool bSucceededRegen = AINAV_FindPathClosestToPoint(AIPlayer->GetNavProfile(), UTIL_GetFloorUnderEntity(AIPlayer->Edict), Path->GetFinalDestination(), Path, AIPlayer->GetPlayerRadius());

		if (!bSucceededRegen)
		{
			return EAINavMoveResult::NAV_MOVE_NOPATH;
		}
	}

	AvHAIMoveTask NewMoveTask;

	if (AINAV_CheckAndAddRequiredMovementTasks(AIPlayer->GetNavProfile(), Path, NewMoveTask))
	{
		AIPlayer->AddMovementTask(NewMoveTask);
		return;
	}

	if (AIPlayer->IsInWater())
	{
		AINAV_NextSwimMove(AIPlayer, AIPlayer->NextFrameMovementInput, CurrentPathNode, NextPathNode);
	}
	else
	{
		AINAV_NextMove(AIPlayer, AIPlayer->NextFrameMovementInput, CurrentPathNode, NextPathNode);
	}

	AINAV_HandlePlayerAvoidance(AIPlayer, CurrentPathNode, AIPlayer->NextFrameMovementInput);

	if (vIsZero(AIPlayer->NextFrameMovementInput.RequiredLookLocation) && vIsZero(AIPlayer->NextFrameMovementInput.DesiredLookLocation))
	{
		Vector FurthestView = AINAV_GetFurthestVisiblePointOnPath(AIPlayer->GetEyePosition(), Path);

		if (vIsZero(FurthestView) || vDist2DSq(FurthestView, AIPlayer->GetEyePosition()) < sqrf(200.0f))
		{
			FurthestView = CurrentPathNode->ToLocation;

			Vector LookNormal = UTIL_GetVectorNormal2D(FurthestView - AIPlayer->GetEyePosition());

			FurthestView = FurthestView + (LookNormal * 1000.0f);
		}
	}
}

void AINAV_HandlePlayerAvoidance(AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, AvHAIMovementInput& OutMovementInputs)
{

}

bool AINAV_CheckAndAddRequiredMovementTasks(const NavAgentProfile* NavProfile, AvHAIPath* Path, AvHAIMoveTask& NewMoveTask)
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
			const DynamicMapObject* PlatformObject = AIMAP_GetDynamicObjectByEdict(FutureNode->Platform);

			return AINAV_CheckPlatformForMovementTasks(NavProfile, FutureNode, PlatformObject, NewMoveTask);
		}

		const DynamicMapObject* BlockingObject = AIMAP_FindObjectBlockingPathPoint(FutureNode, nullptr);

		if (BlockingObject)
		{
			return AINAV_CheckMapObjectForMovementTasks(NavProfile, FutureNode, BlockingObject, NewMoveTask);
		}
	}

	return false;
}

bool AINAV_CheckMapObjectForMovementTasks(const NavAgentProfile* NavProfile, const AvHAIPathNode* ImpactedPathNode, const DynamicMapObject* ImpactingObject, AvHAIMoveTask& NewMoveTask)
{
	if (!NavProfile || !NavProfile->IsValid()) { return false; }
	if (!ImpactedPathNode || !ImpactedPathNode->IsValidMove()) { return false; }
	if (!ImpactingObject || !ImpactingObject->IsValid()) { return false; }

	switch (ImpactingObject->Type)
	{
		case EAIDynamicMapObjectType::MAPOBJECT_PLATFORM:
		case EAIDynamicMapObjectType::MAPOBJECT_TRAIN:
			return AINAV_CheckPlatformForMovementTasks(NavProfile, ImpactedPathNode, ImpactingObject, NewMoveTask);
		case EAIDynamicMapObjectType::TRIGGER_BREAK:
		case EAIDynamicMapObjectType::TRIGGER_SHOOT:
			return AINAV_AddBreakMovementTask(NavProfile, ImpactedPathNode->FromLocation, ImpactingObject->Edict, ImpactingObject, NewMoveTask);
		case EAIDynamicMapObjectType::TRIGGER_WELD:
			return AINAV_AddWeldMovementTask(NavProfile, ImpactedPathNode->FromLocation, ImpactingObject->Edict, ImpactingObject, NewMoveTask);
		default:
			break;
	}

	if (ImpactingObject->State != EAIDynamicMapObjectState::OBJECTSTATE_IDLE) { return false; }

	const DynamicMapObject* Trigger = AIMAP_GetBestTriggerForObject(NavProfile, ImpactingObject, ImpactedPathNode->FromLocation);

	if (!Trigger) { return false; }

	return AINAV_AddTriggerMovementTask(NavProfile, ImpactedPathNode->FromLocation, Trigger, ImpactingObject, NewMoveTask);
}

bool AINAV_CheckPlatformForMovementTasks(const NavAgentProfile* NavProfile, const AvHAIPathNode* ImpactedPathNode, const DynamicMapObject* Platform, AvHAIMoveTask& NewMoveTask)
{
	if (!NavProfile || !ImpactedPathNode || !Platform) { return false; }

	if (!AIMAP_PlatformNeedsActivating(NavProfile, Platform, ImpactedPathNode->FromLocation, ImpactedPathNode->ToLocation)) { return false; }

	const DynamicMapObjectStop* DesiredEmbarkStop = nullptr;
	const DynamicMapObjectStop* DesiredDisembarkStop = nullptr;

	AIMAP_GetDesiredPlatformStops(Platform, ImpactedPathNode->FromLocation, ImpactedPathNode->ToLocation, DesiredEmbarkStop, DesiredDisembarkStop);

	const DynamicMapObject* Trigger = nullptr;

	if (vEquals(UTIL_GetCentreOfEntity(Platform->Edict), DesiredEmbarkStop->StopLocation, 5.0f))
	{
		Trigger = AIMAP_GetTriggerReachableFromPlatform(Platform, ImpactedPathNode->FromLocation.z + 32.0f);
	}

	if (!Trigger)
	{
		Trigger = AIMAP_GetBestTriggerForObject(NavProfile, Platform, ImpactedPathNode->FromLocation);

		if (Trigger)
		{
			if (Platform->State == EAIDynamicMapObjectState::OBJECTSTATE_IDLE)
			{
				return AINAV_AddUseMovementTask(NavProfile, ImpactedPathNode->FromLocation, Trigger->Edict, Trigger, NewMoveTask);
			}
			else
			{
				return AINAV_AddMoveMovementTask(NavProfile, ImpactedPathNode->FromLocation, AIMAP_GetButtonFloorLocation(NavProfile, ImpactedPathNode->FromLocation, Trigger->Edict), nullptr, NewMoveTask);
			}
		}
	}

	return false;
}

bool AINAV_AddTriggerMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const DynamicMapObject* Trigger, const DynamicMapObject* TriggerTarget, AvHAIMoveTask& NewTask)
{
	NewTask.Clear();

	if (!NavProfile || !NavProfile->IsValid()) { return false; }
	if (!Trigger || !Trigger->IsValid()) { return false; }
	if (!TriggerTarget || !TriggerTarget->IsValid()) { return false; }

	switch (Trigger->Type)
	{
		case EAIDynamicMapObjectType::TRIGGER_SHOOT:
		case EAIDynamicMapObjectType::TRIGGER_BREAK:
			return AINAV_AddBreakMovementTask(NavProfile, StartPoint, Trigger->Edict, TriggerTarget, NewTask);
			break;
		case EAIDynamicMapObjectType::TRIGGER_TOUCH:
			return AINAV_AddTouchMovementTask(NavProfile, StartPoint, Trigger->Edict, TriggerTarget, NewTask);
			break;
		case EAIDynamicMapObjectType::TRIGGER_USE:
			return AINAV_AddUseMovementTask(NavProfile, StartPoint, Trigger->Edict, TriggerTarget, NewTask);
			break;
		default:
			return AINAV_AddUseMovementTask(NavProfile, StartPoint, Trigger->Edict, TriggerTarget, NewTask);
			break;
	}
}

bool AINAV_AddPickupMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* ThingToPickup, const DynamicMapObject* TriggerToActivate, AvHAIMoveTask& NewTask)
{
	NewTask.Clear();

	NewTask.TaskType = EAIMovementTaskType::MOVE_TASK_PICKUP;
	NewTask.TaskTarget = ThingToPickup;
	NewTask.TriggerToActivate = (TriggerToActivate) ? TriggerToActivate->Edict : nullptr;
	NewTask.TaskLocation = ThingToPickup->v.origin;

	return true;
}

bool AINAV_AddTouchMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* EntityToTouch, const DynamicMapObject* TriggerToActivate, AvHAIMoveTask& NewTask)
{
	NewTask.Clear();

	AvHAIPath TestPath;
	bool bFoundPath = AINAV_FindPathClosestToPoint(NavProfile, StartPoint, UTIL_GetCentreOfEntity(EntityToTouch), &TestPath, 200.0f);

	if (!bFoundPath) { return false; }

	NewTask.TaskType = EAIMovementTaskType::MOVE_TASK_TOUCH;
	NewTask.TaskTarget = EntityToTouch;
	NewTask.TriggerToActivate = (TriggerToActivate) ? TriggerToActivate->Edict : nullptr;
	NewTask.TaskLocation = TestPath.GetFinalDestination();

	return true;
}

bool AINAV_AddBreakMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* EntityToBreak, const DynamicMapObject* TriggerToActivate, AvHAIMoveTask& NewTask)
{
	NewTask.Clear();

	NewTask.TaskType = EAIMovementTaskType::MOVE_TASK_BREAK;
	NewTask.TaskTarget = EntityToBreak;
	NewTask.TriggerToActivate = (TriggerToActivate) ? TriggerToActivate->Edict : nullptr;
	NewTask.TaskLocation = AIMAP_GetButtonFloorLocation(NavProfile, StartPoint, EntityToBreak);

	return true;
}

bool AINAV_AddWeldMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* EntityToWeld, const DynamicMapObject* TriggerToActivate, AvHAIMoveTask& NewTask)
{
	NewTask.Clear();

	NewTask.TaskType = EAIMovementTaskType::MOVE_TASK_BREAK;
	NewTask.TaskTarget = EntityToWeld;
	NewTask.TriggerToActivate = (TriggerToActivate) ? TriggerToActivate->Edict : nullptr;
	NewTask.TaskLocation = AIMAP_GetButtonFloorLocation(NavProfile, StartPoint, EntityToWeld);

	return true;
}

bool AINAV_AddUseMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* EntityToUse, const DynamicMapObject* TriggerToActivate, AvHAIMoveTask& NewTask)
{
	NewTask.Clear();

	NewTask.TaskType = EAIMovementTaskType::MOVE_TASK_USE;
	NewTask.TaskTarget = EntityToUse;
	NewTask.TriggerToActivate = (TriggerToActivate) ? TriggerToActivate->Edict : nullptr;
	NewTask.TaskLocation = AIMAP_GetButtonFloorLocation(NavProfile, StartPoint, EntityToUse);

	return true;
}

bool AINAV_AddMoveMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const Vector& MoveLocation, const DynamicMapObject* TriggerToActivate, AvHAIMoveTask& NewTask)
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

AvHPlayer* AINAV_GetPlayerRidingOnBot(AvHAIPlayer* AIPlayer)
{
	if (!AIPlayer || !AIPlayer->IsValid()) { return nullptr; }

	PlayerListType PotentialRiders = AIMGR_GetAllActivePlayers();

	for (auto it = PotentialRiders.begin(); it != PotentialRiders.end(); it++)
	{
		AvHPlayer* PotentiallyRidingPlayer = (*it);

		if (!PotentiallyRidingPlayer) { continue; }

		edict_t* PotentialRidingEdict = PotentiallyRidingPlayer->edict();

		if (FNullEnt(PotentialRidingEdict)) { continue; }

		if (PotentialRidingEdict->v.groundentity == AIPlayer->Edict)
		{
			return PotentiallyRidingPlayer;
		}
	}

	return nullptr;
}

bool AINAV_NewGroundMove(const AvHAIPlayer* AIPlayer, AvHAIMovementInput& OutMovementInput, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	if (!AIPlayer || !AIPlayer->IsValid() || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return false; }

	const Vector CurrentPos = (AIPlayer->IsOnGround()) ? AIPlayer->Edict->v.origin : AIPlayer->CurrentFloorPosition;

	const Vector vForward = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - CurrentPos);
	// Same goes for the right vector, might not be the same as the bot's right
	const Vector vRight = UTIL_GetVectorNormal(UTIL_GetCrossProduct(vForward, UP_VECTOR));

	bool bAdjustingForCollision = false;

	const float PlayerRadius = AIPlayer->GetPlayerRadius() + 2.0f;

	Vector stTrcLft = CurrentPos - (vRight * PlayerRadius);
	Vector stTrcRt = CurrentPos + (vRight * PlayerRadius);
	Vector endTrcLft = stTrcLft + (vForward * 24.0f);
	Vector endTrcRt = stTrcRt + (vForward * 24.0f);

	bool bumpLeft = !AINAV_IsPointDirectlyReachable(AIPlayer->GetNavProfile(), stTrcLft, endTrcLft);
	bool bumpRight = !AINAV_IsPointDirectlyReachable(AIPlayer->GetNavProfile(), stTrcRt, endTrcRt);

	OutMovementInput.DesiredMoveDirection = vForward;

	if (bumpRight && !bumpLeft)
	{
		OutMovementInput.DesiredMoveDirection = OutMovementInput.DesiredMoveDirection - vRight;
	}
	else if (bumpLeft && !bumpRight)
	{
		OutMovementInput.DesiredMoveDirection = OutMovementInput.DesiredMoveDirection + vRight;
	}
	else if (bumpLeft && bumpRight)
	{
		stTrcLft.z = AIPlayer->Edict->v.origin.z;
		stTrcRt.z = AIPlayer->Edict->v.origin.z;
		endTrcLft.z = AIPlayer->Edict->v.origin.z;
		endTrcRt.z = AIPlayer->Edict->v.origin.z;

		if (!UTIL_QuickTrace(AIPlayer->Edict, stTrcLft, endTrcLft))
		{
			OutMovementInput.DesiredMoveDirection = OutMovementInput.DesiredMoveDirection + vRight;
		}
		else
		{
			OutMovementInput.DesiredMoveDirection = OutMovementInput.DesiredMoveDirection - vRight;
		}
	}
	else
	{
		const float DistFromLine = vDistanceFromLine2D(CurrentPathNode->FromLocation, CurrentPathNode->ToLocation, CurrentPos);

		if (DistFromLine > 18.0f)
		{
			float modifier = (float)vPointOnLine(CurrentPathNode->FromLocation, CurrentPathNode->ToLocation, CurrentPos);
			OutMovementInput.DesiredMoveDirection = OutMovementInput.DesiredMoveDirection + (vRight * modifier);
		}
	}

	OutMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(OutMovementInput.DesiredMoveDirection);

	if (AIPlayer->CanCrouch())
	{
		if (EnumHasAnyFlags(CurrentPathNode->MovementFlag, EAINavMovementFlag::NAV_FLAG_CROUCH))
		{
			OutMovementInput.bShouldCrouch = true;
		}
		else
		{
			Vector HeadLocation = GetPlayerTopOfCollisionHull(AIPlayer->Edict, false);

			// Crouch if we have something in our way at head height
			if (!UTIL_QuickTrace(AIPlayer->Edict, HeadLocation, (HeadLocation + (OutMovementInput.DesiredMoveDirection * 50.0f))))
			{
				OutMovementInput.bShouldCrouch = true;
			}
		}
	}
}

bool AINAV_NewFallMove(const AvHAIPlayer* AIPlayer, AvHAIMovementInput& OutMovementInput, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	const Vector AIPlayerLocation = AIPlayer->GetLocation();
	const Vector vBotOrientation = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - AIPlayerLocation);
	const Vector vForward = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - CurrentPathNode->FromLocation);

	if (!AIPlayer->IsOnGround())
	{
		OutMovementInput.DesiredMoveDirection = vBotOrientation;
		return true;
	}

	if (vDist2DSq(AIPlayerLocation, CurrentPathNode->ToLocation) > sqrf(AIPlayer->GetPlayerRadius()))
	{
		OutMovementInput.DesiredMoveDirection = vBotOrientation;
	}
	else
	{
		OutMovementInput.DesiredMoveDirection = vForward;
	}

	if (!AIPlayer->CanCrouch()) { return; }

	const Vector HeadLocation = GetPlayerTopOfCollisionHull(AIPlayer->Edict, false);

	if (!UTIL_QuickTrace(AIPlayer->Edict, HeadLocation, (HeadLocation + (OutMovementInput.DesiredMoveDirection * 50.0f))))
	{
		OutMovementInput.bShouldCrouch = true;
	}
}

bool AINAV_NewJumpMove(const AvHAIPlayer* AIPlayer, AvHAIMovementInput& OutMovementInput, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	Vector vForward = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - AIPlayer->GetLocation());

	if (vIsZero(vForward))
	{
		vForward = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - CurrentPathNode->FromLocation);
	}

	const Vector CurrentVelocity = AIPlayer->GetVelocity();
	const Vector CurrentVelocity2D = UTIL_GetVectorNormal2D(CurrentVelocity);

	OutMovementInput.DesiredMoveDirection = vForward;

	float Dot = UTIL_GetDotProduct2D(vForward, CurrentVelocity2D);

	// Yes this is cheating, but I'm up against millions of years of human evolution here...
	if (AIPlayer->IsOnGround() && Dot < 0.95f)
	{
		float MoveSpeed = vSize2D(AIPlayer->GetVelocity());
		Vector NewVelocity = vForward * fmaxf(MoveSpeed, AIPlayer->GetDesiredMovementSpeed(false));
		NewVelocity.z = CurrentVelocity.z;

		OutMovementInput.VelocityOverride = NewVelocity;
	}

	AIPlayer->Jump(OutMovementInput, true);

	if (!AIPlayer->CanCrouch()) { return; }

	Vector HeadLocation = GetPlayerTopOfCollisionHull(AIPlayer->Edict, false);

	if (!UTIL_QuickTrace(AIPlayer->Edict, HeadLocation, (HeadLocation + (OutMovementInput.DesiredMoveDirection * 50.0f))))
	{
		OutMovementInput.bShouldCrouch = true;
	}
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

void AINAV_NewMountLadderMove(const AvHAIPlayer* AIPlayer, AvHAIMovementInput& OutMovementInput, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	edict_t* MountLadder = UTIL_GetNearestLadderAtPoint(CurrentPathNode->FromLocation);

	if (FNullEnt(MountLadder))
	{
		AIPlayer->MoveToWithoutNav(CurrentPathNode->ToLocation, OutMovementInput);
		return;
	}

	const Vector BotCurrentLocation = AIPlayer->GetLocation();

	Vector LadderCentre = UTIL_GetCentreOfEntity(MountLadder);

	Vector MountPoint = AINAV_GetLadderMountPoint(MountLadder, CurrentPathNode->ToLocation);

	bool bMountingFromTop = BotCurrentLocation.z > MountLadder->v.absmax.z;

	if (!vEquals(MountPoint, LadderCentre) && !bMountingFromTop)
	{
		Vector AnglePlayerToLadder = UTIL_GetVectorNormal2D(LadderCentre - BotCurrentLocation);
		Vector AngleMountPointToLadder = UTIL_GetVectorNormal2D(LadderCentre - MountPoint);

		if (UTIL_GetDotProduct2D(AnglePlayerToLadder, AngleMountPointToLadder) > 0.9f)
		{
			MountPoint = LadderCentre;
		}
	}

	if (bMountingFromTop)
	{
		OutMovementInput.bShouldWalk = true;
	}

	OutMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(MountPoint - BotCurrentLocation);
}

bool AINAV_NewLadderMove(const AvHAIPlayer* AIPlayer, AvHAIMovementInput& OutMovementInput, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	bool bIsGoingUpLadder = (CurrentPathNode->FromLocation.z < CurrentPathNode->ToLocation.z);
	bool bAtAppropriateClimbHeight = (bIsGoingUpLadder) ? (AIPlayer->GetLocation().z >= CurrentPathNode->RequiredClimbZ) : (AIPlayer->GetLocation().z <= CurrentPathNode->RequiredClimbZ);

	const NavAgentProfile* NavProfile = AIPlayer->GetNavProfile();

	if (!AIPlayer->IsOnLadder())
	{
		if (!AIPlayer->IsOnGround() || AINAV_IsPointDirectlyReachable(NavProfile, AIPlayer->GetBottomOfHitbox(), CurrentPathNode->ToLocation))
		{
			AIPlayer->MoveToWithoutNav(CurrentPathNode->ToLocation, OutMovementInput);
			return;
		}
		else
		{
			AINAV_NewMountLadderMove(AIPlayer, OutMovementInput, CurrentPathNode, NextPathNode);
			return;
		}
	}

	const Vector BotLocation = AIPlayer->GetLocation();
	const Vector BotEyePosition = AIPlayer->GetEyePosition();
	const Vector CollisionBottomLocation = AIPlayer->GetBottomOfHitbox();
	const Vector CollisionTopLocation = AIPlayer->GetTopOfHitbox();
	const float PlayerRadius = AIPlayer->GetPlayerRadius();

	edict_t* CurrentLadder = UTIL_GetNearestLadderAtPoint(BotLocation);
	Vector LadderTop = UTIL_GetCentreOfEntity(CurrentLadder);
	LadderTop.z = CurrentLadder->v.absmax.z;

	// We're on the ladder and actively climbing

	Vector LadderNormalCheck = CollisionBottomLocation + Vector(0.0f, 0.0f, 18.0f);
	LadderNormalCheck = LadderNormalCheck + UTIL_GetVectorNormal2D(LadderNormalCheck - LadderTop);

	Vector CurrentLadderNormal = UTIL_GetNearestLadderNormal(LadderNormalCheck);

	CurrentLadderNormal = UTIL_GetVectorNormal2D(CurrentLadderNormal);

	if (vIsZero(CurrentLadderNormal))
	{

		if (CurrentPathNode->ToLocation.z > CurrentPathNode->FromLocation.z)
		{
			CurrentLadderNormal = UTIL_GetVectorNormal2D(CurrentPathNode->FromLocation - CurrentPathNode->ToLocation);
		}
		else
		{
			CurrentLadderNormal = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - CurrentPathNode->FromLocation);
		}
	}

	const Vector LadderRightNormal = UTIL_GetVectorNormal(UTIL_GetCrossProduct(CurrentLadderNormal, UP_VECTOR));

	Vector ClimbRightNormal = (bIsGoingUpLadder) ? -LadderRightNormal : LadderRightNormal;

	Vector ClimbDir = (bIsGoingUpLadder) ? -CurrentLadderNormal : CurrentLadderNormal;

	Vector DisembarkDir = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - AIPlayer->GetLocation());
	float DisembarkDot = UTIL_GetDotProduct2D(CurrentLadderNormal, DisembarkDir);

	float DesiredClimbHeight = CurrentPathNode->RequiredClimbZ;

	if (DisembarkDot > 0.75f)
	{
		float JumpDist = vDist2DSq(AIPlayer->GetLocation(), CurrentPathNode->ToLocation);

		float ExtraClimbHeight = (JumpDist > sqrf(2.0f)) ? 100.0f : 50.0f;

		DesiredClimbHeight = fminf((CurrentPathNode->RequiredClimbZ + ExtraClimbHeight), LadderTop.z + GetPlayerOriginOffsetFromFloor(AIPlayer->Edict, true).z);
	}

	// First check if we should dismount the ladder

	bIsGoingUpLadder = BotLocation.z < DesiredClimbHeight;

	if (bIsGoingUpLadder)
	{
		// We've reached the end of the ladder, try and make it to the disembark point if we can
		if (LadderTop.z - CollisionBottomLocation.z <= 2.0f || BotLocation.z >= DesiredClimbHeight)
		{
			AIPlayer->MoveToWithoutNav(CurrentPathNode->ToLocation, OutMovementInput);

			const Vector DesiredLookTarget = (BotEyePosition + (DisembarkDir * 50.0f)) + Vector(0.0f, 0.0f, 50.0f);

			OutMovementInput.RequiredLookLocation = DesiredLookTarget;

			if (DisembarkDot > 0.75f)
			{
				AIPlayer->Jump(OutMovementInput, true);
			}

			return;
		}
	}
	else
	{
		bool bDesiredGoingDownLadder = CurrentPathNode->FromLocation.z > CurrentPathNode->ToLocation.z;

		if (bDesiredGoingDownLadder && (BotLocation.z <= DesiredClimbHeight || (CollisionBottomLocation.z - CurrentPathNode->ToLocation.z < 100.0f)))
		{
			// We're close enough to the end that we can jump off the ladder
			if (UTIL_QuickTrace(AIPlayer->Edict, CollisionTopLocation, CurrentPathNode->ToLocation))
			{
				AIPlayer->MoveToWithoutNav(CurrentPathNode->ToLocation, OutMovementInput);
				AIPlayer->Jump(OutMovementInput, true);
				return;
			}
		}
	}

	// Still climbing

	Vector TraceStartPosition = (bIsGoingUpLadder) ? CollisionTopLocation : CollisionBottomLocation;

	Vector StartLeftTrace = TraceStartPosition - (ClimbRightNormal * PlayerRadius);
	Vector StartRightTrace = TraceStartPosition + (ClimbRightNormal * PlayerRadius);

	Vector EndLeftTrace = (bIsGoingUpLadder) ? StartLeftTrace + Vector(0.0f, 0.0f, 2.0f) : StartLeftTrace - Vector(0.0f, 0.0f, 2.0f);
	Vector EndRightTrace = (bIsGoingUpLadder) ? StartRightTrace + Vector(0.0f, 0.0f, 2.0f) : StartRightTrace - Vector(0.0f, 0.0f, 2.0f);

	bool bBlockedLeft = !UTIL_QuickTrace(AIPlayer->Edict, StartLeftTrace, EndLeftTrace);
	bool bBlockedRight = !UTIL_QuickTrace(AIPlayer->Edict, StartRightTrace, EndRightTrace);

	// Look up at the top of the ladder

	// If we are blocked going up the ladder, face the ladder and slide left/right to avoid blockage
	if (bBlockedLeft && !bBlockedRight)
	{
		Vector LookLocation = BotLocation - (CurrentLadderNormal * 50.0f);
		LookLocation.z = CurrentPathNode->RequiredClimbZ + 100.0f;

		OutMovementInput.RequiredLookLocation = LookLocation;
		OutMovementInput.DesiredMoveDirection = ClimbRightNormal;

		return;
	}

	if (bBlockedRight && !bBlockedLeft)
	{
		Vector LookLocation = BotLocation - (CurrentLadderNormal * 50.0f);
		LookLocation.z = CurrentPathNode->RequiredClimbZ + 100.0f;

		OutMovementInput.RequiredLookLocation = LookLocation;
		OutMovementInput.DesiredMoveDirection = -ClimbRightNormal;

		return;
	}

	if (AIPlayer->CanCrouch())
	{
		Vector HeadTraceLocation = CollisionTopLocation;

		bool bHittingHead = !UTIL_QuickTrace(AIPlayer->Edict, HeadTraceLocation, HeadTraceLocation + Vector(0.0f, 0.0f, 2.0f));

		if (bHittingHead)
		{
			OutMovementInput.bShouldCrouch = true;
		}
	}

	OutMovementInput.DesiredMoveDirection = ClimbDir;

	Vector LookTarget = CurrentPathNode->ToLocation;

	if (bIsGoingUpLadder)
	{
		LookTarget = LadderTop + (ClimbDir * 50.0f);
		LookTarget.z = DesiredClimbHeight + 50.0f;
	}

	OutMovementInput.RequiredLookLocation = LookTarget;
}

bool AINAV_NewPlatformMove(const AvHAIPlayer* AIPlayer, AvHAIMovementInput& OutMovementInput, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{

}

bool AINAV_NextMove(const AvHAIPlayer* AIPlayer, AvHAIMovementInput& OutMovementInput, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	OutMovementInput.Clear();

	if (!AIPlayer || !AIPlayer->IsValid() || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return false; }

	bool bMoveSuccess = false;

	switch (CurrentPathNode->MovementFlag)
	{
		case EAINavMovementFlag::NAV_FLAG_WALK:
			return AINAV_NewGroundMove(AIPlayer, OutMovementInput, CurrentPathNode, NextPathNode);
			break;
		case EAINavMovementFlag::NAV_FLAG_FALL:
			return AINAV_NewFallMove(AIPlayer, OutMovementInput, CurrentPathNode, NextPathNode);
			break;
		case EAINavMovementFlag::NAV_FLAG_JUMP:
			return AINAV_NewJumpMove(AIPlayer, OutMovementInput, CurrentPathNode, NextPathNode);
			break;
		case EAINavMovementFlag::NAV_FLAG_LADDER:
			return AINAV_NewLadderMove(AIPlayer, OutMovementInput, CurrentPathNode, NextPathNode);
			break;
		case EAINavMovementFlag::NAV_FLAG_PLATFORM:
			return AINAV_NewPlatformMove(AIPlayer, OutMovementInput, CurrentPathNode, NextPathNode);
			break;
		default:
			return AINAV_NewGroundMove(AIPlayer, OutMovementInput, CurrentPathNode, NextPathNode);
			break;
	}
}

bool AINAV_NextSwimMove(const AvHAIPlayer* AIPlayer, AvHAIMovementInput& OutMovementInput, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode)
{
	OutMovementInput.Clear();

	if (!AIPlayer || !AIPlayer->IsValid() || !CurrentPathNode || !CurrentPathNode->IsValidMove()) { return false; }
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

EAINavMoveResult AINAV_ProgressMoveTask(AvHAIPlayer* AIPlayer, AvHAIMoveTask* MoveTask, AvHAIMovementInput& OutMovementInputs)
{
	if (!AIPlayer || !AIPlayer->IsValid()) { return EAINavMoveResult::NAV_MOVE_NOTASK; }

	if (!MoveTask || !MoveTask->IsValid()) { return EAINavMoveResult::NAV_MOVE_INVALIDTASK; }

	if (!MoveTask->HasPath())
	{
		const bool bSuccess = AINAV_FindPathClosestToPoint(AIPlayer->GetNavProfile(), UTIL_GetFloorUnderEntity(AIPlayer->Edict), MoveTask->TaskLocation, &MoveTask->TaskPath, AIPlayer->GetPlayerRadius());

		if (!bSuccess)
		{
			return EAINavMoveResult::NAV_MOVE_NOPATH;
		}
	}

	return AINAV_FollowPath(AIPlayer, &MoveTask->TaskPath);
}










void BlockedMove(AvHAIPlayer* pBot, const Vector StartPoint, const Vector EndPoint)
{
	Vector vForward = UTIL_GetVectorNormal2D(EndPoint - pBot->Edict->v.origin);

	if (vIsZero(vForward))
	{
		vForward = UTIL_GetVectorNormal2D(EndPoint - StartPoint);

		if (vIsZero(vForward))
		{
			if (pBot->BotNavInfo.CurrentPathPoint < pBot->BotNavInfo.CurrentPath.size() - 1)
			{
				bot_path_node NextPathNode = pBot->BotNavInfo.CurrentPath[pBot->BotNavInfo.CurrentPathPoint + 1];

				vForward = UTIL_GetVectorNormal2D(NextPathNode.Location - pBot->Edict->v.origin);
			}
			else
			{
				vForward = UTIL_GetForwardVector2D(pBot->Edict->v.angles);
			}
		}
	}

	pBot->desiredMovementDir = vForward;

	Vector CurrVelocity = UTIL_GetVectorNormal2D(pBot->Edict->v.velocity);

	float Dot = UTIL_GetDotProduct2D(vForward, CurrVelocity);

	Vector FaceDir = UTIL_GetForwardVector2D(pBot->Edict->v.angles);

	float FaceDot = UTIL_GetDotProduct2D(FaceDir, vForward);

	// Yes this is cheating, but is it not cheating for humans to have millions of years of evolution
	// driving their ability to judge a jump, while the bots have a single year of coding from a moron?
	if (FaceDot < 0.95f)
	{
		float MoveSpeed = vSize2D(pBot->Edict->v.velocity);
		if (MoveSpeed < 20.0f)
		{
			MoveSpeed = 100.0f;
		}
		Vector NewVelocity = vForward * MoveSpeed;
		NewVelocity.z = pBot->Edict->v.velocity.z;

		pBot->Edict->v.velocity = NewVelocity;
	}

	BotJump(pBot);
}

void PhaseGateMove(AvHAIPlayer* pBot, const Vector StartPoint, const Vector EndPoint)
{
	StructureSearchFilter PGFilter;
	PGFilter.DeployableTeam = pBot->Player->GetTeam();
	PGFilter.DeployableTypes = STRUCTURE_MARINE_PHASEGATE;
	PGFilter.MaxSearchRadius = UTIL_MetresToGoldSrcUnits(2.0f);
	PGFilter.IncludeStatusFlags = STRUCTURE_STATUS_COMPLETED;

	AvHAIBuildableStructure NearestPhaseGate = AITAC_FindClosestDeployableToLocation(pBot->Edict->v.origin, &PGFilter);

	if (!NearestPhaseGate.IsValid()) { return; }

	if (IsPlayerInUseRange(pBot->Edict, NearestPhaseGate.edict))
	{
		BotMoveLookAt(pBot, NearestPhaseGate.edict->v.origin);
		pBot->desiredMovementDir = g_vecZero;
		BotUseObject(pBot, NearestPhaseGate.edict, false);

		if (vDist2DSq(pBot->Edict->v.origin, NearestPhaseGate.edict->v.origin) < sqrf(16.0f))
		{
			pBot->desiredMovementDir = UTIL_GetForwardVector2D(NearestPhaseGate.edict->v.angles);
		}

		return;
	}
	else
	{
		pBot->desiredMovementDir = UTIL_GetVectorNormal2D(NearestPhaseGate.edict->v.origin - pBot->Edict->v.origin);
	}
}

bool IsBotOffLadderNode(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag)
{
	if (!IsPlayerOnLadder(pBot->Edict))
	{
		if (IsPlayerClimbingWall(pBot->Edict)) { return true; }

		if (pBot->BotNavInfo.IsOnGround)
		{
			if (!UTIL_PointIsDirectlyReachable(GetPlayerBottomOfCollisionHull(pBot->Edict), MoveStart) && !UTIL_PointIsDirectlyReachable(GetPlayerBottomOfCollisionHull(pBot->Edict), MoveEnd)) { return true; }
		}
	}

	return false;
}

bool IsBotOffClimbNode(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag)
{
	if (!IsPlayerClimbingWall(pBot->Edict) && (pBot->Edict->v.flags & FL_ONGROUND))
	{
		return (!UTIL_PointIsDirectlyReachable(GetPlayerBottomOfCollisionHull(pBot->Edict), MoveStart) && !UTIL_PointIsDirectlyReachable(GetPlayerBottomOfCollisionHull(pBot->Edict), MoveEnd));
	}

	Vector ClosestPointOnLine = vClosestPointOnLine2D(MoveStart, MoveEnd, pBot->Edict->v.origin);

	return vDist2DSq(pBot->Edict->v.origin, ClosestPointOnLine) > sqrf(GetPlayerRadius(pBot->Edict) * 3.0f);
}

bool IsBotOffPhaseGateNode(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag)
{
	if (vDist2DSq(pBot->Edict->v.origin, MoveStart) > sqrf(UTIL_MetresToGoldSrcUnits(2.0f)) && vDist2DSq(pBot->Edict->v.origin, MoveEnd) > sqrf(UTIL_MetresToGoldSrcUnits(2.0f))) { return true; }

	StructureSearchFilter PGFilter;
	PGFilter.DeployableTeam = pBot->Player->GetTeam();
	PGFilter.IncludeStatusFlags = STRUCTURE_STATUS_COMPLETED;
	PGFilter.ExcludeStatusFlags = STRUCTURE_STATUS_RECYCLING;
	PGFilter.MaxSearchRadius = UTIL_MetresToGoldSrcUnits(2.0f);

	bool StartPGExists = AITAC_DeployableExistsAtLocation(MoveStart, &PGFilter);

	if (!StartPGExists) { return true; }

	bool EndPGExists = AITAC_DeployableExistsAtLocation(MoveEnd, &PGFilter);

	if (!EndPGExists) { return true; }

	return false;
}

bool IsBotOffObstacleNode(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag)
{
	return IsBotOffJumpNode(pBot, MoveStart, MoveEnd, NextMoveDestination, NextMoveFlag);
}

void BlinkClimbMove(AvHAIPlayer* pBot, const Vector StartPoint, const Vector EndPoint, float RequiredClimbHeight)
{
	edict_t* pEdict = pBot->Edict;

	Vector vForward = UTIL_GetVectorNormal2D(EndPoint - StartPoint);
	Vector CheckLine = StartPoint + (vForward * 1000.0f);
	Vector MoveDir = UTIL_GetVectorNormal2D(EndPoint - pBot->Edict->v.origin);

	Vector PointOnMove = vClosestPointOnLine2D(StartPoint, EndPoint, pEdict->v.origin);
	float DistFromLineSq = vDist2DSq(PointOnMove, pEdict->v.origin);

	if (vEquals(PointOnMove, StartPoint, 2.0f) && DistFromLineSq > sqrf(8.0f))
	{
		pBot->desiredMovementDir = UTIL_GetVectorNormal2D(StartPoint - pBot->Edict->v.origin);
		return;
	}

	pBot->desiredMovementDir = MoveDir;

	// Always duck. It doesn't have any downsides and means we don't have to separately handle vent climbing
	pBot->Button |= IN_DUCK;

	pBot->DesiredMoveWeapon = WEAPON_FADE_BLINK;

	// Wait until we have blink equipped before proceeding
	if (GetPlayerCurrentWeapon(pBot->Player) != WEAPON_FADE_BLINK) { return; }

	// Only blink if we're below the target climb height
	if (pEdict->v.origin.z < RequiredClimbHeight + 4.0f && !UTIL_QuickHullTrace(pBot->Edict, pBot->Edict->v.origin, Vector(EndPoint.x, EndPoint.y, pBot->Edict->v.origin.z)))
	{
		Vector CurrVelocity = UTIL_GetVectorNormal2D(pBot->Edict->v.velocity);

		float Dot = UTIL_GetDotProduct2D(MoveDir, CurrVelocity);

		Vector FaceDir = UTIL_GetForwardVector2D(pEdict->v.angles);

		float FaceDot = UTIL_GetDotProduct2D(FaceDir, MoveDir);

		// Yes this is cheating, but the fades were struggling with zipping off-target when trying to blink
		// Better this than fades getting constantly chewed up by marines because they can't escape properly
		if (FaceDot < 0.95f)
		{
			float MoveSpeed = vSize2D(pBot->Edict->v.velocity);

			if (MoveSpeed < 20.0f)
			{
				MoveSpeed = 100.0f;
			}

			Vector NewVelocity = MoveDir * MoveSpeed;
			NewVelocity.z = pBot->Edict->v.velocity.z;

			pBot->Edict->v.velocity = NewVelocity;
		}

		float ZDiff = fabs(pEdict->v.origin.z - (RequiredClimbHeight + 16.0f));

		// We don't want to blast off like a rocket, so only apply enough blink until our upwards velocity is enough to carry us to the desired height
		float DesiredZVelocity = sqrtf(2.0f * GOLDSRC_GRAVITY * (ZDiff + 10.0f));

		if (pBot->Edict->v.velocity.z < DesiredZVelocity)
		{
			bool bHasHeadroom = UTIL_QuickHullTrace(pBot->Edict, pBot->Edict->v.origin, pBot->Edict->v.origin + Vector(0.0f, 0.0f, 4.0f));
			// We're going to cheat and give the bot the necessary energy to make the move. Better the fade cheats a bit than gets stuck somewhere
			if (GetPlayerEnergy(pBot->Edict) < 0.1f)
			{
				pBot->Player->Energize(0.1f);
			}

			if (!bHasHeadroom || pBot->Edict->v.origin.z >= RequiredClimbHeight)
			{
				Vector LookPoint = EndPoint;
				LookPoint.z = pBot->CurrentEyePosition.z + 5.0f;
				BotMoveLookAt(pBot, LookPoint);
			}
			else
			{
				BotMoveLookAt(pBot, EndPoint + Vector(0.0f, 0.0f, 100.0f));
			}


			pBot->Button |= IN_ATTACK2;
		}
		else
		{
			Vector LookAtTarget = EndPoint;
			LookAtTarget.z = pBot->CurrentEyePosition.z;
			BotMoveLookAt(pBot, LookAtTarget);
		}
	}
}

void WallClimbMove(AvHAIPlayer* pBot, const Vector StartPoint, const Vector EndPoint, float RequiredClimbHeight)
{
	edict_t* pEdict = pBot->Edict;

	if (UTIL_PointIsDirectlyReachable(pBot->BotNavInfo.NavProfile, pBot->CurrentFloorPosition, EndPoint))
	{
		Vector PointOnMoveLine = vClosestPointOnLine2D(StartPoint, EndPoint, pBot->Edict->v.origin);

		if (vEquals2D(PointOnMoveLine, EndPoint, 4.0f))
		{

			// Stop holding crouch if we're a skulk so we can actually climb
			if (IsPlayerSkulk(pBot->Edict))
			{
				pBot->Button &= ~IN_DUCK;
			}

			pBot->desiredMovementDir = UTIL_GetVectorNormal2D(EndPoint - pBot->CurrentFloorPosition);

			return;
		}
	}

	Vector vForward = UTIL_GetVectorNormal2D(EndPoint - StartPoint);
	Vector ClimbAngle = UTIL_GetVectorNormal(Vector(EndPoint.x, EndPoint.y, RequiredClimbHeight) - pBot->Edict->v.origin);

	TraceResult SurfaceCheck;

	UTIL_TraceHull(pBot->Edict->v.origin - Vector(0.0f, 0.0f, 1.0f), pBot->Edict->v.origin + Vector(0.0f, 0.0f, 5.0f), ignore_monsters, head_hull, pBot->Edict->v.pContainingEntity, &SurfaceCheck);

	Vector CeilNormal = SurfaceCheck.vecPlaneNormal;

	bool bIsUnderClimbing = false;

	bool bClimbingUnderway = ((pBot->CollisionHullBottomLocation.z - StartPoint.z) >= 32.0f) && IsPlayerClimbingWall(pBot->Edict);

	if (pEdict->v.origin.z < (RequiredClimbHeight - 10.0f) && !(pEdict->v.flags & FL_ONGROUND) && bClimbingUnderway)
	{
		bIsUnderClimbing = (SurfaceCheck.flFraction < 1.0f && UTIL_GetDotProduct(ClimbAngle, CeilNormal) < 0.0f);
	}

	if (bIsUnderClimbing)
	{
		pBot->Button |= IN_WALK;
		vForward = (UTIL_GetDotProduct2D(vForward, CeilNormal) > 0.0f) ? vForward : -vForward;

	}

	Vector vRight = UTIL_GetVectorNormal(UTIL_GetCrossProduct(vForward, UP_VECTOR));

	pBot->desiredMovementDir = vForward;

	Vector CheckLine = StartPoint + (vForward * 1000.0f);

	float DistFromLine = vDistanceFromLine2D(StartPoint, CheckLine, pEdict->v.origin);

	// Draw an imaginary 2D line between from and to movement, and make sure we're aligned. If we've drifted off to one side, readjust.
	if (DistFromLine > 18.0f)
	{
		float modifier = (float)vPointOnLine(StartPoint, CheckLine, pEdict->v.origin);

		pBot->desiredMovementDir = UTIL_GetVectorNormal2D(pBot->desiredMovementDir + (vRight * modifier));
	}

	// Stop holding crouch if we're a skulk so we can actually climb
	if (IsPlayerSkulk(pBot->Edict))
	{
		pBot->Button &= ~IN_DUCK;
	}

	float ZDiff = fabs(pEdict->v.origin.z - RequiredClimbHeight);
	Vector AdjustedTargetLocation = EndPoint + (UTIL_GetVectorNormal2D(EndPoint - StartPoint) * 1000.0f);
	Vector DirectAheadView = pBot->CurrentEyePosition + (UTIL_GetVectorNormal2D(AdjustedTargetLocation - pBot->CurrentEyePosition) * 100.0f);

	Vector LookLocation = g_vecZero;

	if (ZDiff < 1.0f)
	{
		LookLocation = DirectAheadView;
	}
	else
	{
		// Don't look up/down quite so much as we reach the desired height so we slow down a bit, reduces the chance of over-shooting and climbing right over a vent
		if (pEdict->v.origin.z > RequiredClimbHeight)
		{
			if (ZDiff > 16.0f)
			{
				ClimbAngle = ClimbAngle - (2.0f * (UTIL_GetDotProduct(ClimbAngle, UP_VECTOR) * ClimbAngle));
				LookLocation = pBot->CurrentEyePosition + (ClimbAngle * 100.0f);
			}
			else
			{
				LookLocation = DirectAheadView - Vector(0.0f, 0.0f, 20.0f);
			}
		}
		else
		{
			if (bIsUnderClimbing)
			{
				LookLocation = pBot->CurrentEyePosition + vForward;
				LookLocation.z = EndPoint.z + 100.0f;
			}
			else
			{
				if (bClimbingUnderway)
				{
					LookLocation = pBot->CurrentEyePosition + (ClimbAngle * 100.0f);
				}
				else
				{
					LookLocation = pBot->CurrentEyePosition + vForward;
					LookLocation.z = RequiredClimbHeight;
				}
			}
		}
	}

	if (IsPlayerClimbingWall(pBot->Edict) && !bIsUnderClimbing)
	{
		Vector RightDir = UTIL_GetCrossProduct(vForward, UP_VECTOR);

		Vector LeftCheckStart = pBot->Edict->v.origin - (RightDir * (GetPlayerRadius(pBot->Player) + 2.0f));
		Vector LeftCheckEnd = LeftCheckStart + Vector(0.0f, 0.0f, 50.0f);

		Vector RightCheckStart = pBot->Edict->v.origin + (RightDir * (GetPlayerRadius(pBot->Player) + 2.0f));
		Vector RightCheckEnd = RightCheckStart + Vector(0.0f, 0.0f, 50.0f);

		if (!UTIL_QuickTrace(pBot->Edict, LeftCheckStart, LeftCheckEnd))
		{
			if (UTIL_QuickTrace(pBot->Edict, RightCheckStart, RightCheckEnd))
			{
				pBot->desiredMovementDir = UTIL_GetVectorNormal2D(vForward + RightDir);
			}
		}
		else if (!UTIL_QuickTrace(pBot->Edict, RightCheckStart, RightCheckEnd))
		{
			pBot->desiredMovementDir = UTIL_GetVectorNormal2D(vForward - RightDir);
		}
	}

	BotMoveLookAt(pBot, LookLocation, true);

}


void BotMovementInputs(AvHAIPlayer* pBot)
{
	if (vIsZero(pBot->desiredMovementDir)) { return; }

	edict_t* pEdict = pBot->Edict;

	UTIL_NormalizeVector2D(&pBot->desiredMovementDir);

	float currentYaw = pBot->Edict->v.v_angle.y;
	float moveDelta = UTIL_VecToAngles(pBot->desiredMovementDir).y;
	float angleDelta = currentYaw - moveDelta;

	float botSpeed = (pBot->BotNavInfo.bShouldWalk) ? (pBot->Edict->v.maxspeed * 0.4f) : pBot->Edict->v.maxspeed;

	if (pBot->BotNavInfo.bShouldWalk)
	{
		pBot->Button |= IN_WALK;
	}

	if (angleDelta < -180.0f)
	{
		angleDelta += 360.0f;
	}
	else if (angleDelta > 180.0f)
	{
		angleDelta -= 360.0f;
	}

	if (angleDelta >= -22.5f && angleDelta < 22.5f)
	{
		pBot->ForwardMove = botSpeed;
		pBot->SideMove = 0.0f;
		pBot->Button |= IN_FORWARD;
	}
	else if (angleDelta >= 22.5f && angleDelta < 67.5f)
	{
		pBot->ForwardMove = botSpeed;
		pBot->SideMove = botSpeed;
		pBot->Button |= IN_FORWARD;
		pBot->Button |= IN_MOVERIGHT;
	}
	else if (angleDelta >= 67.5f && angleDelta < 112.5f)
	{
		pBot->ForwardMove = 0.0f;
		pBot->SideMove = botSpeed;
		pBot->Button |= IN_MOVERIGHT;
	}
	else if (angleDelta >= 112.5f && angleDelta < 157.5f)
	{
		pBot->ForwardMove = -botSpeed;
		pBot->SideMove = botSpeed;
		pBot->Button |= IN_BACK;
		pBot->Button |= IN_MOVERIGHT;
	}
	else if (angleDelta >= 157.5f || angleDelta <= -157.5f)
	{
		pBot->ForwardMove = -botSpeed;
		pBot->SideMove = 0.0f;
		pBot->Button |= IN_BACK;
	}
	else if (angleDelta >= -157.5f && angleDelta < -112.5f)
	{
		pBot->ForwardMove = -botSpeed;
		pBot->SideMove = -botSpeed;
		pBot->Button |= IN_BACK;
		pBot->Button |= IN_MOVELEFT;
	}
	else if (angleDelta >= -112.5f && angleDelta < -67.5f)
	{
		pBot->ForwardMove = 0.0f;
		pBot->SideMove = -botSpeed;
		pBot->Button |= IN_MOVELEFT;
	}
	else if (angleDelta >= -67.5f && angleDelta < -22.5f)
	{
		pBot->ForwardMove = botSpeed;
		pBot->SideMove = -botSpeed;
		pBot->Button |= IN_FORWARD;
		pBot->Button |= IN_MOVELEFT;
	}

	if (pBot->BotNavInfo.CurrentPath.size() == 0 || pBot->BotNavInfo.CurrentPathPoint >= pBot->BotNavInfo.CurrentPath.size() || pBot->BotNavInfo.CurrentPath[pBot->BotNavInfo.CurrentPathPoint].flag != SAMPLE_POLYFLAGS_LADDER)
	{
		if (pBot->Player->IsOnLadder())
		{
			BotJump(pBot);
		}
	}
}
