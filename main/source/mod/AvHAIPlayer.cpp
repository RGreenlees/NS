
#include "AvHAIPlayer.h"
#include "AvHAIPlayerUtil.h"
#include "AvHAIHelper.h"
#include "AvHAIMath.h"
#include "AvHAINavigation.h"
#include "AvHAIWeaponHelper.h"
#include "AvHAITactical.h"
#include "AvHAITask.h"
#include "AvHAICommander.h"
#include "AvHAIPlayerManager.h"
#include "AvHAIConfig.h"

#include "AvHGamerules.h"
#include "AvHMessage.h"
#include "AvHTurret.h"
#include "AvHWeldable.h"
#include "AvHServerUtil.h"
#include "AvHMovementUtil.h"
#include <pm_shared/pm_shared.h>
#include <pm_shared/pm_defs.h>

namespace
{
	struct AIMovementStructureCollision
	{
		edict_t* Edict = nullptr;
		Vector Mins = ZERO_VECTOR;
		Vector Maxs = ZERO_VECTOR;
		Vector Axis[3];
	};

	bool NS_GetCollisionSizeForUser3(AvHUser3 inUser3, vec3_t& outMinSize, vec3_t& outMaxSize)
	{

		switch (inUser3)
		{

		case AVH_USER3_COMMANDER_STATION:
			outMinSize[0] = -45;
			outMinSize[1] = -16;
			outMinSize[2] = 0;
			outMaxSize[0] = 45;
			outMaxSize[1] = 16;
			outMaxSize[2] = 70.34;
			return true;

		case AVH_USER3_INFANTRYPORTAL:
			outMinSize[0] = -25;
			outMinSize[1] = -25;
			outMinSize[2] = 0;
			outMaxSize[0] = 25;
			outMaxSize[1] = 25;
			outMaxSize[2] = 5;
			return true;

		case AVH_USER3_PHASEGATE:
			outMinSize[0] = -55;
			outMinSize[1] = -40;
			outMinSize[2] = 0;
			outMaxSize[0] = 40;
			outMaxSize[1] = 50;
			outMaxSize[2] = 14.49;
			return true;

		case AVH_USER3_ARMORY:
		case AVH_USER3_ADVANCED_ARMORY:
			outMinSize[0] = -20;
			outMinSize[1] = -20;
			outMinSize[2] = 0;
			outMaxSize[0] = 20;
			outMaxSize[1] = 20;
			outMaxSize[2] = 62.1931;
			return true;

		case AVH_USER3_ARMSLAB:
			outMinSize[0] = -30;
			outMinSize[1] = -20;
			outMinSize[2] = 0;
			outMaxSize[0] = 30;
			outMaxSize[1] = 20;
			outMaxSize[2] = 60;
			return true;

		case AVH_USER3_PROTOTYPE_LAB:
			outMinSize[0] = -16;
			outMinSize[1] = -16;
			outMinSize[2] = 0;
			outMaxSize[0] = 16;
			outMaxSize[1] = 16;
			outMaxSize[2] = 67.7443;
			return true;

		case AVH_USER3_OBSERVATORY:
			outMinSize[0] = -16;
			outMinSize[1] = -16;
			outMinSize[2] = 0;
			outMaxSize[0] = 16;
			outMaxSize[1] = 16;
			outMaxSize[2] = 80.7443;
			return true;

		case AVH_USER3_TURRET_FACTORY:
		case AVH_USER3_ADVANCED_TURRET_FACTORY:
			outMinSize[0] = -40;
			outMinSize[1] = -25;
			outMinSize[2] = 0;
			outMaxSize[0] = 40;
			outMaxSize[1] = 25;
			outMaxSize[2] = 50;
			return true;

		case AVH_USER3_TURRET:
			outMinSize[0] = -16;
			outMinSize[1] = -16;
			outMinSize[2] = 0;
			outMaxSize[0] = 16;
			outMaxSize[1] = 16;
			outMaxSize[2] = 42;
			return true;

		case AVH_USER3_SIEGETURRET:
			outMinSize[0] = -16;
			outMinSize[1] = -16;
			outMinSize[2] = 0;
			outMaxSize[0] = 16;
			outMaxSize[1] = 16;
			outMaxSize[2] = 62.1931;
			return true;
		}

		return false;
	}

	const std::vector<AIMovementStructureCollision>& AIGetMovementStructureCollisions(const bool bForceRefresh = false)
	{
		static float LastCacheTime = -1.0f;
		static std::vector<AIMovementStructureCollision> StructureCollisions;

		if (!bForceRefresh && LastCacheTime == gpGlobals->time) { return StructureCollisions; }

		LastCacheTime = gpGlobals->time;
		StructureCollisions.clear();

		if (StructureCollisions.capacity() == 0) { StructureCollisions.reserve(64); }

		for (int i = 1; i < gpGlobals->maxEntities; ++i)
		{
			edict_t* StructureEdict = INDEXENT(i);

			if (!UTIL_IsEdictActive(StructureEdict) || StructureEdict->v.solid == SOLID_NOT) { continue; }

			vec3_t StructureMins;
			vec3_t StructureMaxs;

			if (!NS_GetCollisionSizeForUser3((AvHUser3)StructureEdict->v.iuser3, StructureMins, StructureMaxs)) { continue; }

			AIMovementStructureCollision StructureCollision;
			StructureCollision.Edict = StructureEdict;
			StructureCollision.Mins = Vector(StructureMins);
			StructureCollision.Maxs = Vector(StructureMaxs);

			UTIL_AnglesToVector(StructureEdict->v.angles, &StructureCollision.Axis[0], &StructureCollision.Axis[1], &StructureCollision.Axis[2]);
			StructureCollision.Axis[1] = StructureCollision.Axis[1] * -1.0f;

			StructureCollisions.push_back(StructureCollision);
		}

		return StructureCollisions;
	}

	bool AISegmentIntersectsAABB(const Vector Start, const Vector End, const Vector Mins, const Vector Maxs, float* ExitFraction = nullptr)
	{
		const Vector Direction = End - Start;
		float MinFraction = 0.0f;
		float MaxFraction = 1.0f;

		for (int i = 0; i < 3; ++i)
		{
			if (fabsf(Direction[i]) < 0.0001f)
			{
				if (Start[i] < Mins[i] || Start[i] > Maxs[i]) { return false; }
				continue;
			}

			float NearFraction = (Mins[i] - Start[i]) / Direction[i];
			float FarFraction = (Maxs[i] - Start[i]) / Direction[i];

			if (NearFraction > FarFraction)
			{
				const float SwapFraction = NearFraction;
				NearFraction = FarFraction;
				FarFraction = SwapFraction;
			}

			MinFraction = max(MinFraction, NearFraction);
			MaxFraction = min(MaxFraction, FarFraction);

			if (MinFraction > MaxFraction) { return false; }
		}

		if (ExitFraction) { *ExitFraction = MaxFraction; }

		return true;
	}

	bool AITraceHitsMovementStructure(const Vector StartTrace, const Vector EndTrace, const enum_hull Hull, const edict_t* IgnoreEdict, const edict_t* OnlyEdict = nullptr, bool* bFoundOnlyEdict = nullptr, const bool bForceRefresh = false)
	{
		// Match NS_HullForBox: movement uses a cylindrical player radius, reduced
		// to 80%, rather than the engine's rectangular head-hull extents.
		const float HullOffset = (Hull == point_hull) ? 0.0f : 12.8f;
		if (bFoundOnlyEdict) { *bFoundOnlyEdict = false; }

		for (const AIMovementStructureCollision& StructureCollision : AIGetMovementStructureCollisions(bForceRefresh))
		{
			if (StructureCollision.Edict == IgnoreEdict || !UTIL_IsEdictActive(StructureCollision.Edict) || StructureCollision.Edict->v.solid == SOLID_NOT) { continue; }

			// Don't check for collisions if we're on top of this structure! Hopefully stops the bot bouncing on top of it
			if (UTIL_IsEdictActive(IgnoreEdict) && StructureCollision.Edict == IgnoreEdict->v.groundentity) { continue; }

			if (OnlyEdict && StructureCollision.Edict != OnlyEdict) { continue; }
			if (bFoundOnlyEdict) { *bFoundOnlyEdict = true; }

			const Vector StartOffset = StartTrace - StructureCollision.Edict->v.origin;
			const Vector EndOffset = EndTrace - StructureCollision.Edict->v.origin;

			Vector LocalStart;
			Vector LocalEnd;
			Vector ExpandedMins = StructureCollision.Mins;
			Vector ExpandedMaxs = StructureCollision.Maxs;

			for (int i = 0; i < 3; ++i)
			{
				const Vector& Axis = StructureCollision.Axis[i];

				LocalStart[i] = UTIL_GetDotProduct(StartOffset, Axis);
				LocalEnd[i] = UTIL_GetDotProduct(EndOffset, Axis);
				ExpandedMins[i] -= HullOffset;
				ExpandedMaxs[i] += HullOffset;
			}

			if (AISegmentIntersectsAABB(LocalStart, LocalEnd, ExpandedMins, ExpandedMaxs))
			{
				if (vPointOverlaps3D(LocalStart, ExpandedMins, ExpandedMaxs))
				{
					const Vector StructureCentre = StructureCollision.Edict->v.origin + ((StructureCollision.Edict->v.mins + StructureCollision.Edict->v.maxs) * 0.5f);

					if (UTIL_GetDotProduct(EndTrace - StartTrace, StartTrace - StructureCentre) >= 0.0f) { continue; }
				}

				return true;
			}
		}

		return false;
	}

	bool AITraceMovementSegment(const Vector StartTrace, const Vector EndTrace, const enum_hull Hull, edict_t* IgnoreEdict, TraceResult& HitResult, bool& bHitMovementStructure)
	{
		bHitMovementStructure = AITraceHitsMovementStructure(StartTrace, EndTrace, Hull, IgnoreEdict);
		if (bHitMovementStructure)
		{
			memset(&HitResult, 0, sizeof(HitResult));
			return true;
		}

		const Vector HullExtents = (Hull == point_hull) ? ZERO_VECTOR : Vector(16.0f, 16.0f, 18.0f);
		Vector EngineTraceStart = StartTrace;

		for (int SkippedCustomBoxes = 0; SkippedCustomBoxes < 8; ++SkippedCustomBoxes)
		{
			UTIL_TraceHull(EngineTraceStart, EndTrace, ignore_monsters, Hull, IgnoreEdict, &HitResult);

			if (HitResult.flFraction >= 1.0f && !HitResult.fStartSolid && !HitResult.fAllSolid) { return false; }

			if (!HitResult.pHit) { return true; }

			vec3_t CustomMins;
			vec3_t CustomMaxs;

			if (!NS_GetCollisionSizeForUser3((AvHUser3)HitResult.pHit->v.iuser3, CustomMins, CustomMaxs)) { return true; }

			bool bFoundCustomEdict = false;

			if (AITraceHitsMovementStructure(EngineTraceStart, EndTrace, Hull, IgnoreEdict, HitResult.pHit, &bFoundCustomEdict)) { return true; }

			if (!bFoundCustomEdict && AITraceHitsMovementStructure(EngineTraceStart, EndTrace, Hull, IgnoreEdict, HitResult.pHit, &bFoundCustomEdict, true)) { return true; }

			const Vector ExpandedMins = HitResult.pHit->v.absmin - HullExtents;
			const Vector ExpandedMaxs = HitResult.pHit->v.absmax + HullExtents;
			float ExitFraction = 1.0f;

			if (!AISegmentIntersectsAABB(EngineTraceStart, EndTrace, ExpandedMins, ExpandedMaxs, &ExitFraction) || ExitFraction >= 1.0f) { break; }

			EngineTraceStart = EngineTraceStart + ((EndTrace - EngineTraceStart) * min(ExitFraction + 0.001f, 1.0f));
		}

		// A rejected server bbox can mask world geometry. Check the world separately
		// if there was no remaining segment beyond that bbox.
		UTIL_TraceHull(StartTrace, EndTrace, ignore_monsters, Hull, IgnoreEdict, &HitResult);

		return HitResult.flFraction < 1.0f || HitResult.fStartSolid || HitResult.fAllSolid;
	}
}


bool AvHAIPlayer::HasValidPath() const
{
	return BotNavInfo.MovementTasks.size() > 0;
}

bool AvHAIPlayer::IsOnGround() const
{
	return (!FNullEnt(Edict) && (Edict->v.flags & FL_ONGROUND));
}

bool AvHAIPlayer::IsOnLadder() const
{
	if (IsPlayerSkulk(Edict))
	{
		return IsPlayerOnLadder(Edict);
	}

	return (!FNullEnt(Edict) && Edict->v.movetype == MOVETYPE_FLY);
}

bool AvHAIPlayer::IsWallSticking() const
{
	return (!FNullEnt(Edict) && IsPlayerSkulk(Edict) && (Edict->v.iuser4 & MASK_WALLSTICKING));
}

bool AvHAIPlayer::CanCrouch() const
{
	if (!IsValid()) { return false; }

	switch (Edict->v.iuser3)
	{
		case AVH_USER3_ALIEN_PLAYER1:
		case AVH_USER3_ALIEN_PLAYER2:
		case AVH_USER3_ALIEN_PLAYER3:
			return false;
		default:
			return true;
	}
}

enum_hull AvHAIPlayer::GetPlayerHull(bool bIsCrouching) const
{
	if (!IsValid()) { return point_hull; }

	AvHUser3 PlayerClass = (AvHUser3)Edict->v.iuser3;

	switch (PlayerClass)
	{
		case AVH_USER3_MARINE_PLAYER: // Regular/heavy marine
		case AVH_USER3_ALIEN_PLAYER4: // Fade
			return (bIsCrouching) ? head_hull : human_hull;
		case AVH_USER3_COMMANDER_PLAYER:
			return head_hull;
		case AVH_USER3_ALIEN_EMBRYO: // Gestating
			return head_hull;
		case AVH_USER3_ALIEN_PLAYER1: // Skulk
		case AVH_USER3_ALIEN_PLAYER2: // Gorge
		case AVH_USER3_ALIEN_PLAYER3:// Lerk
			return head_hull;
		case AVH_USER3_ALIEN_PLAYER5: // Onos
			return (bIsCrouching) ? human_hull : large_hull;
		default:
			return head_hull;
	}
}

float AvHAIPlayer::GetPlayerRadius() const
{
	if (!IsValid()) { return 0.0f; }

	enum_hull PlayerHull = GetPlayerHull(IsCrouching());

	switch (PlayerHull)
	{
		case human_hull:
		case head_hull:
			return 16.0f;
		case large_hull:
			return 32.0f;
		default:
			return 16.0f;
	}
}

float AvHAIPlayer::GetPlayerHeight() const
{
	if (!IsValid()) { return 0.0f; }

	enum_hull PlayerHull = GetPlayerHull(IsCrouching());

	switch (PlayerHull)
	{
		case head_hull:
			return 36.0f;
		case human_hull:
			return 72.0f;
		case large_hull:
			return 108.0f;
		default:
			return 72.0f;
	}
}

Vector AvHAIPlayer::GetEyePosition() const
{
	if (FNullEnt(Edict)) { return ZERO_VECTOR; }

	return (Edict->v.origin + Edict->v.view_ofs);
}

void AvHAIPlayer::AddMovementTask(AvHAIMoveTask& NewTask)
{
	// Movement tasks are handled last-in-first-out, therefore we clear paths on other tasks because this new task
	// will likely take us away from our previous position, so it will ensure a fresh start on older tasks
	for (auto TaskIt = BotNavInfo.MovementTasks.begin(); TaskIt != BotNavInfo.MovementTasks.end(); TaskIt++)
	{
		TaskIt->ClearPath();
	}

	BotNavInfo.MovementTasks.push_back(NewTask);
}

EAINavMoveResult AvHAIPlayer::MoveTo(const Vector& DesiredLocation)
{
	// Already there
	if (vPointOverlaps3D(DesiredLocation, Edict->v.absmin, Edict->v.absmax))
	{
		return EAINavMoveResult::NAV_MOVE_SUCCESS;
	}

	// If the destination is close enough to our core movement task, then we are continuing with our existing movement tasks
	if (BotNavInfo.MovementTasks.size() > 0)
	{
		AvHAIMoveTask& CoreTask = BotNavInfo.MovementTasks.at(0);

		if (vEquals(CoreTask.TaskLocation, DesiredLocation, 18.0f))
		{
			return ProgressMovementTasks();
		}
	}

	const NavAgentProfile* NavProfile = GetNavProfile();

	// This is a brand new destination
	BotNavInfo.ClearPath();

	AvHAIMoveTask NewDestinationTask;

	const bool bAddedCoreMovementTask = AINAV_AddMoveMovementTask(NavProfile, GetBottomOfHitbox(), DesiredLocation, nullptr, NewDestinationTask);

	if (bAddedCoreMovementTask)
	{
		BotNavInfo.MovementTasks.push_back(NewDestinationTask);
		return EAINavMoveResult::NAV_MOVE_SUCCESS;
	}

	AvHAIPath TestPath;
	const bool bFoundPath = AINAV_FindPathClosestToPoint(NavProfile, DesiredLocation, GetBottomOfHitbox(), &TestPath, 200.0f);

	if (bFoundPath)
	{
		NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(TestPath.GetFinalDestination() - GetBottomOfHitbox());
	}

	return (bFoundPath) ? EAINavMoveResult::NAV_MOVE_OFFPATH : EAINavMoveResult::NAV_MOVE_NOPATH;
}

EAINavMoveResult AvHAIPlayer::MoveToWithoutNav(const Vector& DesiredLocation)
{
	const Vector CurrentFloorPosition = UTIL_GetFloorUnderEntity(Edict);

	Vector CurrentPos = (IsOnGround()) ? GetLocation() : CurrentFloorPosition + GetPlayerOriginOffsetFromFloor(Edict, false);
	CurrentPos.z += 18.0f;

	const Vector vForward = UTIL_GetVectorNormal2D(DesiredLocation - CurrentPos);
	// Same goes for the right vector, might not be the same as the bot's right
	const Vector vRight = UTIL_GetVectorNormal2D(UTIL_GetCrossProduct(vForward, UP_VECTOR));

	const float PlayerRadius = GetPlayerRadius();

	Vector stTrcLft = CurrentPos - (vRight * PlayerRadius);
	Vector stTrcRt = CurrentPos + (vRight * PlayerRadius);
	Vector endTrcLft = stTrcLft + (vForward * (PlayerRadius * 1.5f));
	Vector endTrcRt = stTrcRt + (vForward * (PlayerRadius * 1.5f));

	TraceResult hit;

	UTIL_TraceHull(stTrcLft, endTrcLft, ignore_monsters, head_hull, Edict->v.pContainingEntity, &hit);

	const bool bumpLeft = (hit.flFraction < 1.0f || hit.fAllSolid > 0 || hit.fStartSolid > 0);

	UTIL_TraceHull(stTrcRt, endTrcRt, ignore_monsters, head_hull, Edict->v.pContainingEntity, &hit);

	const bool bumpRight = (hit.flFraction < 1.0f || hit.fAllSolid > 0 || hit.fStartSolid > 0);

	NextFrameMovementInput.DesiredMoveDirection = vForward;

	if (bumpRight && !bumpLeft)
	{
		NextFrameMovementInput.DesiredMoveDirection = NextFrameMovementInput.DesiredMoveDirection - vRight;
	}
	else if (bumpLeft && !bumpRight)
	{
		NextFrameMovementInput.DesiredMoveDirection = NextFrameMovementInput.DesiredMoveDirection + vRight;
	}
	else if (bumpLeft && bumpRight)
	{
		float MaxScaleHeight = GetPlayerMaxJumpHeight(Edict);

		float JumpHeight = 0.0f;

		bool bFoundJumpHeight = false;

		Vector StartTrace = CurrentFloorPosition;
		Vector EndTrace = StartTrace + (vForward * 50.0f);
		EndTrace.z = StartTrace.z;

		TraceResult JumpTestHit;

		while (JumpHeight < MaxScaleHeight && !bFoundJumpHeight)
		{
			UTIL_TraceHull(StartTrace, EndTrace, ignore_monsters, head_hull, Edict->v.pContainingEntity, &JumpTestHit);

			if (JumpTestHit.flFraction >= 1.0f && !JumpTestHit.fAllSolid)
			{
				bFoundJumpHeight = true;
				break;
			}

			JumpHeight += 5.0f;

			StartTrace.z += 5.0f;
			EndTrace.z += 5.0f;
		}

		if (JumpHeight <= MaxScaleHeight)
		{
			Jump(true);
		}
		else
		{
			stTrcLft.z = Edict->v.origin.z;
			stTrcRt.z = Edict->v.origin.z;
			endTrcLft.z = Edict->v.origin.z;
			endTrcRt.z = Edict->v.origin.z;

			if (!UTIL_QuickTrace(Edict, stTrcLft, endTrcLft))
			{
				NextFrameMovementInput.DesiredMoveDirection = NextFrameMovementInput.DesiredMoveDirection + vRight;
			}
			else
			{
				NextFrameMovementInput.DesiredMoveDirection = NextFrameMovementInput.DesiredMoveDirection - vRight;
			}
		}
	}

	const float DistFromDestination = vDist2DSq(Edict->v.origin, DesiredLocation);

	return EAINavMoveResult::NAV_MOVE_SUCCESS;
}

void AvHAIPlayer::InterruptReload()
{
	if (IsPlayerReloading(Player))
	{
		NextFrameMovementInput.Button |= IN_ATTACK;
	}
}

AvHAIMoveTask* AvHAIPlayer::GetCurrentMoveTask()
{
	if (BotNavInfo.MovementTasks.size() == 0)
	{
		return nullptr;
	}

	return &BotNavInfo.MovementTasks.at(BotNavInfo.MovementTasks.size() - 1);
}

EAINavMoveResult AvHAIPlayer::ProgressMovementTasks()
{
	AvHAIMoveTask* CurrentMoveTask = GetCurrentMoveTask();

	if (!CurrentMoveTask || !CurrentMoveTask->IsValid())
	{
		return EAINavMoveResult::NAV_MOVE_INVALIDTASK;
	}

	EAINavMoveResult MoveResult = EAINavMoveResult::NAV_MOVE_SUCCESS;

	switch (CurrentMoveTask->TaskType)
	{
		case EAIMovementTaskType::MOVE_TASK_MOVE:
			MoveResult = ProgressMoveTask(CurrentMoveTask);
			break;
		case EAIMovementTaskType::MOVE_TASK_BREAK:
			MoveResult = ProgressBreakTask(CurrentMoveTask);
			break;
		case EAIMovementTaskType::MOVE_TASK_USE:
			MoveResult = ProgressUseTask(CurrentMoveTask);
			break;
		case EAIMovementTaskType::MOVE_TASK_WELD:
			MoveResult = ProgressWeldTask(CurrentMoveTask);
			break;
		default:
			MoveResult = EAINavMoveResult::NAV_MOVE_NOPATH;
			break;
	}

	if (IsMovementTaskComplete(CurrentMoveTask))
	{
		RemoveTask(CurrentMoveTask);
	}

	return MoveResult;
}

bool AvHAIPlayer::IsMovementTaskComplete(const AvHAIMoveTask* Task)
{
	if (!Task || !Task->IsValid() || Task->bTaskComplete) { return true; }

	switch (Task->TaskType)
	{
		case EAIMovementTaskType::MOVE_TASK_MOVE:
			return IsMoveMovementTaskComplete(Task);
		case EAIMovementTaskType::MOVE_TASK_WELD:
			return IsWeldMovementTaskComplete(Task);
		case EAIMovementTaskType::MOVE_TASK_USE:
			return IsUseMovementTaskComplete(Task);
		case EAIMovementTaskType::MOVE_TASK_PICKUP:
			return IsPickupMovementTaskComplete(Task);
		case EAIMovementTaskType::MOVE_TASK_TOUCH:
			return IsTouchMovementTaskComplete(Task);
		case EAIMovementTaskType::MOVE_TASK_BREAK:
			return IsBreakMovementTaskComplete(Task);
		default:
			return true;
	}
}

bool AvHAIPlayer::IsMoveMovementTaskComplete(const AvHAIMoveTask* Task)
{
	if (!Task || !Task->IsValid()) { return true; }

	return vPointOverlaps3D(Task->TaskLocation, Edict->v.absmin, Edict->v.absmax);
}

bool AvHAIPlayer::IsPickupMovementTaskComplete(const AvHAIMoveTask* Task)
{
	if (!Task || !Task->IsValid()) { return true; }

	return !UTIL_IsEdictActive(Task->TaskTarget);
}

bool AvHAIPlayer::IsTouchMovementTaskComplete(const AvHAIMoveTask* Task)
{
	if (!Task || !Task->IsValid()) { return true; }

	if (!UTIL_IsEdictActive(Task->TaskTarget)) { return true; }

	if (Task->RelatedPathData.IsValidMove())
	{
		if (Task->RelatedPathData.MovementFlag == EAINavMovementFlag::NAV_FLAG_PLATFORM)
		{
			const DynamicMapObject* PlatformRef = AIMAP_GetDynamicObjectByEdict(Task->TriggerToActivate);

			if (PlatformRef)
			{
				if (!AIMAP_PlatformNeedsActivating(Task->NavProfile, PlatformRef, Task->RelatedPathData.FromLocation, Task->RelatedPathData.ToLocation))
				{
					return true;
				}
			}
		}
	}

	return vBBOverlaps(Edict->v.absmin, Edict->v.absmax, Task->TaskTarget->v.absmin, Task->TaskTarget->v.absmax);
}

bool AvHAIPlayer::IsUseMovementTaskComplete(const AvHAIMoveTask* Task)
{
	if (!Task || !Task->IsValid()) { return true; }

	if (!UTIL_IsEdictActive(Task->TaskTarget)) { return true; }

	if (Task->RelatedPathData.IsValidMove())
	{
		if (Task->RelatedPathData.MovementFlag == EAINavMovementFlag::NAV_FLAG_PLATFORM)
		{
			const DynamicMapObject* PlatformRef = AIMAP_GetDynamicObjectByEdict(Task->TriggerToActivate);

			if (PlatformRef)
			{
				return !AIMAP_PlatformNeedsActivating(Task->NavProfile, PlatformRef, Task->RelatedPathData.FromLocation, Task->RelatedPathData.ToLocation);
			}
		}
	}

	const DynamicMapObject* TriggerObject = AIMAP_GetDynamicObjectByEdict(Task->TaskTarget);

	if (TriggerObject && TriggerObject->State != EAIDynamicMapObjectState::OBJECTSTATE_IDLE)
	{
		return true;
	}

	const DynamicMapObject* TriggerTargetObject = AIMAP_GetDynamicObjectByEdict(Task->TriggerToActivate);

	if (TriggerTargetObject)
	{
		if (TriggerTargetObject->State != EAIDynamicMapObjectState::OBJECTSTATE_IDLE) { return true; }
	}

	return Task->TaskTarget->v.nextthink > 0.0f;
}

bool AvHAIPlayer::IsBreakMovementTaskComplete(const AvHAIMoveTask* Task)
{
	if (!Task || !Task->IsValid()) { return true; }

	if (Task->RelatedPathData.IsValidMove())
	{
		if (Task->RelatedPathData.MovementFlag == EAINavMovementFlag::NAV_FLAG_PLATFORM)
		{
			const DynamicMapObject* PlatformRef = AIMAP_GetDynamicObjectByEdict(Task->TriggerToActivate);

			if (PlatformRef)
			{
				if (!AIMAP_PlatformNeedsActivating(Task->NavProfile, PlatformRef, Task->RelatedPathData.FromLocation, Task->RelatedPathData.ToLocation))
				{
					return true;
				}
			}
		}
	}

	return !UTIL_IsEdictActive(Task->TaskTarget);
}

bool AvHAIPlayer::IsWeldMovementTaskComplete(const AvHAIMoveTask* Task)
{
	if (!Task || !Task->IsValid()) { return true; }

	if (!UTIL_IsEdictActive(Task->TaskTarget)) { return true; }

	if (Task->RelatedPathData.IsValidMove())
	{
		if (Task->RelatedPathData.MovementFlag == EAINavMovementFlag::NAV_FLAG_PLATFORM)
		{
			const DynamicMapObject* PlatformRef = AIMAP_GetDynamicObjectByEdict(Task->TriggerToActivate);

			if (PlatformRef)
			{
				if (!AIMAP_PlatformNeedsActivating(Task->NavProfile, PlatformRef, Task->RelatedPathData.FromLocation, Task->RelatedPathData.ToLocation))
				{
					return true;
				}
			}
		}
	}

	AvHWeldable* WeldableRef = dynamic_cast<AvHWeldable*>(CBaseEntity::Instance(ENTINDEX(Task->TaskTarget)));

	return (!WeldableRef || WeldableRef->GetIsWelded());
}

void AvHAIPlayer::Jump(bool bDuckJump)
{
	if (IsOnGround() || IsOnLadder())
	{
		if (gpGlobals->time - BotNavInfo.LandedTime >= 0.1f)
		{
			NextFrameMovementInput.Button |= IN_JUMP;
			NextFrameMovementInput.bHasAttemptedJump = true;
		}
	}
	else
	{
		if (bDuckJump)
		{
			NextFrameMovementInput.Button |= IN_DUCK;
		}
	}
}

void AvHAIPlayer::Suicide()
{
	if (!bIsPendingKill && !IsDead())
	{
		Player->Suicide();
		bIsPendingKill = true;
	}
}

bool AvHAIPlayer::IsDead() const
{
	return (Edict->v.deadflag != DEAD_NO || Edict->v.health <= 0.0f);
}

float AvHAIPlayer::GetDesiredMovementSpeed(bool bShouldWalk) const
{
	float MaxSpeed = fminf(CVAR_GET_FLOAT("cl_forwardspeed"), CVAR_GET_FLOAT("sv_maxspeed"));
	return (NextFrameMovementInput.bShouldWalk) ? MaxSpeed * 0.5f : MaxSpeed;
}

Vector AvHAIPlayer::GetBottomOfHitbox() const
{
	if (FNullEnt(Edict)) { return ZERO_VECTOR; }

	return Vector(Edict->v.origin.x, Edict->v.origin.y, Edict->v.absmin.z);
}

Vector AvHAIPlayer::GetTopOfHitbox() const
{
	if (FNullEnt(Edict)) { return ZERO_VECTOR; }

	return Vector(Edict->v.origin.x, Edict->v.origin.y, Edict->v.absmax.z);
}

void AvHAIPlayer::Think(float DeltaTime)
{
	NextFrameMovementInput.Clear();

	if (ShouldThink())
	{
		UpdateNavProfile();

		if (!vIsZero(DebugDestination))
		{
			MoveTo(DebugDestination);
			DEBUG_DrawPath();
		}
	}
}

void AvHAIPlayer::CheckAndSendMessages()
{
	AvHAIPendingMessageList::iterator MessageToDeliver = PendingMessages.end();
	float OldestMessage = -1.0f;

	for (auto MsgIt = PendingMessages.begin(); MsgIt != PendingMessages.end(); MsgIt++)
	{
		const AvHAIBotMsg* ThisPendingMessage = &(*MsgIt);

		if (ThisPendingMessage->SendTime > gpGlobals->time) { continue; }

		const float MessageAge = gpGlobals->time - ThisPendingMessage->SendTime;

		if (MessageAge > OldestMessage)
		{
			MessageToDeliver = MsgIt;
			OldestMessage = MessageAge;
		}
	}

	if (MessageToDeliver != PendingMessages.end())
	{
		AIPlayer_Say(Edict, MessageToDeliver->bIsTeamSay, MessageToDeliver->Message);
		PendingMessages.erase(MessageToDeliver);
	}
}

void AvHAIPlayer::StartThink(float DeltaTime)
{
	CheckAndSendMessages();
}

void AvHAIPlayer::UpdateView(float DeltaTime)
{
	BotUpdateDesiredViewRotation();
	InterpolateView(DeltaTime);
	UpdateViewFrustum();
}

void AvHAIPlayer::EndThink(float DeltaTime)
{
	NextFrameMovementInput.GenerateMovementOutputs(Edict->v.v_angle, Edict->v.maxspeed);

	const EAIWeaponId NewSwitchWeapon = (NextFrameMovementInput.RequiredWeapon != EAIWeaponId::WEAPON_INVALID)
		? NextFrameMovementInput.RequiredWeapon
		: NextFrameMovementInput.DesiredWeapon;

	if (NewSwitchWeapon != EAIWeaponId::WEAPON_INVALID && NewSwitchWeapon != GetCurrentWeapon())
	{
		SwitchToWeapon(NewSwitchWeapon);
	}

	UpdateView(DeltaTime);

	// Thanks to The Storm (ePODBot) for this one, finally fixed the bot running speed!
	int AdjustedTimeMS = (int)roundf((DeltaTime) * 1000.0f);

	if (AdjustedTimeMS > 255)
	{
		AdjustedTimeMS = 255;
	}

	// Simulate PM_PlayerMove so client prediction and stuff can be executed correctly.
	RUN_AI_MOVE(Edict, Edict->v.v_angle, NextFrameMovementInput.ForwardMove,
		NextFrameMovementInput.SideMove, NextFrameMovementInput.UpMove, NextFrameMovementInput.Button, NextFrameMovementInput.Impulse, (byte)AdjustedTimeMS);

	NextFrameMovementInput.Button = 0;
	NextFrameMovementInput.Impulse = 0;
	LastServerUpdateTime = gpGlobals->time;
}

void AvHAIPlayer::BotUpdateDesiredViewRotation()
{
	// If we are in the process of interpolating towards a current view target, don't interrupt and let it finish
	if (!vIsZero(ViewInfo.InterpolatingViewTarget)) { return; }

	const bool bIsRequiredView = !vIsZero(NextFrameMovementInput.RequiredLookLocation);

	const Vector NewDesiredTargetView = (bIsRequiredView)
		? NextFrameMovementInput.RequiredLookLocation
		: (!vIsZero(ViewInfo.LookTargetLocation)) ? ViewInfo.LookTargetLocation : NextFrameMovementInput.DesiredLookLocation;

	if (vIsZero(NewDesiredTargetView)) { return; }

	ViewInfo.ViewInterpStartedTime = gpGlobals->time;

	const Vector DesiredViewForwardVector = UTIL_GetVectorNormal(NewDesiredTargetView - GetEyePosition());

	ViewInfo.InterpolatingViewTarget = UTIL_VecToAngles(DesiredViewForwardVector);

	if (!vIsValid(ViewInfo.InterpolatingViewTarget))
	{
		ViewInfo.InterpolatingViewTarget = ZERO_VECTOR;
	}

	vClampViewAngles(ViewInfo.InterpolatingViewTarget);

	if (ViewInfo.bSnapView)
	{
		ViewInfo.ViewInterpolationSpeed = 1000.0f;
		return;
	}

	Vector ViewDelta = ViewInfo.InterpolatingViewTarget - ViewInfo.CurrentInterpolatedView;

	vClampViewAngles(ViewDelta);

	float MaxViewDelta = fmaxf(fabsf(ViewDelta.y), fabsf(ViewDelta.x));

	float motion_tracking_skill = (IsPlayerMarine(Edict)) ? BotSkillSettings.marine_bot_motion_tracking_skill : BotSkillSettings.alien_bot_motion_tracking_skill;
	float bot_view_speed = (IsPlayerMarine(Edict)) ? BotSkillSettings.marine_bot_view_speed : BotSkillSettings.alien_bot_view_speed;
	float bot_aim_skill = (IsPlayerMarine(Edict)) ? BotSkillSettings.marine_bot_aim_skill : BotSkillSettings.alien_bot_aim_skill;

	ViewInfo.ViewInterpolationSpeed = (MaxViewDelta >= 45.0f)
		? 350.0f
		: (MaxViewDelta >= 25.0f) ? 175.0f
			: (MaxViewDelta >= 5.0f) ? 50.0f : 35.0f;

	ViewInfo.ViewInterpolationSpeed *= bot_view_speed;

	// If this is not a look direction needed for movement, we can add some random offsets to make the bot's aim less perfect and more human-like
	// We don't do this for movement-required look directions because it could cause the bot to miss jumps, wall-climbs, ladder climbs etc.
	if (!bIsRequiredView)
	{
		const float AimOffset = (MaxViewDelta >= 45.0f)
			? frandrange(10.0f, 20.0f)
			: (MaxViewDelta >= 25.0f)
				? frandrange(5.0f, 10.0f)
				: (MaxViewDelta >= 5.0f) ? frandrange(2.0f, 5.0f) : 0.0f;

		const float xOffset = AimOffset * (randbool()) ? -1.0f : 1.0f;
		const float yOffset = AimOffset * (randbool()) ? -1.0f : 1.0f;

		ViewInfo.InterpolatingViewTarget.x += xOffset;
		ViewInfo.InterpolatingViewTarget.y += yOffset;
	}

	// We once again clamp everything to valid values in case the offsets we applied above took us above that

	vClampViewAngles(ViewInfo.InterpolatingViewTarget);
}

void AvHAIPlayer::InterpolateView(float DeltaTime)
{
	Vector ViewDelta = ViewInfo.InterpolatingViewTarget - ViewInfo.CurrentInterpolatedView;

	vClampViewAngles(ViewDelta);

	ViewInfo.CurrentInterpolatedView.x = fInterpConstantTo(ViewInfo.CurrentInterpolatedView.x, ViewInfo.InterpolatingViewTarget.x, DeltaTime, ViewInfo.ViewInterpolationSpeed);
	ViewInfo.CurrentInterpolatedView.y += fInterpConstantTo(0.0f, ViewDelta.y, DeltaTime, ViewInfo.ViewInterpolationSpeed);

	vClampViewAngles(ViewInfo.CurrentInterpolatedView);

	if (vEquals2D(ViewInfo.CurrentInterpolatedView, ViewInfo.InterpolatingViewTarget) || gpGlobals->time - ViewInfo.ViewInterpStartedTime > 2.0f)
	{
		ViewInfo.InterpolatingViewTarget = ZERO_VECTOR;
	}

	Edict->v.v_angle.x = ViewInfo.CurrentInterpolatedView.x;
	Edict->v.v_angle.y = ViewInfo.CurrentInterpolatedView.y;

	// set the body angles to point the gun correctly
	Edict->v.angles.x = Edict->v.v_angle.x / 3;
	Edict->v.angles.y = Edict->v.v_angle.y;
	Edict->v.angles.z = 0;

	// adjust the view angle pitch to aim correctly (MUST be after body v.angles stuff)
	Edict->v.v_angle.x = -Edict->v.v_angle.x;
	// Paulo-La-Frite - END

	Edict->v.ideal_yaw = Edict->v.v_angle.y;

	if (Edict->v.ideal_yaw > 180)
		Edict->v.ideal_yaw -= 360;

	if (Edict->v.ideal_yaw < -180)
		Edict->v.ideal_yaw += 360;
}

void AvHAIPlayer::LookAt(const Vector& LocationTarget)
{
	ViewInfo.LookTargetLocation = LocationTarget;
}

void AvHAIPlayer::LookAt(const edict_t* Target)
{
	if (FNullEnt(Target)) { return; }

	ViewInfo.LookTargetLocation = Target->v.origin;
}

void AvHAIPlayer::UpdateViewFrustum()
{
	MAKE_VECTORS(Edict->v.v_angle);
	Vector up = gpGlobals->v_up;
	Vector forward = gpGlobals->v_forward;
	Vector right = gpGlobals->v_right;

	Vector fc = (Edict->v.origin + Edict->v.view_ofs) + (forward * BOT_MAX_VIEW);

	Vector fbl = fc + (up * f_ffheight * 0.5f) - (right * f_ffwidth * 0.5f);
	Vector fbr = fc + (up * f_ffheight * 0.5f) + (right * f_ffwidth * 0.5f);
	Vector ftl = fc - (up * f_ffheight * 0.5f) - (right * f_ffwidth * 0.5f);
	Vector ftr = fc - (up * f_ffheight * 0.5f) + (right * f_ffwidth * 0.5f);

	Vector nc = (Edict->v.origin + Edict->v.view_ofs) + (forward * BOT_MIN_VIEW);

	Vector nbl = nc + (up * f_fnheight * 0.5f) - (right * f_fnwidth * 0.5f);
	Vector nbr = nc + (up * f_fnheight * 0.5f) + (right * f_fnwidth * 0.5f);
	Vector ntl = nc - (up * f_fnheight * 0.5f) - (right * f_fnwidth * 0.5f);
	Vector ntr = nc - (up * f_fnheight * 0.5f) + (right * f_fnwidth * 0.5f);

	UTIL_SetFrustumPlane(&ViewInfo.ViewFrustumPlanes[FRUSTUM_PLANE_TOP], ftl, ntl, ntr);
	UTIL_SetFrustumPlane(&ViewInfo.ViewFrustumPlanes[FRUSTUM_PLANE_BOTTOM], fbr, nbr, nbl);
	UTIL_SetFrustumPlane(&ViewInfo.ViewFrustumPlanes[FRUSTUM_PLANE_LEFT], fbl, nbl, ntl);
	UTIL_SetFrustumPlane(&ViewInfo.ViewFrustumPlanes[FRUSTUM_PLANE_RIGHT], ftr, ntr, nbr);
	UTIL_SetFrustumPlane(&ViewInfo.ViewFrustumPlanes[FRUSTUM_PLANE_NEAR], nbr, ntr, ntl);
	UTIL_SetFrustumPlane(&ViewInfo.ViewFrustumPlanes[FRUSTUM_PLANE_FAR], fbl, ftl, ftr);
}

bool AvHAIPlayer::IsObjectInFOV(const edict_t* Object) const
{
	if (!UTIL_IsEdictActive(Object) || (Object->v.effects & EF_NODRAW)) { return false; }

	if (IsEdictPlayer(Object) && !IsPlayerActiveInGame(Object)) { return false; }

	// To make things a little more accurate, we're going to treat players as cylinders rather than boxes
	for (int i = 0; i < 6; i++)
	{
		// Our cylinder must be inside all planes to be visible, otherwise return false
		if (!UTIL_CylinderInsidePlane(&ViewInfo.ViewFrustumPlanes[i], Object->v.origin - Vector(0.0f, 0.0f, 5.0f), 60.0f, 16.0f))
		{
			return false;
		}
	}

	return true;
}

bool AvHAIPlayer::UseObject(edict_t* Object, bool bUseContinuously)
{
	if (FNullEnt(Object)) { return false; }

	CBaseEntity* UsedObject = CBaseEntity::Instance(Object);

	if (!UsedObject) { return false; }

	Vector ClosestPoint = UTIL_GetClosestPointOnEntityToLocation(Edict->v.origin, Object);
	Vector TargetCentre = UTIL_GetCentreOfEntity(Object);

	Vector AimPoint = ClosestPoint;

	const bool bIsUsingStructure = AITAC_IsEdictStructure(Object);

	if (bIsUsingStructure)
	{
		AimPoint = TargetCentre;
		AimPoint.z = ClosestPoint.z;
	}

	NextFrameMovementInput.RequiredLookLocation = AimPoint;

	if (!bUseContinuously && ((gpGlobals->time - LastUseTime) < min_player_use_interval)) { return false; }

	Vector AimDir = UTIL_GetForwardVector2D(Edict->v.v_angle);
	Vector TargetAimDir = UTIL_GetVectorNormal2D(AimPoint - GetEyePosition());

	float AimDot = UTIL_GetDotProduct2D(AimDir, TargetAimDir);

	if (AimDot >= 0.95f)
	{
		LastUseTime = gpGlobals->time;

		if (UsedObject)
		{
			UsedObject->Use(Player, Player, USE_TOGGLE, 0);
		}

		return true;
	}

	return false;
}

EAIWeaponId AvHAIPlayer::GetCurrentWeapon() const
{
	if (!Player) { return EAIWeaponId::WEAPON_INVALID; }

	AvHBasePlayerWeapon* theBasePlayerWeapon = dynamic_cast<AvHBasePlayerWeapon*>(Player->m_pActiveItem);

	if (theBasePlayerWeapon)
	{
		return static_cast<EAIWeaponId>(theBasePlayerWeapon->m_iId);
	}

	return EAIWeaponId::WEAPON_INVALID;
}

void AvHAIPlayer::LeaveCommChair()
{
	if (!IsValid() || !IsPlayerCommander(Edict)) { return; }

	Player->SetUser3(AVH_USER3_MARINE_PLAYER);

	// Cheesy way to make sure player class change is sent to everyone
	Player->EffectivePlayerClassChanged();
}

void AvHAIPlayer::UpdateReceivedOrders()
{
	OrderListType ActiveOrders = Player->GetActiveOrders();

	for (auto it = ActiveOrders.begin(); it != ActiveOrders.end(); it++)
	{
		if (it->GetOrderActive() && it->GetReceiver() && ENTINDEX(Edict) == it->GetReceiver())
		{
			Vector OrderLocation = g_vecZero;
			it->GetLocation(OrderLocation);

			switch (it->GetOrderType())
			{
			case ORDERTYPEL_MOVE:
				OnReceiveMoveOrder(OrderLocation);
				break;
			case ORDERTYPET_BUILD:
				OnReceiveBuildOrder(INDEXENT(it->GetTargetIndex()));
				break;
			default:
				break;
			}
		}
	}
}

void AvHAIPlayer::OnReceiveMoveOrder(const Vector& TargetLocation)
{

}

void AvHAIPlayer::OnReceiveBuildOrder(const edict_t* TargetObject)
{
	if (!UTIL_IsEdictActive(TargetObject)) { return; }
}

void AvHAIPlayer::SwitchToWeapon(EAIWeaponId NewWeaponId)
{
	if (NewWeaponId == EAIWeaponId::WEAPON_INVALID) { return; }

	if (!UTIL_PlayerHasWeapon(Player, NewWeaponId)) { return; }

	const char* WeaponName = UTIL_WeaponTypeToClassname(NewWeaponId);
	Player->SwitchWeapon(WeaponName);
}

void AvHAIPlayer::RequestEvolveUpgrade(EAIAlienUpgrade DesiredUpgrade)
{
	AvHMessageID NewImpulse = UTIL_GetEvolveUpgradeImpulse(DesiredUpgrade);

	if (NewImpulse != MESSAGE_NULL)
	{
		NextFrameMovementInput.Impulse = NewImpulse;
	}
}

void AvHAIPlayer::RequestEvolveLifeform(EAIAlienLifeform DesiredLifeform)
{
	AvHMessageID NewImpulse = UTIL_GetEvolveLifeformImpulse(DesiredLifeform);

	if (NewImpulse != MESSAGE_NULL)
	{
		NextFrameMovementInput.Impulse = NewImpulse;
	}
}

void AvHAIPlayer::Say(const char* ThingToSay, bool bTeamSay, float Delay)
{
	AvHAIBotMsg NewMessage;
	NewMessage.bIsTeamSay = bTeamSay;
	NewMessage.SendTime = gpGlobals->time + Delay;
	std::sprintf(NewMessage.Message, ThingToSay);

	PendingMessages.push_back(NewMessage);
}

void AvHAIPlayer::DropWeapon()
{
	// Look straight ahead so we don't accidentally drop the weapon right at our feet and pick it up again instantly

	Vector AimDir = UTIL_GetForwardVector(Edict->v.v_angle);
	Vector TargetAimDir = Vector(AimDir.x, AimDir.y, 0.0f);

	Vector LookLoc = GetEyePosition() + (TargetAimDir * 100.0f);

	LookAt(LookLoc);

	float AimDot = UTIL_GetDotProduct(AimDir, TargetAimDir);

	if (AimDot >= 0.95f)
	{
		NextFrameMovementInput.Impulse = WEAPON_DROP;
	}
}

void AvHAIPlayer::ReloadWeapon()
{
	const EAIWeaponId CurrentWeapon = GetCurrentWeapon();

	if (CurrentWeapon == EAIWeaponId::WEAPON_INVALID) { return; }

	if (!UTIL_WeaponCanBeReloaded(CurrentWeapon)) { return; }

	if (!IsPlayerReloading(Player))
	{
		if (gpGlobals->time - LastUseTime > 1.0f)
		{
			NextFrameMovementInput.Button |= IN_RELOAD;
			LastUseTime = gpGlobals->time;
		}
	}
}

bool AvHAIPlayer::ShouldThink() const
{
	return AIMESH_IsNavMeshLoaded()
		&& GetGameRules()->GetGameStarted()
		&& !AIMGR_HasMatchEnded()
		&& (IsPlayerActiveInGame(Edict) || IsPlayerCommander(Edict))
		&& !IsPlayerGestating(Edict);
}

void AvHAIPlayer::HearEnemy(const edict_t* EmittingEdict, float Volume)
{

}

void AvHAIPlayer::OnNavMeshModified(EAINavMeshIndex ModifiedMeshIndex)
{
	for (auto MoveTaskIt = BotNavInfo.MovementTasks.begin(); MoveTaskIt != BotNavInfo.MovementTasks.end(); MoveTaskIt++)
	{
		AvHAIMoveTask* ThisTask = &(*MoveTaskIt);

		if (!ThisTask || !ThisTask->HasPath()) { continue; }

		if (ThisTask->TaskPath.UsedNavMesh == ModifiedMeshIndex)
		{
			// This will force a recalculation when the bot next wants to handle this movement task
			ThisTask->TaskPath.Clear();
		}
	}
}

void AvHAIPlayer::TakeDamage(float DamageAmount, const edict_t* Inflictor)
{

}

void AvHAIPlayer::UpdateNavProfile()
{
	if (IsPlayerMarine(Edict))
	{
		BotNavInfo.NavProfile = *GetBaseAgentProfile(EAINavProfileIndex::NAV_PROFILE_MARINE);

		const EAINavMovementFlag PhaseGateFlag = (Player->GetTeam() == GetGameRules()->GetTeamANumber())
			? EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM1
			: EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM2;

		BotNavInfo.NavProfile.Filters.addIncludeFlags(static_cast<unsigned int>(PhaseGateFlag));
		return;
	}

	if (IsPlayerSkulk(Edict))
	{
		BotNavInfo.NavProfile = *GetBaseAgentProfile(EAINavProfileIndex::NAV_PROFILE_SKULK);
		return;
	}
}

EAINavMoveResult AvHAIPlayer::FollowPath(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return EAINavMoveResult::NAV_MOVE_NOPATH; }

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();
	const AvHAIPathNode* NextPathNode = Path->GetNextPathNode();

	if (!CurrentPathNode || !CurrentPathNode->IsValidMove()) { return EAINavMoveResult::NAV_MOVE_NOPATH; }

	const bool bHasNextPathNode = NextPathNode && NextPathNode->IsValidMove();

	if (AINAV_IsPathPointComplete(GetNavProfile(), Edict, CurrentPathNode, NextPathNode))
	{
		// We have reached the end of our path. Job done.
		if (!bHasNextPathNode)
		{
			return EAINavMoveResult::NAV_MOVE_PATH_COMPLETE;
		}

		Path->OnPathNodeComplete();

		CurrentPathNode = Path->GetCurrentPathNode();
		NextPathNode = Path->GetNextPathNode();
	}

	if (IsInWater())
	{
		TraceResult Hit;

		AvHAIMutablePathNodeList FutureNodeList = Path->GetMutableFuturePathNodeList();

		for (AvHAIPathNode* ThisNode : FutureNodeList)
		{
			if (!UTIL_IsPointInSwimArea(ThisNode->ToLocation)) { break; }

			UTIL_TraceHull(GetLocation(), ThisNode->ToLocation, ignore_monsters, head_hull, nullptr, &Hit);

			if (!Hit.fAllSolid && !Hit.fStartSolid && Hit.flFraction >= 1.0f)
			{
				Path->JumpToPathNode(ThisNode);
				ThisNode->FromLocation = GetLocation();
			}
		}

		CurrentPathNode = Path->GetCurrentPathNode();
		NextPathNode = Path->GetNextPathNode();
	}

	if (IsPlayerStandingOnPlayer(Edict) && CurrentPathNode->MovementFlag != EAINavMovementFlag::NAV_FLAG_LADDER)
	{
		if (GetVelocity().Length2D() > 10.0f)
		{
			NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(-Edict->v.groundentity->v.velocity);
			return EAINavMoveResult::NAV_MOVE_AVOID_PLAYER;
		}

		MoveToWithoutNav(CurrentPathNode->ToLocation);
		return EAINavMoveResult::NAV_MOVE_AVOID_PLAYER;
	}

	AvHPlayer* RidingPlayer = AINAV_GetPlayerRidingOnBot(Edict);

	if (RidingPlayer)
	{
		// TODO: Something here
		// return EAINavMoveResult::NAV_MOVE_AVOID_PLAYER;
	}

	if (AINAV_IsOffPathNode(GetNavProfile(), Edict, CurrentPathNode))
	{
		const bool bSucceededRegen = AINAV_FindPathClosestToPoint(GetNavProfile(), UTIL_GetFloorUnderEntity(Edict), Path->GetFinalDestination(), Path, GetPlayerRadius());

		if (!bSucceededRegen)
		{
			return EAINavMoveResult::NAV_MOVE_NOPATH;
		}
	}

	AvHAIMoveTask NewMoveTask;

	if (AINAV_CheckAndAddRequiredMovementTasks(GetNavProfile(), Path, NewMoveTask))
	{
		bool bHasExistingTask = false;

		for (auto TaskIt = BotNavInfo.MovementTasks.begin(); TaskIt != BotNavInfo.MovementTasks.end(); TaskIt++)
		{
			const AvHAIMoveTask* ThisTask = &(*TaskIt);
			if (!ThisTask || !ThisTask->IsValid()) { continue; }

			if (ThisTask->TaskType != NewMoveTask.TaskType) { continue; }

			switch (ThisTask->TaskType)
			{
				case EAIMovementTaskType::MOVE_TASK_WELD:
				case EAIMovementTaskType::MOVE_TASK_BREAK:
				case EAIMovementTaskType::MOVE_TASK_PICKUP:
				case EAIMovementTaskType::MOVE_TASK_USE:
				case EAIMovementTaskType::MOVE_TASK_TOUCH:
					bHasExistingTask = (ThisTask->TaskTarget == NewMoveTask.TaskTarget);
					break;
				case EAIMovementTaskType::MOVE_TASK_MOVE:
					bHasExistingTask = vEquals(ThisTask->TaskLocation, NewMoveTask.TaskLocation, 5.0f);
					break;
			}

			if (bHasExistingTask) { break; }
		}

		if (!bHasExistingTask)
		{
			AddMovementTask(NewMoveTask);
			return EAINavMoveResult::NAV_MOVE_SUCCESS;
		}
	}

	if (IsInWater())
	{
		NextSwimMove(Path);
	}
	else
	{
		NextMove(Path);
	}

	HandlePlayerAvoidance(CurrentPathNode);

	if (vIsZero(NextFrameMovementInput.RequiredLookLocation) && vIsZero(NextFrameMovementInput.DesiredLookLocation))
	{
		Vector FurthestView = AINAV_GetFurthestVisiblePointOnPath(GetEyePosition(), NextFrameMovementInput.DesiredMoveDirection, Path);

		if (vIsZero(FurthestView) || vDist2DSq(FurthestView, GetEyePosition()) < sqrf(200.0f))
		{
			FurthestView = CurrentPathNode->ToLocation;

			Vector LookNormal = UTIL_GetVectorNormal2D(FurthestView - GetEyePosition());

			FurthestView = FurthestView + (LookNormal * 1000.0f);
		}

		NextFrameMovementInput.DesiredLookLocation = FurthestView;
	}

	return EAINavMoveResult::NAV_MOVE_SUCCESS;
}

void AvHAIPlayer::HandlePlayerAvoidance(const AvHAIPathNode* CurrentPathNode)
{
	if (!CurrentPathNode || !CurrentPathNode->IsValidMove()) { return; }

	PlayerListType ActivePlayerList = AIMGR_GetAllActivePlayers();

	if (ActivePlayerList.empty()) { return; }

	switch (CurrentPathNode->MovementFlag)
	{
		case EAINavMovementFlag::NAV_FLAG_LADDER:
			HandleLadderPlayerAvoidance(CurrentPathNode);
			break;
		case EAINavMovementFlag::NAV_FLAG_WALK:
		case EAINavMovementFlag::NAV_FLAG_CROUCH:
			HandleGroundPlayerAvoidance(CurrentPathNode);
			break;
		default:
			break;
	}
}

void AvHAIPlayer::HandleGroundPlayerAvoidance(const AvHAIPathNode* CurrentPathNode)
{
	PlayerListType ActivePlayerList = AIMGR_GetAllActivePlayers();

	if (ActivePlayerList.empty()) { return; }

	const Vector BotLocation = GetLocation();

	for (AvHPlayer* OtherPlayer : ActivePlayerList)
	{
		if (OtherPlayer == Player) { continue; }

		const edict_t* OtherPlayerEdict = OtherPlayer->edict();

		if (FNullEnt(OtherPlayerEdict)) { continue; }

		// Other player is not too close to us, we're good
		if (vDist2DSq(UTIL_GetClosestPointOnEntityToLocation(BotLocation, OtherPlayerEdict), BotLocation) > sqrf(GetPlayerRadius() * 2.0f))
		{
			continue;
		}

		// Player is above or below us
		if (OtherPlayerEdict->v.absmax.z < GetBottomOfHitbox().z || OtherPlayerEdict->v.absmin.z > GetTopOfHitbox().z) { continue; }

		// Other player is further away from our destination than us, probably not in the way
		if (vDist2DSq(OtherPlayerEdict->v.origin, CurrentPathNode->ToLocation) > vDist2DSq(BotLocation, CurrentPathNode->ToLocation))
		{
			continue;
		}

		// Other player is definitely not in our way
		if (!vlineIntersectsAABB(CurrentPathNode->FromLocation, CurrentPathNode->ToLocation, OtherPlayerEdict->v.absmin, OtherPlayerEdict->v.absmax))
		{
			continue;
		}

		const Vector DesiredMoveDirection = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - BotLocation);
		const Vector OtherPlayerMoveDirection = UTIL_GetVectorNormal2D(OtherPlayerEdict->v.velocity);
		const float OtherPlayerMoveDot = UTIL_GetDotProduct2D(OtherPlayerMoveDirection, DesiredMoveDirection);

		// Player is trying to move away from us
		if (OtherPlayerMoveDot > 0.0f) { continue; }

		const Vector OtherPlayerDesiredDirection = GetPlayerAttemptedMoveDirection(OtherPlayerEdict);

		if (!vIsZero(OtherPlayerDesiredDirection))
		{
			const float OtherPlayerDesiredMoveDot = UTIL_GetDotProduct2D(OtherPlayerDesiredDirection, DesiredMoveDirection);

			// Player is wants to move away from us
			if (OtherPlayerMoveDot > 0.0f) { continue; }
		}

		// Ok, player is trying to move in the other direction to us.

		const Vector RightMoveDir = UTIL_GetVectorNormal2D(UTIL_GetCrossProduct(DesiredMoveDirection, UP_VECTOR));
		const float MoveTraceDistance = GetPlayerRadius() * 2.0f;

		const bool bPreferMoveLeft = (UTIL_GetDotProduct2D(OtherPlayerMoveDirection, RightMoveDir) >= 0.0f);

		const bool bCanMoveLeft = QuickMovementTrace(EAIMovementTraceType::LOWER_CENTER, -RightMoveDir, false, MoveTraceDistance);
		const bool bCanMoveRight = QuickMovementTrace(EAIMovementTraceType::LOWER_CENTER, RightMoveDir, false, MoveTraceDistance);

		if (bCanMoveLeft && (bPreferMoveLeft || !bCanMoveRight))
		{
			NextFrameMovementInput.DesiredMoveDirection = -RightMoveDir;
			return;
		}

		if (bCanMoveRight)
		{
			NextFrameMovementInput.DesiredMoveDirection = RightMoveDir;
			return;
		}

		if (!IsOnGround()) { return; }

		// We have little room for manoeuvre here...

		if (!IsPlayerOnGround(OtherPlayerEdict))
		{
			NextFrameMovementInput.bShouldCrouch;
			return;
		}

		if (IsOnGround())
		{
			const Vector CheckPoint = BotLocation + Vector(0.0f, 0.0f, 50.0f);
			if (AvHSUGetIsEnoughRoomForHull(CheckPoint, AvHMUGetHull(IsCrouching(), Edict->v.iuser3), Edict->v.pContainingEntity))
			{
				Jump(true);
				return;
			}
		}

		// We have nowhere to move, goal now is to go to the last space where we had some room to manoeuvre.
		if (!vIsZero(LastOpenSpace))
		{
			AvHAIMoveTask NewBackUpTask;
			if (AINAV_AddMoveMovementTask(GetNavProfile(), BotLocation, LastOpenSpace, nullptr, NewBackUpTask, CurrentPathNode))
			{
				UTIL_DrawLine(INDEXENT(1), BotLocation, LastOpenSpace, 10.0f, 0, 0, 128);
				AddMovementTask(NewBackUpTask);
				return;
			}
		}
	}
}

void AvHAIPlayer::HandleLadderPlayerAvoidance(const AvHAIPathNode* CurrentPathNode)
{
	PlayerListType ActivePlayerList = AIMGR_GetAllActivePlayers();

	if (ActivePlayerList.empty()) { return; }

	const edict_t* MyLadder = UTIL_GetNearestLadderAtPoint(CurrentPathNode->FromLocation);

	if (FNullEnt(MyLadder)) { return; }

	for (AvHPlayer* OtherPlayer : ActivePlayerList)
	{
		if (OtherPlayer == Player) { continue; }

		const edict_t* OtherPlayerEdict = OtherPlayer->edict();

		if (FNullEnt(OtherPlayerEdict)) { continue; }

		if (!IsPlayerOnLadder(OtherPlayerEdict)) { continue; }

		edict_t* OtherPlayerLadder = UTIL_GetNearestLadderAtPoint(OtherPlayerEdict->v.origin);

		if (FNullEnt(OtherPlayerLadder) || OtherPlayerLadder != MyLadder) { continue; }

		const bool bIsGoingUpLadder = (CurrentPathNode->FromLocation.z < CurrentPathNode->ToLocation.z);

		const bool bIsOtherPlayerInWay = (bIsGoingUpLadder)
			? OtherPlayerEdict->v.origin.z > Edict->v.origin.z
			: OtherPlayerEdict->v.origin.z < Edict->v.origin.z;

		if (!bIsOtherPlayerInWay) { continue; }

		// Check if the player is trying to climb in the opposite direction to us, ignore if they're going in the same direction
		if (!vIsNearlyZero(OtherPlayerEdict->v.velocity, 5.0f))
		{
			const Vector OtherClimbDirectionNormal = UTIL_GetVectorNormal(OtherPlayerEdict->v.velocity);

			const float ClimbDot = UTIL_GetDotProduct(OtherClimbDirectionNormal, (bIsGoingUpLadder) ? UP_VECTOR : DOWN_VECTOR);

			// Other player is not trying to climb in the opposite direction as us if their climb angle is not acute enough
			if (ClimbDot > -0.5f) { continue; }
		}

		if (!bIsGoingUpLadder && Edict->v.origin.z - CurrentPathNode->ToLocation.z < 200.0f)
		{
			if (IsOnLadder())
			{
				Jump(true);
			}

			NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - UTIL_GetCentreOfEntity(MyLadder));

			return;
		}

		const NavAgentProfile* NavProfile = GetNavProfile();

		if (!IsOnLadder())
		{
			if (AINAV_IsPointDirectlyReachable(NavProfile, Edict->v.origin, CurrentPathNode->ToLocation))
			{
				return;
			}
		}
		else
		{
			if (AINAV_IsPointDirectlyReachable(NavProfile, Edict->v.origin, CurrentPathNode->FromLocation))
			{
				NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(CurrentPathNode->FromLocation - Edict->v.origin);
				return;
			}
		}

		AvHAIMoveTask NewMoveTask;
		const bool bAddedMoveTask = AINAV_AddMoveMovementTask(NavProfile, GetBottomOfHitbox(), CurrentPathNode->FromLocation, nullptr, NewMoveTask, CurrentPathNode);

		if (bAddedMoveTask)
		{
			AddMovementTask(NewMoveTask);
			return;
		}
	}
}

bool AvHAIPlayer::QuickMovementTrace(EAIMovementTraceType TraceType, const Vector MoveDir, bool bPointTrace, float TraceDistance)
{
	if (vIsZero(MoveDir)) { return true; }

	const Vector MoveDirNormal = MoveDir.Normalize();

	Vector StartTrace = GetLocation();
	const Vector RightDir = UTIL_GetVectorNormal2D(UTIL_GetCrossProduct(MoveDir, UP_VECTOR));

	const float HullOffset = (bPointTrace) ? 1.0f : (UTIL_GetHullOffsetFromFloor(head_hull).z + 1.0f);

	switch (TraceType)
	{
		case EAIMovementTraceType::HEAD_CENTER:
			StartTrace.z = GetTopOfHitbox().z - HullOffset;
			break;
		case EAIMovementTraceType::HEAD_LEFT:
			StartTrace.z = GetTopOfHitbox().z - HullOffset;
			StartTrace = StartTrace - (RightDir * GetPlayerRadius());
			break;
		case EAIMovementTraceType::HEAD_RIGHT:
			StartTrace.z = GetTopOfHitbox().z - HullOffset;
			StartTrace = StartTrace + (RightDir * GetPlayerRadius());
			break;
		case EAIMovementTraceType::ORIGIN_LEFT:
			StartTrace = StartTrace - (RightDir * GetPlayerRadius());
			break;
		case EAIMovementTraceType::ORIGIN_RIGHT:
			StartTrace = StartTrace + (RightDir * GetPlayerRadius());
			break;
		case EAIMovementTraceType::LOWER_LEFT:
			StartTrace.z = GetBottomOfHitbox().z + HullOffset;
			StartTrace = StartTrace - (RightDir * GetPlayerRadius());
			break;
		case EAIMovementTraceType::LOWER_RIGHT:
			StartTrace.z = GetBottomOfHitbox().z + HullOffset;
			StartTrace = StartTrace + (RightDir * GetPlayerRadius());
			break;
		case EAIMovementTraceType::LOWER_CENTER:
			StartTrace.z = GetBottomOfHitbox().z + HullOffset;
			break;
		default:
			break;
	}

	// Point probes represent the left/right leading edges of the player's hull.
	if (bPointTrace) { StartTrace = StartTrace + (MoveDirNormal * GetPlayerRadius()); }

	Vector EndTrace = StartTrace + (MoveDirNormal * TraceDistance);

	TraceResult HitResult;
	bool bHitMovementStructure = false;
	const enum_hull TraceHull = (bPointTrace) ? point_hull : head_hull;

	if (!AITraceMovementSegment(StartTrace, EndTrace, TraceHull, Edict->v.pContainingEntity, HitResult, bHitMovementStructure)) { return true; }

	if (HitResult.fStartSolid || HitResult.fAllSolid) { return false; }

	if (TraceType != EAIMovementTraceType::LOWER_LEFT && TraceType != EAIMovementTraceType::LOWER_CENTER && TraceType != EAIMovementTraceType::LOWER_RIGHT) { return false; }

	if (!bHitMovementStructure && UTIL_GetWalkableSlopeAngle(HitResult.vecPlaneNormal) <= 45.0f) { return true; }

	float HeightOffset = 1.0f;

	Vector JumpTraceStart = StartTrace;
	Vector JumpTraceEnd = EndTrace;

	// Keep tracing upwards until we determine the blockage is above step height or not
	while (HeightOffset < 16.0f)
	{
		StartTrace.z = JumpTraceStart.z + HeightOffset;
		EndTrace.z = JumpTraceEnd.z + HeightOffset;

		if (!AITraceMovementSegment(StartTrace, EndTrace, TraceHull, Edict->v.pContainingEntity, HitResult, bHitMovementStructure)) { return true; }

		if (HitResult.fStartSolid || HitResult.fAllSolid) { return false; }

		if (!bHitMovementStructure && UTIL_GetWalkableSlopeAngle(HitResult.vecPlaneNormal) <= 45.0f) { return true; }

		HeightOffset += 1.0f;
	}

	return false;
}

void AvHAIPlayer::RemoveTask(const AvHAIMoveTask* Task)
{
	if (!Task) { return; }

	for (auto TaskIt = BotNavInfo.MovementTasks.begin(); TaskIt != BotNavInfo.MovementTasks.end();)
	{
		const AvHAIMoveTask* ThisTask = &(*TaskIt);

		if (!ThisTask || !ThisTask->IsValid())
		{
			TaskIt = BotNavInfo.MovementTasks.erase(TaskIt);
			continue;
		}

		if (ThisTask == Task)
		{
			BotNavInfo.MovementTasks.erase(TaskIt);
			return;
		}

		TaskIt++;
	}
}

bool AvHAIPlayer::HasWeapon(EAIWeaponId DesiredWeapon)
{
	if (!IsValid()) { return false; }

	return PlayerHasWeapon(Player, DesiredWeapon);
}

void AvHAIPlayer::ShootTarget(const edict_t* Target, EAIWeaponId DesiredWeapon)
{
	if (!UTIL_IsEdictActive(Target) || !PlayerHasWeapon(Player, DesiredWeapon)) { return; }

	const EAIWeaponId CurrentWeapon = GetCurrentWeapon();

	if (IsMeleeWeapon(CurrentWeapon))
	{
		LookAt(Target);
		NextFrameMovementInput.Button |= IN_ATTACK;
		return;
	}

	Vector TargetAimDir = ZERO_VECTOR;

	const Vector EyePosition = GetEyePosition();

	if (IsWeaponAffectedByGravity(DesiredWeapon))
	{
		Vector AimLocation = UTIL_GetCentreOfEntity(Target);

		const float ProjectileVelocity = GetProjectileVelocityForWeapon(DesiredWeapon);

		Vector NewAimAngle = vGetLaunchAngleForProjectile(EyePosition, AimLocation, ProjectileVelocity, GOLDSRC_GRAVITY);

		AimLocation = EyePosition + (NewAimAngle * 200.0f);

		LookAt(AimLocation);
		TargetAimDir = UTIL_GetVectorNormal(AimLocation - EyePosition);
	}
	else
	{
		LookAt(Target);
		TargetAimDir = UTIL_GetVectorNormal(UTIL_GetCentreOfEntity(Target) - EyePosition);
	}

	if (CurrentWeapon != DesiredWeapon)
	{
		NextFrameMovementInput.DesiredWeapon = DesiredWeapon;
		return;
	}

	Vector AimDir = UTIL_GetForwardVector(Edict->v.v_angle);

	bool bWillHit = false;

	float AimDot = UTIL_GetDotProduct(AimDir, TargetAimDir);

	// We can be less accurate with spores and umbra since they have AoE effects
	float MinAcceptableAccuracy = 0.9f;

	bWillHit = (AimDot >= MinAcceptableAccuracy);

	if (!bWillHit && IsHitscanWeapon(DesiredWeapon))
	{
		const edict_t* HitEntity = UTIL_TraceEntity(Edict, EyePosition, EyePosition + (AimDir * GetMaxIdealWeaponRange(DesiredWeapon)));

		bWillHit = (HitEntity == Target);
	}

	if (bWillHit)
	{
		// TODO: Refire timing

		//if ((gpGlobals->time - weapon.LastFireTime) >= pBot->current_weapon.MinRefireTime)
		//{
			NextFrameMovementInput.Button |= IN_ATTACK;
			//pBot->current_weapon.LastFireTime = gpGlobals->time;
		//}
	}
}

void AvHAIPlayer::DEBUG_DrawPath()
{
	AvHAIMoveTask* CurrentMoveTask = GetCurrentMoveTask();

	if (!CurrentMoveTask || !CurrentMoveTask->HasPath()) { return; }

	AINAV_DrawPath(&CurrentMoveTask->TaskPath);
}

EAINavMoveResult AvHAIPlayer::ProgressMoveTask(AvHAIMoveTask* MoveTask)
{
	if (!MoveTask || !MoveTask->IsValid()) { return EAINavMoveResult::NAV_MOVE_NOTASK; }

	if (!MoveTask->HasPath() || MoveTask->TaskPath.UsedNavMesh != GetNavProfile()->MeshIndex)
	{
		const bool bSuccess = AINAV_FindPathClosestToPoint(GetNavProfile(), UTIL_GetFloorUnderEntity(Edict), MoveTask->TaskLocation, &MoveTask->TaskPath, GetPlayerRadius());

		if (!bSuccess)
		{
			return EAINavMoveResult::NAV_MOVE_NOPATH;
		}
	}

	return FollowPath(&MoveTask->TaskPath);
}

EAINavMoveResult AvHAIPlayer::ProgressUseTask(AvHAIMoveTask* MoveTask)
{
	if (!MoveTask || !MoveTask->IsValid()) { return EAINavMoveResult::NAV_MOVE_NOTASK; }

	if (!UTIL_IsEdictActive(MoveTask->TaskTarget)) { return EAINavMoveResult::NAV_MOVE_INVALIDTASK; }

	if (IsPlayerInUseRange(Edict, MoveTask->TaskTarget))
	{
		const DynamicMapObject* TriggerTarget = AIMAP_GetDynamicObjectByEdict(MoveTask->TriggerToActivate);

		if (TriggerTarget && TriggerTarget->IsValid() && TriggerTarget->State != EAIDynamicMapObjectState::OBJECTSTATE_IDLE)
		{
			return EAINavMoveResult::NAV_MOVE_SUCCESS;
		}

		edict_t* NonConstRef = INDEXENT(ENTINDEX(MoveTask->TaskTarget));
		UseObject(NonConstRef, false);

		return EAINavMoveResult::NAV_MOVE_SUCCESS;
	}

	if (!MoveTask->HasPath() || MoveTask->TaskPath.UsedNavMesh != GetNavProfile()->MeshIndex)
	{
		const Vector ClosestPoint = UTIL_GetClosestPointOnEntityToLocation(GetEyePosition(), MoveTask->TaskTarget);

		const bool bSuccess = AINAV_FindPathClosestToPoint(GetNavProfile(), UTIL_GetFloorUnderEntity(Edict), ClosestPoint, &MoveTask->TaskPath, max_ai_use_reach);

		if (!bSuccess)
		{
			return EAINavMoveResult::NAV_MOVE_NOPATH;
		}
	}

	return FollowPath(&MoveTask->TaskPath);
}

EAINavMoveResult AvHAIPlayer::ProgressWeldTask(AvHAIMoveTask* MoveTask)
{
	if (!MoveTask || !MoveTask->IsValid()) { return EAINavMoveResult::NAV_MOVE_NOTASK; }

	if (!UTIL_IsEdictActive(MoveTask->TaskTarget)) { return EAINavMoveResult::NAV_MOVE_INVALIDTASK; }

	if (HasWeapon(EAIWeaponId::WEAPON_MARINE_WELDER))
	{
		EAIAttackResult WeldResult = PerformAttackLOSCheck(GetEyePosition(), EAIWeaponId::WEAPON_MARINE_WELDER, MoveTask->TaskTarget);

		if (WeldResult == EAIAttackResult::ATTACK_SUCCESS)
		{
			ShootTarget(MoveTask->TaskTarget, EAIWeaponId::WEAPON_MARINE_WELDER);

			return EAINavMoveResult::NAV_MOVE_SUCCESS;
		}

		if (!MoveTask->HasPath() || MoveTask->TaskPath.UsedNavMesh != GetNavProfile()->MeshIndex)
		{
			const Vector ClosestPoint = UTIL_GetClosestPointOnEntityToLocation(GetEyePosition(), MoveTask->TaskTarget);

			const bool bSuccess = AINAV_FindPathClosestToPoint(GetNavProfile(), UTIL_GetFloorUnderEntity(Edict), ClosestPoint, &MoveTask->TaskPath, max_ai_use_reach);

			if (!bSuccess)
			{
				return EAINavMoveResult::NAV_MOVE_NOPATH;
			}
		}

		return FollowPath(&MoveTask->TaskPath);
	}

	// TODO: Need to get a welder from somewhere

	return EAINavMoveResult::NAV_MOVE_SUCCESS;
}

EAINavMoveResult AvHAIPlayer::ProgressBreakTask(AvHAIMoveTask* MoveTask)
{
	if (!MoveTask || !MoveTask->IsValid()) { return EAINavMoveResult::NAV_MOVE_NOTASK; }

	const EAIWeaponId CurrentWeapon = GetCurrentWeapon();

	const EAIAttackResult AttackResult = PerformAttackLOSCheck(GetEyePosition(), CurrentWeapon, MoveTask->TaskTarget);

	if (AttackResult != EAIAttackResult::ATTACK_SUCCESS)
	{
		if (!MoveTask->HasPath() || MoveTask->TaskPath.UsedNavMesh != GetNavProfile()->MeshIndex)
		{
			const Vector ClosestPoint = UTIL_GetClosestPointOnEntityToLocation(GetEyePosition(), MoveTask->TaskTarget);

			const bool bSuccess = AINAV_FindPathClosestToPoint(GetNavProfile(), UTIL_GetFloorUnderEntity(Edict), ClosestPoint, &MoveTask->TaskPath, GetMaxIdealWeaponRange(CurrentWeapon));

			if (!bSuccess)
			{
				return EAINavMoveResult::NAV_MOVE_NOPATH;
			}
		}

		return FollowPath(&MoveTask->TaskPath);
	}

	ShootTarget(MoveTask->TaskTarget, CurrentWeapon);

	return EAINavMoveResult::NAV_MOVE_SUCCESS;
}

bool AvHAIPlayer::NextSwimMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	return true;
}

bool AvHAIPlayer::NextMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode_Mutable();

	const Vector CurrentPosition = GetLocation();

	if (vEquals(CurrentPosition, CurrentPathNode->LastRecordedPosition, 1.0f))
	{
		CurrentPathNode->TimeSinceMovement += gpGlobals->frametime;
	}
	else
	{
		CurrentPathNode->LastRecordedPosition = CurrentPosition;
		CurrentPathNode->TimeSinceMovement = 0.0f;
	}

	bool bMoveSuccess = false;

	if (IsPlayerSkulk(Edict))
	{
		// Always crouch as skulk so we don't wall stick. We undo this in the wall climb move.
		NextFrameMovementInput.bShouldCrouch = true;
	}

	bool bMadeMove = false;

	switch (CurrentPathNode->MovementFlag)
	{
		case EAINavMovementFlag::NAV_FLAG_WALK:
			bMadeMove = NewGroundMove(Path);
			CheckGroundMoveStuckAndReact(Path);
			break;
		case EAINavMovementFlag::NAV_FLAG_FALL:
		case EAINavMovementFlag::NAV_FLAG_FATALFALL:
			bMadeMove = NewFallMove(Path);
			CheckFallMoveStuckAndReact(Path);
			break;
		case EAINavMovementFlag::NAV_FLAG_JUMP:
		case EAINavMovementFlag::NAV_FLAG_FATALJUMP:
			bMadeMove = NewJumpMove(Path);
			CheckJumpMoveStuckAndReact(Path);
			break;
		case EAINavMovementFlag::NAV_FLAG_LADDER:
			bMadeMove = NewLadderMove(Path);
			CheckLadderMoveStuckAndReact(Path);
			break;
		case EAINavMovementFlag::NAV_FLAG_WALLCLIMB:
			bMadeMove = NewWallClimbMove(Path);
			CheckWallClimbMoveStuckAndReact(Path);
			break;
		case EAINavMovementFlag::NAV_FLAG_PLATFORM:
			return NewPlatformMove(Path);
		case EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM1:
		case EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM2:
			return NewPhaseGateMove(Path);
		default:
			bMadeMove = NewGroundMove(Path);
			CheckGroundMoveStuckAndReact(Path);
			break;
	}

	return bMadeMove;
}

void AvHAIPlayer::CheckGroundMoveStuckAndReact(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return; }

	AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode_Mutable();

	if (!CurrentPathNode || !CurrentPathNode->IsValidMove()) { return; }

	// We are hopelessly stuck
	if (CurrentPathNode->TimeSinceMovement > 10.0f)
	{
		Suicide();
		return;
	}

	const AvHAIPathNode* NextPathNode = Path->GetNextPathNode();

	const Vector BotLocation = GetLocation();

	if (!IsOnGround())
	{
		// Failing that, try and snap the bot back onto the path
		if (CurrentPathNode->TimeSinceMovement > 4.0f)
		{
			const Vector FloorPosition = UTIL_GetFloorUnderEntity(Edict) + Vector(0.0f, 0.0f, (GetPlayerHeight() + 1.0f));
			Vector PointOnPath = vClosestPointOnLine(CurrentPathNode->FromLocation, CurrentPathNode->ToLocation, FloorPosition);

			if (AvHSUGetIsEnoughRoomForHull(PointOnPath, AvHMUGetHull(IsCrouching(), Edict->v.iuser3), Edict->v.pContainingEntity))
			{
				Edict->v.origin = PointOnPath;
				return;
			}

			const Vector TravelDir = UTIL_GetVectorNormal(CurrentPathNode->ToLocation - CurrentPathNode->FromLocation);
			const float TeleportDistance = (Edict->v.absmax - Edict->v.absmin).Length2D();

			PointOnPath = PointOnPath + (TravelDir * TeleportDistance);

			if (AvHSUGetIsEnoughRoomForHull(PointOnPath, AvHMUGetHull(IsCrouching(), Edict->v.iuser3), Edict->v.pContainingEntity))
			{
				Edict->v.origin = PointOnPath;
				return;
			}

			PointOnPath = PointOnPath + (TravelDir * TeleportDistance);

			if (AvHSUGetIsEnoughRoomForHull(PointOnPath, AvHMUGetHull(IsCrouching(), Edict->v.iuser3), Edict->v.pContainingEntity))
			{
				Edict->v.origin = PointOnPath;
				return;
			}

			return;
		}

		// First, stop movement to see if we
		if (CurrentPathNode->TimeSinceMovement > 2.0f)
		{
			NextFrameMovementInput.Clear();
			NextFrameMovementInput.bShouldCrouch = true;

			const Vector CheckPoint = BotLocation + Vector(0.0f, 0.0f, 50.0f);
			if (AvHSUGetIsEnoughRoomForHull(CheckPoint, AvHMUGetHull(IsCrouching(), Edict->v.iuser3), Edict->v.pContainingEntity))
			{
				Edict->v.origin = CheckPoint;
			}

			return;
		}

		return;
	}

	// We are on the ground, check if we're stuck
	if (CurrentPathNode->TimeSinceMovement > 4.0f)
	{
		Vector PointOnPath = vClosestPointOnLine(CurrentPathNode->FromLocation, CurrentPathNode->ToLocation, BotLocation);
		PointOnPath.z = Edict->v.origin.z;

		const Vector TravelDir = UTIL_GetVectorNormal(CurrentPathNode->ToLocation - CurrentPathNode->FromLocation);
		const float TeleportDistance = (Edict->v.absmax - Edict->v.absmin).Length2D();

		PointOnPath = PointOnPath + (TravelDir * TeleportDistance);
		float HeightOffset = 0.0f;

		while (HeightOffset < 20.0f)
		{
			const Vector CheckPointOnPath = PointOnPath + Vector(0.0f, 0.0f, HeightOffset);
			if (AvHSUGetIsEnoughRoomForHull(CheckPointOnPath, AvHMUGetHull(IsCrouching(), Edict->v.iuser3), Edict->v.pContainingEntity))
			{
				Edict->v.origin = CheckPointOnPath;
				return;
			}

			HeightOffset += 2.0f;
		}

		HeightOffset = 0.0f;

		PointOnPath = PointOnPath + (TravelDir * TeleportDistance);

		while (HeightOffset < 20.0f)
		{
			const Vector CheckPointOnPath = PointOnPath + Vector(0.0f, 0.0f, HeightOffset);
			if (AvHSUGetIsEnoughRoomForHull(CheckPointOnPath, AvHMUGetHull(IsCrouching(), Edict->v.iuser3), Edict->v.pContainingEntity))
			{
				Edict->v.origin = CheckPointOnPath;
				return;
			}

			HeightOffset += 2.0f;
		}

		HeightOffset = 0.0f;

		PointOnPath = PointOnPath + (TravelDir * TeleportDistance);

		while (HeightOffset < 20.0f)
		{
			const Vector CheckPointOnPath = PointOnPath + Vector(0.0f, 0.0f, HeightOffset);
			if (AvHSUGetIsEnoughRoomForHull(CheckPointOnPath, AvHMUGetHull(IsCrouching(), Edict->v.iuser3), Edict->v.pContainingEntity))
			{
				Edict->v.origin = CheckPointOnPath;
				return;
			}

			HeightOffset += 2.0f;
		}

		return;
	}
}

void AvHAIPlayer::CheckFallMoveStuckAndReact(AvHAIPath* Path)
{

}

void AvHAIPlayer::CheckJumpMoveStuckAndReact(AvHAIPath* Path)
{

}

void AvHAIPlayer::CheckLadderMoveStuckAndReact(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return; }

	AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode_Mutable();

	if (!CurrentPathNode || !CurrentPathNode->IsValidMove()) { return; }

	// We are hopelessly stuck
	if (CurrentPathNode->TimeSinceMovement > 10.0f)
	{
		Suicide();
		return;
	}

	const Vector BotLocation = GetLocation();

	if (CurrentPathNode->TimeSinceMovement > 4.0f)
	{
		NextFrameMovementInput.bShouldCrouch = true;

		const bool bGoingUpLadder = (CurrentPathNode->ToLocation.z > CurrentPathNode->FromLocation.z);

		const float ClimbedHeight = (bGoingUpLadder)
			? fmaxf(BotLocation.z - CurrentPathNode->FromLocation.z, 0.0f)
			: fmaxf(CurrentPathNode->FromLocation.z - BotLocation.z, 0.0f);

		const float DesiredClimbHeight = (bGoingUpLadder)
			? CurrentPathNode->ToLocation.z - CurrentPathNode->FromLocation.z
			: CurrentPathNode->FromLocation.z - CurrentPathNode->ToLocation.z;

		const float ClimbProgress = (ClimbedHeight / DesiredClimbHeight);

		if (ClimbProgress < 0.25f)
		{
			const edict_t* MountLadder = UTIL_GetNearestLadderAtPoint(CurrentPathNode->FromLocation);
			const Vector LadderNormal = UTIL_GetLadderNormal(CurrentPathNode->FromLocation, MountLadder);

			if (UTIL_IsEdictActive(MountLadder))
			{
				const Vector LadderMount = AINAV_GetLadderMountPoint(MountLadder, CurrentPathNode->FromLocation);

				if (!vIsZero(LadderMount))
				{
					float Offset = 0.0f;

					while (Offset < 20.0f)
					{
						const Vector CheckPointOnLadder = (bGoingUpLadder)
							? LadderMount + ((LadderNormal * Offset) + Vector(0.0f, 0.0f, Offset))
							: LadderMount + ((LadderNormal * Offset) - Vector(0.0f, 0.0f, Offset));

						if (AvHSUGetIsEnoughRoomForHull(CheckPointOnLadder, AvHMUGetHull(IsCrouching(), Edict->v.iuser3), Edict->v.pContainingEntity))
						{
							Edict->v.origin = CheckPointOnLadder;
							return;
						}

						Offset += 2.0f;
					}
				}
			}
		}
		else if (ClimbProgress > 0.75f)
		{
			if (vDist3DSq(BotLocation, CurrentPathNode->ToLocation) < sqrf(100.0f))
			{
				float Offset = 0.0f;

				while (Offset < 20.0f)
				{
					const Vector Checkpoint = CurrentPathNode->ToLocation + Vector(0.0f, 0.0f, Offset);

					if (AvHSUGetIsEnoughRoomForHull(Checkpoint, AvHMUGetHull(IsCrouching(), Edict->v.iuser3), Edict->v.pContainingEntity))
					{
						Edict->v.origin = Checkpoint;
						return;
					}

					Offset += 2.0f;
				}
			}
		}
		else
		{
			if (IsOnLadder())
			{
				Jump(true);
			}
		}
	}
}

void AvHAIPlayer::CheckWallClimbMoveStuckAndReact(AvHAIPath* Path)
{

}

bool AvHAIPlayer::NewGroundMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	if (IsOnLadder())
	{
		Jump(false);
	}

	AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode_Mutable();
	const AvHAIPathNode* NextPathNode = Path->GetNextPathNode();

	const Vector CurrentPos = GetBottomOfHitbox();

	const Vector vForward = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - CurrentPos);
	// Same goes for the right vector, might not be the same as the bot's right
	const Vector vRight = UTIL_GetVectorNormal(UTIL_GetCrossProduct(vForward, UP_VECTOR));

	if (IsOnGround())
	{
		const bool bCanMoveRight = QuickMovementTrace(EAIMovementTraceType::LOWER_CENTER, vRight, false, GetPlayerRadius() * 2.0f);
		const bool bCanMoveLeft = QuickMovementTrace(EAIMovementTraceType::LOWER_CENTER, -vRight, false, GetPlayerRadius() * 2.0f);

		if (bCanMoveRight || bCanMoveLeft)
		{
			LastOpenSpace = GetLocation();
		}
	}

	bool bAdjustingForCollision = false;

	const bool bBumpLeft = !QuickMovementTrace(EAIMovementTraceType::ORIGIN_LEFT, vForward, /* Point trace? */ true);
	const bool bBumpRight = !QuickMovementTrace(EAIMovementTraceType::ORIGIN_RIGHT, vForward, /* Point trace? */ true);

	NextFrameMovementInput.DesiredMoveDirection = vForward;

	if (bBumpRight && !bBumpLeft)
	{
		NextFrameMovementInput.DesiredMoveDirection = NextFrameMovementInput.DesiredMoveDirection - vRight;
	}
	else if (bBumpLeft && !bBumpRight)
	{
		NextFrameMovementInput.DesiredMoveDirection = NextFrameMovementInput.DesiredMoveDirection + vRight;
	}

	if (!QuickMovementTrace(EAIMovementTraceType::HEAD_CENTER, vForward, /* Point trace? */ false))
	{
		NextFrameMovementInput.bShouldCrouch = true;
	}

	if (!QuickMovementTrace(EAIMovementTraceType::LOWER_CENTER, vForward, /* Point trace? */ false))
	{
		Jump(true);
	}

	NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(NextFrameMovementInput.DesiredMoveDirection);

	if (CanCrouch())
	{
		if (EnumHasAnyFlags(CurrentPathNode->MovementFlag, EAINavMovementFlag::NAV_FLAG_CROUCH))
		{
			NextFrameMovementInput.bShouldCrouch = true;
		}
		else
		{
			if (NextPathNode && NextPathNode->MovementArea == EAINavArea::NAV_AREA_CROUCH)
			{
				NextFrameMovementInput.bShouldCrouch = true;
				return true;
			}

			Vector HeadLocation = GetPlayerTopOfCollisionHull(Edict, false);

			// Crouch if we have something in our way at head height
			if (!UTIL_QuickTrace(Edict, HeadLocation, (HeadLocation + (NextFrameMovementInput.DesiredMoveDirection * 50.0f))))
			{
				NextFrameMovementInput.bShouldCrouch = true;
			}
		}
	}

	return true;
}

bool AvHAIPlayer::NewFallMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	if (IsOnLadder())
	{
		Jump(false);
	}

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();

	const Vector AIPlayerLocation = GetLocation();
	const Vector vBotOrientation = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - AIPlayerLocation);
	const Vector vForward = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - CurrentPathNode->FromLocation);

	if (!IsOnGround())
	{
		NextFrameMovementInput.DesiredMoveDirection = vBotOrientation;
		return true;
	}

	if (vDist2DSq(AIPlayerLocation, CurrentPathNode->ToLocation) > sqrf(GetPlayerRadius()))
	{
		NextFrameMovementInput.DesiredMoveDirection = vBotOrientation;
	}
	else
	{
		NextFrameMovementInput.DesiredMoveDirection = vForward;
	}

	if (!CanCrouch()) { return true; }

	const Vector HeadLocation = GetPlayerTopOfCollisionHull(Edict, false);

	if (!UTIL_QuickTrace(Edict, HeadLocation, (HeadLocation + (NextFrameMovementInput.DesiredMoveDirection * 50.0f))))
	{
		NextFrameMovementInput.bShouldCrouch = true;
	}

	return true;
}

bool AvHAIPlayer::NewJumpMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	if (IsOnLadder())
	{
		Jump(false);
		return true;
	}

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();
	const AvHAIPathNode* NextPathNode = Path->GetNextPathNode();

	Vector vForward = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - GetLocation());

	if (vIsZero(vForward))
	{
		vForward = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - CurrentPathNode->FromLocation);
	}

	const Vector CurrentVelocity = GetVelocity();
	const Vector CurrentVelocity2D = UTIL_GetVectorNormal2D(CurrentVelocity);

	NextFrameMovementInput.DesiredMoveDirection = vForward;

	// Yes, this is cheating
	if (IsOnGround())
	{
		float MoveSpeed = vSize2D(GetVelocity());
		Vector NewVelocity = vForward * fmaxf(MoveSpeed, GetDesiredMovementSpeed(false));
		NewVelocity.z = CurrentVelocity.z;

		NextFrameMovementInput.VelocityOverride = NewVelocity;
	}

	Jump(true);

	if (!CanCrouch()) { return true; }

	if (NextPathNode && NextPathNode->MovementArea == EAINavArea::NAV_AREA_CROUCH)
	{
		NextFrameMovementInput.bShouldCrouch = true;
		return true;
	}

	Vector HeadLocation = GetPlayerTopOfCollisionHull(Edict, false);

	if (!UTIL_QuickTrace(Edict, HeadLocation, (HeadLocation + (NextFrameMovementInput.DesiredMoveDirection * 50.0f))))
	{
		NextFrameMovementInput.bShouldCrouch = true;
	}

	return true;
}

bool AvHAIPlayer::NewLadderMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();

	if (!CurrentPathNode || !CurrentPathNode->IsValidMove()) { return false; }

	const Vector BotLocation = GetLocation();
	const Vector BotEyePosition = GetEyePosition();
	const Vector CollisionBottomLocation = GetBottomOfHitbox();
	const Vector CollisionTopLocation = GetTopOfHitbox();
	const float PlayerRadius = GetPlayerRadius();

	const bool bIsUpwardsLadderMove = (CurrentPathNode->FromLocation.z < CurrentPathNode->ToLocation.z);

	if (bIsUpwardsLadderMove)
	{
		return NewClimbUpLadderMove(Path);
	}
	else
	{
		return NewClimbDownLadderMove(Path);
	}
}

bool AvHAIPlayer::NewClimbUpLadderMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();
	const AvHAIPathNode* NextPathNode = Path->GetNextPathNode();

	if (!CurrentPathNode || !CurrentPathNode->IsValidMove()) { return false; }

	const bool bHasNextPathNode = NextPathNode && NextPathNode->IsValidMove();

	const Vector BotLocation = GetLocation();
	const Vector CollisionTopLocation = GetTopOfHitbox();
	const Vector CollisionBottomLocation = GetBottomOfHitbox();

	edict_t* CurrentLadder = UTIL_GetNearestLadderAtPoint(BotLocation);

	if (FNullEnt(CurrentLadder))
	{
		return MoveToWithoutNav(CurrentPathNode->ToLocation) == EAINavMoveResult::NAV_MOVE_SUCCESS;
	}

	Vector LadderTop = UTIL_GetCentreOfEntity(CurrentLadder);
	LadderTop.z = CurrentLadder->v.absmax.z;

	if (!UTIL_QuickHullTrace(Edict, BotLocation, CollisionTopLocation, GetPlayerHull(false)))
	{
		NextFrameMovementInput.bShouldCrouch = true;
	}

	if (!IsOnLadder())
	{
		if (IsOnGround())
		{
			if (AINAV_IsPointDirectlyReachable(GetNavProfile(), CollisionBottomLocation, CurrentPathNode->ToLocation))
			{
				NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - GetLocation());

				if (bHasNextPathNode && NextPathNode->MovementFlag == EAINavMovementFlag::NAV_FLAG_CROUCH)
				{
					NextFrameMovementInput.bShouldCrouch = true;
				}

				return true;
			}

			return NewMountLadderMove(Path);
		}

		if (BotLocation.z >= CurrentPathNode->ToLocation.z)
		{
			NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - GetLocation());
			NextFrameMovementInput.DesiredLookLocation = CurrentPathNode->ToLocation;

			if (bHasNextPathNode && CanCrouch() && NextPathNode->MovementArea == EAINavArea::NAV_AREA_CROUCH)
			{
				NextFrameMovementInput.bShouldCrouch = true;
			}

			return true;
		}

		NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(LadderTop - GetLocation());
		NextFrameMovementInput.RequiredLookLocation = LadderTop;

		return true;
	}

	// We're on the ladder and actively climbing

	Vector LadderNormalCheck = CollisionBottomLocation + Vector(0.0f, 0.0f, 18.0f);
	LadderNormalCheck = LadderNormalCheck + UTIL_GetVectorNormal2D(LadderNormalCheck - LadderTop);

	Vector CurrentLadderNormal = UTIL_GetNearestLadderNormal(LadderNormalCheck);

	CurrentLadderNormal = UTIL_GetVectorNormal2D(CurrentLadderNormal);

	if (vIsZero(CurrentLadderNormal))
	{
		CurrentLadderNormal = UTIL_GetVectorNormal2D(CurrentPathNode->FromLocation - CurrentPathNode->ToLocation);
	}

	const Vector LadderRightNormal = UTIL_GetVectorNormal(UTIL_GetCrossProduct(CurrentLadderNormal, UP_VECTOR));

	Vector ClimbRightNormal = -LadderRightNormal;
	Vector ClimbDir = -CurrentLadderNormal;

	Vector DisembarkDir = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - BotLocation);
	float DisembarkDot = UTIL_GetDotProduct2D(CurrentLadderNormal, DisembarkDir);

	float DesiredClimbHeight = CurrentPathNode->RequiredClimbZ;

	if (DisembarkDot > 0.75f)
	{
		float JumpDist = vDist2DSq(BotLocation, CurrentPathNode->ToLocation);

		float ExtraClimbHeight = (JumpDist > sqrf(2.0f)) ? 100.0f : 50.0f;

		DesiredClimbHeight = fminf((CurrentPathNode->RequiredClimbZ + ExtraClimbHeight), LadderTop.z);
	}

	// We've reached the end of the ladder, try and make it to the disembark point if we can
	if (LadderTop.z - CollisionBottomLocation.z <= 2.0f || BotLocation.z >= DesiredClimbHeight)
	{
		if (bHasNextPathNode && CanCrouch() && NextPathNode->MovementArea == EAINavArea::NAV_AREA_CROUCH)
		{
			NextFrameMovementInput.bShouldCrouch = true;
		}

		if (DisembarkDot > 0.75f)
		{
			Jump(true);
			return true;
		}

		NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - GetLocation());
		NextFrameMovementInput.RequiredLookLocation = (GetEyePosition() + (DisembarkDir * 50.0f)) + Vector(0.0f, 0.0f, 50.0f);

		return true;
	}

	// Still climbing

	Vector TraceStartPosition = CollisionTopLocation;

	Vector StartLeftTrace = TraceStartPosition - (ClimbRightNormal * GetPlayerRadius());
	Vector StartRightTrace = TraceStartPosition + (ClimbRightNormal * GetPlayerRadius());

	Vector EndLeftTrace = StartLeftTrace + Vector(0.0f, 0.0f, 2.0f);
	Vector EndRightTrace =StartRightTrace + Vector(0.0f, 0.0f, 2.0f);

	bool bBlockedLeft = !UTIL_QuickTrace(Edict, StartLeftTrace, EndLeftTrace);
	bool bBlockedRight = !UTIL_QuickTrace(Edict, StartRightTrace, EndRightTrace);

	// Look up at the top of the ladder

	// If we are blocked going up the ladder, face the ladder and slide left/right to avoid blockage
	if (bBlockedLeft && !bBlockedRight)
	{
		Vector LookLocation = BotLocation - (CurrentLadderNormal * 50.0f);
		LookLocation.z = DesiredClimbHeight + 100.0f;
		NextFrameMovementInput.RequiredLookLocation = LookLocation;
		NextFrameMovementInput.DesiredMoveDirection = ClimbRightNormal;
		return true;
	}

	if (bBlockedRight && !bBlockedLeft)
	{
		Vector LookLocation = BotLocation - (CurrentLadderNormal * 50.0f);
		LookLocation.z = DesiredClimbHeight + 100.0f;
		NextFrameMovementInput.RequiredLookLocation = LookLocation;
		NextFrameMovementInput.DesiredMoveDirection = -ClimbRightNormal;
		return true;
	}

	NextFrameMovementInput.DesiredMoveDirection = ClimbDir;

	Vector LookTarget = CurrentPathNode->ToLocation;
	LookTarget = LadderTop + (ClimbDir * 50.0f);
	LookTarget.z = DesiredClimbHeight + 50.0f;

	NextFrameMovementInput.RequiredLookLocation = LookTarget;

	return true;
}

bool AvHAIPlayer::NewClimbDownLadderMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();

	if (!CurrentPathNode || !CurrentPathNode->IsValidMove()) { return false; }

	const AvHAIPathNode* NextPathNode = Path->GetNextPathNode();

	const bool bHasNextPathNode = NextPathNode && NextPathNode->IsValidMove();
	const Vector BotLocation = GetLocation();
	const Vector CollisionTopLocation = GetTopOfHitbox();
	const Vector CollisionBottomLocation = GetBottomOfHitbox();

	edict_t* CurrentLadder = UTIL_GetNearestLadderAtPoint(BotLocation);

	if (FNullEnt(CurrentLadder))
	{
		return MoveToWithoutNav(CurrentPathNode->ToLocation) == EAINavMoveResult::NAV_MOVE_SUCCESS;
	}

	Vector LadderBottom = UTIL_GetCentreOfEntity(CurrentLadder);
	LadderBottom.z = CurrentLadder->v.absmin.z;

	if (!IsOnLadder())
	{
		if (IsOnGround())
		{
			if (AINAV_IsPointDirectlyReachable(GetNavProfile(), CollisionBottomLocation, CurrentPathNode->ToLocation))
			{
				NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - GetLocation());

				if (bHasNextPathNode && NextPathNode->MovementArea == EAINavArea::NAV_AREA_CROUCH)
				{
					NextFrameMovementInput.bShouldCrouch = true;
				}

				return true;
			}

			return NewMountLadderMove(Path);
		}

		if (BotLocation.z >= CurrentPathNode->RequiredClimbZ)
		{
			NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - GetLocation());

			if (bHasNextPathNode && NextPathNode->MovementArea == EAINavArea::NAV_AREA_CROUCH)
			{
				NextFrameMovementInput.bShouldCrouch = true;
			}

			return true;
		}

		NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(LadderBottom - GetLocation());
		NextFrameMovementInput.RequiredLookLocation = LadderBottom;

		return true;
	}

	// We're on the ladder and actively climbing

	Vector LadderNormalCheck = CollisionBottomLocation + Vector(0.0f, 0.0f, 18.0f);
	LadderNormalCheck = LadderNormalCheck + UTIL_GetVectorNormal2D(LadderNormalCheck - LadderBottom);

	Vector CurrentLadderNormal = UTIL_GetNearestLadderNormal(LadderNormalCheck);

	CurrentLadderNormal = UTIL_GetVectorNormal2D(CurrentLadderNormal);

	if (vIsZero(CurrentLadderNormal))
	{
		CurrentLadderNormal = UTIL_GetVectorNormal2D(CurrentPathNode->FromLocation - CurrentPathNode->ToLocation);
	}

	const Vector LadderRightNormal = UTIL_GetVectorNormal(UTIL_GetCrossProduct(CurrentLadderNormal, UP_VECTOR));

	Vector ClimbRightNormal = LadderRightNormal;
	Vector ClimbDir = CurrentLadderNormal;

	Vector DisembarkDir = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - BotLocation);
	float DisembarkDot = UTIL_GetDotProduct2D(CurrentLadderNormal, DisembarkDir);

	float DesiredClimbHeight = CurrentPathNode->RequiredClimbZ;

	if (DisembarkDot > 0.75f)
	{
		float JumpDist = vDist2DSq(BotLocation, CurrentPathNode->ToLocation);

		float ExtraClimbHeight = (JumpDist > sqrf(2.0f)) ? 100.0f : 50.0f;

		DesiredClimbHeight = fmaxf((CurrentPathNode->RequiredClimbZ + ExtraClimbHeight), LadderBottom.z + GetPlayerOriginOffsetFromFloor(Edict, true).z);
	}

	if (bHasNextPathNode)
	{
		if (BotLocation.z <= DesiredClimbHeight || (CollisionBottomLocation.z - NextPathNode->ToLocation.z < 100.0f) || fabsf(CollisionBottomLocation.z - LadderBottom.z) <= 2.0f)
		{
			// We're close enough to the end that we can jump off the ladder
			if (UTIL_QuickTrace(Edict, CollisionTopLocation, NextPathNode->ToLocation))
			{
				NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - GetLocation());
				Jump(true);
				return true;
			}
		}
	}

	// Still climbing

	Vector TraceStartPosition = CollisionBottomLocation;

	Vector StartLeftTrace = TraceStartPosition - (ClimbRightNormal * GetPlayerRadius());
	Vector StartRightTrace = TraceStartPosition + (ClimbRightNormal * GetPlayerRadius());

	Vector EndLeftTrace = StartLeftTrace - Vector(0.0f, 0.0f, 2.0f);
	Vector EndRightTrace = StartRightTrace - Vector(0.0f, 0.0f, 2.0f);

	bool bBlockedLeft = !UTIL_QuickTrace(Edict, StartLeftTrace, EndLeftTrace);
	bool bBlockedRight = !UTIL_QuickTrace(Edict, StartRightTrace, EndRightTrace);

	// Look up at the top of the ladder

	// If we are blocked going up the ladder, face the ladder and slide left/right to avoid blockage
	if (bBlockedLeft && !bBlockedRight)
	{
		Vector LookLocation = BotLocation - (CurrentLadderNormal * 50.0f);
		LookLocation.z = DesiredClimbHeight + 100.0f;
		NextFrameMovementInput.RequiredLookLocation = LookLocation;
		NextFrameMovementInput.DesiredMoveDirection = ClimbRightNormal;
		return true;
	}

	if (bBlockedRight && !bBlockedLeft)
	{
		Vector LookLocation = BotLocation - (CurrentLadderNormal * 50.0f);
		LookLocation.z = DesiredClimbHeight + 100.0f;
		NextFrameMovementInput.RequiredLookLocation = LookLocation;
		NextFrameMovementInput.DesiredMoveDirection = -ClimbRightNormal;
		return true;
	}

	NextFrameMovementInput.DesiredMoveDirection = ClimbDir;
	NextFrameMovementInput.RequiredLookLocation = CurrentPathNode->ToLocation;

	return true;
}

bool AvHAIPlayer::NewMountLadderMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();

	edict_t* MountLadder = UTIL_GetNearestLadderAtPoint(CurrentPathNode->FromLocation);

	if (FNullEnt(MountLadder))
	{
		return MoveToWithoutNav(CurrentPathNode->ToLocation) == EAINavMoveResult::NAV_MOVE_SUCCESS;
	}

	const Vector BotCurrentLocation = GetLocation();

	Vector LadderCentre = UTIL_GetCentreOfEntity(MountLadder);

	Vector MountPoint = AINAV_GetLadderMountPoint(MountLadder, CurrentPathNode->FromLocation);

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

	if (bMountingFromTop && !IsCrouching())
	{
		NextFrameMovementInput.bShouldWalk = true;
		NextFrameMovementInput.RequiredLookLocation = CurrentPathNode->ToLocation + (UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - BotCurrentLocation) * 100.0f);
	}

	NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(MountPoint - BotCurrentLocation);

	return true;
}

bool AvHAIPlayer::NewPlatformMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	if (IsOnLadder())
	{
		Jump(false);
	}

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();

	if (!CurrentPathNode || !CurrentPathNode->IsValidMove()) { return false; }

	const DynamicMapObject* Platform = nullptr;

	if (!FNullEnt(Edict->v.groundentity))
	{
		Platform = AIMAP_GetDynamicObjectByEdict(Edict->v.groundentity);
	}

	if (!Platform || !Platform->IsValid())
	{
		Platform = AIMAP_GetDynamicObjectByEdict(CurrentPathNode->MovementObject);
	}

	if (!Platform || !Platform->IsValid())
	{
		return NewGroundMove(Path);
	}

	const NavAgentProfile* NavProfile = GetNavProfile();
	const Vector BotLocation = GetLocation();
	const Vector PlatformPosition = UTIL_GetCentreOfEntity(Platform->Edict);

	DynamicMapObjectStop DesiredStartStop;
	DynamicMapObjectStop DesiredEndStop;

	const bool bFoundStops = AIMAP_GetDesiredPlatformStops(Platform, CurrentPathNode->FromLocation, CurrentPathNode->ToLocation, DesiredStartStop, DesiredEndStop);

	if (!bFoundStops)
	{
		return false;
	}

	// We are on the lift
	if (Edict->v.groundentity == Platform->Edict)
	{
		return NewRidePlatformMove(Path, Platform->Edict, DesiredEndStop.StopLocation);
	}

	const Vector CollisionBottomLocation = GetBottomOfHitbox();

	// We have disembarked and just need to head to the end point
	if (AINAV_IsPointDirectlyReachable(NavProfile, CollisionBottomLocation, CurrentPathNode->ToLocation) || vDist3DSq(CollisionBottomLocation, CurrentPathNode->ToLocation) < sqrf(100.0f))
	{
		return MoveToWithoutNav(CurrentPathNode->ToLocation) == EAINavMoveResult::NAV_MOVE_SUCCESS;
	}

	if (!AIMAP_PlatformNeedsActivating(NavProfile, Platform, CurrentPathNode->FromLocation, CurrentPathNode->ToLocation))
	{
		if (AIMAP_CanBoardPlatform(NavProfile, Platform, CurrentPathNode->FromLocation, DesiredStartStop.StopLocation))
		{
			return NewBoardPlatformMove(Path, Platform->Edict);
		}
		else
		{
			AvHAIMoveTask NewDestinationTask;

			const bool bSuccess = AINAV_AddMoveMovementTask(NavProfile, UTIL_GetFloorUnderEntity(Edict), CurrentPathNode->FromLocation, nullptr, NewDestinationTask);

			if (bSuccess)
			{
				AddMovementTask(NewDestinationTask);
			}

			return bSuccess;
		}
	}

	const DynamicMapObjectStop* CurrentStop = Platform->GetCurrentStop();

	if (CurrentStop && vEquals(CurrentStop->StopLocation, DesiredStartStop.StopLocation))
	{
		const DynamicMapObject* PlatTrigger = AIMAP_GetTriggerReachableFromPlatform(Platform, CurrentPathNode->FromLocation.z + 32.0f, DesiredStartStop.StopLocation);

		if (PlatTrigger)
		{
			return NewBoardPlatformMove(Path, Platform->Edict);
		}
	}

	const DynamicMapObject* BestTrigger = nullptr;

	BestTrigger = AIMAP_GetBestTriggerForObject(NavProfile, Platform, BotLocation);

	if (BestTrigger)
	{
		AvHAIMoveTask NewDestinationTask;

		const bool bSuccess = AINAV_AddTriggerMovementTask(NavProfile, UTIL_GetFloorUnderEntity(Edict), CurrentPathNode->ToLocation, BestTrigger->Edict, Platform->Edict, NewDestinationTask);

		if (bSuccess)
		{
			AddMovementTask(NewDestinationTask);
		}

		return bSuccess;
	}

	if (Platform->State == EAIDynamicMapObjectState::OBJECTSTATE_IDLE)
	{
		for (auto it = Platform->Triggers.begin(); it != Platform->Triggers.end(); it++)
		{
			const DynamicMapObject* ThisTrigger = AIMAP_GetDynamicObjectByEdict((*it));

			if (ThisTrigger && ThisTrigger->IsValid() && ThisTrigger->bIsActive && ThisTrigger->State == EAIDynamicMapObjectState::OBJECTSTATE_IDLE)
			{
				AIMAP_ForceActivateTrigger(this, ThisTrigger);
				return true;
			}
		}
	}
	else
	{
		AvHAIMoveTask NewDestinationTask;

		const bool bSuccess = AINAV_AddMoveMovementTask(NavProfile, UTIL_GetFloorUnderEntity(Edict), CurrentPathNode->FromLocation, nullptr, NewDestinationTask, CurrentPathNode);

		if (bSuccess)
		{
			AddMovementTask(NewDestinationTask);
		}

		return bSuccess;
	}

	return false;
}

bool AvHAIPlayer::NewBoardPlatformMove(AvHAIPath* Path, const edict_t* Platform)
{
	if (!Path || !Path->IsValidPath() || !UTIL_IsEdictActive(Platform)) { return false; }

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();

	if (!CurrentPathNode || !CurrentPathNode->IsValidMove()) { return false; }

	const Vector BotLocation = GetLocation();

	Vector ClosestPointOnLift = UTIL_GetClosestPointOnEntityToLocation(CurrentPathNode->FromLocation, Platform);

	Vector ClosestPointOnLine = vClosestPointOnLine2D(CurrentPathNode->FromLocation, ClosestPointOnLift, BotLocation);

	if (vEquals2D(ClosestPointOnLine, CurrentPathNode->FromLocation) && vDist2DSq(BotLocation, CurrentPathNode->FromLocation) > sqrf(32.0f))
	{
		AvHAIMoveTask NewDestinationTask;

		const bool bSuccess = AINAV_AddMoveMovementTask(GetNavProfile(), UTIL_GetFloorUnderEntity(Edict), CurrentPathNode->FromLocation, nullptr, NewDestinationTask, CurrentPathNode);

		if (bSuccess)
		{
			AddMovementTask(NewDestinationTask);
		}

		return bSuccess;
	}
	else
	{
		return MoveToWithoutNav(UTIL_GetCentreOfEntity(Platform)) == EAINavMoveResult::NAV_MOVE_SUCCESS;
	}
}

bool AvHAIPlayer::NewRidePlatformMove(AvHAIPath* Path, const edict_t* Platform, const Vector& DesiredStop)
{
	if (!Path || !Path->IsValidPath()) { return false; }
	if (!UTIL_IsEdictActive(Platform)) { return false; }

	Vector LiftPosition = UTIL_GetCentreOfEntity(Platform);

	bool bLiftHasReachedDestination = false;

	const DynamicMapObject* PlatformRef = AIMAP_GetDynamicObjectByEdict(Platform);

	if (!PlatformRef || !PlatformRef->IsValid()) { return false; }

	const DynamicMapObjectStop* CurrentLiftStop = PlatformRef->GetCurrentStop();
	const DynamicMapObjectStop* PreviousLiftStop = PlatformRef->GetPreviousStop();

	if (!CurrentLiftStop) { return false; }

	const bool bCurrentStopIsDisembarkStop = vEquals(CurrentLiftStop->StopLocation, DesiredStop, 5.0f);
	const bool bPreviousStopIsDisembarkStop = PreviousLiftStop && vEquals(PreviousLiftStop->StopLocation, DesiredStop, 5.0f);

	// Check if we should get off the platform now
	if (bCurrentStopIsDisembarkStop || bPreviousStopIsDisembarkStop)
	{
		const DynamicMapObjectStop* DisembarkStop = bCurrentStopIsDisembarkStop ? CurrentLiftStop : PreviousLiftStop;

		bool bLiftHasReachedDestination = (vDist3DSq(LiftPosition, DisembarkStop->StopLocation) < sqrf(32.0f));

		if (bLiftHasReachedDestination)
		{
			Vector DisembarkPoint = AIMAP_GetNearestPlatformDisembarkPoint(GetNavProfile(), GetLocation(), PlatformRef);
			return MoveToWithoutNav(DisembarkPoint) == EAINavMoveResult::NAV_MOVE_SUCCESS;
		}
	}

	// We are riding a moving platform, stay away from the edges so we don't get clipped by something and fall off
	if (PlatformRef->State == EAIDynamicMapObjectState::OBJECTSTATE_MOVING || PlatformRef->State == EAIDynamicMapObjectState::OBJECTSTATE_PREPARING)
	{
		float DistToCentre = vDist2DSq(GetLocation(), LiftPosition);
		// Check we're not already in the middle, we might be on a really small moving platform
		if (DistToCentre > sqrf(8.0f))
		{
			Vector NearestEdgePoint = vClampPointToAABBEdge2D(GetLocation(), Platform->v.absmin, Platform->v.absmax);
			float DistToEdge = vDist2DSq(NearestEdgePoint, GetLocation());
			// We're a little too close to the edge of the lift, we might fall off. Move to the centre a bit
			if (DistToEdge < sqrf(GetPlayerRadius()))
			{
				return MoveToWithoutNav(LiftPosition) == EAINavMoveResult::NAV_MOVE_SUCCESS;
			}
		}

		return true;
	}

	const Vector BotLocation = GetLocation();

	// Lift is stopped, we need to reactivate it

	const DynamicMapObject* Trigger = AIMAP_GetTriggerReachableFromPlatform(PlatformRef, BotLocation.z, LiftPosition);

	if (Trigger && Trigger->IsValid())
	{
		// Self-activated platform, move to the centre
		if (Trigger == PlatformRef)
		{
			NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(LiftPosition - BotLocation);
			return true;
		}

		if (IsPlayerInUseRange(Edict, Trigger->Edict))
		{
			UseObject(Trigger->Edict, false);
			return true;
		}

		NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(UTIL_GetCentreOfEntity(Trigger->Edict) - BotLocation);
		NextFrameMovementInput.RequiredLookLocation = UTIL_GetCentreOfEntity(Trigger->Edict);

		return true;
	}

	const Vector DisembarkPoint = AIMAP_GetNearestPlatformDisembarkPoint(GetNavProfile(), BotLocation, PlatformRef);

	if (!vIsZero(DisembarkPoint))
	{
		const DynamicMapObject* DisembarkTrigger = AIMAP_GetBestTriggerForObject(GetNavProfile(), PlatformRef, DisembarkPoint);

		if (DisembarkTrigger && DisembarkTrigger->IsValid())
		{
			NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(DisembarkPoint - BotLocation);
			return true;
		}
	}

	// We couldn't find a trigger within reach and have nowhere to get off. Use our telepathic powers to re-activate it so we're not trapped forever
	for (auto it = PlatformRef->Triggers.begin(); it != PlatformRef->Triggers.end(); it++)
	{
		const DynamicMapObject* ThisTrigger = AIMAP_GetDynamicObjectByEdict(*it);

		if (!ThisTrigger || !ThisTrigger->IsValid()) { continue; }

		if (ThisTrigger->bIsActive && ThisTrigger->State == EAIDynamicMapObjectState::OBJECTSTATE_IDLE)
		{
			AIMAP_ForceActivateTrigger(this, ThisTrigger);
			return true;
		}
	}

	return false;
}

bool AvHAIPlayer::NewWallClimbMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();
	const NavAgentProfile* NavProfile = GetNavProfile();
	const Vector CurrentFloorPosition = UTIL_GetFloorUnderEntity(Edict);
	const Vector BottomOfHitbox = GetBottomOfHitbox();
	const Vector CurrentPosition = GetLocation();

	if (IsPlayerSkulk(Edict))
	{
		// We always crouch as skulk in NextMove, so we reverse it here so we can wall-stick
		NextFrameMovementInput.bShouldCrouch = false;
	}

	if (AINAV_IsPointDirectlyReachable(NavProfile, CurrentFloorPosition, CurrentPathNode->ToLocation, 5.0f))
	{
		Vector PointOnMoveLine = vClosestPointOnLine2D(CurrentPathNode->FromLocation, CurrentPathNode->ToLocation, CurrentFloorPosition);

		if (vEquals2D(PointOnMoveLine, CurrentPathNode->ToLocation, 4.0f))
		{
			if (IsPlayerSkulk(Edict))
			{
				NextFrameMovementInput.bShouldCrouch = true;
			}

			NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - CurrentFloorPosition);

			return true;
		}
	}

	Vector vForward = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - CurrentPathNode->FromLocation);
	Vector ClimbAngle = UTIL_GetVectorNormal(Vector(CurrentPathNode->ToLocation.x, CurrentPathNode->ToLocation.y, CurrentPathNode->RequiredClimbZ) - CurrentPosition);

	TraceResult SurfaceCheck;

	UTIL_TraceHull(CurrentPosition - Vector(0.0f, 0.0f, 1.0f), CurrentPosition + Vector(0.0f, 0.0f, 5.0f), ignore_monsters, head_hull, Edict->v.pContainingEntity, &SurfaceCheck);

	const Vector CeilNormal = SurfaceCheck.vecPlaneNormal;

	bool bIsUnderClimbing = false;

	bool bClimbingUnderway = ((BottomOfHitbox.z - CurrentPathNode->FromLocation.z) >= 32.0f) && IsWallSticking();

	if (CurrentPosition.z < (CurrentPathNode->RequiredClimbZ - 10.0f) && !IsOnGround() && bClimbingUnderway)
	{
		bIsUnderClimbing = (SurfaceCheck.flFraction < 1.0f && UTIL_GetDotProduct(ClimbAngle, CeilNormal) < 0.0f);
	}

	if (bIsUnderClimbing)
	{
		NextFrameMovementInput.bShouldWalk = true;
		vForward = (UTIL_GetDotProduct2D(vForward, CeilNormal) > 0.0f) ? vForward : -vForward;

	}

	Vector vRight = UTIL_GetVectorNormal(UTIL_GetCrossProduct(vForward, UP_VECTOR));

	NextFrameMovementInput.DesiredMoveDirection = vForward;

	Vector CheckLine = CurrentPathNode->FromLocation + (vForward * 1000.0f);

	float DistFromLine = vDistanceFromLine2D(CurrentPathNode->FromLocation, CheckLine, CurrentPosition);

	// Draw an imaginary 2D line between from and to movement, and make sure we're aligned. If we've drifted off to one side, readjust.
	if (DistFromLine > 18.0f)
	{
		float modifier = (float)vPointOnLine(CurrentPathNode->FromLocation, CheckLine, CurrentPosition);

		NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(NextFrameMovementInput.DesiredMoveDirection + (vRight * modifier));
	}

	const Vector EyePosition = GetEyePosition();

	float ZDiff = fabs(CurrentPosition.z - CurrentPathNode->RequiredClimbZ);
	Vector AdjustedTargetLocation = CurrentPathNode->ToLocation + (UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - CurrentPathNode->FromLocation) * 1000.0f);
	Vector DirectAheadView = EyePosition + (UTIL_GetVectorNormal2D(AdjustedTargetLocation - EyePosition) * 100.0f);

	Vector LookLocation = g_vecZero;

	if (ZDiff < 1.0f)
	{
		LookLocation = DirectAheadView;
	}
	else
	{
		// Don't look up/down quite so much as we reach the desired height so we slow down a bit, reduces the chance of over-shooting and climbing right over a vent
		if (CurrentPosition.z > CurrentPathNode->RequiredClimbZ)
		{
			if (ZDiff > 16.0f)
			{
				ClimbAngle = ClimbAngle - (2.0f * (UTIL_GetDotProduct(ClimbAngle, UP_VECTOR) * ClimbAngle));
				LookLocation = EyePosition + (ClimbAngle * 100.0f);
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
				LookLocation = EyePosition + vForward;
				LookLocation.z = CurrentPathNode->ToLocation.z + 100.0f;
			}
			else
			{
				if (bClimbingUnderway)
				{
					LookLocation = EyePosition + (ClimbAngle * 100.0f);
				}
				else
				{
					LookLocation = EyePosition + vForward;
					LookLocation.z = CurrentPathNode->RequiredClimbZ;
				}
			}
		}
	}

	if (IsWallSticking() && !bIsUnderClimbing)
	{
		Vector RightDir = UTIL_GetCrossProduct(vForward, UP_VECTOR);

		Vector LeftCheckStart = CurrentPosition - (RightDir * (GetPlayerRadius() + 2.0f));
		Vector LeftCheckEnd = LeftCheckStart + Vector(0.0f, 0.0f, 50.0f);

		Vector RightCheckStart = CurrentPosition + (RightDir * (GetPlayerRadius() + 2.0f));
		Vector RightCheckEnd = RightCheckStart + Vector(0.0f, 0.0f, 50.0f);

		if (!UTIL_QuickTrace(Edict, LeftCheckStart, LeftCheckEnd))
		{
			if (UTIL_QuickTrace(Edict, RightCheckStart, RightCheckEnd))
			{
				NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(vForward + RightDir);
			}
		}
		else if (!UTIL_QuickTrace(Edict, RightCheckStart, RightCheckEnd))
		{
			NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(vForward - RightDir);
		}
	}

	NextFrameMovementInput.RequiredLookLocation = LookLocation;

	return true;
}

bool AvHAIPlayer::NewPhaseGateMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	if (IsOnLadder())
	{
		Jump(false);
	}

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();

	const AvHAIBuildableStructure* UsePhaseGate = AITAC_GetStructureFromEdict(CurrentPathNode->MovementObject);

	if (!UsePhaseGate || !UsePhaseGate->IsValid()) { return true; }

	if (IsPlayerInUseRange(Edict, UsePhaseGate->Edict))
	{
		NextFrameMovementInput.RequiredLookLocation = UsePhaseGate->Location;
		NextFrameMovementInput.DesiredMoveDirection = g_vecZero;
		UseObject(UsePhaseGate->Edict, false);

		if (vDist2DSq(GetLocation(), UsePhaseGate->Location) < sqrf(16.0f))
		{
			NextFrameMovementInput.DesiredMoveDirection = UTIL_GetForwardVector2D(Edict->v.angles);
		}

		return true;
	}
	else
	{
		NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(UsePhaseGate->Location - GetLocation());
	}

	return true;
}

void AvHAIMovementInput::GenerateMovementOutputs(const Vector& CurrentViewAngles, float MaxSpeed)
{
	ClearMovementOutputs();

	if (bShouldCrouch)
	{
		Button |= IN_DUCK;
	}

	float BotSpeed = MaxSpeed;

	if (bShouldWalk)
	{
		BotSpeed *= 0.4f;
		Button |= IN_WALK;
	}

	if (vIsZero(DesiredMoveDirection)) { return; }

	UTIL_NormalizeVector2D(&DesiredMoveDirection);

	float CurrentYaw = CurrentViewAngles.y;
	float MoveDelta = UTIL_VecToAngles(DesiredMoveDirection).y;
	float AngleDelta = CurrentYaw - MoveDelta;

	if (AngleDelta < -180.0f)
	{
		AngleDelta += 360.0f;
	}
	else if (AngleDelta > 180.0f)
	{
		AngleDelta -= 360.0f;
	}

	if (AngleDelta >= -22.5f && AngleDelta < 22.5f)
	{
		ForwardMove = BotSpeed;
		SideMove = 0.0f;
		Button |= IN_FORWARD;
	}
	else if (AngleDelta >= 22.5f && AngleDelta < 67.5f)
	{
		ForwardMove = BotSpeed;
		SideMove = BotSpeed;
		Button |= IN_FORWARD;
		Button |= IN_MOVERIGHT;
	}
	else if (AngleDelta >= 67.5f && AngleDelta < 112.5f)
	{
		ForwardMove = 0.0f;
		SideMove = BotSpeed;
		Button |= IN_MOVERIGHT;
	}
	else if (AngleDelta >= 112.5f && AngleDelta < 157.5f)
	{
		ForwardMove = -BotSpeed;
		SideMove = BotSpeed;
		Button |= IN_BACK;
		Button |= IN_MOVERIGHT;
	}
	else if (AngleDelta >= 157.5f || AngleDelta <= -157.5f)
	{
		ForwardMove = -BotSpeed;
		SideMove = 0.0f;
		Button |= IN_BACK;
	}
	else if (AngleDelta >= -157.5f && AngleDelta < -112.5f)
	{
		ForwardMove = -BotSpeed;
		SideMove = -BotSpeed;
		Button |= IN_BACK;
		Button |= IN_MOVELEFT;
	}
	else if (AngleDelta >= -112.5f && AngleDelta < -67.5f)
	{
		ForwardMove = 0.0f;
		SideMove = -BotSpeed;
		Button |= IN_MOVELEFT;
	}
	else if (AngleDelta >= -67.5f && AngleDelta < -22.5f)
	{
		ForwardMove = BotSpeed;
		SideMove = -BotSpeed;
		Button |= IN_FORWARD;
		Button |= IN_MOVELEFT;
	}
}

Vector GetVisiblePointOnPlayerFromObserver(edict_t* Observer, edict_t* TargetPlayer)
{
	Vector TargetCentre = UTIL_GetCentreOfEntity(TargetPlayer);

	TraceResult hit;
	UTIL_TraceLine(GetPlayerEyePosition(Observer), TargetCentre, ignore_monsters, ignore_glass, Observer->v.pContainingEntity, &hit);

	if (hit.flFraction >= 1.0f) { return TargetCentre; }

	AvHUser3 TargetClass = (AvHUser3)TargetPlayer->v.iuser3;

	// Only check the head and feet if we're not a short-arse (i.e. marine, fade or onos)
	if (TargetClass == AVH_USER3_MARINE_PLAYER || TargetClass == AVH_USER3_ALIEN_PLAYER4 || TargetClass == AVH_USER3_ALIEN_PLAYER5)
	{

		UTIL_TraceLine(GetPlayerEyePosition(Observer), GetPlayerEyePosition(TargetPlayer), ignore_monsters, ignore_glass, Observer->v.pContainingEntity, &hit);

		if (hit.flFraction >= 1.0f) { return GetPlayerEyePosition(TargetPlayer); }

		UTIL_TraceLine(GetPlayerEyePosition(Observer), GetPlayerBottomOfCollisionHull(TargetPlayer) + Vector(0.0f, 0.0f, 5.0f), ignore_monsters, ignore_glass, Observer->v.pContainingEntity, &hit);

		if (hit.flFraction >= 1.0f) { return GetPlayerBottomOfCollisionHull(TargetPlayer) + Vector(0.0f, 0.0f, 5.0f); }
	}

	// Skulks and Onos are long bois, so check to make sure they don't have their boots or snoots poking out round a corner...
	if (TargetClass == AVH_USER3_ALIEN_PLAYER1 || TargetClass == AVH_USER3_ALIEN_PLAYER5)
	{
		float Length = (TargetClass == AVH_USER3_ALIEN_PLAYER1) ? 55.0f : 110.0f;

		Vector ForwardVector = UTIL_GetForwardVector(TargetPlayer->v.angles);

		Vector MinLoc = TargetCentre - (ForwardVector * Length);

		UTIL_TraceLine(GetPlayerEyePosition(Observer), MinLoc, ignore_monsters, ignore_glass, Observer->v.pContainingEntity, &hit);

		if (hit.flFraction >= 1.0f) { return MinLoc; }

		Vector MaxLoc = TargetCentre + (ForwardVector * Length);

		UTIL_TraceLine(GetPlayerEyePosition(Observer), MaxLoc, ignore_monsters, ignore_glass, Observer->v.pContainingEntity, &hit);

		return (hit.flFraction >= 1.0f) ? MaxLoc : ZERO_VECTOR;
	}

	return ZERO_VECTOR;
}
