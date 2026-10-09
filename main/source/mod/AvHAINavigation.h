//
// EvoBot - Neoptolemus' Natural Selection bot, based on Botman's HPB bot template
//
// AvHAINavigation.h
//
// Handles all bot path finding and movement
//

#pragma once
#ifndef AVH_AI_NAVIGATION_H
#define AVH_AI_NAVIGATION_H

#include <unordered_map>

#include "DetourStatus.h"
#include "DetourNavMeshQuery.h"
#include "DetourTileCache.h"

#include "AvHAIConstants.h"
#include "AvHAINavConstants.h"
#include "AvHAIMath.h"

constexpr auto MIN_PATH_RECALC_TIME = 0.33f; // How frequently can a bot recalculate its path? Default to max 3 times per second
constexpr auto MAX_BOT_STUCK_TIME = 30.0f; // How long a bot can be stuck, unable to move, before giving up and suiciding

// What should the lerk do for this movement?
enum LerkFlightBehaviour
{
	FLIGHT_DROP = 0, // Drop like a stone
	FLIGHT_GLIDE, // Hold jump to glide
	FLIGHT_FLAP // Rapidly tap jump to flap and speed up
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

// Bot path node. A path will be several of these strung together to lead the bot to its destination
struct AvHAIPathNode
{
	Vector FromLocation = ZERO_VECTOR; // Location to move from
	Vector ToLocation = ZERO_VECTOR; // Location to move to
	float RequiredClimbZ = 0.0f; // If climbing a up ladder or wall, how high should they aim to get before dismounting.
	EAINavMovementFlag MovementFlag = EAINavMovementFlag::NAV_FLAG_DISABLED; // Is this a ladder movement, wall climb, walk etc
	EAINavArea MovementArea = EAINavArea::NAV_AREA_NULL; // Is this a crouch area, normal walking area etc
	unsigned int FromMeshPoly = 0; // The nav mesh poly the start point resides on
	unsigned int ToMeshPoly = 0; // The nav mesh poly the end point resides on
	edict_t* MovementObject = nullptr;

	bool IsValidMove() const
	{
		return !vEquals(FromLocation, ToLocation) && MovementFlag != EAINavMovementFlag::NAV_FLAG_DISABLED && MovementArea != EAINavArea::NAV_AREA_NULL;
	}

	// Returns true if this movement requires careful alignment from start to end point to avoid screwing it up
	bool IsPrecisionMove() const
	{
		EAINavMovementFlag PrecisionFlags = (EAINavMovementFlag::NAV_FLAG_WALLCLIMB | EAINavMovementFlag::NAV_FLAG_LADDER | EAINavMovementFlag::NAV_FLAG_JUMP | EAINavMovementFlag::NAV_FLAG_FALL);
		return EnumHasAnyFlags(MovementFlag, PrecisionFlags);
	}

	bool IsTeleportMove() const
	{
		EAINavMovementFlag TeleportFlags = (EAINavMovementFlag::NAV_FLAG_TELEPORT | EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM1 | EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM1);
		return EnumHasAnyFlags(MovementFlag, TeleportFlags);
	}
};
typedef std::vector<AvHAIPathNode> AvHAIPathList;
typedef std::vector<const AvHAIPathNode*> AvHAIPathNodeList;
typedef std::vector<AvHAIPathNode*> AvHAIMutablePathNodeList;

struct AvHAIPath
{
	AvHAIPathList PathNodes;

	Vector DesiredDestination = ZERO_VECTOR;
	EAINavMovementFlag RequiredMoveFlags = EAINavMovementFlag::NAV_FLAG_NONE;
	uint32 CurrentNodeIndex = 0;
	EAINavMeshIndex UsedNavMesh = EAINavMeshIndex::NAV_MESH_INVALID;

	void Clear()
	{
		PathNodes.clear();
		RequiredMoveFlags = EAINavMovementFlag::NAV_FLAG_NONE;
		CurrentNodeIndex = 0;
		DesiredDestination = ZERO_VECTOR;
		UsedNavMesh = EAINavMeshIndex::NAV_MESH_INVALID;
	}

	bool IsValidPath() const
	{
		return UsedNavMesh != EAINavMeshIndex::NAV_MESH_INVALID && !vIsZero(DesiredDestination) && CurrentNodeIndex < PathNodes.size();
	}

	const AvHAIPathNode* GetCurrentPathNode() const
	{
		if (!IsValidPath()) { return nullptr; }

		return &PathNodes[CurrentNodeIndex];
	}

	void JumpToPathNode(const AvHAIPathNode* PathNode)
	{
		if (!PathNode) { return; }

		for (int32 i = 0; i < GetPathSize(); i++)
		{
			if (PathNode == &PathNodes[i])
			{
				CurrentNodeIndex = i;
			}
		}
	}

	AvHAIPathNodeList GetFuturePathNodeList() const
	{
		AvHAIPathNodeList Result;

		for (int32 i = CurrentNodeIndex + 1; i < GetPathSize(); i++)
		{
			if (!PathNodes[i].IsValidMove()) { break; }

			Result.push_back(&PathNodes[i]);
		}

		return Result;
	}

	AvHAIMutablePathNodeList GetMutableFuturePathNodeList()
	{
		AvHAIMutablePathNodeList Result;

		for (int32 i = CurrentNodeIndex + 1; i < GetPathSize(); i++)
		{
			if (!PathNodes[i].IsValidMove()) { break; }

			Result.push_back(&PathNodes[i]);
		}

		return Result;
	}

	const AvHAIPathNode* GetNextPathNode() const
	{
		if (!IsValidPath()) { return nullptr; }

		if (CurrentNodeIndex + 1 < PathNodes.size())
		{
			return &PathNodes[CurrentNodeIndex + 1];
		}

		return nullptr;
	}

	const AvHAIPathNode* GetPreviousPathNode() const
	{
		if (IsValidPath() || CurrentNodeIndex == 0) { return nullptr; }

		if (CurrentNodeIndex - 1 < PathNodes.size())
		{
			return &PathNodes[CurrentNodeIndex - 1];
		}

		return nullptr;
	}

	const AvHAIPathNode* GetNodeAtIndex(int32 Index) const
	{
		if (Index > PathNodes.size() || Index < 0) { return nullptr; }

		return &PathNodes[Index];
	}

	AvHAIPathNode* GetNodeAtIndex_Mutable(int32 Index)
	{
		if (Index > PathNodes.size() || Index < 0) { return nullptr; }

		return &PathNodes[Index];
	}

	int32 GetPathSize() const
	{
		return PathNodes.size();
	}

	Vector GetFinalDestination() const
	{
		if (!IsValidPath()) { return ZERO_VECTOR; }

		return PathNodes[PathNodes.size() - 1].ToLocation;
	}

	void OnPathNodeComplete()
	{
		if (!IsValidPath()) { return; }

		if (CurrentNodeIndex < GetPathSize() - 1)
		{
			CurrentNodeIndex++;
		}
	}
};

struct AvHAIMoveTask
{
	EAIMovementTaskType TaskType = EAIMovementTaskType::MOVE_TASK_NONE;
	Vector TaskLocation = ZERO_VECTOR;
	const edict_t* TaskTarget = nullptr;
	const edict_t* TriggerToActivate = nullptr;
	AvHAIPath TaskPath;

	void Clear()
	{
		TaskPath.Clear();
		TaskType = EAIMovementTaskType::MOVE_TASK_NONE;
		TaskLocation = ZERO_VECTOR;
		TaskTarget = nullptr;
		TriggerToActivate = nullptr;
	}

	bool HasPath() const
	{
		return TaskPath.IsValidPath();
	}

	bool IsValid() const
	{
		return TaskType != EAIMovementTaskType::MOVE_TASK_NONE;
	}
};
typedef std::vector<AvHAIMoveTask> AIMoveTaskList;

// Contains the bot's current navigation info, such as current path
struct AvHAINavStatus
{
	unsigned int CurrentPoly = 0; // Which nav mesh poly the bot is currently on

	float LandedTime = 0.0f; // When the bot last landed after a fall/jump.
	bool bHasAttemptedJump = false; // Last frame, the bot tried a jump. If the bot is still on the ground, it probably tried to jump in a vent or something

	NavAgentProfile NavProfile;

	AIMoveTaskList MovementTasks;

	void Reset()
	{
		MovementTasks.clear();
	}

	void ClearPath()
	{
		MovementTasks.clear();
	}
};



bool AINAV_IsPointDirectlyReachable(const NavAgentProfile* NavProfile, const Vector& FromLocation, const Vector& ToLocation, float MaxAcceptableDistance = max_ai_use_reach);
bool AINAV_IsPointReachable(const NavAgentProfile* NavProfile, const Vector& FromLocation, const Vector& ToLocation, float MaxAcceptableDistance = max_ai_use_reach);
Vector AINAV_FindClosestNavigablePointTo(const NavAgentProfile* NavProfile, const Vector& FromLocation, const Vector& ToLocation);

AvHPlayer* AINAV_GetPlayerRidingOnBot(const edict_t* AIPlayer);

// Checks the bot's current path and sees if there are any necessary movement tasks to progress (e.g. press button, break something)
// Returns true if a movement task was required and added.
bool AINAV_CheckAndAddRequiredMovementTasks(const NavAgentProfile* NavProfile, const AvHAIPath* Path, AvHAIMoveTask& NewMoveTask);

bool AINAV_CheckMapObjectForMovementTasks(const NavAgentProfile* NavProfile, const AvHAIPathNode* ImpactedPathNode, const edict_t* ImpactingObject, AvHAIMoveTask& NewMoveTask);
bool AINAV_CheckPlatformForMovementTasks(const NavAgentProfile* NavProfile, const AvHAIPathNode* ImpactedPathNode, const edict_t* Platform, AvHAIMoveTask& NewMoveTask);

Vector AINAV_GetLadderMountPoint(const edict_t* MountLadder, const Vector StartPoint);

Vector AINAV_AdjustPointForPathfinding(const NavAgentProfile* NavProfile, const Vector& Point);

bool AINAV_FindPathClosestToPoint(const NavAgentProfile* NavProfile, const Vector FromLocation, const Vector ToLocation, AvHAIPath* ResultPath, float MaxAcceptableDistance);

Vector AINAV_FindNewPathStartPoint(const NavAgentProfile* NavProfile, const AvHAIPath* ExistingPath, const Vector& DesiredStartPoint, const Vector& Destination);

Vector AINAV_GetNearestPlatformDisembarkPoint(const NavAgentProfile* NavProfile, const edict_t* Rider, const edict_t* LiftReference);

bool AINAV_AddTriggerMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* Trigger, const edict_t* TriggerTarget, AvHAIMoveTask& NewTask);
bool AINAV_AddPickupMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* ThingToPickup, const edict_t* TriggerToActivate, AvHAIMoveTask& NewTask);
bool AINAV_AddMoveMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const Vector& MoveLocation, const edict_t* TriggerToActivate, AvHAIMoveTask& NewTask);
bool AINAV_AddTouchMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* EntityToTouch, const edict_t* TriggerToActivate, AvHAIMoveTask& NewTask);
bool AINAV_AddUseMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* EntityToUse, const edict_t* TriggerToActivate, AvHAIMoveTask& NewTask);
bool AINAV_AddBreakMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* EntityToBreak, const edict_t* TriggerToActivate, AvHAIMoveTask& NewTask);
bool AINAV_AddWeldMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* EntityToWeld, const edict_t* TriggerToActivate, AvHAIMoveTask& NewTask);

bool AINAV_IsPathPointComplete(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_IsWalkMoveComplete(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_IsFallMoveComplete(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_IsLiftMoveComplete(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_IsWallClimbMoveComplete(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_IsJumpMoveComplete(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_IsLadderMoveComplete(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_IsObstacleMoveComplete(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_IsPhaseGateMoveComplete(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);

bool AINAV_IsOffPathNode(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* PathNode);
bool AINAV_IsOffWalkNode(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* PathNode);
bool AINAV_IsOffLadderNode(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* PathNode);
bool AINAV_IsOffFallNode(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* PathNode);
bool AINAV_IsOffJumpNode(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* PathNode);
bool AINAV_IsOffPlatformNode(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* PathNode);
bool AINAV_IsOffPhaseGateNode(const NavAgentProfile* NavProfile, const edict_t* AIPlayer, const AvHAIPathNode* PathNode);

Vector AINAV_GetFurthestVisiblePointOnPath(const Vector& ViewerLocation, const AvHAIPath* Path);

// From the given start point, determine how high up the bot needs to climb to get to climb end. Will allow the bot to climb over railings
float AINAV_FindZHeightForClimb(const Vector ClimbStart, const Vector ClimbEnd, const int HullNum);


#endif // BOT_NAVIGATION_H

