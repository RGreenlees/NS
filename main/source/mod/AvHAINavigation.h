//
// EvoBot - Neoptolemus' Natural Selection bot, based on Botman's HPB bot template
//
// bot_navigation.h
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
#include "AvHAIPlayer.h"
#include "AvHAIConstants.h"
#include "AvHAIMapData.h"

constexpr auto MIN_PATH_RECALC_TIME = 0.33f; // How frequently can a bot recalculate its path? Default to max 3 times per second
constexpr auto MAX_BOT_STUCK_TIME = 30.0f; // How long a bot can be stuck, unable to move, before giving up and suiciding

// What should the lerk do for this movement?
enum LerkFlightBehaviour
{
	FLIGHT_DROP = 0, // Drop like a stone
	FLIGHT_GLIDE, // Hold jump to glide
	FLIGHT_FLAP // Rapidly tap jump to flap and speed up
};

bool AINAV_IsPointDirectlyReachable(const NavAgentProfile* NavProfile, const Vector& FromLocation, const Vector& ToLocation, float MaxAcceptableDistance = max_ai_use_reach);
bool AINAV_IsPointReachable(const NavAgentProfile* NavProfile, const Vector& FromLocation, const Vector& ToLocation, float MaxAcceptableDistance = max_ai_use_reach);
Vector AINAV_FindClosestNavigablePointTo(const NavAgentProfile* NavProfile, const Vector& FromLocation, const Vector& ToLocation);

AvHPlayer* AINAV_GetPlayerRidingOnBot(AvHAIPlayer* AIPlayer);

// Checks the bot's current path and sees if there are any necessary movement tasks to progress (e.g. press button, break something)
// Returns true if a movement task was required and added.
bool AINAV_CheckAndAddRequiredMovementTasks(const NavAgentProfile* NavProfile, const AvHAIPath* Path, AvHAIMoveTask& NewMoveTask);

bool AINAV_CheckMapObjectForMovementTasks(const NavAgentProfile* NavProfile, const AvHAIPathNode* ImpactedPathNode, const DynamicMapObject* ImpactingObject, AvHAIMoveTask& NewMoveTask);
bool AINAV_CheckPlatformForMovementTasks(const NavAgentProfile* NavProfile, const AvHAIPathNode* ImpactedPathNode, const DynamicMapObject* Platform, AvHAIMoveTask& NewMoveTask);

bool AINAV_AddTriggerMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const DynamicMapObject* Trigger, const DynamicMapObject* TriggerTarget, AvHAIMoveTask& NewTask);
bool AINAV_AddPickupMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* ThingToPickup, const DynamicMapObject* TriggerToActivate, AvHAIMoveTask& NewTask);
bool AINAV_AddMoveMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const Vector& MoveLocation, const DynamicMapObject* TriggerToActivate, AvHAIMoveTask& NewTask);
bool AINAV_AddTouchMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* EntityToTouch, const DynamicMapObject* TriggerToActivate, AvHAIMoveTask& NewTask);
bool AINAV_AddUseMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* EntityToUse, const DynamicMapObject* TriggerToActivate, AvHAIMoveTask& NewTask);
bool AINAV_AddBreakMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* EntityToBreak, const DynamicMapObject* TriggerToActivate, AvHAIMoveTask& NewTask);
bool AINAV_AddWeldMovementTask(const NavAgentProfile* NavProfile, const Vector& StartPoint, const edict_t* EntityToWeld, const DynamicMapObject* TriggerToActivate, AvHAIMoveTask& NewTask);

bool AINAV_HasBotCompletedPathPoint(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_HasBotCompletedWalkMove(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_HasBotCompletedLiftMove(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_HasBotCompletedWallClimbMove(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_HasBotCompletedJumpMove(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_HasBotCompletedLadderMove(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_HasBotCompletedObstacleMove(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_HasBotCompletedPhaseGateMove(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);

Vector AINAV_AdjustPointForPathfinding(const NavAgentProfile* NavProfile, const Vector& Point);

bool AINAV_FindPathClosestToPoint(const NavAgentProfile* NavProfile, const Vector FromLocation, const Vector ToLocation, AvHAIPath* ResultPath, float MaxAcceptableDistance);

Vector AINAV_FindNewPathStartPoint(const NavAgentProfile* NavProfile, const AvHAIPath* ExistingPath, const Vector& DesiredStartPoint, const Vector& Destination);

Vector AINAV_GetNearestPlatformDisembarkPoint(const NavAgentProfile* NavProfile, edict_t* Rider, DynamicMapObject* LiftReference);

bool AINAV_IsBotOffPathNode(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* PathNode);
bool AINAV_IsBotOffWalkNode(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* PathNode);
bool AINAV_IsBotOffLadderNode(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* PathNode);
bool AINAV_IsBotOffFallNode(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* PathNode);
bool AINAV_IsBotOffJumpNode(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* PathNode);
bool AINAV_IsBotOffPlatformNode(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* PathNode);
bool AINAV_IsBotOffPhaseGateNode(const AvHAIPlayer* AIPlayer, const AvHAIPathNode* PathNode);

bool AINAV_NextMove(AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_NextSwimMove(AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);

bool AINAV_NewGroundMove(AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_NewFallMove(AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_NewJumpMove(AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_NewLadderMove(AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_NewPlatformMove(AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_NewPhaseGateMove(AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);
bool AINAV_NewMountLadderMove(AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode, const AvHAIPathNode* NextPathNode);


EAINavMoveResult AINAV_FollowPath(AvHAIPlayer* AIPlayer, AvHAIPath* Path);

void AINAV_HandlePlayerAvoidance(AvHAIPlayer* AIPlayer, const AvHAIPathNode* CurrentPathNode);

Vector AINAV_GetFurthestVisiblePointOnPath(const Vector& ViewerLocation, const AvHAIPath* Path);

EAINavMoveResult AINAV_ProgressMoveTask(AvHAIPlayer* AIPlayer, AvHAIMoveTask* MoveTask, AvHAIMovementInput& OutMovementInputs);

// From the given start point, determine how high up the bot needs to climb to get to climb end. Will allow the bot to climb over railings
float AINAV_FindZHeightForClimb(const Vector ClimbStart, const Vector ClimbEnd, const int HullNum);


// Returns true if the bot is considered to have strayed off the path (e.g. missed a jump and fallen)

bool IsBotOffLadderNode(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);
bool IsBotOffClimbNode(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);
bool IsBotOffPhaseGateNode(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);
bool IsBotOffObstacleNode(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);

void WallClimbMove(AvHAIPlayer* pBot, const Vector StartPoint, const Vector EndPoint, float RequiredClimbHeight);
void BlinkClimbMove(AvHAIPlayer* pBot, const Vector StartPoint, const Vector EndPoint, float RequiredClimbHeight);
// Called by NewMove, determines the movement direction and inputs required to use a phase gate to reach end point
void PhaseGateMove(AvHAIPlayer* pBot, const Vector StartPoint, const Vector EndPoint);


// Used by Detour for the FindRandomPointInCircle type functions
static float frand();


#endif // BOT_NAVIGATION_H

