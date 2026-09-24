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

#include "DetourStatus.h"
#include "DetourNavMeshQuery.h"
#include "DetourTileCache.h"
#include "AvHAIPlayer.h"

constexpr auto MIN_PATH_RECALC_TIME = 0.33f; // How frequently can a bot recalculate its path? Default to max 3 times per second
constexpr auto MAX_BOT_STUCK_TIME = 30.0f; // How long a bot can be stuck, unable to move, before giving up and suiciding

// What should the lerk do for this movement?
enum LerkFlightBehaviour
{
	FLIGHT_DROP = 0, // Drop like a stone
	FLIGHT_GLIDE, // Hold jump to glide
	FLIGHT_FLAP // Rapidly tap jump to flap and speed up
};

void SetBaseNavProfile(AvHAIPlayer* pBot);
void UpdateBotMoveProfile(AvHAIPlayer* pBot, EAIMoveStyle MoveStyle);
void MarineUpdateBotMoveProfile(AvHAIPlayer* pBot, EAIMoveStyle MoveStyle);
void SkulkUpdateBotMoveProfile(AvHAIPlayer* pBot, EAIMoveStyle MoveStyle);
void GorgeUpdateBotMoveProfile(AvHAIPlayer* pBot, EAIMoveStyle MoveStyle);
void LerkUpdateBotMoveProfile(AvHAIPlayer* pBot, EAIMoveStyle MoveStyle);
void FadeUpdateBotMoveProfile(AvHAIPlayer* pBot, EAIMoveStyle MoveStyle);
void OnosUpdateBotMoveProfile(AvHAIPlayer* pBot, EAIMoveStyle MoveStyle);



// Roughly estimates the movement cost to move between FromLocation and ToLocation. Uses simple formula of distance between points x cost modifier for that movement
float UTIL_GetPathCostBetweenLocations(const nav_profile &NavProfile, const Vector FromLocation, const Vector ToLocation);

// Returns true is the bot is grounded, on the nav mesh, and close enough to the Destination to be considered at that point
bool BotIsAtLocation(const AvHAIPlayer* pBot, const Vector Destination);

// Sets the bot's desired movement direction and performs jumps/crouch/etc. to traverse the current path point
void NewMove(AvHAIPlayer* pBot);
// Returns true if the bot has completed the current movement along their path
bool HasBotReachedPathPoint(const AvHAIPlayer* pBot);
bool HasBotCompletedLadderMove(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);
bool HasBotCompletedWalkMove(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, SamplePolyAreas MoveArea, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag, SamplePolyAreas NextMoveArea);
bool HasBotCompletedFallMove(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);
bool HasBotCompletedClimbMove(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, float RequiredClimbHeight, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);
bool HasBotCompletedJumpMove(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);
bool HasBotCompletedPhaseGateMove(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);
bool HasBotCompletedLiftMove(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);
bool HasBotCompletedObstacleMove(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);

// Returns true if the bot is considered to have strayed off the path (e.g. missed a jump and fallen)
bool IsBotOffPath(const AvHAIPlayer* pBot);
bool IsBotOffLadderNode(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);
bool IsBotOffWalkNode(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);
bool IsBotOffFallNode(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);
bool IsBotOffClimbNode(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);
bool IsBotOffJumpNode(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);
bool IsBotOffPhaseGateNode(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);
bool IsBotOffLiftNode(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);
bool IsBotOffObstacleNode(const AvHAIPlayer* pBot, Vector MoveStart, Vector MoveEnd, Vector NextMoveDestination, SamplePolyFlags NextMoveFlag);

// Called by NewMove, determines the movement direction and inputs required to walk/crouch between start and end points
void GroundMove(AvHAIPlayer* pBot, const Vector StartPoint, const Vector EndPoint);
// Called by NewMove, determines the movement direction and inputs required to jump between start and end points
void JumpMove(AvHAIPlayer* pBot, const Vector StartPoint, const Vector EndPoint);
// Called by NewMove, determines movement direction and jump inputs to hop over obstructions (structures)
void BlockedMove(AvHAIPlayer* pBot, const Vector StartPoint, const Vector EndPoint);
// Called by NewMove, determines which structure is in the way and attacks it
void StructureBlockedMove(AvHAIPlayer* pBot, const Vector StartPoint, const Vector EndPoint);
// Called by NewMove, determines the movement direction and inputs required to drop down from start to end points
void FallMove(AvHAIPlayer* pBot, const Vector StartPoint, const Vector EndPoint);
// Called by NewMove, determines the movement direction and inputs required to climb a ladder to reach endpoint
void LadderMove(AvHAIPlayer* pBot, const Vector StartPoint, const Vector EndPoint, float RequiredClimbHeight, unsigned char NextArea);

// Called by NewMove, determines the movement direction and inputs required to climb a wall to reach endpoint
LerkFlightBehaviour BotFlightClimbMove(AvHAIPlayer* pBot, Vector FromLocation, Vector ToLocation, float RequiredZ);
void WallClimbMove(AvHAIPlayer* pBot, const Vector StartPoint, const Vector EndPoint, float RequiredClimbHeight);
void BlinkClimbMove(AvHAIPlayer* pBot, const Vector StartPoint, const Vector EndPoint, float RequiredClimbHeight);
// Called by NewMove, determines the movement direction and inputs required to use a phase gate to reach end point
void PhaseGateMove(AvHAIPlayer* pBot, const Vector StartPoint, const Vector EndPoint);
// Called by NewMove, determines the movement direction and inputs required to use a lift to reach an end point
void LiftMove(AvHAIPlayer* pBot, const Vector StartPoint, const Vector EndPoint);

DoorTrigger* UTIL_GetDoorTriggerByEntity(edict_t* TriggerEntity);

bool UTIL_TriggerHasBeenRecentlyActivated(edict_t* TriggerEntity);

// Directly calls the Use function on this trigger, regardless of where the bot is
void NAV_ForceActivateTrigger(AvHAIPlayer* pBot, DoorTrigger* TriggerRef);

// Will check for any func_breakable which might be in the way (e.g. window, vent) and make the bot aim and attack it to break it. Marines will switch to knife to break it.
void CheckAndHandleBreakableObstruction(AvHAIPlayer* pBot, const Vector MoveFrom, const Vector MoveTo, unsigned int MovementFlags);

void CheckAndHandleDoorObstruction(AvHAIPlayer* pBot);

DoorTrigger* UTIL_GetNearestDoorTrigger(const Vector Location, nav_door* Door, CBaseEntity* IgnoreTrigger, bool bCheckBlockedByDoor);
DoorTrigger* UTIL_GetNearestDoorTriggerFromLift(edict_t* LiftEdict, nav_door* Door, CBaseEntity* IgnoreTrigger);
bool UTIL_IsPathBlockedByDoor(const Vector StartLoc, const Vector EndLoc, edict_t* SearchDoor);

edict_t* UTIL_GetDoorBlockingPathPoint(AvHAIPlayer* pBot, bot_path_node* PathNode, edict_t* SearchDoor);
edict_t* UTIL_GetDoorBlockingPathPoint(const Vector FromLocation, const Vector ToLocation, const unsigned int MovementFlag, edict_t* SearchDoor);
edict_t* UTIL_GetBreakableBlockingPathPoint(AvHAIPlayer* pBot, bot_path_node* PathNode, edict_t* SearchBreakable);
edict_t* UTIL_GetBreakableBlockingPathPoint(AvHAIPlayer* pBot, const Vector FromLocation, const Vector ToLocation, const unsigned int MovementFlag, edict_t* SearchBreakable);


Vector UTIL_GetButtonFloorLocation(const Vector UserLocation, edict_t* ButtonEdict);

// Clears all tracking of a bot's stuck status
void ClearBotStuck(AvHAIPlayer* pBot);
// Clears all bot movement data, including the current path, their stuck status. Effectively stops all movement the bot is performing.
void ClearBotMovement(AvHAIPlayer* pBot);

// Called every bot frame (default is 60fps). Ensures the tile cache is updated after obstacles are placed
bool UTIL_UpdateTileCache();

Vector UTIL_GetNearestPointOnNavWall(AvHAIPlayer* pBot, const float MaxRadius);
Vector UTIL_GetNearestPointOnNavWall(const nav_profile& NavProfile, const Vector Location, const float MaxRadius);

/*	Places a temporary obstacle of the given height and radius on the mesh.Will modify that part of the nav mesh to be the given area.
	An example use case is to place an obstacle of area type SAMPLE_POLYAREA_OBSTRUCTION to mark where buildings are.
	Using DT_AREA_NULL will effectively cut a hole in the nav mesh, meaning it's no longer considered a valid mesh position.
*/
unsigned int UTIL_AddTemporaryObstacle(unsigned int NavMeshIndex, const Vector Location, float Radius, float Height, int area);
void UTIL_AddTemporaryObstacles(const Vector Location, float Radius, float Height, int area, unsigned int* ObstacleRefArray);
void UTIL_AddStructureTemporaryObstacles(AvHAIBuildableStructure* Structure);
void UTIL_RemoveStructureTemporaryObstacles(AvHAIBuildableStructure* Structure);

unsigned int UTIL_AddTemporaryBoxObstacle(Vector bMin, Vector bMax, int area);

/*	Removes the temporary obstacle from the mesh. The area will return to its default type (either walk or crouch).
	Removing a DT_AREA_NULL obstacle will "fill in" the hole again, making it traversable and considered a valid mesh position.
*/
void UTIL_RemoveTemporaryObstacle(unsigned int ObstacleRef);

void UTIL_RemoveTemporaryObstacles(unsigned int* ObstacleRefs);

void UTIL_AddOffMeshConnection(Vector StartLoc, Vector EndLoc, unsigned char area, unsigned int flags, bool bBiDirectional, AvHAIOffMeshConnection* RemoveConnectionDef);
void UTIL_RemoveOffMeshConnections(AvHAIOffMeshConnection* RemoveConnectionDef);


/*
	Safely aborts the current movement the bot is performing. Returns true if the bot has successfully aborted, and is ready to calculate a new path.

	The purpose of this is to avoid sudden path changes while on a ladder or performing a wall climb, which can cause the bot to get confused.

	NewDestination is where the bot wants to go now, so it can figure out how best to abort the current move.
*/
bool AbortCurrentMove(AvHAIPlayer* pBot, const Vector NewDestination);


// Will clear current path and recalculate it for the supplied destination
bool BotRecalcPath(AvHAIPlayer* pBot, const Vector Destination);

/*	Main function for instructing a bot to move to the destination, and what type of movement to favour. Should be called every frame you want the bot to move.
	Will handle path calculation, following the path, detecting if stuck and trying to unstick itself.
	Will only recalculate paths if it decides it needs to, so is safe to call every frame.
*/
bool MoveTo(AvHAIPlayer* pBot, const Vector Destination, const BotMoveStyle MoveStyle, const float MaxAcceptableDist = max_ai_use_reach);

void UpdateBotStuck(AvHAIPlayer* pBot);

// Used by the MoveTo command, handles the bot's movement and inputs to follow a path it has calculated for itself
void BotFollowPath(AvHAIPlayer* pBot);
void BotFollowFlightPath(AvHAIPlayer* pBot, bool bAllowSkip);
void BotFollowSwimPath(AvHAIPlayer* pBot);

void SkipAheadInFlightPath(AvHAIPlayer* pBot);


// Walks directly towards the destination. No path finding, just raw movement input. Will detect obstacles and try to jump/duck under them.
void MoveDirectlyTo(AvHAIPlayer* pBot, const Vector Destination);
void MoveToWithoutNav(AvHAIPlayer* pBot, const Vector Destination);

// Check if there are any players in our way and try to move around them. If we can't, then back up to let them through
void HandlePlayerAvoidance(AvHAIPlayer* pBot, const Vector MoveDestination);

Vector AdjustPointForPathfinding(const Vector Point);
Vector AdjustPointForPathfinding(const Vector Point, const nav_profile& NavProfile);

// Special path finding for lerks
dtStatus FindFlightPathToPoint(const nav_profile& NavProfile, Vector FromLocation, Vector ToLocation, vector<bot_path_node>& path, float MaxAcceptableDistance);

Vector UTIL_FindHighestSuccessfulTracePoint(const Vector TraceFrom, const Vector TargetPoint, const Vector NextPoint, const float IterationStep, const float MinIdealHeight, const float MaxHeight);

// Similar to FindPathToPoint, but you can specify a max acceptable distance for partial results. Will return a failure if it can't reach at least MaxAcceptableDistance away from the ToLocation
dtStatus FindPathClosestToPoint(AvHAIPlayer* pBot, const BotMoveStyle MoveStyle, const Vector ToLocation, vector<bot_path_node>& path, float MaxAcceptableDistance);
dtStatus FindPathClosestToPoint(const nav_profile& NavProfile, const Vector FromLocation, const Vector ToLocation, vector<bot_path_node>& path, float MaxAcceptableDistance);

dtStatus DEBUG_TestFindPath(const nav_profile& NavProfile, const Vector FromLocation, const Vector ToLocation, vector<bot_path_node>& path, float MaxAcceptableDistance);

// If the bot is stuck and off the path or nav mesh, this will try to find a point it can directly move towards to get it back on track
Vector FindClosestPointBackOnPath(AvHAIPlayer* pBot, Vector Destination);

Vector FindClosestNavigablePointToDestination(const nav_profile& NavProfile, const Vector FromLocation, const Vector ToLocation, float MaxAcceptableDistance);

// Will attempt to move directly towards MoveDestination while jumping/ducking as needed, and avoiding obstacles in the way
void PerformUnstuckMove(AvHAIPlayer* pBot, const Vector MoveDestination);

// Used by Detour for the FindRandomPointInCircle type functions
static float frand();

// Finds the appropriate nav mesh for the requested profile
const dtNavMesh* UTIL_GetNavMeshForProfile(const nav_profile & NavProfile);
// Finds the appropriate nav mesh query for the requested profile
const dtNavMeshQuery* UTIL_GetNavMeshQueryForProfile(const nav_profile& NavProfile);
// Finds the appropriatetile cache for the requested profile
const dtTileCache* UTIL_GetTileCacheForProfile(const nav_profile& NavProfile);

float UTIL_PointIsDirectlyReachable_DEBUG(const Vector start, const Vector target);

/*
	Point is directly reachable:
	Determines if the bot can walk directly between the two points.
	Ignores map geometry, so will return true even if stairs are in the way, provided the bot can walk up/down them unobstructed
*/
bool UTIL_PointIsDirectlyReachable(const AvHAIPlayer* pBot, const Vector targetPoint, const float MaxDist = max_ai_use_reach);
bool UTIL_PointIsDirectlyReachable(const AvHAIPlayer* pBot, const Vector start, const Vector target, const float MaxDist = max_ai_use_reach);
bool UTIL_PointIsDirectlyReachable(const Vector start, const Vector target, const float MaxDist = max_ai_use_reach);
bool UTIL_PointIsDirectlyReachable(const nav_profile& NavProfile, const Vector start, const Vector target, const float MaxDist = max_ai_use_reach);

// Will trace along the nav mesh from start to target and return true if the trace reaches within MaxAcceptableDistance
bool UTIL_TraceNav(const nav_profile& NavProfile, const Vector start, const Vector target, const float MaxAcceptableDistance);

void UTIL_TraceNavLine(const nav_profile& NavProfile, const Vector Start, const Vector End, nav_hitresult* HitResult);

/*
	Project point to navmesh:
	Takes the supplied location in the game world, and returns the nearest point on the nav mesh within the supplied extents.
	Uses pExtents by default if not supplying one.
	Returns ZERO_VECTOR if not projected successfully
*/
Vector UTIL_ProjectPointToNavmesh(const Vector Location);
Vector UTIL_ProjectPointToNavmesh(const Vector Location, const Vector Extents);
Vector UTIL_ProjectPointToNavmesh(const Vector Location, const nav_profile& NavProfile);
Vector UTIL_ProjectPointToNavmesh(const Vector Location, const Vector Extents, const nav_profile& NavProfile);

/*
	Point is on navmesh:
	Returns true if it was able to project the point to the navmesh (see UTIL_ProjectPointToNavmesh())
*/
bool UTIL_PointIsOnNavmesh(const NavAgentProfile* NavProfile, const Vector Location, const Vector SearchExtents = DefaultReachableExtents);

// Sets the BotNavInfo so the bot can track if it's on the ground, in the air, climbing a wall, on a ladder etc.
void UTIL_UpdateBotMovementStatus(AvHAIPlayer* pBot);

// Returns true if a path could be found between From and To location. Cheaper than full path finding, only a rough check to confirm it can be done.
bool UTIL_PointIsReachable(const NavAgentProfile* NavProfile, const Vector FromLocation, const Vector ToLocation, const float MaxAcceptableDistance);

// If the bot has a path, it will work out how far along the path it can see and return the furthest point. Used so that the bot looks ahead along the path rather than just at its next path point
Vector UTIL_GetFurthestVisiblePointOnPath(const AvHAIPlayer* pBot);
// For the given viewer location and path, will return the furthest point along the path the viewer could see
Vector UTIL_GetFurthestVisiblePointOnPath(const Vector ViewerLocation, vector<bot_path_node>& path, bool bPrecise);
Vector UTIL_GetFurthestVisiblePointOnLineWithHull(const Vector ViewerLocation, const Vector LineStart, const Vector LineEnd, int HullNumber);

// Returns the nearest nav mesh poly reference for the edict's current world position
dtPolyRef UTIL_GetNearestPolyRefForEntity(const edict_t* Edict);
dtPolyRef UTIL_GetNearestPolyRefForLocation(const Vector Location);
dtPolyRef UTIL_GetNearestPolyRefForLocation(const nav_profile& NavProfile, const Vector Location);

// Returns the area for the nearest nav mesh poly to the given location. Returns BLOCKED if none found
unsigned char UTIL_GetNavAreaAtLocation(const Vector Location);
unsigned char UTIL_GetNavAreaAtLocation(const nav_profile& NavProfile, const Vector Location);

// For printing out human-readable nav mesh areas
const char* UTIL_NavmeshAreaToChar(const unsigned char Area);



// From the given start point, determine how high up the bot needs to climb to get to climb end. Will allow the bot to climb over railings
float UTIL_FindZHeightForWallClimb(const Vector ClimbStart, const Vector ClimbEnd, const int HullNum);


// Clears the bot's path and sets the path size to 0
void ClearBotPath(AvHAIPlayer* pBot);
// Clears just the bot's current stuck movement attempt (see PerformUnstuckMove())
void ClearBotStuckMovement(AvHAIPlayer* pBot);

void UTIL_ClearDoorData();
void UTIL_ClearWeldablesData();

const nav_profile GetBaseNavProfile(const int index);

// Based on the direction the bot wants to move and it's current facing angle, sets the forward and side move, and the directional buttons to make the bot actually move
void BotMovementInputs(AvHAIPlayer* pBot);

// Event called when a bot starts climbing a ladder
void OnBotStartLadder(AvHAIPlayer* pBot);
// Event called when a bot leaves a ladder
void OnBotEndLadder(AvHAIPlayer* pBot);

// Tracks all doors and their current status
void UTIL_PopulateDoors();
void UTIL_PopulateTrainStopPoints(nav_door* TrainDoor);

void UTIL_UpdateWeldableObstacles();
void UTIL_UpdateDoors(bool bInitial = false);
void UTIL_UpdateDoorTriggers(nav_door* Door);

void UTIL_ModifyOffMeshConnectionFlag(AvHAIOffMeshConnection* Connection, const unsigned int NewFlag);

bool UTIL_IsTriggerLinkedToDoor(CBaseEntity* TriggerEntity, vector<CBaseEntity*>& CheckedTriggers, CBaseEntity* Door);
void UTIL_PopulateTriggersForEntity(edict_t* Entity, vector<DoorTrigger>& TriggerList);
void UTIL_PopulateAffectedConnectionsForDoor(nav_door* Door);

void UTIL_PopulateWeldableObstacles();

void UTIL_ApplyTempObstaclesToDoor(nav_door* DoorRef, const int Area);

nav_door* UTIL_GetLiftReferenceByEdict(const edict_t* DoorEdict);
nav_door* UTIL_GetNavDoorByEdict(const edict_t* DoorEdict);
nav_door* UTIL_GetClosestLiftToPoints(const Vector StartPoint, const Vector EndPoint);
AvHAIOffMeshConnection* UTIL_GetOffMeshConnectionForLift(nav_door* LiftRef);

bool UTIL_IsTileCacheUpToDate();

Vector UTIL_AdjustPointAwayFromNavWall(const Vector Location, const float MaxDistanceFromWall);

void UTIL_PopulateBaseNavProfiles();

void OnOffMeshConnectionAdded(dtOffMeshConnection* NewConnection);

const dtOffMeshConnection* DEBUG_FindNearestOffMeshConnectionToPoint(const Vector Point, unsigned int FilterFlags);

void NAV_SetPickupMovementTask(AvHAIPlayer* pBot, edict_t* ThingToPickup, DoorTrigger* TriggerToActivate);
void NAV_SetMoveMovementTask(AvHAIPlayer* pBot, Vector MoveLocation, DoorTrigger* TriggerToActivate);
void NAV_SetTouchMovementTask(AvHAIPlayer* pBot, edict_t* EntityToTouch, DoorTrigger* TriggerToActivate);
void NAV_SetUseMovementTask(AvHAIPlayer* pBot, edict_t* EntityToUse, DoorTrigger* TriggerToActivate);
void NAV_SetBreakMovementTask(AvHAIPlayer* pBot, edict_t* EntityToBreak, DoorTrigger* TriggerToActivate);
void NAV_SetWeldMovementTask(AvHAIPlayer* pBot, edict_t* EntityToWeld, DoorTrigger* TriggerToActivate);

void NAV_ClearMovementTask(AvHAIPlayer* pBot);

void NAV_ProgressMovementTask(AvHAIPlayer* pBot);
bool NAV_IsMovementTaskStillValid(AvHAIPlayer* pBot);

vector<NavHint*> NAV_GetHintsOfType(unsigned int HintType, bool bUnoccupiedOnly = false);
vector<NavHint*> NAV_GetHintsOfTypeInRadius(unsigned int HintType, Vector SearchLocation, float Radius, bool bUnoccupiedOnly = false);

void RefineFlightPath(vector<bot_path_node>& InputPath, vector<bot_path_node>& RefinedPath);


#endif // BOT_NAVIGATION_H

