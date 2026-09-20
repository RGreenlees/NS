//
// EvoBot - Neoptolemus' Natural Selection bot, based on Botman's HPB bot template
//
// AvHAIMapData.h
//
// Handles all dynamic map data that impacts bot navigation and movement
//

#pragma once
#ifndef AVH_AI_MAP_DATA_H
#define AVH_AI_MAP_DATA_H

#include "DetourStatus.h"
#include "DetourNavMeshQuery.h"
#include "DetourTileCache.h"
#include "AvHAIPlayer.h"
#include "AvHAINavMesh.h"

// Dynamic map object type
enum class EAIDynamicMapObjectType
{
	MAPOBJECT_STATIC = 0,   // Cannot be moved, permanent obstacle
	MAPOBJECT_DOOR,			// Object type that moves between two points (e.g. a door) and can block off routes
	MAPOBJECT_PLATFORM,		// A basic up/down platform with only two states
	MAPOBJECT_TRAIN,        // Train with potentially multiple stops along a route (could also be plain up/down)
	TRIGGER_USE,			// Trigger activated by using it
	TRIGGER_TOUCH,			// Trigger activated by touching it
	TRIGGER_SHOOT,			// Trigger activated by shooting it (damage to activate)
	TRIGGER_BREAK,			// Trigger activated by breaking it (permanently destroy)
	TRIGGER_WELD			// Trigger activated by welding it (permanently destroy)
};

// Dynamic map object type
enum class EAIDynamicMapObjectState
{
	OBJECTSTATE_START = 0,  // The object has just been registered and does not yet know its state
	OBJECTSTATE_IDLE = 1,   // Object is idling and not going to move until triggered
	OBJECTSTATE_PREPARING,	// Object has been triggered and is getting ready to move
	OBJECTSTATE_MOVING,		// Object is on the move
	OBJECTSTATE_OPEN		// For buttons which don't move, this marks a button which has been pressed and is waiting to release for another use
};

struct DynamicMapObjectStop
{
	edict_t* CornerEdict = nullptr;
	Vector StopLocation = ZERO_VECTOR;
	bool bWaitForRetrigger = true;
	float WaitTime = 0.0f;
	std::vector<NavOffMeshConnection*> AffectedConnections;
};

struct DynamicMapObject
{
	int EdictIndex = -1;
	edict_t* Edict = nullptr;
	const char* ObjectName = nullptr;
	EAIDynamicMapObjectType Type = EAIDynamicMapObjectType::MAPOBJECT_STATIC;
	std::vector<NavTempObstacle> TempObstacles; // Dynamic obstacle ref. Used to add/remove the obstacle as the door is opened/closed
	std::vector<edict_t*> Triggers; // Reference to the trigger edicts (e.g. func_trigger, func_button etc.)
	std::vector<DynamicMapObjectStop> StopPoints; // Where the object stops when triggered. Doors will always have two stop points (open and shut positions), trains could have many
	int NextStopIndex = 0;
	std::vector<edict_t*> Targets;
	EAIDynamicMapObjectState State = EAIDynamicMapObjectState::OBJECTSTATE_IDLE; // What is the object currently doing
	edict_t* Master = nullptr; // The entity which has locked this object and must be triggered first
	std::string GlobalState = ""; // The global state this object relies upon to be active
	float Wait = 0.0f; // Once finished its trigger action (e.g. opening), how long it waits before resetting
	float Delay = 0.0f; // After being triggered, how long this object will wait before doing its thing
	float LastActivatedTime = 0.0f;
	bool bToggleActive = false; // Can this be toggled active/inactive?
	bool bIsActive = true; // If false, means this trigger or object has a dependent multisource or env_global which have not been satisfied yet, therefore this object is inert.
	int NumTimesActivated = 0; // How many times this object has been triggered

	void ClearObject()
	{
		EdictIndex = -1;
		Edict = nullptr;
		ObjectName = nullptr;
		Type = EAIDynamicMapObjectType::MAPOBJECT_STATIC;
		TempObstacles.clear();
		Triggers.clear();
		StopPoints.clear();
		NextStopIndex = 0;
		Targets.clear();
		State = EAIDynamicMapObjectState::OBJECTSTATE_IDLE;
		Master = nullptr;
		GlobalState = "";
		Wait = 0.0f;
		Delay = 0.0f;
		LastActivatedTime = 0.0f;
		bToggleActive = false;
		bIsActive = true;
		NumTimesActivated = 0;
	}
};

bool AIMAP_BuildMapData();
bool AIMAP_PopulateDynamicMapObjects();
bool AIMAP_PopulateDynamicDoorObject(edict_t* DoorObject);
bool AIMAP_PopulateDynamicTrainObject(edict_t* TrainObject);
void AIMAP_PopulateTrainStopPoints(edict_t* TrainObject, DynamicMapObject* MapObject);
bool AIMAP_PopulateDynamicPlatObject(edict_t* PlatObject);
bool AIMAP_PopulateDynamicButtonObject(edict_t* ButtonObject);
bool AIMAP_PopulateDynamicTriggerObject(edict_t* TriggerObject);
bool AIMAP_PopulateDynamicWeldableObject(edict_t* WeldableObject);
bool AIMAP_PopulateDynamicBreakableObject(edict_t* BreakableObject);

void AIMAP_SetDynamicObjectStatus(DynamicMapObject* Object, EAIDynamicMapObjectState NewState);

void AIMAP_OnDynamicMapObjectStopIdle(DynamicMapObject* Object);
void AIMAP_OnDynamicMapObjectBecomeIdle(DynamicMapObject* Object);

void AIMAP_UpdateDynamicMapObjects();
void AIMAP_UpdateDynamicDoorObject(DynamicMapObject* DoorObject);
void AIMAP_UpdateDynamicTouchTriggerObject(DynamicMapObject* TriggerObject);
void AIMAP_UpdateDynamicBreakableObject(DynamicMapObject* BreakableObject);
void AIMAP_UpdateDynamicWeldableObject(DynamicMapObject* WeldableObject);
void AIMAP_UpdateDynamicPlatformObject(DynamicMapObject* PlatformObject);
void AIMAP_UpdateDynamicTrainObject(DynamicMapObject* TrainObject);
void AIMAP_UpdateDynamicButtonObject(DynamicMapObject* ButtonObject);
void AIMAP_UpdateDynamicInactiveObject(DynamicMapObject* InactiveObject);

void AIMAP_OnTriggerActivated(DynamicMapObject* UsedObject);

DynamicMapObject* AIMAP_GetDynamicObjectByEdict(const edict_t* SearchEdict);
void AIMAP_LinkDynamicMapObjectsToTriggers();
void AIMAP_LinkDynamicMapObjectsToOffmeshConnections();
void AIMAP_SetTrainStartPoints();
void AIMAP_PopulateAllConnectionsAffectedByDynamicObjects();
bool AIMAP_IsDynamicMapTriggerLinkedToObject(edict_t* TriggerObject, edict_t* TargetObject, vector<edict_t*> CheckedObjects);
void AIMAP_PopulateConnectionsAffectedByDynamicObject(DynamicMapObject* Object);
DynamicMapObject* AIMAP_GetClosestPlatformToPoints(const Vector StartPoint, const Vector EndPoint);

bool AIMAP_IsOffMeshConnectionAffectedByObject(const DynamicMapObject* TestObject, const Vector& ObjectPosition, const NavOffMeshConnection* Connection);

DynamicMapObject* AIMAP_GetBestTriggerForObject(DynamicMapObject* ObjectToActivate, Vector ActivateLocation, const NavAgentProfile& NavProfile);
Vector AIMAP_GetButtonFloorLocation(const NavAgentProfile& NavProfile, const Vector UserLocation, edict_t* ButtonEdict);

bool AIMAP_IsPathBlockedByObject(const NavAgentProfile& NavProfile, const Vector StartLoc, const Vector EndLoc, DynamicMapObject* SearchObject);

DynamicMapObject* AIMAP_GetObjectBlockingPathPoint(const Vector FromLocation, const Vector ToLocation, const unsigned int MovementFlag, DynamicMapObject* SearchObject, DynamicMapObject* IgnoreObject);

// Removes all temporary obstacles from the map
void AIMAP_ApplyTempObstaclesToObject(DynamicMapObject* Object, const int Area);
void AIMAP_RemoveAllTempObstaclesFromObject(DynamicMapObject* Object);

// Clear all stored map data for the AI
void AIMAP_ClearCachedMapData();

void DEBUG_PrintObjectInfo(DynamicMapObject* Object);

#endif // AVH_AI_MAP_DATA_H
