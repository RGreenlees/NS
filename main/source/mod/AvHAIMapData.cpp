//
// EvoBot - Neoptolemus' Natural Selection bot, based on Botman's HPB bot template
//
// AvHAIMapData.cpp
//
// Handles all dynamic map data that impacts bot navigation and movement
//
#include "AvHAIMapData.h"
#include "AvHAINavigation.h"
#include "AvHServerUtil.h"
#include "AvHAIHelper.h"
#include "AvHWeldable.h"

#include <dlls/triggers.h>
#include <dlls/plats.h>

#include <dlls/cbase.h>       // Core base entity classes (CBaseEntity)
#include "AvHAIPlayerUtil.h"

// Be very careful if you're access this directly, prefer to use the handles array where possible
std::unordered_map<uint32_t, DynamicMapObject> DynamicMapObjects;

// The safe list of dynamic map objects for external access
std::vector<DynamicMapObjectHandle> DynamicMapObjectHandles;

uint32_t NextDynamicObjectIndex = 1;
extern uint32_t CurrentNavMeshGeneration;

static const int DOOR_START_OPEN = 1;
static const int DOOR_USE_ONLY = 256; // Flag used by GoldSrc to determine if a door entity can only be used to open (i.e. can't be triggered)
static const int SF_BUTTON_TOGGLE = 32;	// button stays pushed until reactivated
static const int SF_PLAT_TRIGGER_ONLY = 1;

bool AIMAP_BuildMapData()
{
	AIMAP_PopulateDynamicMapObjects();
	AIMAP_LinkDynamicMapObjectsToTriggers();
	AIMAP_LinkDynamicMapObjectsToOffMeshConnections();
	AIMAP_SetTrainStartPoints();
	AIMAP_PopulateAllConnectionsAffectedByDynamicObjects();

	return true;
}

bool AIMAP_PopulateDynamicMapObjects()
{
	DynamicMapObjects.clear();

	FOR_ALL_ENTITIES("func_door", CBaseEntity*)
		AIMAP_PopulateDynamicDoorObject(theEntity->edict());
	END_FOR_ALL_ENTITIES("func_door")

	FOR_ALL_ENTITIES("func_seethroughdoor", CBaseEntity*)
		AIMAP_PopulateDynamicDoorObject(theEntity->edict());
	END_FOR_ALL_ENTITIES("func_seethroughdoor")

	FOR_ALL_ENTITIES("func_door_rotating", CBaseEntity*)
		AIMAP_PopulateDynamicDoorObject(theEntity->edict());
	END_FOR_ALL_ENTITIES("func_door_rotating")

	FOR_ALL_ENTITIES("func_train", CBaseEntity*)
		AIMAP_PopulateDynamicTrainObject(theEntity->edict());
	END_FOR_ALL_ENTITIES("func_train")

	FOR_ALL_ENTITIES("func_plat", CBaseEntity*)
		AIMAP_PopulateDynamicPlatObject(theEntity->edict());
	END_FOR_ALL_ENTITIES("func_plat")

	FOR_ALL_ENTITIES("func_button", CBaseEntity*)
		AIMAP_PopulateDynamicButtonObject(theEntity->edict());
	END_FOR_ALL_ENTITIES("func_button")

	FOR_ALL_ENTITIES("trigger_once", CBaseEntity*)
		AIMAP_PopulateDynamicTriggerObject(theEntity->edict());
	END_FOR_ALL_ENTITIES("trigger_once")

	FOR_ALL_ENTITIES("trigger_multiple", CBaseEntity*)
		AIMAP_PopulateDynamicTriggerObject(theEntity->edict());
	END_FOR_ALL_ENTITIES("trigger_multiple")

	FOR_ALL_ENTITIES("avhweldable", CBaseEntity*)
		AIMAP_PopulateDynamicWeldableObject(theEntity->edict());
	END_FOR_ALL_ENTITIES("avhweldable")

	FOR_ALL_ENTITIES("func_breakable", CBaseEntity*)
		AIMAP_PopulateDynamicBreakableObject(theEntity->edict());
	END_FOR_ALL_ENTITIES("func_breakable")

	AIMAP_RefreshDynamicHandleList();

	return true;
}

void AIMAP_RefreshDynamicHandleList()
{
	DynamicMapObjectHandles.clear();

	for (auto it = DynamicMapObjects.begin(); it != DynamicMapObjects.end(); it++)
	{
		const uint32_t ObjectId = it->first;
		const DynamicMapObject* ThisObject = &it->second;

		if (!ThisObject || !ThisObject->IsValid()) { continue; }

		DynamicMapObjectHandle NewHandle;
		NewHandle.ObjectId = ObjectId;
		NewHandle.Generation = CurrentNavMeshGeneration;

		DynamicMapObjectHandles.push_back(NewHandle);
	}
}

bool AIMAP_PopulateDynamicDoorObject(edict_t* DoorObject)
{
	if (FNullEnt(DoorObject)) { return false; }

	CBaseDoor* DoorRef = dynamic_cast<CBaseDoor*>(CBaseEntity::Instance(DoorObject));

	if (!DoorRef) { return false; }

	DynamicMapObject NewObject;

	NewObject.ObjectId = NextDynamicObjectIndex;
	NewObject.ObjectRef = DoorRef;
	NewObject.EdictIndex = ENTINDEX(DoorObject);
	NewObject.Edict = DoorObject;

	CBaseEntity* MasterEdict = UTIL_FindEntityByTargetname(NULL, STRING(DoorRef->m_sMaster));

	if (MasterEdict)
	{
		NewObject.Master = MasterEdict->edict();
	}

	NewObject.ObjectName = STRING(NewObject.Edict->v.targetname);
	NewObject.Triggers.clear();
	NewObject.Wait = DoorRef->m_flWait;
	NewObject.Delay = DoorRef->m_flDelay;
	NewObject.Type = EAIDynamicMapObjectType::MAPOBJECT_DOOR;

	Vector DoorCentre = UTIL_GetCentreOfEntity(NewObject.Edict);
	const bool bZeroOrigin = (vIsZero(DoorRef->m_vecPosition1));

	DynamicMapObjectStop FirstStop;
	FirstStop.StopLocation = (bZeroOrigin) ? DoorCentre : DoorRef->m_vecPosition1;
	FirstStop.WaitTime = 0.0f;
	FirstStop.bWaitForRetrigger = true;

	NewObject.StopPoints.push_back(FirstStop);

	DynamicMapObjectStop SecondStop;
	SecondStop.StopLocation = (bZeroOrigin) ? DoorCentre + DoorRef->m_vecPosition2 : DoorRef->m_vecPosition2;
	SecondStop.WaitTime = NewObject.Wait;
	SecondStop.bWaitForRetrigger = ((NewObject.Edict->v.spawnflags & SF_DOOR_NO_AUTO_RETURN) || NewObject.Wait < 0.0f);

	NewObject.bToggleActive = SecondStop.bWaitForRetrigger;

	NewObject.StopPoints.push_back(SecondStop);

	if (CBaseEntity* DoorTarget = DoorRef->GetNextTarget())
	{
		edict_t* TargetEdict = DoorTarget->edict();

		if (!FNullEnt(TargetEdict))
		{
			NewObject.Targets.push_back(DoorTarget->edict());
		}
	}

	NewObject.State = EAIDynamicMapObjectState::OBJECTSTATE_START;

	if (CBaseEntity* Target = DoorRef->GetNextTarget())
	{
		NewObject.Targets.push_back(Target->edict());
	}

	DynamicMapObjects.try_emplace(NextDynamicObjectIndex, NewObject);

	NextDynamicObjectIndex++;

	return true;
}

bool AIMAP_PopulateDynamicTrainObject(edict_t* TrainObject)
{
	if (FNullEnt(TrainObject)) { return false; }

	CBaseToggle* TrainRef = dynamic_cast<CBaseToggle*>(CBaseEntity::Instance(TrainObject));

	if (!TrainRef) { return false; }

	DynamicMapObject NewObject;

	NewObject.ObjectId = NextDynamicObjectIndex;
	NewObject.ObjectRef = TrainRef;
	NewObject.EdictIndex = ENTINDEX(TrainObject);
	NewObject.Edict = TrainObject;

	CBaseEntity* MasterEdict = UTIL_FindEntityByTargetname(NULL, STRING(TrainRef->m_sMaster));

	if (MasterEdict)
	{
		NewObject.Master = MasterEdict->edict();
	}

	NewObject.ObjectName = STRING(NewObject.Edict->v.targetname);
	NewObject.Triggers.clear();
	NewObject.Wait = TrainRef->m_flWait;
	NewObject.Delay = TrainRef->m_flDelay;
	NewObject.Type = EAIDynamicMapObjectType::MAPOBJECT_TRAIN;

	AIMAP_PopulateTrainStopPoints(TrainObject, &NewObject);

	NewObject.NextStopIndex = (NewObject.StopPoints.size() > 1) ? 1 : 0;

	if (CBaseEntity* DoorTarget = TrainRef->GetNextTarget())
	{
		edict_t* TargetEdict = DoorTarget->edict();

		if (!FNullEnt(TargetEdict))
		{
			NewObject.Targets.push_back(DoorTarget->edict());
		}
	}

	NewObject.State = EAIDynamicMapObjectState::OBJECTSTATE_START;

	if (CBaseEntity* Target = TrainRef->GetNextTarget())
	{
		NewObject.Targets.push_back(Target->edict());
	}

	DynamicMapObjects.try_emplace(NextDynamicObjectIndex, NewObject);

	NextDynamicObjectIndex++;

	return true;
}

void AIMAP_PopulateTrainStopPoints(edict_t* TrainObject, DynamicMapObject* MapObject)
{
	if (FNullEnt(TrainObject) || !MapObject) { return; }

	CBaseToggle* TrainRef = dynamic_cast<CBaseToggle*>(CBaseEntity::Instance(TrainObject));

	if (!TrainRef) { return; }

	edict_t* TrainEdict = MapObject->Edict;
	CBaseEntity* StartCorner = nullptr;

	if (TrainEdict->v.target)
	{
		StartCorner = UTIL_FindEntityByTargetname(NULL, STRING(TrainEdict->v.target));
	}

	// We aren't using path corners, so treat like a door
	if (!StartCorner || FNullEnt(StartCorner->edict()))
	{
		edict_t* ObjectEdict = MapObject->Edict;

		Vector ObjectCentre = UTIL_GetCentreOfEntity(ObjectEdict);

		DynamicMapObjectStop FirstStop;
		FirstStop.StopLocation = ObjectCentre;
		FirstStop.WaitTime = 0.0f;
		FirstStop.bWaitForRetrigger = true;

		MapObject->StopPoints.push_back(FirstStop);

		Vector OpenPosition = UTIL_GetCentreOfEntity(MapObject->Edict) + TrainRef->m_vecPosition2;

		DynamicMapObjectStop SecondStop;
		SecondStop.StopLocation = OpenPosition;
		SecondStop.WaitTime = 0.0f;
		SecondStop.bWaitForRetrigger = (ObjectEdict->v.spawnflags & SF_DOOR_NO_AUTO_RETURN);

		MapObject->StopPoints.push_back(SecondStop);

		return;
	}

	// We already check to ensure the edict is not null above
	edict_t* StartCornerEdict = StartCorner->edict();

	DynamicMapObjectStop NextStop;
	NextStop.CornerEdict = StartCornerEdict;
	NextStop.StopLocation = StartCornerEdict->v.origin;
	NextStop.WaitTime = 0.0f;
	NextStop.bWaitForRetrigger = (StartCornerEdict->v.spawnflags & SF_TRAIN_WAIT_RETRIGGER);

	MapObject->StopPoints.push_back(NextStop);

	// Populate all path corners at which this func_train stops. Bot will use this to determine when to board the train

	const char* StartCornerName = STRING(StartCornerEdict->v.targetname);

	CBaseEntity* CurrentCorner = UTIL_FindEntityByTargetname(NULL, STRING(StartCornerEdict->v.target));

	while (CurrentCorner != NULL && CurrentCorner != StartCorner)
	{
		edict_t* CurrentCornerEdict = CurrentCorner->edict();

		if (FNullEnt(CurrentCornerEdict)) { break; }

		NextStop.CornerEdict = CurrentCornerEdict;
		NextStop.StopLocation = CurrentCornerEdict->v.origin;
		NextStop.WaitTime = 0.0f;
		NextStop.bWaitForRetrigger = (CurrentCornerEdict->v.spawnflags & SF_TRAIN_WAIT_RETRIGGER);

		MapObject->StopPoints.push_back(NextStop);

		CurrentCorner = UTIL_FindEntityByTargetname(NULL, STRING(CurrentCornerEdict->v.target));
	}
}

bool AIMAP_PopulateDynamicPlatObject(edict_t* PlatObject)
{
	if (!PlatObject) { return false; }

	CBaseToggle* PlatRef = dynamic_cast<CBaseToggle*>(CBaseEntity::Instance(PlatObject));

	if (!PlatRef) { return false; }

	DynamicMapObject NewObject;

	NewObject.ObjectId = NextDynamicObjectIndex;
	NewObject.ObjectRef = PlatRef;
	NewObject.EdictIndex = ENTINDEX(PlatObject);
	NewObject.Edict = PlatObject;

	CBaseEntity* MasterEdict = UTIL_FindEntityByTargetname(NULL, STRING(PlatRef->m_sMaster));

	if (MasterEdict)
	{
		NewObject.Master = MasterEdict->edict();
	}

	const Vector PlatformCentre = UTIL_GetCentreOfEntity(PlatObject);

	NewObject.ObjectName = STRING(NewObject.Edict->v.targetname);
	NewObject.Triggers.clear();
	NewObject.Wait = PlatRef->m_flWait;
	NewObject.Delay = PlatRef->m_flDelay;
	NewObject.Type = EAIDynamicMapObjectType::MAPOBJECT_PLATFORM;

	const bool bZeroOrigin = vIsZero(PlatRef->m_vecPosition1);

	DynamicMapObjectStop FirstStop;
	FirstStop.StopLocation = (bZeroOrigin) ? PlatformCentre : PlatRef->m_vecPosition1;
	FirstStop.WaitTime = 0.0f;
	FirstStop.bWaitForRetrigger = true;

	NewObject.StopPoints.push_back(FirstStop);

	DynamicMapObjectStop SecondStop;
	SecondStop.StopLocation = (bZeroOrigin) ? PlatformCentre + Vector(0.0f, 0.0f, PlatRef->m_flHeight) : PlatRef->m_vecPosition2;
	SecondStop.WaitTime = 3.0f;
	SecondStop.bWaitForRetrigger = (NewObject.Edict->v.spawnflags & SF_PLAT_TRIGGER_ONLY);

	NewObject.StopPoints.push_back(SecondStop);

	NewObject.NextStopIndex = (NewObject.StopPoints.size() > 1) ? 1 : 0;

	if (CBaseEntity* PlatTarget = PlatRef->GetNextTarget())
	{
		edict_t* TargetEdict = PlatTarget->edict();

		if (!FNullEnt(TargetEdict))
		{
			NewObject.Targets.push_back(PlatTarget->edict());
		}
	}

	NewObject.State = EAIDynamicMapObjectState::OBJECTSTATE_START;

	if (CBaseEntity* Target = PlatRef->GetNextTarget())
	{
		NewObject.Targets.push_back(Target->edict());
	}

	DynamicMapObjects.try_emplace(NextDynamicObjectIndex, NewObject);

	NextDynamicObjectIndex++;

	return true;
}

bool AIMAP_PopulateDynamicButtonObject(edict_t* ButtonObject)
{
	if (FNullEnt(ButtonObject)) { return false; }

	CBaseButton* ButtonRef = dynamic_cast<CBaseButton*>(CBaseEntity::Instance(ButtonObject));

	if (!ButtonRef) { return false; }

	DynamicMapObject NewObject;

	NewObject.ObjectId = NextDynamicObjectIndex;
	NewObject.ObjectRef = ButtonRef;
	NewObject.EdictIndex = ENTINDEX(ButtonObject);
	NewObject.Edict = ButtonObject;

	CBaseEntity* MasterEdict = UTIL_FindEntityByTargetname(NULL, STRING(ButtonRef->m_sMaster));

	if (MasterEdict)
	{
		NewObject.Master = MasterEdict->edict();
	}

	NewObject.ObjectName = STRING(NewObject.Edict->v.targetname);
	NewObject.Triggers.clear();
	NewObject.Wait = ButtonRef->m_flWait;
	NewObject.Delay = ButtonRef->m_flDelay;

	if (NewObject.Edict->v.health > 0.0f)
	{
		NewObject.Type = EAIDynamicMapObjectType::TRIGGER_SHOOT;
	}
	else
	{
		NewObject.Type = EAIDynamicMapObjectType::TRIGGER_USE;
	}

	const bool bZeroOrigin = (vIsZero(ButtonRef->m_vecPosition1));

	DynamicMapObjectStop FirstStop;
	FirstStop.StopLocation = (bZeroOrigin) ? UTIL_GetCentreOfEntity(ButtonObject) : ButtonRef->m_vecPosition1;
	FirstStop.WaitTime = 0.0f;
	FirstStop.bWaitForRetrigger = true;

	NewObject.StopPoints.push_back(FirstStop);

	DynamicMapObjectStop SecondStop;
	SecondStop.StopLocation = (bZeroOrigin) ? FirstStop.StopLocation + ButtonRef->m_vecPosition2 : ButtonRef->m_vecPosition2;
	SecondStop.WaitTime = ButtonRef->m_flWait;
	SecondStop.bWaitForRetrigger = (NewObject.Edict->v.spawnflags & SF_DOOR_NO_AUTO_RETURN);

	NewObject.StopPoints.push_back(SecondStop);
	NewObject.NextStopIndex = (NewObject.StopPoints.size() > 1) ? 1 : 0;
	NewObject.State = EAIDynamicMapObjectState::OBJECTSTATE_START;

	DynamicMapObjects.try_emplace(NextDynamicObjectIndex, NewObject);

	NextDynamicObjectIndex++;

	return true;
}

bool AIMAP_PopulateDynamicWeldableObject(edict_t* WeldableObject)
{
	if (FNullEnt(WeldableObject)) { return false; }

	AvHWeldable* WeldableRef = dynamic_cast<AvHWeldable*>(CBaseEntity::Instance(WeldableObject));

	if (!WeldableRef) { return false; }

	DynamicMapObject NewObject;

	NewObject.ObjectId = NextDynamicObjectIndex;
	NewObject.ObjectRef = WeldableRef;
	NewObject.EdictIndex = ENTINDEX(WeldableObject);
	NewObject.Edict = WeldableObject;
	NewObject.ObjectName = STRING(NewObject.Edict->v.targetname);
	NewObject.Triggers.clear();
	NewObject.Wait = 0.0f;
	NewObject.Delay = WeldableRef->GetDelay();
	NewObject.Type = EAIDynamicMapObjectType::TRIGGER_WELD;
	NewObject.State = EAIDynamicMapObjectState::OBJECTSTATE_START;
	NewObject.NextStopIndex = (NewObject.StopPoints.size() > 1) ? 1 : 0;

	DynamicMapObjects.try_emplace(NextDynamicObjectIndex, NewObject);

	NextDynamicObjectIndex++;

	return true;
}

bool AIMAP_PopulateDynamicTriggerObject(edict_t* TriggerObject)
{
	if (FNullEnt(TriggerObject)) { return false; }

	CBaseTrigger* TriggerRef = dynamic_cast<CBaseTrigger*>(CBaseEntity::Instance(TriggerObject));

	if (!TriggerRef) { return false; }

	DynamicMapObject NewObject;

	NewObject.ObjectId = NextDynamicObjectIndex;
	NewObject.ObjectRef = TriggerRef;
	NewObject.EdictIndex = ENTINDEX(TriggerObject);
	NewObject.Edict = TriggerObject;

	CBaseEntity* MasterEdict = UTIL_FindEntityByTargetname(NULL, STRING(TriggerRef->m_sMaster));

	if (MasterEdict)
	{
		NewObject.Master = MasterEdict->edict();
	}

	NewObject.ObjectName = STRING(NewObject.Edict->v.targetname);
	NewObject.Triggers.clear();
	NewObject.Wait = TriggerRef->m_flWait;
	NewObject.Delay = TriggerRef->m_flDelay;
	NewObject.Type = EAIDynamicMapObjectType::TRIGGER_TOUCH;
	NewObject.State = EAIDynamicMapObjectState::OBJECTSTATE_START;
	NewObject.NextStopIndex = (NewObject.StopPoints.size() > 1) ? 1 : 0;

	DynamicMapObjects.try_emplace(NextDynamicObjectIndex, NewObject);

	NextDynamicObjectIndex++;

	return true;
}

bool AIMAP_PopulateDynamicBreakableObject(edict_t* BreakableObject)
{
	if (FNullEnt(BreakableObject)) { return false; }

	CBreakable* BreakableRef = dynamic_cast<CBreakable*>(CBaseEntity::Instance(BreakableObject));

	if (!BreakableRef) { return false; }

	DynamicMapObject NewObject;

	NewObject.ObjectId = NextDynamicObjectIndex;
	NewObject.ObjectRef = BreakableRef;
	NewObject.EdictIndex = ENTINDEX(BreakableObject);
	NewObject.Edict = BreakableObject;
	NewObject.ObjectName = STRING(NewObject.Edict->v.targetname);
	NewObject.Triggers.clear();
	NewObject.Delay = BreakableRef->m_flDelay;
	NewObject.Type = EAIDynamicMapObjectType::TRIGGER_BREAK;

	NewObject.State = EAIDynamicMapObjectState::OBJECTSTATE_START;

	DynamicMapObjects.try_emplace(NextDynamicObjectIndex, NewObject);

	NextDynamicObjectIndex++;

	return true;
}

void AIMAP_InitializeDynamicMapObject(DynamicMapObject* Object)
{
	if (!Object || !Object->IsValid()) { return; }

	int32 CurrentStopIndex = 0;

	for (int32 StopIndex = 0; StopIndex < Object->StopPoints.size(); StopIndex++)
	{
		const DynamicMapObjectStop* StopPoint = &Object->StopPoints[StopIndex];

		if (vEquals(StopPoint->StopLocation, UTIL_GetCentreOfEntity(Object->Edict), 5.0f))
		{
			Object->CurrentStop = &Object->StopPoints[StopIndex];
			CurrentStopIndex = StopIndex;
			break;
		}
	}

	if (Object->Type == EAIDynamicMapObjectType::MAPOBJECT_TRAIN)
	{
		CBaseToggle* TrainRef = dynamic_cast<CBaseToggle*>(CBaseEntity::Instance(Object->Edict));

		if (!TrainRef) { return; }

		CBaseEntity* NextPathCorner = TrainRef->GetNextTarget();

		if (!NextPathCorner) { return; }

		const edict_t* CornerEdict = NextPathCorner->edict();

		if (FNullEnt(CornerEdict)) { return; }

		for (int32 StopIndex = 0; StopIndex < Object->StopPoints.size(); StopIndex++)
		{
			const DynamicMapObjectStop* StopPoint = &Object->StopPoints[StopIndex];

			if (vEquals(StopPoint->StopLocation, CornerEdict->v.origin, 5.0f))
			{
				Object->NextStopIndex = StopIndex;
				return;
			}
		}
	}

	Object->NextStopIndex = CurrentStopIndex + 1;

	if (Object->NextStopIndex >= Object->StopPoints.size())
	{
		Object->NextStopIndex = 0;
	}

	AIMAP_SetDynamicObjectStatus(Object, EAIDynamicMapObjectState::OBJECTSTATE_IDLE);
}

void AIMAP_UpdateDynamicMapObjects()
{
	if (DynamicMapObjects.empty()) { return; }

	bool bListModified = false;

	for (auto it = DynamicMapObjects.begin(); it != DynamicMapObjects.end();)
	{
		DynamicMapObject* ThisObject = &it->second;

		if (!ThisObject)
		{
			it = DynamicMapObjects.erase(it);
			bListModified = true;
			continue;
		}

		if (!ThisObject->IsValid())
		{
			for (auto it = ThisObject->Targets.begin(); it != ThisObject->Targets.end(); it++)
			{
				DynamicMapObjectHandle FoundObjectHandle = AIMAP_GetDynamicObjectByEdict(*it);

				if (!FoundObjectHandle.IsValid()) { continue; }

				DynamicMapObject* TargetObject = FoundObjectHandle.GetObjectRef_Mutable();

				if (!TargetObject || !TargetObject->IsValid()) { continue; }

				AIMAP_ForceRefreshDynamicObjectConnections(TargetObject);
			}

			ThisObject->ClearTemporaryObstacles();
			ThisObject->UndoOffMeshConnectionChanges();

			it = DynamicMapObjects.erase(it);
			bListModified = true;
			continue;
		}

		CBaseToggle* ToggleEntityRef = dynamic_cast<CBaseToggle*>(CBaseEntity::Instance(ThisObject->Edict));

		if (ToggleEntityRef)
		{
			ThisObject->bIsActive = !ToggleEntityRef->IsLockedByMaster();
		}
		else
		{
			ThisObject->bIsActive = true;
		}

		if (ThisObject->State == EAIDynamicMapObjectState::OBJECTSTATE_START)
		{
			AIMAP_InitializeDynamicMapObject(ThisObject);
		}

		switch (ThisObject->Type)
		{
			case EAIDynamicMapObjectType::MAPOBJECT_DOOR:
				AIMAP_UpdateDynamicDoorObject(ThisObject);
				break;
			case EAIDynamicMapObjectType::TRIGGER_BREAK:
				AIMAP_UpdateDynamicBreakableObject(ThisObject);
				break;
			case EAIDynamicMapObjectType::TRIGGER_TOUCH:
				AIMAP_UpdateDynamicTouchTriggerObject(ThisObject);
				break;
			case EAIDynamicMapObjectType::TRIGGER_WELD:
				AIMAP_UpdateDynamicWeldableObject(ThisObject);
				break;
			case EAIDynamicMapObjectType::MAPOBJECT_PLATFORM:
				AIMAP_UpdateDynamicPlatformObject(ThisObject);
				break;
			case EAIDynamicMapObjectType::MAPOBJECT_TRAIN:
				AIMAP_UpdateDynamicTrainObject(ThisObject);
				break;
			case EAIDynamicMapObjectType::TRIGGER_SHOOT:
			case EAIDynamicMapObjectType::TRIGGER_USE:
				AIMAP_UpdateDynamicButtonObject(ThisObject);
				break;
			case EAIDynamicMapObjectType::MAPOBJECT_STATIC:
				AIMAP_UpdateDynamicInactiveObject(ThisObject);
				break;
			default:
				break;
		}

		++it;
	}

	if (bListModified)
	{
		AIMAP_RefreshDynamicHandleList();
	}
}

void AIMAP_UpdateDynamicInactiveObject(DynamicMapObject* InactiveObject)
{
	if (!InactiveObject || FNullEnt(InactiveObject->Edict)) { return;  }

	if (InactiveObject->Edict->v.velocity.Length() > 0.0f)
	{
		AIMAP_SetDynamicObjectStatus(InactiveObject, EAIDynamicMapObjectState::OBJECTSTATE_MOVING);
		return;
	}

	AIMAP_SetDynamicObjectStatus(InactiveObject, EAIDynamicMapObjectState::OBJECTSTATE_IDLE);
}

void AIMAP_UpdateDynamicButtonObject(DynamicMapObject* ButtonObject)
{
	if (!ButtonObject || FNullEnt(ButtonObject->Edict)) { return; }

	CBaseButton* ButtonRef = dynamic_cast<CBaseButton*>(CBaseEntity::Instance(ButtonObject->Edict));

	if (!ButtonRef) { return; }

	const TOGGLE_STATE ButtonCurrentState = static_cast<TOGGLE_STATE>(ButtonRef->GetToggleState());

	if (ButtonCurrentState == TS_GOING_UP)
	{
		if (ButtonObject->State == EAIDynamicMapObjectState::OBJECTSTATE_IDLE)
		{
			AIMAP_OnTriggerActivated(ButtonObject);
		}

		AIMAP_SetDynamicObjectStatus(ButtonObject, EAIDynamicMapObjectState::OBJECTSTATE_MOVING);
		return;
	}

	if (ButtonCurrentState == TS_GOING_DOWN)
	{
		AIMAP_SetDynamicObjectStatus(ButtonObject, EAIDynamicMapObjectState::OBJECTSTATE_PREPARING);
		return;
	}

	if (ButtonCurrentState == TS_AT_BOTTOM)
	{
		AIMAP_SetDynamicObjectStatus(ButtonObject, EAIDynamicMapObjectState::OBJECTSTATE_IDLE);
		return;
	}

	if (ButtonCurrentState == TS_AT_TOP)
	{
		// Button doesn't move so it will go direct from TS_AT_BOTTOM to TS_AT_TOP
		if (ButtonObject->State == EAIDynamicMapObjectState::OBJECTSTATE_IDLE)
		{
			AIMAP_OnTriggerActivated(ButtonObject);
		}

		// Button is single use only, remove it from the nav data as it no longer has a role to play
		if (ButtonRef->m_fStayPushed)
		{
			if (!FBitSet(ButtonRef->pev->spawnflags, SF_BUTTON_TOGGLE))
			{
				ButtonObject->Edict = nullptr;
				return;
			}
		}

		AIMAP_SetDynamicObjectStatus(ButtonObject, EAIDynamicMapObjectState::OBJECTSTATE_PREPARING);
		return;
	}
}

void AIMAP_UpdateDynamicPlatformObject(DynamicMapObject* PlatformObject)
{
	if (!PlatformObject || !PlatformObject->IsValid()) { return; }

	CBaseToggle* PlatRef = dynamic_cast<CBaseToggle*>(CBaseEntity::Instance(PlatformObject->Edict));

	if (!PlatRef) { return; }

	const TOGGLE_STATE PlatCurrentState = static_cast<TOGGLE_STATE>(PlatRef->GetToggleState());

	if (PlatCurrentState == TS_GOING_UP || PlatCurrentState == TS_GOING_DOWN)
	{
		AIMAP_SetDynamicObjectStatus(PlatformObject, EAIDynamicMapObjectState::OBJECTSTATE_MOVING);
		return;
	}

	if (PlatCurrentState == TS_AT_TOP)
	{
		const EAIDynamicMapObjectState NewState = (PlatRef->pev->spawnflags & SF_PLAT_TOGGLE) ? EAIDynamicMapObjectState::OBJECTSTATE_IDLE : EAIDynamicMapObjectState::OBJECTSTATE_PREPARING;
		AIMAP_SetDynamicObjectStatus(PlatformObject, NewState);
		return;
	}

	if (PlatCurrentState == TS_AT_BOTTOM)
	{
		AIMAP_SetDynamicObjectStatus(PlatformObject, EAIDynamicMapObjectState::OBJECTSTATE_IDLE);
		return;
	}
}

void AIMAP_UpdateDynamicTrainObject(DynamicMapObject* TrainObject)
{
	if (!TrainObject || FNullEnt(TrainObject->Edict)) { return; }

	CBaseToggle* TrainRef = dynamic_cast<CBaseToggle*>(CBaseEntity::Instance(TrainObject->Edict));

	if (!TrainRef) { return; }

	// We have no way of activating this door. It's now a permanent blockage once it finishes moving
	if (TrainObject->Triggers.size() == 0)
	{
		TrainObject->Type = EAIDynamicMapObjectType::MAPOBJECT_STATIC;
	}

	if (TrainObject->Edict->v.velocity.Length() > 0.0f)
	{
		if (TrainObject->State != EAIDynamicMapObjectState::OBJECTSTATE_MOVING)
		{
			TrainObject->CurrentStop = nullptr;
		}

		AIMAP_SetDynamicObjectStatus(TrainObject, EAIDynamicMapObjectState::OBJECTSTATE_MOVING);

		return;
	}

	if (TrainObject->State == EAIDynamicMapObjectState::OBJECTSTATE_MOVING)
	{
		for (int32 StopIndex = 0; StopIndex < TrainObject->StopPoints.size(); StopIndex++)
		{
			const DynamicMapObjectStop* StopPoint = &TrainObject->StopPoints[StopIndex];

			if (vEquals(StopPoint->StopLocation, UTIL_GetCentreOfEntity(TrainObject->Edict), 5.0f))
			{
				TrainObject->CurrentStop = &TrainObject->StopPoints[StopIndex];
				break;
			}
		}

		CBaseEntity* NextPathCorner = TrainRef->GetNextTarget();

		if (NextPathCorner)
		{
			const edict_t* CornerEdict = NextPathCorner->edict();

			if (!FNullEnt(CornerEdict))
			{
				for (int32 StopIndex = 0; StopIndex < TrainObject->StopPoints.size(); StopIndex++)
				{
					const DynamicMapObjectStop* StopPoint = &TrainObject->StopPoints[StopIndex];

					if (vEquals(StopPoint->StopLocation, CornerEdict->v.origin, 5.0f))
					{
						TrainObject->NextStopIndex = StopIndex;
						break;
					}
				}
			}
		}
	}

	if (TrainObject->State != EAIDynamicMapObjectState::OBJECTSTATE_PREPARING)
	{
		const DynamicMapObjectStop* CurrentStop = TrainObject->GetCurrentStop();

		if (CurrentStop && !CurrentStop->bWaitForRetrigger)
		{
			AIMAP_SetDynamicObjectStatus(TrainObject, EAIDynamicMapObjectState::OBJECTSTATE_PREPARING);
		}
		else
		{
			AIMAP_SetDynamicObjectStatus(TrainObject, EAIDynamicMapObjectState::OBJECTSTATE_IDLE);
		}
	}
}

void AIMAP_UpdateDynamicDoorObject(DynamicMapObject* DoorObject)
{
	if (!DoorObject || FNullEnt(DoorObject->Edict)) { return; }

	CBaseDoor* DoorRef = dynamic_cast<CBaseDoor*>(CBaseEntity::Instance(DoorObject->Edict));

	if (!DoorRef) { return; }

	// We have no way of activating this door. It's now a permanent blockage once it finishes moving
	if (DoorObject->Triggers.size() == 0 || (!DoorObject->bToggleActive && DoorObject->Wait < 0.0f))
	{
		DoorObject->Type = EAIDynamicMapObjectType::MAPOBJECT_STATIC;
	}

	const TOGGLE_STATE DoorCurrentState = static_cast<TOGGLE_STATE>(DoorRef->GetToggleState());

	if (DoorCurrentState == TS_GOING_UP || DoorCurrentState == TS_GOING_DOWN)
	{
		if (DoorObject->State != EAIDynamicMapObjectState::OBJECTSTATE_MOVING)
		{
			CBaseToggle* BaseToggle = dynamic_cast<CBaseToggle*>(DoorObject->ObjectRef);

			if (BaseToggle)
			{
				const Vector FinalDestination = BaseToggle->m_vecFinalDest;

				for (int32 StopIndex = 0; StopIndex < DoorObject->StopPoints.size(); StopIndex++)
				{
					const DynamicMapObjectStop* StopPoint = &DoorObject->StopPoints[StopIndex];

					if (vEquals(StopPoint->StopLocation, FinalDestination, 5.0f))
					{
						DoorObject->NextStopIndex = StopIndex;
						break;
					}
				}
			}

			DoorObject->CurrentStop = nullptr;
		}

		AIMAP_SetDynamicObjectStatus(DoorObject, EAIDynamicMapObjectState::OBJECTSTATE_MOVING);
		return;
	}

	if (DoorObject->State == EAIDynamicMapObjectState::OBJECTSTATE_MOVING)
	{
		for (int32 StopIndex = 0; StopIndex < DoorObject->StopPoints.size(); StopIndex++)
		{
			const DynamicMapObjectStop* StopPoint = &DoorObject->StopPoints[StopIndex];

			if (vEquals(StopPoint->StopLocation, UTIL_GetCentreOfEntity(DoorObject->Edict), 5.0f))
			{
				DoorObject->CurrentStop = &DoorObject->StopPoints[StopIndex];
				break;
			}
		}

		DoorObject->NextStopIndex++;

		if (DoorObject->NextStopIndex >= DoorObject->StopPoints.size())
		{
			DoorObject->NextStopIndex = 0;
		}
	}

	if (DoorObject->State != EAIDynamicMapObjectState::OBJECTSTATE_PREPARING && DoorCurrentState == TS_AT_BOTTOM)
	{
		AIMAP_SetDynamicObjectStatus(DoorObject, EAIDynamicMapObjectState::OBJECTSTATE_IDLE);
		return;
	}

	if (DoorObject->State != EAIDynamicMapObjectState::OBJECTSTATE_PREPARING && DoorCurrentState == TS_AT_TOP)
	{
		if (DoorObject->bToggleActive)
		{
			AIMAP_SetDynamicObjectStatus(DoorObject, EAIDynamicMapObjectState::OBJECTSTATE_IDLE);
		}
		else
		{
			AIMAP_SetDynamicObjectStatus(DoorObject, EAIDynamicMapObjectState::OBJECTSTATE_PREPARING);
		}

		return;
	}
}

void AIMAP_UpdateDynamicTouchTriggerObject(DynamicMapObject* TriggerObject)
{
	if (!TriggerObject || FNullEnt(TriggerObject->Edict)) { return; }

	if (TriggerObject->State == EAIDynamicMapObjectState::OBJECTSTATE_IDLE && TriggerObject->Edict->v.nextthink > 0.0f)
	{
		AIMAP_OnTriggerActivated(TriggerObject);

		AIMAP_SetDynamicObjectStatus(TriggerObject, EAIDynamicMapObjectState::OBJECTSTATE_PREPARING);

		// This is a trigger once, so it now needs to remove itself from the object list
		if (TriggerObject->Wait < 0.0f)
		{
			TriggerObject->Edict = nullptr;
			return;
		}
	}

	if (TriggerObject->State == EAIDynamicMapObjectState::OBJECTSTATE_PREPARING && TriggerObject->Edict->v.nextthink <= 0.0f)
	{
		AIMAP_SetDynamicObjectStatus(TriggerObject, EAIDynamicMapObjectState::OBJECTSTATE_IDLE);
	}
}

void AIMAP_UpdateDynamicBreakableObject(DynamicMapObject* BreakableObject)
{
	if (!BreakableObject || FNullEnt(BreakableObject->Edict)) { return; }

	AIMAP_SetDynamicObjectStatus(BreakableObject, EAIDynamicMapObjectState::OBJECTSTATE_IDLE);

	if (BreakableObject->Edict->v.health <= 0.0f)
	{
		AIMAP_OnTriggerActivated(BreakableObject);
		BreakableObject->Edict = nullptr;
		return;
	}
}

void AIMAP_UpdateDynamicWeldableObject(DynamicMapObject* WeldableObject)
{
	if (!WeldableObject || FNullEnt(WeldableObject->Edict)) { return; }

	AIMAP_SetDynamicObjectStatus(WeldableObject, EAIDynamicMapObjectState::OBJECTSTATE_IDLE);

	AvHWeldable* WeldableRef = dynamic_cast<AvHWeldable*>(CBaseEntity::Instance(WeldableObject->Edict));

	if (WeldableRef && WeldableRef->GetIsWelded())
	{
		AIMAP_OnTriggerActivated(WeldableObject);
		WeldableObject->Edict = nullptr;
		return;
	}
}

DynamicMapObjectHandle AIMAP_GetBestTriggerForObject_Internal(const NavAgentProfile* NavProfile, const DynamicMapObject* ObjectToActivate, const Vector& ActivateLocation)
{
	DynamicMapObjectHandle NullHandle = DynamicMapObjectHandle();

	if (!NavProfile || !NavProfile->IsValid()) { return NullHandle; }
	if (vIsZero(ActivateLocation)) { return NullHandle; }
	if (!ObjectToActivate || !ObjectToActivate->IsValid()) { return NullHandle; }

	if (ObjectToActivate->GetNumTriggers() == 0) { return NullHandle; }

	DynamicMapObjectHandle WinningHandle = DynamicMapObjectHandle();
	const DynamicMapObject* WinningTrigger = nullptr;

	Vector FromLoc = ActivateLocation;

	float MinDist = FLT_MAX;

	// This object is triggered by itself, such as a door set to USE_ONLY or a func_plat which needs to be touched to activate
	if (ObjectToActivate->Triggers.size() == 1 && ObjectToActivate->Triggers[0] == ObjectToActivate->Edict)
	{
		return AIMAP_GetDynamicObjectByEdict(ObjectToActivate->Triggers[0]);
	}

	for (auto it = ObjectToActivate->Triggers.begin(); it != ObjectToActivate->Triggers.end(); it++)
	{
		DynamicMapObjectHandle FoundObjectHandle = AIMAP_GetDynamicObjectByEdict(*it);

		if (!FoundObjectHandle.IsValid()) { continue; }

		const DynamicMapObject* ThisTrigger = FoundObjectHandle.GetObjectRef();

		if (!ThisTrigger || !ThisTrigger->bIsActive || !ThisTrigger->IsValid()) { continue; }

		// For triggers we can activate from a distance and are in our LOS, short-cut and add them to the list
		if (ThisTrigger->Type == EAIDynamicMapObjectType::TRIGGER_SHOOT || ThisTrigger->Type == EAIDynamicMapObjectType::TRIGGER_BREAK)
		{
			TraceResult hit;

			UTIL_TraceLine(ActivateLocation + Vector(0.0f, 0.0f, 5.0f), UTIL_GetCentreOfEntity(ThisTrigger->Edict), ignore_monsters, ignore_glass, nullptr, &hit);

			if (hit.pHit == ThisTrigger->Edict)
			{
				float ThisDist = vDist3DSq(FromLoc, UTIL_GetCentreOfEntity(ThisTrigger->Edict));

				if (ThisDist < MinDist)
				{
					WinningTrigger = ThisTrigger;
					WinningHandle = FoundObjectHandle;
					MinDist = ThisDist;
				}

				continue;
			}
		}

		Vector TriggerLocation = AIMAP_GetButtonFloorLocation(NavProfile, FromLoc, ThisTrigger->Edict);

		if (vIsZero(TriggerLocation))
		{
			TriggerLocation = UTIL_GetClosestPointOnEntityToLocation(FromLoc, ThisTrigger->Edict);
		}

		float MaxDist = (ThisTrigger->Type == EAIDynamicMapObjectType::TRIGGER_BREAK || ThisTrigger->Type == EAIDynamicMapObjectType::TRIGGER_SHOOT) ? UTIL_MetresToGoldSrcUnits(5.0f) : 64.0f;

		if (!AINAV_IsPointReachable(NavProfile, FromLoc, TriggerLocation, MaxDist)) { continue; }

		if (ObjectToActivate->Type != EAIDynamicMapObjectType::MAPOBJECT_PLATFORM)
		{
			if (AIMAP_IsPathBlockedByObject(NavProfile, FromLoc, TriggerLocation, ObjectToActivate)) { continue; }
		}
		else
		{
			AvHAIPath CheckPath;

			bool bFoundPath = AINAV_FindPathClosestToPoint(NavProfile, FromLoc, TriggerLocation, &CheckPath, MaxDist);

			if (!bFoundPath) { continue; }

			bool bOtherSideOfLift = false;

			for (int32 i = 0; i < CheckPath.GetPathSize(); i++)
			{
				const AvHAIPathNode* ThisPathNode = CheckPath.GetNodeAtIndex(i);

				if (!ThisPathNode || !ThisPathNode->IsValidMove()) { break; }

				if (ThisPathNode->HasMovementFlag(EAINavMovementFlag::NAV_FLAG_PLATFORM))
				{
					if (AIMAP_GetClosestPlatformToPoints(ThisPathNode->FromLocation, ThisPathNode->ToLocation) == ObjectToActivate)
					{
						bOtherSideOfLift = true;
						break;
					}
				}
			}

			if (bOtherSideOfLift) { continue; }
		}

		float ThisDist = vDist3DSq(FromLoc, TriggerLocation);

		// If we have an alternative trigger that doesn't require welding, then prefer that since it avoids locking out players without a welder
		const bool bCanReplaceWeldTrigger = (WinningTrigger)
			? (WinningTrigger->Type == EAIDynamicMapObjectType::TRIGGER_WELD && ThisTrigger->Type != EAIDynamicMapObjectType::TRIGGER_WELD)
			: true;

		if (ThisDist < MinDist || bCanReplaceWeldTrigger)
		{
			WinningTrigger = ThisTrigger;
			WinningHandle = FoundObjectHandle;
		}
	}

	return WinningHandle;
}

DynamicMapObjectHandle AIMAP_TraceForDynamicObject(const Vector& TraceFrom, const Vector& TraceTo)
{
	TraceResult Hit;
	UTIL_TraceHull(TraceFrom, TraceTo, ignore_monsters, head_hull, nullptr, &Hit);

	return AIMAP_GetDynamicObjectByEdict(Hit.pHit);
}

void AIMAP_OnTriggerActivated(DynamicMapObject* UsedObject)
{
	if (!UsedObject || !UsedObject->IsValid()) { return; }

	if (!UsedObject->bIsActive) { return; }

	UsedObject->NumTimesActivated++;

	UsedObject->LastActivatedTime = gpGlobals->time;

	UsedObject->State = EAIDynamicMapObjectState::OBJECTSTATE_PREPARING;

	for (auto it = UsedObject->Targets.begin(); it != UsedObject->Targets.end(); it++)
	{
		DynamicMapObjectHandle FoundObjectHandle = AIMAP_GetDynamicObjectByEdict(*it);

		if (!FoundObjectHandle.IsValid()) { continue; }

		DynamicMapObject* TargetObject = FoundObjectHandle.GetObjectRef_Mutable();

		if (!TargetObject || !TargetObject->IsValid()) { continue; }

		AIMAP_OnTriggerActivated(TargetObject);
	}
}

void AIMAP_SetDynamicObjectStatus(DynamicMapObject* Object, EAIDynamicMapObjectState NewState)
{
	if (!Object || !Object->IsValid()) { return; }

	if (NewState == Object->State) { return; }

	Object->State = NewState;

	if (NewState == EAIDynamicMapObjectState::OBJECTSTATE_MOVING)
	{
		AIMAP_OnDynamicMapObjectStopIdle(Object);
	}

	if (NewState == EAIDynamicMapObjectState::OBJECTSTATE_IDLE)
	{
		AIMAP_OnDynamicMapObjectBecomeIdle(Object);
	}
}

void AIMAP_ForceRefreshDynamicObjectConnections(DynamicMapObject* Object)
{
	if (!Object || !Object->IsValid()) { return; }

	for (auto it = Object->Targets.begin(); it != Object->Targets.end(); it++)
	{
		DynamicMapObjectHandle FoundObjectHandle = AIMAP_GetDynamicObjectByEdict(*it);

		if (!FoundObjectHandle.IsValid()) { continue; }

		DynamicMapObject* TargetObject = FoundObjectHandle.GetObjectRef_Mutable();

		if (!TargetObject || !TargetObject->IsValid()) { continue; }

		AIMAP_ForceRefreshDynamicObjectConnections(TargetObject);
	}

	Object->UndoOffMeshConnectionChanges();

	if (Object->State == EAIDynamicMapObjectState::OBJECTSTATE_MOVING) { return; }

	if (Object->StopPoints.empty()) { return; }

	if (Object->Type == EAIDynamicMapObjectType::MAPOBJECT_STATIC)
	{
		int CurrStopIndex = (Object->NextStopIndex > 0) ? Object->NextStopIndex - 1 : Object->StopPoints.size() - 1;

		for (auto it = Object->StopPoints[CurrStopIndex].AffectedConnections.begin(); it != Object->StopPoints[CurrStopIndex].AffectedConnections.end();)
		{
			OffMeshConnectionHandle& ThisConnectionHandle = (*it);

			if (!ThisConnectionHandle.IsValid())
			{
				it = Object->StopPoints[CurrStopIndex].AffectedConnections.erase(it);
				continue;
			}

			AIMESH_ModifyOffMeshConnectionFlag(ThisConnectionHandle, EAINavMovementFlag::NAV_FLAG_DISABLED);

			++it;
		}

		return;
	}

	if (Object->StopPoints.empty()) { return; }

	// Door is not permanently blocking anything and can still be activated. We need to do a more nuanced take on how connections are modified
	// The idea is that if a connection is one-way, we detect how we can activate the door from that side and modify the connection flag accordingly
	// Two-way connections will end up being treated like a one-way connection, so use 2 one-way connections if a door requires different capabilities for each side!

	const int CurrStopIndex = (Object->NextStopIndex > 0) ? Object->NextStopIndex - 1 : Object->StopPoints.size() - 1;

	const NavAgentProfile* DefaultBase = GetBaseAgentProfile(EAINavProfileIndex::NAV_PROFILE_DEFAULT);

	NavAgentProfile TestProfile = *DefaultBase;

	for (auto it = Object->StopPoints[CurrStopIndex].AffectedConnections.begin(); it != Object->StopPoints[CurrStopIndex].AffectedConnections.end();)
	{
		OffMeshConnectionHandle& ThisConnectionHandle = (*it);

		if (!ThisConnectionHandle.IsValid())
		{
			it = Object->StopPoints[CurrStopIndex].AffectedConnections.erase(it);
			continue;
		}

		const NavOffMeshConnection* ThisConnection = ThisConnectionHandle.GetConnectionRef();

		if (!ThisConnection)
		{
			it = Object->StopPoints[CurrStopIndex].AffectedConnections.erase(it);
			continue;
		}

		DynamicMapObjectHandle ThisTriggerHandle = AIMAP_GetBestTriggerForObject_Internal(&TestProfile, Object, ThisConnection->FromLocation);

		const DynamicMapObject* ThisTrigger = ThisTriggerHandle.GetObjectRef();

		if (!ThisTrigger || !ThisTrigger->IsValid())
		{
			AIMESH_ModifyOffMeshConnectionFlag(ThisConnectionHandle, EAINavMovementFlag::NAV_FLAG_DISABLED);
		}
		else if (ThisTrigger->Type == EAIDynamicMapObjectType::TRIGGER_WELD)
		{
			AIMESH_ModifyOffMeshConnectionFlag(ThisConnectionHandle, EAINavMovementFlag::NAV_FLAG_WELD);
		}

		++it;
	}
}

void AIMAP_OnDynamicMapObjectBecomeIdle(DynamicMapObject* Object)
{
	if (!Object || !Object->IsValid()) { return; }

	// Object will not move again, so block off all connections permanently, and place null obstacles to block nav mesh
	if (Object->Type == EAIDynamicMapObjectType::MAPOBJECT_STATIC)
	{
		AIMAP_ApplyTempObstaclesToObject(Object, EAINavArea::NAV_AREA_NULL);

		if (Object->StopPoints.empty()) { return; }

		int CurrStopIndex = (Object->NextStopIndex > 0) ? Object->NextStopIndex - 1 : Object->StopPoints.size() - 1;

		for (auto it = Object->StopPoints[CurrStopIndex].AffectedConnections.begin(); it != Object->StopPoints[CurrStopIndex].AffectedConnections.end();)
		{
			OffMeshConnectionHandle& ThisConnectionHandle = (*it);

			if (!ThisConnectionHandle.IsValid())
			{
				it = Object->StopPoints[CurrStopIndex].AffectedConnections.erase(it);
				continue;
			}

			AIMESH_ModifyOffMeshConnectionFlag(ThisConnectionHandle, EAINavMovementFlag::NAV_FLAG_DISABLED);

			++it;
		}

		return;
	}

	if (Object->StopPoints.empty()) { return; }

	// Door is not permanently blocking anything and can still be activated. We need to do a more nuanced take on how connections are modified
	// The idea is that if a connection is one-way, we detect how we can activate the door from that side and modify the connection flag accordingly
	// Two-way connections will end up being treated like a one-way connection, so use 2 one-way connections if a door requires different capabilities for each side!

	const int CurrStopIndex = (Object->NextStopIndex > 0) ? Object->NextStopIndex - 1 : Object->StopPoints.size() - 1;

	const NavAgentProfile* DefaultBase = GetBaseAgentProfile(EAINavProfileIndex::NAV_PROFILE_DEFAULT);

	NavAgentProfile TestProfile = *DefaultBase;

	for (auto it = Object->StopPoints[CurrStopIndex].AffectedConnections.begin(); it != Object->StopPoints[CurrStopIndex].AffectedConnections.end();)
	{
		OffMeshConnectionHandle& ThisConnectionHandle = (*it);

		if (!ThisConnectionHandle.IsValid())
		{
			it = Object->StopPoints[CurrStopIndex].AffectedConnections.erase(it);
			continue;
		}

		const NavOffMeshConnection* ThisConnection = ThisConnectionHandle.GetConnectionRef();

		if (!ThisConnection)
		{
			it = Object->StopPoints[CurrStopIndex].AffectedConnections.erase(it);
			continue;
		}

		DynamicMapObjectHandle ThisTriggerHandle = AIMAP_GetBestTriggerForObject_Internal(&TestProfile, Object, ThisConnection->FromLocation);

		const DynamicMapObject* ThisTrigger = ThisTriggerHandle.GetObjectRef();

		if (!ThisTrigger || !ThisTrigger->IsValid())
		{
			AIMESH_ModifyOffMeshConnectionFlag(ThisConnectionHandle, EAINavMovementFlag::NAV_FLAG_DISABLED);
		}
		else if(ThisTrigger->Type == EAIDynamicMapObjectType::TRIGGER_WELD)
		{
			AIMESH_ModifyOffMeshConnectionFlag(ThisConnectionHandle, EAINavMovementFlag::NAV_FLAG_WELD);
		}

		++it;
	}
}

void AIMAP_OnDynamicMapObjectStopIdle(DynamicMapObject* Object)
{
	Object->UndoOffMeshConnectionChanges();
	Object->ClearTemporaryObstacles();
}

void AIMAP_LinkDynamicMapObjectsToTriggers()
{
	for (auto objectIt = DynamicMapObjects.begin(); objectIt != DynamicMapObjects.end(); objectIt++)
	{
		DynamicMapObject* ThisObject = &objectIt->second;

		if (!ThisObject) { continue; }

		switch (ThisObject->Type)
		{
			case EAIDynamicMapObjectType::MAPOBJECT_DOOR:
			{
				// Gotta use this door to open it, so add the door itself as its own trigger
				if (ThisObject->Edict->v.spawnflags & DOOR_USE_ONLY)
				{
					ThisObject->Triggers.push_back(ThisObject->Edict);
				}
				// Must be one o' them fancy touch-activamated doors. Technically the bot will still try to use the door, but they will touch it in the process so no big deal
				else if (!ThisObject->ObjectName || ThisObject->ObjectName[0] == '\0')
				{
					ThisObject->Triggers.push_back(ThisObject->Edict);
				}
			}
			break;

			case EAIDynamicMapObjectType::MAPOBJECT_PLATFORM:
			{
				if (FClassnameIs(ThisObject->Edict, "func_plat"))
				{
					ThisObject->Triggers.push_back(ThisObject->Edict);
				}
			}
			break;

			case EAIDynamicMapObjectType::TRIGGER_USE:
			case EAIDynamicMapObjectType::TRIGGER_TOUCH:
			case EAIDynamicMapObjectType::TRIGGER_SHOOT:
			case EAIDynamicMapObjectType::TRIGGER_BREAK:
			case EAIDynamicMapObjectType::TRIGGER_WELD:
			{
				for (auto TargetsIt = DynamicMapObjects.begin(); TargetsIt != DynamicMapObjects.end(); TargetsIt++)
				{
					DynamicMapObject* OtherObject = &TargetsIt->second;

					if (OtherObject == ThisObject) { continue; }

					vector<edict_t*> CheckedObjects;

					const EAITriggerLinkType LinkType = AIMAP_IsDynamicMapTriggerLinkedToObject(ThisObject->Edict, OtherObject->Edict, CheckedObjects);

					if (LinkType != EAITriggerLinkType::NOT_LINKED)
					{
						OtherObject->Triggers.push_back(ThisObject->Edict);

						if (LinkType == EAITriggerLinkType::LINKED_DIRECT)
						{
							ThisObject->Targets.push_back(OtherObject->Edict);
						}
					}
				}
			}
			break;

			default:
				break;
		}
	}
}

void AIMAP_LinkDynamicMapObjectsToOffMeshConnections()
{
	const int InvalidIndex = static_cast<int>(EAINavMeshIndex::NAV_MESH_INVALID);

	for (int i = 0; i < InvalidIndex; i++)
	{
		const EAINavMeshIndex MeshIndex = static_cast<EAINavMeshIndex>(i);

		NavMesh* FoundNavMesh = AIMESH_GetNavMeshAtIndex(MeshIndex);

		if (!FoundNavMesh) { continue; }

		for (auto it = FoundNavMesh->MeshConnectionHandles.begin(); it != FoundNavMesh->MeshConnectionHandles.end(); it++)
		{
			const OffMeshConnectionHandle& ThisConnectionHandle = (*it);

			if (!ThisConnectionHandle.IsValid()) { continue; }

			NavOffMeshConnection* ThisConnection = ThisConnectionHandle.GetConnectionRef_Mutable();

			if (!ThisConnection || !ThisConnection->IsValid()) { continue; }

			if (ThisConnection->HasDefaultFlag(EAINavMovementFlag::NAV_FLAG_PLATFORM))
			{
				const DynamicMapObject* NearestPlatform = AIMAP_GetClosestPlatformToPoints(ThisConnection->FromLocation, ThisConnection->ToLocation);

				if (NearestPlatform)
				{
					ThisConnection->LinkedObject = NearestPlatform->Edict;
				}
			}
		}
	}
}

void AIMAP_SetTrainStartPoints()
{
	for (auto it = DynamicMapObjects.begin(); it != DynamicMapObjects.end(); it++)
	{
		DynamicMapObject* ThisObject = &it->second;

		if (!ThisObject) { continue; }

		// We are only interested in trains which have more than 2 stop points and are using path corners.
		// If the train is acting like a regular up/down lift then we will just treat it like a regular platform.
		if (ThisObject->Type != EAIDynamicMapObjectType::MAPOBJECT_TRAIN
			|| ThisObject->StopPoints.size() <= 2
			|| ThisObject->Targets.size() == 0) { continue; }


		edict_t* TargetEdict = ThisObject->Targets[0];
		const char* TargetEdictName = STRING(TargetEdict->v.targetname);

		int CurrIndex = 0;

		for (auto stopIt = ThisObject->StopPoints.begin(); stopIt != ThisObject->StopPoints.end(); stopIt++)
		{
			if (stopIt->CornerEdict == TargetEdict)
			{
				break;
			}

			CurrIndex++;
		}

		ThisObject->NextStopIndex = CurrIndex + 1;

		if (ThisObject->NextStopIndex >= ThisObject->StopPoints.size())
		{
			ThisObject->NextStopIndex = 0;
		}
	}
}

bool AIMAP_IsPathBlockedByObject(const NavAgentProfile* NavProfile, const Vector StartLoc, const Vector EndLoc, const DynamicMapObject* SearchObject)
{
	if (UTIL_IsPointInSwimArea(StartLoc) && UTIL_IsPointInSwimArea(EndLoc))
	{
		if (UTIL_QuickHullTrace(nullptr, StartLoc, EndLoc))
		{
			return vlineIntersectsAABB(StartLoc, EndLoc, SearchObject->Edict->v.absmin, SearchObject->Edict->v.absmax);
		}
	}

	Vector ValidNavmeshPoint = AIMESH_ProjectPointToNavmesh(NavProfile, EndLoc);

	if (UTIL_IsPointInSwimArea(EndLoc))
	{
		TraceResult Hit;
		UTIL_TraceLine(EndLoc, EndLoc - Vector(0.0f, 0.0f, 1000.0f), ignore_monsters, nullptr, &Hit);

		if (Hit.flFraction < 1.0f)
		{
			ValidNavmeshPoint = AIMESH_ProjectPointToNavmesh(NavProfile, Hit.vecEndPos);
		}
	}

	if (!ValidNavmeshPoint)
	{
		return false;
	}

	AvHAIPath TestPath;

	// Now we find a path backwards from the valid nav mesh point to our location, trying to get as close as we can to it

	const bool bFoundPath = AINAV_FindPathClosestToPoint(NavProfile, StartLoc, ValidNavmeshPoint, &TestPath, 50.0f);

	if (bFoundPath)
	{
		for (int32 i = 0; i < TestPath.GetPathSize(); i++)
		{
			const AvHAIPathNode* ThisPathNode = TestPath.GetNodeAtIndex(i);

			if (!ThisPathNode || !ThisPathNode->IsValidMove()) { break; }

			// Check that this path does not require us to go up the very platform we are checking for blockages
			if (ThisPathNode->MovementFlag == EAINavMovementFlag::NAV_FLAG_PLATFORM)
			{
				DynamicMapObjectHandle RetrievedHandle = AIMAP_GetDynamicObjectByEdict(ThisPathNode->MovementObject);

				if (RetrievedHandle.IsValid())
				{
					const DynamicMapObject* FoundObject = RetrievedHandle.GetObjectRef();

					if (FoundObject == SearchObject) { return true; }
				}
			}

			if (AIMAP_GetObjectBlockingPathPoint(ThisPathNode->FromLocation, ThisPathNode->ToLocation, ThisPathNode->MovementFlag, SearchObject, nullptr) != nullptr)
			{
				return true;
			}
		}

		return false;
	}

	return true;
}

const DynamicMapObject* AIMAP_GetObjectBlockingPathPoint(const Vector FromLocation, const Vector ToLocation, const EAINavMovementFlag MovementFlag, const DynamicMapObject* SearchObject, const DynamicMapObject* IgnoreObject)
{
	if (IsFlagTeleportType((EAINavMovementFlag)MovementFlag)) { return nullptr; }

	Vector FromLoc = FromLocation;
	Vector ToLoc = ToLocation;

	TraceResult doorHit;

	if (MovementFlag == EAINavMovementFlag::NAV_FLAG_LADDER)
	{
		Vector TargetLoc = (ToLocation.z > FromLocation.z) ? Vector(FromLoc.x, FromLoc.y, ToLoc.z) : Vector(ToLoc.x, ToLoc.y, FromLoc.z);

		if (SearchObject != nullptr)
		{
			if (vlineIntersectsAABB(FromLoc, TargetLoc, SearchObject->Edict->v.absmin, SearchObject->Edict->v.absmax))
			{
				return SearchObject;
			}
		}
		else
		{
			for (auto it = DynamicMapObjects.begin(); it != DynamicMapObjects.end(); it++)
			{
				const DynamicMapObject* ThisObject = &it->second;

				if (!ThisObject) { continue; }

				if (ThisObject->Type == EAIDynamicMapObjectType::MAPOBJECT_PLATFORM || (IgnoreObject && ThisObject->Edict == IgnoreObject->Edict)) { continue; }

				if (vlineIntersectsAABB(FromLoc, TargetLoc, ThisObject->Edict->v.absmin, ThisObject->Edict->v.absmax))
				{
					return ThisObject;
				}
			}
		}

		if (SearchObject != nullptr)
		{
			if (vlineIntersectsAABB(TargetLoc, ToLoc, SearchObject->Edict->v.absmin, SearchObject->Edict->v.absmax))
			{
				return SearchObject;
			}
		}
		else
		{
			for (auto it = DynamicMapObjects.begin(); it != DynamicMapObjects.end(); it++)
			{
				const DynamicMapObject* ThisObject = &it->second;

				if (!ThisObject) { continue; }

				if (ThisObject->Type == EAIDynamicMapObjectType::MAPOBJECT_PLATFORM || (IgnoreObject && ThisObject->Edict == IgnoreObject->Edict)) { continue; }

				if (vlineIntersectsAABB(TargetLoc, ToLoc, ThisObject->Edict->v.absmin, ThisObject->Edict->v.absmax))
				{
					return ThisObject;
				}
			}
		}

	}
	else if (MovementFlag == EAINavMovementFlag::NAV_FLAG_FALL)
	{
		Vector TargetLoc = Vector(ToLoc.x, ToLoc.y, FromLoc.z);

		if (SearchObject != nullptr)
		{
			if (vlineIntersectsAABB(FromLoc, TargetLoc, SearchObject->Edict->v.absmin, SearchObject->Edict->v.absmax))
			{
				return SearchObject;
			}
		}
		else
		{
			for (auto it = DynamicMapObjects.begin(); it != DynamicMapObjects.end(); it++)
			{
				const DynamicMapObject* ThisObject = &it->second;

				if (!ThisObject) { continue; }

				if (ThisObject->Type == EAIDynamicMapObjectType::MAPOBJECT_PLATFORM || (IgnoreObject && ThisObject->Edict == IgnoreObject->Edict)) { continue; }

				if (vlineIntersectsAABB(FromLoc, TargetLoc, ThisObject->Edict->v.absmin, ThisObject->Edict->v.absmax))
				{
					return ThisObject;
				}
			}
		}

		if (SearchObject != nullptr)
		{
			if (vlineIntersectsAABB(TargetLoc, ToLoc, SearchObject->Edict->v.absmin, SearchObject->Edict->v.absmax))
			{
				return SearchObject;
			}
		}
		else
		{
			for (auto it = DynamicMapObjects.begin(); it != DynamicMapObjects.end(); it++)
			{
				const DynamicMapObject* ThisObject = &it->second;

				if (!ThisObject) { continue; }

				if (ThisObject->Type == EAIDynamicMapObjectType::MAPOBJECT_PLATFORM || (IgnoreObject && ThisObject->Edict == IgnoreObject->Edict)) { continue; }

				if (vlineIntersectsAABB(TargetLoc, ToLoc, ThisObject->Edict->v.absmin, ThisObject->Edict->v.absmax))
				{
					return ThisObject;
				}
			}
		}

	}

	Vector TargetLoc = ToLoc + Vector(0.0f, 0.0f, 10.0f);

	if (SearchObject != nullptr)
	{
		if (vlineIntersectsAABB(FromLoc, TargetLoc, SearchObject->Edict->v.absmin, SearchObject->Edict->v.absmax))
		{
			return SearchObject;
		}
	}
	else
	{
		for (auto it = DynamicMapObjects.begin(); it != DynamicMapObjects.end(); it++)
		{
			const DynamicMapObject* ThisObject = &it->second;

			if (!ThisObject) { continue; }

			if (ThisObject->Type == EAIDynamicMapObjectType::MAPOBJECT_PLATFORM || (IgnoreObject && ThisObject->Edict == IgnoreObject->Edict)) { continue; }

			if (vlineIntersectsAABB(FromLoc, TargetLoc, ThisObject->Edict->v.absmin, ThisObject->Edict->v.absmax))
			{
				return ThisObject;
			}
		}
	}

	return nullptr;
}

Vector AIMAP_GetButtonFloorLocation(const NavAgentProfile* NavProfile, const Vector UserLocation, const edict_t* ButtonEdict)
{
	if (UTIL_IsPointInSwimArea(UserLocation))
	{
		Vector NearestTriggerPoint = UTIL_GetClosestPointOnEntityToLocation(UserLocation, ButtonEdict);

		TraceResult Hit;

		UTIL_TraceHull(UserLocation, NearestTriggerPoint, ignore_monsters, head_hull, nullptr, &Hit);

		if (Hit.fInWater)
		{
			if (vDist3DSq(NearestTriggerPoint, Hit.vecEndPos) < sqrf(max_ai_use_reach)) { return Hit.vecEndPos; }
		}
	}

	Vector ClosestPoint = ZERO_VECTOR;

	if (ButtonEdict->v.size.x > 64.0f || ButtonEdict->v.size.y > 64.0f)
	{
		ClosestPoint = UTIL_GetClosestPointOnEntityToLocation(UserLocation, ButtonEdict);
	}
	else
	{
		ClosestPoint = UTIL_GetCentreOfEntity(ButtonEdict);
	}

	if (UTIL_IsPointInSwimArea(ClosestPoint))
	{
		return ClosestPoint;
	}

	Vector ButtonAccessPoint = AIMESH_ProjectPointToNavmesh(NavProfile, ClosestPoint, Vector(100.0f, 100.0f, 100.0f));

	if (vIsZero(ButtonAccessPoint))
	{
		ButtonAccessPoint = ClosestPoint;
	}

	Vector PlayerAccessLoc = ButtonAccessPoint;

	if (ButtonAccessPoint.z > ClosestPoint.z)
	{
		PlayerAccessLoc.z += 18.0f;
	}
	else
	{
		PlayerAccessLoc.z += 36.0f;
	}

	if (fabsf(PlayerAccessLoc.z - ClosestPoint.z) <= max_ai_use_reach)
	{
		return ButtonAccessPoint;
	}

	Vector NewProjection = ClosestPoint;

	if (ButtonAccessPoint.z > ClosestPoint.z)
	{
		NewProjection = ClosestPoint - Vector(0.0f, 0.0f, 100.0f);
	}
	else
	{
		NewProjection = ClosestPoint + Vector(0.0f, 0.0f, 100.0f);
	}

	Vector NewButtonAccessPoint = AIMESH_ProjectPointToNavmesh(NavProfile, NewProjection);

	if (vIsZero(NewButtonAccessPoint))
	{
		NewButtonAccessPoint = ClosestPoint;
	}
	else
	{
		if (UTIL_IsPointInSwimArea(NewButtonAccessPoint))
		{
			Vector NewClosestPoint = NewButtonAccessPoint + ((ClosestPoint - NewButtonAccessPoint) * 0.95f);

			if (UTIL_IsPointInSwimArea(NewClosestPoint))
			{
				NewButtonAccessPoint = NewClosestPoint;
			}
		}
	}

	return NewButtonAccessPoint;
}

const DynamicMapObject* AIMAP_GetClosestPlatformToPoints(const Vector StartPoint, const Vector EndPoint)
{
	const DynamicMapObject* Result = nullptr;

	float minDist = 0.0f;

	for (auto it = DynamicMapObjects.begin(); it != DynamicMapObjects.end(); it++)
	{
		const DynamicMapObject* ThisObject = &it->second;

		if (!ThisObject) { continue; }

		if (ThisObject->Type != EAIDynamicMapObjectType::MAPOBJECT_DOOR && ThisObject->Type != EAIDynamicMapObjectType::MAPOBJECT_PLATFORM && ThisObject->Type != EAIDynamicMapObjectType::MAPOBJECT_TRAIN) { continue; }

		float distTopPoint = FLT_MAX;
		float distBottomPoint = FLT_MAX;

		for (auto stop = ThisObject->StopPoints.begin(); stop != ThisObject->StopPoints.end(); stop++)
		{
			distTopPoint = fminf(distTopPoint, vDist3D(UTIL_GetClosestPointOnEntityToLocation(StartPoint, ThisObject->Edict, (*stop).StopLocation), StartPoint));
			distBottomPoint = fminf(distBottomPoint, vDist3D(UTIL_GetClosestPointOnEntityToLocation(EndPoint, ThisObject->Edict, (*stop).StopLocation), EndPoint));
		}

		// Get the average distance from our desired start and end points, whichever scores lowest is probably the lift/train/door we want to ride
		float thisDist = fminf(distTopPoint, distBottomPoint);

		if (!Result || thisDist < minDist)
		{
			Result = ThisObject;
			minDist = thisDist;
		}
	}

	return Result;
}

DynamicMapObjectHandle AIMAP_FindObjectBlockingPathPoint(const AvHAIPathNode* PathNode, const DynamicMapObjectHandle& IgnoreObjectHandle)
{
	DynamicMapObjectHandle NullHandle = DynamicMapObjectHandle();

	if (!PathNode || !PathNode->IsValidMove() || PathNode->IsTeleportMove()) { return NullHandle; }

	Vector FromLoc = PathNode->FromLocation;
	Vector ToLoc = PathNode->ToLocation;

	TraceResult doorHit;

	const DynamicMapObject* IgnoreObject = IgnoreObjectHandle.GetObjectRef();

	if (PathNode->HasMovementFlag(EAINavMovementFlag::NAV_FLAG_LADDER | EAINavMovementFlag::NAV_FLAG_WALLCLIMB))
	{
		const Vector TargetLoc = (ToLoc.z > FromLoc.z) ? Vector(FromLoc.x, FromLoc.y, ToLoc.z) : Vector(ToLoc.x, ToLoc.y, FromLoc.z);

		for (auto it = DynamicMapObjectHandles.begin(); it != DynamicMapObjectHandles.end(); it++)
		{
			const DynamicMapObjectHandle ThisObjectHandle = *it;

			const DynamicMapObject* ThisObject = ThisObjectHandle.GetObjectRef();

			if (!ThisObject) { continue; }

			if (ThisObject->Type == EAIDynamicMapObjectType::MAPOBJECT_PLATFORM || (IgnoreObject && ThisObject->Edict == IgnoreObject->Edict)) { continue; }

			if (vlineIntersectsAABB(FromLoc, TargetLoc, ThisObject->Edict->v.absmin, ThisObject->Edict->v.absmax))
			{
				return ThisObjectHandle;
			}

			if (vlineIntersectsAABB(TargetLoc, ToLoc, ThisObject->Edict->v.absmin, ThisObject->Edict->v.absmax))
			{
				return ThisObjectHandle;
			}
		}

		return NullHandle;
	}

	if (PathNode->HasMovementFlag(EAINavMovementFlag::NAV_FLAG_FALL))
	{
		const Vector TargetLoc = Vector(ToLoc.x, ToLoc.y, FromLoc.z);

		for (auto it = DynamicMapObjectHandles.begin(); it != DynamicMapObjectHandles.end(); it++)
		{
			const DynamicMapObjectHandle ThisObjectHandle = *it;

			const DynamicMapObject* ThisObject = ThisObjectHandle.GetObjectRef();

			if (!ThisObject) { continue; }

			if (ThisObject->Type == EAIDynamicMapObjectType::MAPOBJECT_PLATFORM || (IgnoreObject && ThisObject == IgnoreObject)) { continue; }

			if (vlineIntersectsAABB(FromLoc, TargetLoc, ThisObject->Edict->v.absmin, ThisObject->Edict->v.absmax))
			{
				return ThisObjectHandle;
			}

			if (vlineIntersectsAABB(TargetLoc, ToLoc, ThisObject->Edict->v.absmin, ThisObject->Edict->v.absmax))
			{
				return ThisObjectHandle;
			}
		}

		return NullHandle;
	}

	Vector TargetLoc = FromLoc + (ToLoc - FromLoc) * 0.5f;
	TargetLoc.z = fmaxf(FromLoc.z, ToLoc.z);

	for (auto it = DynamicMapObjectHandles.begin(); it != DynamicMapObjectHandles.end(); it++)
	{
		const DynamicMapObjectHandle ThisObjectHandle = *it;

		const DynamicMapObject* ThisObject = ThisObjectHandle.GetObjectRef();

		if (!ThisObject) { continue; }

		if (ThisObject->Type == EAIDynamicMapObjectType::MAPOBJECT_PLATFORM || (IgnoreObject && ThisObject == IgnoreObject)) { continue; }

		if (vlineIntersectsAABB(FromLoc, TargetLoc, ThisObject->Edict->v.absmin, ThisObject->Edict->v.absmax))
		{
			return ThisObjectHandle;
		}

		if (vlineIntersectsAABB(TargetLoc, ToLoc, ThisObject->Edict->v.absmin, ThisObject->Edict->v.absmax))
		{
			return ThisObjectHandle;
		}
	}

	return NullHandle;
}

void AIMAP_PopulateAllConnectionsAffectedByDynamicObjects()
{
	for (auto ObjectIt = DynamicMapObjects.begin(); ObjectIt != DynamicMapObjects.end(); ObjectIt++)
	{
		DynamicMapObject* ThisObject = &ObjectIt->second;

		if (!ThisObject || !ThisObject->IsValid()) { continue; }

		DynamicMapObjectHandle NewHandle = DynamicMapObjectHandle(ThisObject);

		AIMAP_PopulateConnectionsAffectedByDynamicObject(NewHandle);
	}
}

void AIMAP_PopulateConnectionsAffectedByDynamicObject(DynamicMapObjectHandle& ObjectHandle)
{
	if (!ObjectHandle.IsValid()) { return; }

	DynamicMapObject* Object = ObjectHandle.GetObjectRef_Mutable();

	const int InvalidIndex = static_cast<int>(EAINavMeshIndex::NAV_MESH_INVALID);

	for (auto stopIt = Object->StopPoints.begin(); stopIt != Object->StopPoints.end(); stopIt++)
	{
		DynamicMapObjectStop* ThisStop = &(*stopIt);

		if (!ThisStop) { continue; }

		ThisStop->AffectedConnections.clear();
	}

	for (int i = 0; i < InvalidIndex; i++)
	{
		const EAINavMeshIndex MeshIndex = static_cast<EAINavMeshIndex>(i);

		NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(MeshIndex);

		if (!FoundMesh) { continue; }

		for (auto stopIt = Object->StopPoints.begin(); stopIt != Object->StopPoints.end(); stopIt++)
		{
			DynamicMapObjectStop* ThisStop = &(*stopIt);

			if (!ThisStop) { continue; }

			Vector ObjectCentre = ThisStop->StopLocation;

			for (auto it = FoundMesh->MeshConnectionHandles.begin(); it != FoundMesh->MeshConnectionHandles.end(); it++)
			{
				const OffMeshConnectionHandle& TestConnectionHandle = *it;

				if (!TestConnectionHandle.IsValid()) { continue; }

				if (AIMAP_IsOffMeshConnectionAffectedByObject(ObjectHandle, ObjectCentre, TestConnectionHandle))
				{
					ThisStop->AffectedConnections.push_back(TestConnectionHandle);
				}
			}
		}
	}
}

bool AIMAP_IsOffMeshConnectionAffectedByObject(const DynamicMapObjectHandle& TestObjectHandle, const Vector& ObjectPosition, const OffMeshConnectionHandle& ConnectionHandle)
{
	if (!TestObjectHandle.IsValid() || !ConnectionHandle.IsValid()) { return false; }

	const NavOffMeshConnection* Connection = ConnectionHandle.GetConnectionRef();

	if (!Connection) { return false; }

	EAINavMovementFlag MovementTypes = static_cast<EAINavMovementFlag>(Connection->ConnectionFlags);

	if (IsFlagTeleportType(MovementTypes)) { return false; }

	const DynamicMapObject* TestObject = TestObjectHandle.GetObjectRef();

	if (!TestObject) { return false; }

	if (TestObject->Type == EAIDynamicMapObjectType::TRIGGER_TOUCH) { return false; }

	if (TestObject->Edict->v.solid == SOLID_NOT) { return false; }

	Vector HalfExtents = (TestObject->Edict->v.size * 0.5f);
	HalfExtents.x += 16.0f;
	HalfExtents.y += 16.0f;
	HalfExtents.z += 16.0f;

	Vector ConnStart = Connection->FromLocation + Vector(0.0f, 0.0f, 15.0f);
	Vector ConnEnd = Connection->ToLocation + Vector(0.0f, 0.0f, 15.0f);
	Vector MidPoint = ConnStart + ((ConnEnd - ConnStart) * 0.5f);
	MidPoint.z = fmaxf(ConnStart.z, ConnEnd.z);

	if (vlineIntersectsAABB(ConnStart, MidPoint, ObjectPosition - HalfExtents, ObjectPosition + HalfExtents))
	{
		return true;
	}

	if (vlineIntersectsAABB(MidPoint, ConnEnd, ObjectPosition - HalfExtents, ObjectPosition + HalfExtents))
	{
		return true;
	}

	return false;
}

EAITriggerLinkType AIMAP_IsDynamicMapTriggerLinkedToObject(edict_t* TriggerObject, edict_t* TargetObject, vector<edict_t*> CheckedObjects)
{
	if (FNullEnt(TriggerObject) || FNullEnt(TargetObject)) { return EAITriggerLinkType::NOT_LINKED; }

	if (TriggerObject == TargetObject) { return EAITriggerLinkType::LINKED_DIRECT; }

	CheckedObjects.push_back(TriggerObject);

	CBaseEntity* TriggerObjectRef = CBaseEntity::Instance(TriggerObject);
	CBaseEntity* TargetObjectRef = CBaseEntity::Instance(TargetObject);

	if (!TriggerObjectRef || !TargetObjectRef) { return EAITriggerLinkType::NOT_LINKED; }

	// Check the trigger's target for a direct reference, walk down the chain
	if (TriggerObject->v.target)
	{
		const char* Target = STRING(TriggerObject->v.target);

		CBaseEntity* CurrentTarget = UTIL_FindEntityByTargetname(NULL, Target);

		if (CurrentTarget != NULL && CurrentTarget == TargetObjectRef)
		{
			return EAITriggerLinkType::LINKED_DIRECT;
		}

		while (CurrentTarget != NULL)
		{
			edict_t* TargetEdict = CurrentTarget->edict();
			if (!FNullEnt(TargetEdict))
			{
				if (find(CheckedObjects.begin(), CheckedObjects.end(), TargetEdict) == CheckedObjects.end())
				{
					const EAITriggerLinkType LinkType = AIMAP_IsDynamicMapTriggerLinkedToObject(TargetEdict, TargetObject, CheckedObjects);

					if (LinkType != EAITriggerLinkType::NOT_LINKED) { return EAITriggerLinkType::LINKED_INDIRECT; }
				}
			}

			CurrentTarget = UTIL_FindEntityByTargetname(CurrentTarget, Target);
		}
	}

	// If the target is locked by us, then we are linked of course
	if (CBaseToggle* TargetToggleRef = dynamic_cast<CBaseToggle*>(TargetObjectRef))
	{
		CBaseEntity* MasterEntity = UTIL_FindEntityByTargetname(NULL, STRING(TargetToggleRef->m_sMaster));

		if (MasterEntity == TriggerObjectRef) { return EAITriggerLinkType::LINKED_INDIRECT; }
	}

	// If we are a multi manager, check all our targets
	if (CMultiManager* MultiManagerRef = dynamic_cast<CMultiManager*>(TriggerObjectRef))
	{
		for (int i = 0; i < MultiManagerRef->m_cTargets; i++)
		{
			CBaseEntity* MMTargetEntity = UTIL_FindEntityByTargetname(NULL, STRING(MultiManagerRef->m_iTargetName[i]));

			if (!MMTargetEntity) { continue; }

			edict_t* MMTargetEdict = MMTargetEntity->edict();

			if (FNullEnt(MMTargetEdict)) { continue; }

			if (find(CheckedObjects.begin(), CheckedObjects.end(), MMTargetEdict) != CheckedObjects.end()) { continue; }

			const EAITriggerLinkType LinkedType = AIMAP_IsDynamicMapTriggerLinkedToObject(MMTargetEdict, TargetObject, CheckedObjects);

			if (LinkedType != EAITriggerLinkType::NOT_LINKED) { return EAITriggerLinkType::LINKED_INDIRECT; }
		}

		return EAITriggerLinkType::NOT_LINKED;
	}

	if (CEnvGlobal* EnvGlobalRef = dynamic_cast<CEnvGlobal*>(TriggerObjectRef))
	{
		edict_t* pent = FIND_ENTITY_BY_CLASSNAME(NULL, "multisource");

		while (!FNullEnt(pent))
		{
			CMultiSource* MultiSourceRef = dynamic_cast<CMultiSource*>(CBaseEntity::Instance(pent));

			if (MultiSourceRef)
			{
				const char* MSState = STRING(MultiSourceRef->m_globalstate);
				const char* EnvState = STRING(EnvGlobalRef->m_globalstate);

				if (FStrEq(MSState, EnvState))
				{
					if (find(CheckedObjects.begin(), CheckedObjects.end(), pent) == CheckedObjects.end())
					{
						const EAITriggerLinkType LinkedType = AIMAP_IsDynamicMapTriggerLinkedToObject(pent, TargetObject, CheckedObjects);

						if (LinkedType != EAITriggerLinkType::NOT_LINKED) { return EAITriggerLinkType::LINKED_INDIRECT; }
					}
				}
			}

			pent = FIND_ENTITY_BY_CLASSNAME(pent, "multisource");
		}

		return EAITriggerLinkType::NOT_LINKED;
	}

	if (AvHWeldable* WeldableRef = dynamic_cast<AvHWeldable*>(TriggerObjectRef))
	{
		string targetString = WeldableRef->GetTargetOnFinish();
		const char* targetOnFinish = targetString.c_str();
		CBaseEntity* TargetOnFinish = UTIL_FindEntityByTargetname(NULL, targetOnFinish);

		if (!TargetOnFinish) { return EAITriggerLinkType::NOT_LINKED; }

		edict_t* TargetOnFinishEdict = TargetOnFinish->edict();

		if (FNullEnt(TargetOnFinishEdict)) { return EAITriggerLinkType::NOT_LINKED; }

		if (TargetOnFinishEdict == TargetObject) { return EAITriggerLinkType::LINKED_DIRECT; }

		if (find(CheckedObjects.begin(), CheckedObjects.end(), TargetOnFinishEdict) == CheckedObjects.end())
		{
			const EAITriggerLinkType LinkedType = AIMAP_IsDynamicMapTriggerLinkedToObject(TargetOnFinishEdict, TargetObject, CheckedObjects);

			if (LinkedType != EAITriggerLinkType::NOT_LINKED) { return EAITriggerLinkType::LINKED_INDIRECT; }
		}

		return EAITriggerLinkType::NOT_LINKED;
	}

	if (CMultiSource* MultiSourceRef = dynamic_cast<CMultiSource*>(TriggerObjectRef))
	{
		for (auto it = DynamicMapObjects.begin(); it != DynamicMapObjects.end(); it++)
		{
			const DynamicMapObject* ThisObject = &it->second;

			if (!ThisObject) { continue; }

			if (ThisObject->Edict == TriggerObject) { continue; }
			if (find(CheckedObjects.begin(), CheckedObjects.end(), ThisObject->Edict) != CheckedObjects.end()) { continue; }

			CBaseToggle* ToggleObjectRef = dynamic_cast<CBaseToggle*>(CBaseEntity::Instance(ThisObject->Edict));

			if (!ToggleObjectRef) { continue; }

			CBaseEntity* ToggleObjectMaster = UTIL_FindEntityByTargetname(NULL, STRING(ToggleObjectRef->m_sMaster));

			if (!ToggleObjectMaster || ToggleObjectMaster != TriggerObjectRef) { continue; }

			const EAITriggerLinkType LinkedType = AIMAP_IsDynamicMapTriggerLinkedToObject(ToggleObjectRef->edict(), TargetObject, CheckedObjects);

			if (LinkedType != EAITriggerLinkType::NOT_LINKED) { return EAITriggerLinkType::LINKED_INDIRECT; }
		}

		return EAITriggerLinkType::NOT_LINKED;
	}

	return EAITriggerLinkType::NOT_LINKED;
}

DynamicMapObjectHandle AIMAP_GetDynamicObjectByEdict(const edict_t* SearchEdict)
{
	DynamicMapObjectHandle NullHandle = DynamicMapObjectHandle();

	if (!SearchEdict || FNullEnt(SearchEdict)) { return NullHandle; }

	for (auto it = DynamicMapObjectHandles.begin(); it != DynamicMapObjectHandles.end(); it++)
	{
		DynamicMapObjectHandle ThisHandle = *it;

		const DynamicMapObject* ThisObject = ThisHandle.GetObjectRef();

		if (!ThisObject) { continue; }

		if (ThisObject->Edict == SearchEdict)
		{
			return ThisHandle;
		}
	}

	return NullHandle;
}

void AIMAP_ApplyTempObstaclesToObject(DynamicMapObject* Object, EAINavArea Area)
{
	if (!Object) { return; }

	Object->ClearTemporaryObstacles();

	if (!Object->IsValid()) { return; }

	float SizeX = Object->Edict->v.size.x;
	float SizeY = Object->Edict->v.size.y;
	float SizeZ = Object->Edict->v.size.z;

	bool bUseXAxis = (SizeX >= SizeY);

	float CylinderRadius = fminf(SizeX, SizeY) * 0.5f;

	if (CylinderRadius < 1.0f) { return; }

	float Ratio = (bUseXAxis) ? (SizeX / (CylinderRadius * 2.0f)) : (SizeY / (CylinderRadius * 2.0f));

	int NumObstacles = (int)ceil(Ratio);

	Vector Dir = (bUseXAxis) ? RIGHT_VECTOR : FWD_VECTOR;

	Vector StartPoint = UTIL_GetCentreOfEntity(Object->Edict);

	if (bUseXAxis)
	{
		StartPoint.x = Object->Edict->v.absmin.x + CylinderRadius;
	}
	else
	{
		StartPoint.y = Object->Edict->v.absmin.y + CylinderRadius;
	}

	StartPoint.z -= 25.0f;

	Vector CurrentPoint = StartPoint;

	for (int ii = 0; ii < NumObstacles; ii++)
	{
		const int InvalidIndex = static_cast<int>(EAINavMeshIndex::NAV_MESH_INVALID);

		for (int NavIndex = 0; NavIndex < InvalidIndex; NavIndex++)
		{
			const EAINavMeshIndex MeshIndex = static_cast<EAINavMeshIndex>(NavIndex);

			NavTempObstacleHandle NewObstacle = AIMESH_AddTemporaryObstacle(MeshIndex, CurrentPoint, CylinderRadius, SizeZ, Area);

			if (NewObstacle.IsValid())
			{
				Object->TempObstacles.push_back(NewObstacle);
			}
		}

		if (bUseXAxis)
		{
			CurrentPoint.x += CylinderRadius * 2.0f;
		}
		else
		{
			CurrentPoint.y += CylinderRadius * 2.0f;
		}
	}
}

void AIMAP_RemoveAllTempObstaclesFromObject(DynamicMapObject* Object)
{
	if (!Object) { return; }

	for (auto ObsIt = Object->TempObstacles.begin(); ObsIt != Object->TempObstacles.end(); ObsIt++)
	{
		AIMESH_RemoveTemporaryObstacle(*ObsIt);
	}

	Object->TempObstacles.clear();
}

void AIMAP_ClearCachedMapData()
{
	if (!AIMESH_IsNavMeshLoaded())
	{
		DynamicMapObjects.clear();
		return;
	}

	for (auto ObjIt = DynamicMapObjects.begin(); ObjIt != DynamicMapObjects.end(); ObjIt++)
	{
		DynamicMapObject* ThisObject = &ObjIt->second;

		if (!ThisObject || !ThisObject->IsValid()) { continue; }

		ThisObject->UndoOffMeshConnectionChanges();
		ThisObject->ClearTemporaryObstacles();
	}

	DynamicMapObjects.clear();
}

Vector AIMAP_GetNearestPlatformDisembarkPoint(const NavAgentProfile* NavProfile, const Vector& RiderPosition, const DynamicMapObjectHandle& LiftObjectHandle)
{
	if (!LiftObjectHandle.IsValid()) { return ZERO_VECTOR; }

	OffMeshConnectionHandle NearestConnectionHandle = OffMeshConnectionHandle();
	float MinDist = 0.0f;

	NavMesh* ChosenNavMesh = AIMESH_GetNavMeshAtIndex(NavProfile->MeshIndex);

	if (!ChosenNavMesh) { return ZERO_VECTOR; }

	const DynamicMapObject* LiftReference = LiftObjectHandle.GetObjectRef();

	if (!LiftReference) { return ZERO_VECTOR; }

	for (auto it = ChosenNavMesh->MeshConnectionHandles.begin(); it != ChosenNavMesh->MeshConnectionHandles.end(); it++)
	{
		const OffMeshConnectionHandle& ThisConnectionHandle = *it;

		if (!ThisConnectionHandle.IsValid()) { continue; }

		const NavOffMeshConnection* ThisConnection = ThisConnectionHandle.GetConnectionRef();

		if (!ThisConnection || !ThisConnection->IsValid()) { continue; }

		if (!ThisConnection->HasFlag(EAINavMovementFlag::NAV_FLAG_PLATFORM)) { continue; }

		if (ThisConnection->LinkedObject == LiftReference->Edict)
		{
			const float ThisDist = fminf(vDist3DSq(ThisConnection->FromLocation, UTIL_GetClosestPointOnEntityToLocation(ThisConnection->FromLocation, LiftReference->Edict)),
									vDist3DSq(ThisConnection->ToLocation, UTIL_GetClosestPointOnEntityToLocation(ThisConnection->ToLocation, LiftReference->Edict)));

			if (ThisDist < sqrf(100.0f) && (!NearestConnectionHandle.IsValid() || ThisDist < MinDist))
			{
				NearestConnectionHandle = ThisConnectionHandle;
				MinDist = ThisDist;
			}
		}
	}

	const NavOffMeshConnection* NearestConnection = NearestConnectionHandle.GetConnectionRef();

	if (NearestConnection)
	{
		Vector NearestPointFromLocation = UTIL_GetClosestPointOnEntityToLocation(NearestConnection->FromLocation, LiftReference->Edict);
		NearestPointFromLocation.z = RiderPosition.z;

		Vector NearestPointToLocation = UTIL_GetClosestPointOnEntityToLocation(NearestConnection->ToLocation, LiftReference->Edict);
		NearestPointToLocation.z = RiderPosition.z;

		float DistFromLocation = vDist3DSq(NearestConnection->FromLocation, NearestPointFromLocation);
		float DistToLocation = vDist3DSq(NearestConnection->ToLocation, NearestPointToLocation);
		return (DistFromLocation < DistToLocation) ? NearestConnection->FromLocation : NearestConnection->ToLocation;
	}
	else
	{
		Vector NearestProjectedPoint = ZERO_VECTOR;
		Vector LiftCentre = UTIL_GetCentreOfEntity(LiftReference->Edict);
		float DisembarkHeight = RiderPosition.z;

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
	}

	return ZERO_VECTOR;
}

OffMeshConnectionHandle AIMAP_GetOffMeshConnectionForPlatform(const NavAgentProfile* NavProfile, const DynamicMapObjectHandle& PlatformRefHandle)
{
	OffMeshConnectionHandle NullHandle = OffMeshConnectionHandle();

	if (!NavProfile || !PlatformRefHandle.IsValid()) { return NullHandle; }

	NavMesh* ChosenNavMesh = AIMESH_GetNavMeshAtIndex(NavProfile->MeshIndex);

	if (!ChosenNavMesh) { return NullHandle; }

	const DynamicMapObject* PlatformRef = PlatformRefHandle.GetObjectRef();

	if (!PlatformRef) { return NullHandle; }

	// TODO: What about cases where multiple off-mesh connections are associated with this?
	for (auto it = ChosenNavMesh->MeshConnectionHandles.begin(); it != ChosenNavMesh->MeshConnectionHandles.end(); it++)
	{
		OffMeshConnectionHandle ThisConnectionHandle = *it;

		if (!ThisConnectionHandle.IsValid()) { continue; }

		const NavOffMeshConnection* ThisConnection = ThisConnectionHandle.GetConnectionRef();

		if (!ThisConnection || !ThisConnection->IsValid()) { continue; }

		if (!ThisConnection->HasFlag(EAINavMovementFlag::NAV_FLAG_PLATFORM)) { continue; }

		if (ThisConnection->LinkedObject == PlatformRef->Edict)
		{
			return ThisConnectionHandle;
		}
	}

	return NullHandle;
}

bool AIMAP_CanBoardPlatform(const NavAgentProfile* NavProfile, const DynamicMapObject* Platform, const Vector& BoardingPoint, const Vector& DesiredStop)
{
	if (!NavProfile || !NavProfile->IsValid() || !Platform || !Platform->IsValid()) { return false; }

	Vector IdealClosestPoint = UTIL_GetClosestPointOnEntityToLocation(BoardingPoint, Platform->Edict, DesiredStop);

	float Dist = vDist2D(IdealClosestPoint, BoardingPoint);

	Vector ClosestCurrentPoint = UTIL_GetClosestPointOnEntityToLocation(BoardingPoint, Platform->Edict);
	ClosestCurrentPoint.z = BoardingPoint.z;

	Vector ProjectedLocation = AIMESH_ProjectPointToNavmesh(NavProfile, ClosestCurrentPoint, Vector(Dist, Dist, 50.0f));

	return (!vIsZero(ProjectedLocation) && AINAV_IsPointDirectlyReachable(NavProfile, BoardingPoint, ProjectedLocation) && vDist2DSq(BoardingPoint, ProjectedLocation) <= sqrf(Dist + 16.0f));
}

bool AIMAP_PlatformNeedsActivating(const NavAgentProfile* NavProfile, const DynamicMapObjectHandle& PlatformHandle , const Vector& EmbarkPoint, const Vector& DisembarkPoint)
{
	if (!NavProfile || !NavProfile->IsValid() || !PlatformHandle.IsValid()) { return false; }

	const DynamicMapObject* Platform = PlatformHandle.GetObjectRef();

	if (!Platform) { return false; }

	if (Platform->Triggers.size() <= 1 || Platform->Triggers[0] == Platform->Edict) { return false; }

	// Obviously if the platform is idle then it needs activating regardless of where it is
	if (Platform->State == EAIDynamicMapObjectState::OBJECTSTATE_IDLE) { return true; }

	DynamicMapObjectStop DesiredEmbarkStop;
	DynamicMapObjectStop DesiredDisembarkStop;

	const bool bFoundStops = AIMAP_GetDesiredPlatformStops(PlatformHandle, EmbarkPoint, DisembarkPoint, DesiredEmbarkStop, DesiredDisembarkStop);

	if (!bFoundStops) { return false; }

	const DynamicMapObjectStop* CurrentStop = Platform->GetCurrentStop();

	// Platform is at or leaving our desired stop, check if we can get on. If so then we don't need to retrigger
	if (CurrentStop && vEquals(CurrentStop->StopLocation, DesiredEmbarkStop.StopLocation))
	{
		return false;
	}

	int32 CurrentIndex = Platform->NextStopIndex;
	int32 PrevIndex = (CurrentIndex == 0) ? Platform->StopPoints.size() - 1 : CurrentIndex - 1;

	int32 DesiredStopIndex = 0;

	// We will go through every stop the platform will go to on its way to us, and if it doesn't stop before reaching us
	// (and isn't stopping at our desired stop) then we don't need to trigger it
	while (true)
	{
		const DynamicMapObjectStop* PrevStop = &Platform->StopPoints[PrevIndex];
		const DynamicMapObjectStop* NextStop = &Platform->StopPoints[CurrentIndex];

		if (vEquals(NextStop->StopLocation, DesiredEmbarkStop.StopLocation))
		{
			return DesiredEmbarkStop.bWaitForRetrigger;
		}

		const Vector ClosestPointOnLine = vClosestPointOnLine(PrevStop->StopLocation, NextStop->StopLocation, DesiredEmbarkStop.StopLocation);

		// The platform will pass by our desired stop point, so don't need to trigger it
		if (vEquals(ClosestPointOnLine, DesiredEmbarkStop.StopLocation, 5.0f) && !vEquals(PrevStop->StopLocation, ClosestPointOnLine, 5.0f) && !vEquals(NextStop->StopLocation, ClosestPointOnLine, 5.0f))
		{
			return false;
		}

		if (NextStop->bWaitForRetrigger) { return true; }

		CurrentIndex++;

		if (CurrentIndex > Platform->StopPoints.size() - 1)
		{
			CurrentIndex = 0;
		}

		PrevIndex = (CurrentIndex == 0) ? Platform->StopPoints.size() - 1 : CurrentIndex - 1;
	}

	return false;
}

bool AIMAP_GetDesiredPlatformStops(const DynamicMapObjectHandle& PlatformHandle, const Vector& EmbarkPoint, const Vector& DisembarkPoint, DynamicMapObjectStop& EmbarkStop, DynamicMapObjectStop& DisembarkStop)
{
	const DynamicMapObject* PlatformRef = PlatformHandle.GetObjectRef();

	if (!PlatformRef) { return false; }

	float MinStartDist = FLT_MAX;
	float MinEndDist = FLT_MAX;

	bool bFoundStartStop = false;
	bool bFoundEndStop = false;

	// Find the desired stop point for us to get onto the lift
	for (auto it = PlatformRef->StopPoints.begin(); it != PlatformRef->StopPoints.end(); it++)
	{
		const DynamicMapObjectStop* ThisStopRef = &(*it);

		Vector CheckLocation = it->StopLocation;
		CheckLocation.z += PlatformRef->Edict->v.size.z * 0.25f;

		Vector NearestPointStart = UTIL_GetClosestPointOnEntityToLocation(EmbarkPoint, PlatformRef->Edict, it->StopLocation);
		Vector NearestPointEnd = UTIL_GetClosestPointOnEntityToLocation(DisembarkPoint, PlatformRef->Edict, it->StopLocation);

		NearestPointStart.z = it->StopLocation.z + (PlatformRef->Edict->v.size.z * 0.5f);
		NearestPointEnd.z = it->StopLocation.z + (PlatformRef->Edict->v.size.z * 0.5f);

		float ThisStartTouchingDist = vDist2DSq(EmbarkPoint, NearestPointStart);
		float ThisEndTouchingDist = vDist2DSq(DisembarkPoint, NearestPointEnd);

		float ThisStartDist = vDist3DSq(EmbarkPoint, NearestPointStart);
		float ThisEndDist = vDist3DSq(DisembarkPoint, NearestPointEnd);

		if (ThisStartTouchingDist <= sqrf(100.0f) && ThisStartDist < MinStartDist)
		{
			EmbarkStop = *ThisStopRef;
			MinStartDist = ThisStartDist;
			bFoundStartStop = true;
		}

		if (ThisEndTouchingDist <= sqrf(100.0f) && ThisEndDist < MinEndDist)
		{
			DisembarkStop = *ThisStopRef;
			MinEndDist = ThisEndDist;
			bFoundEndStop = true;
		}
	}

	return (bFoundStartStop && bFoundEndStop);
}

Vector AIMAP_GetTriggerFloorLocation(const NavAgentProfile* NavProfile, const DynamicMapObject* TriggerObject, const Vector& UserLocation)
{
	if (!NavProfile || !NavProfile->IsValid()) { return ZERO_VECTOR; }
	if (!TriggerObject || !TriggerObject->IsValid()) { return ZERO_VECTOR; }

	const edict_t* TriggerEdict = TriggerObject->Edict;

	if (UTIL_IsPointInSwimArea(UserLocation))
	{
		Vector NearestTriggerPoint = UTIL_GetClosestPointOnEntityToLocation(UserLocation, TriggerEdict);

		TraceResult Hit;

		UTIL_TraceHull(UserLocation, NearestTriggerPoint, ignore_monsters, head_hull, nullptr, &Hit);

		if (Hit.fInWater)
		{
			if (vDist3DSq(NearestTriggerPoint, Hit.vecEndPos) < sqrf(max_player_use_reach)) { return Hit.vecEndPos; }
		}
	}

	Vector ClosestPoint = ZERO_VECTOR;

	if (TriggerEdict->v.size.x > 64.0f || TriggerEdict->v.size.y > 64.0f)
	{
		ClosestPoint = UTIL_GetClosestPointOnEntityToLocation(UserLocation, TriggerEdict);
	}
	else
	{
		ClosestPoint = UTIL_GetCentreOfEntity(TriggerEdict);
	}

	if (UTIL_IsPointInSwimArea(ClosestPoint))
	{
		return ClosestPoint;
	}

	Vector ButtonAccessPoint = AIMESH_ProjectPointToNavmesh(NavProfile, ClosestPoint, Vector(100.0f, 100.0f, 100.0f));

	if (vIsZero(ButtonAccessPoint))
	{
		ButtonAccessPoint = ClosestPoint;
	}

	Vector PlayerAccessLoc = ButtonAccessPoint;

	if (ButtonAccessPoint.z > ClosestPoint.z)
	{
		PlayerAccessLoc.z += 18.0f;
	}
	else
	{
		PlayerAccessLoc.z += 36.0f;
	}

	if (fabsf(PlayerAccessLoc.z - ClosestPoint.z) <= max_player_use_reach)
	{
		return ButtonAccessPoint;
	}

	Vector NewProjection = ClosestPoint;

	if (ButtonAccessPoint.z > ClosestPoint.z)
	{
		NewProjection = ClosestPoint - Vector(0.0f, 0.0f, 100.0f);
	}
	else
	{
		NewProjection = ClosestPoint + Vector(0.0f, 0.0f, 100.0f);
	}

	Vector NewButtonAccessPoint = AIMESH_ProjectPointToNavmesh(NavProfile, NewProjection);

	if (vIsZero(NewButtonAccessPoint))
	{
		NewButtonAccessPoint = ClosestPoint;
	}
	else
	{
		if (UTIL_IsPointInSwimArea(NewButtonAccessPoint))
		{
			Vector NewClosestPoint = NewButtonAccessPoint + ((ClosestPoint - NewButtonAccessPoint) * 0.95f);

			if (UTIL_IsPointInSwimArea(NewClosestPoint))
			{
				NewButtonAccessPoint = NewClosestPoint;
			}
		}
	}

	return NewButtonAccessPoint;
}

DynamicMapObjectHandle AIMAP_GetBestTriggerForObject(const NavAgentProfile* NavProfile, const DynamicMapObjectHandle ObjectToActivateHandle, const Vector& ActivateLocation)
{
	return AIMAP_GetBestTriggerForObject_Internal(NavProfile, ObjectToActivateHandle.GetObjectRef(), ActivateLocation);
}

void AIMAP_ForceActivateTrigger(const AvHAIPlayer* AIPlayer, const DynamicMapObject* TriggerObject)
{
	if (!AIPlayer || !AIPlayer->IsValid()) { return; }
	if (!TriggerObject || !TriggerObject->IsValid()) { return; }

	switch (TriggerObject->Type)
	{
		case EAIDynamicMapObjectType::TRIGGER_TOUCH:
			TriggerObject->ObjectRef->Touch(AIPlayer->Player);
			break;
		case EAIDynamicMapObjectType::TRIGGER_USE:
			TriggerObject->ObjectRef->Use(AIPlayer->Player, AIPlayer->Player, USE_TOGGLE, 0.0f);
			break;
		case EAIDynamicMapObjectType::TRIGGER_BREAK:
		case EAIDynamicMapObjectType::TRIGGER_SHOOT:
			TriggerObject->ObjectRef->TakeDamage(AIPlayer->Player->pev, AIPlayer->Player->pev, 10000.0f, NS_DMG_NORMAL);
			break;
		case EAIDynamicMapObjectType::TRIGGER_WELD:
		{
			if (AvHWeldable* WeldableRef = dynamic_cast<AvHWeldable*>(TriggerObject->ObjectRef))
			{
				WeldableRef->AddBuildTime(300.0f);
			}
		}
		break;

		default:
			TriggerObject->ObjectRef->Use(AIPlayer->Player, AIPlayer->Player, USE_TOGGLE, 0.0f);
			break;
	}
}

DynamicMapObjectHandle AIMAP_GetTriggerReachableFromPlatform(const DynamicMapObjectHandle& PlatformHandle, float LiftHeight, const Vector& PlatformPosition)
{
	DynamicMapObjectHandle NullHandle = DynamicMapObjectHandle();

	if (!PlatformHandle.IsValid()) { return NullHandle; }

	const DynamicMapObject* Platform = PlatformHandle.GetObjectRef();

	const Vector CheckPlatformLocation = (vIsZero(PlatformPosition)) ? UTIL_GetCentreOfEntity(Platform->Edict) : PlatformPosition;

	for (auto it = Platform->Triggers.begin(); it != Platform->Triggers.end(); it++)
	{
		DynamicMapObjectHandle ThisTriggerHandle = AIMAP_GetDynamicObjectByEdict((*it));

		if (!ThisTriggerHandle.IsValid()) { continue; }

		const DynamicMapObject* ThisTrigger = ThisTriggerHandle.GetObjectRef();

		if (!ThisTrigger || !ThisTrigger->bIsActive) { continue; }

		Vector ClosestPointOnButton = UTIL_GetClosestPointOnEntityToLocation(CheckPlatformLocation, ThisTrigger->Edict);
		Vector ClosestPointOnLift = UTIL_GetClosestPointOnEntityToLocation(ClosestPointOnButton, Platform->Edict, CheckPlatformLocation);

		if (vDist2DSq(ClosestPointOnButton, ClosestPointOnLift) < sqrf(max_player_use_reach - 32.0f) && fabsf(LiftHeight - ClosestPointOnButton.z) < 64.0f)
		{
			return ThisTriggerHandle;
		}
	}

	return NullHandle;
}

void DEBUG_PrintObjectInfo(const DynamicMapObjectHandle& ObjectHandle)
{
	char buf[511];
	char interbuf[164];

	const DynamicMapObject* Object = ObjectHandle.GetObjectRef();

	if (!Object) { return; }

	sprintf(buf, "Info for %s (%s):\n\n", (Object->Edict->v.targetname != 0) ? STRING(Object->Edict->v.targetname) : "Unnamed", STRING(Object->Edict->v.classname));

	string CurrentType;

	switch (Object->Type)
	{
		case EAIDynamicMapObjectType::MAPOBJECT_STATIC:
			CurrentType = "Static";
			break;
		case EAIDynamicMapObjectType::MAPOBJECT_DOOR:
			CurrentType = "Door";
			break;
		case EAIDynamicMapObjectType::MAPOBJECT_PLATFORM:
			CurrentType = "Platform";
			break;
		case EAIDynamicMapObjectType::MAPOBJECT_TRAIN:
			CurrentType = "Train";
			break;
		case EAIDynamicMapObjectType::TRIGGER_WELD:
			CurrentType = "Weldable";
			break;
		default:
			CurrentType = "Other";
			break;
	}

	sprintf(interbuf, "Type: %s\n", CurrentType.c_str());
	strcat(buf, interbuf);

	string CurrentState;

	switch (Object->State)
	{
		case EAIDynamicMapObjectState::OBJECTSTATE_START:
			CurrentState = "Start";
			break;
		case EAIDynamicMapObjectState::OBJECTSTATE_IDLE:
			CurrentState = "Idle";
			break;
		case EAIDynamicMapObjectState::OBJECTSTATE_PREPARING:
			CurrentState = "Activated";
			break;
		case EAIDynamicMapObjectState::OBJECTSTATE_MOVING:
			CurrentState = "Moving";
			break;
		case EAIDynamicMapObjectState::OBJECTSTATE_OPEN:
			CurrentState = "Open";
			break;
		default:
			CurrentState = "Invalid";
			break;
	}

	sprintf(interbuf, "State: %s\n", CurrentState.c_str());
	strcat(buf, interbuf);

	sprintf(interbuf, "Is Active: %s\n", (Object->bIsActive) ? "True" : "False");
	strcat(buf, interbuf);

	sprintf(interbuf, "Current Stop: %s\n", (Object->GetCurrentStop()) ? vToString(Object->GetCurrentStop()->StopLocation) : "NONE");
	strcat(buf, interbuf);

	sprintf(interbuf, "Next Stop: %s\n", (Object->GetNextStop() != nullptr) ? vToString(Object->GetNextStop()->StopLocation) : "NONE");
	strcat(buf, interbuf);

	UTIL_DrawHUDText(INDEXENT(1), 0, 0.1, 0.1f, 255, 255, 255, buf);

	Vector ObjectLocation = UTIL_GetCentreOfEntity(Object->Edict);

	if (!Object->StopPoints.empty())
	{
		Vector NextStop = Object->StopPoints[Object->NextStopIndex].StopLocation;

		UTIL_DrawLine(INDEXENT(1), ObjectLocation, NextStop, 0, 0, 255);
	}

	sprintf(buf, "Trigger States:\n\n");

	if (Object->Triggers.size() == 0)
	{
		sprintf(interbuf, "NONE");
		strcat(buf, interbuf);
		UTIL_DrawHUDText(INDEXENT(1), 1, 0.6, 0.1f, 255, 255, 255, buf);
		return;
	}

	for (auto it = Object->Triggers.begin(); it != Object->Triggers.end(); it++)
	{
		const DynamicMapObjectHandle FoundHandle = AIMAP_GetDynamicObjectByEdict(*it);

		if (!FoundHandle.IsValid()) { continue; }

		const DynamicMapObject* ThisTrigger = FoundHandle.GetObjectRef();

		if (!ThisTrigger || !ThisTrigger->IsValid()) { continue; }

		sprintf(interbuf, "Trigger %s (%s):\n", (ThisTrigger->Edict->v.targetname != 0) ? STRING(ThisTrigger->Edict->v.targetname) : "Unnamed", STRING(ThisTrigger->Edict->v.classname));
		strcat(buf, interbuf);

		switch (ThisTrigger->State)
		{
			case EAIDynamicMapObjectState::OBJECTSTATE_IDLE:
				CurrentState = "Idle";
				break;
			case EAIDynamicMapObjectState::OBJECTSTATE_PREPARING:
				CurrentState = "Activated";
				break;
			case EAIDynamicMapObjectState::OBJECTSTATE_MOVING:
				CurrentState = "Moving";
				break;
			case EAIDynamicMapObjectState::OBJECTSTATE_OPEN:
				CurrentState = "Open";
				break;
			default:
				CurrentState = "Invalid";
				break;
		}

		sprintf(interbuf, "State: %s\n", CurrentState.c_str());
		strcat(buf, interbuf);

		switch (ThisTrigger->Type)
		{
			case EAIDynamicMapObjectType::TRIGGER_TOUCH:
				CurrentState = "Touch";
				break;
			case EAIDynamicMapObjectType::TRIGGER_USE:
				CurrentState = "Use";
				break;
			case EAIDynamicMapObjectType::TRIGGER_SHOOT:
				CurrentState = "Shoot";
				break;
			case EAIDynamicMapObjectType::TRIGGER_BREAK:
				CurrentState = "Break";
				break;
			case EAIDynamicMapObjectType::TRIGGER_WELD:
				CurrentState = "Weld";
				break;
			case EAIDynamicMapObjectType::MAPOBJECT_PLATFORM:
				CurrentState = "Touch";
				break;
			case EAIDynamicMapObjectType::MAPOBJECT_DOOR:
				CurrentState = "Use";
				break;
			default:
				CurrentState = "Other";
				break;
		}

		sprintf(interbuf, "Activation Method: %s\n", CurrentState.c_str());
		strcat(buf, interbuf);

		sprintf(interbuf, "Is Active: %s\n\n", (ThisTrigger->bIsActive) ? "True" : "False");
		strcat(buf, interbuf);
	}

	UTIL_DrawHUDText(INDEXENT(1), 1, 0.6, 0.1f, 255, 255, 255, buf);
}

void DynamicMapObject::ClearTemporaryObstacles()
{
	for (auto ObsIt = TempObstacles.begin(); ObsIt != TempObstacles.end(); ObsIt++)
	{
		NavTempObstacleHandle& ThisObstacle = (*ObsIt);

		AIMESH_RemoveTemporaryObstacle(ThisObstacle);
	}

	TempObstacles.clear();
}

void DynamicMapObject::UndoOffMeshConnectionChanges()
{
	for (auto stopIt = StopPoints.begin(); stopIt != StopPoints.end(); stopIt++)
	{
		for (auto it = stopIt->AffectedConnections.begin(); it != stopIt->AffectedConnections.end();)
		{
			OffMeshConnectionHandle& ThisConnectionHandle = (*it);

			if (!ThisConnectionHandle.IsValid())
			{
				it = stopIt->AffectedConnections.erase(it);
				continue;
			}

			AIMESH_RestoreDefaultOffMeshConnectionFlags(ThisConnectionHandle);

			++it;
		}
	}
}

DynamicMapObjectHandle::DynamicMapObjectHandle(const DynamicMapObject* ObjectRef)
{
	if (ObjectRef && ObjectRef->IsValid())
	{
		ObjectId = ObjectRef->ObjectId;
		Generation = CurrentNavMeshGeneration;
	}
	else
	{
		ObjectId = 0;
		Generation = 0;
	}
}

bool DynamicMapObjectHandle::IsValid() const
{
	return ObjectId > 0 && Generation > 0 && Generation == CurrentNavMeshGeneration;
}

const DynamicMapObject* DynamicMapObjectHandle::GetObjectRef() const
{
	if (!IsValid()) { return nullptr; }

	auto FoundObjectIt = DynamicMapObjects.find(ObjectId);

	if (FoundObjectIt == DynamicMapObjects.end()) { return nullptr; }

	const DynamicMapObject* FoundObject = &FoundObjectIt->second;

	if (!FoundObject || !FoundObject->IsValid()) { return nullptr; }

	return FoundObject;
}

DynamicMapObject* DynamicMapObjectHandle::GetObjectRef_Mutable() const
{
	if (!IsValid()) { return nullptr; }

	auto FoundObjectIt = DynamicMapObjects.find(ObjectId);

	if (FoundObjectIt == DynamicMapObjects.end()) { return nullptr; }

	DynamicMapObject* FoundObject = &FoundObjectIt->second;

	if (!FoundObject || !FoundObject->IsValid()) { return nullptr; }

	return FoundObject;
}
