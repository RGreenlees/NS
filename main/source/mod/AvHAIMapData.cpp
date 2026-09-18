//
// EvoBot - Neoptolemus' Natural Selection bot, based on Botman's HPB bot template
//
// AvHAIMapData.cpp
//
// Handles all dynamic map data that impacts bot navigation and movement
//
#include "AvHAIMapData.h"
#include "AvHServerUtil.h"
#include "AvHAIHelper.h"
#include "AvHWeldable.h"

#include <dlls/triggers.h>
#include <dlls/plats.h>

#include <dlls/cbase.h>       // Core base entity classes (CBaseEntity)

std::vector<DynamicMapObject> DynamicMapObjects;

static const int DOOR_START_OPEN = 1;
static const int DOOR_USE_ONLY = 256; // Flag used by GoldSrc to determine if a door entity can only be used to open (i.e. can't be triggered)
static const int SF_BUTTON_TOGGLE = 32;	// button stays pushed until reactivated
static const int SF_PLAT_TRIGGER_ONLY = 1;

bool AIMAP_BuildMapData()
{
	if (!AIMAP_PopulateDynamicMapObjects()) { return false; }

	AIMAP_LinkDynamicMapObjectsToTriggers();
	AIMAP_LinkDynamicMapObjectsToOffmeshConnections();
	AIMAP_SetTrainStartPoints();
	AIMAP_PopulateAllConnectionsAffectedByDynamicObjects();

	return true;
}

bool AIMAP_PopulateDynamicMapObjects()
{
	DynamicMapObjects.clear();

	FOR_ALL_ENTITIES("func_door", CBaseEntity*)
		const bool bSuccess = AIMAP_PopulateDynamicDoorObject(theEntity->edict());

		if (!bSuccess)
		{

		}
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

	return true;
}

bool AIMAP_PopulateDynamicDoorObject(edict_t* DoorObject)
{
	if (FNullEnt(DoorObject)) { return false; }

	CBaseDoor* DoorRef = dynamic_cast<CBaseDoor*>(CBaseEntity::Instance(DoorObject));

	if (!DoorRef) { return false; }

	DynamicMapObject NewObject;

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
	NewObject.Type = MAPOBJECT_DOOR;

	Vector DoorCentre = UTIL_GetCentreOfEntity(NewObject.Edict);

	DynamicMapObjectStop FirstStop;
	FirstStop.StopLocation = DoorRef->m_vecPosition1;
	FirstStop.WaitTime = 0.0f;
	FirstStop.bWaitForRetrigger = true;

	NewObject.StopPoints.push_back(FirstStop);

	DynamicMapObjectStop SecondStop;
	SecondStop.StopLocation = DoorRef->m_vecPosition2;
	SecondStop.WaitTime = NewObject.Wait;
	SecondStop.bWaitForRetrigger = ((NewObject.Edict->v.spawnflags & SF_DOOR_NO_AUTO_RETURN) || NewObject.Wait < 0.0f);

	NewObject.bToggleActive = SecondStop.bWaitForRetrigger;

	NewObject.StopPoints.push_back(SecondStop);

	NewObject.NextStopIndex = (NewObject.StopPoints.size() > 1) ? 1 : 0;

	if (CBaseEntity* DoorTarget = DoorRef->GetNextTarget())
	{
		edict_t* TargetEdict = DoorTarget->edict();

		if (!FNullEnt(TargetEdict))
		{
			NewObject.Targets.push_back(DoorTarget->edict());
		}
	}

	NewObject.State = OBJECTSTATE_START;

	if (CBaseEntity* Target = DoorRef->GetNextTarget())
	{
		NewObject.Targets.push_back(Target->edict());
	}

	DynamicMapObjects.push_back(NewObject);

	return true;
}

bool AIMAP_PopulateDynamicTrainObject(edict_t* TrainObject)
{
	if (FNullEnt(TrainObject)) { return false; }

	CBaseToggle* TrainRef = dynamic_cast<CBaseToggle*>(CBaseEntity::Instance(TrainObject));

	if (!TrainRef) { return false; }

	DynamicMapObject NewObject;

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
	NewObject.Type = MAPOBJECT_TRAIN;

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

	NewObject.State = OBJECTSTATE_START;

	if (CBaseEntity* Target = TrainRef->GetNextTarget())
	{
		NewObject.Targets.push_back(Target->edict());
	}

	DynamicMapObjects.push_back(NewObject);

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

	NewObject.EdictIndex = ENTINDEX(PlatObject);
	NewObject.Edict = PlatObject;

	CBaseEntity* MasterEdict = UTIL_FindEntityByTargetname(NULL, STRING(PlatRef->m_sMaster));

	if (MasterEdict)
	{
		NewObject.Master = MasterEdict->edict();
	}

	NewObject.ObjectName = STRING(NewObject.Edict->v.targetname);
	NewObject.Triggers.clear();
	NewObject.Wait = PlatRef->m_flWait;
	NewObject.Delay = PlatRef->m_flDelay;
	NewObject.Type = MAPOBJECT_PLATFORM;

	DynamicMapObjectStop FirstStop;
	FirstStop.StopLocation = PlatRef->m_vecPosition1;
	FirstStop.WaitTime = 0.0f;
	FirstStop.bWaitForRetrigger = true;

	NewObject.StopPoints.push_back(FirstStop);

	DynamicMapObjectStop SecondStop;
	SecondStop.StopLocation = PlatRef->m_vecPosition2;
	SecondStop.WaitTime = 3.0f;
	SecondStop.bWaitForRetrigger = (NewObject.Edict->v.spawnflags & SF_PLAT_TRIGGER_ONLY);

	NewObject.NextStopIndex = (NewObject.StopPoints.size() > 1) ? 1 : 0;

	if (CBaseEntity* PlatTarget = PlatRef->GetNextTarget())
	{
		edict_t* TargetEdict = PlatTarget->edict();

		if (!FNullEnt(TargetEdict))
		{
			NewObject.Targets.push_back(PlatTarget->edict());
		}
	}

	NewObject.State = OBJECTSTATE_START;

	NewObject.StopPoints.push_back(SecondStop);

	if (CBaseEntity* Target = PlatRef->GetNextTarget())
	{
		NewObject.Targets.push_back(Target->edict());
	}

	DynamicMapObjects.push_back(NewObject);

	return true;
}

bool AIMAP_PopulateDynamicButtonObject(edict_t* ButtonObject)
{
	if (FNullEnt(ButtonObject)) { return false; }

	CBaseButton* ButtonRef = dynamic_cast<CBaseButton*>(CBaseEntity::Instance(ButtonObject));

	if (!ButtonRef) { return false; }

	DynamicMapObject NewObject;

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
		NewObject.Type = TRIGGER_SHOOT;
	}
	else
	{
		NewObject.Type = TRIGGER_USE;
	}

	DynamicMapObjectStop FirstStop;
	FirstStop.StopLocation = ButtonRef->m_vecPosition1;
	FirstStop.WaitTime = 0.0f;
	FirstStop.bWaitForRetrigger = true;

	NewObject.StopPoints.push_back(FirstStop);

	DynamicMapObjectStop SecondStop;
	SecondStop.StopLocation = ButtonRef->m_vecPosition2;
	SecondStop.WaitTime = ButtonRef->m_flWait;
	SecondStop.bWaitForRetrigger = (NewObject.Edict->v.spawnflags & SF_DOOR_NO_AUTO_RETURN);

	NewObject.StopPoints.push_back(SecondStop);

	NewObject.NextStopIndex = (NewObject.StopPoints.size() > 1) ? 1 : 0;

	NewObject.State = OBJECTSTATE_START;

	if (CBaseEntity* Target = ButtonRef->GetNextTarget())
	{
		NewObject.Targets.push_back(Target->edict());
	}

	DynamicMapObjects.push_back(NewObject);

	return true;
}

bool AIMAP_PopulateDynamicWeldableObject(edict_t* WeldableObject)
{
	if (FNullEnt(WeldableObject)) { return false; }

	AvHWeldable* WeldableRef = dynamic_cast<AvHWeldable*>(CBaseEntity::Instance(WeldableObject));

	if (!WeldableRef) { return false; }

	DynamicMapObject NewObject;

	NewObject.EdictIndex = ENTINDEX(WeldableObject);
	NewObject.Edict = WeldableObject;
	NewObject.ObjectName = STRING(NewObject.Edict->v.targetname);
	NewObject.Triggers.clear();
	NewObject.Wait = 0.0f;
	NewObject.Delay = WeldableRef->GetDelay();

	NewObject.Type = TRIGGER_WELD;

	NewObject.State = OBJECTSTATE_START;

	if (CBaseEntity* Target = WeldableRef->GetNextTarget())
	{
		NewObject.Targets.push_back(Target->edict());
	}

	NewObject.NextStopIndex = (NewObject.StopPoints.size() > 1) ? 1 : 0;

	DynamicMapObjects.push_back(NewObject);

	return true;
}

bool AIMAP_PopulateDynamicTriggerObject(edict_t* TriggerObject)
{
	if (FNullEnt(TriggerObject)) { return false; }

	CBaseTrigger* TriggerRef = dynamic_cast<CBaseTrigger*>(CBaseEntity::Instance(TriggerObject));

	if (!TriggerRef) { return false; }

	DynamicMapObject NewObject;

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

	NewObject.Type = TRIGGER_TOUCH;

	NewObject.State = OBJECTSTATE_START;

	if (CBaseEntity* Target = TriggerRef->GetNextTarget())
	{
		NewObject.Targets.push_back(Target->edict());
	}

	NewObject.NextStopIndex = (NewObject.StopPoints.size() > 1) ? 1 : 0;

	DynamicMapObjects.push_back(NewObject);

	return true;
}

bool AIMAP_PopulateDynamicBreakableObject(edict_t* BreakableObject)
{
	if (FNullEnt(BreakableObject)) { return false; }

	CBreakable* BreakableRef = dynamic_cast<CBreakable*>(CBaseEntity::Instance(BreakableObject));

	if (!BreakableRef) { return false; }

	DynamicMapObject NewObject;

	NewObject.EdictIndex = ENTINDEX(BreakableObject);
	NewObject.Edict = BreakableObject;
	NewObject.ObjectName = STRING(NewObject.Edict->v.targetname);
	NewObject.Triggers.clear();
	NewObject.Delay = BreakableRef->m_flDelay;
	NewObject.Type = TRIGGER_BREAK;

	NewObject.State = OBJECTSTATE_START;

	if (CBaseEntity* Target = BreakableRef->GetNextTarget())
	{
		NewObject.Targets.push_back(Target->edict());
	}

	DynamicMapObjects.push_back(NewObject);

	return true;
}

void AIMAP_UpdateDynamicMapObjects()
{
	if (DynamicMapObjects.empty()) { return; }

	for (auto it = DynamicMapObjects.begin(); it != DynamicMapObjects.end();)
	{
		DynamicMapObject* ThisObject = &(*it);

		if (!ThisObject)
		{
			it = DynamicMapObjects.erase(it);
			continue;
		}

		if (FNullEnt(ThisObject->Edict) || ThisObject->Edict->v.deadflag != DEAD_NO)
		{
			AIMAP_RemoveAllTempObstaclesFromObject(ThisObject);

			it = DynamicMapObjects.erase(it);
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

		switch (ThisObject->Type)
		{
			case DynamicMapObjectType::MAPOBJECT_DOOR:
				AIMAP_UpdateDynamicDoorObject(ThisObject);
				break;
			case DynamicMapObjectType::TRIGGER_BREAK:
				AIMAP_UpdateDynamicBreakableObject(ThisObject);
				break;
			case DynamicMapObjectType::TRIGGER_TOUCH:
				AIMAP_UpdateDynamicTouchTriggerObject(ThisObject);
				break;
			case DynamicMapObjectType::TRIGGER_WELD:
				AIMAP_UpdateDynamicWeldableObject(ThisObject);
				break;
			case DynamicMapObjectType::MAPOBJECT_PLATFORM:
				AIMAP_UpdateDynamicPlatformObject(ThisObject);
				break;
			case DynamicMapObjectType::MAPOBJECT_TRAIN:
				AIMAP_UpdateDynamicTrainObject(ThisObject);
				break;
			case DynamicMapObjectType::TRIGGER_SHOOT:
			case DynamicMapObjectType::TRIGGER_USE:
				AIMAP_UpdateDynamicButtonObject(ThisObject);
				break;
			case DynamicMapObjectType::MAPOBJECT_STATIC:
				AIMAP_UpdateDynamicInactiveObject(ThisObject);
				break;
			default:
				break;
		}

		++it;
	}
}

void AIMAP_UpdateDynamicInactiveObject(DynamicMapObject* InactiveObject)
{
	if (!InactiveObject || FNullEnt(InactiveObject->Edict)) { return;  }

	if (InactiveObject->Edict->v.velocity.Length() > 0.0f)
	{
		AIMAP_SetDynamicObjectStatus(InactiveObject, OBJECTSTATE_MOVING);
		return;
	}

	AIMAP_SetDynamicObjectStatus(InactiveObject, OBJECTSTATE_IDLE);
}

void AIMAP_UpdateDynamicButtonObject(DynamicMapObject* ButtonObject)
{
	if (!ButtonObject || FNullEnt(ButtonObject->Edict)) { return; }

	CBaseButton* ButtonRef = dynamic_cast<CBaseButton*>(CBaseEntity::Instance(ButtonObject->Edict));

	if (!ButtonRef) { return; }

	const TOGGLE_STATE ButtonCurrentState = static_cast<TOGGLE_STATE>(ButtonRef->GetToggleState());

	if (ButtonCurrentState == TS_GOING_UP)
	{
		if (ButtonObject->State == OBJECTSTATE_IDLE)
		{
			AIMAP_OnTriggerActivated(ButtonObject);
		}

		AIMAP_SetDynamicObjectStatus(ButtonObject, DynamicMapObjectState::OBJECTSTATE_MOVING);
		return;
	}

	if (ButtonCurrentState == TS_GOING_DOWN)
	{
		AIMAP_SetDynamicObjectStatus(ButtonObject, DynamicMapObjectState::OBJECTSTATE_PREPARING);
		return;
	}

	if (ButtonCurrentState == TS_AT_BOTTOM)
	{
		AIMAP_SetDynamicObjectStatus(ButtonObject, DynamicMapObjectState::OBJECTSTATE_IDLE);
		return;
	}

	if (ButtonCurrentState == TS_AT_TOP)
	{
		// Button doesn't move so it will go direct from TS_AT_BOTTOM to TS_AT_TOP
		if (ButtonObject->State == OBJECTSTATE_IDLE)
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

		AIMAP_SetDynamicObjectStatus(ButtonObject, DynamicMapObjectState::OBJECTSTATE_PREPARING);
		return;
	}
}

void AIMAP_UpdateDynamicPlatformObject(DynamicMapObject* PlatformObject)
{
	if (!PlatformObject || FNullEnt(PlatformObject->Edict)) { return; }

	CBaseToggle* PlatRef = dynamic_cast<CBaseToggle*>(CBaseEntity::Instance(PlatformObject->Edict));

	if (!PlatRef) { return; }

	const TOGGLE_STATE PlatCurrentState = static_cast<TOGGLE_STATE>(PlatRef->GetToggleState());

	if (PlatCurrentState == TS_GOING_UP || PlatCurrentState == TS_GOING_DOWN)
	{
		AIMAP_SetDynamicObjectStatus(PlatformObject, OBJECTSTATE_MOVING);
		return;
	}

	if (PlatCurrentState == TS_AT_TOP)
	{
		const DynamicMapObjectState NewState = (PlatRef->pev->spawnflags & SF_PLAT_TOGGLE) ? OBJECTSTATE_IDLE : OBJECTSTATE_PREPARING;
		AIMAP_SetDynamicObjectStatus(PlatformObject, NewState);
		return;
	}

	if (PlatCurrentState == TS_AT_BOTTOM)
	{
		AIMAP_SetDynamicObjectStatus(PlatformObject, OBJECTSTATE_IDLE);
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
		TrainObject->Type = MAPOBJECT_STATIC;
	}

	if (TrainObject->Edict->v.velocity.Length() > 0.0f)
	{
		AIMAP_SetDynamicObjectStatus(TrainObject, OBJECTSTATE_MOVING);

		CBaseEntity* NextStopRef = TrainRef->GetNextTarget();

		for (int i = 0; i < TrainObject->StopPoints.size(); i++)
		{
			DynamicMapObjectStop CheckStop = TrainObject->StopPoints[i];

			if (CheckStop.CornerEdict == NextStopRef->edict())
			{
				TrainObject->NextStopIndex = i;
				break;
			}
		}

		return;
	}

	// Train was moving last tick, but is now stationary
	if (TrainObject->State == OBJECTSTATE_MOVING)
	{
		CBaseEntity* NextStopRef = TrainRef->GetNextTarget();

		for (int i = 0; i < TrainObject->StopPoints.size(); i++)
		{
			DynamicMapObjectStop CheckStop = TrainObject->StopPoints[i];

			if (CheckStop.CornerEdict == NextStopRef->edict())
			{
				TrainObject->NextStopIndex = i;
				break;
			}
		}

		const DynamicMapObjectState NewState = (TrainObject->Edict->v.nextthink > 0.0)
			? DynamicMapObjectState::OBJECTSTATE_PREPARING
			: DynamicMapObjectState::OBJECTSTATE_IDLE;

		AIMAP_SetDynamicObjectStatus(TrainObject, NewState);
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
		DoorObject->Type = MAPOBJECT_STATIC;
	}

	const TOGGLE_STATE DoorCurrentState = static_cast<TOGGLE_STATE>(DoorRef->GetToggleState());

	if (DoorCurrentState == TS_GOING_UP || DoorCurrentState == TS_GOING_DOWN)
	{
		AIMAP_SetDynamicObjectStatus(DoorObject, OBJECTSTATE_MOVING);
		return;
	}

	if (DoorCurrentState == TS_AT_BOTTOM)
	{
		AIMAP_SetDynamicObjectStatus(DoorObject, OBJECTSTATE_IDLE);
		return;
	}

	if (DoorCurrentState == TS_AT_TOP)
	{
		if (DoorObject->bToggleActive)
		{
			AIMAP_SetDynamicObjectStatus(DoorObject, OBJECTSTATE_IDLE);
		}
		else
		{
			AIMAP_SetDynamicObjectStatus(DoorObject, OBJECTSTATE_PREPARING);
		}

		return;
	}
}

void AIMAP_UpdateDynamicTouchTriggerObject(DynamicMapObject* TriggerObject)
{
	if (!TriggerObject || FNullEnt(TriggerObject->Edict)) { return; }

	if (TriggerObject->State == OBJECTSTATE_START)
	{
		AIMAP_SetDynamicObjectStatus(TriggerObject, OBJECTSTATE_IDLE);
	}

	if (TriggerObject->State == OBJECTSTATE_IDLE && TriggerObject->Edict->v.nextthink > 0.0f)
	{
		AIMAP_OnTriggerActivated(TriggerObject);

		AIMAP_SetDynamicObjectStatus(TriggerObject, OBJECTSTATE_PREPARING);

		// This is a trigger once, so it now needs to remove itself from the object list
		if (TriggerObject->Wait < 0.0f)
		{
			TriggerObject->Edict = nullptr;
			return;
		}
	}

	if (TriggerObject->State == OBJECTSTATE_PREPARING && TriggerObject->Edict->v.nextthink <= 0.0f)
	{
		AIMAP_SetDynamicObjectStatus(TriggerObject, OBJECTSTATE_IDLE);
	}
}

void AIMAP_UpdateDynamicBreakableObject(DynamicMapObject* BreakableObject)
{
	if (!BreakableObject || FNullEnt(BreakableObject->Edict)) { return; }

	AIMAP_SetDynamicObjectStatus(BreakableObject, DynamicMapObjectState::OBJECTSTATE_IDLE);

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

	AIMAP_SetDynamicObjectStatus(WeldableObject, DynamicMapObjectState::OBJECTSTATE_IDLE);

	AvHWeldable* WeldableRef = dynamic_cast<AvHWeldable*>(CBaseEntity::Instance(WeldableObject->Edict));

	if (WeldableRef && WeldableRef->GetIsWelded())
	{
		AIMAP_OnTriggerActivated(WeldableObject);
		WeldableObject->Edict = nullptr;
		return;
	}
}

void AIMAP_OnTriggerActivated(DynamicMapObject* UsedObject)
{
	if (!UsedObject) { return; }

	UsedObject->NumTimesActivated++;

	UsedObject->LastActivatedTime = gpGlobals->time;

	UsedObject->State = OBJECTSTATE_PREPARING;

	for (auto it = UsedObject->Targets.begin(); it != UsedObject->Targets.end(); it++)
	{
		DynamicMapObject* TargetObject = AIMAP_GetDynamicObjectByEdict((*it));

		if (TargetObject)
		{
			AIMAP_OnTriggerActivated(TargetObject);
		}
	}
}

void AIMAP_SetDynamicObjectStatus(DynamicMapObject* Object, DynamicMapObjectState NewState)
{
	if (NewState == Object->State) { return; }

	Object->State = NewState;

	if (NewState == OBJECTSTATE_MOVING)
	{
		AIMAP_OnDynamicMapObjectStopIdle(Object);
	}

	if (NewState == OBJECTSTATE_IDLE)
	{
		AIMAP_OnDynamicMapObjectBecomeIdle(Object);
	}
}

void AIMAP_OnDynamicMapObjectBecomeIdle(DynamicMapObject* Object)
{
	// Object will not move again, so block off all connections permanently, and place null obstacles to block nav mesh
	if (Object->Type == MAPOBJECT_STATIC)
	{
		AIMAP_ApplyTempObstaclesToObject(Object, DT_AREA_NULL);

		int CurrStopIndex = (Object->NextStopIndex > 0) ? Object->NextStopIndex - 1 : Object->StopPoints.size() - 1;

		for (auto it = Object->StopPoints[CurrStopIndex].AffectedConnections.begin(); it != Object->StopPoints[CurrStopIndex].AffectedConnections.end(); it++)
		{
			NavOffMeshConnection* ThisConnection = (*it);

			AIMESH_ModifyOffMeshConnectionFlag(ThisConnection, NAV_FLAG_DISABLED);
		}

		return;
	}

	// Door is not permanently blocking anything and can still be activated. We need to do a more nuanced take on how connections are modified
	// The idea is that if a connection is one-way, we detect how we can activate the door from that side and modify the connection flag accordingly
	// Two-way connections will end up being treated like a one-way connection, so use 2 one-way connections if a door requires different capabilities for each side!

	int CurrStopIndex = (Object->NextStopIndex > 0) ? Object->NextStopIndex - 1 : Object->StopPoints.size() - 1;

	NavAgentProfile TestProfile = GetBaseAgentProfile(NAV_PROFILE_DEFAULT);

	for (auto it = Object->StopPoints[CurrStopIndex].AffectedConnections.begin(); it != Object->StopPoints[CurrStopIndex].AffectedConnections.end(); it++)
	{
		NavOffMeshConnection* ThisConnection = (*it);

		TestProfile.MeshIndex = ThisConnection->NavMeshIndex;

		DynamicMapObject* ThisTrigger = AIMAP_GetBestTriggerForObject(Object, ThisConnection->FromLocation, TestProfile);

		if (!ThisTrigger)
		{
			AIMESH_ModifyOffMeshConnectionFlag(ThisConnection, NAV_FLAG_DISABLED);
			continue;
		}
	}
}

void AIMAP_OnDynamicMapObjectStopIdle(DynamicMapObject* Object)
{
	for (auto stopIt = Object->StopPoints.begin(); stopIt != Object->StopPoints.end(); stopIt++)
	{
		for (auto it = stopIt->AffectedConnections.begin(); it != stopIt->AffectedConnections.end(); it++)
		{
			NavOffMeshConnection* ThisConnection = (*it);

			AIMESH_ModifyOffMeshConnectionFlag(ThisConnection, ThisConnection->DefaultConnectionFlags);
		}
	}

	for (auto it = Object->TempObstacles.begin(); it != Object->TempObstacles.end(); it++)
	{
		AIMESH_RemoveTemporaryObstacle(&(*it));
	}

	Object->TempObstacles.clear();
}

void AIMAP_LinkDynamicMapObjectsToTriggers()
{
	for (auto objectIt = DynamicMapObjects.begin(); objectIt != DynamicMapObjects.end(); objectIt++)
	{
		DynamicMapObject* ThisObject = &(*objectIt);

		if (ThisObject->Type == MAPOBJECT_DOOR)
		{
			// Gotta use this door to open it, so add the door itself as its own trigger
			if (ThisObject->Edict->v.spawnflags & DOOR_USE_ONLY)
			{
				ThisObject->Triggers.push_back(ThisObject->Edict);
			}
			// Must be one o' them fancy touch-activamated doors. Technically the bot will still try to use the door, but they will touch it in the process so no big deal
			else if (ThisObject->ObjectName == NULL || ThisObject->ObjectName[0] == '\0')
			{
				ThisObject->Triggers.push_back(ThisObject->Edict);
			}

			continue;
		}

		if (ThisObject->Type == MAPOBJECT_PLATFORM)
		{
			if (FClassnameIs(ThisObject->Edict, "func_plat"))
			{
				ThisObject->Triggers.push_back(ThisObject->Edict);
			}

			continue;
		}

		if (ThisObject->Type == TRIGGER_USE
			|| ThisObject->Type == TRIGGER_TOUCH
			|| ThisObject->Type == TRIGGER_SHOOT
			|| ThisObject->Type == TRIGGER_BREAK
			|| ThisObject->Type == TRIGGER_WELD)
		{
			for (auto targetsIt = DynamicMapObjects.begin(); targetsIt != DynamicMapObjects.end(); targetsIt++)
			{
				DynamicMapObject* OtherObject = &(*targetsIt);

				if (OtherObject == ThisObject) { continue; }

				vector<edict_t*> CheckedObjects;
				if (AIMAP_IsDynamicMapTriggerLinkedToObject(ThisObject->Edict, OtherObject->Edict, CheckedObjects))
				{
					OtherObject->Triggers.push_back(ThisObject->Edict);
				}
			}
		}
	}
}

void AIMAP_LinkDynamicMapObjectsToOffmeshConnections()
{
	/*for (int i = 0; i < NUM_NAV_MESHES; i++)
	{
		for (auto it = NavMeshes[i].MeshConnections.begin(); it != NavMeshes[i].MeshConnections.end(); it++)
		{
			if (it->DefaultConnectionFlags & NAV_FLAG_PLATFORM)
			{
				DynamicMapObject* NearestPlatform = AIMAP_GetClosestPlatformToPoints(it->FromLocation, it->ToLocation);

				if (NearestPlatform)
				{
					it->LinkedObject = NearestPlatform->Edict;
				}
			}
		}
	}*/
}

void AIMAP_SetTrainStartPoints()
{
	for (auto it = DynamicMapObjects.begin(); it != DynamicMapObjects.end(); it++)
	{
		if (it->Type == MAPOBJECT_PLATFORM && it->StopPoints.size() > 2 && it->Targets.size() > 0)
		{
			edict_t* TargetEdict = it->Targets[0];
			const char* TargetEdictName = STRING(TargetEdict->v.targetname);

			int CurrIndex = 0;

			for (auto stopIt = it->StopPoints.begin(); stopIt != it->StopPoints.end(); stopIt++)
			{
				if (stopIt->CornerEdict == TargetEdict)
				{
					break;
				}

				CurrIndex++;
			}

			it->NextStopIndex = CurrIndex + 1;

			if (it->NextStopIndex >= it->StopPoints.size())
			{
				it->NextStopIndex = 0;
			}
		}
	}
}

bool AIMAP_IsPathBlockedByObject(const NavAgentProfile& NavProfile, const Vector StartLoc, const Vector EndLoc, DynamicMapObject* SearchObject)
{
	if (UTIL_IsPointInSwimArea(StartLoc) && UTIL_IsPointInSwimArea(EndLoc))
	{
		if (UTIL_QuickHullTrace(nullptr, StartLoc, EndLoc))
		{
			return vlineIntersectsAABB(StartLoc, EndLoc, SearchObject->Edict->v.absmin, SearchObject->Edict->v.absmax);
		}
	}

	Vector ValidNavmeshPoint = AIMESH_ProjectPointToNavmesh(NavProfile.MeshIndex, EndLoc, NavProfile);

	if (UTIL_IsPointInSwimArea(EndLoc))
	{
		TraceResult Hit;
		UTIL_TraceLine(EndLoc, EndLoc - Vector(0.0f, 0.0f, 1000.0f), ignore_monsters, nullptr, &Hit);

		if (Hit.flFraction < 1.0f)
		{
			ValidNavmeshPoint = AIMESH_ProjectPointToNavmesh(NavProfile.MeshIndex, Hit.vecEndPos, NavProfile);
		}
	}

	if (!ValidNavmeshPoint)
	{
		return false;
	}

	vector<bot_path_node> TestPath;
	TestPath.clear();

	// Now we find a path backwards from the valid nav mesh point to our location, trying to get as close as we can to it

	dtStatus PathFindingStatus = FindPathClosestToPoint(NavProfile, StartLoc, ValidNavmeshPoint, TestPath, 50.0f);

	if (dtStatusSucceed(PathFindingStatus))
	{
		for (auto it = TestPath.begin(); it != TestPath.end(); it++)
		{
			if (AIMAP_GetObjectBlockingPathPoint(&(*it), SearchObject, nullptr) != nullptr)
			{
				return true;
			}
		}

		return false;
	}

	return true;
}

DynamicMapObject* AIMAP_GetObjectBlockingPathPoint(bot_path_node* PathNode, DynamicMapObject* SearchObject, DynamicMapObject* IgnoreObject)
{
	Vector FromLocation = PathNode->FromLocation;
	Vector ToLocation = PathNode->Location;
	ToLocation.z = PathNode->requiredZ;

	return AIMAP_GetObjectBlockingPathPoint(FromLocation, ToLocation, PathNode->flag, SearchObject, IgnoreObject);
}

DynamicMapObject* AIMAP_GetObjectBlockingPathPoint(const Vector FromLocation, const Vector ToLocation, const unsigned int MovementFlag, DynamicMapObject* SearchObject, DynamicMapObject* IgnoreObject)
{
	if (IsFlagTeleportType((NavMovementFlag)MovementFlag)) { return nullptr; }

	Vector FromLoc = FromLocation;
	Vector ToLoc = ToLocation;

	TraceResult doorHit;

	if (MovementFlag == NAV_FLAG_LADDER)
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
				if (it->Type == MAPOBJECT_PLATFORM || (IgnoreObject && it->Edict == IgnoreObject->Edict)) { continue; }

				if (vlineIntersectsAABB(FromLoc, TargetLoc, it->Edict->v.absmin, it->Edict->v.absmax))
				{
					return &(*it);
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
				if (it->Type == MAPOBJECT_PLATFORM || (IgnoreObject && it->Edict == IgnoreObject->Edict)) { continue; }

				if (vlineIntersectsAABB(TargetLoc, ToLoc, it->Edict->v.absmin, it->Edict->v.absmax))
				{
					return &(*it);
				}
			}
		}

	}
	else if (MovementFlag == NAV_FLAG_FALL)
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
				if (it->Type == MAPOBJECT_PLATFORM || (IgnoreObject && it->Edict == IgnoreObject->Edict)) { continue; }

				if (vlineIntersectsAABB(FromLoc, TargetLoc, it->Edict->v.absmin, it->Edict->v.absmax))
				{
					return &(*it);
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
				if (it->Type == MAPOBJECT_PLATFORM || (IgnoreObject && it->Edict == IgnoreObject->Edict)) { continue; }

				if (vlineIntersectsAABB(TargetLoc, ToLoc, it->Edict->v.absmin, it->Edict->v.absmax))
				{
					return &(*it);
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
			if (it->Type == MAPOBJECT_PLATFORM || (IgnoreObject && it->Edict == IgnoreObject->Edict)) { continue; }

			if (vlineIntersectsAABB(FromLoc, TargetLoc, it->Edict->v.absmin, it->Edict->v.absmax))
			{
				return &(*it);
			}
		}
	}

	return nullptr;
}

DynamicMapObject* AIMAP_GetBestTriggerForObject(DynamicMapObject* ObjectToActivate, Vector ActivateLocation, const NavAgentProfile& NavProfile)
{
	if (!ObjectToActivate || ObjectToActivate->Triggers.size() == 0 || vIsZero(ActivateLocation)) { return nullptr; }

	DynamicMapObject* WinningTrigger = nullptr;

	Vector FromLoc = ActivateLocation;

	float MinDist = FLT_MAX;

	// This object is triggered by itself, such as a door set to USE_ONLY or a func_plat which needs to be touched to activate
	if (ObjectToActivate->Triggers.size() == 1 && ObjectToActivate->Triggers[0] == ObjectToActivate->Edict)
	{
		return AIMAP_GetDynamicObjectByEdict(ObjectToActivate->Triggers[0]);
	}

	for (auto it = ObjectToActivate->Triggers.begin(); it != ObjectToActivate->Triggers.end(); it++)
	{
		DynamicMapObject* ThisTrigger = AIMAP_GetDynamicObjectByEdict((*it));

		if (!ThisTrigger || !ThisTrigger->bIsActive) { continue; }

		// For triggers we can activate from a distance and are in our LOS, short-cut and add them to the list
		if (ThisTrigger->Type == TRIGGER_SHOOT || ThisTrigger->Type == TRIGGER_BREAK)
		{
			TraceResult hit;

			UTIL_TraceLine(ActivateLocation + Vector(0.0f, 0.0f, 5.0f), UTIL_GetCentreOfEntity(ThisTrigger->Edict), ignore_monsters, ignore_glass, nullptr, &hit);

			if (hit.pHit == ThisTrigger->Edict)
			{
				float ThisDist = vDist3DSq(FromLoc, UTIL_GetCentreOfEntity(ThisTrigger->Edict));

				if (ThisDist < MinDist)
				{
					WinningTrigger = ThisTrigger;
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

		float MaxDist = (ThisTrigger->Type == TRIGGER_BREAK || ThisTrigger->Type == TRIGGER_SHOOT) ? UTIL_MetresToGoldSrcUnits(5.0f) : 64.0f;

		if (!UTIL_PointIsReachable(NavProfile, FromLoc, TriggerLocation, MaxDist)) { continue; }

		if (ObjectToActivate->Type != MAPOBJECT_PLATFORM)
		{
			if (AIMAP_IsPathBlockedByObject(NavProfile, FromLoc, TriggerLocation, ObjectToActivate)) { continue; }
		}
		else
		{
			vector<bot_path_node> CheckPath;

			dtStatus PathFindStatus = FindPathClosestToPoint(NavProfile, FromLoc, TriggerLocation, CheckPath, MaxDist);

			if (!dtStatusSucceed(PathFindStatus)) { continue; }

			bool bOtherSideOfLift = false;

			for (auto pathIt = CheckPath.begin(); pathIt != CheckPath.end(); pathIt++)
			{
				if (pathIt->flag & NAV_FLAG_PLATFORM)
				{
					if (AIMAP_GetClosestPlatformToPoints(pathIt->FromLocation, pathIt->Location) == ObjectToActivate)
					{
						bOtherSideOfLift = true;
						break;
					}
				}
			}

			if (bOtherSideOfLift) { continue; }
		}

		float ThisDist = vDist3DSq(FromLoc, TriggerLocation);

		if (ThisDist < MinDist)
		{
			WinningTrigger = ThisTrigger;
		}
	}

	return WinningTrigger;
}

Vector AIMAP_GetButtonFloorLocation(const NavAgentProfile& NavProfile, const Vector UserLocation, edict_t* ButtonEdict)
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

	Vector ButtonAccessPoint = AIMESH_ProjectPointToNavmesh(NavProfile.MeshIndex, ClosestPoint, NavProfile, Vector(100.0f, 100.0f, 100.0f));

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

	Vector NewButtonAccessPoint = AIMESH_ProjectPointToNavmesh(NavProfile.MeshIndex, NewProjection, NavProfile);

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

DynamicMapObject* AIMAP_GetClosestPlatformToPoints(const Vector StartPoint, const Vector EndPoint)
{
	DynamicMapObject* Result = nullptr;

	float minDist = 0.0f;

	for (auto it = DynamicMapObjects.begin(); it != DynamicMapObjects.end(); it++)
	{
		if (it->Type != MAPOBJECT_DOOR && it->Type != MAPOBJECT_PLATFORM) { continue; }

		float distTopPoint = FLT_MAX;
		float distBottomPoint = FLT_MAX;

		for (auto stop = it->StopPoints.begin(); stop != it->StopPoints.end(); stop++)
		{
			distTopPoint = fminf(distTopPoint, vDist3D(UTIL_GetClosestPointOnEntityToLocation(StartPoint, it->Edict, (*stop).StopLocation), StartPoint));
			distBottomPoint = fminf(distBottomPoint, vDist3D(UTIL_GetClosestPointOnEntityToLocation(EndPoint, it->Edict, (*stop).StopLocation), EndPoint));
		}

		// Get the average distance from our desired start and end points, whichever scores lowest is probably the lift/train/door we want to ride
		float thisDist = fminf(distTopPoint, distBottomPoint);

		if (!Result || thisDist < minDist)
		{
			Result = &(*it);
			minDist = thisDist;
		}
	}

	return Result;
}

void AIMAP_PopulateAllConnectionsAffectedByDynamicObjects()
{
	for (auto objectIt = DynamicMapObjects.begin(); objectIt != DynamicMapObjects.end(); objectIt++)
	{
		AIMAP_PopulateConnectionsAffectedByDynamicObject(&(*objectIt));
	}
}

void AIMAP_PopulateConnectionsAffectedByDynamicObject(DynamicMapObject* Object)
{
	if (!Object || FNullEnt(Object->Edict)) { return; }

	Vector HalfExtents = (Object->Edict->v.size * 0.5f);
	HalfExtents.x += 16.0f;
	HalfExtents.y += 16.0f;
	HalfExtents.z += 16.0f;

	const int InvalidIndex = static_cast<int>(NAV_MESH_INVALID);

	for (int i = 0; i < InvalidIndex; i++)
	{
		const NavMeshIndex MeshIndex = static_cast<NavMeshIndex>(i);

		NavMesh* FoundMesh = AIMESH_GetNavMeshAtIndex(MeshIndex);

		if (!FoundMesh) { continue; }

		for (auto stopIt = Object->StopPoints.begin(); stopIt != Object->StopPoints.end(); stopIt++)
		{
			Vector ObjectCentre = (*stopIt).StopLocation;

			if (stopIt->AffectedConnections.size() > 0)
			{
				for (auto ConnIt = stopIt->AffectedConnections.begin(); ConnIt != stopIt->AffectedConnections.end(); ConnIt++)
				{
					NavOffMeshConnection* Connection = (*ConnIt);

					AIMESH_RemoveOffMeshConnection(Connection);
				}

				stopIt->AffectedConnections.clear();
			}

			for (auto it = FoundMesh->MeshConnections.begin(); it != FoundMesh->MeshConnections.end(); it++)
			{
				NavOffMeshConnection* TestConnection = &(*it);

				if (AIMAP_IsOffMeshConnectionAffectedByObject(Object, ObjectCentre, TestConnection))
				{
					stopIt->AffectedConnections.push_back(TestConnection);
					continue;
				}
			}
		}
	}
}

bool AIMAP_IsOffMeshConnectionAffectedByObject(const DynamicMapObject* TestObject, const Vector& ObjectPosition, const NavOffMeshConnection* Connection)
{
	if (!TestObject || FNullEnt(TestObject->Edict) || !Connection) { return false; }

	NavMovementFlag MovementTypes = static_cast<NavMovementFlag>(Connection->ConnectionFlags);

	if (IsFlagTeleportType(MovementTypes)) { return false; }

	if (TestObject->Type == TRIGGER_TOUCH) { return false; }

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

bool AIMAP_IsDynamicMapTriggerLinkedToObject(edict_t* TriggerObject, edict_t* TargetObject, vector<edict_t*> CheckedObjects)
{
	if (FNullEnt(TriggerObject) || FNullEnt(TargetObject)) { return false; }

	if (TriggerObject == TargetObject) { return true; }

	CheckedObjects.push_back(TriggerObject);

	CBaseEntity* TriggerObjectRef = CBaseEntity::Instance(TriggerObject);
	CBaseEntity* TargetObjectRef = CBaseEntity::Instance(TargetObject);

	if (!TriggerObjectRef || !TargetObjectRef) { return false; }

	// Check the trigger's target for a direct reference, walk down the chain
	if (TriggerObject->v.target)
	{
		const char* Target = STRING(TriggerObject->v.target);

		CBaseEntity* CurrentTarget = UTIL_FindEntityByTargetname(NULL, Target);

		while (CurrentTarget != NULL)
		{
			edict_t* TargetEdict = CurrentTarget->edict();
			if (!FNullEnt(TargetEdict))
			{
				if (find(CheckedObjects.begin(), CheckedObjects.end(), TargetEdict) == CheckedObjects.end())
				{
					if (AIMAP_IsDynamicMapTriggerLinkedToObject(TargetEdict, TargetObject, CheckedObjects)) { return true; }
				}
			}

			CurrentTarget = UTIL_FindEntityByTargetname(CurrentTarget, Target);
		}
	}

	// If the target is locked by us, then we are linked of course
	if (CBaseToggle* TargetToggleRef = dynamic_cast<CBaseToggle*>(TargetObjectRef))
	{
		CBaseEntity* MasterEntity = UTIL_FindEntityByTargetname(NULL, STRING(TargetToggleRef->m_sMaster));

		if (MasterEntity == TriggerObjectRef) { return true; }
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

			if (AIMAP_IsDynamicMapTriggerLinkedToObject(MMTargetEdict, TargetObject, CheckedObjects)) { return true; }
		}

		return false;
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
						if (AIMAP_IsDynamicMapTriggerLinkedToObject(pent, TargetObject, CheckedObjects)) { return true; }
					}
				}
			}

			pent = FIND_ENTITY_BY_CLASSNAME(pent, "multisource");
		}

		return false;
	}

	if (AvHWeldable* WeldableRef = dynamic_cast<AvHWeldable*>(TriggerObjectRef))
	{
		string targetString = WeldableRef->GetTargetOnFinish();
		const char* targetOnFinish = targetString.c_str();
		CBaseEntity* TargetOnFinish = UTIL_FindEntityByTargetname(NULL, targetOnFinish);

		if (!TargetOnFinish) { return false; }

		edict_t* TargetOnFinishEdict = TargetOnFinish->edict();

		if (FNullEnt(TargetOnFinishEdict)) { return false; }

		if (TargetOnFinishEdict == TargetObject) { return true; }

		if (find(CheckedObjects.begin(), CheckedObjects.end(), TargetOnFinishEdict) == CheckedObjects.end())
		{
			if (AIMAP_IsDynamicMapTriggerLinkedToObject(TargetOnFinishEdict, TargetObject, CheckedObjects)) { return true; }
		}

		return false;
	}

	if (CMultiSource* MultiSourceRef = dynamic_cast<CMultiSource*>(TriggerObjectRef))
	{
		for (auto it = DynamicMapObjects.begin(); it != DynamicMapObjects.end(); it++)
		{
			if (it->Edict == TriggerObject) { continue; }
			if (find(CheckedObjects.begin(), CheckedObjects.end(), it->Edict) != CheckedObjects.end()) { continue; }

			CBaseToggle* ToggleObjectRef = dynamic_cast<CBaseToggle*>(CBaseEntity::Instance(it->Edict));

			if (!ToggleObjectRef) { continue; }

			CBaseEntity* ToggleObjectMaster = UTIL_FindEntityByTargetname(NULL, STRING(ToggleObjectRef->m_sMaster));

			if (!ToggleObjectMaster || ToggleObjectMaster != TriggerObjectRef) { continue; }

			if (AIMAP_IsDynamicMapTriggerLinkedToObject(ToggleObjectRef->edict(), TargetObject, CheckedObjects)) { return true; }
		}

		return false;
	}

	return false;
}

DynamicMapObject* AIMAP_GetDynamicObjectByEdict(const edict_t* SearchEdict)
{
	if (FNullEnt(SearchEdict)) { return nullptr; }

	for (auto it = DynamicMapObjects.begin(); it != DynamicMapObjects.end(); it++)
	{
		if (it->Edict == SearchEdict)
		{
			return &(*it);
		}
	}

	return nullptr;
}

void AIMAP_ApplyTempObstaclesToObject(DynamicMapObject* Object, const int Area)
{
	if (!Object) { return; }

	for (auto ObsIt = Object->TempObstacles.begin(); ObsIt != Object->TempObstacles.begin(); ObsIt++)
	{
		AIMESH_RemoveTemporaryObstacle(&(*ObsIt));
	}

	Object->TempObstacles.clear();

	if (FNullEnt(Object->Edict) || Object->Edict->free)
	{
		return;
	}

	float SizeX = Object->Edict->v.size.x;
	float SizeY = Object->Edict->v.size.y;
	float SizeZ = Object->Edict->v.size.z;

	bool bUseXAxis = (SizeX >= SizeY);

	float CylinderRadius = fminf(SizeX, SizeY) * 0.5f;

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
		const int InvalidIndex = static_cast<int>(NAV_MESH_INVALID);

		for (int NavIndex = 0; NavIndex < InvalidIndex; NavIndex++)
		{
			const NavMeshIndex MeshIndex = static_cast<NavMeshIndex>(NavIndex);

			NavTempObstacle* NewObstacle = AIMESH_AddTemporaryObstacle(MeshIndex, CurrentPoint, CylinderRadius, SizeZ, Area);

			if (NewObstacle)
			{
				Object->TempObstacles.push_back(*NewObstacle);
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

	for (auto ObsIt = Object->TempObstacles.begin(); ObsIt != Object->TempObstacles.begin(); ObsIt++)
	{
		AIMESH_RemoveTemporaryObstacle(&(*ObsIt));
	}

	Object->TempObstacles.clear();
}

void AIMAP_ClearCachedMapData()
{
	DynamicMapObjects.clear();
}

void DEBUG_PrintObjectInfo(DynamicMapObject* Object)
{
	char buf[511];
	char interbuf[164];

	sprintf(buf, "Info for %s (%s):\n\n", (Object->Edict->v.targetname != 0) ? STRING(Object->Edict->v.targetname) : "Unnamed", STRING(Object->Edict->v.classname));

	string CurrentType;

	switch (Object->Type)
	{
		case MAPOBJECT_STATIC:
			CurrentType = "Static";
			break;
		case MAPOBJECT_DOOR:
			CurrentType = "Door";
			break;
		case MAPOBJECT_PLATFORM:
			CurrentType = "Platform";
			break;
		case MAPOBJECT_TRAIN:
			CurrentType = "Train";
			break;
		case TRIGGER_WELD:
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
		case OBJECTSTATE_START:
			CurrentState = "Start";
			break;
		case OBJECTSTATE_IDLE:
			CurrentState = "Idle";
			break;
		case OBJECTSTATE_PREPARING:
			CurrentState = "Activated";
			break;
		case OBJECTSTATE_MOVING:
			CurrentState = "Moving";
			break;
		case OBJECTSTATE_OPEN:
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
		DynamicMapObject* ThisTrigger = AIMAP_GetDynamicObjectByEdict((*it));

		if (!ThisTrigger) { continue; }

		sprintf(interbuf, "Trigger %s (%s):\n", (ThisTrigger->Edict->v.targetname != 0) ? STRING(ThisTrigger->Edict->v.targetname) : "Unnamed", STRING(ThisTrigger->Edict->v.classname));
		strcat(buf, interbuf);

		switch (ThisTrigger->State)
		{
			case OBJECTSTATE_IDLE:
				CurrentState = "Idle";
				break;
			case OBJECTSTATE_PREPARING:
				CurrentState = "Activated";
				break;
			case OBJECTSTATE_MOVING:
				CurrentState = "Moving";
				break;
			case OBJECTSTATE_OPEN:
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
			case TRIGGER_TOUCH:
				CurrentState = "Touch";
				break;
			case TRIGGER_USE:
				CurrentState = "Use";
				break;
			case TRIGGER_SHOOT:
				CurrentState = "Shoot";
				break;
			case TRIGGER_BREAK:
				CurrentState = "Break";
				break;
			case TRIGGER_WELD:
				CurrentState = "Weld";
				break;
			case MAPOBJECT_PLATFORM:
				CurrentState = "Touch";
				break;
			case MAPOBJECT_DOOR:
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
