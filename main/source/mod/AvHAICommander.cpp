
#include "AvHAICommander.h"
#include "AvHAITactical.h"
#include "AvHAIMath.h"
#include "AvHAIPlayerUtil.h"
#include "AvHAIWeaponHelper.h"
#include "AvHAINavigation.h"
#include "AvHAITask.h"
#include "AvHAIHelper.h"
#include "AvHAIPlayerManager.h"
#include "AvHAIConfig.h"

#include "AvHSharedUtil.h"
#include "AvHServerUtil.h"

AvHAIBuildableStructure* AICOMM_DeployStructure(AvHAIPlayer* pBot, const EAIStructureType StructureToDeploy, const Vector Location)
{
	if (vIsZero(Location)) { return nullptr; }

	NavAgentProfile WelderProfile = *GetBaseAgentProfile(EAINavProfileIndex::NAV_PROFILE_MARINE);
	WelderProfile.Filters.addIncludeFlags(EAINavMovementFlag::NAV_FLAG_WELD);

	// Don't allow the commander to place a structure somewhere unreachable to marines
	if (!UTIL_PointIsReachable(&WelderProfile, AITAC_GetTeamStartingLocation(pBot->Player->GetTeam()), Location, max_player_use_reach))
	{
		return false;
	}

	AvHMessageID StructureID = UTIL_StructureTypeToImpulseCommand(StructureToDeploy);

	Vector BuildLocation = Location;
	BuildLocation.z += 4.0f;

	// This would be rejected if a human was trying to build here, so don't let the bot do it
	if (!AvHSHUGetIsSiteValidForBuild(StructureID, &BuildLocation))
	{
		return nullptr;
	}

	string theErrorMessage;
	int theCost = 0;
	bool thePurchaseAllowed = pBot->Player->GetPurchaseAllowed(StructureID, theCost, &theErrorMessage);

	if (!thePurchaseAllowed) { return nullptr; }

	CBaseEntity* NewStructureEntity = AvHSUBuildTechForPlayer(StructureID, BuildLocation, pBot->Player);

	if (!NewStructureEntity) { return nullptr; }

	AITAC_RegisterNewBuildableStructure(NewStructureEntity);

	AvHAIBuildableStructure* NewStructure = AITAC_RegisterNewBuildableStructure(NewStructureEntity);

	pBot->Player->PayPurchaseCost(theCost);

	pBot->next_commander_action_time = gpGlobals->time + 1.0f;

	return NewStructure;
}

bool AICOMM_DeployItem(AvHAIPlayer* pBot, EAIDeployableItemType ItemToDeploy, const Vector& Location)
{
	AvHMessageID StructureID =  UTIL_ItemTypeToImpulseCommand(ItemToDeploy);

	Vector BuildLocation = Location;

	string theErrorMessage;
	int theCost = 0;
	bool thePurchaseAllowed = pBot->Player->GetPurchaseAllowed(StructureID, theCost, &theErrorMessage);

	if (!thePurchaseAllowed) { return false; }

	if (!AvHSHUGetIsSiteValidForBuild(StructureID, &BuildLocation)) { return false; }

	CBaseEntity* NewItem = AvHSUBuildTechForPlayer(StructureID, BuildLocation, pBot->Player);

	if (!NewItem) { return false; }

	AITAC_RegisterNewDroppedItem(NewItem, ItemToDeploy);

	pBot->Player->PayPurchaseCost(theCost);

	pBot->next_commander_action_time = gpGlobals->time + 0.2f;

	return true;
}

bool AICOMM_ResearchTech(AvHAIPlayer* pBot, const AvHAIBuildableStructure* StructureToResearch, AvHMessageID Research)
{
	if (!StructureToResearch || !StructureToResearch->IsValid()) { return false; }

	// Don't do anything if the structure is being recycled, or we DON'T want to recycle but the structure is already busy
	if (StructureToResearch->IsRecycling() || (Research != BUILD_RECYCLE && !StructureToResearch->IsIdle())) { return false; }

	int StructureIndex = ENTINDEX(StructureToResearch->Edict);

	if (StructureIndex < 0) { return false; }

	AvHBaseBuildable* BuildableRef = dynamic_cast<AvHBaseBuildable*>(StructureToResearch->EntityRef);

	if (!BuildableRef) { return false; }

	if (!BuildableRef->GetIsTechnologyAvailable(Research)) { return false; }

	AvHTeam* CommanderTeamRef = AIMGR_GetTeamRef(pBot->Player->GetTeam());

	if (!CommanderTeamRef) { return false; }

	AvHResearchManager& theResearchManager = CommanderTeamRef->GetResearchManager();

	bool theIsResearchable = false;
	int theResearchCost = 0.0f;
	float theResearchTime = 0.0f;

	theResearchManager.GetResearchInfo(Research, theIsResearchable, theResearchCost, theResearchTime);

	if (pBot->Player->GetResources() < theResearchCost) { return false; }

	pBot->Player->SetSelection(StructureIndex, true);

	pBot->Button |= IN_ATTACK2;
	pBot->Impulse = Research;

	pBot->next_commander_action_time = gpGlobals->time + 0.2f;

	return true;
}

bool AICOMM_UpgradeStructure(AvHAIPlayer* pBot, const AvHAIBuildableStructure* StructureToUpgrade)
{
	if (!pBot || !StructureToUpgrade || !StructureToUpgrade->IsValid()) { return false; }

	AvHMessageID UpgradeImpulse = MESSAGE_NULL;

	switch (StructureToUpgrade->StructureType)
	{
		case EAIStructureType::STRUCTURE_MARINE_ARMORY:
			UpgradeImpulse = ARMORY_UPGRADE;
			break;
		case EAIStructureType::STRUCTURE_MARINE_TURRETFACTORY:
			UpgradeImpulse = TURRET_FACTORY_UPGRADE;
			break;
		default:
			return false;
	}

	return AICOMM_ResearchTech(pBot, StructureToUpgrade, UpgradeImpulse);
}

bool AICOMM_RecycleStructure(AvHAIPlayer* pBot, const AvHAIBuildableStructure* StructureToRecycle)
{
	if (!StructureToRecycle || !StructureToRecycle->IsValid()) { return false; }

	if (!StructureToRecycle->IsIdle()) { return false; }

	return AICOMM_ResearchTech(pBot, StructureToRecycle, BUILD_RECYCLE);
}

bool AICOMM_IssueMovementOrder(AvHAIPlayer* pBot, AvHPlayer* Recipient, const Vector MoveLocation)
{
	if (!pBot || !Recipient || vIsZero(MoveLocation)) { return false; }

	edict_t* RecipientEdict = Recipient->edict();

	if (FNullEnt(RecipientEdict)) { return false; }

	int RecipientEntIndex = ENTINDEX(RecipientEdict);

	if (RecipientEntIndex < 0) { return false; }

	if (!IsPlayerActiveInGame(RecipientEdict)) { return false; }

	AvHOrder NewOrder;

	NewOrder.SetOrderType(ORDERTYPEL_MOVE);
	NewOrder.SetReceiver(RecipientEntIndex);
	NewOrder.SetLocation(MoveLocation);
	NewOrder.SetOrderID();

	pBot->Player->SetSelection(RecipientEntIndex, true);

	pBot->Player->GiveOrderToSelection(NewOrder);

	return true;
}

bool AICOMM_IssueBuildOrder(AvHAIPlayer* pBot, AvHPlayer* Recipient, const AvHAIBuildableStructure* TargetStructure)
{
	if (!pBot || !Recipient || !TargetStructure || !TargetStructure->IsValid()) { return false; }

	if (TargetStructure->IsRecycling() || TargetStructure->IsCompleted()) { return false; }

	edict_t* RecipientEdict = Recipient->edict();

	if (FNullEnt(RecipientEdict)) { return false; }

	if (!IsPlayerActiveInGame(RecipientEdict)) { return false; }

	int ReceiverIndex = ENTINDEX(RecipientEdict);
	int TargetIndex = ENTINDEX(TargetStructure->Edict);

	if (ReceiverIndex <= 0 || TargetIndex <= 0) { return false; }

	AvHOrder NewOrder;

	NewOrder.SetOrderType(ORDERTYPET_BUILD);
	NewOrder.SetReceiver(ReceiverIndex);
	NewOrder.SetTargetIndex(TargetIndex);
	NewOrder.SetUser3TargetType((AvHUser3)TargetStructure->Edict->v.iuser3);
	NewOrder.SetOrderTargetType(ORDERTARGETTYPE_LOCATION);
	NewOrder.SetLocation(TargetStructure->Location);
	NewOrder.SetOrderID();

	pBot->Player->SetSelection(ReceiverIndex, true);

	pBot->Player->GiveOrderToSelection(NewOrder);

	return true;
}

void AICOMM_CheckNewRequests(AvHAIPlayer* pBot)
{
	if (!pBot) { return; }

	AvHTeam* TeamRef = pBot->Player->GetTeamPointer();

	AlertListType HealthRequests = TeamRef->GetAlerts(COMMANDER_NEXTHEALTH);
	AlertListType AmmoRequests = TeamRef->GetAlerts(COMMANDER_NEXTAMMO);
	AlertListType OrderRequests = TeamRef->GetAlerts(COMMANDER_NEXTIDLE);

	// Cycle through all active health requests and see if any are new or overriding existing ones
	for (auto it = HealthRequests.begin(); it != HealthRequests.end(); it++)
	{
		edict_t* Requestor = INDEXENT(it->GetEntityIndex());
	}

	// Do same for ammo requests
	for (auto it = AmmoRequests.begin(); it != AmmoRequests.end(); it++)
	{
		edict_t* Requestor = INDEXENT(it->GetEntityIndex());
	}

	// And for order requests
	for (auto it = OrderRequests.begin(); it != OrderRequests.end(); it++)
	{
		edict_t* Requestor = INDEXENT(it->GetEntityIndex());
	}
}

void AICOMM_CommanderThink(AvHAIPlayer* pBot)
{

}

bool AICOMM_GetRelocationMessage(Vector RelocationPoint, char* MessageBuffer)
{
	string LocationName = UTIL_GetLocationName(RelocationPoint);

	if (LocationName.empty())
	{
		sprintf(MessageBuffer, "Get ready to relocate");
		return true;
	}

	int MsgIndex = irandrange(0, 2);

	switch (MsgIndex)
	{
		case 0:
			sprintf(MessageBuffer, "We're relocating to %s, lads", LocationName.c_str());
			return true;
		case 1:
			sprintf(MessageBuffer, "Relocate to %s, go go go", LocationName.c_str());
			return true;
		case 2:
			sprintf(MessageBuffer, "I'm relocating to %s", LocationName.c_str());
			return true;
		default:
			sprintf(MessageBuffer, "We're relocating, get ready");
			return true;
	}

	return false;
}

bool AICOMM_ShouldCommanderLeaveChair(AvHAIPlayer* pBot)
{
	return false;
}

bool AICOMM_ShouldBeacon(AvHAIPlayer* pBot)
{
	return false;
}

void AICOMM_ReceiveChatRequest(AvHAIPlayer* Commander, edict_t* Requestor, const char* Request)
{
	AvHMessageID NewRequestType = MESSAGE_NULL;

	if (!stricmp(Request, "shotgun") || !stricmp(Request, "sg") || !stricmp(Request, "shotty"))
	{
		NewRequestType = BUILD_SHOTGUN;
	}
	else if (!stricmp(Request, "welder"))
	{
		NewRequestType = BUILD_WELDER;
	}
	else if (!stricmp(Request, "HMG"))
	{
		NewRequestType = BUILD_HMG;
	}
	else if (!stricmp(Request, "gl"))
	{
		NewRequestType = BUILD_GRENADE_GUN;
	}
	else if (!stricmp(Request, "mines"))
	{
		NewRequestType = BUILD_MINES;
	}
	else if (!stricmp(Request, "ha") || !stricmp(Request, "heavy") || !stricmp(Request, "heavyarmor") || !stricmp(Request, "heavy armor"))
	{
		NewRequestType = BUILD_HEAVY;
	}
	else if (!stricmp(Request, "jp") || !stricmp(Request, "jetpack") || !stricmp(Request, "jet pack"))
	{
		NewRequestType = BUILD_JETPACK;
	}
	else if (!stricmp(Request, "cat") || !stricmp(Request, "cats") || !stricmp(Request, "catalysts"))
	{
		NewRequestType = BUILD_CAT;
	}
	else if (!stricmp(Request, "pg") || !stricmp(Request, "phase") || !stricmp(Request, "phasegate"))
	{
		NewRequestType = BUILD_PHASEGATE;
	}
	else if (!stricmp(Request, "TF") || !stricmp(Request, "turretfactory"))
	{
		NewRequestType = BUILD_TURRET_FACTORY;
	}
	else if (!stricmp(Request, "turret"))
	{
		NewRequestType = BUILD_TURRET;
	}
	else if (!stricmp(Request, "armory") || !stricmp(Request, "armoury"))
	{
		NewRequestType = BUILD_ARMORY;
	}
	else if (!stricmp(Request, "scan"))
	{
		NewRequestType = BUILD_SCAN;
	}
	else if (!stricmp(Request, "cc") || !stricmp(Request, "chair") || !stricmp(Request, "command chair"))
	{
		NewRequestType = BUILD_COMMANDSTATION;
	}
	else if (!stricmp(Request, "obs") || !stricmp(Request, "observatory"))
	{
		NewRequestType = BUILD_OBSERVATORY;
	}
	else if (!stricmp(Request, "pl") || !stricmp(Request, "protolab") || !stricmp(Request, "proto lab") || !stricmp(Request, "prototype lab"))
	{
		NewRequestType = BUILD_PROTOTYPE_LAB;
	}
	else if (!stricmp(Request, "ip") || !stricmp(Request, "portal") || !stricmp(Request, "inf portal") || !stricmp(Request, "infantry portal"))
	{
		NewRequestType = BUILD_INFANTRYPORTAL;
	}
	else if (!stricmp(Request, "al") || !stricmp(Request, "armslab") || !stricmp(Request, "arms lab"))
	{
		NewRequestType = BUILD_ARMSLAB;
	}
	else if (!stricmp(Request, "sc") || !stricmp(Request, "st") || !stricmp(Request, "siegeturret") || !stricmp(Request, "siege turret") || !stricmp(Request, "siegecannon") || !stricmp(Request, "siege cannon"))
	{
		NewRequestType = BUILD_SIEGE;
	}

	if (NewRequestType == MESSAGE_NULL) { return; }
}

bool AICOMM_ShouldCommanderRelocate(AvHAIPlayer* pBot)
{
	if (!CONFIG_IsRelocationAllowed()) { return false; }

	return false;
}
