//
// EvoBot - Neoptolemus' Natural Selection bot, based on Botman's HPB bot template
//
// bot_navigation.h
// 
// Handles all bot path finding and movement
//

#pragma once
#ifndef AVH_AI_COMMANDER_H
#define AVH_AI_COMMANDER_H

#include "AvHAIConstants.h"

static const float MIN_COMMANDER_REMIND_TIME = 20.0f; // How frequently the commander can nag a player to do something, if they don't think they're doing it

AvHAIBuildableStructure* AICOMM_DeployStructure(AvHAIPlayer* pBot, const AvHAIDeployableStructureType StructureToDeploy, const Vector Location, StructurePurpose Purpose = STRUCTURE_PURPOSE_GENERAL, bool bPlacedByHuman = false);
bool AICOMM_DeployItem(AvHAIPlayer* pBot, const AvHAIDeployableItemType ItemToDeploy, const Vector Location);
bool AICOMM_UpgradeStructure(AvHAIPlayer* pBot, AvHAIBuildableStructure* StructureToUpgrade);
bool AICOMM_ResearchTech(AvHAIPlayer* pBot, AvHAIBuildableStructure* StructureToResearch, AvHMessageID Research);
bool AICOMM_RecycleStructure(AvHAIPlayer* pBot, AvHAIBuildableStructure* StructureToRecycle);

bool AICOMM_IssueMovementOrder(AvHAIPlayer* pBot, edict_t* Recipient, const Vector MoveLocation);
bool AICOMM_IssueBuildOrder(AvHAIPlayer* pBot, edict_t* Recipient, edict_t* TargetStructuree);
bool AICOMM_IssueSecureHiveOrder(AvHAIPlayer* pBot, edict_t* Recipient, const AvHAIHiveDefinition* HiveToSecure);
bool AICOMM_IssueSiegeHiveOrder(AvHAIPlayer* pBot, edict_t* Recipient, const AvHAIHiveDefinition* HiveToSiege, const Vector SiegePosition);
bool AICOMM_IssueSecureResNodeOrder(AvHAIPlayer* pBot, edict_t* Recipient, const AvHAIResourceNode* ResNode);

void AICOMM_AssignNewPlayerOrder(AvHAIPlayer* pBot, edict_t* Assignee, edict_t* TargetEntity, AvHAIOrderPurpose OrderPurpose);
void AICOMM_AssignNewPlayerOrder(AvHAIPlayer* pBot, edict_t* Assignee, Vector OrderLocation, AvHAIOrderPurpose OrderPurpose);
int AICOMM_GetNumPlayersAssignedToOrder(AvHAIPlayer* pBot, edict_t* TargetEntity, AvHAIOrderPurpose OrderPurpose);
int AICOMM_GetNumPlayersAssignedToOrderType(AvHAIPlayer* pBot, AvHAIOrderPurpose OrderPurpose);
int AICOMM_GetNumPlayersAssignedToOrderLocation(AvHAIPlayer* pBot, Vector OrderLocation, AvHAIOrderPurpose OrderPurpose);
bool AICOMM_IsOrderStillValid(AvHAIPlayer* pBot, ai_commander_order* Order);
void AICOMM_UpdatePlayerOrders(AvHAIPlayer* pBot);
edict_t* AICOMM_GetPlayerWithNoOrderNearestLocation(AvHAIPlayer* pBot, Vector SearchLocation);
edict_t* AICOMM_GetPlayerWithoutSpecificOrderNearestLocation(AvHAIPlayer* pBot, Vector SearchLocation, AvHAIOrderPurpose OrderPurpose);
bool AICOMM_DoesPlayerOrderNeedReminder(AvHAIPlayer* pBot, ai_commander_order* Order);
void AICOMM_IssueOrderForAssignedJob(AvHAIPlayer* pBot, ai_commander_order* Order);

bool AICOMM_CheckForNextBuildAction(AvHAIPlayer* pBot);
bool AICOMM_CheckForNextSupportAction(AvHAIPlayer* pBot);
bool AICOMM_CheckForNextRecycleAction(AvHAIPlayer* pBot);
bool AICOMM_CheckForNextResearchAction(AvHAIPlayer* pBot);
bool AICOMM_CheckForNextSupplyAction(AvHAIPlayer* pBot);

Vector AICOMM_GetNextScanLocation(AvHAIPlayer* pBot);

void AICOMM_CommanderThink(AvHAIPlayer* pBot);

const AvHAIHiveDefinition* AICOMM_GetEmptyHiveOpportunityNearestLocation(AvHAIPlayer* CommanderBot, const Vector SearchLocation);

ai_commander_request* AICOMM_GetExistingRequestForPlayer(AvHAIPlayer* pBot, edict_t* Requestor);
void AICOMM_CheckNewRequests(AvHAIPlayer* pBot);
bool AICOMM_IsRequestValid(ai_commander_request* Request);

bool AICOMM_IsHiveFullySecured(AvHAIPlayer* CommanderBot, const AvHAIHiveDefinition* Hive, bool bIncludeElectrical);

bool AICOMM_ShouldCommanderLeaveChair(AvHAIPlayer* pBot);

const AvHAIResourceNode* AICOMM_GetNearestResourceNodeCapOpportunity(const AvHTeamNumber Team, const Vector SearchLocation);
const AvHAIHiveDefinition* AICOMM_GetHiveSiegeOpportunityNearestLocation(AvHAIPlayer* CommanderBot, const Vector SearchLocation);

bool AICOMM_ShouldCommanderPrioritiseNodes(AvHAIPlayer* pBot);
bool AICOMM_ShouldBeacon(AvHAIPlayer* pBot);

void AICOMM_ReceiveChatRequest(AvHAIPlayer* Commander, edict_t* Requestor, const char* Request);

bool AICOMM_ShouldCommanderRelocate(AvHAIPlayer* pBot);

bool AICOMM_GetRelocationMessage(Vector RelocationPoint, char* MessageBuffer);

AvHAIMarineBase* AICOMM_AddNewBase(AvHAIPlayer* pBot, Vector NewBaseLocation, MarineBaseType NewBaseType);
bool AICOMM_AddStructureToBase(AvHAIPlayer* pBot, AvHAIDeployableStructureType StructureToDeploy, Vector BuildLocation, AvHAIMarineBase* BaseToAdd);

void AICOMM_ManageActiveBases(AvHAIPlayer* pBot);
bool AICOMM_IsMarineBaseValid(AvHAIMarineBase* Base);
void AICOMM_DeployBases(AvHAIPlayer* pBot);
vector<AvHAIBuildableStructure> AICOMM_GetBaseStructures(AvHAIMarineBase* Base);

void AICOMM_UpdateBaseStatus(AvHAIPlayer* pBot, AvHAIMarineBase* Base);
void AICOMM_UpdateSiegeBaseStatus(AvHAIPlayer* pBot, AvHAIMarineBase* Base);
void AICOMM_UpdateOutpostStatus(AvHAIPlayer* pBot, AvHAIMarineBase* Base);
void AICOMM_UpdateGuardpostStatus(AvHAIPlayer* pBot, AvHAIMarineBase* Base);
void AICOMM_UpdateMainBaseStatus(AvHAIPlayer* pBot, AvHAIMarineBase* Base);

bool AICOMM_BuildOutBase(AvHAIPlayer* pBot, AvHAIMarineBase* BaseToBuildOut);
bool AICOMM_BuildOutMainBase(AvHAIPlayer* pBot, AvHAIMarineBase* BaseToBuildOut);
bool AICOMM_BuildOutOutpost(AvHAIPlayer* pBot, AvHAIMarineBase* BaseToBuildOut);
bool AICOMM_BuildOutSiege(AvHAIPlayer* pBot, AvHAIMarineBase* BaseToBuildOut);
bool AICOMM_BuildOutGuardPost(AvHAIPlayer* pBot, AvHAIMarineBase* BaseToBuildOut);

AvHAIMarineBase* AICOMM_GetNearestBaseToLocation(AvHAIPlayer* pBot, Vector SearchLocation);

#endif // AVH_AI_COMMANDER_H