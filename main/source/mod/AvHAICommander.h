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

#include "AvHAITactical.h"
#include "AvHAIPlayer.h"

static const float MIN_COMMANDER_REMIND_TIME = 20.0f; // How frequently the commander can nag a player to do something, if they don't think they're doing it

AvHAIBuildableStructure* AICOMM_DeployStructure(AvHAIPlayer* pBot, const EAIStructureType StructureToDeploy, const Vector Location);
bool AICOMM_DeployItem(AvHAIPlayer* pBot, EAIDeployableItemType ItemToDeploy, const Vector& Location);
bool AICOMM_UpgradeStructure(AvHAIPlayer* pBot, const AvHAIBuildableStructure* StructureToUpgrade);
bool AICOMM_ResearchTech(AvHAIPlayer* pBot, const AvHAIBuildableStructure* StructureToResearch, AvHMessageID Research);
bool AICOMM_RecycleStructure(AvHAIPlayer* pBot, const AvHAIBuildableStructure* StructureToRecycle);

bool AICOMM_IssueMovementOrder(AvHAIPlayer* pBot, AvHPlayer* Recipient, const Vector MoveLocation);
bool AICOMM_IssueBuildOrder(AvHAIPlayer* pBot, AvHPlayer* Recipient, const AvHAIBuildableStructure* TargetStructuree);

void AICOMM_CommanderThink(AvHAIPlayer* pBot);

void AICOMM_CheckNewRequests(AvHAIPlayer* pBot);

bool AICOMM_ShouldCommanderLeaveChair(AvHAIPlayer* pBot);

bool AICOMM_ShouldBeacon(AvHAIPlayer* pBot);

void AICOMM_ReceiveChatRequest(AvHAIPlayer* Commander, edict_t* Requestor, const char* Request);

bool AICOMM_ShouldCommanderRelocate(AvHAIPlayer* pBot);

bool AICOMM_GetRelocationMessage(Vector RelocationPoint, char* MessageBuffer);

#endif // AVH_AI_COMMANDER_H