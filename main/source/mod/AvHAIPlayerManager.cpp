#include "AvHAIPlayerManager.h"
#include "AvHAIPlayer.h"
#include "AvHAIMath.h"
#include "AvHAITactical.h"
#include "AvHAINavigation.h"
#include "AvHAIConfig.h"
#include "AvHAIWeaponHelper.h"
#include "AvHAIHelper.h"
#include "AvHAICommander.h"
#include "AvHAIPlayerUtil.h"
#include "AvHAISoundQueue.h"
#include "AvHGamerules.h"
#include "AvHAIMapData.h"
#include "../dlls/client.h"
#include <time.h>

double last_think_time = 0.0;

vector<AvHAIPlayer> ActiveAIPlayers;
vector<EAINavMeshIndex> RecentlyModifiedNavMeshes;

extern cvar_t avh_botautomode;
extern cvar_t avh_botsenabled;
extern cvar_t avh_botminplayers;
extern cvar_t avh_botusemapdefaults;
extern cvar_t avh_botcommandermode;
extern cvar_t avh_botdebugmode;
extern cvar_t avh_botskill;
extern cvar_t avh_limitteams;

float LastAIPlayerCountUpdate = 0.0f;

int BotNameIndex = 0;

float AIStartedTime = 0.0f; // Used to give 5-second grace period before adding bots

bool bHasRoundStarted = false;

float NextCommanderAllowedTimeTeamA = 0.0f;
float NextCommanderAllowedTimeTeamB = 0.0f;

extern int m_spriteTexture;

bool bPlayerSpawned = false;

float CountdownStartedTime = 0.0f;

bool bBotsEnabled = false;

float CurrentFrameDelta = 0.01f;

#ifdef BOTDEBUG
AvHAIPlayer* DebugAIPlayer = nullptr;
edict_t* DebugBots[MAX_PLAYERS];
const DynamicMapObject* DebugDynamicMapObject = nullptr;
Vector DebugVector1 = ZERO_VECTOR;
Vector DebugVector2 = ZERO_VECTOR;
#endif

EAICommanderMode AIMGR_GetCommanderMode()
{
	if (avh_botcommandermode.value == 1)
	{
		return EAICommanderMode::COMMANDERMODE_ENABLED;
	}

	if (avh_botcommandermode.value == 2)
	{
		return EAICommanderMode::COMMANDERMODE_IFNOHUMAN;
	}

	return EAICommanderMode::COMMANDERMODE_DISABLED;

}

float AIMGR_GetCommanderAllowedTime(AvHTeamNumber Team)
{
	return (Team == GetGameRules()->GetTeamANumber()) ? NextCommanderAllowedTimeTeamA : NextCommanderAllowedTimeTeamB;
}

void AIMGR_UpdateAIPlayerCounts()
{
	for (auto BotIt = ActiveAIPlayers.begin(); BotIt != ActiveAIPlayers.end();)
	{
		AvHAIPlayer* ThisAIPlayer = &(*BotIt);

		// If bot has been kicked from the server then remove from active AI player list
		if (!ThisAIPlayer || !ThisAIPlayer->IsValid())
		{
			BotIt = ActiveAIPlayers.erase(BotIt);
		}
		else
		{
			BotIt++;
		}
	}

	// Don't add or remove bots too quickly, otherwise it can cause lag or even overflows
	if (gpGlobals->time - LastAIPlayerCountUpdate < CONFIG_GetBotFillRate()) { return; }

	LastAIPlayerCountUpdate = gpGlobals->time;

	float MaxMinutes = CONFIG_GetMaxAIMatchTimeMinutes();
	float MaxSeconds = MaxMinutes * 60.0f;

	bool bMatchExceededMaxLength = (GetGameRules()->GetGameTime() > MaxSeconds);

	// If bots are disabled or we've exceeded max AI time and no humans are playing, ensure we've removed all bots from the game
	// Max AI time is configurable in nsbots.ini, and helps prevent infinite stalemates
	// Default time is 90 minutes before bots start leaving to let the map cycle
	if (!AIMGR_IsBotEnabled() || (bMatchExceededMaxLength && AIMGR_GetNumActiveHumanPlayers() == 0) || (AIMGR_HasMatchEnded() && gpGlobals->time - GetGameRules()->GetVictoryTime() > 5.0f))
	{
		if (AIMGR_GetNumAIPlayers() > 0)
		{
			AIMGR_RemoveAIPlayerFromTeam(0);

		}
		return;
	}

	if (!AIMGR_ShouldStartPlayerBalancing()) { return; }

	if (avh_botautomode.value == 1) // Fill teams: bots will be added and removed to maintain a minimum player count
	{
		AIMGR_UpdateFillTeams();
		return;
	}

	if (avh_botautomode.value == 2) // Balance only: bots will only be added and removed to ensure teams remain balanced
	{
		AIMGR_UpdateTeamBalance();
		return;
	}

	// Assume manual mode: do nothing, host can manually add/remove as they wish via sv_addaiplayer
	return;
}

int AIMGR_GetNumPlayersOnTeam(AvHTeamNumber Team)
{
	AvHTeamNumber teamA = GetGameRules()->GetTeamANumber();
	AvHTeamNumber teamB = GetGameRules()->GetTeamBNumber();

	if (Team != teamA && Team != teamB) { return 0; }

	return (Team == teamA) ? GetGameRules()->GetTeamAPlayerCount() : GetGameRules()->GetTeamBPlayerCount();
}

void AIMGR_UpdateTeamBalance()
{
	AvHTeamNumber teamA = GetGameRules()->GetTeamANumber();
	AvHTeamNumber teamB = GetGameRules()->GetTeamBNumber();

	// If team A has more players, then either remove a bot from team A, or add one to B to bring them in line
	if (GetGameRules()->GetTeamAPlayerCount() > GetGameRules()->GetTeamBPlayerCount())
	{
		// Favour removing bots to balance teams over adding more
		if (AIMGR_AIPlayerExistsOnTeam(teamA))
		{
			AIMGR_RemoveAIPlayerFromTeam(1);
			return;
		}
		else
		{
			AIMGR_AddAIPlayerToTeam(2);
			return;
		}
	}

	// Do the same if team B outmatches team A
	if (GetGameRules()->GetTeamBPlayerCount() > GetGameRules()->GetTeamAPlayerCount())
	{
		// Again, favour removing bots over adding more
		if (AIMGR_AIPlayerExistsOnTeam(teamB))
		{
			AIMGR_RemoveAIPlayerFromTeam(2);
			return;
		}
		else
		{
			AIMGR_AddAIPlayerToTeam(1);
			return;
		}
	}

	// If both teams are evenly matched, check to ensure we don't have bots on both sides. The purpose of balance mode
	// is to maintain the minimum bots required to keep teams even, so get rid of extras if needed
	if (AIMGR_AIPlayerExistsOnTeam(teamA) && AIMGR_AIPlayerExistsOnTeam(teamB))
	{
		AIMGR_RemoveAIPlayerFromTeam(teamB);
		return;
	}

}

void AIMGR_UpdateFillTeams()
{
	const char* MapName = STRING(gpGlobals->mapname);

	AvHTeamNumber teamA = GetGameRules()->GetTeamANumber();
	AvHTeamNumber teamB = GetGameRules()->GetTeamBNumber();

	int TeamSizeA = GetGameRules()->GetTeamAPlayerCount();
	int TeamSizeB = GetGameRules()->GetTeamBPlayerCount();

	int NumDesiredTeamA = (avh_botusemapdefaults.value > 0) ? CONFIG_GetTeamASizeForMap(MapName) : (int)ceilf(avh_botminplayers.value * 0.5f);
	int NumDesiredTeamB = (avh_botusemapdefaults.value > 0) ? CONFIG_GetTeamBSizeForMap(MapName) : (int)floorf(avh_botminplayers.value * 0.5f);

	if ((NumDesiredTeamA + NumDesiredTeamB) > gpGlobals->maxClients)
	{
		int Delta = (NumDesiredTeamA + NumDesiredTeamB) - gpGlobals->maxClients;

		bool bRemoveB = true;
		int BotsRemoved = 0;

		while (BotsRemoved < Delta)
		{
			if (bRemoveB)
			{
				NumDesiredTeamB--;
			}
			else
			{
				NumDesiredTeamA--;
			}
			BotsRemoved++;
			bRemoveB = !bRemoveB;
		}
	}

	bool bCanAddToTeamA = (GetGameRules()->GetCheatsEnabled() || TeamSizeA < TeamSizeB || TeamSizeA - TeamSizeB < avh_limitteams.value);
	bool bCanAddToTeamB = (GetGameRules()->GetCheatsEnabled() || TeamSizeB < TeamSizeA || TeamSizeB - TeamSizeA < avh_limitteams.value);

	if (TeamSizeA < NumDesiredTeamA && bCanAddToTeamA)
	{
		// Don't add a bot if we have any stuck in the ready room, wait for teams to resolve themselves
		if (AIMGR_GetNumAIPlayersOnTeam(TEAM_IND) > 0) { return; }
		AIMGR_AddAIPlayerToTeam(1);
		return;
	}

	if (TeamSizeA > NumDesiredTeamA)
	{
		if (AIMGR_GetNumAIPlayersOnTeam(teamA) > 0)
		{
			AIMGR_RemoveAIPlayerFromTeam(1);
			return;
		}
	}

	if (TeamSizeB < NumDesiredTeamB && bCanAddToTeamB)
	{
		// Don't add a bot if we have any stuck in the ready room, wait for teams to resolve themselves
		if (AIMGR_GetNumAIPlayersOnTeam(TEAM_IND) > 0) { return; }
		AIMGR_AddAIPlayerToTeam(2);
		return;
	}

	if (TeamSizeB > NumDesiredTeamB)
	{
		if (AIMGR_GetNumAIPlayersOnTeam(teamB) > 0)
		{
			AIMGR_RemoveAIPlayerFromTeam(2);
			return;
		}
	}

}

void AIMGR_RemoveAIPlayerFromTeam(int Team)
{
	if (AIMGR_GetNumAIPlayers() == 0) { return; }

	AvHTeamNumber DesiredTeam = TEAM_IND;

	AvHTeamNumber teamA = GetGameRules()->GetTeamANumber();
	AvHTeamNumber teamB = GetGameRules()->GetTeamBNumber();

	if (AIMGR_HasMatchEnded() && Team == 0)
	{
		vector<AvHAIPlayer>::iterator ItemToRemove = ActiveAIPlayers.end(); // Current bot to be kicked

		for (auto it = ActiveAIPlayers.begin(); it != ActiveAIPlayers.end(); it++)
		{
			if (IsPlayerInReadyRoom(it->Edict))
			{
				ItemToRemove = it;
				break;
			}
		}

		if (ItemToRemove != ActiveAIPlayers.end())
		{
			ItemToRemove->Player->Kick();

			ActiveAIPlayers.erase(ItemToRemove);
			return;
		}
	}

	if (Team > 0)
	{
		DesiredTeam = (Team == 1) ? teamA : teamB;
	}
	else
	{
		if (GetGameRules()->GetTeamAPlayerCount() > GetGameRules()->GetTeamBPlayerCount())
		{
			DesiredTeam = teamA;
		}
		else
		{
			DesiredTeam = teamB;
		}
	}

	// We will go through the potential bots we could kick. We want to avoid kicking bots which have a lot of
	// resources tied up in them or are commanding, which could cause big disruption to the team they're leaving

	int MinValue = 0; // Track the least valuable bot on the desired team.
	vector<AvHAIPlayer>::iterator ItemToRemove = ActiveAIPlayers.end(); // Current bot to be kicked

	for (auto it = ActiveAIPlayers.begin(); it != ActiveAIPlayers.end(); it++)
	{
		// Don't kick if the slot is empty, or the bot in that slot isn't on the right team
		if (it->Player->GetTeam() != DesiredTeam) { continue; }

		AvHPlayer* theAIPlayer = it->Player;

		float BotValue = theAIPlayer->GetResources();

		AvHPlayerClass theAIPlayerClass = (AvHPlayerClass)theAIPlayer->GetEffectivePlayerClass();

		switch (theAIPlayerClass)
		{
			case PLAYERCLASS_COMMANDER:
				BotValue += 1000.0f; // Ensure this guy isn't kicked unless he's the only bot on the team!
				break;
			case PLAYERCLASS_ALIVE_HEAVY_MARINE:
				BotValue += BALANCE_VAR(kHeavyArmorCost);
				break;
			case PLAYERCLASS_ALIVE_JETPACK_MARINE:
				BotValue += BALANCE_VAR(kJetpackCost);
				break;
			case PLAYERCLASS_ALIVE_LEVEL2:
				BotValue += BALANCE_VAR(kGorgeCost);
				break;
			case PLAYERCLASS_ALIVE_LEVEL3:
				BotValue += BALANCE_VAR(kLerkCost);
				break;
			case PLAYERCLASS_ALIVE_LEVEL4:
				BotValue += BALANCE_VAR(kFadeCost);
				break;
			case PLAYERCLASS_ALIVE_LEVEL5:
				BotValue += BALANCE_VAR(kOnosCost);
				break;
			case PLAYERCLASS_ALIVE_GESTATING:
				BotValue += 10.0f;
				break;
			case PLAYERCLASS_DEAD_ALIEN:
			case PLAYERCLASS_DEAD_MARINE:
				BotValue -= 10.0f; // Favour kicking bots who are dead rather than alive
				break;
			case PLAYERCLASS_REINFORCING:
				BotValue -= 5.0f;
				break;
			default:
				break;
		}

		if (ItemToRemove == ActiveAIPlayers.end() || BotValue < MinValue)
		{
			ItemToRemove = it;
			MinValue = BotValue;
		}
	}


	if (ItemToRemove != ActiveAIPlayers.end())
	{
		ItemToRemove->Player->Kick();

		ActiveAIPlayers.erase(ItemToRemove);
	}

}

void AIMGR_AddAIPlayerToTeam(int Team)
{
	int NewBotIndex = -1;
	edict_t* BotEnt = nullptr;

	// If bots aren't enabled or the game has ended, don't allow new bots to be added
	if (!AIMGR_IsBotEnabled() || GetGameRules()->GetVictoryTeam() != TEAM_IND)
	{
		return;
	}

	if (ActiveAIPlayers.size() >= gpGlobals->maxClients)
	{
		g_engfuncs.pfnServerPrint("Bot limit reached, cannot add more\n");
		return;
	}

	if (AIMGR_GetNumAIPlayers() == 0)
	{
		// Initialise the name index to a random number so we don't always get the same bot names
		BotNameIndex = RANDOM_LONG(0, 31);
	}

	// Retrieve the next configured bot name from the list
	string NewName = CONFIG_GetBotPrefix() + CONFIG_GetNextBotName();

	BotEnt = (*g_engfuncs.pfnCreateFakeClient)(NewName.c_str());

	if (FNullEnt(BotEnt))
	{
		g_engfuncs.pfnServerPrint("Failed to create AI player: server is full\n");
		return;
	}

	AvHTeamNumber DesiredTeam = TEAM_IND;

	AvHTeamNumber teamA = GetGameRules()->GetTeamANumber();
	AvHTeamNumber teamB = GetGameRules()->GetTeamBNumber();

	if (Team > 0)
	{
		DesiredTeam = (Team == 1) ? teamA : teamB;
	}

	BotNameIndex++;

	if (BotNameIndex > 31)
	{
		BotNameIndex = 0;
	}

	char ptr[128];  // allocate space for message from ClientConnect
	int clientIndex;

	char* infobuffer = (*g_engfuncs.pfnGetInfoKeyBuffer)(BotEnt);
	clientIndex = ENTINDEX(BotEnt);

	(*g_engfuncs.pfnSetClientKeyValue)(clientIndex, infobuffer, "model", "");
	(*g_engfuncs.pfnSetClientKeyValue)(clientIndex, infobuffer, "rate", "3500.000000");
	(*g_engfuncs.pfnSetClientKeyValue)(clientIndex, infobuffer, "cl_updaterate", "20");

	(*g_engfuncs.pfnSetClientKeyValue)(clientIndex, infobuffer, "cl_lw", "0");
	(*g_engfuncs.pfnSetClientKeyValue)(clientIndex, infobuffer, "cl_lc", "0");

	(*g_engfuncs.pfnSetClientKeyValue)(clientIndex, infobuffer, "tracker", "0");
	(*g_engfuncs.pfnSetClientKeyValue)(clientIndex, infobuffer, "cl_dlmax", "128");
	(*g_engfuncs.pfnSetClientKeyValue)(clientIndex, infobuffer, "lefthand", "1");
	(*g_engfuncs.pfnSetClientKeyValue)(clientIndex, infobuffer, "friends", "0");
	(*g_engfuncs.pfnSetClientKeyValue)(clientIndex, infobuffer, "dm", "0");
	(*g_engfuncs.pfnSetClientKeyValue)(clientIndex, infobuffer, "ah", "1");
	(*g_engfuncs.pfnSetClientKeyValue)(clientIndex, infobuffer, "_vgui_menus", "0");

	ClientConnect(BotEnt, STRING(BotEnt->v.netname), "127.0.0.1", ptr);
	ClientPutInServer(BotEnt);

	BotEnt->v.flags |= FL_FAKECLIENT; // Shouldn't be needed but just to be sure

	BotEnt->v.idealpitch = BotEnt->v.v_angle.x;
	BotEnt->v.ideal_yaw = BotEnt->v.v_angle.y;

	BotEnt->v.pitch_speed = 270;  // slightly faster than HLDM of 225
	BotEnt->v.yaw_speed = 250; // slightly faster than HLDM of 210

	BotEnt->v.modelindex = 0;

	AvHPlayer* theNewAIPlayer = GetClassPtr((AvHPlayer*)&BotEnt->v);

	if (theNewAIPlayer)
	{
		AvHAIPlayer NewAIPlayer;
		NewAIPlayer.Player = theNewAIPlayer;
		NewAIPlayer.Edict = BotEnt;
		NewAIPlayer.Team = theNewAIPlayer->GetTeam();

		const AvHAISkillLevel* BotSkillSettings = CONFIG_GetBotSkillLevel();

		if (BotSkillSettings)
		{
			memcpy(&NewAIPlayer.BotSkillSettings, BotSkillSettings, sizeof(AvHAISkillLevel));
		}

		ActiveAIPlayers.push_back(NewAIPlayer);

		if (DesiredTeam != TEAM_IND)
		{
			ALERT(at_console, "Adding AI Player to team: %d\n", (int)Team);
			GetGameRules()->AttemptToJoinTeam(theNewAIPlayer, DesiredTeam, false);
		}
		else
		{
			ALERT(at_console, "Auto-assigning AI Player to team\n");
			GetGameRules()->AutoAssignPlayer(theNewAIPlayer);
		}
	}
	else
	{
		ALERT(at_console, "Failed to create AI player: invalid AvHPlayer instance\n");
	}

}

byte BotThrottledMsec(AvHAIPlayer* inAIPlayer, float CurrentTime)
{
	// Thanks to The Storm (ePODBot) for this one, finally fixed the bot running speed!
	int newmsec = (int)roundf((CurrentTime - inAIPlayer->LastServerUpdateTime) * 1000.0f);

	if (newmsec > 255)
	{
		newmsec = 255;
	}

	return (byte)newmsec;
}

#ifdef BOTDEBUG
void AIDEBUG_SetDebugVector1(const Vector NewVector)
{
	DebugVector1 = NewVector;
}

void AIDEBUG_SetDebugVector2(const Vector NewVector)
{
	DebugVector2 = NewVector;
}

Vector AIDEBUG_GetDebugVector1()
{
	return DebugVector1;
}

Vector AIDEBUG_GetDebugVector2()
{
	return DebugVector2;
}

void AIDEBUG_TestPathFind()
{
	if (vIsZero(DebugVector1) || vIsZero(DebugVector2)) { return; }
}

void AIDEBUG_TestFlightPathFind(Vector FromLoc, Vector ToLoc)
{
	if (vIsZero(FromLoc) || vIsZero(ToLoc)) { return; }
}

const DynamicMapObject* AIDEBUG_GetDebugDynamicMapObject()
{
	return DebugDynamicMapObject;
}

void AIDEBUG_SetDebugDynamicMapObject(edict_t* NewObject)
{
	DebugDynamicMapObject = AIMAP_GetDynamicObjectByEdict(NewObject);
}
#endif

void AIMGR_UpdateAIPlayers()
{
	// If bots are not enabled then do nothing
	if (!AIMGR_IsBotEnabled()) { return; }

	static float PrevTime = 0.0f;
	static float CurrTime = 0.0f;

	static int CurrentBotSkill = 1;

	static int UpdateIndex = 0;

	CurrTime = gpGlobals->time;

	if (CurrTime < PrevTime)
	{
		PrevTime = 0.0f;
	}

	float FrameDelta = CurrTime - PrevTime;

	AIMGR_SetFrameDelta(FrameDelta);

	int cvarBotSkill = clampi((int)avh_botskill.value, 0, 3);

	bool bSkillChanged = (cvarBotSkill != CurrentBotSkill);

	if (bSkillChanged)
	{
		CurrentBotSkill = cvarBotSkill;
	}

	if (bHasRoundStarted)
	{
		AvHTeamNumber TeamANumber = GetGameRules()->GetTeamANumber();
		AvHTeamNumber TeamBNumber = GetGameRules()->GetTeamBNumber();

		AvHTeam* TeamA = GetGameRules()->GetTeam(TeamANumber);
		AvHTeam* TeamB = GetGameRules()->GetTeam(TeamBNumber);

		if (TeamA->GetTeamType() == AVH_CLASS_TYPE_MARINE)
		{
			if (TeamA->GetCommanderPlayer() && !(TeamA->GetCommanderPlayer()->pev->flags & FL_FAKECLIENT))
			{
				AIMGR_SetCommanderAllowedTime(TeamANumber, gpGlobals->time + 15.0f);
			}
		}

		if (TeamB->GetTeamType() == AVH_CLASS_TYPE_MARINE)
		{
			if (TeamB->GetCommanderPlayer() && !(TeamB->GetCommanderPlayer()->pev->flags & FL_FAKECLIENT))
			{
				AIMGR_SetCommanderAllowedTime(TeamBNumber, gpGlobals->time + 15.0f);
			}
		}

		AIMGR_ProcessPendingSounds();
	}

	int NumCommanders = AIMGR_GetNumAICommanders();
	int NumRegularBots = AIMGR_GetNumAIPlayers() - NumCommanders;

	int NumBotsThinkThisFrame = 0;

	int BotsPerFrame = max(1, (int)round(BOT_THINK_RATE_HZ * NumRegularBots * FrameDelta));

	int BotIndex = 0;

	for (auto BotIt = ActiveAIPlayers.begin(); BotIt != ActiveAIPlayers.end();)
	{
		AvHAIPlayer* AIPlayer = &(*BotIt);

		if (!AIPlayer || !AIPlayer->IsValid())
		{
			BotIt = ActiveAIPlayers.erase(BotIt);
			continue;
		}

		if (bSkillChanged)
		{
			const AvHAISkillLevel* NewSkillSettings = CONFIG_GetBotSkillLevel();

			if (NewSkillSettings)
			{
				memcpy(&AIPlayer->BotSkillSettings, NewSkillSettings, sizeof(AvHAISkillLevel));
			}
		}

		AIPlayer->StartThink(FrameDelta);

		if (bHasRoundStarted)
		{
			for (int32 i = 0; i < RecentlyModifiedNavMeshes.size(); i++)
			{
				AIPlayer->OnNavMeshModified(RecentlyModifiedNavMeshes[i]);
			}

			if (IsPlayerCommander(AIPlayer->Edict))
			{
				if (UpdateIndex == -1)
				{
					AIPlayer->Think(FrameDelta);
				}
			}
			else
			{
				if (UpdateIndex > -1 && BotIndex >= UpdateIndex && NumBotsThinkThisFrame < BotsPerFrame)
				{
					AIPlayer->Think(FrameDelta);

					NumBotsThinkThisFrame++;
				}

				BotIndex++;
			}
		}

		AIPlayer->EndThink(FrameDelta);

		BotIt++;
	}

	if (UpdateIndex < 0)
	{
		UpdateIndex = 0;
	}
	else
	{
		UpdateIndex += NumBotsThinkThisFrame;
	}

	if (UpdateIndex >= NumRegularBots)
	{
		if (NumCommanders > 0)
		{
			UpdateIndex = -1;
		}
		else
		{
			UpdateIndex = 0;
		}
	}

	RecentlyModifiedNavMeshes.clear();
	PrevTime = CurrTime;
}

int AIMGR_GetNumAIPlayers()
{
	return ActiveAIPlayers.size();
}

int AIMGR_GetNumAICommanders()
{
	int Result = 0;

	for (auto it = ActiveAIPlayers.begin(); it != ActiveAIPlayers.end(); it++)
	{
		if (it->Player->GetUser3() == AVH_USER3_COMMANDER_PLAYER)
		{
			Result++;
		}
	}

	return Result;
}

AvHTeamNumber AIMGR_GetTeamANumber()
{
	return GetGameRules()->GetTeamANumber();
}

AvHTeamNumber AIMGR_GetTeamBNumber()
{
	return GetGameRules()->GetTeamBNumber();
}

AvHTeam* AIMGR_GetTeamRef(const AvHTeamNumber Team)
{
	return GetGameRules()->GetTeam(Team);
}

vector<AvHPlayer*> AIMGR_GetAllPlayersOnTeam(AvHTeamNumber Team)
{
	vector<AvHPlayer*> Result;

	for (int i = 1; i <= gpGlobals->maxClients; i++)
	{
		edict_t* PlayerEdict = INDEXENT(i);

		if (!FNullEnt(PlayerEdict) && (Team == TEAM_IND || PlayerEdict->v.team == Team))
		{
			AvHPlayer* PlayerRef = dynamic_cast<AvHPlayer*>(CBaseEntity::Instance(PlayerEdict));

			if (PlayerRef)
			{
				Result.push_back(PlayerRef);
			}
		}
	}

	return Result;
}

int AIMGR_GetNumAIPlayersOnTeam(AvHTeamNumber Team)
{
	int Result = 0;

	for (auto it = ActiveAIPlayers.begin(); it != ActiveAIPlayers.end(); it++)
	{
		if (it->Player->GetTeam() == Team)
		{
			Result++;
		}
	}

	return Result;
}

int AIMGR_GetNumHumanPlayersOnTeam(AvHTeamNumber Team)
{
	int Result = 0;

	vector<AvHPlayer*> TeamPlayers = AIMGR_GetAllPlayersOnTeam(Team);

	for (auto it = TeamPlayers.begin(); it != TeamPlayers.end(); it++)
	{
		AvHPlayer* ThisPlayer = (*it);
		edict_t* PlayerEdict = ThisPlayer->edict();

		if (!(PlayerEdict->v.flags & FL_FAKECLIENT))
		{
			Result++;
		}
	}

	return Result;
}

int AIMGR_GetNumHumanPlayersOnServer()
{
	int Result = 0;

	for (int i = 1; i <= gpGlobals->maxClients; i++)
	{
		edict_t* PlayerEdict = INDEXENT(i);

		if (!FNullEnt(PlayerEdict) && IsPlayerHuman(PlayerEdict))
		{
			Result++;
		}
	}

	return Result;
}

int AIMGR_GetNumActiveHumanPlayers()
{
	int Result = 0;

	for (int i = 1; i <= gpGlobals->maxClients; i++)
	{
		edict_t* PlayerEdict = INDEXENT(i);

		if (!FNullEnt(PlayerEdict) && IsPlayerHuman(PlayerEdict) && PlayerEdict->v.team != TEAM_IND)
		{
			Result++;
		}
	}

	return Result;
}

int AIMGR_GetNumHumansOfClassOnTeam(AvHTeamNumber Team, AvHUser3 PlayerType)
{
	int Result = 0;

	vector<AvHPlayer*> TeamPlayers = AIMGR_GetAllPlayersOnTeam(Team);

	for (auto it = TeamPlayers.begin(); it != TeamPlayers.end(); it++)
	{
		AvHPlayer* ThisPlayer = (*it);
		edict_t* PlayerEdict = ThisPlayer->edict();

		if (!(PlayerEdict->v.flags & FL_FAKECLIENT))
		{
			Result++;
		}
	}

	return Result;
}

int AIMGR_AIPlayerExistsOnTeam(AvHTeamNumber Team)
{
	for (auto it = ActiveAIPlayers.begin(); it != ActiveAIPlayers.end(); it++)
	{
		if (it->Player->GetTeam() == Team)
		{
			return true;
		}
	}

	return false;
}

void AIMGR_RemoveBotsInReadyRoom()
{
	for (auto it = ActiveAIPlayers.begin(); it != ActiveAIPlayers.end();)
	{
		if (it->Player->GetInReadyRoom())
		{
			it->Player->Kick();
			it = ActiveAIPlayers.erase(it);
		}
		else
		{
			it++;
		}
	}
}

void AIMGR_RoundStarted()
{
	if (!AIMGR_IsBotEnabled()) { return; } // Do nothing if we're not using bots

	bHasRoundStarted = true;

	AvHTeamNumber TeamANumber = GetGameRules()->GetTeamANumber();
	AvHTeamNumber TeamBNumber = GetGameRules()->GetTeamBNumber();

	// If our team has no humans on it, then we can take command right away. Otherwise, wait the allotted grace period to allow the human to take command

	if (AIMGR_GetNumHumanPlayersOnTeam(TeamANumber) > 0)
	{
		AIMGR_SetCommanderAllowedTime(TeamANumber, gpGlobals->time + CONFIG_GetCommanderWaitTime());
	}
	else
	{
		AIMGR_SetCommanderAllowedTime(TeamANumber, 0.0f);
	}

	if (AIMGR_GetNumHumanPlayersOnTeam(TeamBNumber) > 0)
	{
		AIMGR_SetCommanderAllowedTime(TeamBNumber, gpGlobals->time + CONFIG_GetCommanderWaitTime());
	}
	else
	{
		AIMGR_SetCommanderAllowedTime(TeamBNumber, 0.0f);
	}
}

void AIMGR_SetCommanderAllowedTime(AvHTeamNumber Team, float NewValue)
{
	if (Team == GetGameRules()->GetTeamANumber())
	{
		NextCommanderAllowedTimeTeamA = NewValue;
	}
	else
	{
		NextCommanderAllowedTimeTeamB = NewValue;
	}
}

void AIMGR_ClearBotData()
{
	// We have to be careful here, depending on how the nav data is being unloaded, there could be stale references in the ActiveAIPlayers list.
	for (int i = 1; i <= gpGlobals->maxClients; i++)
	{
		edict_t* PlayerEdict = INDEXENT(i);

		if (!FNullEnt(PlayerEdict) && !PlayerEdict->free && (PlayerEdict->v.flags & FL_FAKECLIENT))
		{
			for (auto it = ActiveAIPlayers.begin(); it != ActiveAIPlayers.end();)
			{
				if (it->Edict == PlayerEdict && it->Player)
				{
					it->Player->Kick();
					it = ActiveAIPlayers.erase(it);
				}
				else
				{
					it++;
				}
			}
		}
	}

	// We shouldn't have any bots in the server when this is called, but this ensures no bots end up "orphans" and no longer tracked by the system
	ActiveAIPlayers.clear();
}

void AIMGR_NewMap()
{
	AIMGR_BotPrecache();

	if (!AIMGR_IsBotEnabled()) { return; } // Do nothing if we're not using bots. Data will be already cleared if bot is disabled via AIMGR_OnBotDisabled()

	AIMESH_UnloadNavMesh();
	AIMAP_ClearCachedMapData();
	AITAC_ClearMapAIData();

	AIMESH_LoadNavMesh(STRING(gpGlobals->mapname));
	AIMAP_BuildMapData();

	ActiveAIPlayers.clear();

	AIStartedTime = gpGlobals->time;
	LastAIPlayerCountUpdate = 0.0f;

	bHasRoundStarted = false;

	bPlayerSpawned = false;

	CONFIG_ParseConfigFile();
	CONFIG_PopulateBotNames();
}

void AIMGR_ResetRound()
{
	if (!AIMGR_IsBotEnabled()) { return; } // Do nothing if we're not using bots, as the data will be cleared out via AIMGR_OnBotDisabled()

	AITAC_ClearMapAIData();
	AIMAP_ClearCachedMapData();

	// AI Players would be 0 if the round is being reset because a new game is starting. If the round is reset
	// from a console command, or tournament mode readying up etc, then bot logic is unaffected
	if (AIMGR_GetNumAIPlayers() == 0)
	{
		// This is used to track the 5-second "grace period" before adding bots to the game if fill teams is enabled
		AIStartedTime = gpGlobals->time;
	}

	LastAIPlayerCountUpdate = 0.0f;

	AIMAP_BuildMapData();

	bHasRoundStarted = false;

	CountdownStartedTime = 0.0f;
}

bool AIMGR_IsBotEnabled()
{
	return avh_botsenabled.value > 0;
}

AvHAIPlayer* AIMGR_GetAICommander(AvHTeamNumber Team)
{
	AvHPlayer* ActiveCommander = GetGameRules()->GetTeam(Team)->GetCommanderPlayer();

	if (!ActiveCommander || !(ActiveCommander->pev->flags & FL_FAKECLIENT)) { return nullptr; }

	for (auto it = ActiveAIPlayers.begin(); it != ActiveAIPlayers.end(); it++)
	{
		if (it->Player == ActiveCommander)
		{
			return &(*it);
		}
	}

	return nullptr;
}

AvHAIPlayer* AIMGR_GetBotRefFromPlayer(AvHPlayer* PlayerRef)
{
	for (auto BotIt = ActiveAIPlayers.begin(); BotIt != ActiveAIPlayers.end(); BotIt++)
	{
		if (BotIt->Player == PlayerRef) { return &(*BotIt); }
	}

	return nullptr;
}

AvHAIPlayer* AIMGR_GetBotRefFromEdict(edict_t* PlayerEdict)
{
	for (auto BotIt = ActiveAIPlayers.begin(); BotIt != ActiveAIPlayers.end(); BotIt++)
	{
		if (BotIt->Edict == PlayerEdict) { return &(*BotIt); }
	}

	return nullptr;
}

AvHTeamNumber AIMGR_GetEnemyTeam(const AvHTeamNumber FriendlyTeam)
{
	AvHTeamNumber TeamANumber = GetGameRules()->GetTeamANumber();
	AvHTeamNumber TeamBNumber = GetGameRules()->GetTeamBNumber();

	return (FriendlyTeam == TeamANumber) ? TeamBNumber : TeamANumber;
}

AvHClassType AIMGR_GetTeamType(const AvHTeamNumber Team)
{
	AvHTeam* TeamRef = GetGameRules()->GetTeam(Team);

	return (TeamRef) ? TeamRef->GetTeamType() : AVH_CLASS_TYPE_UNDEFINED;
}

AvHClassType AIMGR_GetEnemyTeamType(const AvHTeamNumber FriendlyTeam)
{
	AvHTeamNumber EnemyTeamNumber = AIMGR_GetEnemyTeam(FriendlyTeam);

	AvHTeam* TeamRef = GetGameRules()->GetTeam(EnemyTeamNumber);

	return (TeamRef) ? TeamRef->GetTeamType() : AVH_CLASS_TYPE_UNDEFINED;
}

vector<AvHAIPlayer*> AIMGR_GetAllAIPlayers()
{
	vector<AvHAIPlayer*> Result;

	Result.clear();

	for (auto BotIt = ActiveAIPlayers.begin(); BotIt != ActiveAIPlayers.end(); BotIt++)
	{
		if (FNullEnt(BotIt->Edict)) { continue; }

		Result.push_back(&(*BotIt));
	}

	return Result;
}

vector<AvHPlayer*> AIMGR_GetAllActivePlayers()
{
	vector<AvHPlayer*> Result;

	for (int i = 1; i <= gpGlobals->maxClients; i++)
	{
		edict_t* PlayerEdict = INDEXENT(i);

		if (!FNullEnt(PlayerEdict) && !PlayerEdict->free && IsPlayerActiveInGame(PlayerEdict))
		{
			AvHPlayer* PlayerRef = dynamic_cast<AvHPlayer*>(CBaseEntity::Instance(PlayerEdict));

			if (PlayerRef)
			{
				Result.push_back(PlayerRef);
			}
		}
	}

	return Result;
}

vector<AvHAIPlayer*> AIMGR_GetAIPlayersOnTeam(AvHTeamNumber Team)
{
	vector<AvHAIPlayer*> Result;

	Result.clear();

	for (auto BotIt = ActiveAIPlayers.begin(); BotIt != ActiveAIPlayers.end(); BotIt++)
	{
		if (FNullEnt(BotIt->Edict)) { continue; }

		if (BotIt->Player->GetTeam() == Team)
		{
			Result.push_back(&(*BotIt));
		}
	}

	return Result;
}

vector<AvHPlayer*> AIMGR_GetNonAIPlayersOnTeam(AvHTeamNumber Team)
{
	vector<AvHPlayer*> TeamPlayers = AIMGR_GetAllPlayersOnTeam(Team);

	for (auto it = ActiveAIPlayers.begin(); it != ActiveAIPlayers.end(); it++)
	{
		AvHPlayer* ThisPlayer = it->Player;

		if (!ThisPlayer) { continue; }

		std::vector<AvHPlayer*>::iterator FoundPlayer = std::find(TeamPlayers.begin(), TeamPlayers.end(), ThisPlayer);

		if (FoundPlayer != TeamPlayers.end())
		{
			TeamPlayers.erase(FoundPlayer);
		}
	}

	return TeamPlayers;
}

bool AIMGR_ShouldStartPlayerBalancing()
{
	if (gpGlobals->time - AIStartedTime < AI_GRACE_PERIOD) { return false; }

	if (AIMGR_HasMatchEnded()) { return false; }

	EAIFillTiming FillTiming = CONFIG_GetBotFillTiming();

	switch (FillTiming)
	{
		case EAIFillTiming::FILLTIMING_MAPLOAD:
				return true;
		case EAIFillTiming::FILLTIMING_ROUNDSTART:
				return GetGameRules()->GetGameStarted();
			default:
				break;
	}

	if (!bPlayerSpawned)
	{
		return (gpGlobals->time - AIStartedTime > AI_MAX_START_TIMEOUT);
	}

	// We've started adding bots, keep going
	if (AIMGR_GetNumAIPlayers() > 0) { return true; }

	for (int i = 1; i <= gpGlobals->maxClients; i++)
	{
		edict_t* PlayerEdict = INDEXENT(i);
		if (FNullEnt(PlayerEdict) || PlayerEdict->free || (PlayerEdict->v.flags & FL_FAKECLIENT)) { continue; } // Ignore fake clients

		AvHPlayer* PlayerRef = dynamic_cast<AvHPlayer*>(CBaseEntity::Instance(PlayerEdict));

		if (!PlayerRef) { continue; }

		if (PlayerRef->GetInReadyRoom()) { return false; } // If there is a human in the ready room, don't add any more bots
	}

	return true;
}

void AIMGR_UpdateAIMapData()
{
	if (!AIMESH_IsNavMeshLoaded()) { return; }

	if (GetGameRules()->GetCountdownStarted() && CountdownStartedTime == 0.0f)
	{
		CountdownStartedTime = gpGlobals->time;
	}

	if (CountdownStartedTime > 0.0f && (gpGlobals->time - 1.0f) > CountdownStartedTime)
	{
		AITAC_UpdateMapAIData();
	}
}

void AIMGR_RegenBotIni()
{
	CONFIG_RegenerateIniFile();
}

void AIMGR_BotPrecache()
{
	m_spriteTexture = PRECACHE_MODEL("sprites/zbeam6.spr");
}

#ifdef BOTDEBUG
AvHAIPlayer* AIMGR_GetDebugAIPlayer()
{
	return DebugAIPlayer;
}

void AIMGR_SetDebugAIPlayer(edict_t* SpectatingPlayer, edict_t* AIPlayer)
{

	int PlayerIndex = ENTINDEX(SpectatingPlayer) - 1;

	if (FNullEnt(AIPlayer))
	{
		DebugBots[PlayerIndex] = nullptr;
		return;
	}

	for (auto it = ActiveAIPlayers.begin(); it != ActiveAIPlayers.end(); it++)
	{
		if (it->Edict == AIPlayer)
		{
			DebugBots[PlayerIndex] = it->Edict;
			return;
		}
	}

	DebugBots[PlayerIndex] = nullptr;
}

void AIDEBUG_DisplayTeamGoals()
{
	AvHTeamNumber TeamANumber = AIMGR_GetTeamANumber();
	AvHTeamNumber TeamBNumber = AIMGR_GetTeamBNumber();

	vector<AvHAIPlayer*> Team1Players = AIMGR_GetAIPlayersOnTeam(TeamANumber);
	vector<AvHAIPlayer*> Team2Players = AIMGR_GetAIPlayersOnTeam(TeamBNumber);

	char buf[511];
	char interbuf[164];

	sprintf(buf, "Team A (%s)\n\n", (AIMGR_GetTeamType(TeamANumber) == AVH_CLASS_TYPE_MARINE) ? "Marines" : "Aliens");

	for (auto it = Team1Players.begin(); it != Team1Players.end(); it++)
	{
		AvHAIPlayer* ThisPlayer = (*it);

		if (!ThisPlayer->CurrentTask || !IsPlayerActiveInGame(ThisPlayer->Edict)) { continue; }

		if (!FNullEnt(ThisPlayer->CurrentTask->TaskTarget))
		{
			sprintf(interbuf, "%s: %s (%s)\n", STRING(ThisPlayer->Player->pev->netname), UTIL_TaskTypeToChar(ThisPlayer->CurrentTask->TaskType), AITAC_StructTypeToChar(UTIL_IUSER3ToStructureType(ThisPlayer->CurrentTask->TaskTarget->v.iuser3)));
		}
		else
		{
			sprintf(interbuf, "%s: %s\n", STRING(ThisPlayer->Player->pev->netname), UTIL_TaskTypeToChar(ThisPlayer->CurrentTask->TaskType));
		}


		strcat(buf, interbuf);
	}

	UTIL_DrawHUDText(INDEXENT(1), 0, 0.1, 0.1f, 255, 255, 255, buf);

	sprintf(buf, "Team B (%s)\n\n", (AIMGR_GetTeamType(TeamBNumber) == AVH_CLASS_TYPE_MARINE) ? "Marines" : "Aliens");

	for (auto it = Team2Players.begin(); it != Team2Players.end(); it++)
	{
		AvHAIPlayer* ThisPlayer = (*it);

		if (!ThisPlayer->CurrentTask || !IsPlayerActiveInGame(ThisPlayer->Edict)) { continue; }

		if (!FNullEnt(ThisPlayer->CurrentTask->TaskTarget))
		{
			sprintf(interbuf, "%s: %s (%s)\n", STRING(ThisPlayer->Player->pev->netname), UTIL_TaskTypeToChar(ThisPlayer->CurrentTask->TaskType), AITAC_StructTypeToChar(UTIL_IUSER3ToStructureType(ThisPlayer->CurrentTask->TaskTarget->v.iuser3)));
		}
		else
		{
			sprintf(interbuf, "%s: %s\n", STRING(ThisPlayer->Player->pev->netname), UTIL_TaskTypeToChar(ThisPlayer->CurrentTask->TaskType));
		}


		strcat(buf, interbuf);
	}

	UTIL_DrawHUDText(INDEXENT(1), 1, 0.6, 0.1f, 255, 255, 255, buf);
}
#endif

void AIMGR_ReceiveCommanderRequest(AvHTeamNumber Team, edict_t* Requestor, const char* Request)
{
	AvHTeam* TeamRef = GetGameRules()->GetTeam(Team);

	if (!TeamRef || TeamRef->GetTeamType() != AVH_CLASS_TYPE_MARINE)
	{
		return;
	}

	AvHAIPlayer* BotCommander = AIMGR_GetAICommander(Team);

	if (BotCommander)
	{
		AICOMM_ReceiveChatRequest(BotCommander, Requestor, Request);
	}
}

void AIMGR_ClientConnected(edict_t* NewClient)
{

}

void AIMGR_PlayerSpawned()
{
	bPlayerSpawned = true;
}

void AIMGR_PrintNavMeshLoadResult(EAINavMeshLoadResult Result, const char* MapName)
{
	char ErrMsg[256];

	switch (Result)
	{
		case EAINavMeshLoadResult::NAVMESH_LOAD_NOTFOUND:
			sprintf(ErrMsg, "No nav file found for %s. Please create or download one and place it in the navmeshes folder in the NS root directory.\n", (MapName) ? MapName : "the current map");
			break;
		case EAINavMeshLoadResult::NAVMESH_LOAD_INVALID:
			sprintf(ErrMsg, "The nav file found for %s is not a valid navmesh file or has been corrupted. Please download or generate a new one.\n", (MapName) ? MapName : "the current map");
			break;
		case EAINavMeshLoadResult::NAVMESH_LOAD_WRONGVERSION:
			sprintf(ErrMsg, "The nav file found for %s uses an outdated version (3.3b9 or earlier). Please download or generate a new one.\n", (MapName) ? MapName : "the current map");
			break;
		case EAINavMeshLoadResult::NAVMESH_STATUS_ALLOCFAIL:
		case EAINavMeshLoadResult::NAVMESH_STATUS_MESHINITFAIL:
		case EAINavMeshLoadResult::NAVMESH_STATUS_CACHEINITFAIL:
		case EAINavMeshLoadResult::NAVMESH_STATUS_QUERYINITFAIL:
			sprintf(ErrMsg, "Failed to allocate memory for the nav data for %s. Possible system instability or RAM issue?\n", (MapName) ? MapName : "the current map");
			break;
		case EAINavMeshLoadResult::NAVMESH_LOAD_SUCCESS:
			sprintf(ErrMsg, "Successfully loaded navigation data for %s.\n", (MapName) ? MapName : "the current map");
			break;
		default:
			return;
	}

	g_engfuncs.pfnServerPrint(ErrMsg);
}

void AIMGR_OnBotEnabled()
{
	AIMAP_ClearCachedMapData();
	AITAC_ClearMapAIData();

	const char* MapName = STRING(gpGlobals->mapname);

	EAINavMeshLoadResult Result = AIMESH_LoadNavMesh(MapName);

	AIMGR_PrintNavMeshLoadResult(Result, MapName);

	if (Result != EAINavMeshLoadResult::NAVMESH_LOAD_SUCCESS)
	{
		return;
	}

	AIMAP_BuildMapData();

	CONFIG_ParseConfigFile();
	CONFIG_PopulateBotNames();

	bBotsEnabled = true;

	ActiveAIPlayers.clear();

	AIStartedTime = gpGlobals->time;
	LastAIPlayerCountUpdate = 0.0f;

	bHasRoundStarted = GetGameRules()->GetGameStarted();

	CountdownStartedTime = (bHasRoundStarted || GetGameRules()->GetCountdownStarted()) ? gpGlobals->time : 0.0f;

}

void AIMGR_OnBotDisabled()
{
	// Clear all data out
	AIMESH_UnloadNavMesh();
	AITAC_ClearMapAIData();
	AIMAP_ClearCachedMapData();

	bBotsEnabled = false;
}

void AIMGR_UpdateAISystem()
{
	AIMGR_UpdateAIPlayerCounts();

	bool bBotsCurrentlyEnabled = AIMGR_IsBotEnabled();

	if (bBotsCurrentlyEnabled != bBotsEnabled)
	{
		if (bBotsCurrentlyEnabled)
		{
			AIMGR_OnBotEnabled();
		}
		else
		{
			AIMGR_OnBotDisabled();
		}

		bBotsEnabled = bBotsCurrentlyEnabled;
		return;
	}

	if (!bBotsCurrentlyEnabled) { return; }

	if (!AIMGR_HasMatchEnded())
	{
		AIMESH_UpdateTileCaches(RecentlyModifiedNavMeshes);
		AIMGR_UpdateAIMapData();
	}

	AIMGR_UpdateAIPlayers();
}

bool AIMGR_HasMatchEnded()
{
	// Game has finished
	if (GetGameRules()->GetVictoryTeam() != TEAM_IND) { return true; }

	// Game is still going, but if it's exceeded the max AI match time and there are no humans playing, consider the match over
	// Helps prevent stalemates if bots get stuck and keeps map rotations going
	float MaxMinutes = CONFIG_GetMaxAIMatchTimeMinutes();
	float MaxSeconds = MaxMinutes * 60.0f;

	bool bMatchExceededMaxLength = (GetGameRules()->GetGameTime() > MaxSeconds);

	return (bMatchExceededMaxLength && AIMGR_GetNumActiveHumanPlayers() == 0);
}

bool AIMGR_IsMatchPracticallyOver()
{
	return false;
}

void AIMGR_ProcessPendingSounds()
{
	float FrameDelta = AIMGR_GetFrameDelta();

	for (auto it = ActiveAIPlayers.begin(); it != ActiveAIPlayers.end(); it++)
	{
		it->HearingThreshold -= FrameDelta;
		it->HearingThreshold = clampf(it->HearingThreshold, 0.0f, 1.0f);
	}

	AvHAISound Sound = AISND_PopSound();

	AvHTeamNumber TeamANumber = AIMGR_GetTeamANumber();
	AvHTeamNumber TeamBNumber = AIMGR_GetTeamBNumber();

	while (Sound.SoundType != EAISoundType::AI_SOUND_NONE)
	{
		edict_t* EmittingEntity = INDEXENT(Sound.EntIndex);
		//string SoundType = "Unknown";

		float MaxDist = 0.0f;

		switch (Sound.SoundType)
		{
			case EAISoundType::AI_SOUND_FOOTSTEP:
				MaxDist = UTIL_MetresToGoldSrcUnits(20.0f);
				//SoundType = "Footstep";
				break;
			case EAISoundType::AI_SOUND_SHOOT:
				MaxDist = UTIL_MetresToGoldSrcUnits(30.0f);
				//SoundType = "Shoot";
				break;
			case EAISoundType::AI_SOUND_VOICELINE:
				MaxDist = UTIL_MetresToGoldSrcUnits(20.0f);
				//SoundType = "Voiceline";
				break;
			case EAISoundType::AI_SOUND_LANDING:
				MaxDist = UTIL_MetresToGoldSrcUnits(20.0f);
				//SoundType = "Landing";
				break;
			case EAISoundType::AI_SOUND_OTHER:
			default:
				MaxDist = UTIL_MetresToGoldSrcUnits(5.0f);
				//SoundType = "Other";
				break;
		}

		MaxDist = sqrf(MaxDist);

		if (!FNullEnt(EmittingEntity) && EmittingEntity->v.team != 0 && IsEdictPlayer(EmittingEntity) && IsPlayerActiveInGame(EmittingEntity))
		{
			AvHTeamNumber EmitterTeam = (AvHTeamNumber)EmittingEntity->v.team;

			for (auto it = ActiveAIPlayers.begin(); it != ActiveAIPlayers.end(); it++)
			{
				AvHAIPlayer* AIPlayer = &(*it);

				if (!AIPlayer || !AIPlayer->IsValid()) { continue; }

				AvHTeamNumber ThisTeam = AIPlayer->Player->GetTeam();
				float Volume = Sound.Volume;
				float HearingThresholdScalar = (ThisTeam != EmitterTeam || EmittingEntity == AIPlayer->Edict) ? 1.0f : 0.5f;

				if (EmitterTeam != ThisTeam)
				{
					float DistFromSound = vDist3DSq(Sound.SoundLocation, it->Edict->v.origin);

					if (DistFromSound > MaxDist) { continue; }

					Volume = Sound.Volume - (Sound.Volume * clampf((DistFromSound / MaxDist), 0.0f, 1.0f));
				}

				Volume = Volume * HearingThresholdScalar;

				if (Volume > AIPlayer->HearingThreshold)
				{
					AIPlayer->HearingThreshold = Volume;

					if (EmitterTeam != ThisTeam)
					{
						AIPlayer->HearEnemy(EmittingEntity, Volume);
					}
				}
			}
		}

		Sound = AISND_PopSound();
	}
}

void AIMGR_SetFrameDelta(float NewValue)
{
	CurrentFrameDelta = NewValue;
}

float AIMGR_GetFrameDelta()
{
	return CurrentFrameDelta;
}

float AIMGR_GetMatchLength()
{
	return (gpGlobals->time - GetGameRules()->GetTimeGameStarted());
}