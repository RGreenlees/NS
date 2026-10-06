//
// EvoBot - Neoptolemus' Natural Selection bot, based on Botman's HPB bot template
//
// AvHAITactical.cpp
//
// Tracks information about the game state, including structures and research
//

#include "AvHAITactical.h"
#include "AvHAINavigation.h"
#include "AvHAITask.h"
#include "AvHAIMath.h"
#include "AvHAIPlayerUtil.h"
#include "AvHAIHelper.h"
#include "AvHAIConstants.h"
#include "AvHAIPlayerManager.h"
#include "AvHAIConfig.h"
#include "AvHAICommander.h"
#include "AvHAIMapData.h"

#include "AvHGamerules.h"
#include "AvHServerUtil.h"
#include "AvHSharedUtil.h"
#include "AvHMarineEquipment.h"
#include "AvHTurret.h"

#include <float.h>

#include "DetourTileCacheBuilder.h"

vector<AvHAIResourceNode> ResourceNodes;
vector<AvHAIHive> Hives;

float CommanderViewZHeight;

std::vector<AvHAITeamStartingLocation> TeamAStartingLocations;
std::vector<AvHAITeamStartingLocation> TeamBStartingLocations;

AIBuildableStructureMap TeamAStructureMap;
AIBuildableStructureMap TeamBStructureMap;

AIDroppedItemMap MarineDroppedItemMap;

float last_structure_refresh_time = 0.0f;
float last_item_refresh_time = 0.0f;

// Increments by 1 every time the structure list is refreshed. Used to detect if structures have been destroyed and no longer show up
uint32 StructureRefreshFrame = 1;
// Increments by 1 every time the item list is refreshed. Used to detect if items have been removed from play and no longer show up
uint32 ItemRefreshFrame = 1;

bool bEnableRelocationAtStart = false; // For this round, should the AI commander try relocating at the start of the match?

edict_t* LastSeenLerkTeamA = nullptr; // Track who went lerk on team A last time. This ensures we don't get endless cycles of lerks
edict_t* LastSeenLerkTeamB = nullptr; // Track who went lerk on team B last time. This ensures we don't get endless cycles of lerks

float LastSeenLerkTeamATime = 0.0f;
float LastSeenLerkTeamBTime = 0.0f;

bool AITAC_DoesStructureMatchFilter(const AvHAIBuildableStructure* Structure, const StructureSearchFilter* Filter, const Vector& SearchLocation)
{
	if (!Structure || !Structure->IsValid() || !Filter) { return false; }

	if (!EnumHasAnyFlags(Structure->StructureType, Filter->DeployableTypes)) { return false; }
	if (EnumHasAnyFlags(Structure->StructureStatusFlags, Filter->ExcludeStatusFlags)) { return false; }
	if (!EnumHasAllFlags(Structure->StructureStatusFlags, Filter->IncludeStatusFlags)) { return false; }

	const AvHAITeamStartingLocation* ReachabilityCheck = Filter->ReachabilityCheckLocation;

	if (ReachabilityCheck && Filter->ReachabilityFlags != EAIReachabilityFlags::AI_REACHABILITY_NONE)
	{
		AIStructureReachabilityMap::const_iterator FoundEntry = ReachabilityCheck->StructureReachabilityMap.find(Structure);

		// No reachability flags for this structure, fail the check
		if (FoundEntry == ReachabilityCheck->StructureReachabilityMap.end()) { return false; }

		const EAIReachabilityFlags StructureReachabilityFlags = FoundEntry->second;

		if (!EnumHasAnyFlags(StructureReachabilityFlags, Filter->ReachabilityFlags)) { return false; }
	}

	if (vIsZero(SearchLocation) || (Filter->MaxSearchRadius <= 0.0f && Filter->MinSearchRadius <= 0.0f)) { return true; }

	const float MinDistSq = sqrf(Filter->MinSearchRadius);
	const float MaxDistSq = sqrf(Filter->MaxSearchRadius);

	const float DistSq = (Filter->bConsiderPhaseDistance)
		? sqrf(AITAC_GetPhaseDistanceBetweenPoints(Structure->Location, SearchLocation))
		: vDist2DSq(Structure->Location, SearchLocation);

	if (Filter->MaxSearchRadius > 0.0f)
	{
		if (DistSq > MaxDistSq) { return false; }
	}

	if (Filter->MinSearchRadius > 0.0f)
	{
		if (DistSq < MinDistSq) { return false; }
	}

	return true;
}

bool AITAC_DoesDroppedItemMatchFilter(const AvHAIDroppedItem* Item, const DroppedItemSearchFilter* Filter, const Vector& SearchLocation)
{
	if (!Item || !Item->IsValid() || !Filter) { return false; }

	if (!EnumHasAnyFlags(Item->ItemType, Filter->DeployableTypes)) { return false; }

	const AvHAITeamStartingLocation* ReachabilityCheck = Filter->ReachabilityCheckLocation;

	if (ReachabilityCheck && Filter->ReachabilityFlags != EAIReachabilityFlags::AI_REACHABILITY_NONE)
	{
		AIDroppedItemReachabilityMap::const_iterator FoundEntry = ReachabilityCheck->DroppedItemReachabilityMap.find(Item);

		// No reachability flags for this structure, fail the check
		if (FoundEntry == ReachabilityCheck->DroppedItemReachabilityMap.end()) { return false; }

		const EAIReachabilityFlags ItemReachabilityFlags = FoundEntry->second;

		if (!EnumHasAnyFlags(ItemReachabilityFlags, Filter->ReachabilityFlags)) { return false; }
	}

	if (vIsZero(SearchLocation) || (Filter->MaxSearchRadius <= 0.0f && Filter->MinSearchRadius <= 0.0f)) { return true; }

	const float MinDistSq = sqrf(Filter->MinSearchRadius);
	const float MaxDistSq = sqrf(Filter->MaxSearchRadius);

	const float DistSq = (Filter->bConsiderPhaseDistance)
		? sqrf(AITAC_GetPhaseDistanceBetweenPoints(Item->Location, SearchLocation))
		: vDist2DSq(Item->Location, SearchLocation);

	if (Filter->MaxSearchRadius > 0.0f)
	{
		if (DistSq > MaxDistSq) { return false; }
	}

	if (Filter->MinSearchRadius > 0.0f)
	{
		if (DistSq < MinDistSq) { return false; }
	}

	return true;
}

bool AITAC_DoesResourceNodeMatchFilter(const AvHAIResourceNode* ResourceNode, const ResourceNodeSearchFilter* Filter, const Vector& SearchLocation)
{
	if (!ResourceNode || !ResourceNode->IsValid() || !Filter) { return false; }

	if (Filter->OwningTeam > -1 && ResourceNode->OwningTeam != static_cast<AvHTeamNumber>(Filter->OwningTeam)) { return false; }

	const AvHAITeamStartingLocation* ReachabilityCheck = Filter->ReachabilityCheckLocation;

	if (ReachabilityCheck && Filter->ReachabilityFlags != EAIReachabilityFlags::AI_REACHABILITY_NONE)
	{
		AIResourceReachabilityMap::const_iterator FoundEntry = ReachabilityCheck->ResourceReachabilityMap.find(ResourceNode);

		// No reachability flags for this structure, fail the check
		if (FoundEntry == ReachabilityCheck->ResourceReachabilityMap.end()) { return false; }

		const EAIReachabilityFlags ResourceReachabilityFlags = FoundEntry->second;

		if (!EnumHasAnyFlags(ResourceReachabilityFlags, Filter->ReachabilityFlags)) { return false; }
	}

	if (vIsZero(SearchLocation) || (Filter->MaxSearchRadius <= 0.0f && Filter->MinSearchRadius <= 0.0f)) { return true; }

	const float MinDistSq = sqrf(Filter->MinSearchRadius);
	const float MaxDistSq = sqrf(Filter->MaxSearchRadius);

	const float DistSq = (Filter->bConsiderPhaseDistance)
		? sqrf(AITAC_GetPhaseDistanceBetweenPoints(ResourceNode->Location, SearchLocation))
		: vDist2DSq(ResourceNode->Location, SearchLocation);

	if (Filter->MaxSearchRadius > 0.0f)
	{
		if (DistSq > MaxDistSq) { return false; }
	}

	if (Filter->MinSearchRadius > 0.0f)
	{
		if (DistSq < MinDistSq) { return false; }
	}

	return true;
}

AIBuildableStructureList AITAC_FindAllMatchingStructures(const Vector& Location, const StructureSearchFilter* Filter)
{
	AIBuildableStructureList Result;

	AvHTeamNumber TeamA = GetGameRules()->GetTeamANumber();
	AvHTeamNumber TeamB = GetGameRules()->GetTeamBNumber();

	if (Filter->DeployableTeam == TeamA || Filter->DeployableTeam == TEAM_IND)
	{
		for (auto& it : TeamAStructureMap)
		{
			const AvHAIBuildableStructure* Structure = &it.second;

			if (AITAC_DoesStructureMatchFilter(Structure, Filter, Location))
			{
				Result.push_back(Structure);
			}
		}
	}

	if (Filter->DeployableTeam == TeamB || Filter->DeployableTeam == TEAM_IND)
	{
		for (auto& it : TeamBStructureMap)
		{
			const AvHAIBuildableStructure* Structure = &it.second;

			if (AITAC_DoesStructureMatchFilter(Structure, Filter, Location))
			{
				Result.push_back(Structure);
			}
		}
	}

	return Result;
}

const AvHAIBuildableStructure* AITAC_FindSingleMatchingStructure(const Vector& Location, const StructureSearchFilter* Filter, EAIStructureSortType SortBy)
{
	const AIBuildableStructureList AllMatchingStructures = AITAC_FindAllMatchingStructures(Location, Filter);

	if (AllMatchingStructures.empty()) { return nullptr; }

	if (AllMatchingStructures.size() == 1) { return AllMatchingStructures.at(0); }

	if (SortBy == EAIStructureSortType::FIND_STRUCTURE_RANDOM)
	{
		int RandomIndex = RANDOM_LONG(0, AllMatchingStructures.size() - 1);
		return AllMatchingStructures[RandomIndex];
	}

	const AvHAIBuildableStructure* WinningStructure = nullptr;

	float CurrentBestScore = 0.0f;

	for (const AvHAIBuildableStructure* ThisStructure : AllMatchingStructures)
	{
		switch (SortBy)
		{
			case EAIStructureSortType::FIND_STRUCTURE_NEAREST:
			{
				const float ThisScore = vDist2DSq(ThisStructure->Location, Location);

				if (!WinningStructure || ThisScore < CurrentBestScore)
				{
					WinningStructure = ThisStructure;
					CurrentBestScore = ThisScore;
				}
			}
			break;

			case EAIStructureSortType::FIND_STRUCTURE_FURTHEST:
			{
				const float ThisScore = vDist2DSq(ThisStructure->Location, Location);

				if (!WinningStructure || ThisScore > CurrentBestScore)
				{
					WinningStructure = ThisStructure;
					CurrentBestScore = ThisScore;
				}
			}
			break;

			case EAIStructureSortType::FIND_STRUCTURE_WEAKEST:
			{
				const float ThisScore = ThisStructure->HealthPercent;

				if (!WinningStructure || ThisScore < CurrentBestScore)
				{
					WinningStructure = ThisStructure;
					CurrentBestScore = ThisScore;
				}
			}
			break;

			case EAIStructureSortType::FIND_STRUCTURE_STRONGEST:
			{
				const float ThisScore = ThisStructure->HealthPercent;

				if (!WinningStructure || ThisScore > CurrentBestScore)
				{
					WinningStructure = ThisStructure;
					CurrentBestScore = ThisScore;
				}
			}
			break;

			default:
				WinningStructure = ThisStructure;
				break;
		}
	}

	return WinningStructure;
}

const AvHAIDroppedItem* AITAC_GetDroppedItemRefFromEdict(const edict_t* ItemEdict)
{
	if (FNullEnt(ItemEdict)) { return nullptr; }

	const int EntIndex = ENTINDEX(ItemEdict);

	if (EntIndex < 0) { return nullptr; }

	const std::unordered_map<int, AvHAIDroppedItem>::const_iterator Found = MarineDroppedItemMap.find(EntIndex);

	if (Found == MarineDroppedItemMap.end()) { return nullptr; }

	return &Found->second;
}

const AvHAIDroppedItem* AITAC_FindClosestItemToLocation(const Vector& Location, const DroppedItemSearchFilter* ItemFilters)
{
	const AvHAIDroppedItem* Result = nullptr;
	float CurrMinDist = FLT_MAX;

	for (auto& it : MarineDroppedItemMap)
	{
		const AvHAIDroppedItem* Item = &it.second;

		if (AITAC_DoesDroppedItemMatchFilter(Item, ItemFilters, Location))
		{
			const float DistSq = (ItemFilters->bConsiderPhaseDistance)
				? sqrf(AITAC_GetPhaseDistanceBetweenPoints(Item->Location, Location))
				: vDist2DSq(Item->Location, Location);

			if (DistSq < CurrMinDist)
			{
				Result = Item;
				CurrMinDist = DistSq;
			}
		}
	}

	return Result;
}

float AITAC_GetPhaseDistanceBetweenPoints(const Vector StartPoint, const Vector EndPoint)
{
	StructureSearchFilter PGFilter;
	PGFilter.DeployableTypes = EAIStructureType::STRUCTURE_MARINE_PHASEGATE;
	PGFilter.IncludeStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_COMPLETED;
	PGFilter.bConsiderPhaseDistance = false;

	int NumPhaseGates = AITAC_GetNumStructuresAtLocation(ZERO_VECTOR, &PGFilter);

	float DirectDist = vDist2D(StartPoint, EndPoint);

	if (NumPhaseGates < 2)
	{
		return DirectDist;
	}

	PGFilter.MaxSearchRadius = DirectDist;

	const AvHAIBuildableStructure* StartPhase = AITAC_FindSingleMatchingStructure(StartPoint, &PGFilter, EAIStructureSortType::FIND_STRUCTURE_NEAREST);

	if (!StartPhase || !StartPhase->IsValid())
	{
		return DirectDist;
	}

	const AvHAIBuildableStructure* EndPhase = AITAC_FindSingleMatchingStructure(EndPoint, &PGFilter, EAIStructureSortType::FIND_STRUCTURE_NEAREST);

	if (!EndPhase || !EndPhase->IsValid())
	{
		return DirectDist;
	}

	float PhaseDist = vDist2DSq(StartPoint, StartPhase->Location) + vDist2DSq(EndPoint, EndPhase->Location);
	PhaseDist = sqrtf(PhaseDist);

	return fminf(DirectDist, PhaseDist);
}

int	AITAC_GetNumItemsInLocation(const Vector& Location, const DroppedItemSearchFilter* ItemFilters)
{
	int Result = 0;

	for (auto& it : MarineDroppedItemMap)
	{
		const AvHAIDroppedItem* Item = &it.second;

		if (AITAC_DoesDroppedItemMatchFilter(Item, ItemFilters, Location))
		{
			Result++;
		}
	}

	return Result;
}

bool AITAC_ItemExistsInLocation(const Vector& Location, const DroppedItemSearchFilter* ItemFilters)
{
	for (auto& it : MarineDroppedItemMap)
	{
		const AvHAIDroppedItem* Item = &it.second;

		if (AITAC_DoesDroppedItemMatchFilter(Item, ItemFilters, Location))
		{
			return true;
		}
	}

	return false;
}

const AvHAIBuildableStructure* AITAC_GetStructureFromEdict(const edict_t* Structure)
{
	if (FNullEnt(Structure)) { return nullptr; }

	int EntIndex = ENTINDEX(Structure);

	if (EntIndex < 0) { return nullptr; }

	AvHTeamNumber TeamA = GetGameRules()->GetTeamANumber();
	AvHTeamNumber TeamB = GetGameRules()->GetTeamBNumber();

	if (Structure->v.team == TeamA)
	{
		AIBuildableStructureMap::const_iterator Found = TeamAStructureMap.find(EntIndex);

		if (Found == TeamAStructureMap.end()) { return nullptr; }

		return &Found->second;
	}
	else
	{
		AIBuildableStructureMap::const_iterator Found = TeamBStructureMap.find(EntIndex);

		if (Found == TeamBStructureMap.end()) { return nullptr; }

		return &Found->second;
	}

	return nullptr;
}

int	AITAC_GetNumStructuresAtLocation(const Vector& Location, const StructureSearchFilter* Filter)
{
	AvHTeamNumber TeamA = GetGameRules()->GetTeamANumber();
	AvHTeamNumber TeamB = GetGameRules()->GetTeamBNumber();

	int Result = 0;

	if (Filter->DeployableTeam == TeamA || Filter->DeployableTeam == TEAM_IND)
	{
		for (auto& it : TeamAStructureMap)
		{
			const AvHAIBuildableStructure* Structure = &it.second;

			if (AITAC_DoesStructureMatchFilter(Structure, Filter))
			{
				Result++;
			}
		}
	}

	if (Filter->DeployableTeam == TeamB || Filter->DeployableTeam == TEAM_IND)
	{
		for (auto& it : TeamBStructureMap)
		{
			const AvHAIBuildableStructure* Structure = &it.second;

			if (AITAC_DoesStructureMatchFilter(Structure, Filter))
			{
				Result++;
			}
		}
	}

	return Result;
}

void AITAC_PopulateHiveData()
{
	Hives.clear();

	const AvHBaseInfoLocationListType& theInfoLocations = GetGameRules()->GetInfoLocations();

	FOR_ALL_ENTITIES(kesTeamHive, AvHHive*)
		AvHAIHive NewHive;
		NewHive.HiveEntity = theEntity;
		NewHive.Edict = theEntity->edict();
		NewHive.Location = theEntity->pev->origin;

		string HiveName = UTIL_GetLocationName(NewHive.Location);

		if (HiveName.empty())
		{
			sprintf(NewHive.HiveName, "Hive");
		}
		else
		{
			sprintf(NewHive.HiveName, HiveName.c_str(), "%s");
		}

		Hives.push_back(NewHive);

	END_FOR_ALL_ENTITIES(kesTeamHive)
}

void AITAC_RefreshHiveData()
{
	if (ResourceNodes.size() == 0)
	{
		AITAC_PopulateResourceNodes();
	}

	if (Hives.size() == 0)
	{
		AITAC_PopulateHiveData();
	}

	int NextRefresh = 0;

	if (Hives.size() == 0) { return; }

	for (auto it = Hives.begin(); it != Hives.end(); it++)
	{
		AvHAIHive* ThisHive = &(*it);

		if (!ThisHive || !ThisHive->IsValid()) { continue; }

		ThisHive->Update();

		NextRefresh++;
	}

}

Vector AITAC_GetTeamStartingLocation(AvHTeamNumber Team)
{
	return ZERO_VECTOR;
}

void AITAC_RefreshReachabilityForItem(AvHAIDroppedItem* Item)
{
	if (!Item || !Item->IsValid()) { return; }

	if (Item->ItemType == EAIDeployableItemType::DEPLOYABLE_ITEM_SCAN)
	{
		Item->TeamAReachabilityFlags = EAIReachabilityFlags::AI_REACHABILITY_ALL;
		Item->TeamBReachabilityFlags = EAIReachabilityFlags::AI_REACHABILITY_ALL;
		return;
	}

	Item->TeamAReachabilityFlags = EAIReachabilityFlags::AI_REACHABILITY_NONE;
	Item->TeamBReachabilityFlags = EAIReachabilityFlags::AI_REACHABILITY_NONE;
}

void AITAC_CalculateMarineReachabilityFlags(const Vector& FromLocation, const Vector& ToLocation, EAIReachabilityFlags& OutReachabilityFlags, float MaxAcceptableDistance)
{
	OutReachabilityFlags = EAIReachabilityFlags::AI_REACHABILITY_NONE;

	const NavAgentProfile* MarineBaseProfile = GetBaseAgentProfile(EAINavProfileIndex::NAV_PROFILE_MARINE);

	if (!MarineBaseProfile) { return; }

	if (!AIMESH_IsPointOnNavmesh(MarineBaseProfile->MeshIndex, ToLocation)) { return; }

	if (UTIL_PointIsReachable(MarineBaseProfile, FromLocation, ToLocation, MaxAcceptableDistance))
	{
		OutReachabilityFlags = EnumGetCombinedFlags(EAIReachabilityFlags::AI_REACHABILITY_MARINE, EAIReachabilityFlags::AI_REACHABILITY_WELDER);
		return;
	}

	NavAgentProfile WelderProfile = *MarineBaseProfile;
	WelderProfile.Filters.addIncludeFlags(EAINavMovementFlag::NAV_FLAG_WELD);

	if (UTIL_PointIsReachable(&WelderProfile, FromLocation, ToLocation, MaxAcceptableDistance))
	{
		OutReachabilityFlags = EAIReachabilityFlags::AI_REACHABILITY_WELDER;
	}
}

void AITAC_CalculateAlienReachabilityFlags(const Vector& FromLocation, const Vector& ToLocation, EAIReachabilityFlags& OutReachabilityFlags, float MaxAcceptableDistance)
{
	OutReachabilityFlags = EAIReachabilityFlags::AI_REACHABILITY_NONE;

	// Check Onos as their movement profile is unique and uses a different mesh
	if (const NavAgentProfile* OnosBaseProfile = GetBaseAgentProfile(EAINavProfileIndex::NAV_PROFILE_ONOS))
	{
		if (AIMESH_IsPointOnNavmesh(OnosBaseProfile->MeshIndex, ToLocation))
		{
			if (UTIL_PointIsReachable(OnosBaseProfile, FromLocation, ToLocation, MaxAcceptableDistance))
			{
				EnumAddFlags(OutReachabilityFlags, EAIReachabilityFlags::AI_REACHABILITY_ONOS);
			}
		}
	}

	// If the chonky gorge can heave his fat arse here, then any of the other non-Onos aliens can.
	if (const NavAgentProfile* GorgeBaseProfile = GetBaseAgentProfile(EAINavProfileIndex::NAV_PROFILE_GORGE))
	{
		if (AIMESH_IsPointOnNavmesh(GorgeBaseProfile->MeshIndex, ToLocation))
		{
			if (UTIL_PointIsReachable(GorgeBaseProfile, FromLocation, ToLocation, MaxAcceptableDistance))
			{
				OutReachabilityFlags = (EAIReachabilityFlags::AI_REACHABILITY_GORGE
					| EAIReachabilityFlags::AI_REACHABILITY_SKULK
					| EAIReachabilityFlags::AI_REACHABILITY_SKULK_LEAP
					| EAIReachabilityFlags::AI_REACHABILITY_LERK
					| EAIReachabilityFlags::AI_REACHABILITY_FADE
					);
				return;
			}
		}
	}

	if (const NavAgentProfile* SkulkBaseProfile = GetBaseAgentProfile(EAINavProfileIndex::NAV_PROFILE_SKULK))
	{
		if (AIMESH_IsPointOnNavmesh(SkulkBaseProfile->MeshIndex, ToLocation))
		{
			if (UTIL_PointIsReachable(SkulkBaseProfile, FromLocation, ToLocation, MaxAcceptableDistance))
			{
				// Assume that if a basic skulk can reach it, so can fade and lerk (which can blink/fly respectively)
				OutReachabilityFlags = (EAIReachabilityFlags::AI_REACHABILITY_SKULK
					| EAIReachabilityFlags::AI_REACHABILITY_SKULK_LEAP
					| EAIReachabilityFlags::AI_REACHABILITY_LERK
					| EAIReachabilityFlags::AI_REACHABILITY_FADE
					);
				return;
			}
			else
			{
				NavAgentProfile SkulkWithLeapProfile = *SkulkBaseProfile;
				SkulkWithLeapProfile.Filters.addIncludeFlags(EAINavMovementFlag::NAV_FLAG_LEAP);

				if (UTIL_PointIsReachable(&SkulkWithLeapProfile, FromLocation, ToLocation, MaxAcceptableDistance))
				{
					// If a skulk with leap can, then assume lerk and fade also can (since they can blink/fly for leap)
					OutReachabilityFlags = (EAIReachabilityFlags::AI_REACHABILITY_SKULK_LEAP
						| EAIReachabilityFlags::AI_REACHABILITY_LERK
						| EAIReachabilityFlags::AI_REACHABILITY_FADE
						);
					return;
				}
			}
		}
	}

	// Finally, check for any lerk-only reachability given their unique ability to fly
	if (const NavAgentProfile* LerkBaseProfile = GetBaseAgentProfile(EAINavProfileIndex::NAV_PROFILE_LERK))
	{
		if (AIMESH_IsPointOnNavmesh(LerkBaseProfile->MeshIndex, ToLocation))
		{
			if (UTIL_PointIsReachable(LerkBaseProfile, FromLocation, ToLocation, MaxAcceptableDistance))
			{
				OutReachabilityFlags = EAIReachabilityFlags::AI_REACHABILITY_LERK;
				return;
			}
		}
	}
}

void AITAC_PopulateResourceNodes()
{
	ResourceNodes.clear();

	FOR_ALL_ENTITIES(kesFuncResource, AvHFuncResource*)

		AvHAIResourceNode NewResNode;
		NewResNode.ResourceNodeEntity = theEntity;
		NewResNode.Edict = theEntity->edict();
		NewResNode.Location = theEntity->pev->origin;
		NewResNode.bReachabilityMarkedDirty = true;

		ResourceNodes.push_back(NewResNode);

	END_FOR_ALL_ENTITIES(kesFuncResource)
}

void AITAC_RefreshResourceNodes()
{
	if (ResourceNodes.size() == 0)
	{
		AITAC_PopulateResourceNodes();
	}

	for (auto it = ResourceNodes.begin(); it != ResourceNodes.end(); it++)
	{
		AvHAIResourceNode* ResourceNode = &(*it);

		if (!ResourceNode || !ResourceNode->IsValid()) { continue; }

		AvHFuncResource* ResourceEntity = ResourceNode->ResourceNodeEntity;

		ResourceNode->bIsOccupied = ResourceEntity->GetIsOccupied();

		if (ResourceNode->bIsOccupied && !ResourceNode->ActiveTowerEntity)
		{
			StructureSearchFilter TowerFilter;
			TowerFilter.DeployableTypes = (EAIStructureType::STRUCTURE_MARINE_RESTOWER | EAIStructureType::STRUCTURE_ALIEN_RESTOWER);

			std::vector<const AvHAIBuildableStructure*> AllTowerEntities = AITAC_FindAllMatchingStructures(ZERO_VECTOR, &TowerFilter);

			for (auto TowerEntityIt : AllTowerEntities)
			{
				const AvHAIBuildableStructure* TowerStructure = &(*TowerEntityIt);

				AvHResourceTower* TowerEntity = dynamic_cast<AvHResourceTower*>(TowerStructure->EntityRef);

				if (!TowerEntity) { continue; }

				if (TowerEntity->GetHostResource() == ResourceEntity)
				{
					ResourceNode->ActiveTowerEntity = TowerStructure;
					ResourceNode->OwningTeam = TowerStructure->Team;
					break;
				}
			}
		}
		else
		{
			ResourceNode->ActiveTowerEntity = nullptr;
			ResourceNode->OwningTeam = TEAM_IND;
		}
	}
}

void AITAC_UpdateMapAIData()
{
	AITAC_RefreshHiveData();

	if (gpGlobals->time - last_structure_refresh_time >= structure_inventory_refresh_rate)
	{
		AITAC_RefreshBuildableStructures();
		AITAC_RefreshResourceNodes();
		last_structure_refresh_time = gpGlobals->time;
	}

	if (gpGlobals->time - last_item_refresh_time >= item_inventory_refresh_rate)
	{
		AITAC_RefreshMarineItems();
		last_item_refresh_time = gpGlobals->time;
	}

	vector<AvHPlayer*> AllTeamAPlayers = AITAC_GetAllPlayersOnTeamOfClass(GetGameRules()->GetTeamANumber(), AVH_USER3_ALIEN_PLAYER3, nullptr);
	edict_t* LastTeamALerk = LastSeenLerkTeamA;

	if (!FNullEnt(LastTeamALerk) && IsPlayerLerk(LastTeamALerk))
	{
		LastSeenLerkTeamATime = gpGlobals->time;
	}
	else
	{
		for (auto it = AllTeamAPlayers.begin(); it != AllTeamAPlayers.end(); it++)
		{
			edict_t* PlayerEdict = (*it)->edict();

			if (FNullEnt(LastTeamALerk) || IsPlayerHuman(PlayerEdict))
			{
				LastTeamALerk = PlayerEdict;
				LastSeenLerkTeamATime = gpGlobals->time;
			}
		}

		LastSeenLerkTeamA = LastTeamALerk;
	}

	vector<AvHPlayer*> AllTeamBPlayers = AITAC_GetAllPlayersOnTeamOfClass(GetGameRules()->GetTeamBNumber(), AVH_USER3_ALIEN_PLAYER3, nullptr);
	edict_t* LastTeamBLerk = LastSeenLerkTeamB;

	if (!FNullEnt(LastTeamBLerk) && IsPlayerLerk(LastTeamBLerk))
	{
		LastSeenLerkTeamBTime = gpGlobals->time;
	}
	else
	{
		for (auto it = AllTeamBPlayers.begin(); it != AllTeamBPlayers.end(); it++)
		{
			edict_t* PlayerEdict = (*it)->edict();

			if (FNullEnt(LastTeamBLerk) || IsPlayerHuman(PlayerEdict))
			{
				LastTeamBLerk = PlayerEdict;
				LastSeenLerkTeamBTime = gpGlobals->time;
			}
		}

		LastSeenLerkTeamB = LastTeamBLerk;
	}

}

void AITAC_CheckNavMeshModified()
{

}

void AITAC_RefreshBuildableStructures()
{
	if (!AIMESH_IsNavMeshLoaded()) { return; }

	FOR_ALL_BASEENTITIES()
		// We are only interested in buildings and deployed mines
		AvHBaseBuildable* TheBuildableRef = dynamic_cast<AvHBaseBuildable*>(theBaseEntity);

		if (!TheBuildableRef)
		{
			AvHDeployedMine* TheMineRef = dynamic_cast<AvHDeployedMine*>(theBaseEntity);

			if (!TheMineRef)
			{
				continue;
			}
		}

		edict_t* TheBuildableEdict = theBaseEntity->edict();

		if (FNullEnt(TheBuildableEdict)) { continue; }

		const EAIStructureType ThisStructureType = UTIL_IUSER3ToStructureType(TheBuildableEdict->v.iuser3);

		if (ThisStructureType == EAIStructureType::STRUCTURE_NONE || ThisStructureType == EAIStructureType::STRUCTURE_ALIEN_HIVE) { continue; }

		AITAC_UpdateBuildableStructure(theBaseEntity);
	END_FOR_ALL_BASEENTITIES()

	StructureRefreshFrame++;
}

void AITAC_RefreshMarineItems()
{
	if (!AIMESH_IsNavMeshLoaded()) { return; }

	FOR_ALL_ENTITIES(kwsHealth, CBaseEntity*);
		AITAC_RefreshMarineItem(theEntity);
	END_FOR_ALL_ENTITIES(kwsHealth);

	FOR_ALL_ENTITIES(kwsGenericAmmo, CBaseEntity*);
		AITAC_RefreshMarineItem(theEntity);
	END_FOR_ALL_ENTITIES(kwsGenericAmmo);

	FOR_ALL_ENTITIES(kwsHeavyArmor, CBaseEntity*);
		AITAC_RefreshMarineItem(theEntity);
	END_FOR_ALL_ENTITIES(kwsHeavyArmor);

	FOR_ALL_ENTITIES(kwsJetpack, CBaseEntity*);
		AITAC_RefreshMarineItem(theEntity);
	END_FOR_ALL_ENTITIES(kwsJetpack);

	FOR_ALL_ENTITIES(kwsCatalyst, CBaseEntity*);
		AITAC_RefreshMarineItem(theEntity);
	END_FOR_ALL_ENTITIES(kwsCatalyst);

	FOR_ALL_ENTITIES(kwsMine, CBaseEntity*);
		AITAC_RefreshMarineItem(theEntity);
	END_FOR_ALL_ENTITIES(kwsMine);

	FOR_ALL_ENTITIES(kwsShotGun, CBaseEntity*);
		AITAC_RefreshMarineItem(theEntity);
	END_FOR_ALL_ENTITIES(kwsShotGun);

	FOR_ALL_ENTITIES(kwsHeavyMachineGun, CBaseEntity*);
		AITAC_RefreshMarineItem(theEntity);
	END_FOR_ALL_ENTITIES(kwsHeavyMachineGun);

	FOR_ALL_ENTITIES(kwsGrenadeGun, CBaseEntity*);
		AITAC_RefreshMarineItem(theEntity);
	END_FOR_ALL_ENTITIES(kwsGrenadeGun);

	FOR_ALL_ENTITIES(kwsWelder, CBaseEntity*);
		AITAC_RefreshMarineItem(theEntity);
	END_FOR_ALL_ENTITIES(kwsWelder);

	FOR_ALL_ENTITIES(kwsScan, CBaseEntity*);
		AITAC_RefreshMarineItem(theEntity);
	END_FOR_ALL_ENTITIES(kwsScan);

	ItemRefreshFrame++;
}

void AITAC_RefreshMarineItem(CBaseEntity* ItemRef)
{

}

void AITAC_OnItemDropped(const AvHAIDroppedItem* NewItem)
{
	AvHTeamNumber TeamANumber = GetGameRules()->GetTeamANumber();
	AvHTeamNumber TeamBNumber = GetGameRules()->GetTeamBNumber();


	AvHAIPlayer* TeamACommander = AIMGR_GetAICommander(TeamANumber);
	AvHAIPlayer* TeamBCommander = AIMGR_GetAICommander(TeamBNumber);

	if (TeamACommander)
	{
		AITAC_LinkDeployedItemToAction(TeamACommander, NewItem);
	}

	if (TeamBCommander)
	{
		AITAC_LinkDeployedItemToAction(TeamBCommander, NewItem);
	}
}

void AITAC_UpdateBuildableStructure(CBaseEntity* Structure)
{
	if (!Structure || !UTIL_IsEdictActive(Structure->edict()) || (Structure->pev->effects & EF_NODRAW)) { return; }

	const edict_t* BuildingEdict = Structure->edict();

	if (FNullEnt(BuildingEdict)) { return; }

	int EntIndex = ENTINDEX(BuildingEdict);

	if (EntIndex < 0) { return; }

	EAIStructureType StructureType = UTIL_IUSER3ToStructureType(BuildingEdict->v.iuser3);

	if (StructureType == EAIStructureType::STRUCTURE_NONE) { return; }

	AvHTeamNumber TeamANumber = GetGameRules()->GetTeamANumber();

	AIBuildableStructureMap& BuildingMap = ((AvHTeamNumber)BuildingEdict->v.team == TeamANumber) ? TeamAStructureMap : TeamBStructureMap;

	AIBuildableStructureMap::iterator ExistingAIStructureIndex = BuildingMap.find(EntIndex);

	if (ExistingAIStructureIndex == BuildingMap.end())
	{
		AITAC_RegisterNewBuildableStructure(Structure);
		return;
	}

	AvHAIBuildableStructure* StructureRef = &ExistingAIStructureIndex->second;

	if (!StructureRef || !StructureRef->IsValid()) { return; }

	StructureRef->LastSeen = StructureRefreshFrame;

	AvHBaseBuildable* BaseBuildable = dynamic_cast<AvHBaseBuildable*>(Structure);

	if (!BaseBuildable) { return; }

	StructureRef->StructureType = StructureType;

	if (vIsZero(StructureRef->Location) || !vEquals(BaseBuildable->pev->origin, StructureRef->Location, 5.0f))
	{
		AITAC_RefreshReachabilityForStructure(&BuildingMap[EntIndex]);

		StructureRef->Location = BaseBuildable->pev->origin;
	}

	StructureRef->HealthPercent = (BuildingEdict->v.health / BuildingEdict->v.max_health);

	const bool bOldGhost = StructureRef->IsGhost();
	const bool bOldCompleted = StructureRef->IsCompleted();
	const bool bOldRecycling = StructureRef->IsRecycling();

	AITAC_UpdateBuildableStructureStatusFlags(StructureRef);

	if (bOldGhost && !StructureRef->IsGhost())
	{
		AITAC_OnStructureBecomeSolid(StructureRef);
	}

	if (!bOldCompleted && StructureRef->IsCompleted())
	{
		AITAC_OnStructureCompleted(StructureRef);
	}

	if (!bOldRecycling && StructureRef->IsRecycling())
	{
		AITAC_OnStructureBeginRecycling(StructureRef);
	}

	StructureRef->LastSeen = StructureRefreshFrame;
}

void AITAC_UpdateBuildableStructureStatusFlags(AvHAIBuildableStructure* Structure)
{
	if (!Structure || !Structure->IsValid()) { return; }

	int StructureEdictIndex = ENTINDEX(Structure->Edict);

	Structure->StructureStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_NONE;

	AvHBaseBuildable* BaseBuildable = dynamic_cast<AvHBaseBuildable*>(Structure->EntityRef);

	if (!BaseBuildable || Structure->StructureType == EAIStructureType::STRUCTURE_MARINE_DEPLOYEDMINE)
	{
		EnumAddFlags(Structure->StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_COMPLETED);
		return;
	}

	if (Structure->HealthPercent < 1.0f)
	{
		EnumAddFlags(Structure->StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_DAMAGED);
	}

	if (GetGameRules()->GetIsEntityUnderAttack(StructureEdictIndex))
	{
		EnumAddFlags(Structure->StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_UNDERATTACK);
	}

	if (BaseBuildable->GetIsBuilt())
	{
		EnumAddFlags(Structure->StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_COMPLETED);
	}
	else
	{
		if (BaseBuildable->pev->rendermode == kRenderTransTexture)
		{
			EnumAddFlags(Structure->StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_GHOST);
		}
		else
		{
			EnumAddFlags(Structure->StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_PARTIAL);
		}
	}

	if (BaseBuildable->GetIsRecycling())
	{
		EnumAddFlags(Structure->StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_RECYCLING);
	}

	if (BaseBuildable->pev->iuser4 & MASK_UPGRADE_11)
	{
		EnumAddFlags(Structure->StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_ELECTRIFIED);
	}

	if (BaseBuildable->pev->iuser4 & MASK_PARASITED)
	{
		EnumAddFlags(Structure->StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_PARASITED);
	}

	if (BaseBuildable->GetIsResearching())
	{
		EnumAddFlags(Structure->StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_RESEARCHING);
	}

	if (Structure->StructureType == EAIStructureType::STRUCTURE_MARINE_TURRET)
	{
		AvHTurret* TurretRef = dynamic_cast<AvHTurret*>(BaseBuildable);

		if (TurretRef && !TurretRef->GetEnabledState())
		{
			EnumAddFlags(Structure->StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_DISABLED);
		}
	}
}

AvHAIBuildableStructure* AITAC_RegisterNewBuildableStructure(CBaseEntity* NewStructure)
{
	if (!NewStructure) { return nullptr; }

	edict_t* NewStructureEdict = NewStructure->edict();

	if (FNullEnt(NewStructureEdict)) { return nullptr; }

	int NewStructureIndex = ENTINDEX(NewStructureEdict);

	if (NewStructureIndex < 0) { return nullptr; }

	AvHTeamNumber TeamANumber = GetGameRules()->GetTeamANumber();

	AIBuildableStructureMap& BuildingMap = ((AvHTeamNumber)NewStructureEdict->v.team == TeamANumber) ? TeamAStructureMap : TeamBStructureMap;

	AIBuildableStructureMap::iterator ExistingAIStructureIndex = BuildingMap.find(NewStructureIndex);

	if (ExistingAIStructureIndex != BuildingMap.end())
	{
		return &ExistingAIStructureIndex->second;
	}

	AvHAIBuildableStructure NewStructureData;
	NewStructureData.EntityRef = NewStructure;
	NewStructureData.Edict = NewStructureEdict;
	NewStructureData.StructureType = UTIL_IUSER3ToStructureType(NewStructure->pev->iuser3);
	NewStructureData.Location = NewStructure->pev->origin;
	NewStructureData.LastSeen = StructureRefreshFrame;
	NewStructureData.Team = (AvHTeamNumber)NewStructure->pev->team;

	AvHBaseBuildable* BaseBuildable = dynamic_cast<AvHBaseBuildable*>(NewStructure);

	AITAC_UpdateBuildableStructureStatusFlags(&NewStructureData);

	if (!NewStructureData.IsGhost())
	{
		AITAC_AddStructureTemporaryObstacles(&NewStructureData);
	}

	AITAC_RefreshReachabilityForStructure(&NewStructureData);
	AITAC_LinkStructureToPlayer(&NewStructureData);

	AIBuildableStructureMap::iterator NewEntry = BuildingMap.insert(ExistingAIStructureIndex, pair<int, AvHAIBuildableStructure>(NewStructureIndex, NewStructureData));

	return (NewEntry != BuildingMap.end())
		? &NewEntry->second
		: nullptr;
}

AvHAIDroppedItem* AITAC_RegisterNewDroppedItem(CBaseEntity* NewItem, EAIDeployableItemType ItemType)
{
	if (!NewItem || ItemType == EAIDeployableItemType::DEPLOYABLE_ITEM_NONE) { return nullptr; }

	edict_t* NewItemEdict = NewItem->edict();

	if (FNullEnt(NewItemEdict)) { return nullptr; }

	int NewEntIndex = ENTINDEX(NewItemEdict);

	if (NewEntIndex < 0) { return nullptr; }

	AIDroppedItemMap::iterator ExistingIndex = MarineDroppedItemMap.find(NewEntIndex);

	if (ExistingIndex != MarineDroppedItemMap.end())
	{
		return &ExistingIndex->second;
	}

	AvHAIDroppedItem NewItemEntry;
	NewItemEntry.ItemType = ItemType;
	NewItemEntry.Edict = NewItemEdict;
	NewItemEntry.Location = NewItemEdict->v.origin;

	ExistingIndex = MarineDroppedItemMap.insert(ExistingIndex, pair<int, AvHAIDroppedItem>(NewEntIndex, NewItemEntry));

	return (ExistingIndex != MarineDroppedItemMap.end())
		? &ExistingIndex->second
		: nullptr;
}

void AITAC_LinkStructureToPlayer(const AvHAIBuildableStructure* NewStructure)
{

}

void AITAC_RefreshReachabilityForStructure(AvHAIBuildableStructure* Structure)
{
	if (!Structure || !Structure->IsValid()) { return; }
}

void AITAC_OnStructureBecomeSolid(AvHAIBuildableStructure* Structure)
{
	if (!Structure || !Structure->IsValid()) { return; }

	AITAC_AddStructureTemporaryObstacles(Structure);
}

void AITAC_OnStructureCompleted(AvHAIBuildableStructure* Structure)
{
	if (!Structure || !Structure->IsValid()) { return; }
}

void AITAC_OnStructureBeginRecycling(AvHAIBuildableStructure* RecyclingStructure)
{
	if (!RecyclingStructure || !RecyclingStructure->IsValid()) { return; }
}

void AITAC_OnStructureDestroyed(AvHAIBuildableStructure* DestroyedStructure)
{
	if (!DestroyedStructure || !DestroyedStructure->IsValid()) { return; }


}

void AITAC_LinkDeployedItemToAction(AvHAIPlayer* CommanderBot, const AvHAIDroppedItem* NewItem)
{

}

void AITAC_ClearStructureNavData()
{
	for (auto& it : TeamAStructureMap)
	{
		AvHAIBuildableStructure* Structure = &it.second;

		if (!Structure) { continue; }

		Structure->ClearNavInformation();
	}

	for (auto& it : TeamBStructureMap)
	{
		AvHAIBuildableStructure* Structure = &it.second;

		if (!Structure) { continue; }

		Structure->ClearNavInformation();
	}
}

void AITAC_ClearHiveNavData()
{
	for (auto HiveIt = Hives.begin(); HiveIt != Hives.end(); HiveIt++)
	{
		AvHAIHive* ThisHive = &(*HiveIt);

		if (!ThisHive) { continue; }

		ThisHive->ClearNavInformation();
	}
}

void AITAC_ClearMapAIData()
{
	UTIL_ClearLocalizations();

	// If we are clearing with the nav mesh still loaded (e.g. round restart rather than a new map) then clean up the nav mesh nicely.
	if (AIMESH_IsNavMeshLoaded())
	{
		AITAC_ClearStructureNavData();
		AITAC_ClearHiveNavData();
	}

	MarineDroppedItemMap.clear();
	TeamAStructureMap.clear();
	TeamBStructureMap.clear();
	ResourceNodes.clear();

	StructureRefreshFrame = 1;
	ItemRefreshFrame = 1;

	last_structure_refresh_time = 0.0f;
	last_item_refresh_time = 0.0f;
}

void AITAC_RefreshTeamStartingLocations()
{
	AITAC_GetTeamStartingLocation(GetGameRules()->GetTeamANumber());
}

EAIStructureType UTIL_IUSER3ToStructureType(const int inIUSER3)
{
	switch (inIUSER3)
	{
		case AVH_USER3_COMMANDER_STATION:
			return EAIStructureType::STRUCTURE_MARINE_COMMCHAIR;
		case AVH_USER3_RESTOWER:
			return EAIStructureType::STRUCTURE_MARINE_RESTOWER;
		case AVH_USER3_INFANTRYPORTAL:
			return EAIStructureType::STRUCTURE_MARINE_INFANTRYPORTAL;
		case AVH_USER3_ARMORY:
			return EAIStructureType::STRUCTURE_MARINE_ARMORY;
		case AVH_USER3_ADVANCED_ARMORY:
			return EAIStructureType::STRUCTURE_MARINE_ADVARMORY;
		case AVH_USER3_TURRET_FACTORY:
			return EAIStructureType::STRUCTURE_MARINE_TURRETFACTORY;
		case AVH_USER3_ADVANCED_TURRET_FACTORY:
			return EAIStructureType::STRUCTURE_MARINE_ADVTURRETFACTORY;
		case AVH_USER3_TURRET:
			return EAIStructureType::STRUCTURE_MARINE_TURRET;
		case AVH_USER3_SIEGETURRET:
			return EAIStructureType::STRUCTURE_MARINE_SIEGETURRET;
		case AVH_USER3_ARMSLAB:
			return EAIStructureType::STRUCTURE_MARINE_ARMSLAB;
		case AVH_USER3_PROTOTYPE_LAB:
			return EAIStructureType::STRUCTURE_MARINE_PROTOTYPELAB;
		case AVH_USER3_OBSERVATORY:
			return EAIStructureType::STRUCTURE_MARINE_OBSERVATORY;
		case AVH_USER3_PHASEGATE:
			return EAIStructureType::STRUCTURE_MARINE_PHASEGATE;
		case AVH_USER3_MINE:
			return EAIStructureType::STRUCTURE_MARINE_DEPLOYEDMINE;

		case AVH_USER3_ALIENRESTOWER:
			return EAIStructureType::STRUCTURE_ALIEN_RESTOWER;
		case AVH_USER3_DEFENSE_CHAMBER:
			return EAIStructureType::STRUCTURE_ALIEN_DEFENSECHAMBER;
		case AVH_USER3_MOVEMENT_CHAMBER:
			return EAIStructureType::STRUCTURE_ALIEN_MOVEMENTCHAMBER;
		case AVH_USER3_SENSORY_CHAMBER:
			return EAIStructureType::STRUCTURE_ALIEN_SENSORYCHAMBER;
		case AVH_USER3_OFFENSE_CHAMBER:
			return EAIStructureType::STRUCTURE_ALIEN_OFFENSECHAMBER;

		default:
			return EAIStructureType::STRUCTURE_NONE;
	}

	return EAIStructureType::STRUCTURE_NONE;
}

bool UTIL_ShouldStructureCollide(const AvHAIBuildableStructure* Structure)
{
	if (!Structure || !Structure->IsValid()) { return false; }

	if (Structure->StructureType == EAIStructureType::STRUCTURE_NONE) { return false; }

	switch (Structure->StructureType)
	{
		case EAIStructureType::STRUCTURE_MARINE_INFANTRYPORTAL:
		case EAIStructureType::STRUCTURE_MARINE_PHASEGATE:
		case EAIStructureType::STRUCTURE_MARINE_DEPLOYEDMINE:
			return false;
		default:
			return true;
	}

	return true;
}

float UTIL_GetStructureRadiusForObstruction(const AvHAIBuildableStructure* Structure)
{
	if (!Structure || !Structure->IsValid()) { return 0.0f; }

	if (Structure->StructureType == EAIStructureType::STRUCTURE_NONE) { return 0.0f; }

	switch (Structure->StructureType)
	{
		case EAIStructureType::STRUCTURE_MARINE_TURRETFACTORY:
		case EAIStructureType::STRUCTURE_MARINE_COMMCHAIR:
			return 60.0f;
		case EAIStructureType::STRUCTURE_MARINE_TURRET:
			return 30.0f;
		case EAIStructureType::STRUCTURE_MARINE_DEPLOYEDMINE:
			return 12.0f;
		default:
			return 40.0f;
	}

	return 40.0f;
}

EAINavArea UTIL_GetAreaForStructuralObstruction(const AvHAIBuildableStructure* Structure)
{
	if (Structure->StructureType == EAIStructureType::STRUCTURE_NONE) { return EAINavArea::NAV_AREA_NULL; }

	AvHTeamNumber TeamA = GetGameRules()->GetTeamANumber();
	AvHTeamNumber TeamB = GetGameRules()->GetTeamBNumber();

	const EAINavArea TeamStructureArea = (Structure->Team == TeamA) ? EAINavArea::NAV_AREA_BLOCKAGE_TEAM1 : EAINavArea::NAV_AREA_BLOCKAGE_TEAM2;

	switch (Structure->StructureType)
	{
		case EAIStructureType::STRUCTURE_MARINE_COMMCHAIR:
		case EAIStructureType::STRUCTURE_MARINE_ARMORY:
		case EAIStructureType::STRUCTURE_MARINE_ADVARMORY:
		case EAIStructureType::STRUCTURE_MARINE_OBSERVATORY:
		case EAIStructureType::STRUCTURE_ALIEN_RESTOWER:
		case EAIStructureType::STRUCTURE_MARINE_RESTOWER:
		case EAIStructureType::STRUCTURE_ALIEN_HIVE:
			return TeamStructureArea;
		default:
			return EAINavArea::NAV_AREA_OBSTRUCTED;
	}

	return EAINavArea::NAV_AREA_OBSTRUCTED;
}

void AITAC_ClearStructureTemporaryObstacles(AvHAIBuildableStructure* Structure)
{
	if (!Structure) { return; }

	if (Structure->TempObstacles.empty()) { return; }

	for (auto it = Structure->TempObstacles.begin(); it != Structure->TempObstacles.end(); it++)
	{
		NavTempObstacle* TempObstacle = (*it);

		AIMESH_RemoveTemporaryObstacle(TempObstacle);
	}

	Structure->TempObstacles.clear();
}

void AITAC_AddStructureTemporaryObstacles(AvHAIBuildableStructure* Structure)
{
	if (!Structure || !Structure->IsValid()) { return; }

	if (Structure->StructureType == EAIStructureType::STRUCTURE_MARINE_DEPLOYEDMINE) { return; }

	AITAC_ClearStructureTemporaryObstacles(Structure);

	const bool bCollideWithPlayers = UTIL_ShouldStructureCollide(Structure);

	const float Radius = UTIL_GetStructureRadiusForObstruction(Structure);

	if (Radius <= 0.0f) { return; }

	// Not all structures collide with players (e.g. phase gate)
	if (bCollideWithPlayers)
	{
		const EAINavArea StructureObstacleArea = UTIL_GetAreaForStructuralObstruction(Structure);

		NavTempObstacle* NewRegularMeshObstacle = AIMESH_AddTemporaryObstacle(EAINavMeshIndex::NAV_MESH_REGULAR, UTIL_GetCentreOfEntity(Structure->Edict), Radius, 100.0f, StructureObstacleArea);

		if (NewRegularMeshObstacle)
		{
			Structure->TempObstacles.push_back(NewRegularMeshObstacle);
		}

		NavTempObstacle* NewOnosMeshObstacle = AIMESH_AddTemporaryObstacle(EAINavMeshIndex::NAV_MESH_ONOS, UTIL_GetCentreOfEntity(Structure->Edict), Radius, 100.0f, StructureObstacleArea);

		if (NewOnosMeshObstacle)
		{
			Structure->TempObstacles.push_back(NewOnosMeshObstacle);
		}
	}

	NavTempObstacle* NewBuildingMeshObstacle = AIMESH_AddTemporaryObstacle(EAINavMeshIndex::NAV_MESH_CONSTRUCTION, UTIL_GetCentreOfEntity(Structure->Edict), Radius, 100.0f, EAINavArea::NAV_AREA_NULL);

	if (NewBuildingMeshObstacle)
	{
		Structure->TempObstacles.push_back(NewBuildingMeshObstacle);
	}
}

EAIWeaponId UTIL_GetWeaponTypeFromEdict(const edict_t* ItemEdict)
{
	int Index = ENTINDEX(ItemEdict);

	if (Index < 0) { return EAIWeaponId::WEAPON_INVALID; }

	EAIDeployableItemType ItemType = MarineDroppedItemMap[Index].ItemType;

	switch (ItemType)
	{
	case EAIDeployableItemType::DEPLOYABLE_ITEM_WELDER:
		return EAIWeaponId::WEAPON_MARINE_WELDER;
	case EAIDeployableItemType::DEPLOYABLE_ITEM_HMG:
		return EAIWeaponId::WEAPON_MARINE_HMG;
	case EAIDeployableItemType::DEPLOYABLE_ITEM_GRENADELAUNCHER:
		return EAIWeaponId::WEAPON_MARINE_GL;
	case EAIDeployableItemType::DEPLOYABLE_ITEM_SHOTGUN:
		return EAIWeaponId::WEAPON_MARINE_SHOTGUN;
	case EAIDeployableItemType::DEPLOYABLE_ITEM_MINES:
		return EAIWeaponId::WEAPON_MARINE_MINES;
	default:
		return EAIWeaponId::WEAPON_INVALID;
	}

	return EAIWeaponId::WEAPON_INVALID;
}

bool AITAC_MarineResearchIsAvailable(const AvHTeamNumber Team, const AvHMessageID Research)
{
	AvHTeam* PlayerTeam = GetGameRules()->GetTeam(Team);

	if (!PlayerTeam) { return false; }

	AvHMessageID Message = Research;

	return PlayerTeam->GetResearchManager().GetIsMessageAvailable(Message);
}

const AvHAIHive* AITAC_GetHiveFromEdict(const edict_t* Edict)
{
	if (Edict->v.iuser3 != AVH_USER3_HIVE) { return nullptr; }

	for (auto it = Hives.begin(); it != Hives.end(); it++)
	{
		if (it->Edict == Edict)
		{
			return &(*it);
		}
	}

	return nullptr;
}

const AvHAIResourceNode* AITAC_GetResourceNodeFromEdict(const edict_t* Edict)
{
	for (auto it = ResourceNodes.begin(); it != ResourceNodes.end(); it++)
	{
		if (it->Edict == Edict)
		{
			return &(*it);
		}
	}

	return nullptr;
}

int	AITAC_GetNumResourceNodesNearLocation(const Vector Location, const ResourceNodeSearchFilter* Filter)
{
	int Result = 0;

	for (auto it = ResourceNodes.begin(); it != ResourceNodes.end(); it++)
	{
		const AvHAIResourceNode* ResourceNode = &(*it);

		if (AITAC_DoesResourceNodeMatchFilter(ResourceNode, Filter, Location))
		{
			Result++;
		}
	}

	return Result;
}

vector<const AvHAIResourceNode*> AITAC_GetAllMatchingResourceNodes(const Vector Location, const ResourceNodeSearchFilter* Filter)
{
	vector<const AvHAIResourceNode*> Results;

	for (auto it = ResourceNodes.begin(); it != ResourceNodes.end(); it++)
	{
		const AvHAIResourceNode* ResourceNode = &(*it);

		if (AITAC_DoesResourceNodeMatchFilter(ResourceNode, Filter, Location))
		{
			Results.push_back(ResourceNode);
		}
	}

	return Results;
}

const AvHAIResourceNode* AITAC_FindNearestResourceNodeToLocation(const Vector Location, const ResourceNodeSearchFilter* Filter)
{
	const AvHAIResourceNode* Result = nullptr;

	float CurrMinDist = FLT_MAX;

	for (auto it = ResourceNodes.begin(); it != ResourceNodes.end(); it++)
	{
		const AvHAIResourceNode* ResourceNode = &(*it);

		if (!AITAC_DoesResourceNodeMatchFilter(ResourceNode, Filter, Location)) { continue; }

		const float DistSq = (Filter->bConsiderPhaseDistance)
			? sqrf(AITAC_GetPhaseDistanceBetweenPoints(ResourceNode->Location, Location))
			: vDist2DSq(ResourceNode->Location, Location);

		if (DistSq < CurrMinDist)
		{
			CurrMinDist = DistSq;
			Result = ResourceNode;
		}
	}

	return Result;
}

int AITAC_GetNumActivePlayersOnTeam(const AvHTeamNumber Team)
{
	int Result = 0;

	for (int i = 1; i <= gpGlobals->maxClients; i++)
	{
		edict_t* PlayerEdict = INDEXENT(i);

		if (!FNullEnt(PlayerEdict) && !PlayerEdict->free && IsPlayerActiveInGame(PlayerEdict)) { Result++; }
	}

	return Result;
}

int AITAC_GetNumPlayersOfTeamAndClassInArea(const AvHTeamNumber Team, const Vector SearchLocation, const float SearchRadius, const bool bConsiderPhaseDist, const edict_t* IgnorePlayer, const AvHUser3 SearchClass)
{
	int Result = 0;
	float MaxRadiusSq = sqrf(SearchRadius);

	for (int i = 1; i <= gpGlobals->maxClients; i++)
	{
		edict_t* PlayerEdict = INDEXENT(i);

		if (FNullEnt(PlayerEdict) || PlayerEdict->free || PlayerEdict == IgnorePlayer) { continue; }

		AvHPlayer* PlayerRef = dynamic_cast<AvHPlayer*>(CBaseEntity::Instance(PlayerEdict));

		if (PlayerRef != nullptr && GetPlayerActiveClass(PlayerRef) == SearchClass && (Team == TEAM_IND || PlayerRef->GetTeam() == Team) && IsPlayerActiveInGame(PlayerEdict))
		{
			float Dist = (bConsiderPhaseDist) ? sqrf(AITAC_GetPhaseDistanceBetweenPoints(PlayerEdict->v.origin, SearchLocation)) : vDist2DSq(PlayerEdict->v.origin, SearchLocation);

			if (Dist <= MaxRadiusSq)
			{
				Result++;
			}
		}
	}

	return Result;
}

vector<AvHPlayer*> AITAC_GetAllPlayersOfTeamInArea(const AvHTeamNumber Team, const Vector SearchLocation, const float SearchRadius, const bool bConsiderPhaseDist, const edict_t* IgnorePlayer, const AvHUser3 IgnoreClass)
{
	vector<AvHPlayer*> Result;

	float MaxRadiusSq = sqrf(SearchRadius);

	for (int i = 1; i <= gpGlobals->maxClients; i++)
	{
		edict_t* PlayerEdict = INDEXENT(i);

		if (FNullEnt(PlayerEdict) || PlayerEdict->free || PlayerEdict == IgnorePlayer) { continue; }

		AvHPlayer* PlayerRef = dynamic_cast<AvHPlayer*>(CBaseEntity::Instance(PlayerEdict));

		if (PlayerRef != nullptr && GetPlayerActiveClass(PlayerRef) != IgnoreClass && (Team == TEAM_IND || PlayerRef->GetTeam() == Team) && IsPlayerActiveInGame(PlayerEdict))
		{
			float Dist = (bConsiderPhaseDist) ? sqrf(AITAC_GetPhaseDistanceBetweenPoints(PlayerEdict->v.origin, SearchLocation)) : vDist2DSq(PlayerEdict->v.origin, SearchLocation);

			if (Dist <= MaxRadiusSq)
			{
				Result.push_back(PlayerRef);
			}
		}
	}

	return Result;
}

int AITAC_GetNumPlayersOfTeamInArea(const AvHTeamNumber Team, const Vector SearchLocation, const float SearchRadius, const bool bConsiderPhaseDist, const edict_t* IgnorePlayer, const AvHUser3 IgnoreClass)
{
	int Result = 0;
	float MaxRadiusSq = sqrf(SearchRadius);

	for (int i = 1; i <= gpGlobals->maxClients; i++)
	{
		edict_t* PlayerEdict = INDEXENT(i);

		if (FNullEnt(PlayerEdict) || PlayerEdict->free || PlayerEdict == IgnorePlayer) { continue; }

		AvHPlayer* PlayerRef = dynamic_cast<AvHPlayer*>(CBaseEntity::Instance(PlayerEdict));

		if (PlayerRef != nullptr && GetPlayerActiveClass(PlayerRef) != IgnoreClass && (Team == TEAM_IND || PlayerRef->GetTeam() == Team) && IsPlayerActiveInGame(PlayerEdict))
		{
			float Dist = (bConsiderPhaseDist) ? sqrf(AITAC_GetPhaseDistanceBetweenPoints(PlayerEdict->v.origin, SearchLocation)) : vDist2DSq(PlayerEdict->v.origin, SearchLocation);

			if (Dist <= MaxRadiusSq)
			{
				Result++;
			}
		}
	}

	return Result;

}

bool AITAC_AnyPlayersOfTeamInArea(const AvHTeamNumber Team, const Vector SearchLocation, const float SearchRadius, const bool bConsiderPhaseDist, const edict_t* IgnorePlayer, const AvHUser3 IgnoreClass)
{
	float MaxRadiusSq = sqrf(SearchRadius);

	for (int i = 1; i <= gpGlobals->maxClients; i++)
	{
		edict_t* PlayerEdict = INDEXENT(i);

		if (FNullEnt(PlayerEdict) || PlayerEdict->free || PlayerEdict == IgnorePlayer) { continue; }

		AvHPlayer* PlayerRef = dynamic_cast<AvHPlayer*>(CBaseEntity::Instance(PlayerEdict));

		if (PlayerRef != nullptr && GetPlayerActiveClass(PlayerRef) != IgnoreClass && (Team == TEAM_IND || PlayerRef->GetTeam() == Team) && IsPlayerActiveInGame(PlayerEdict))
		{
			float Dist = (bConsiderPhaseDist) ? sqrf(AITAC_GetPhaseDistanceBetweenPoints(PlayerEdict->v.origin, SearchLocation)) : vDist2DSq(PlayerEdict->v.origin, SearchLocation);

			if (Dist <= MaxRadiusSq)
			{
				return true;
			}
		}
	}

	return false;
}

vector<AvHPlayer*> AITAC_GetAllPlayersOnTeamOfClass(const AvHTeamNumber Team, const AvHUser3 SearchClass, const edict_t* IgnorePlayer)
{
	vector<AvHPlayer*> Result;

	for (int i = 1; i <= gpGlobals->maxClients; i++)
	{
		edict_t* PlayerEdict = INDEXENT(i);

		if (FNullEnt(PlayerEdict) || PlayerEdict->free || PlayerEdict == IgnorePlayer) { continue; }

		AvHPlayer* PlayerRef = dynamic_cast<AvHPlayer*>(CBaseEntity::Instance(PlayerEdict));

		if (PlayerRef != nullptr && (SearchClass == AVH_USER3_NONE || GetPlayerActiveClass(PlayerRef) == SearchClass) && (Team == TEAM_IND || PlayerRef->GetTeam() == Team) && IsPlayerActiveInGame(PlayerEdict))
		{
			Result.push_back(PlayerRef);
		}

	}

	return Result;
}

int AITAC_GetNumPlayersOnTeamOfClass(const AvHTeamNumber Team, const AvHUser3 SearchClass, const edict_t* IgnorePlayer)
{
	int Result = 0;

	for (int i = 1; i <= gpGlobals->maxClients; i++)
	{
		edict_t* PlayerEdict = INDEXENT(i);

		if (FNullEnt(PlayerEdict) || PlayerEdict->free || PlayerEdict == IgnorePlayer) { continue; }

		AvHPlayer* PlayerRef = dynamic_cast<AvHPlayer*>(CBaseEntity::Instance(PlayerEdict));

		if (PlayerRef != nullptr && (SearchClass == AVH_USER3_NONE || GetPlayerActiveClass(PlayerRef) == SearchClass) && (Team == TEAM_IND || PlayerRef->GetTeam() == Team) && IsPlayerActiveInGame(PlayerEdict))
		{
			Result++;
		}

	}

	return Result;
}

edict_t* AITAC_GetNearestPlayerOfClassInArea(const AvHTeamNumber Team, const Vector SearchLocation, const float SearchRadius, const bool bConsiderPhaseDist, const edict_t* IgnorePlayer, const AvHUser3 SearchClass)
{
	edict_t* Result = nullptr;
	float MaxRadiusSq = sqrf(SearchRadius);
	float MinDistSq = 0.0f;

	for (int i = 1; i <= gpGlobals->maxClients; i++)
	{
		edict_t* PlayerEdict = INDEXENT(i);

		if (FNullEnt(PlayerEdict) || PlayerEdict->free || PlayerEdict == IgnorePlayer) { continue; }

		AvHPlayer* PlayerRef = dynamic_cast<AvHPlayer*>(CBaseEntity::Instance(PlayerEdict));

		if (PlayerRef != nullptr && (SearchClass == AVH_USER3_NONE || GetPlayerActiveClass(PlayerRef) == SearchClass) && (Team == TEAM_IND || PlayerRef->GetTeam() == Team) && IsPlayerActiveInGame(PlayerEdict))
		{
			float Dist = (bConsiderPhaseDist) ? sqrf(AITAC_GetPhaseDistanceBetweenPoints(PlayerEdict->v.origin, SearchLocation)) : vDist2DSq(PlayerEdict->v.origin, SearchLocation);

			if (Dist <= MaxRadiusSq && (FNullEnt(Result) || Dist < MinDistSq))
			{
				Result = PlayerEdict;
				MinDistSq = Dist;
			}
		}
	}

	return Result;

}

vector<edict_t*> AITAC_GetAllPlayersOfClassInArea(const AvHTeamNumber Team, const Vector SearchLocation, const float SearchRadius, const bool bConsiderPhaseDist, const edict_t* IgnorePlayer, const AvHUser3 SearchClass)
{
	vector<edict_t*> Result;
	float MaxRadiusSq = sqrf(SearchRadius);
	float MinDistSq = 0.0f;

	for (int i = 1; i <= gpGlobals->maxClients; i++)
	{
		edict_t* PlayerEdict = INDEXENT(i);

		if (FNullEnt(PlayerEdict) || PlayerEdict->free || PlayerEdict == IgnorePlayer) { continue; }

		AvHPlayer* PlayerRef = dynamic_cast<AvHPlayer*>(CBaseEntity::Instance(PlayerEdict));

		if (PlayerRef != nullptr && (SearchClass == AVH_USER3_NONE || GetPlayerActiveClass(PlayerRef) == SearchClass) && (Team == TEAM_IND || PlayerRef->GetTeam() == Team) && IsPlayerActiveInGame(PlayerEdict))
		{
			float Dist = (bConsiderPhaseDist) ? sqrf(AITAC_GetPhaseDistanceBetweenPoints(PlayerEdict->v.origin, SearchLocation)) : vDist2DSq(PlayerEdict->v.origin, SearchLocation);

			if (Dist <= MaxRadiusSq)
			{
				Result.push_back(PlayerEdict);
				MinDistSq = Dist;
			}
		}
	}

	return Result;
}

const AvHAIHive* AITAC_GetTeamHiveWithTech(const AvHTeamNumber Team, const EAIHiveTechStatus Tech)
{
	AvHTeam* TeamRef = GetGameRules()->GetTeam(Team);

	// If the team is invalid or marine team, return nothing since marines can't own hives.
	if (!TeamRef || TeamRef->GetTeamType() != AVH_CLASS_TYPE_ALIEN) { return nullptr; }

	for (auto it = Hives.begin(); it != Hives.end(); it++)
	{
		const AvHAIHive* HiveRef = &(*it);

		if (!HiveRef) { continue; }

		// Only return active hives with the tech
		if (HiveRef->OwningTeam == Team && HiveRef->Status == EAIHiveStatus::HIVE_STATUS_BUILT && HiveRef->TechStatus == Tech)
		{
			return HiveRef;
		}
	}

	return nullptr;
}

bool AITAC_TeamHiveWithTechExists(const AvHTeamNumber Team, const EAIHiveTechStatus Tech)
{
	return AITAC_GetTeamHiveWithTech(Team, Tech) != nullptr;
}

EAIDeployableItemType UTIL_GetItemTypeFromEdict(const edict_t* ItemEdict)
{
	const AvHAIDroppedItem* FoundItem = AITAC_GetDroppedItemRefFromEdict(ItemEdict);

	if (!FoundItem) { return EAIDeployableItemType::DEPLOYABLE_ITEM_NONE; }

	return FoundItem->ItemType;
}

EAIWeaponId UTIL_GetWeaponTypeFromDroppedItem(const EAIDeployableItemType ItemType)
{
	switch (ItemType)
	{
		case EAIDeployableItemType::DEPLOYABLE_ITEM_GRENADELAUNCHER:
			return EAIWeaponId::WEAPON_MARINE_GL;
		case EAIDeployableItemType::DEPLOYABLE_ITEM_HMG:
			return EAIWeaponId::WEAPON_MARINE_HMG;
		case EAIDeployableItemType::DEPLOYABLE_ITEM_SHOTGUN:
			return EAIWeaponId::WEAPON_MARINE_SHOTGUN;
		case EAIDeployableItemType::DEPLOYABLE_ITEM_WELDER:
			return EAIWeaponId::WEAPON_MARINE_WELDER;
		case EAIDeployableItemType::DEPLOYABLE_ITEM_MINES:
			return EAIWeaponId::WEAPON_MARINE_MINES;
		default:
			return EAIWeaponId::WEAPON_INVALID;
	}

	return EAIWeaponId::WEAPON_INVALID;
}

Vector UTIL_GetNextMinePosition(const AvHAIBuildableStructure* StructureToMine)
{
	if (!StructureToMine || !StructureToMine->IsValid()) { return ZERO_VECTOR; }

	AvHTeamNumber StructureTeam = (AvHTeamNumber)StructureToMine->Edict->v.team;

	NavAgentProfile MineCheckProfile = *GetBaseAgentProfile(EAINavProfileIndex::NAV_PROFILE_MARINE);
	MineCheckProfile.Filters.addExcludeFlags(static_cast<unsigned int>(EAINavMovementFlag::NAV_FLAG_BLOCKAGE_TEAM1));
	MineCheckProfile.Filters.addExcludeFlags(static_cast<unsigned int>(EAINavMovementFlag::NAV_FLAG_BLOCKAGE_TEAM2));
	MineCheckProfile.Filters.addExcludeFlags(static_cast<unsigned int>(EAINavMovementFlag::NAV_FLAG_JUMP));
	MineCheckProfile.Filters.addExcludeFlags(static_cast<unsigned int>(EAINavMovementFlag::NAV_FLAG_WELD));

	Vector FwdVector = UTIL_GetForwardVector2D(StructureToMine->Edict->v.angles);
	Vector RightVector = UTIL_GetVectorNormal2D(UTIL_GetCrossProduct(FwdVector, UP_VECTOR));

	bool bFwd = false;
	bool bRight = false;
	bool bBack = false;
	bool bLeft = false;

	StructureSearchFilter MineFilter;
	MineFilter.DeployableTeam = StructureTeam;
	MineFilter.DeployableTypes = EAIStructureType::STRUCTURE_MARINE_DEPLOYEDMINE;
	MineFilter.MaxSearchRadius = UTIL_MetresToGoldSrcUnits(3.0f);

	vector<const AvHAIBuildableStructure*> SurroundingMines = AITAC_FindAllMatchingStructures(StructureToMine->Location, &MineFilter);

	for (auto it = SurroundingMines.begin(); it != SurroundingMines.end(); it++)
	{
		const AvHAIBuildableStructure* ThisMine = (*it);

		Vector Dir = UTIL_GetVectorNormal2D(ThisMine->Location - StructureToMine->Location);

		const float ForwardDotProduct = UTIL_GetDotProduct2D(FwdVector, Dir);
		const float SideDotProduct = UTIL_GetDotProduct2D(RightVector, Dir);

		if (ForwardDotProduct > 0.7f)
		{
			bFwd = true;
		}

		if (ForwardDotProduct < -0.7f)
		{
			bBack = true;
		}

		if (SideDotProduct > 0.7f)
		{
			bRight = true;
		}

		if (SideDotProduct < -0.7f)
		{
			bLeft = true;
		}
	}

	float Size = fmaxf(StructureToMine->Edict->v.size.x, StructureToMine->Edict->v.size.y);
	Size += 8.0f;

	if (!bFwd)
	{
		Vector SearchLocation = StructureToMine->Location + (FwdVector * Size);

		Vector BuildLocation = AIMESH_ProjectPointToNavmesh(&MineCheckProfile, SearchLocation);

		if (!vIsZero(BuildLocation))
		{
			return BuildLocation;
		}
	}

	if (!bBack)
	{
		Vector SearchLocation = StructureToMine->Location - (FwdVector * Size);

		Vector BuildLocation = AIMESH_ProjectPointToNavmesh(&MineCheckProfile, SearchLocation);

		if (!vIsZero(BuildLocation))
		{
			return BuildLocation;
		}
	}

	if (!bRight)
	{
		Vector SearchLocation = StructureToMine->Location + (RightVector * Size);

		Vector BuildLocation = AIMESH_ProjectPointToNavmesh(&MineCheckProfile, SearchLocation);

		if (!vIsZero(BuildLocation))
		{
			return BuildLocation;
		}
	}

	if (!bLeft)
	{
		Vector SearchLocation = StructureToMine->Location - (RightVector * Size);

		Vector BuildLocation = AIMESH_ProjectPointToNavmesh(&MineCheckProfile, SearchLocation);

		if (!vIsZero(BuildLocation))
		{
			return BuildLocation;
		}
	}

	return AIMESH_GetRandomPointOnNavmeshInRadius(MineCheckProfile, StructureToMine->Location, Size, false);
}

int UTIL_GetCostOfStructureType(EAIStructureType StructureType)
{
	switch (StructureType)
	{
		case EAIStructureType::STRUCTURE_MARINE_ARMORY:
			return BALANCE_VAR(kArmoryCost);
		case EAIStructureType::STRUCTURE_MARINE_ARMSLAB:
			return BALANCE_VAR(kArmsLabCost);
		case EAIStructureType::STRUCTURE_MARINE_COMMCHAIR:
			return BALANCE_VAR(kCommandStationCost);
		case EAIStructureType::STRUCTURE_MARINE_INFANTRYPORTAL:
			return BALANCE_VAR(kInfantryPortalCost);
		case EAIStructureType::STRUCTURE_MARINE_OBSERVATORY:
			return BALANCE_VAR(kObservatoryCost);
		case EAIStructureType::STRUCTURE_MARINE_PHASEGATE:
			return BALANCE_VAR(kPhaseGateCost);
		case EAIStructureType::STRUCTURE_MARINE_PROTOTYPELAB:
			return BALANCE_VAR(kPrototypeLabCost);
		case EAIStructureType::STRUCTURE_MARINE_RESTOWER:
		case EAIStructureType::STRUCTURE_ALIEN_RESTOWER:
			return BALANCE_VAR(kResourceTowerCost);
		case EAIStructureType::STRUCTURE_MARINE_SIEGETURRET:
			return BALANCE_VAR(kSiegeCost);
		case EAIStructureType::STRUCTURE_MARINE_TURRET:
			return BALANCE_VAR(kSentryCost);
		case EAIStructureType::STRUCTURE_MARINE_TURRETFACTORY:
			return BALANCE_VAR(kTurretFactoryCost);
		case EAIStructureType::STRUCTURE_ALIEN_HIVE:
			return BALANCE_VAR(kHiveCost);
		case EAIStructureType::STRUCTURE_ALIEN_OFFENSECHAMBER:
			return BALANCE_VAR(kOffenseChamberCost);
		case EAIStructureType::STRUCTURE_ALIEN_DEFENSECHAMBER:
			return BALANCE_VAR(kDefenseChamberCost);
		case EAIStructureType::STRUCTURE_ALIEN_MOVEMENTCHAMBER:
			return BALANCE_VAR(kMovementChamberCost);
		case EAIStructureType::STRUCTURE_ALIEN_SENSORYCHAMBER:
			return BALANCE_VAR(kSensoryChamberCost);
		default:
			return 0;
	}

	return 0;
}

int AITAC_GetNumHives()
{
	return Hives.size();
}

int AITAC_GetNumTeamHives(AvHTeamNumber Team, bool bFullyCompletedOnly)
{
	int Result = 0;

	for (auto it = Hives.begin(); it != Hives.end(); it++)
	{
		if (it->OwningTeam == Team && (!bFullyCompletedOnly || it->Status == EAIHiveStatus::HIVE_STATUS_BUILT))
		{
			Result++;
		}
	}

	return Result;
}

AvHMessageID UTIL_StructureTypeToImpulseCommand(const EAIStructureType StructureType)
{
	switch (StructureType)
	{
		case EAIStructureType::STRUCTURE_MARINE_ARMORY:
			return BUILD_ARMORY;
		case EAIStructureType::STRUCTURE_MARINE_ARMSLAB:
			return BUILD_ARMSLAB;
		case EAIStructureType::STRUCTURE_MARINE_COMMCHAIR:
			return BUILD_COMMANDSTATION;
		case EAIStructureType::STRUCTURE_MARINE_INFANTRYPORTAL:
			return BUILD_INFANTRYPORTAL;
		case EAIStructureType::STRUCTURE_MARINE_OBSERVATORY:
			return BUILD_OBSERVATORY;
		case EAIStructureType::STRUCTURE_MARINE_PHASEGATE:
			return BUILD_PHASEGATE;
		case EAIStructureType::STRUCTURE_MARINE_PROTOTYPELAB:
			return BUILD_PROTOTYPE_LAB;
		case EAIStructureType::STRUCTURE_MARINE_RESTOWER:
			return BUILD_RESOURCES;
		case EAIStructureType::STRUCTURE_MARINE_SIEGETURRET:
			return BUILD_SIEGE;
		case EAIStructureType::STRUCTURE_MARINE_TURRET:
			return BUILD_TURRET;
		case EAIStructureType::STRUCTURE_MARINE_TURRETFACTORY:
			return BUILD_TURRET_FACTORY;

		case EAIStructureType::STRUCTURE_ALIEN_DEFENSECHAMBER:
			return ALIEN_BUILD_DEFENSE_CHAMBER;
		case EAIStructureType::STRUCTURE_ALIEN_MOVEMENTCHAMBER:
			return ALIEN_BUILD_MOVEMENT_CHAMBER;
		case EAIStructureType::STRUCTURE_ALIEN_SENSORYCHAMBER:
			return ALIEN_BUILD_SENSORY_CHAMBER;
		case EAIStructureType::STRUCTURE_ALIEN_OFFENSECHAMBER:
			return ALIEN_BUILD_OFFENSE_CHAMBER;
		case EAIStructureType::STRUCTURE_ALIEN_RESTOWER:
			return ALIEN_BUILD_RESOURCES;
		case EAIStructureType::STRUCTURE_ALIEN_HIVE:
			return ALIEN_BUILD_HIVE;
		default:
			return MESSAGE_NULL;
	}

	return MESSAGE_NULL;
}

AvHMessageID UTIL_ItemTypeToImpulseCommand(const EAIDeployableItemType ItemType)
{
	switch (ItemType)
	{
		case EAIDeployableItemType::DEPLOYABLE_ITEM_HEAVYARMOUR:
			return BUILD_HEAVY;
		case EAIDeployableItemType::DEPLOYABLE_ITEM_JETPACK:
			return BUILD_JETPACK;
		case EAIDeployableItemType::DEPLOYABLE_ITEM_CATALYSTS:
			return BUILD_CAT;
		case EAIDeployableItemType::DEPLOYABLE_ITEM_SCAN:
			return BUILD_SCAN;
		case EAIDeployableItemType::DEPLOYABLE_ITEM_HEALTHPACK:
			return BUILD_HEALTH;
		case EAIDeployableItemType::DEPLOYABLE_ITEM_AMMO:
			return BUILD_AMMO;
		case EAIDeployableItemType::DEPLOYABLE_ITEM_MINES:
			return BUILD_MINES;
		case EAIDeployableItemType::DEPLOYABLE_ITEM_WELDER:
			return BUILD_WELDER;
		case EAIDeployableItemType::DEPLOYABLE_ITEM_SHOTGUN:
			return BUILD_SHOTGUN;
		case EAIDeployableItemType::DEPLOYABLE_ITEM_HMG:
			return BUILD_HMG;
		case EAIDeployableItemType::DEPLOYABLE_ITEM_GRENADELAUNCHER:
			return BUILD_GRENADE_GUN;
		default:
			return MESSAGE_NULL;

	}

	return MESSAGE_NULL;
}

const AvHAIBuildableStructure* AITAC_GetCommChair(AvHTeamNumber Team)
{
	const AvHTeam* ChairTeam = GetGameRules()->GetTeam(Team);

	// Invalid team, or team is alien and there can't have a comm chair
	if (!ChairTeam || ChairTeam->GetTeamType() != AVH_CLASS_TYPE_MARINE) { return nullptr; }

	StructureSearchFilter ChairFilter;
	ChairFilter.DeployableTypes = EAIStructureType::STRUCTURE_MARINE_COMMCHAIR;
	ChairFilter.IncludeStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_COMPLETED;
	ChairFilter.DeployableTeam = Team;

	vector<const AvHAIBuildableStructure*> CommChairs = AITAC_FindAllMatchingStructures(ZERO_VECTOR, &ChairFilter);

	const AvHAIBuildableStructure* BackupChair = nullptr;

	// If the team has more than one comm chair, pick the one in use
	for (auto it = CommChairs.begin(); it != CommChairs.end(); it++)
	{
		const AvHAIBuildableStructure* ChairStructure = *it;

		if (!ChairStructure || !ChairStructure->IsValid()) { continue; }

		const AvHCommandStation* ChairRef = dynamic_cast<AvHCommandStation*>(ChairStructure->EntityRef);

		if (!ChairRef) { continue; }

		// Idle animation will be 3 if the chair is in use (closed animation). See AvHCommandStation::GetIdleAnimation
		if (ChairRef->GetIdleAnimation() == 3)
		{
			return ChairStructure;
		}
		else
		{
			if (!BackupChair || !BackupChair->IsValid() || !EnumHasAnyFlags(BackupChair->StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_COMPLETED))
			{
				BackupChair = ChairStructure;
			}
		}
	}

	return BackupChair;
}

EAIStructureType UTIL_GetChamberTypeForHiveTech(EAIHiveTechStatus HiveTech)
{
	switch (HiveTech)
	{
		case EAIHiveTechStatus::HIVE_TECH_DEFENSE:
			return EAIStructureType::STRUCTURE_ALIEN_DEFENSECHAMBER;
		case EAIHiveTechStatus::HIVE_TECH_MOVEMENT:
			return EAIStructureType::STRUCTURE_ALIEN_MOVEMENTCHAMBER;
		case EAIHiveTechStatus::HIVE_TECH_SENSORY:
			return EAIStructureType::STRUCTURE_ALIEN_SENSORYCHAMBER;
		default:
			return EAIStructureType::STRUCTURE_NONE;
	}

	return EAIStructureType::STRUCTURE_NONE;
}

bool AITAC_ResearchIsComplete(const AvHTeamNumber Team, const AvHTechID Research)
{
	AvHTeam* TeamRef = GetGameRules()->GetTeam(Team);

	if (!TeamRef) { return false; }

	AvHResearchManager ResearchManager = TeamRef->GetResearchManager();

	return ResearchManager.GetTechNodes().GetIsTechResearched(Research);
}

bool AITAC_PhaseGatesAvailable(const AvHTeamNumber Team)
{
	StructureSearchFilter ObsFilter;
	ObsFilter.IncludeStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_COMPLETED;
	ObsFilter.ExcludeStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_RECYCLING;
	ObsFilter.DeployableTypes = EAIStructureType::STRUCTURE_MARINE_OBSERVATORY;
	ObsFilter.DeployableTeam = Team;

	AIBuildableStructureList FoundObs = AITAC_FindAllMatchingStructures(AITAC_GetTeamStartingLocation(Team), &ObsFilter);

	bool bObsExists = !FoundObs.empty();

	return bObsExists && AITAC_ResearchIsComplete(Team, TECH_RESEARCH_PHASETECH);
}

int AITAC_GetNumDeadPlayersOnTeam(const AvHTeamNumber Team)
{
	AvHTeam* TeamRef = GetGameRules()->GetTeam(Team);

	if (!TeamRef) { return 0; }

	return TeamRef->GetPlayerCount(true);
}

const vector<AvHAIResourceNode*> AITAC_GetAllResourceNodes()
{
	vector<AvHAIResourceNode*> Results;

	for (auto it = ResourceNodes.begin(); it != ResourceNodes.end(); it++)
	{
		Results.push_back(&(*it));
	}

	return Results;
}

const vector<AvHAIHive*> AITAC_GetAllHives()
{
	vector<AvHAIHive*> Results;

	for (auto it = Hives.begin(); it != Hives.end(); it++)
	{
		Results.push_back(&(*it));
	}

	return Results;
}

const vector<AvHAIHive*> AITAC_GetAllTeamHives(AvHTeamNumber Team, bool bFullyBuiltOnly)
{
	vector<AvHAIHive*> Results;

	for (auto it = Hives.begin(); it != Hives.end(); it++)
	{
		if (it->OwningTeam == Team && (!bFullyBuiltOnly || it->Status == EAIHiveStatus::HIVE_STATUS_BUILT))
		{
			Results.push_back(&(*it));
		}
	}

	return Results;
}

bool AITAC_AnyPlayerOnTeamWithLOS(AvHTeamNumber Team, const Vector& Location, float SearchRadius)
{
	float distSq = sqrf(SearchRadius);

	vector<AvHPlayer*> Players = AIMGR_GetAllPlayersOnTeam(Team);

	for (auto it = Players.begin(); it != Players.end(); it++)
	{
		edict_t* PlayerRef = (*it)->edict();

		if (!IsPlayerActiveInGame(PlayerRef)) { continue; }

		if (vDist2DSq(PlayerRef->v.origin, Location) <= distSq && UTIL_QuickTrace(PlayerRef, GetPlayerEyePosition(PlayerRef), Location))
		{
			return true;
		}
	}

	return false;
}

EAIStructureType AITAC_GetNextMissingUpgradeChamberForTeam(AvHTeamNumber Team, int& NumMissing)
{
	if (AIMGR_GetTeamType(Team) != AVH_CLASS_TYPE_ALIEN) { return EAIStructureType::STRUCTURE_NONE; }

	EAIHiveTechStatus HiveTechOne = CONFIG_GetHiveTechAtIndex(0);
	EAIHiveTechStatus HiveTechTwo = CONFIG_GetHiveTechAtIndex(1);
	EAIHiveTechStatus HiveTechThree = CONFIG_GetHiveTechAtIndex(2);

	EAIStructureType ChamberTypeOne = UTIL_GetChamberTypeForHiveTech(HiveTechOne);
	EAIStructureType ChamberTypeTwo = UTIL_GetChamberTypeForHiveTech(HiveTechTwo);
	EAIStructureType ChamberTypeThree = UTIL_GetChamberTypeForHiveTech(HiveTechThree);

	StructureSearchFilter SearchFilter;
	SearchFilter.DeployableTeam = Team;

	bool bHasFreeHive = AITAC_TeamHiveWithTechExists(Team, EAIHiveTechStatus::HIVE_TECH_NONE);

	if (ChamberTypeOne != EAIStructureType::STRUCTURE_NONE && (bHasFreeHive || AITAC_TeamHiveWithTechExists(Team, HiveTechOne)))
	{
		SearchFilter.DeployableTypes = ChamberTypeOne;

		int NumChambers = AITAC_GetNumStructuresAtLocation(AITAC_GetTeamStartingLocation(Team), &SearchFilter);

		if (NumChambers < 3)
		{
			NumMissing = 3 - NumChambers;
			return ChamberTypeOne;
		}
	}

	if (ChamberTypeTwo != EAIStructureType::STRUCTURE_NONE && (bHasFreeHive || AITAC_TeamHiveWithTechExists(Team, HiveTechTwo)))
	{
		SearchFilter.DeployableTypes = ChamberTypeTwo;

		int NumChambers = AITAC_GetNumStructuresAtLocation(AITAC_GetTeamStartingLocation(Team), &SearchFilter);

		if (NumChambers < 3)
		{
			NumMissing = 3 - NumChambers;
			return ChamberTypeTwo;
		}
	}

	if (ChamberTypeThree != EAIStructureType::STRUCTURE_NONE && (bHasFreeHive || AITAC_TeamHiveWithTechExists(Team, HiveTechThree)))
	{
		SearchFilter.DeployableTypes = ChamberTypeThree;

		int NumChambers = AITAC_GetNumStructuresAtLocation(AITAC_GetTeamStartingLocation(Team), &SearchFilter);

		if (NumChambers < 3)
		{
			NumMissing = 3 - NumChambers;
			return ChamberTypeThree;
		}
	}

	return EAIStructureType::STRUCTURE_NONE;
}

bool AITAC_IsAlienUpgradeAvailableForTeam(AvHTeamNumber Team, EAIHiveTechStatus DesiredTech)
{
	EAIStructureType SearchType;

	switch (DesiredTech)
	{
		case EAIHiveTechStatus::HIVE_TECH_DEFENSE:
			SearchType = EAIStructureType::STRUCTURE_ALIEN_DEFENSECHAMBER;
			break;
		case EAIHiveTechStatus::HIVE_TECH_MOVEMENT:
			SearchType = EAIStructureType::STRUCTURE_ALIEN_MOVEMENTCHAMBER;
			break;
		case EAIHiveTechStatus::HIVE_TECH_SENSORY:
			SearchType = EAIStructureType::STRUCTURE_ALIEN_SENSORYCHAMBER;
			break;
		default:
			return false;
	}

	StructureSearchFilter ChamberFilter;
	ChamberFilter.DeployableTeam = Team;
	ChamberFilter.IncludeStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_COMPLETED;
	ChamberFilter.DeployableTypes = SearchType;

	AIBuildableStructureList AllCompletedChambers = AITAC_FindAllMatchingStructures(ZERO_VECTOR, &ChamberFilter);

	return !AllCompletedChambers.empty();
}

edict_t* AITAC_GetLastSeenLerkForTeam(AvHTeamNumber Team, float& LastSeenTime)
{
	if (Team == GetGameRules()->GetTeamANumber())
	{
		LastSeenTime = LastSeenLerkTeamATime;
		return LastSeenLerkTeamA;
	}
	else
	{
		LastSeenTime = LastSeenLerkTeamBTime;
		return LastSeenLerkTeamB;
	}
}

void AvHAITeamStartingLocation::RefreshReachabilityMap()
{
	if (Team == TEAM_IND) { return; }

	for (auto ReachabilityMap : StructureReachabilityMap)
	{
		const AvHAIBuildableStructure* Structure = ReachabilityMap.first;

		if (!Structure || !Structure->IsValid() || !Structure->bReachabilityMarkedDirty)
		{
			StructureReachabilityMap.erase(Structure);
			continue;
		}

		if (TeamType == AVH_CLASS_TYPE_MARINE)
		{
			AITAC_CalculateMarineReachabilityFlags(StartingPoint, Structure->Location, ReachabilityMap.second);
			continue;
		}

		if (TeamType == AVH_CLASS_TYPE_ALIEN)
		{
			AITAC_CalculateAlienReachabilityFlags(StartingPoint, Structure->Location, ReachabilityMap.second);
			continue;
		}
	}
}

void AvHAIBuildableStructure::ClearNavInformation()
{
	for (auto ObsIt = TempObstacles.begin(); ObsIt != TempObstacles.end(); ObsIt++)
	{
		NavTempObstacle* ThisObstacle = (*ObsIt);

		if (!ThisObstacle || !ThisObstacle->IsValid()) { continue; }

		AIMESH_RemoveTemporaryObstacle(ThisObstacle);
	}

	for (auto ConnIt = OffMeshConnections.begin(); ConnIt != OffMeshConnections.end(); ConnIt++)
	{
		NavOffMeshConnection* ThisConnection = (*ConnIt);

		if (!ThisConnection || !ThisConnection->IsValid()) { continue; }

		AIMESH_RemoveOffMeshConnection(ThisConnection);
	}

	TempObstacles.clear();
	OffMeshConnections.clear();
}

void AvHAIHive::Update()
{
	if (!IsValid()) { return; }

	TechStatus = UTIL_GetHiveTechStatusFromMessageID(HiveEntity->GetTechnology());
	bIsUnderAttack = GetGameRules()->GetIsEntityUnderAttack(ENTINDEX(Edict));

	OwningTeam = HiveEntity->GetTeamNumber();

	const EAIHiveStatus PreviousStatus = Status;

	Status = (HiveEntity->GetIsActive())
		? EAIHiveStatus::HIVE_STATUS_BUILT
		: (HiveEntity->GetIsSpawning()) ? EAIHiveStatus::HIVE_STATUS_BUILDING : EAIHiveStatus::HIVE_STATUS_UNBUILT;

	HealthPercent = (IsBuilt())
		? (Edict->v.health / Edict->v.max_health)
		: 1.0f;

	if (PreviousStatus != Status)
	{
		OnBuiltStatusChanged(PreviousStatus, Status);
	}

}

void AvHAIHive::OnBuiltStatusChanged(const EAIHiveStatus OldStatus, const EAIHiveStatus NewStatus)
{
	// Gone from the "ghost" to a solid hive. Add temporary obstacles.
	if (OldStatus == EAIHiveStatus::HIVE_STATUS_UNBUILT)
	{
		NavMeshList MeshList = AIMESH_GetAllNavMeshes();

		if (MeshList.size() == 0) { return; }

		const float HiveWidth = fmaxf(Edict->v.size.x, Edict->v.size.y);
		const float HiveHeight = Edict->v.size.z;

		const EAINavArea NewObstacleType = (OwningTeam == GetGameRules()->GetTeamANumber())
			? EAINavArea::NAV_AREA_BLOCKAGE_TEAM1
			: EAINavArea::NAV_AREA_BLOCKAGE_TEAM2;

		for (NavMesh* Mesh : MeshList)
		{
			NavTempObstacle* NewObstacle = AIMESH_AddTemporaryObstacle(Mesh->MeshIndex, UTIL_GetCentreOfEntity(Edict), HiveWidth, HiveHeight, NewObstacleType);

			if (NewObstacle)
			{
				TempObstacles.push_back(NewObstacle);
			}
		}

		// TODO: Add off-mesh connections here so aliens can eventually use hives to teleport around

		return;
	}

	// Hive was destroyed
	if (NewStatus == EAIHiveStatus::HIVE_STATUS_UNBUILT)
	{
		ClearNavInformation();
	}
}

void AvHAIHive::ClearNavInformation()
{
	for (auto ObsIt = TempObstacles.begin(); ObsIt != TempObstacles.end(); ObsIt++)
	{
		NavTempObstacle* ThisObstacle = (*ObsIt);

		if (!ThisObstacle || !ThisObstacle->IsValid()) { continue; }

		AIMESH_RemoveTemporaryObstacle(ThisObstacle);
	}

	for (auto ConnIt = OffMeshConnections.begin(); ConnIt != OffMeshConnections.end(); ConnIt++)
	{
		NavOffMeshConnection* ThisConnection = (*ConnIt);

		if (!ThisConnection || !ThisConnection->IsValid()) { continue; }

		AIMESH_RemoveOffMeshConnection(ThisConnection);
	}

	TempObstacles.clear();
	OffMeshConnections.clear();
}
