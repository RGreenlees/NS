//
// EvoBot - Neoptolemus' Natural Selection bot, based on Botman's HPB bot template
//
// AvHAITactical.cpp
//
// Tracks information about the game state, including structures and research
//

#pragma once

#ifndef AVH_AI_TACTICAL_H
#define AVH_AI_TACTICAL_H

#include <unordered_map>

#include "AvHAIPlayer.h"
#include "AvHAIConstants.h"
#include "AvHAINavMesh.h"

// How frequently to update the global list of built structures (in seconds). 0 = every frame
static const float structure_inventory_refresh_rate = 0.2f;

// How frequently to update the global list of dropped marine items (in seconds). 0 = every frame
static const float item_inventory_refresh_rate = 0.2f;

// Data structure to hold information on any kind of buildable structure (hive, resource tower, chamber, marine building etc)
struct AvHAIBuildableStructure
{
	CBaseEntity* EntityRef = nullptr;
	edict_t* Edict = nullptr; // Reference to structure edict
	EAIStructureType StructureType = EAIStructureType::STRUCTURE_NONE; // Type of structure it is (e.g. hive, comm chair, infantry portal, defence chamber etc.)
	Vector Location = g_vecZero; // origin of the structure edict
	float HealthPercent = 1.0f; // Current health of the building
	EAIStructureStatus StructureStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_NONE;
	EAIReachabilityFlags TeamAReachabilityFlags = EAIReachabilityFlags::AI_REACHABILITY_NONE;
	EAIReachabilityFlags TeamBReachabilityFlags = EAIReachabilityFlags::AI_REACHABILITY_NONE;
	int LastSeen = 0; // Which refresh cycle was this last seen on? Used to determine if the building has been removed from play
	std::vector<NavTempObstacle*> TempObstacles;
	vector<NavOffMeshConnection*> OffMeshConnections; // References to any off-mesh connections this structure is associated with
	bool bReachabilityMarkedDirty = true; // If true, reachability flags will be recalculated for this structure
	AvHTeamNumber Team = TEAM_IND;

	bool IsValid() const { return EntityRef != nullptr && UTIL_IsEdictActive(Edict); }

	bool IsGhost() const { return IsValid() && EnumHasAnyFlags(StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_GHOST); }

	bool IsPartiallyBuilt() const { return IsValid() && EnumHasAnyFlags(StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_PARTIAL); }

	bool IsParasited() const { return IsValid() && EnumHasAnyFlags(StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_PARASITED); }

	bool IsCompleted() const { return IsValid() && EnumHasAnyFlags(StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_COMPLETED); }

	bool IsRecycling() const { return IsValid() && EnumHasAnyFlags(StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_RECYCLING); }

	bool IsResearching() const { return IsValid() && EnumHasAnyFlags(StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_RESEARCHING); }

	bool IsUpgrading() const { return (IsResearching() && (Edict->v.iuser2 == ARMORY_UPGRADE || Edict->v.iuser2 == TURRET_FACTORY_UPGRADE)); }

	bool IsUnderAttack() const { return IsValid() && !IsRecycling() && EnumHasAnyFlags(StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_UNDERATTACK); }

	bool IsElectrified() const { return IsValid() && !IsRecycling() && EnumHasAnyFlags(StructureStatusFlags, EAIStructureStatus::STRUCTURE_STATUS_ELECTRIFIED); }

	bool IsDamagingStructure() const { return IsValid() && !IsRecycling() && EnumHasAnyFlags(StructureType, (EAIStructureType::STRUCTURE_MARINE_TURRET | EAIStructureType::STRUCTURE_ALIEN_OFFENSECHAMBER)); }

	bool IsIdle() const { return IsValid() && !IsResearching() && !IsRecycling(); }

	bool CanBeUpgraded() const
	{
		return IsCompleted()
			&& IsIdle()
			&& EnumHasAnyFlags(StructureType, (EAIStructureType::STRUCTURE_MARINE_TURRETFACTORY | EAIStructureType::STRUCTURE_MARINE_ARMORY));
	}

	void ClearNavInformation();
};
typedef unordered_map<const AvHAIBuildableStructure*, EAIReachabilityFlags> AIStructureReachabilityMap;
typedef unordered_map<int, AvHAIBuildableStructure> AIBuildableStructureMap;
typedef vector<const AvHAIBuildableStructure*> AIBuildableStructureList;

// Data structure used to track resource nodes in the map
struct AvHAIResourceNode
{
	AvHFuncResource* ResourceNodeEntity = nullptr;						// The func_resource edict reference
	edict_t* Edict = nullptr;
	Vector Location = g_vecZero;									// origin of the func_resource edict (not the tower itself)
	AvHTeamNumber OwningTeam = TEAM_IND;							// The team that has currently capped this node (TEAM_IND if none)
	bool bIsOccupied = false;
	const AvHAIBuildableStructure* ActiveTowerEntity = nullptr;							// Reference to the resource tower edict (if capped)
	bool bIsBaseNode = false;										// Is this a node in the marine base or active alien hive?
	edict_t* ParentHive = nullptr;
	bool bReachabilityMarkedDirty = false;							// Reachability needs to be recalculated

	bool IsValid() const { return ResourceNodeEntity != nullptr && UTIL_IsEdictActive(Edict) && !(Edict->v.flags & EF_NODRAW); }
};
typedef unordered_map<const AvHAIResourceNode*, EAIReachabilityFlags> AIResourceReachabilityMap;

// Data structure to hold information about each hive in the map
struct AvHAIHive
{
	AvHHive* HiveEntity = nullptr;					// Hive entity reference
	edict_t* Edict = nullptr;					// Hive edict reference
	Vector Location = g_vecZero;					// Origin of the hive
	unordered_map<EAINavProfileIndex, Vector> FloorLocations; // Closest point each agent type can get to the hive
	EAIHiveStatus Status = EAIHiveStatus::HIVE_STATUS_UNBUILT;	// Can be unbuilt, in progress, or fully built
	EAIHiveTechStatus TechStatus = EAIHiveTechStatus::HIVE_TECH_NONE;			// What tech (if any) is assigned to this hive right now
	bool bIsUnderAttack = false;					// Is the hive currently under attack? Becomes false if not taken damage for more than 10 seconds
	float HealthPercent = 0.0f;						// If the hive is built and active, what its health currently is
	AvHAIResourceNode* HiveResNodeRef = nullptr;	// Which resource node (indexes into ResourceNodes array) belongs to this hive?
	std::vector<NavTempObstacle*> TempObstacles;		// When in progress or built, will place an obstacle so bots don't try to walk through it
	vector<NavOffMeshConnection*> OffMeshConnections; // References to any off-mesh connections this hive is associated with
	float NextFloorLocationCheck = 0.0f;			// When should the closest navigable point to the hive be calculated? Used to delay the check after a hive is built
	AvHTeamNumber OwningTeam = TEAM_IND;			// Which team owns this hive currently (TEAM_IND if empty)
	EAIReachabilityFlags TeamAReachabilityFlags = EAIReachabilityFlags::AI_REACHABILITY_NONE;		// Who on team A can reach this node?
	EAIReachabilityFlags TeamBReachabilityFlags = EAIReachabilityFlags::AI_REACHABILITY_NONE;		// Who on team B can reach this node?
	char HiveName[64] = { '\0' };

	bool IsValid() const { return HiveEntity != nullptr && !FNullEnt(Edict) && !Edict->free; }
	void Update();
	void ClearNavInformation();
	bool IsBuilt() const { return Status == EAIHiveStatus::HIVE_STATUS_BUILT; }
	void OnBuiltStatusChanged(const EAIHiveStatus OldStatus, const EAIHiveStatus NewStatus);
};
typedef unordered_map<const AvHAIHive*, EAIReachabilityFlags> AIHiveReachabilityMap;

// Any kind of pickup that has been dropped either by the commander or by a player
struct AvHAIDroppedItem
{
	edict_t* Edict = nullptr; // Reference to the item edict
	Vector Location = g_vecZero; // Origin of the entity
	EAIDeployableItemType ItemType = EAIDeployableItemType::DEPLOYABLE_ITEM_NONE; // Is it a weapon, health pack, ammo pack etc?
	EAIReachabilityFlags TeamAReachabilityFlags = EAIReachabilityFlags::AI_REACHABILITY_NONE;
	EAIReachabilityFlags TeamBReachabilityFlags = EAIReachabilityFlags::AI_REACHABILITY_NONE;
	bool bReachabilityMarkedDirty = false; // Reachability needs to be recalculated
	int LastSeen = 0; // Which refresh cycle was this last seen on? Used to determine if the item has been removed from play

	bool IsValid() const { return UTIL_IsEdictActive(Edict) && !(Edict->v.flags & EF_NODRAW); }

	bool IsPrimaryWeapon() const
	{
		switch (ItemType)
		{
			case EAIDeployableItemType::DEPLOYABLE_ITEM_LMG:
			case EAIDeployableItemType::DEPLOYABLE_ITEM_GRENADELAUNCHER:
			case EAIDeployableItemType::DEPLOYABLE_ITEM_HMG:
			case EAIDeployableItemType::DEPLOYABLE_ITEM_SHOTGUN:
				return true;
			default:
				return false;
		}
	}
};
typedef unordered_map<const AvHAIDroppedItem*, EAIReachabilityFlags> AIDroppedItemReachabilityMap;
typedef unordered_map<int, AvHAIDroppedItem> AIDroppedItemMap;

// Defines a player's starting location on a given team. Can be multiple locations
struct AvHAITeamStartingLocation
{
	AvHTeamNumber Team;
	AvHClassType TeamType;
	Vector StartingPoint;
	AIStructureReachabilityMap StructureReachabilityMap;
	AIHiveReachabilityMap HiveReachabilityMap;
	AIResourceReachabilityMap ResourceReachabilityMap;
	AIDroppedItemReachabilityMap DroppedItemReachabilityMap;

	void RefreshReachabilityMap();
};

struct StructureSearchFilter
{
	EAIStructureType DeployableTypes = EAIStructureType::ALL_STRUCTURES;
	EAIStructureStatus IncludeStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_NONE;
	EAIStructureStatus ExcludeStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_NONE;
	float MinSearchRadius = 0.0f;
	float MaxSearchRadius = 0.0f;
	bool bConsiderPhaseDistance = false;
	AvHTeamNumber DeployableTeam = TEAM_IND;
	EAIReachabilityFlags ReachabilityFlags = EAIReachabilityFlags::AI_REACHABILITY_NONE;
	const AvHAITeamStartingLocation* ReachabilityCheckLocation = nullptr;
};

struct DroppedItemSearchFilter
{
	EAIDeployableItemType DeployableTypes = EAIDeployableItemType::DEPLOYABLE_ITEM_ALL;
	EAIStructureStatus IncludeStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_NONE;
	EAIStructureStatus ExcludeStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_NONE;
	float MinSearchRadius = 0.0f;
	float MaxSearchRadius = 0.0f;
	bool bConsiderPhaseDistance = false;
	AvHTeamNumber DeployableTeam = TEAM_IND;
	EAIReachabilityFlags ReachabilityFlags = EAIReachabilityFlags::AI_REACHABILITY_NONE;
	const AvHAITeamStartingLocation* ReachabilityCheckLocation = nullptr;
};

enum class EAIStructureSortType
{
	FIND_STRUCTURE_RANDOM = 0,
	FIND_STRUCTURE_NEAREST,
	FIND_STRUCTURE_FURTHEST,
	FIND_STRUCTURE_WEAKEST,
	FIND_STRUCTURE_STRONGEST
};

struct ResourceNodeSearchFilter
{
	int32 OwningTeam = -1;
	AvHTeamNumber ReachabilityTeam = TEAM_IND;
	float MinSearchRadius = 0.0f;
	float MaxSearchRadius = 0.0f;
	bool bConsiderPhaseDistance = false;
	EAIReachabilityFlags ReachabilityFlags = EAIReachabilityFlags::AI_REACHABILITY_NONE;
	const AvHAITeamStartingLocation* ReachabilityCheckLocation = nullptr;
};


bool						AITAC_DoesStructureMatchFilter(const AvHAIBuildableStructure* Structure, const StructureSearchFilter* Filter, const Vector& SearchLocation = ZERO_VECTOR);
bool						AITAC_DoesDroppedItemMatchFilter(const AvHAIDroppedItem* Item, const DroppedItemSearchFilter* Filter, const Vector& SearchLocation = ZERO_VECTOR);
bool						AITAC_DoesResourceNodeMatchFilter(const AvHAIResourceNode* ResourceNode, const ResourceNodeSearchFilter* Filter, const Vector& SearchLocation = ZERO_VECTOR);
AIBuildableStructureList	 AITAC_FindAllMatchingStructures(const Vector& Location, const StructureSearchFilter* Filter);
const AvHAIBuildableStructure*		AITAC_FindSingleMatchingStructure(const Vector& Location, const StructureSearchFilter* Filter, EAIStructureSortType SortBy);
const AvHAIBuildableStructure*		AITAC_GetStructureFromEdict(const edict_t* Structure);
int							AITAC_GetNumStructuresAtLocation(const Vector& Location, const StructureSearchFilter* Filter);
void						AITAC_PopulateHiveData();
void						AITAC_RefreshHiveData();
void						AITAC_PopulateResourceNodes();
void						AITAC_RefreshResourceNodes();
void						AITAC_UpdateMapAIData();
void						AITAC_CheckNavMeshModified();
void						AITAC_RefreshBuildableStructures();
void						AITAC_AddStructureTemporaryObstacles(AvHAIBuildableStructure* Structure);
void						AITAC_ClearStructureTemporaryObstacles(AvHAIBuildableStructure* Structure);
void						AITAC_UpdateBuildableStructure(CBaseEntity* Structure);
AvHAIBuildableStructure*	AITAC_RegisterNewBuildableStructure(CBaseEntity* NewStructure);
void						AITAC_UpdateBuildableStructureStatusFlags(AvHAIBuildableStructure* Structure);
float						AITAC_GetPhaseDistanceBetweenPoints(const Vector StartPoint, const Vector EndPoint);

void						AITAC_RefreshReachabilityForStructure(AvHAIBuildableStructure* Structure);
void						AITAC_CalculateMarineReachabilityFlags(const Vector& FromLocation, const Vector& ToLocation, EAIReachabilityFlags& OutReachabilityFlags, float MaxAcceptableDistance = max_ai_use_reach);
void						AITAC_CalculateAlienReachabilityFlags(const Vector& FromLocation, const Vector& ToLocation, EAIReachabilityFlags& OutReachabilityFlags, float MaxAcceptableDistance = max_ai_use_reach);
void						AITAC_RefreshReachabilityForItem(AvHAIDroppedItem* Item);
void						AITAC_OnStructureBecomeSolid(AvHAIBuildableStructure* Structure);
void						AITAC_OnStructureCompleted(AvHAIBuildableStructure* Structure);
void						AITAC_OnStructureBeginRecycling(AvHAIBuildableStructure* RecyclingStructure);
void						AITAC_OnStructureDestroyed(AvHAIBuildableStructure* DestroyedStructure);
void						AITAC_LinkDeployedItemToAction(AvHAIPlayer* CommanderBot, const AvHAIDroppedItem* NewItem);
void						AITAC_LinkStructureToPlayer(const AvHAIBuildableStructure* NewStructure);

AvHAIDroppedItem*			AITAC_RegisterNewDroppedItem(CBaseEntity* NewItem, EAIDeployableItemType ItemType);


// Will prefer to find whichever chair is in use, and if not then ideally a fully built one. Failing that, a partially-constructed one.
const AvHAIBuildableStructure*	AITAC_GetCommChair(AvHTeamNumber Team);

Vector						AITAC_GetTeamStartingLocation(AvHTeamNumber Team);

const AvHAIDroppedItem*			AITAC_FindClosestItemToLocation(const Vector& Location, const DroppedItemSearchFilter* ItemFilters);
bool						AITAC_ItemExistsInLocation(const Vector& Location, const DroppedItemSearchFilter* ItemFilters);
int							AITAC_GetNumItemsInLocation(const Vector& Location, const DroppedItemSearchFilter* ItemFilters);

const AvHAIDroppedItem*			AITAC_GetDroppedItemRefFromEdict(const edict_t* ItemEdict);

int AITAC_GetNumHives();
int AITAC_GetNumTeamHives(AvHTeamNumber Team, bool bFullyCompletedOnly);

AvHMessageID UTIL_StructureTypeToImpulseCommand(const EAIStructureType StructureType);
AvHMessageID UTIL_ItemTypeToImpulseCommand(const EAIDeployableItemType ItemType);

// Clears out the marine and alien buildable structure maps, resource node and hive lists, and the marine item list
void AITAC_ClearMapAIData();

void AITAC_RefreshTeamStartingLocations();

void AITAC_ClearStructureNavData();
void AITAC_ClearHiveNavData();

void AITAC_RefreshMarineItems();
void AITAC_RefreshMarineItem(CBaseEntity* ItemRef);

void AITAC_OnItemDropped(const AvHAIDroppedItem* NewItem);

EAIStructureType UTIL_IUSER3ToStructureType(const int inIUSER3);

const AvHAIHive* AITAC_GetHiveFromEdict(const edict_t* Edict);
const AvHAIResourceNode* AITAC_GetResourceNodeFromEdict(const edict_t* Edict);

int	AITAC_GetNumResourceNodesNearLocation(const Vector Location, const ResourceNodeSearchFilter* Filter);
const AvHAIResourceNode* AITAC_FindNearestResourceNodeToLocation(const Vector Location, const ResourceNodeSearchFilter* Filter);
vector<const AvHAIResourceNode*> AITAC_GetAllMatchingResourceNodes(const Vector Location, const ResourceNodeSearchFilter* Filter);

EAIWeaponId UTIL_GetWeaponTypeFromEdict(const edict_t* ItemEdict);

int AITAC_GetNumActivePlayersOnTeam(const AvHTeamNumber Team);
int AITAC_GetNumPlayersOfTeamInArea(const AvHTeamNumber Team, const Vector SearchLocation, const float SearchRadius, const bool bConsiderPhaseDist, const edict_t* IgnorePlayer, const AvHUser3 IgnoreClass);
bool AITAC_AnyPlayersOfTeamInArea(const AvHTeamNumber Team, const Vector SearchLocation, const float SearchRadius, const bool bConsiderPhaseDist, const edict_t* IgnorePlayer, const AvHUser3 IgnoreClass);
vector<AvHPlayer*> AITAC_GetAllPlayersOfTeamInArea(const AvHTeamNumber Team, const Vector SearchLocation, const float SearchRadius, const bool bConsiderPhaseDist, const edict_t* IgnorePlayer, const AvHUser3 IgnoreClass);
int AITAC_GetNumPlayersOfTeamAndClassInArea(const AvHTeamNumber Team, const Vector SearchLocation, const float SearchRadius, const bool bConsiderPhaseDist, const edict_t* IgnorePlayer, const AvHUser3 SearchClass);
int AITAC_GetNumPlayersOnTeamOfClass(const AvHTeamNumber Team, const AvHUser3 SearchClass, const edict_t* IgnorePlayer);
vector<AvHPlayer*> AITAC_GetAllPlayersOnTeamOfClass(const AvHTeamNumber Team, const AvHUser3 SearchClass, const edict_t* IgnorePlayer);
edict_t* AITAC_GetNearestPlayerOfClassInArea(const AvHTeamNumber Team, const Vector SearchLocation, const float SearchRadius, const bool bConsiderPhaseDist, const edict_t* IgnorePlayer, const AvHUser3 SearchClass);
vector<edict_t*> AITAC_GetAllPlayersOfClassInArea(const AvHTeamNumber Team, const Vector SearchLocation, const float SearchRadius, const bool bConsiderPhaseDist, const edict_t* IgnorePlayer, const AvHUser3 SearchClass);

const AvHAIHive* AITAC_GetTeamHiveWithTech(const AvHTeamNumber Team, const EAIHiveTechStatus Tech);
bool AITAC_TeamHiveWithTechExists(const AvHTeamNumber Team, const EAIHiveTechStatus Tech);

EAIDeployableItemType UTIL_GetItemTypeFromEdict(const edict_t* ItemEdict);

EAIWeaponId UTIL_GetWeaponTypeFromDroppedItem(const EAIDeployableItemType ItemType);


bool AITAC_MarineResearchIsAvailable(const AvHTeamNumber Team, const AvHMessageID Research);

Vector UTIL_GetNextMinePosition(const AvHAIBuildableStructure* StructureToMine);
int UTIL_GetCostOfStructureType(EAIStructureType StructureType);

bool AITAC_ResearchIsComplete(const AvHTeamNumber Team, const AvHTechID Research);

bool AITAC_PhaseGatesAvailable(const AvHTeamNumber Team);

int AITAC_GetNumDeadPlayersOnTeam(const AvHTeamNumber Team);

const vector<AvHAIResourceNode*> AITAC_GetAllResourceNodes();
const vector<AvHAIHive*> AITAC_GetAllHives();
const vector<AvHAIHive*> AITAC_GetAllTeamHives(AvHTeamNumber Team, bool bFullyBuiltOnly);

bool AITAC_AnyPlayerOnTeamWithLOS(AvHTeamNumber Team, const Vector& Location, float SearchRadius);


EAIStructureType AITAC_GetNextMissingUpgradeChamberForTeam(AvHTeamNumber Team, int& NumMissing);

bool AITAC_IsAlienUpgradeAvailableForTeam(AvHTeamNumber Team, EAIHiveTechStatus DesiredTech);

edict_t* AITAC_GetLastSeenLerkForTeam(AvHTeamNumber Team, float& LastSeenTime);

#endif