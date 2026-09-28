
#include "AvHAIWeaponHelper.h"
#include "AvHAIPlayerUtil.h"
#include "AvHAIMath.h"
#include "AvHAINavigation.h"
#include "AvHAIHelper.h"
#include "AvHAITactical.h"
#include "AvHAIPlayerManager.h"

#include "AvHGamerules.h"
#include "AvHAlienWeaponConstants.h"
#include "AvHAlienWeapons.h"
#include "AvHMarineEquipmentConstants.h"
#include "AvHServerUtil.h"

int GetPlayerCurrentWeaponClipAmmo(const AvHPlayer* Player)
{
	AvHBasePlayerWeapon* theBasePlayerWeapon = dynamic_cast<AvHBasePlayerWeapon*>(Player->m_pActiveItem);

	if (theBasePlayerWeapon)
	{
		return theBasePlayerWeapon->m_iClip;
	}

	return 0;
}

int GetPlayerCurrentWeaponMaxClipAmmo(const AvHPlayer* Player)
{
	AvHBasePlayerWeapon* theBasePlayerWeapon = dynamic_cast<AvHBasePlayerWeapon*>(Player->m_pActiveItem);

	if (theBasePlayerWeapon)
	{
		return theBasePlayerWeapon->iMaxClip();
	}

	return 0;
}

int GetPlayerCurrentWeaponReserveAmmo(const AvHPlayer* Player)
{
	AvHBasePlayerWeapon* theBasePlayerWeapon = dynamic_cast<AvHBasePlayerWeapon*>(Player->m_pActiveItem);

	if (theBasePlayerWeapon)
	{
		return Player->m_rgAmmo[theBasePlayerWeapon->m_iPrimaryAmmoType];
	}

	return 0;
}

float GetProjectileVelocityForWeapon(const EAIWeaponId Weapon)
{
	switch (Weapon)
	{
		case EAIWeaponId::WEAPON_GORGE_SPIT:
			return (float)BALANCE_VAR(kSpitVelocity);
		case EAIWeaponId::WEAPON_LERK_SPORES:
			return (float)BALANCE_VAR(kShootCloudVelocity);
		case EAIWeaponId::WEAPON_FADE_ACIDROCKET:
			return (float)BALANCE_VAR(kAcidRocketVelocity);
		case EAIWeaponId::WEAPON_GORGE_BILEBOMB:
			return (float)BALANCE_VAR(kBileBombVelocity);
		case EAIWeaponId::WEAPON_MARINE_GRENADE:
		case EAIWeaponId::WEAPON_MARINE_GL:
			return (float)BALANCE_VAR(kGrenadeForce);
		default:
			return 0.0f; // Hitscan. We don't bother with bile bomb as it's so short range that it doesn't really need leading the target
	}
}

bool CanInterruptWeaponReload(EAIWeaponId Weapon)
{
	switch (Weapon)
	{
		case EAIWeaponId::WEAPON_MARINE_SHOTGUN:
		case EAIWeaponId::WEAPON_MARINE_GL:
			return true;
		default:
			return false;
	}

	return false;
}

float GetReloadTimeForWeapon(EAIWeaponId Weapon)
{
	switch (Weapon)
	{
		case EAIWeaponId::WEAPON_MARINE_PISTOL:
		case EAIWeaponId::WEAPON_MARINE_MG:
			return 3.0f;
		case EAIWeaponId::WEAPON_MARINE_HMG:
			return 6.3f;
		case EAIWeaponId::WEAPON_MARINE_SHOTGUN:
			return 0.22f;
		case EAIWeaponId::WEAPON_MARINE_GL:
			return 1.5f;
		default:
			return 0.0f;
	}

	return 0.0f;
}

float GetEnergyCostForWeapon(const EAIWeaponId Weapon)
{
	switch (Weapon)
	{
		case EAIWeaponId::WEAPON_SKULK_BITE:
			return BALANCE_VAR(kBiteEnergyCost);
		case EAIWeaponId::WEAPON_SKULK_PARASITE:
			return BALANCE_VAR(kParasiteEnergyCost);
		case EAIWeaponId::WEAPON_SKULK_LEAP:
			return BALANCE_VAR(kLeapEnergyCost);
		case EAIWeaponId::WEAPON_SKULK_XENOCIDE:
			return BALANCE_VAR(kDivineWindEnergyCost);

		case EAIWeaponId::WEAPON_GORGE_SPIT:
			return BALANCE_VAR(kSpitEnergyCost);
		case EAIWeaponId::WEAPON_GORGE_HEALINGSPRAY:
			return BALANCE_VAR(kHealingSprayEnergyCost);
		case EAIWeaponId::WEAPON_GORGE_BILEBOMB:
			return BALANCE_VAR(kBileBombEnergyCost);
		case EAIWeaponId::WEAPON_GORGE_WEB:
			return BALANCE_VAR(kWebEnergyCost);

		case EAIWeaponId::WEAPON_LERK_BITE:
			return BALANCE_VAR(kBite2EnergyCost);
		case EAIWeaponId::WEAPON_LERK_SPORES:
			return BALANCE_VAR(kSporesEnergyCost);
		case EAIWeaponId::WEAPON_LERK_UMBRA:
			return BALANCE_VAR(kUmbraEnergyCost);
		case EAIWeaponId::WEAPON_LERK_PRIMALSCREAM:
			return BALANCE_VAR(kPrimalScreamEnergyCost);

		case EAIWeaponId::WEAPON_FADE_SWIPE:
			return BALANCE_VAR(kSwipeEnergyCost);
		case EAIWeaponId::WEAPON_FADE_BLINK:
			return BALANCE_VAR(kBlinkEnergyCost);
		case EAIWeaponId::WEAPON_FADE_METABOLIZE:
			return BALANCE_VAR(kMetabolizeEnergyCost);
		case EAIWeaponId::WEAPON_FADE_ACIDROCKET:
			return BALANCE_VAR(kAcidRocketEnergyCost);

		case EAIWeaponId::WEAPON_ONOS_GORE:
			return BALANCE_VAR(kClawsEnergyCost);
		case EAIWeaponId::WEAPON_ONOS_DEVOUR:
			return BALANCE_VAR(kDevourEnergyCost);
		case EAIWeaponId::WEAPON_ONOS_STOMP:
			return BALANCE_VAR(kStompEnergyCost);
		case EAIWeaponId::WEAPON_ONOS_CHARGE:
			return BALANCE_VAR(kChargeEnergyCost);

		default:
			return 0.0f;
	}
}

void InterruptReload(AvHAIPlayer* pBot)
{
	pBot->Button |= IN_ATTACK;
}

EAIWeaponId UTIL_GetPlayerPrimaryWeapon(const AvHPlayer* Player)
{
	AvHBasePlayerWeapon* Weapon = dynamic_cast<AvHBasePlayerWeapon*>(Player->m_rgpPlayerItems[1]);

	if (Weapon)
	{
		return static_cast<EAIWeaponId>(Weapon->m_iId);
	}

	return EAIWeaponId::WEAPON_INVALID;
}

EAIWeaponId UTIL_GetPlayerSecondaryWeapon(const AvHPlayer* Player)
{
	AvHBasePlayerWeapon* Weapon = dynamic_cast<AvHBasePlayerWeapon*>(Player->m_rgpPlayerItems[2]);

	if (Weapon)
	{
		return static_cast<EAIWeaponId>(Weapon->m_iId);
	}

	return EAIWeaponId::WEAPON_INVALID;
}

bool IsHitscanWeapon(EAIWeaponId Weapon)
{
	switch (Weapon)
	{
		case EAIWeaponId::WEAPON_MARINE_MG:
		case EAIWeaponId::WEAPON_MARINE_HMG:
		case EAIWeaponId::WEAPON_MARINE_PISTOL:
		case EAIWeaponId::WEAPON_MARINE_SHOTGUN:
		case EAIWeaponId::WEAPON_SKULK_PARASITE:
		case EAIWeaponId::WEAPON_MARINE_WELDER:
			return true;
		default:
			return false;
	}

	return false;
}

float GetTimeUntilPlayerNextRefire(const AvHPlayer* Player)
{
	AvHBasePlayerWeapon* WeaponRef = dynamic_cast<AvHBasePlayerWeapon*>(Player->m_pActiveItem);

	if (!WeaponRef) { return 0.0f; }

	return WeaponRef->m_flNextPrimaryAttack;
}

EAIWeaponId GetBotMarineSecondaryWeapon(const AvHAIPlayer* pBot)
{
	if (PlayerHasWeapon(pBot->Player, EAIWeaponId::WEAPON_MARINE_PISTOL))
	{
		return EAIWeaponId::WEAPON_MARINE_PISTOL;
	}

	return EAIWeaponId::WEAPON_INVALID;
}

int UTIL_GetPlayerPrimaryMaxAmmoReserve(AvHPlayer* Player)
{
	AvHBasePlayerWeapon* theBasePlayerWeapon = dynamic_cast<AvHBasePlayerWeapon*>(Player->m_rgpPlayerItems[1]);

	if (theBasePlayerWeapon)
	{
		return theBasePlayerWeapon->iMaxAmmo1();
	}

	return 0;
}

int UTIL_GetPlayerPrimaryAmmoReserve(AvHPlayer* Player)
{
	AvHBasePlayerWeapon* theBasePlayerWeapon = dynamic_cast<AvHBasePlayerWeapon*>(Player->m_rgpPlayerItems[1]);

	if (theBasePlayerWeapon)
	{
		return Player->m_rgAmmo[theBasePlayerWeapon->m_iPrimaryAmmoType];
	}

	return 0;
}

int UTIL_GetPlayerSecondaryMaxAmmoReserve(AvHPlayer* Player)
{
	AvHBasePlayerWeapon* theBasePlayerWeapon = dynamic_cast<AvHBasePlayerWeapon*>(Player->m_rgpPlayerItems[2]);

	if (theBasePlayerWeapon)
	{
		return theBasePlayerWeapon->iMaxAmmo1();
	}

	return 0;
}

int UTIL_GetPlayerSecondaryAmmoReserve(AvHPlayer* Player)
{
	AvHBasePlayerWeapon* theBasePlayerWeapon = dynamic_cast<AvHBasePlayerWeapon*>(Player->m_rgpPlayerItems[2]);

	if (theBasePlayerWeapon)
	{
		return Player->m_rgAmmo[theBasePlayerWeapon->m_iPrimaryAmmoType];
	}

	return 0;
}

int BotGetSecondaryWeaponAmmoReserve(AvHAIPlayer* pBot)
{
	AvHBasePlayerWeapon* theBasePlayerWeapon = dynamic_cast<AvHBasePlayerWeapon*>(pBot->Player->m_rgpPlayerItems[2]);

	if (theBasePlayerWeapon)
	{
		return pBot->Player->m_rgAmmo[theBasePlayerWeapon->m_iPrimaryAmmoType];
	}

	return 0;
}

int UTIL_GetPlayerPrimaryWeaponClipAmmo(const AvHPlayer* Player)
{
	AvHBasePlayerWeapon* theBasePlayerWeapon = dynamic_cast<AvHBasePlayerWeapon*>(Player->m_rgpPlayerItems[1]);

	if (theBasePlayerWeapon)
	{
		return theBasePlayerWeapon->m_iClip;
	}

	return 0;
}

int BotGetSecondaryWeaponClipAmmo(const AvHAIPlayer* pBot)
{
	AvHBasePlayerWeapon* theBasePlayerWeapon = dynamic_cast<AvHBasePlayerWeapon*>(pBot->Player->m_rgpPlayerItems[2]);

	if (theBasePlayerWeapon)
	{
		return theBasePlayerWeapon->m_iClip;
	}

	return 0;
}

int UTIL_GetPlayerPrimaryWeaponMaxClipSize(const AvHPlayer* Player)
{
	AvHBasePlayerWeapon* theBasePlayerWeapon = dynamic_cast<AvHBasePlayerWeapon*>(Player->m_rgpPlayerItems[1]);

	if (theBasePlayerWeapon)
	{
		return theBasePlayerWeapon->iMaxClip();
	}

	return 0;
}

int UTIL_GetPlayerSecondaryWeaponClipAmmo(const AvHPlayer* Player)
{
	AvHBasePlayerWeapon* theBasePlayerWeapon = dynamic_cast<AvHBasePlayerWeapon*>(Player->m_rgpPlayerItems[2]);

	if (theBasePlayerWeapon)
	{
		return theBasePlayerWeapon->m_iClip;
	}

	return 0;
}

int UTIL_GetPlayerSecondaryWeaponMaxClipSize(const AvHPlayer* Player)
{
	AvHBasePlayerWeapon* theBasePlayerWeapon = dynamic_cast<AvHBasePlayerWeapon*>(Player->m_rgpPlayerItems[2]);

	if (theBasePlayerWeapon)
	{
		return theBasePlayerWeapon->iMaxClip();
	}

	return 0;
}

int BotGetSecondaryWeaponMaxClipSize(const AvHAIPlayer* pBot)
{
	AvHBasePlayerWeapon* theBasePlayerWeapon = dynamic_cast<AvHBasePlayerWeapon*>(pBot->Player->m_rgpPlayerItems[2]);

	if (theBasePlayerWeapon)
	{
		return theBasePlayerWeapon->iMaxClip();
	}

	return 0;
}

int BotGetSecondaryWeaponMaxAmmoReserve(AvHAIPlayer* pBot)
{
	AvHBasePlayerWeapon* theBasePlayerWeapon = dynamic_cast<AvHBasePlayerWeapon*>(pBot->Player->m_rgpPlayerItems[2]);

	if (theBasePlayerWeapon)
	{
		return theBasePlayerWeapon->iMaxClip();
	}

	return 0;
}

float GetMaxIdealWeaponRange(const EAIWeaponId Weapon)
{
	switch (Weapon)
	{
		case EAIWeaponId::WEAPON_LERK_PRIMALSCREAM:
			return UTIL_MetresToGoldSrcUnits(100.0f);
		case EAIWeaponId::WEAPON_LERK_SPORES:
		case EAIWeaponId::WEAPON_LERK_UMBRA:
		case EAIWeaponId::WEAPON_MARINE_GL:
		case EAIWeaponId::WEAPON_MARINE_MG:
		case EAIWeaponId::WEAPON_MARINE_PISTOL:
		case EAIWeaponId::WEAPON_FADE_ACIDROCKET:
		case EAIWeaponId::WEAPON_SKULK_PARASITE:
		case EAIWeaponId::WEAPON_SKULK_LEAP:
		case EAIWeaponId::WEAPON_ONOS_CHARGE:
		case EAIWeaponId::WEAPON_GORGE_SPIT:
			return UTIL_MetresToGoldSrcUnits(50.0f);
		case EAIWeaponId::WEAPON_MARINE_HMG:
		case EAIWeaponId::WEAPON_MARINE_GRENADE:
			return UTIL_MetresToGoldSrcUnits(10.0f);
		case EAIWeaponId::WEAPON_MARINE_SHOTGUN:
		case EAIWeaponId::WEAPON_GORGE_BILEBOMB:
		case EAIWeaponId::WEAPON_ONOS_STOMP:
			return UTIL_MetresToGoldSrcUnits(8.0f);
		case EAIWeaponId::WEAPON_SKULK_XENOCIDE:
			return (float)BALANCE_VAR(kDivineWindRadius) * 0.8f;
		case EAIWeaponId::WEAPON_ONOS_GORE:
			return (float)BALANCE_VAR(kClawsRange) + 20.0f;
		case EAIWeaponId::WEAPON_ONOS_DEVOUR:
			return (float)BALANCE_VAR(kDevourRange);
		case EAIWeaponId::WEAPON_FADE_SWIPE:
			return (float)BALANCE_VAR(kSwipeRange) + 30.0f;
		case EAIWeaponId::WEAPON_SKULK_BITE:
			return (float)BALANCE_VAR(kBiteRange) + 20.0f;
		case EAIWeaponId::WEAPON_LERK_BITE:
			return (float)BALANCE_VAR(kBite2Range) + 20.0f;
		case EAIWeaponId::WEAPON_GORGE_HEALINGSPRAY:
			return (float)BALANCE_VAR(kHealingSprayRange) * 0.5f;
		case EAIWeaponId::WEAPON_MARINE_WELDER:
			return (float)BALANCE_VAR(kWelderRange) + 10.0f;
		default:
			return max_player_use_reach;
	}
}

float GetMinIdealWeaponRange(const EAIWeaponId Weapon)
{
	switch (Weapon)
	{
		case EAIWeaponId::WEAPON_MARINE_GL:
		case EAIWeaponId::WEAPON_MARINE_GRENADE:
		case EAIWeaponId::WEAPON_FADE_ACIDROCKET:
			return UTIL_MetresToGoldSrcUnits(5.0f);
		case EAIWeaponId::WEAPON_SKULK_LEAP:
			return UTIL_MetresToGoldSrcUnits(3.0f);
		case EAIWeaponId::WEAPON_MARINE_MG:
		case EAIWeaponId::WEAPON_MARINE_PISTOL:
		case EAIWeaponId::WEAPON_MARINE_HMG:
		case EAIWeaponId::WEAPON_SKULK_PARASITE:
			return UTIL_MetresToGoldSrcUnits(5.0f);
		case EAIWeaponId::WEAPON_MARINE_SHOTGUN:
			return UTIL_MetresToGoldSrcUnits(2.0f);
		case EAIWeaponId::WEAPON_GORGE_BILEBOMB:
		case EAIWeaponId::WEAPON_ONOS_STOMP:
			return UTIL_MetresToGoldSrcUnits(2.0f);
		default:
			return max_player_use_reach * 0.5f;
	}
}

bool IsMeleeWeapon(const EAIWeaponId Weapon)
{
	switch (Weapon)
	{
		case EAIWeaponId::WEAPON_MARINE_KNIFE:
		case EAIWeaponId::WEAPON_SKULK_BITE:
		case EAIWeaponId::WEAPON_FADE_SWIPE:
		case EAIWeaponId::WEAPON_ONOS_GORE:
		case EAIWeaponId::WEAPON_ONOS_DEVOUR:
		case EAIWeaponId::WEAPON_LERK_BITE:
			return true;
		default:
			return false;
	}
}

bool WeaponCanBeReloaded(const EAIWeaponId CheckWeapon)
{
	switch (CheckWeapon)
	{
		case EAIWeaponId::WEAPON_MARINE_GL:
		case EAIWeaponId::WEAPON_MARINE_HMG:
		case EAIWeaponId::WEAPON_MARINE_MG:
		case EAIWeaponId::WEAPON_MARINE_PISTOL:
		case EAIWeaponId::WEAPON_MARINE_SHOTGUN:
			return true;
		default:
			return false;
	}
}


Vector UTIL_GetGrenadeThrowTarget(edict_t* Player, const Vector TargetLocation, const float ExplosionRadius, bool bPrecise)
{
	if (UTIL_PlayerHasLOSToLocation(Player, TargetLocation, UTIL_MetresToGoldSrcUnits(10.0f)))
	{
		return TargetLocation;
	}

	if (UTIL_PointIsDirectlyReachable(Player->v.origin, TargetLocation))
	{
		Vector Orientation = UTIL_GetVectorNormal(Player->v.origin - TargetLocation);

		Vector NewSpot = TargetLocation + (Orientation * UTIL_MetresToGoldSrcUnits(1.5f));

		NewSpot = UTIL_ProjectPointToNavmesh(NewSpot);

		if (NewSpot != ZERO_VECTOR)
		{
			NewSpot.z += 10.0f;
		}

		return NewSpot;
	}

	vector<AvHAIPathNode> CheckPath;
	CheckPath.clear();

	dtStatus Status = FindPathClosestToPoint(GetBaseNavProfile(ALL_NAV_PROFILE), Player->v.origin, TargetLocation, CheckPath, ExplosionRadius);

	if (dtStatusSucceed(Status))
	{
		Vector FurthestPointVisible = UTIL_GetFurthestVisiblePointOnPath(GetPlayerEyePosition(Player), CheckPath, bPrecise);

		if (vDist3DSq(FurthestPointVisible, TargetLocation) <= sqrf(ExplosionRadius))
		{
			return FurthestPointVisible;
		}

		Vector ThrowDir = UTIL_GetVectorNormal(FurthestPointVisible - Player->v.origin);

		Vector LineEnd = FurthestPointVisible + (ThrowDir * UTIL_MetresToGoldSrcUnits(5.0f));

		Vector ClosestPointInTrajectory = vClosestPointOnLine(FurthestPointVisible, LineEnd, TargetLocation);

		ClosestPointInTrajectory = UTIL_ProjectPointToNavmesh(ClosestPointInTrajectory);
		ClosestPointInTrajectory.z += 10.0f;

		if (vDist2DSq(ClosestPointInTrajectory, TargetLocation) < sqrf(ExplosionRadius) && UTIL_PlayerHasLOSToLocation(Player, ClosestPointInTrajectory, UTIL_MetresToGoldSrcUnits(10.0f)) && UTIL_PointIsDirectlyReachable(ClosestPointInTrajectory, TargetLocation))
		{
			return ClosestPointInTrajectory;
		}
		else
		{
			return ZERO_VECTOR;
		}
	}
	else
	{
		return ZERO_VECTOR;
	}
}

EAIWeaponId BotAlienChooseBestWeapon(AvHAIPlayer* pBot, edict_t* target)
{
	if (FNullEnt(target))
	{
		return UTIL_GetPlayerPrimaryWeapon(pBot->Player);
	}

	return BotAlienChooseBestWeaponForStructure(pBot, target);
}

EAIWeaponId BotMarineChooseBestWeapon(AvHAIPlayer* pBot, edict_t* target)
{
	if (FNullEnt(target))
	{
		if (IsPlayerReloading(pBot->Player))
		{
			return GetPlayerCurrentWeapon(pBot->Player);
		}

		if (UTIL_GetPlayerPrimaryWeaponClipAmmo(pBot->Player) > 0 || UTIL_GetPlayerPrimaryAmmoReserve(pBot->Player) > 0)
		{
			return UTIL_GetPlayerPrimaryWeapon(pBot->Player);
		}
		else if (BotGetSecondaryWeaponClipAmmo(pBot) > 0 || BotGetSecondaryWeaponAmmoReserve(pBot) > 0)
		{
			return GetBotMarineSecondaryWeapon(pBot);
		}
		return UTIL_GetPlayerPrimaryWeapon(pBot->Player);
	}

	if (IsEdictPlayer(target))
	{
		return MarineGetBestWeaponForPlayerTarget(pBot, dynamic_cast<AvHPlayer*>(CBaseEntity::Instance(target)));
	}
	else
	{
		return BotMarineChooseBestWeaponForStructure(pBot, target);
	}
}

bool BotAnyWeaponNeedsReloading(AvHAIPlayer* pBot)
{
	if (UTIL_GetPlayerPrimaryWeaponClipAmmo(pBot->Player) < UTIL_GetPlayerPrimaryWeaponMaxClipSize(pBot->Player) && UTIL_GetPlayerPrimaryAmmoReserve(pBot->Player) > 0) { return true; }
	if (UTIL_GetPlayerSecondaryWeaponClipAmmo(pBot->Player) < UTIL_GetPlayerSecondaryWeaponMaxClipSize(pBot->Player) && UTIL_GetPlayerSecondaryAmmoReserve(pBot->Player) > 0) { return true; }

	return false;
}

EAIWeaponId BotAlienChooseBestWeaponForStructure(AvHAIPlayer* pBot, edict_t* target)
{
	EAIStructureType StructureType = GetStructureTypeFromEdict(target);

	if (StructureType == EAIStructureType::STRUCTURE_NONE)
	{
		return UTIL_GetPlayerPrimaryWeapon(pBot->Player);
	}

	if (PlayerHasWeapon(pBot->Player, EAIWeaponId::WEAPON_GORGE_BILEBOMB))
	{
		return EAIWeaponId::WEAPON_GORGE_BILEBOMB;
	}

	if (PlayerHasWeapon(pBot->Player, EAIWeaponId::WEAPON_FADE_ACIDROCKET) && (StructureType == EAIStructureType::STRUCTURE_ALIEN_HIVE || IsDamagingStructure(StructureType)))
	{
		return EAIWeaponId::WEAPON_FADE_ACIDROCKET;
	}

	// If we have xenocide, then choose it if we have lots of good targets in blast radius
	if (PlayerHasWeapon(pBot->Player, EAIWeaponId::WEAPON_SKULK_XENOCIDE))
	{
		AvHTeamNumber EnemyTeam = AIMGR_GetEnemyTeam(pBot->Player->GetTeam());

		int NumEnemyTargetsInArea = AITAC_GetNumPlayersOfTeamInArea(EnemyTeam, target->v.origin, UTIL_MetresToGoldSrcUnits(5.0f), false, nullptr, AVH_USER3_NONE);

		AvHTeam* EnemyTeamRef = GetGameRules()->GetTeam(EnemyTeam);

		if (EnemyTeamRef)
		{
			EAIStructureType StructureSearchType = (EnemyTeamRef->GetTeamType() == AVH_CLASS_TYPE_MARINE) ? EAIStructureType::ALL_MARINE_STRUCTURES : EAIStructureType::ALL_ALIEN_STRUCTURES;

			StructureSearchFilter SearchFilter;
			SearchFilter.DeployableTypes = StructureSearchType;
			SearchFilter.MaxSearchRadius = UTIL_MetresToGoldSrcUnits(5.0f);
			SearchFilter.DeployableTeam = EnemyTeam;

			AIBuildableStructureList AllMatching = AITAC_FindAllMatchingStructures(target->v.origin, &SearchFilter);

			NumEnemyTargetsInArea += AllMatching.size();
		}

		if (NumEnemyTargetsInArea > 2)
		{
			return EAIWeaponId::WEAPON_SKULK_XENOCIDE;
		}
	}

	return UTIL_GetPlayerPrimaryWeapon(pBot->Player);
}

EAIWeaponId BotMarineChooseBestWeaponForStructure(AvHAIPlayer* pBot, edict_t* target)
{
	const AvHAIBuildableStructure* MatchedStructure = AITAC_GetStructureFromEdict(target);

	if (!MatchedStructure || MatchedStructure->StructureType == EAIStructureType::STRUCTURE_ALIEN_HIVE || MatchedStructure->IsElectrified() || MatchedStructure->IsDamagingStructure())
	{
		if (UTIL_GetPlayerPrimaryWeaponClipAmmo(pBot->Player) > 0 || UTIL_GetPlayerPrimaryAmmoReserve(pBot->Player) > 0)
		{
			return UTIL_GetPlayerPrimaryWeapon(pBot->Player);
		}
		else if (BotGetSecondaryWeaponClipAmmo(pBot) > 0 || BotGetSecondaryWeaponAmmoReserve(pBot) > 0)
		{
			return GetBotMarineSecondaryWeapon(pBot);
		}
		else
		{
			return EAIWeaponId::WEAPON_MARINE_KNIFE;
		}
	}

	EAIWeaponId PrimaryWeapon = UTIL_GetPlayerPrimaryWeapon(pBot->Player);

	if ((PrimaryWeapon == EAIWeaponId::WEAPON_MARINE_GL || PrimaryWeapon == EAIWeaponId::WEAPON_MARINE_SHOTGUN) && (UTIL_GetPlayerPrimaryWeaponClipAmmo(pBot->Player) > 0 || UTIL_GetPlayerPrimaryAmmoReserve(pBot->Player) > 0))
	{
		return PrimaryWeapon;
	}

	return EAIWeaponId::WEAPON_MARINE_KNIFE;
}

EAIWeaponId MarineGetBestWeaponForPlayerTarget(AvHAIPlayer* pBot, AvHPlayer* Target)
{
	EAIWeaponId PrimaryWeapon = UTIL_GetPlayerPrimaryWeapon(pBot->Player);
	EAIWeaponId SecondaryWeapon = UTIL_GetPlayerSecondaryWeapon(pBot->Player);
	EAIWeaponId CurrentWeapon = GetPlayerCurrentWeapon(pBot->Player);

	float DistToEnemy = vDist2DSq(pBot->Edict->v.origin, Target->pev->origin);

	bool bHasAmmoForPrimary = (PrimaryWeapon != EAIWeaponId::WEAPON_INVALID && UTIL_GetPlayerPrimaryWeaponClipAmmo(pBot->Player) > 0 || UTIL_GetPlayerPrimaryAmmoReserve(pBot->Player) > 0);
	bool bHasAmmoForSecondary = (SecondaryWeapon != EAIWeaponId::WEAPON_INVALID && UTIL_GetPlayerSecondaryWeaponClipAmmo(pBot->Player) > 0 || UTIL_GetPlayerSecondaryAmmoReserve(pBot->Player) > 0);

	if (PrimaryWeapon != EAIWeaponId::WEAPON_INVALID && UTIL_GetPlayerPrimaryWeaponClipAmmo(pBot->Player) > 0)
	{
		if (PrimaryWeapon == EAIWeaponId::WEAPON_MARINE_GL)
		{
			if (DistToEnemy > sqrf(BALANCE_VAR(kGrenadeRadius)) || !bHasAmmoForSecondary)
			{
				return PrimaryWeapon;
			}
			else
			{
				if (bHasAmmoForSecondary)
				{
					return SecondaryWeapon;
				}
				else
				{
					return EAIWeaponId::WEAPON_MARINE_KNIFE;
				}
			}
		}
		else if (PrimaryWeapon == EAIWeaponId::WEAPON_MARINE_SHOTGUN)
		{
			float MaxDist = (IsPlayerMarine(Target) || Target->GetUser3() > AVH_USER3_ALIEN_PLAYER3) ? UTIL_MetresToGoldSrcUnits(15.0f) : UTIL_MetresToGoldSrcUnits(8.0f);

			// Give a little extra leeway if the bot is currently holding a shotgun. Helps prevent rapid switching if the enemy is right on the edge of the max distance
			if (CurrentWeapon == PrimaryWeapon)
			{
				MaxDist *= 1.25f;
			}

			if (DistToEnemy < sqrf(MaxDist) || !bHasAmmoForSecondary)
			{
				return PrimaryWeapon;
			}
			else
			{
				if (bHasAmmoForSecondary)
				{
					return SecondaryWeapon;
				}
				else
				{
					return EAIWeaponId::WEAPON_MARINE_KNIFE;
				}
			}
		}
		else
		{
			return PrimaryWeapon;
		}
	}

	bool bEnemyIsRanged = IsPlayerMarine(Target) || ((GetPlayerCurrentWeapon(Target) == EAIWeaponId::WEAPON_FADE_ACIDROCKET || GetPlayerCurrentWeapon(Target) == EAIWeaponId::WEAPON_LERK_SPORES) && DistToEnemy > sqrf(UTIL_MetresToGoldSrcUnits(5.0f)));

	if (bEnemyIsRanged)
	{
		if (bHasAmmoForSecondary)
		{
			return SecondaryWeapon;
		}
		else
		{
			return EAIWeaponId::WEAPON_MARINE_KNIFE;
		}
	}

	if (DistToEnemy > sqrf(UTIL_MetresToGoldSrcUnits(5.0f)))
	{
		if (bHasAmmoForPrimary)
		{
			return PrimaryWeapon;
		}
		else if (bHasAmmoForSecondary)
		{
			return SecondaryWeapon;
		}
		else
		{
			return EAIWeaponId::WEAPON_MARINE_KNIFE;
		}
	}

	if (UTIL_GetPlayerPrimaryWeaponClipAmmo(pBot->Player) > 0)
	{
		return PrimaryWeapon;
	}
	else if (UTIL_GetPlayerSecondaryWeaponClipAmmo(pBot->Player) > 0)
	{
		return SecondaryWeapon;
	}
	else
	{
		return EAIWeaponId::WEAPON_MARINE_KNIFE;
	}
}

EAIWeaponId GorgeGetBestWeaponForCombatTarget(AvHAIPlayer* pBot, edict_t* Target)
{
	// Apparently I only imagined bile bomb doing damage to marine armour. Leaving it commented out in case we want to enable it again in future
	/*if (Target->v.armorvalue > 0.0f && PlayerHasWeapon(pBot->Edict, WEAPON_GORGE_BILEBOMB) && vDist2DSq(pBot->Edict->v.origin, Target->v.origin) < sqrf(GetMaxIdealWeaponRange(WEAPON_GORGE_BILEBOMB)))
	{
		return WEAPON_GORGE_BILEBOMB;
	}*/

	return EAIWeaponId::WEAPON_GORGE_SPIT;
}

EAIWeaponId SkulkGetBestWeaponForCombatTarget(AvHAIPlayer* pBot, edict_t* Target)
{
	return EAIWeaponId::WEAPON_SKULK_BITE;
}

EAIWeaponId LerkGetBestWeaponForCombatTarget(AvHAIPlayer* pBot, edict_t* Target)
{
	return EAIWeaponId::WEAPON_LERK_BITE;
}

EAIWeaponId OnosGetBestWeaponForCombatTarget(AvHAIPlayer* pBot, edict_t* Target)
{
	return EAIWeaponId::WEAPON_ONOS_GORE;
}

EAIWeaponId FadeGetBestWeaponForCombatTarget(AvHAIPlayer* pBot, edict_t* Target)
{
	return EAIWeaponId::WEAPON_FADE_SWIPE;
}

void BotReloadCurrentWeapon(AvHAIPlayer* pBot)
{
	EAIWeaponId CurrentWeapon = GetPlayerCurrentWeapon(pBot->Player);

	if (!WeaponCanBeReloaded(CurrentWeapon)) { return; }

	if (!IsPlayerReloading(pBot->Player))
	{
		if (gpGlobals->time - pBot->LastUseTime > 1.0f)
		{
			pBot->Button |= IN_RELOAD;
			pBot->LastUseTime = gpGlobals->time;
		}
	}
}

EAIAttackResult PerformAttackLOSCheck(AvHAIPlayer* pBot, const EAIWeaponId Weapon, const edict_t* Target)
{
	if (FNullEnt(Target) || (Target->v.deadflag != DEAD_NO)) { return EAIAttackResult::ATTACK_INVALIDTARGET; }

	if (Weapon == EAIWeaponId::WEAPON_INVALID) { return EAIAttackResult::ATTACK_NOWEAPON; }

	// Don't need aiming or special LOS checks for primal scream as it's AoE buff
	if (Weapon == EAIWeaponId::WEAPON_LERK_PRIMALSCREAM)
	{
		return EAIAttackResult::ATTACK_SUCCESS;
	}

	// Add a LITTLE bit of give to avoid edge cases where the bot is a smidge out of range
	float MaxWeaponRange = GetMaxIdealWeaponRange(Weapon) - 5.0f;

	// Don't need aiming or special LOS checks for Xenocide as it's an AOE attack, just make sure we're close enough and don't have a wall in the way
	if (Weapon == EAIWeaponId::WEAPON_SKULK_XENOCIDE)
	{
		if (vDist3DSq(pBot->Edict->v.origin, Target->v.origin) <= sqrf(MaxWeaponRange) && UTIL_QuickTrace(pBot->Edict, pBot->Edict->v.origin, Target->v.origin))
		{
			return EAIAttackResult::ATTACK_SUCCESS;
		}
		else
		{
			return EAIAttackResult::ATTACK_OUTOFRANGE;
		}
	}

	// For charge and stomp, we can go through stuff so don't need to check for being blocked
	if (Weapon == EAIWeaponId::WEAPON_ONOS_CHARGE || Weapon == EAIWeaponId::WEAPON_ONOS_STOMP)
	{
		if (vDist3DSq(pBot->Edict->v.origin, Target->v.origin) > sqrf(MaxWeaponRange)) { return EAIAttackResult::ATTACK_OUTOFRANGE; }

		if (!UTIL_QuickTrace(pBot->Edict, pBot->Edict->v.origin, Target->v.origin) || fabsf(Target->v.origin.z - Target->v.origin.z) > 50.0f) { return EAIAttackResult::ATTACK_OUTOFRANGE; }

		return EAIAttackResult::ATTACK_SUCCESS;
	}

	TraceResult hit;

	Vector StartTrace = pBot->CurrentEyePosition;

	Vector AttackDir = UTIL_GetVectorNormal(UTIL_GetCentreOfEntity(Target) - StartTrace);

	Vector EndTrace = pBot->CurrentEyePosition + (AttackDir * MaxWeaponRange);

	UTIL_TraceLine(StartTrace, EndTrace, dont_ignore_monsters, dont_ignore_glass, pBot->Edict->v.pContainingEntity, &hit);

	if (FNullEnt(hit.pHit)) { return EAIAttackResult::ATTACK_OUTOFRANGE; }

	if (hit.pHit != Target)
	{
		if (vDist3DSq(pBot->CurrentEyePosition, Target->v.origin) > sqrf(MaxWeaponRange))
		{
			return EAIAttackResult::ATTACK_OUTOFRANGE;
		}
		else
		{
			return EAIAttackResult::ATTACK_BLOCKED;
		}
	}

	return EAIAttackResult::ATTACK_SUCCESS;
}

EAIAttackResult PerformAttackLOSCheck(AvHAIPlayer* pBot, const EAIWeaponId Weapon, const Vector TargetLocation)
{
	if (!TargetLocation) { return EAIAttackResult::ATTACK_INVALIDTARGET; }

	if (Weapon == EAIWeaponId::WEAPON_INVALID) { return EAIAttackResult::ATTACK_NOWEAPON; }

	// Don't need aiming or special LOS checks for primal scream as it's AoE buff
	if (Weapon == EAIWeaponId::WEAPON_LERK_PRIMALSCREAM)
	{
		return EAIAttackResult::ATTACK_SUCCESS;
	}

	// Add a LITTLE bit of give to avoid edge cases where the bot is a smidge out of range
	float MaxWeaponRange = GetMaxIdealWeaponRange(Weapon) - 5.0f;

	// Don't need aiming or special LOS checks for Xenocide as it's an AOE attack, just make sure we're close enough and don't have a wall in the way
	if (Weapon == EAIWeaponId::WEAPON_SKULK_XENOCIDE)
	{
		if (vDist3DSq(pBot->Edict->v.origin, TargetLocation) <= sqrf(MaxWeaponRange) && UTIL_QuickTrace(pBot->Edict, pBot->Edict->v.origin, TargetLocation))
		{
			return EAIAttackResult::ATTACK_SUCCESS;
		}
		else
		{
			return EAIAttackResult::ATTACK_OUTOFRANGE;
		}
	}

	// For charge and stomp, we can go through stuff so don't need to check for being blocked
	if (Weapon == EAIWeaponId::WEAPON_ONOS_CHARGE || Weapon == EAIWeaponId::WEAPON_ONOS_STOMP)
	{
		if (vDist3DSq(pBot->Edict->v.origin, TargetLocation) > sqrf(MaxWeaponRange)) { return EAIAttackResult::ATTACK_OUTOFRANGE; }

		if (!UTIL_QuickTrace(pBot->Edict, pBot->Edict->v.origin, TargetLocation) || fabsf(TargetLocation.z - TargetLocation.z) > 50.0f) { return EAIAttackResult::ATTACK_OUTOFRANGE; }

		return EAIAttackResult::ATTACK_SUCCESS;
	}

	TraceResult hit;

	Vector StartTrace = pBot->CurrentEyePosition;

	Vector AttackDir = UTIL_GetVectorNormal(TargetLocation - StartTrace);

	Vector EndTrace = pBot->CurrentEyePosition + (AttackDir * MaxWeaponRange);

	if (vDist3DSq(StartTrace, EndTrace) < vDist3DSq(StartTrace, TargetLocation)) { return EAIAttackResult::ATTACK_OUTOFRANGE; }

	UTIL_TraceLine(StartTrace, EndTrace, dont_ignore_monsters, dont_ignore_glass, pBot->Edict->v.pContainingEntity, &hit);

	return (hit.flFraction >= 1.0f) ? EAIAttackResult::ATTACK_SUCCESS : EAIAttackResult::ATTACK_BLOCKED;
}

EAIAttackResult PerformAttackLOSCheck(AvHAIPlayer* pBot, const EAIWeaponId Weapon, const Vector TargetLocation, const edict_t* Target)
{
	if (!TargetLocation) { return EAIAttackResult::ATTACK_INVALIDTARGET; }

	if (Weapon == EAIWeaponId::WEAPON_INVALID) { return EAIAttackResult::ATTACK_NOWEAPON; }

	// Don't need aiming or special LOS checks for primal scream as it's AoE buff
	if (Weapon == EAIWeaponId::WEAPON_LERK_PRIMALSCREAM)
	{
		return EAIAttackResult::ATTACK_SUCCESS;
	}

	// Add a LITTLE bit of give to avoid edge cases where the bot is a smidge out of range
	float MaxWeaponRange = GetMaxIdealWeaponRange(Weapon) - 5.0f;

	// Don't need aiming or special LOS checks for Xenocide as it's an AOE attack, just make sure we're close enough and don't have a wall in the way
	if (Weapon == EAIWeaponId::WEAPON_SKULK_XENOCIDE)
	{
		if (vDist3DSq(pBot->Edict->v.origin, TargetLocation) <= sqrf(MaxWeaponRange) && UTIL_QuickTrace(pBot->Edict, pBot->Edict->v.origin, TargetLocation))
		{
			return EAIAttackResult::ATTACK_SUCCESS;
		}
		else
		{
			return EAIAttackResult::ATTACK_OUTOFRANGE;
		}
	}

	// For charge and stomp, we can go through stuff so don't need to check for being blocked
	if (Weapon == EAIWeaponId::WEAPON_ONOS_CHARGE || Weapon == EAIWeaponId::WEAPON_ONOS_STOMP)
	{
		if (vDist3DSq(pBot->Edict->v.origin, TargetLocation) > sqrf(MaxWeaponRange)) { return EAIAttackResult::ATTACK_OUTOFRANGE; }

		if (!UTIL_QuickTrace(pBot->Edict, pBot->Edict->v.origin, TargetLocation) || fabsf(TargetLocation.z - TargetLocation.z) > 50.0f) { return EAIAttackResult::ATTACK_OUTOFRANGE; }

		return EAIAttackResult::ATTACK_SUCCESS;
	}

	TraceResult hit;

	Vector StartTrace = pBot->CurrentEyePosition;

	Vector AttackDir = UTIL_GetVectorNormal(TargetLocation - StartTrace);

	Vector EndTrace = pBot->CurrentEyePosition + (AttackDir * MaxWeaponRange);

	if (vDist3DSq(StartTrace, EndTrace) < vDist3DSq(StartTrace, TargetLocation)) { return EAIAttackResult::ATTACK_OUTOFRANGE; }

	UTIL_TraceLine(StartTrace, EndTrace, dont_ignore_monsters, dont_ignore_glass, pBot->Edict->v.pContainingEntity, &hit);

	return (hit.flFraction >= 1.0f || hit.pHit == Target) ? EAIAttackResult::ATTACK_SUCCESS : EAIAttackResult::ATTACK_BLOCKED;

}

EAIAttackResult PerformAttackLOSCheck(const Vector Location, const EAIWeaponId Weapon, const edict_t* Target)
{
	if (FNullEnt(Target) || (Target->v.deadflag != DEAD_NO)) { return EAIAttackResult::ATTACK_INVALIDTARGET; }

	if (Weapon == EAIWeaponId::WEAPON_INVALID) { return EAIAttackResult::ATTACK_NOWEAPON; }

	float MaxWeaponRange = GetMaxIdealWeaponRange(Weapon);

	// Don't need aiming or special LOS checks for Xenocide as it's an AOE attack, just make sure we're close enough and don't have a wall in the way
	if (Weapon == EAIWeaponId::WEAPON_SKULK_XENOCIDE)
	{
		if (vDist3DSq(Location, Target->v.origin) <= sqrf(MaxWeaponRange) && UTIL_QuickTrace(nullptr, Location, Target->v.origin))
		{
			return EAIAttackResult::ATTACK_SUCCESS;
		}
		else
		{
			return EAIAttackResult::ATTACK_OUTOFRANGE;
		}
	}

	// For charge and stomp, we can go through stuff so don't need to check for being blocked
	if (Weapon == EAIWeaponId::WEAPON_ONOS_CHARGE || Weapon == EAIWeaponId::WEAPON_ONOS_STOMP)
	{
		if (vDist3DSq(Location, Target->v.origin) > sqrf(MaxWeaponRange)) { return EAIAttackResult::ATTACK_OUTOFRANGE; }

		if (!UTIL_QuickTrace(nullptr, Location, Target->v.origin) || fabsf(Target->v.origin.z - Target->v.origin.z) > 50.0f) { return EAIAttackResult::ATTACK_OUTOFRANGE; }

		return EAIAttackResult::ATTACK_SUCCESS;
	}

	bool bIsMeleeWeapon = IsMeleeWeapon(Weapon);

	TraceResult hit;

	Vector StartTrace = Location;

	Vector AttackDir = UTIL_GetVectorNormal(UTIL_GetCentreOfEntity(Target) - StartTrace);

	Vector EndTrace = Location + (AttackDir * MaxWeaponRange);

	UTIL_TraceLine(StartTrace, EndTrace, dont_ignore_monsters, dont_ignore_glass, nullptr, &hit);

	if (FNullEnt(hit.pHit)) { return EAIAttackResult::ATTACK_OUTOFRANGE; }

	if (hit.pHit != Target)
	{
		if (vDist3DSq(Location, Target->v.origin) > sqrf(MaxWeaponRange))
		{
			return EAIAttackResult::ATTACK_OUTOFRANGE;
		}
		else
		{
			return EAIAttackResult::ATTACK_BLOCKED;
		}
	}

	return EAIAttackResult::ATTACK_SUCCESS;
}


bool IsAreaAffectedBySpores(const Vector Location)
{
	bool Result = false;

	FOR_ALL_ENTITIES(kwsSporeProjectile, AvHSporeProjectile*)

		if (vDist2DSq(theEntity->pev->origin, Location) <= BALANCE_VAR(kSporeCloudRadius))
		{
			Result = true;
			break;
		}

	END_FOR_ALL_ENTITIES(kwsSporeProjectile)

	return Result;
}

float UTIL_GetProjectileVelocityForWeapon(const EAIWeaponId Weapon)
{
	switch (Weapon)
	{
		case EAIWeaponId::WEAPON_GORGE_SPIT:
			return (float)kSpitVelocity;
		case EAIWeaponId::WEAPON_LERK_SPORES:
			return (float)kShootCloudVelocity;
		case EAIWeaponId::WEAPON_FADE_ACIDROCKET:
			return (float)kAcidRocketVelocity;
		case EAIWeaponId::WEAPON_GORGE_BILEBOMB:
			return (float)kBileBombVelocity;
		case EAIWeaponId::WEAPON_LERK_SPIKE:
			return (float)kSpikeVelocity;
		case EAIWeaponId::WEAPON_MARINE_GRENADE:
		case EAIWeaponId::WEAPON_MARINE_GL:
			return (float)BALANCE_VAR(kGrenadeForce);
		default:
			return 0.0f; // Hitscan.
	}
}

char* UTIL_WeaponTypeToClassname(const EAIWeaponId WeaponType)
{
	switch (WeaponType)
	{
		case EAIWeaponId::WEAPON_MARINE_MG:
			return kwsMachineGun;
		case EAIWeaponId::WEAPON_MARINE_PISTOL:
			return kwsPistol;
		case EAIWeaponId::WEAPON_MARINE_KNIFE:
			return kwsKnife;
		case EAIWeaponId::WEAPON_MARINE_SHOTGUN:
			return kwsShotGun;
		case EAIWeaponId::WEAPON_MARINE_HMG:
			return kwsHeavyMachineGun;
		case EAIWeaponId::WEAPON_MARINE_WELDER:
			return kwsWelder;
		case EAIWeaponId::WEAPON_MARINE_MINES:
			return kwsMine;
		case EAIWeaponId::WEAPON_MARINE_GRENADE:
			return kwsGrenade;
		case EAIWeaponId::WEAPON_MARINE_GL:
			return kwsGrenadeGun;

		case EAIWeaponId::WEAPON_SKULK_BITE:
			return kwsBiteGun;
		case EAIWeaponId::WEAPON_SKULK_PARASITE:
			return kwsParasiteGun;
		case EAIWeaponId::WEAPON_SKULK_LEAP:
			return kwsLeap;
		case EAIWeaponId::WEAPON_SKULK_XENOCIDE:
			return kwsDivineWind;

		case EAIWeaponId::WEAPON_GORGE_SPIT:
			return kwsSpitGun;
		case EAIWeaponId::WEAPON_GORGE_HEALINGSPRAY:
			return kwsHealingSpray;
		case EAIWeaponId::WEAPON_GORGE_BILEBOMB:
			return kwsBileBombGun;
		case EAIWeaponId::WEAPON_GORGE_WEB:
			return kwsWebSpinner;

		case EAIWeaponId::WEAPON_LERK_BITE:
			return kwsBite2Gun;
		case EAIWeaponId::WEAPON_LERK_SPORES:
			return kwsSporeGun;
		case EAIWeaponId::WEAPON_LERK_UMBRA:
			return kwsUmbraGun;
		case EAIWeaponId::WEAPON_LERK_PRIMALSCREAM:
			return kwsPrimalScream;

		case EAIWeaponId::WEAPON_FADE_SWIPE:
			return kwsSwipe;
		case EAIWeaponId::WEAPON_FADE_BLINK:
			return kwsBlinkGun;
		case EAIWeaponId::WEAPON_FADE_METABOLIZE:
			return kwsMetabolize;
		case EAIWeaponId::WEAPON_FADE_ACIDROCKET:
			return kwsAcidRocketGun;

		case EAIWeaponId::WEAPON_ONOS_GORE:
			return kwsClaws;
		case EAIWeaponId::WEAPON_ONOS_DEVOUR:
			return kwsDevour;
		case EAIWeaponId::WEAPON_ONOS_STOMP:
			return kwsStomp;
		case EAIWeaponId::WEAPON_ONOS_CHARGE:
			return kwsCharge;
		default:
			return "";
	}

	return "";
}