#pragma once

#ifndef AVH_AI_WEAPON_HELPER_H
#define AVH_AI_WEAPON_HELPER_H

#include "AvHAIPlayer.h"

int GetPlayerCurrentWeaponClipAmmo(const AvHPlayer* Player);
int GetPlayerCurrentWeaponMaxClipAmmo(const AvHPlayer* Player);
int GetPlayerCurrentWeaponReserveAmmo(const AvHPlayer* Player);

EAIWeaponId UTIL_GetPlayerPrimaryWeapon(const AvHPlayer* Player);
EAIWeaponId UTIL_GetPlayerSecondaryWeapon(const AvHPlayer* Player);

int UTIL_GetPlayerPrimaryWeaponClipAmmo(const AvHPlayer* Player);
int UTIL_GetPlayerPrimaryWeaponMaxClipSize(const AvHPlayer* Player);
int UTIL_GetPlayerPrimaryAmmoReserve(AvHPlayer* Player);
int UTIL_GetPlayerPrimaryMaxAmmoReserve(AvHPlayer* Player);

int UTIL_GetPlayerSecondaryWeaponClipAmmo(const AvHPlayer* Player);
int UTIL_GetPlayerSecondaryWeaponMaxClipSize(const AvHPlayer* Player);
int UTIL_GetPlayerSecondaryAmmoReserve(AvHPlayer* Player);
int UTIL_GetPlayerSecondaryMaxAmmoReserve(AvHPlayer* Player);

EAIWeaponId GetBotMarineSecondaryWeapon(const AvHAIPlayer* pBot);
int BotGetSecondaryWeaponClipAmmo(const AvHAIPlayer* pBot);
int BotGetSecondaryWeaponMaxClipSize(const AvHAIPlayer* pBot);
int BotGetSecondaryWeaponAmmoReserve(AvHAIPlayer* pBot);
int BotGetSecondaryWeaponMaxAmmoReserve(AvHAIPlayer* pBot);

float GetEnergyCostForWeapon(const EAIWeaponId Weapon);
float GetProjectileVelocityForWeapon(const EAIWeaponId Weapon);

float GetMaxIdealWeaponRange(const EAIWeaponId Weapon);
float GetMinIdealWeaponRange(const EAIWeaponId Weapon);

bool UTIL_WeaponCanBeReloaded(const EAIWeaponId CheckWeapon);
bool IsMeleeWeapon(const EAIWeaponId Weapon);

Vector UTIL_GetGrenadeThrowTarget(edict_t* Player, const Vector TargetLocation, const float ExplosionRadius, bool bPrecise);

EAIWeaponId BotMarineChooseBestWeaponForStructure(AvHAIPlayer* pBot, edict_t* target);
EAIWeaponId MarineGetBestWeaponForPlayerTarget(AvHAIPlayer* pBot, AvHPlayer* Target);
EAIWeaponId BotAlienChooseBestWeaponForStructure(AvHAIPlayer* pBot, edict_t* target);

bool BotAnyWeaponNeedsReloading(AvHAIPlayer* pBot);

// Helper function to pick the best weapon for any given situation and target type.
EAIWeaponId BotMarineChooseBestWeapon(AvHAIPlayer* pBot, edict_t* target);
EAIWeaponId BotAlienChooseBestWeapon(AvHAIPlayer* pBot, edict_t* target);

EAIWeaponId FadeGetBestWeaponForCombatTarget(AvHAIPlayer* pBot, edict_t* Target);
EAIWeaponId OnosGetBestWeaponForCombatTarget(AvHAIPlayer* pBot, edict_t* Target);
EAIWeaponId SkulkGetBestWeaponForCombatTarget(AvHAIPlayer* pBot, edict_t* Target);
EAIWeaponId GorgeGetBestWeaponForCombatTarget(AvHAIPlayer* pBot, edict_t* Target);
EAIWeaponId LerkGetBestWeaponForCombatTarget(AvHAIPlayer* pBot, edict_t* Target);

float GetReloadTimeForWeapon(EAIWeaponId Weapon);

bool CanInterruptWeaponReload(EAIWeaponId Weapon);

bool IsHitscanWeapon(EAIWeaponId Weapon);
float GetTimeUntilPlayerNextRefire(const AvHPlayer* Player);

EAIAttackResult PerformAttackLOSCheck(AvHAIPlayer* pBot, const EAIWeaponId Weapon, const edict_t* Target);
EAIAttackResult PerformAttackLOSCheck(AvHAIPlayer* pBot, const EAIWeaponId Weapon, const Vector TargetLocation);
EAIAttackResult PerformAttackLOSCheck(AvHAIPlayer* pBot, const EAIWeaponId Weapon, const Vector TargetLocation, const edict_t* Target);
EAIAttackResult PerformAttackLOSCheck(const Vector Location, const EAIWeaponId Weapon, const edict_t* Target);

float UTIL_GetProjectileVelocityForWeapon(const EAIWeaponId Weapon);
bool IsAreaAffectedBySpores(const Vector Location);

char* UTIL_WeaponTypeToClassname(const EAIWeaponId WeaponType);

#endif