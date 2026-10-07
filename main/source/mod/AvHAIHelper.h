#pragma once

#ifndef AVH_AI_HELPER_H
#define AVH_AI_HELPER_H

#include "AvHPlayer.h"
#include "AvHAIConstants.h"

template<class T> inline bool EnumHasAnyFlags(T a, T b) { return (static_cast<uint32>(a) & static_cast<uint32>(b)) > 0; }
template<class T> inline bool EnumHasAllFlags(T a, T b) { return (static_cast<uint32>(a) & static_cast<uint32>(b)) == static_cast<uint32>(b); }
template<class T> inline void EnumAddFlags(T& a, T b) { a = static_cast<T>(static_cast<uint32>(a) | static_cast<uint32>(b)); }
template<class T> inline void EnumRemoveFlags(T& a, T b) { a = static_cast<T>(static_cast<uint32>(a) & ~static_cast<uint32>(b)); }
template<class T> inline T EnumGetCombinedFlags(T a, T b) { return static_cast<T>(static_cast<uint32>(a) | static_cast<uint32>(b)); }

bool UTIL_IsEdictActive(const edict_t* Edict);
bool UTIL_QuickTrace(const edict_t* pEdict, const Vector& start, const Vector& end, bool bAllowStartSolid = false);
bool UTIL_QuickHullTrace(const edict_t* pEdict, const Vector& start, const Vector& end, bool bAllowStartSolid = false);
bool UTIL_QuickHullTrace(const edict_t* pEdict, const Vector& start, const Vector& end, int hullNum, bool bAllowStartSolid = false);
edict_t* UTIL_TraceEntity(const edict_t* pEdict, const Vector& start, const Vector& end);
edict_t* UTIL_TraceEntityHull(const edict_t* pEdict, const Vector& start, const Vector& end);
Vector UTIL_GetTraceHitLocation(const Vector Start, const Vector End);
Vector UTIL_GetHullTraceHitLocation(const Vector Start, const Vector End, int HullNum);

Vector UTIL_GetCentreOfEntity(const edict_t* Entity);
Vector UTIL_GetFloorUnderEntity(const edict_t* Edict);
Vector UTIL_FindFloor(const Vector& CheckLocation, const edict_t* IgnoreEntity = nullptr);

// Returns the name of the supplied location on the map. This will be the same as what appears in the bottom left of the player's screen
string UTIL_GetLocationName(Vector Location);

Vector UTIL_GetClosestPointOnEntityToLocation(const Vector UserLocation, const edict_t* Entity);
Vector UTIL_GetClosestPointOnEntityToLocation(const Vector Location, const edict_t* Entity, const Vector EntityLocation);


bool IsEdictHive(const edict_t* edict);

AvHMessageID UTIL_GetVoicelineId(EAIVoiceLine RequiredVoiceLine);





bool UTIL_IsCloakedPlayerInvisible(const edict_t* Observer, const AvHPlayer* Player);

AvHMessageID UTIL_GetEvolveUpgradeImpulse(EAIAlienUpgrade DesiredUpgrade);
AvHMessageID UTIL_GetEvolveLifeformImpulse(EAIAlienLifeform DesiredLifeform);
float UTIL_GetEvolveLifeformCost(EAIAlienLifeform DesiredLifeform);

bool GetNearestMapLocationAtPoint(vec3_t SearchLocation, string& outLocation);


// Draws a white line between start and end for the given player (pEntity) for 0.1s
void UTIL_DrawLine(edict_t* pEntity, Vector start, Vector end);
// Draws a white line between start and end for the given player (pEntity) for given number of seconds
void UTIL_DrawLine(edict_t* pEntity, Vector start, Vector end, float drawTimeSeconds);
// Draws a coloured line using RGB input, between start and end for the given player (pEntity) for 0.1s
void UTIL_DrawLine(edict_t* pEntity, Vector start, Vector end, int r, int g, int b);
// Draws a coloured line using RGB input, between start and end for the given player (pEntity) for given number of seconds
void UTIL_DrawLine(edict_t* pEntity, Vector start, Vector end, float drawTimeSeconds, int r, int g, int b);

void UTIL_DrawBox(edict_t* pEntity, Vector bMin, Vector bMax, float drawTimeSeconds);
void UTIL_DrawBox(edict_t* pEntity, Vector bMin, Vector bMax, float drawTimeSeconds, int r, int g, int b);

void UTIL_DrawHUDText(edict_t* pEntity, char channel, float x, float y, unsigned char r, unsigned char g, unsigned char b, const char* string);

void UTIL_ClearLocalizations();
void UTIL_LocalizeText(const char* InputText, string& OutputText);

char* UTIL_TaskTypeToChar(const EAITaskType TaskType);

bool UTIL_IsPointInSwimArea(const Vector& TestPoint);

#endif