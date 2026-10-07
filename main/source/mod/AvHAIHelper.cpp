#include "AvHAIHelper.h"
#include "AvHAIMath.h"
#include "AvHAIPlayerUtil.h"
#include "AvHAITactical.h"
#include "AvHAINavigation.h"
#include "AvHAIPlayerManager.h"

#include "AvHGamerules.h"

#include <unordered_map>
#include "AvHSharedUtil.h"

int m_spriteTexture;

std::unordered_map<const char*, std::string> LocalizedLocationsMap;

bool UTIL_QuickTrace(const edict_t* pEdict, const Vector& start, const Vector& end, bool bAllowStartSolid)
{
	TraceResult hit;
	edict_t* IgnoreEdict = (!FNullEnt(pEdict)) ? pEdict->v.pContainingEntity : NULL;
	UTIL_TraceLine(start, end, ignore_monsters, ignore_glass, IgnoreEdict, &hit);
	return (hit.flFraction >= 1.0f && hit.fAllSolid == 0 && (bAllowStartSolid || hit.fStartSolid == 0));
}

bool UTIL_QuickHullTrace(const edict_t* pEdict, const Vector& start, const Vector& end, bool bAllowStartSolid)
{
	int hullNum = (!FNullEnt(pEdict)) ? GetPlayerHullIndex(pEdict) : point_hull;
	edict_t* IgnoreEdict = (!FNullEnt(pEdict)) ? pEdict->v.pContainingEntity : NULL;
	TraceResult hit;
	UTIL_TraceHull(start, end, ignore_monsters, hullNum, IgnoreEdict, &hit);

	return (hit.flFraction >= 1.0f && hit.fAllSolid == 0 && (bAllowStartSolid || hit.fStartSolid == 0));
}

bool UTIL_QuickHullTrace(const edict_t* pEdict, const Vector& start, const Vector& end, int hullNum, bool bAllowStartSolid)
{
	TraceResult hit;
	edict_t* IgnoreEdict = (!FNullEnt(pEdict)) ? pEdict->v.pContainingEntity : NULL;
	UTIL_TraceHull(start, end, ignore_monsters, hullNum, IgnoreEdict, &hit);

	return (hit.flFraction >= 1.0f && hit.fAllSolid == 0 && (bAllowStartSolid || hit.fStartSolid == 0));
}

edict_t* UTIL_TraceEntity(const edict_t* pEdict, const Vector& start, const Vector& end)
{
	TraceResult hit;
	edict_t* IgnoreEdict = (!FNullEnt(pEdict)) ? pEdict->v.pContainingEntity : NULL;
	UTIL_TraceLine(start, end, dont_ignore_monsters, dont_ignore_glass, IgnoreEdict, &hit);
	return hit.pHit;
}

edict_t* UTIL_TraceEntityHull(const edict_t* pEdict, const Vector& start, const Vector& end)
{
	TraceResult hit;
	edict_t* IgnoreEdict = (!FNullEnt(pEdict)) ? pEdict->v.pContainingEntity : NULL;
	UTIL_TraceHull(start, end, dont_ignore_monsters, head_hull, IgnoreEdict, &hit);
	return hit.pHit;
}

Vector UTIL_GetTraceHitLocation(const Vector Start, const Vector End)
{
	TraceResult hit;
	UTIL_TraceHull(Start, End, ignore_monsters, point_hull, NULL, &hit);

	if (hit.flFraction < 1.0f && !hit.fAllSolid)
	{
		return hit.vecEndPos;
	}

	return Start;
}

bool UTIL_IsEdictActive(const edict_t* Edict)
{
	return (!FNullEnt(Edict) && !Edict->free && Edict->v.deadflag == DEAD_NO);
}

Vector UTIL_GetHullTraceHitLocation(const Vector Start, const Vector End, int HullNum)
{
	TraceResult hit;
	UTIL_TraceHull(Start, End, ignore_monsters, HullNum, NULL, &hit);

	if (hit.flFraction < 1.0f && !hit.fAllSolid)
	{
		return hit.vecEndPos;
	}

	return Start;
}

Vector UTIL_GetCentreOfEntity(const edict_t* Entity)
{
	if (!Entity) { return g_vecZero; }
	return (Entity->v.absmin + (Entity->v.size * 0.5f));
}

Vector UTIL_GetFloorUnderEntity(const edict_t* Edict)
{
	if (FNullEnt(Edict)) { return g_vecZero; }

	return UTIL_FindFloor(Edict->v.origin);
}

Vector UTIL_FindFloor(const Vector& CheckLocation, const edict_t* IgnoreEntity)
{
	TraceResult hit;

	Vector TraceStart = CheckLocation + Vector(0.0f, 0.0f, 1.0f);
	Vector TraceEnd = (TraceStart - Vector(0.0f, 0.0f, 1000.0f));

	UTIL_TraceHull(TraceStart, TraceEnd, ignore_monsters, head_hull, (IgnoreEntity) ? IgnoreEntity->v.pContainingEntity : nullptr, &hit);

	while (hit.flFraction < 1.0f)
	{
		if (IsEdictPlayer(hit.pHit) || AITAC_IsEdictStructure(hit.pHit) || IsEdictHive(hit.pHit))
		{
			TraceStart.z = hit.pHit->v.origin.z;
			TraceEnd = (TraceStart - Vector(0.0f, 0.0f, 1000.0f));
			UTIL_TraceHull(TraceStart, TraceEnd, ignore_monsters, head_hull, hit.pHit->v.pContainingEntity, &hit);
		}
		else
		{
			return (hit.vecEndPos + Vector(0.0f, 0.0f, 1.0f));
		}
	}

	return CheckLocation;
}

string UTIL_GetLocationName(Vector Location)
{
	string Result;

	string theLocationName;
	if (AvHSHUGetNameOfLocation(GetGameRules()->GetInfoLocations(), Location, theLocationName))
	{
		UTIL_LocalizeText(theLocationName.c_str(), theLocationName);
		Result = theLocationName;
	}

	return Result;
}

Vector UTIL_GetClosestPointOnEntityToLocation(const Vector Location, const edict_t* Entity)
{
	return Vector(clampf(Location.x, Entity->v.absmin.x, Entity->v.absmax.x), clampf(Location.y, Entity->v.absmin.y, Entity->v.absmax.y), clampf(Location.z, Entity->v.absmin.z, Entity->v.absmax.z));
}

Vector UTIL_GetClosestPointOnEntityToLocation(const Vector Location, const edict_t* Entity, const Vector EntityLocation)
{
	Vector MinVec = EntityLocation - (Entity->v.size * 0.5f);
	Vector MaxVec = EntityLocation + (Entity->v.size * 0.5f);

	return Vector(clampf(Location.x, MinVec.x, MaxVec.x), clampf(Location.y, MinVec.y, MaxVec.y), clampf(Location.z, MinVec.z, MaxVec.z));
}

bool IsEdictHive(const edict_t* edict)
{
	if (FNullEnt(edict)) { return false; }
	return (edict->v.iuser3 == AVH_USER3_HIVE);
}

bool GetNearestMapLocationAtPoint(vec3_t SearchLocation, string& outLocation)
{
	bool theSuccess = false;

	const AvHBaseInfoLocationListType& inLocations = GetGameRules()->GetInfoLocations();

	bool bFoundNearest = false;
	float MinDist = 0.0f;

	// Look at our current position, and see if we lie within of the map locations
	for (AvHBaseInfoLocationListType::const_iterator theIter = inLocations.begin(); theIter != inLocations.end(); theIter++)
	{
		if (theIter->GetIsPointInRegion(SearchLocation))
		{
			outLocation = theIter->GetLocationName();
			return true;
		}

		float NearestX = clampf(SearchLocation.x, theIter->GetMinExtent().x, theIter->GetMaxExtent().x);
		float NearestY = clampf(SearchLocation.y, theIter->GetMinExtent().y, theIter->GetMaxExtent().y);

		float ThisDist = vDist2DSq(SearchLocation, Vector(NearestX, NearestY, 0.0f));

		if (!bFoundNearest || ThisDist < MinDist)
		{
			outLocation = theIter->GetLocationName();
			bFoundNearest = true;
			theSuccess = true;
			MinDist = ThisDist;
		}
	}

	return theSuccess;
}

bool UTIL_IsPointInSwimArea(const Vector& TestPoint)
{
	return UTIL_PointContents(TestPoint) == CONTENTS_WATER
		|| UTIL_PointContents(TestPoint) == CONTENTS_SLIME
		|| UTIL_PointContents(TestPoint) == CONTENTS_LAVA;
}

bool UTIL_IsCloakedPlayerInvisible(const edict_t* Observer, const AvHPlayer* Player)
{
	if (Player->GetOpacity() > 0.6f) { return false; }

	if (Player->GetIsCloaked()) { return true; }

	switch (Player->GetUser3())
	{
	case AVH_USER3_ALIEN_PLAYER1:
	case AVH_USER3_ALIEN_PLAYER2:
	case AVH_USER3_ALIEN_PLAYER3:
	{
		if (Player->GetOpacity() < 0.3f) { return true; }

		return (vDist3DSq(Observer->v.origin, Player->pev->origin) > sqrf(UTIL_MetresToGoldSrcUnits(10.0f)) || Player->pev->velocity.Length2D() < 50.0f);
	}
	case AVH_USER3_ALIEN_PLAYER4:
	case AVH_USER3_ALIEN_PLAYER5:
	{
		if (Player->GetOpacity() > 0.4f) { return false; }
		if (Player->GetOpacity() < 0.2f) { return true; }

		return vDist3DSq(Observer->v.origin, Player->pev->origin) > sqrf(UTIL_MetresToGoldSrcUnits(10.0f));
	}
	}

	return false;
}

AvHMessageID UTIL_GetEvolveUpgradeImpulse(EAIAlienUpgrade DesiredUpgrade)
{
	switch (DesiredUpgrade)
	{
		case EAIAlienUpgrade::ALIEN_UPGRADE_CARAPACE:
			return ALIEN_EVOLUTION_ONE;
		case EAIAlienUpgrade::ALIEN_UPGRADE_REGENERATION:
			return ALIEN_EVOLUTION_TWO;
		case EAIAlienUpgrade::ALIEN_UPGRADE_REDEMPTION:
			return ALIEN_EVOLUTION_THREE;
		case EAIAlienUpgrade::ALIEN_UPGRADE_ADRENALINE:
			return ALIEN_EVOLUTION_EIGHT;
		case EAIAlienUpgrade::ALIEN_UPGRADE_CELERITY:
			return ALIEN_EVOLUTION_SEVEN;
		case EAIAlienUpgrade::ALIEN_UPGRADE_SILENCE:
			return ALIEN_EVOLUTION_NINE;
		case EAIAlienUpgrade::ALIEN_UPGRADE_FOCUS:
			return ALIEN_EVOLUTION_ELEVEN;
		case EAIAlienUpgrade::ALIEN_UPGRADE_SCENTOFFEAR:
			return ALIEN_EVOLUTION_TWELVE;
		case EAIAlienUpgrade::ALIEN_UPGRADE_CLOAK:
			return ALIEN_EVOLUTION_TEN;
		default:
			return MESSAGE_NULL;
	}
}

AvHMessageID UTIL_GetEvolveLifeformImpulse(EAIAlienLifeform DesiredLifeform)
{
	switch (DesiredLifeform)
	{
		case EAIAlienLifeform::ALIEN_LIFEFORM_SKULK:
			return ALIEN_LIFEFORM_ONE;
		case EAIAlienLifeform::ALIEN_LIFEFORM_GORGE:
			return ALIEN_LIFEFORM_TWO;
		case EAIAlienLifeform::ALIEN_LIFEFORM_LERK:
			return ALIEN_LIFEFORM_THREE;
		case EAIAlienLifeform::ALIEN_LIFEFORM_FADE:
			return ALIEN_LIFEFORM_FOUR;
		case EAIAlienLifeform::ALIEN_LIFEFORM_ONOS:
			return ALIEN_LIFEFORM_FIVE;
		default:
			return MESSAGE_NULL;
	}
}

float UTIL_GetEvolveLifeformCost(EAIAlienLifeform DesiredLifeform)
{
	switch (DesiredLifeform)
	{
		case EAIAlienLifeform::ALIEN_LIFEFORM_SKULK:
			return 0.0f;
		case EAIAlienLifeform::ALIEN_LIFEFORM_GORGE:
			return BALANCE_VAR(kGorgeCost);
		case EAIAlienLifeform::ALIEN_LIFEFORM_LERK:
			return BALANCE_VAR(kLerkCost);
		case EAIAlienLifeform::ALIEN_LIFEFORM_FADE:
			return BALANCE_VAR(kFadeCost);
		case EAIAlienLifeform::ALIEN_LIFEFORM_ONOS:
			return BALANCE_VAR(kOnosCost);
		default:
			return MESSAGE_NULL;
	}
}

AvHMessageID UTIL_GetVoicelineId(EAIVoiceLine RequiredVoiceLine)
{
	switch (RequiredVoiceLine)
	{
		case EAIVoiceLine::AI_MARINE_VOICELINE_NEEDHEALTH:
		case EAIVoiceLine::AI_ALIEN_VOICELINE_HEALME:
			return SAYING_4;
		case EAIVoiceLine::AI_MARINE_VOICELINE_NEEDAMMO:
			return SAYING_5;
		case EAIVoiceLine::AI_MARINE_VOICELINE_WELDME:
			return SAYING_8;
		case EAIVoiceLine::AI_MARINE_VOICELINE_TAUNT:
			return SAYING_5;
		case EAIVoiceLine::AI_MARINE_VOICELINE_NEEDORDER:
			return ORDER_REQUEST;
		case EAIVoiceLine::AI_MARINE_VOICELINE_ACKORDER:
			return ORDER_ACK;
		default:
			return MESSAGE_NULL;
	}
}

void AIDEBUG_DrawPath(edict_t* OutputPlayer, const AvHAIPath* Path, float DrawTime)
{
	if (!Path || !Path->IsValidPath()) { return; }

	for (auto it = Path->PathNodes.begin(); it != Path->PathNodes.end(); it++)
	{
		const AvHAIPathNode* PathNode = &(*it);

		if (!PathNode || !PathNode->IsValidMove()) { continue; }

		Vector FromLoc = PathNode->FromLocation;
		Vector ToLoc = PathNode->ToLocation;

		unsigned char r;
		unsigned char g;
		unsigned char b;

		GetDebugColorForFlag(PathNode->MovementFlag, r, g, b);

		UTIL_DrawLine(OutputPlayer, FromLoc, ToLoc, DrawTime, r, g, b);
	}
}

void UTIL_DrawLine(edict_t* pEntity, Vector start, Vector end)
{

	if (FNullEnt(pEntity) || pEntity->free)
	{
		MESSAGE_BEGIN(MSG_BROADCAST, SVC_TEMPENTITY);
	}
	else
	{
		MESSAGE_BEGIN(MSG_ONE, SVC_TEMPENTITY, NULL, pEntity);
	}

	WRITE_BYTE(TE_BEAMPOINTS);
	WRITE_COORD(start.x);
	WRITE_COORD(start.y);
	WRITE_COORD(start.z);
	WRITE_COORD(end.x);
	WRITE_COORD(end.y);
	WRITE_COORD(end.z);
	WRITE_SHORT(m_spriteTexture);
	WRITE_BYTE(1);               // framestart
	WRITE_BYTE(10);              // framerate
	WRITE_BYTE(1);              // life in 0.1's
	WRITE_BYTE(5);           // width
	WRITE_BYTE(0);           // noise

	WRITE_BYTE(255);             // r, g, b
	WRITE_BYTE(255);           // r, g, b
	WRITE_BYTE(255);            // r, g, b

	WRITE_BYTE(250);      // brightness
	WRITE_BYTE(5);           // speed
	MESSAGE_END();
}

void UTIL_DrawLine(edict_t* pEntity, Vector start, Vector end, float drawTimeSeconds)
{
	int timeTenthSeconds = (int)floorf(drawTimeSeconds * 10.0f);
	timeTenthSeconds = fmaxf(timeTenthSeconds, 1);

	if (FNullEnt(pEntity) || pEntity->free)
	{
		MESSAGE_BEGIN(MSG_BROADCAST, SVC_TEMPENTITY);
	}
	else
	{
		MESSAGE_BEGIN(MSG_ONE, SVC_TEMPENTITY, NULL, pEntity);
	}

	WRITE_BYTE(TE_BEAMPOINTS);
	WRITE_COORD(start.x);
	WRITE_COORD(start.y);
	WRITE_COORD(start.z);
	WRITE_COORD(end.x);
	WRITE_COORD(end.y);
	WRITE_COORD(end.z);
	WRITE_SHORT(m_spriteTexture);
	WRITE_BYTE(1);               // framestart
	WRITE_BYTE(10);              // framerate
	WRITE_BYTE(timeTenthSeconds);              // life in 0.1's
	WRITE_BYTE(5);           // width
	WRITE_BYTE(0);           // noise

	WRITE_BYTE(255);             // r, g, b
	WRITE_BYTE(255);           // r, g, b
	WRITE_BYTE(255);            // r, g, b

	WRITE_BYTE(250);      // brightness
	WRITE_BYTE(5);           // speed
	MESSAGE_END();
}

void UTIL_DrawLine(edict_t* pEntity, Vector start, Vector end, float drawTimeSeconds, int r, int g, int b)
{
	int timeTenthSeconds = (int)ceilf(drawTimeSeconds * 10.0f);
	timeTenthSeconds = fmaxf(timeTenthSeconds, 1);

	if (FNullEnt(pEntity) || pEntity->free)
	{
		MESSAGE_BEGIN(MSG_BROADCAST, SVC_TEMPENTITY);
	}
	else
	{
		MESSAGE_BEGIN(MSG_ONE, SVC_TEMPENTITY, NULL, pEntity);
	}

	WRITE_BYTE(TE_BEAMPOINTS);
	WRITE_COORD(start.x);
	WRITE_COORD(start.y);
	WRITE_COORD(start.z);
	WRITE_COORD(end.x);
	WRITE_COORD(end.y);
	WRITE_COORD(end.z);
	WRITE_SHORT(m_spriteTexture);
	WRITE_BYTE(1);               // framestart
	WRITE_BYTE(10);              // framerate
	WRITE_BYTE(timeTenthSeconds);              // life in 0.1's
	WRITE_BYTE(5);           // width
	WRITE_BYTE(0);           // noise

	WRITE_BYTE(r);             // r, g, b
	WRITE_BYTE(g);           // r, g, b
	WRITE_BYTE(b);            // r, g, b

	WRITE_BYTE(250);      // brightness
	WRITE_BYTE(5);           // speed
	MESSAGE_END();
}

void UTIL_DrawLine(edict_t* pEntity, Vector start, Vector end, int r, int g, int b)
{
	if (FNullEnt(pEntity) || pEntity->free)
	{
		MESSAGE_BEGIN(MSG_BROADCAST, SVC_TEMPENTITY);
	}
	else
	{
		MESSAGE_BEGIN(MSG_ONE, SVC_TEMPENTITY, NULL, pEntity);
	}

	WRITE_BYTE(TE_BEAMPOINTS);
	WRITE_COORD(start.x);
	WRITE_COORD(start.y);
	WRITE_COORD(start.z);
	WRITE_COORD(end.x);
	WRITE_COORD(end.y);
	WRITE_COORD(end.z);
	WRITE_SHORT(m_spriteTexture);
	WRITE_BYTE(1);               // framestart
	WRITE_BYTE(10);              // framerate
	WRITE_BYTE(1);              // life in 0.1's
	WRITE_BYTE(5);           // width
	WRITE_BYTE(0);           // noise

	WRITE_BYTE(r);             // r, g, b
	WRITE_BYTE(g);           // r, g, b
	WRITE_BYTE(b);            // r, g, b

	WRITE_BYTE(250);      // brightness
	WRITE_BYTE(5);           // speed
	MESSAGE_END();
}

void UTIL_DrawBox(edict_t* pEntity, Vector bMin, Vector bMax, float drawTimeSeconds)
{
	Vector LowerBottomLeftCorner = bMin;
	Vector LowerTopLeftCorner = Vector(bMin.x, bMax.y, bMin.z);
	Vector LowerTopRightCorner = Vector(bMax.x, bMax.y, bMin.z);
	Vector LowerBottomRightCorner = Vector(bMax.x, bMin.y, bMin.z);

	Vector UpperBottomLeftCorner = Vector(bMin.x, bMin.y, bMax.z);
	Vector UpperTopLeftCorner = Vector(bMin.x, bMax.y, bMax.z);
	Vector UpperTopRightCorner = Vector(bMax.x, bMax.y, bMax.z);
	Vector UpperBottomRightCorner = Vector(bMax.x, bMin.y, bMax.z);


	UTIL_DrawLine(pEntity, LowerTopLeftCorner, LowerTopRightCorner, drawTimeSeconds, 255, 255, 255);
	UTIL_DrawLine(pEntity, LowerTopRightCorner, LowerBottomRightCorner, drawTimeSeconds, 255, 255, 255);
	UTIL_DrawLine(pEntity, LowerBottomRightCorner, LowerBottomLeftCorner, drawTimeSeconds, 255, 255, 255);

	UTIL_DrawLine(pEntity, UpperBottomLeftCorner, UpperTopLeftCorner, drawTimeSeconds, 255, 255, 255);
	UTIL_DrawLine(pEntity, UpperTopLeftCorner, UpperTopRightCorner, drawTimeSeconds, 255, 255, 255);
	UTIL_DrawLine(pEntity, UpperTopRightCorner, UpperBottomRightCorner, drawTimeSeconds, 255, 255, 255);
	UTIL_DrawLine(pEntity, UpperBottomRightCorner, UpperBottomLeftCorner, drawTimeSeconds, 255, 255, 255);

	UTIL_DrawLine(pEntity, LowerBottomLeftCorner, UpperBottomLeftCorner, drawTimeSeconds, 255, 255, 255);
	UTIL_DrawLine(pEntity, LowerTopLeftCorner, UpperTopLeftCorner, drawTimeSeconds, 255, 255, 255);
	UTIL_DrawLine(pEntity, LowerTopRightCorner, UpperTopRightCorner, drawTimeSeconds, 255, 255, 255);
	UTIL_DrawLine(pEntity, LowerBottomRightCorner, UpperBottomRightCorner, drawTimeSeconds, 255, 255, 255);
}

void UTIL_DrawBox(edict_t* pEntity, Vector bMin, Vector bMax, float drawTimeSeconds, int r, int g, int b)
{
	Vector LowerBottomLeftCorner = bMin;
	Vector LowerTopLeftCorner = Vector(bMin.x, bMax.y, bMin.z);
	Vector LowerTopRightCorner = Vector(bMax.x, bMax.y, bMin.z);
	Vector LowerBottomRightCorner = Vector(bMax.x, bMin.y, bMin.z);

	Vector UpperBottomLeftCorner = Vector(bMin.x, bMin.y, bMax.z);
	Vector UpperTopLeftCorner = Vector(bMin.x, bMax.y, bMax.z);
	Vector UpperTopRightCorner = Vector(bMax.x, bMax.y, bMax.z);
	Vector UpperBottomRightCorner = Vector(bMax.x, bMin.y, bMax.z);


	UTIL_DrawLine(pEntity, LowerTopLeftCorner, LowerTopRightCorner, drawTimeSeconds, r, g, b);
	UTIL_DrawLine(pEntity, LowerTopRightCorner, LowerBottomRightCorner, drawTimeSeconds, r, g, b);
	UTIL_DrawLine(pEntity, LowerBottomRightCorner, LowerBottomLeftCorner, drawTimeSeconds, r, g, b);

	UTIL_DrawLine(pEntity, UpperBottomLeftCorner, UpperTopLeftCorner, drawTimeSeconds, r, g, b);
	UTIL_DrawLine(pEntity, UpperTopLeftCorner, UpperTopRightCorner, drawTimeSeconds, r, g, b);
	UTIL_DrawLine(pEntity, UpperTopRightCorner, UpperBottomRightCorner, drawTimeSeconds, r, g, b);
	UTIL_DrawLine(pEntity, UpperBottomRightCorner, UpperBottomLeftCorner, drawTimeSeconds, r, g, b);

	UTIL_DrawLine(pEntity, LowerBottomLeftCorner, UpperBottomLeftCorner, drawTimeSeconds, r, g, b);
	UTIL_DrawLine(pEntity, LowerTopLeftCorner, UpperTopLeftCorner, drawTimeSeconds, r, g, b);
	UTIL_DrawLine(pEntity, LowerTopRightCorner, UpperTopRightCorner, drawTimeSeconds, r, g, b);
	UTIL_DrawLine(pEntity, LowerBottomRightCorner, UpperBottomRightCorner, drawTimeSeconds, r, g, b);
}

void UTIL_DrawHUDText(edict_t* pEntity, char channel, float x, float y, unsigned char r, unsigned char g, unsigned char b, const char* string)
{
	if (FNullEnt(pEntity)) { return; }

	float delta = AIMGR_GetFrameDelta();

	// TODO: Be able to turn these off as a preference
	hudtextparms_t	theTextParms;

	// Init text parms
	theTextParms.x = x;
	theTextParms.y = y;
	theTextParms.effect = 0;
	theTextParms.r1 = 240;
	theTextParms.g1 = 240;
	theTextParms.b1 = 240;
	theTextParms.a1 = 128;
	theTextParms.r2 = 240;
	theTextParms.g2 = 240;
	theTextParms.b2 = 240;
	theTextParms.a2 = 128;
	theTextParms.fadeinTime = .0f;
	theTextParms.fadeoutTime = .0f;
	theTextParms.holdTime = 0.2f;
	theTextParms.channel = channel;

	UTIL_HudMessage(CBaseEntity::Instance(pEntity), theTextParms, string);
}

void UTIL_ClearLocalizations()
{
	LocalizedLocationsMap.clear();
}

void UTIL_LocalizeText(const char* InputText, string& OutputText)
{
	// Don't localize empty strings
	if (!strcmp(InputText, ""))
	{
		OutputText = "";
	}

	char theInputString[1024];

	sprintf(theInputString, "%s", InputText);

	std::unordered_map<const char*, std::string>::const_iterator FoundLocalization = LocalizedLocationsMap.find(theInputString);

	if (FoundLocalization != LocalizedLocationsMap.end())
	{
		OutputText = FoundLocalization->second;
		return;
	}

	char filename[256];

	std::string localizedString(theInputString);

	string titlesPath = string(getModDirectory()) + "/titles.txt";
	strcpy(filename, titlesPath.c_str());

	std::ifstream cFile(filename);
	if (cFile.is_open())
	{
		std::string line;
		while (getline(cFile, line))
		{
			line.erase(std::remove_if(line.begin(), line.end(), ::isspace),
				line.end());
			if (line[0] == '/' || line.empty())
				continue;

			if (line.compare(theInputString) == 0)
			{
				getline(cFile, line);
				getline(cFile, localizedString);
				break;

			}
		}
	}

	char theOutputString[1024];

	sprintf(theOutputString, "%s", localizedString.c_str());

	string Delimiter = "Hive -";
	auto delimiterPos = localizedString.find(Delimiter);

	if (delimiterPos == std::string::npos)
	{
		Delimiter = "Hive Location -";
		delimiterPos = localizedString.find(Delimiter);
	}

	if (delimiterPos == std::string::npos)
	{
		Delimiter = "Hive Location  -";
		delimiterPos = localizedString.find("Hive Location  -");
	}

	if (delimiterPos != std::string::npos)
	{
		auto AreaName = localizedString.substr(delimiterPos + Delimiter.length());

		AreaName.erase(0, AreaName.find_first_not_of(" \r\n\t\v\f"));

		sprintf(theOutputString, "%s", AreaName.c_str());
	}

	OutputText = theOutputString;

	LocalizedLocationsMap[InputText] = OutputText;

}

char* UTIL_TaskTypeToChar(const EAITaskType TaskType)
{
	switch (TaskType)
	{
		case EAITaskType::TASK_ATTACK:
			return "Attack";
		case EAITaskType::TASK_BUILD:
			return "Build";
		case EAITaskType::TASK_CAP_RESNODE:
			return "Cap Res Node";
		case EAITaskType::TASK_COMMAND:
			return "Take Command";
		case EAITaskType::TASK_DEFEND:
			return "Defend Structure";
		case EAITaskType::TASK_EVOLVE:
			return "Evolve";
		case EAITaskType::TASK_GET_AMMO:
			return "Get Ammo Pack";
		case EAITaskType::TASK_GET_EQUIPMENT:
			return "Get Equipment";
		case EAITaskType::TASK_GET_HEALTH:
			return "Get Health Pack";
		case EAITaskType::TASK_GET_WEAPON:
			return "Get Weapon";
		case EAITaskType::TASK_GUARD:
			return "Guard";
		case EAITaskType::TASK_HEAL:
			return "Heal Target";
		case EAITaskType::TASK_MOVE:
			return "Move to Location";
		case EAITaskType::TASK_PLACE_MINE:
			return "Place Mine";
		case EAITaskType::TASK_REINFORCE_STRUCTURE:
			return "Reinforce Structure";
		case EAITaskType::TASK_RESUPPLY:
			return "Resupply";
		case EAITaskType::TASK_SECURE_HIVE:
			return "Secure Hive";
		case EAITaskType::TASK_TOUCH:
			return "Touch Trigger";
		case EAITaskType::TASK_WELD:
			return "Weld Target";
		case EAITaskType::TASK_ASSAULT_MARINE_BASE:
			return "Assault Marine Base";
		default:
			return "None";
	}

	return "None";
}
