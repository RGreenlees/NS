/***
*
*	Copyright (c) 1999, Valve LLC. All rights reserved.
*	
*	This product contains software technology licensed from Id 
*	Software, Inc. ("Id Technology").  Id Technology (c) 1996 Id Software, Inc. 
*	All Rights Reserved.
*
*   Use, distribution, and modification of this source code and/or resulting
*   object code is restricted to non-commercial enhancements to products from
*   Valve LLC.  All other use, distribution, or modification is prohibited
*   without written permission from Valve LLC.
*
****/
//
// Health.cpp
//
// implementation of CHudHealth class
//

#include "stdio.h"
#include "stdlib.h"
#include "math.h"

#include "hud.h"
#include "cl_util.h"
#include <string.h>
#include "mod/AvHHudConstants.h"
#include "mod/AvHPlayerUpgrade.h"
#include "mod/AvHNetworkMessages.h"
#include "chudmisc.h"

DECLARE_MESSAGE(m_Health, Health )
DECLARE_MESSAGE(m_Health, Damage )

#define PAIN_NAME "sprites/%d_pain.spr"
#define DAMAGE_NAME "sprites/%d_dmg.spr"

int giDmgHeight, giDmgWidth;

int giDmgFlags[NUM_DMG_TYPES] = 
{
	DMG_POISON,
	DMG_ACID,
	DMG_FREEZE|DMG_SLOWFREEZE,
	DMG_DROWN,
	DMG_BURN|DMG_SLOWBURN,
	DMG_NERVEGAS, 
	DMG_RADIATION,
	DMG_SHOCK,
	DMG_CALTROP,
	DMG_TRANQ,
	DMG_CONCUSS,
	DMG_HALLUC
};

int CHudHealth::Init(void)
{
	HOOK_MESSAGE(Health);
	HOOK_MESSAGE(Damage);
	//HOOK_MESSAGE(ClCorpse);

	m_iHealth = 100;
	m_fFade = 0;
	m_iFlags = 0;
	m_bitsDamage = 0;
	m_fAttackFront = m_fAttackRear = m_fAttackRight = m_fAttackLeft = 0;
	giDmgHeight = 0;
	giDmgWidth = 0;
	m_fHealthScale = 1.0f;

	memset(m_dmg, 0, sizeof(DAMAGE_IMAGE) * NUM_DMG_TYPES);

	hud_health_x = CVAR_CREATE("hud_health_x", "0", FCVAR_ARCHIVE);
	hud_health_y = CVAR_CREATE("hud_health_y", "0", FCVAR_ARCHIVE);
	hud_health_scale = CVAR_CREATE("hud_health_scale", "1", FCVAR_ARCHIVE);
	hud_health_alphamin = CVAR_CREATE("hud_health_alphamin", "128", FCVAR_ARCHIVE);

	gHUD.AddHudElem(this);
	return 1;
}

void CHudHealth::Reset( void )
{
	// make sure the pain compass is cleared when the player respawns
	m_fAttackFront = m_fAttackRear = m_fAttackRight = m_fAttackLeft = 0;


	// force all the flashing damage icons to expire
	m_bitsDamage = 0;
	for ( int i = 0; i < NUM_DMG_TYPES; i++ )
	{
		m_dmg[i].fExpire = 0;
	}
}

int CHudHealth::VidInit(void)
{
	m_hSprite = 0;

	m_HUD_dmg_bio = gHUD.GetSpriteIndex( "dmg_bio" ) + 1;
	m_HUD_cross = gHUD.GetSpriteIndex( "cross" );

	giDmgHeight = gHUD.GetSpriteRect(m_HUD_dmg_bio).right - gHUD.GetSpriteRect(m_HUD_dmg_bio).left;
	giDmgWidth = gHUD.GetSpriteRect(m_HUD_dmg_bio).bottom - gHUD.GetSpriteRect(m_HUD_dmg_bio).top;
	return 1;
}

int CHudHealth:: MsgFunc_Health(const char *pszName,  int iSize, void *pbuf )
{
	int x;
	NetMsg_Health( pbuf, iSize, x );

	// TODO: update local health data
	m_iFlags |= HUD_ACTIVE;

	// Only update the fade if we've changed health
	if (x != m_iHealth)
	{
		// We're sent the health of the player we're observing as if it were our own
		m_fFade = FADE_TIME;
		m_iHealth = x;
	}

	return 2;
}


int CHudHealth:: MsgFunc_Damage(const char *pszName,  int iSize, void *pbuf )
{
	int armor, damageTaken;
	long bitsDamage;
	float origin[3];
	NetMsg_Damage( pbuf, iSize, armor, damageTaken, bitsDamage, origin );

	vec3_t vecFrom;

	for ( int i = 0 ; i < 3 ; i++)
		vecFrom[i] = origin[i];

	UpdateTiles(gHUD.m_flTime, bitsDamage);

	// Actually took damage?
	if ( damageTaken > 0 || armor > 0 )
		CalcDamageDirection(vecFrom);

	return 1;
}


// Returns back a color from the
// Green <-> Yellow <-> Red ramp
void CHudHealth::GetPainColor( int &r, int &g, int &b )
{
	int theMaxHealth = gHUD.GetHUDMaxHealth();

	if (m_iHealth > theMaxHealth/4)
	{
		gHUD.GetPrimaryHudColor(r, g, b);
	}
	else
	{
		r = 250;
		g = 0;
		b = 0;
	}
}


//// Old HUD drawing.
//int CHudHealth::Draw(float flTime)
//{
//	int r, g, b;
//	int a = 0, x, y;
//	int HealthWidth;
//
//	if ( /*!gHUD.GetIsAlive() ||*/ (gHUD.m_iHideHUDDisplay & HIDEHUD_HEALTH) /*|| gEngfuncs.IsSpectateOnly()*/)
//		return 1;
//
//	if (!m_hSprite)
//		m_hSprite = LoadSprite(PAIN_NAME);
//
//	// Has health changed? Flash the health #
//	if (m_fFade)
//	{
//		m_fFade -= (gHUD.m_flTimeDelta * 20);
//		if (m_fFade <= 0)
//		{
//			a = MIN_ALPHA;
//			m_fFade = 0;
//		}
//
//		// Fade the health number back to dim
//
//		a = MIN_ALPHA + (m_fFade / FADE_TIME) * 128;
//
//	}
//	else
//		a = MIN_ALPHA;
//
//	// Potentially se upgrades and health of spectator target
//	int theUpgrades = gHUD.GetHUDUpgrades();
//
//	// If health is getting low, make it bright red
//	int theMaxHealth = gHUD.GetHUDMaxHealth();
//	if (m_iHealth <= theMaxHealth / 10)
//	{
//		a = 255;
//	}
//
//	GetPainColor(r, g, b);
//	ScaleColors(r, g, b, a);
//
//	int theViewport[4];
//	gHUD.GetViewport(theViewport);
//
//	// Only draw health if we have the suit.
//	if (gHUD.m_iWeaponBits & (1 << (WEAPON_SUIT)))
//	{
//		HealthWidth = gHUD.GetSpriteRect(gHUD.m_HUD_number_0).right - gHUD.GetSpriteRect(gHUD.m_HUD_number_0).left;
//		int CrossWidth = gHUD.GetSpriteRect(m_HUD_cross).right - gHUD.GetSpriteRect(m_HUD_cross).left;
//
//		y = theViewport[1] + theViewport[3] - gHUD.m_iFontHeight - gHUD.m_iFontHeight / 2;
//
//		x = theViewport[0] + CrossWidth / 2;
//
//		int theInset = 0;
//		//if(gHUD.GetIsAlien() && !gHUD.GetIsCombatMode())
//		//{
//		//	theInset = ScreenWidth()*kResourceEnergyBarWidth;
//		//}
//		x += theInset;// + kHealthLeftInset*ScreenWidth;
//
//		SPR_Set(gHUD.GetSprite(m_HUD_cross), r, g, b);
//		SPR_DrawAdditive(0, x, y, &gHUD.GetSpriteRect(m_HUD_cross));
//
//		x += CrossWidth + HealthWidth / 2;
//
//		x = gHUD.DrawHudNumber(x, y, DHN_3DIGITS | DHN_DRAWZERO, m_iHealth, r, g, b);
//
//		x += HealthWidth / 2;
//
//		gHUD.GetPrimaryHudColor(r, g, b);
//
//		int iHeight = gHUD.m_iFontHeight;
//		int iWidth = HealthWidth / 10;
//		FillRGBA(x, y, iWidth, iHeight, r, g, b, a);
//	}
//
//	DrawDamage(flTime);
//	return DrawPain(flTime);
//}

int CHudHealth::Draw(float flTime)
{
	int r, g, b;
	int a = 0, x, y;
	int HealthWidth;

	if ( /*!gHUD.GetIsAlive() ||*/ (gHUD.m_iHideHUDDisplay & HIDEHUD_HEALTH) /*|| gEngfuncs.IsSpectateOnly()*/ )
		return 1;

	if ( !m_hSprite )
		m_hSprite = LoadSprite(PAIN_NAME);
	
	if (hud_health_alphamin)
		m_iMinAlpha = max(0, min(hud_health_alphamin->value, 255));
	else
		m_iMinAlpha = MIN_ALPHA;

	// Has health changed? Flash the health #
	if (m_fFade)
	{
		m_fFade -= (gHUD.m_flTimeDelta * 20);
		if (m_fFade <= 0)
		{
			m_fFade = 0;
		}
		// Fade the health number back to dim
		a = m_iMinAlpha +  (m_fFade/FADE_TIME) * 128;
	}
	else
		a = m_iMinAlpha;

	// Potentially se upgrades and health of spectator target
	int theUpgrades = gHUD.GetHUDUpgrades();

	// If health is getting low, make it bright red
	int theMaxHealth = gHUD.GetHUDMaxHealth();
	if (m_iHealth <= theMaxHealth/10)
	{
		a = 255;
	}
		
	GetPainColor( r, g, b );
	ScaleColors(r, g, b, a );

	// Only draw health if we have the suit.
	if (gHUD.m_iWeaponBits & (1<<(WEAPON_SUIT)))
	{
		if (hud_health_scale)
		{
			m_fHealthScale = min(max(0.01f, hud_health_scale->value), 5.0f);
		}

		HealthWidth = (gHUD.GetSpriteRect(gHUD.m_HUD_number_0).right - gHUD.GetSpriteRect(gHUD.m_HUD_number_0).left) * m_fHealthScale;
		int CrossWidth = (gHUD.GetSpriteRect(m_HUD_cross).right - gHUD.GetSpriteRect(m_HUD_cross).left) * m_fHealthScale;

		if (hud_health_x && hud_health_x->value != 0.0f)
		{
			x = min(max(0.0f, hud_health_x->value), 1.0f) * gHUD.GetWidth();
		}
		else
		{
			//TODO: Inset position for ultrawide monitors.
			//if (gHUD.GetWidth() > (gHUD.GetHeight() * 1.78f)){}

			x = CrossWidth / 2;
		}
		int initialY = gHUD.GetHeight();
		if (hud_health_y && hud_health_y->value != 0.0f)
		{
			initialY *= min(max(0.0f, hud_health_y->value), 1.0f);
		}

		y = initialY - gHUD.m_iFontHeight / 2;

		gHUD.DrawHudSpriteIndex(m_HUD_cross, x, y, r, g, b, 255, m_fHealthScale, CHud::a_southwest);

		x += CrossWidth + HealthWidth / 2;
		
		//Reserve space for 3 digits by default, but allow it to expand
		x += gHUD.GetHudNumberWidth(m_iHealth, 3, DHN_DRAWZERO, m_fHealthScale);

		gHUD.DrawHudNumberReverse(x, y, m_iHealth, DHN_DRAWZERO, r, g, b, 255, m_fHealthScale, CHud::a_southwest);

		x += HealthWidth /2;

		gHUD.GetPrimaryHudColor(r, g, b);

		int iHeight = gHUD.m_iFontHeight * -1;
		int iWidth = max(1, HealthWidth /10);

		gHUD.DrawHudFill(x, y, iWidth, iHeight, r ,g , b, a, m_fHealthScale);

		gHUD.m_Battery.m_iAnchorX = x + iWidth + HealthWidth / 2;
		gHUD.m_Battery.m_iAnchorY = initialY;
	}

	DrawDamage(flTime);
	return DrawPain(flTime);
}

void CHudHealth::CalcDamageDirection(vec3_t vecFrom)
{
	vec3_t	forward, right, up;
	float	side, front;
	vec3_t vecOrigin, vecAngles;

	if (!vecFrom[0] && !vecFrom[1] && !vecFrom[2])
	{
		m_fAttackFront = m_fAttackRear = m_fAttackRight = m_fAttackLeft = 0;
		return;
	}


	memcpy(vecOrigin, gHUD.m_vecOrigin, sizeof(vec3_t));
	memcpy(vecAngles, gHUD.m_vecAngles, sizeof(vec3_t));


	VectorSubtract (vecFrom, vecOrigin, vecFrom);

	float flDistToTarget = vecFrom.Length();

	vecFrom = vecFrom.Normalize();
	AngleVectors (vecAngles, forward, right, up);

	front = DotProduct (vecFrom, right);
	side = DotProduct (vecFrom, forward);

	if (flDistToTarget <= 50)
	{
		m_fAttackFront = m_fAttackRear = m_fAttackRight = m_fAttackLeft = 1;
	}
	else 
	{
		if (side > 0)
		{
			if (side > 0.3)
				m_fAttackFront = max(m_fAttackFront, side);
		}
		else
		{
			float f = fabs(side);
			if (f > 0.3)
				m_fAttackRear = max(m_fAttackRear, f);
		}

		if (front > 0)
		{
			if (front > 0.3)
				m_fAttackRight = max(m_fAttackRight, front);
		}
		else
		{
			float f = fabs(front);
			if (f > 0.3)
				m_fAttackLeft = max(m_fAttackLeft, f);
		}
	}
}

int CHudHealth::DrawPain(float flTime)
{
	if (!(m_fAttackFront || m_fAttackRear || m_fAttackLeft || m_fAttackRight))
		return 1;

	int r, g, b;
	int x, y, a, shade;

	// TODO:  get the shift value of the health
	a = 255;	// max brightness until then

	float fFade = gHUD.m_flTimeDelta * 2;
	
	// SPR_Draw top
	if (m_fAttackFront > 0.4)
	{
		GetPainColor(r,g,b);
		shade = a * max( m_fAttackFront, 0.5 );
		ScaleColors(r, g, b, shade);
		SPR_Set(m_hSprite, r, g, b );

		x = ScreenWidth()/2 - SPR_Width(m_hSprite, 0)/2;
		y = ScreenHeight()/2 - SPR_Height(m_hSprite,0) * 3;
		SPR_DrawAdditive(0, x, y, NULL);
		m_fAttackFront = max( 0, m_fAttackFront - fFade );
	} else
		m_fAttackFront = 0;

	if (m_fAttackRight > 0.4)
	{
		GetPainColor(r,g,b);
		shade = a * max( m_fAttackRight, 0.5 );
		ScaleColors(r, g, b, shade);
		SPR_Set(m_hSprite, r, g, b );

		x = ScreenWidth()/2 + SPR_Width(m_hSprite, 1) * 2;
		y = ScreenHeight()/2 - SPR_Height(m_hSprite,1)/2;
		SPR_DrawAdditive(1, x, y, NULL);
		m_fAttackRight = max( 0, m_fAttackRight - fFade );
	} else
		m_fAttackRight = 0;

	if (m_fAttackRear > 0.4)
	{
		GetPainColor(r,g,b);
		shade = a * max( m_fAttackRear, 0.5 );
		ScaleColors(r, g, b, shade);
		SPR_Set(m_hSprite, r, g, b );

		x = ScreenWidth()/2 - SPR_Width(m_hSprite, 2)/2;
		y = ScreenHeight()/2 + SPR_Height(m_hSprite,2) * 2;
		SPR_DrawAdditive(2, x, y, NULL);
		m_fAttackRear = max( 0, m_fAttackRear - fFade );
	} else
		m_fAttackRear = 0;

	if (m_fAttackLeft > 0.4)
	{
		GetPainColor(r,g,b);
		shade = a * max( m_fAttackLeft, 0.5 );
		ScaleColors(r, g, b, shade);
		SPR_Set(m_hSprite, r, g, b );

		x = ScreenWidth()/2 - SPR_Width(m_hSprite, 3) * 3;
		y = ScreenHeight()/2 - SPR_Height(m_hSprite,3)/2;
		SPR_DrawAdditive(3, x, y, NULL);

		m_fAttackLeft = max( 0, m_fAttackLeft - fFade );
	} else
		m_fAttackLeft = 0;

	return 1;
}

int CHudHealth::DrawDamage(float flTime)
{
	int r, g, b, a;
	DAMAGE_IMAGE *pdmg;

	if (!m_bitsDamage)
		return 1;

	gHUD.GetPrimaryHudColor(r, g, b);
	a = (int)( fabs(sin(flTime*2)) * 256.0);

	ScaleColors(r, g, b, a);

	// Draw all the items
	int i;
	for (i = 0; i < NUM_DMG_TYPES; i++)
	{
		if (m_bitsDamage & giDmgFlags[i])
		{
			pdmg = &m_dmg[i];
			SPR_Set(gHUD.GetSprite(m_HUD_dmg_bio + i), r, g, b );
			SPR_DrawAdditive(0, pdmg->x, pdmg->y, &gHUD.GetSpriteRect(m_HUD_dmg_bio + i));
		}
	}


	// check for bits that should be expired
	for ( i = 0; i < NUM_DMG_TYPES; i++ )
	{
		DAMAGE_IMAGE *pdmg = &m_dmg[i];

		if ( m_bitsDamage & giDmgFlags[i] )
		{
			pdmg->fExpire = min( flTime + DMG_IMAGE_LIFE, pdmg->fExpire );

			if ( pdmg->fExpire <= flTime		// when the time has expired
				&& a < 40 )						// and the flash is at the low point of the cycle
			{
				pdmg->fExpire = 0;

				int y = pdmg->y;
				pdmg->x = pdmg->y = 0;

				// move everyone above down
				for (int j = 0; j < NUM_DMG_TYPES; j++)
				{
					pdmg = &m_dmg[j];
					if ((pdmg->y) && (pdmg->y < y))
						pdmg->y += giDmgHeight;

				}

				m_bitsDamage &= ~giDmgFlags[i];  // clear the bits
			}
		}
	}

	return 1;
}
 

void CHudHealth::UpdateTiles(float flTime, long bitsDamage)
{	
	DAMAGE_IMAGE *pdmg;

	// Which types are new?
	long bitsOn = ~m_bitsDamage & bitsDamage;
	
	for (int i = 0; i < NUM_DMG_TYPES; i++)
	{
		pdmg = &m_dmg[i];

		// Is this one already on?
		if (m_bitsDamage & giDmgFlags[i])
		{
			pdmg->fExpire = flTime + DMG_IMAGE_LIFE; // extend the duration
			if (!pdmg->fBaseline)
				pdmg->fBaseline = flTime;
		}

		// Are we just turning it on?
		if (bitsOn & giDmgFlags[i])
		{
			// put this one at the bottom
			pdmg->x = giDmgWidth/8;
			pdmg->y = ScreenHeight() - giDmgHeight * 2;
			pdmg->fExpire=flTime + DMG_IMAGE_LIFE;
			
			// move everyone else up
			for (int j = 0; j < NUM_DMG_TYPES; j++)
			{
				if (j == i)
					continue;

				pdmg = &m_dmg[j];
				if (pdmg->y)
					pdmg->y -= giDmgHeight;

			}
			pdmg = &m_dmg[i];
		}	
	}	

	// damage bits are only turned on here;  they are turned off when the draw time has expired (in DrawDamage())
	m_bitsDamage |= bitsDamage;
}


void CreateCorpse ( Vector vOrigin, Vector vAngles, const char *pModel, float flAnimTime, int iSequence, int iBody );
