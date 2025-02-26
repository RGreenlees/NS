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
// battery.cpp
//
// implementation of CHudBattery class
//

#include "hud.h"
#include "cl_util.h"

#include <string.h>
#include <stdio.h>
#include "AvHPlayerUpgrade.h"
#include "AvHHudConstants.h"
#include "AvHNetworkMessages.h"

DECLARE_MESSAGE(m_Battery, Battery)

int CHudBattery::Init(void)
{
	m_iBat = 0;
	m_fFade = 0;
	m_iFlags = 0;

	HOOK_MESSAGE(Battery);

	gHUD.AddHudElem(this);

	return 1;
};


int CHudBattery::VidInit(void)
{
	int HUD_suit_empty = gHUD.GetSpriteIndex( "suit_empty" );
	int HUD_suit_full = gHUD.GetSpriteIndex( "suit_full" );

	m_hSprite1 = m_hSprite2 = 0;  // delaying get sprite handles until we know the sprites are loaded
	m_prc1 = &gHUD.GetSpriteRect( HUD_suit_empty );
	m_prc2 = &gHUD.GetSpriteRect( HUD_suit_full );
	m_iHeight = m_prc2->bottom - m_prc1->top;
	m_fFade = 0;
	return 1;
};

int CHudBattery:: MsgFunc_Battery(const char *pszName,  int iSize, void *pbuf )
{
	m_iFlags |= HUD_ACTIVE;

	
	int x;
	NetMsg_Battery( pbuf, iSize, x );

	if (x != m_iBat)
	{
		// We're sent the health of the player we're observing as if it were our own
		m_fFade = FADE_TIME;
		m_iBat = x;
	}

	return 1;
}


int CHudBattery::Draw(float flTime)
{
	
    if ( gHUD.m_iHideHUDDisplay & HIDEHUD_HEALTH )
		return 1;

	int r, g, b, x, y, a;
	wrect_t rc;
	
	rc = *m_prc2;
	
	int theMaxArmor = gHUD.GetHUDMaxArmor();
	float theScalar = 1.0f/theMaxArmor;
	
	rc.top  += m_iHeight * ((float)(theMaxArmor-(min(theMaxArmor, m_iBat))) * theScalar);	// battery can go from 0 to 100 so * 0.01 goes from 0 to 1
	
	gHUD.GetPrimaryHudColor(r, g, b);
	
	if (!(gHUD.m_iWeaponBits & (1<<(WEAPON_SUIT)) ))
		return 1;

	// Has health changed? Flash the health #
	if (m_fFade)
	{
		if (m_fFade > FADE_TIME)
			m_fFade = FADE_TIME;
		
		m_fFade -= (gHUD.m_flTimeDelta * 20);
		if (m_fFade <= 0)
		{
			//a = 128;
			m_fFade = 0;
		}
		
		// Fade the health number back to dim
		
		a = gHUD.m_Health.m_iMinAlpha +  (m_fFade/FADE_TIME) * 128;
		
	}
	else
		a = gHUD.m_Health.m_iMinAlpha;
	
	ScaleColors(r, g, b, a );
	
	//int iOffset = (m_prc1->bottom - m_prc1->top)/6;

	//int theInset = 0;
	//if(gHUD.GetIsAlien())
	//{
	//	theInset = ScreenWidth()*kResourceEnergyBarWidth;
	//}

    int theViewport[4];
    gHUD.GetViewport(theViewport);

	//y = theViewport[1] + theViewport[3] - gHUD.m_iFontHeight - gHUD.m_iFontHeight / 2;
	//x = theViewport[0] + theInset + kArmorLeftInset*ScreenWidth();
	//y = m_iAnchorY - gHUD.m_iFontHeight - gHUD.m_iFontHeight / 2;
	y = m_iAnchorY - gHUD.m_iFontHeight / 2;
	x = m_iAnchorX;

	// make sure we have the right sprite handles
	if ( !m_hSprite1 )
		m_hSprite1 = gHUD.GetSprite( gHUD.GetSpriteIndex( "suit_empty" ) );
	if ( !m_hSprite2 )
		m_hSprite2 = gHUD.GetSprite( gHUD.GetSpriteIndex( "suit_full" ) );

	//SPR_Set(m_hSprite1, r, g, b );
	//SPR_DrawAdditive( 0,  x, y - iOffset, m_prc1);
	gHUD.DrawHudSprite(m_hSprite1, 0, m_prc1, x, y, r, g, b, a, gHUD.m_Health.m_fHealthScale, CHud::a_southwest);

	if (rc.bottom > rc.top)
	{
		//SPR_Set(m_hSprite2, r, g, b );
		//SPR_DrawAdditive( 0, x, y - iOffset + (rc.top - m_prc2->top), &rc);
		gHUD.DrawHudSprite(m_hSprite2, 0, &rc, x, y, r, g, b, a, gHUD.m_Health.m_fHealthScale, CHud::a_southwest);
	}

	x += (m_prc1->right - m_prc1->left) * gHUD.m_Health.m_fHealthScale;
	x += ((gHUD.GetSpriteRect(gHUD.m_HUD_number_0).right - gHUD.GetSpriteRect(gHUD.m_HUD_number_0).left) / 2) * gHUD.m_Health.m_fHealthScale;
	x = gHUD.DrawHudNumber(x, y, DHN_3DIGITS | DHN_DRAWZERO, m_iBat, r, g, b, 255, gHUD.m_Health.m_fHealthScale, CHud::a_southwest);

	return 1;
    
}

//// Old HUD drawing.
//int CHudBattery::Draw(float flTime)
//{
//
//	if (gHUD.m_iHideHUDDisplay & HIDEHUD_HEALTH)
//		return 1;
//
//	int r, g, b, x, y, a;
//	wrect_t rc;
//
//	rc = *m_prc2;
//
//	int theMaxArmor = gHUD.GetHUDMaxArmor();
//	float theScalar = 1.0f / theMaxArmor;
//
//	rc.top += m_iHeight * ((float)(theMaxArmor - (min(theMaxArmor, m_iBat))) * theScalar);	// battery can go from 0 to 100 so * 0.01 goes from 0 to 1
//
//	gHUD.GetPrimaryHudColor(r, g, b);
//
//	if (!(gHUD.m_iWeaponBits & (1 << (WEAPON_SUIT))))
//		return 1;
//
//	// Has health changed? Flash the health #
//	if (m_fFade)
//	{
//		if (m_fFade > FADE_TIME)
//			m_fFade = FADE_TIME;
//
//		m_fFade -= (gHUD.m_flTimeDelta * 20);
//		if (m_fFade <= 0)
//		{
//			a = 128;
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
//	ScaleColors(r, g, b, a);
//
//	int iOffset = (m_prc1->bottom - m_prc1->top) / 6;
//
//	int theInset = 0;
//	//if(gHUD.GetIsAlien())
//	//{
//	//	theInset = ScreenWidth()*kResourceEnergyBarWidth;
//	//}
//
//	int theViewport[4];
//	gHUD.GetViewport(theViewport);
//
//	y = theViewport[1] + theViewport[3] - gHUD.m_iFontHeight - gHUD.m_iFontHeight / 2;
//	x = theViewport[0] + theInset + kArmorLeftInset * ScreenWidth();
//
//	// make sure we have the right sprite handles
//	if (!m_hSprite1)
//		m_hSprite1 = gHUD.GetSprite(gHUD.GetSpriteIndex("suit_empty"));
//	if (!m_hSprite2)
//		m_hSprite2 = gHUD.GetSprite(gHUD.GetSpriteIndex("suit_full"));
//
//	SPR_Set(m_hSprite1, r, g, b);
//	SPR_DrawAdditive(0, x, y - iOffset, m_prc1);
//
//	if (rc.bottom > rc.top)
//	{
//		SPR_Set(m_hSprite2, r, g, b);
//		SPR_DrawAdditive(0, x, y - iOffset + (rc.top - m_prc2->top), &rc);
//	}
//
//	x += (m_prc1->right - m_prc1->left);
//	x = gHUD.DrawHudNumber(x, y, DHN_3DIGITS | DHN_DRAWZERO, m_iBat, r, g, b);
//
//	return 1;
//
//}