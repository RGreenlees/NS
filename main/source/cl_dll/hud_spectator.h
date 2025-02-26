//========= Copyright � 1996-2002, Valve LLC, All rights reserved. ============
//
// Purpose: 
//
// $NoKeywords: $
//=============================================================================

#ifndef SPECTATOR_H
#define SPECTATOR_H
#pragma once

#include "cl_entity.h"

// Removed by mmcguire.
/*
#define INSET_OFF				0
#define	INSET_CHASE_LOCKED		1
#define	INSET_IN_EYE			2
*/

#define MAX_SPEC_HUD_MESSAGES	8



#define OVERVIEW_TILE_SIZE		128		// don't change this
#define OVERVIEW_MAX_LAYERS		1

//-----------------------------------------------------------------------------
// Purpose: Handles the drawing of the spectator stuff (camera & top-down map and all the things on it )
//-----------------------------------------------------------------------------

typedef struct overviewInfo_s {
	char		map[64];	// cl.levelname or empty
	vec3_t		origin;		// center of map
	float		zoom;		// zoom of map images
	int			layers;		// how may layers do we have
	float		layersHeights[OVERVIEW_MAX_LAYERS];
	char		layersImages[OVERVIEW_MAX_LAYERS][255];
	qboolean	rotated;	// are map images rotated (90 degrees) ?
	
	int			insetWindowX;
	int			insetWindowY;
	int			insetWindowHeight;
	int			insetWindowWidth;
} overviewInfo_t;

typedef struct overviewEntity_s {

	AVHHSPRITE					hSprite;
	struct cl_entity_s *	entity;
	double					killTime;
	int						mFrame;
    int                     mRenderMode;
    float                   mColorR;
    float                   mColorG;
    float                   mColorB;
} overviewEntity_t;

#define	 MAX_OVERVIEW_ENTITIES		256
const int kOverviewEntityZHeight = 100;

class CHudSpectator : public CHudBase
{
public:
	void Reset();
	void CheckSettings();
	void InitHUDData( void );
	bool AddOverviewEntityToList(AVHHSPRITE sprite, cl_entity_t * ent, double killTime, int inFrame, int inRenderMode, float r, float g, float b);
	void DeathMessage(int victim);
	bool AddOverviewEntity( int type, struct cl_entity_s *ent, const char *modelname );
	void CheckOverviewEntities();
	void DrawOverview();
    void DrawOverviewEntities();
	void GetMapPosition( float * returnvec );
	void DrawOverviewLayer();
	void LoadMapSprites();
	bool ParseOverviewFile();
	bool IsActivePlayer(cl_entity_t * ent);
	void SetMode(int iMainMode);
	void HandleButtonsDown(int ButtonPressed);
	void HandleButtonsUp(int ButtonPressed);
	void FindNextPlayer( bool bReverse );
	void DirectorMessage( int iSize, void *pbuf );
	void SetSpectatorStartPosition();
	int Init();
	int VidInit();
    
    bool IsInOverviewMode() const;
    void SetOverviewMode(bool overviewMode);

	int Draw(float flTime);
	void DrawOverviewMap();

	int m_iDrawCycle;
	client_textmessage_t m_HUDMessages[MAX_SPEC_HUD_MESSAGES];
	char				m_HUDMessageText[MAX_SPEC_HUD_MESSAGES][128];
	int					m_lastHudMessage;
	overviewInfo_t		m_OverviewData;
	overviewEntity_t	m_OverviewEntities[MAX_OVERVIEW_ENTITIES];
	int					m_iObserverFlags;
	int					m_iSpectatorNumber;
	
	float				m_mapZoom;		// zoom the user currently uses
	vec3_t				m_mapOrigin;	// origin where user rotates around
	cvar_t *			m_drawnames;
	cvar_t *			m_drawcone;
	cvar_t *			m_drawstatus;
	cvar_t *			m_autoDirector;

    // Removed by mmcguire.
    bool                m_overviewMode;
    //cvar_t *			m_overview;
    //cvar_t *			m_pip;


	qboolean			m_chatEnabled;

	vec3_t				m_cameraOrigin;	// a help camera
	vec3_t				m_cameraAngles;	// and it's angles

private:
	vec3_t		m_vPlayerPos[MAX_PLAYERS];
	AVHHSPRITE	m_hsprPlayerMarine;
	AVHHSPRITE	m_hsprPlayerAlien;
	AVHHSPRITE	m_hsprCamera;
	AVHHSPRITE	m_hsprPlayerDead;
	AVHHSPRITE	m_hsprViewcone;
	AVHHSPRITE	m_hsprUnkownMap;
	AVHHSPRITE	m_hsprBeam;
	AVHHSPRITE	m_hCrosshair;
	AVHHSPRITE	m_hsprWhite;

	wrect_t		m_crosshairRect;

	struct model_s * m_MapSprite;	// each layer image is saved in one sprite, where each tile is a sprite frame
	float		m_flNextObserverInput;
	float		m_zoomDelta;
	float		m_moveDelta;
	int			m_lastPrimaryObject;
	int			m_lastSecondaryObject;
};

#endif // SPECTATOR_H
