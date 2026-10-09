#ifndef AVH_AI_PLAYER_H
#define AVH_AI_PLAYER_H

#include "AvHPlayer.h"
#include "AvHAIConstants.h"
#include "AvHAINavConstants.h"
#include "AvHAIMath.h"
#include "AvHAITactical.h"
#include "AvHAINavigation.h"

// These define the bot's view frustum sides
#define FRUSTUM_PLANE_TOP 0
#define FRUSTUM_PLANE_BOTTOM 1
#define FRUSTUM_PLANE_LEFT 2
#define FRUSTUM_PLANE_RIGHT 3
#define FRUSTUM_PLANE_NEAR 4
#define FRUSTUM_PLANE_FAR 5

static const float BOT_FOV = 100.0f;  // Bot's field of view;
static const float BOT_MAX_VIEW = 9999.0f; // Bot's maximum view distance;
static const float BOT_MIN_VIEW = 5.0f; // Bot's minimum view distance;
static const float BOT_ASPECT_RATIO = 1.77778f; // Bot's view aspect ratio, 1.333333 for 4:3, 1.777778 for 16:9, 1.6 for 16:10;

static const float f_fnheight = 2.0f * tan((BOT_FOV * 0.0174532925f) * 0.5f) * BOT_MIN_VIEW;
static const float f_fnwidth = f_fnheight * BOT_ASPECT_RATIO;

static const float f_ffheight = 2.0f * tan((BOT_FOV * 0.0174532925f) * 0.5f) * BOT_MAX_VIEW;
static const float f_ffwidth = f_ffheight * BOT_ASPECT_RATIO;

enum class EAICombatStrategy
{
	COMBAT_STRATEGY_IGNORE = 0, // Don't engage this enemy
	COMBAT_STRATEGY_AMBUSH,		// Set up an ambush for this enemy
	COMBAT_STRATEGY_RETREAT,	// Retreat and find health
	COMBAT_STRATEGY_SKIRMISH,	// Maintain distance, whittle down their health from range and generally be a pain the arse
	COMBAT_STRATEGY_ATTACK		// Attack the enemy
};

struct AvHAIMovementInput
{
	float			ForwardMove = 0.0f;
	float			SideMove = 0.0f;
	float			UpMove = 0.0f;
	int				Button = 0;
	int				Impulse = 0;
	Vector			RequiredLookLocation = g_vecZero; // Where the bot MUST look to complete this movement (e.g. look up on ladder)
	Vector			DesiredLookLocation = g_vecZero;  // Where the bot might want to look if they're not doing a precise movement (e.g. an enemy target)
	EAIWeaponId		RequiredWeapon = EAIWeaponId::WEAPON_INVALID; // Which weapon the bot MUST switch to for movement purposes (e.g. leap/blink)
	EAIWeaponId		DesiredWeapon = EAIWeaponId::WEAPON_INVALID; // Which weapon the bot desires to use (e.g. for combat)
	Vector			DesiredMoveDirection = g_vecZero;
	Vector			VelocityOverride = g_vecZero; // Used to force a bot's velocity to a particular direction/magnitude for "cheating" moves
	bool			bHasAttemptedJump = false;
	bool			bShouldWalk = false;
	bool			bShouldCrouch = false;

	void Clear()
	{
		ForwardMove = 0.0f;
		SideMove = 0.0f;
		UpMove = 0.0f;
		Button = 0;
		Impulse = 0;
		RequiredLookLocation = g_vecZero;
		DesiredLookLocation = g_vecZero;
		RequiredWeapon = EAIWeaponId::WEAPON_INVALID;
		DesiredWeapon = EAIWeaponId::WEAPON_INVALID;
		DesiredMoveDirection = g_vecZero;
		VelocityOverride = g_vecZero;
		bHasAttemptedJump = false;
		bShouldWalk = false;
		bShouldCrouch = false;
	}

	void ClearMovementOutputs()
	{
		ForwardMove = 0.0f;
		SideMove = 0.0f;
		UpMove = 0.0f;
	}

	void GenerateMovementOutputs(const Vector& CurrentViewAngles, float MaxSpeed);
};


struct AvHAIViewInfo
{
	Vector InterpolatingViewTarget = g_vecZero;
	Vector CurrentInterpolatedView = g_vecZero;
	Vector LookTargetLocation = g_vecZero; // This is the bot's current desired look target. Could be an enemy (see LookTarget), or point of interest
	Vector MoveLookLocation = g_vecZero; // If the bot has to look somewhere specific for movement (e.g. up for a ladder or wall-climb), this will override LookTargetLocation so the bot doesn't get distracted and mess the move up
	bool bSnapView = false; // Use for rapid, precise snapping of the bot's view to the target. Useful if the bot requires more precise view angles for movement or other reasons
	float LastTargetTrackUpdate = 0.0f; // Add a delay to how frequently a bot can track a target's movements
	float ViewInterpolationSpeed = 0.0f; // How fast should the bot turn its view for this interpolated movement? Depends on distance to turn
	float ViewInterpStartedTime = 0.0f; // Used for interpolation

	float ViewUpdateRate = 0.2f; // How frequently the bot can react to new sightings of enemies etc.
	float LastViewUpdateTime = 0.0f; // Used to throttle view updates based on ViewUpdateRate

	AvHBotViewFrustumPlane ViewFrustumPlanes[6]; // Bot's view frustum. Essentially, their "screen" for determining visibility of stuff
};

// Pending message a bot wants to say. Allows for a delay in sending a message to simulate typing, or prevent too many messages on the same frame
struct AvHAIBotMsg
{
	char Message[64]; // Message to send
	float SendTime = 0.0f; // When the bot should send this message
	bool bIsPending = false; // Represents a valid pending message
	bool bIsTeamSay = false; // Is this a team-only message?
};
typedef std::vector<AvHAIBotMsg> AvHAIPendingMessageList;

struct AvHAIGuardInfo
{
	Vector GuardLocation = g_vecZero; // What position are we guarding?
	Vector GuardStandPosition = g_vecZero; // Where the bot should stand to guard position (moves around a bit)
	std::vector<Vector> GuardPoints; // All potential areas to watch that an enemy could approach from
	int NumGuardPoints = 0; // How many watch areas there are for the current location
	Vector GuardLookLocation = g_vecZero; // Which area are we currently watching?
	float GuardStartLookTime = 0.0f; // When did we start watching the current area?
	float ThisGuardLookTime = 0.0f; // How long should we watch this area for?
	float ThisGuardStandTime = 0.0f; // How long should we watch this area for?
	float GuardStartStandTime = 0.0f; // How long should we watch this area for?
};

// Bot skill settings. Affects things like aim accuracy and speed.
struct AvHAISkillLevel
{
	float marine_bot_reaction_time = 0.2f; // How quickly the bot will react to seeing an enemy
	float marine_bot_aim_skill = 0.5f; // How quickly the bot can lock on to an enemy
	float marine_bot_motion_tracking_skill = 0.5f; // How well the bot can follow an enemy target's motion
	float marine_bot_view_speed = 1.0f; // How fast a bot can spin its view to aim in a given direction
	float alien_bot_reaction_time = 0.2f; // How quickly the bot will react to seeing an enemy
	float alien_bot_aim_skill = 0.5f; // How quickly the bot can lock on to an enemy
	float alien_bot_motion_tracking_skill = 0.5f; // How well the bot can follow an enemy target's motion
	float alien_bot_view_speed = 0.5f; // How fast a bot can spin its view to aim in a given direction
};

// A bot task is a goal the bot wants to perform, such as attacking a structure, placing a structure etc. NOT USED BY COMMANDER
struct AvHAIPlayerTask
{
	EAITaskType TaskType = EAITaskType::TASK_NONE; // Task Type (e.g. build, attack, defend, heal etc)
	Vector TaskLocation = g_vecZero; // Task location, if task needs one (e.g. where to place structure for TASK_BUILD)
	edict_t* TaskTarget = nullptr; // Reference to a target, if task needs one (e.g. TASK_ATTACK)
	edict_t* TaskSecondaryTarget = nullptr; // Secondary target, if task needs one (e.g. TASK_REINFORCE)
	EAIStructureType StructureType = EAIStructureType::STRUCTURE_NONE; // For Gorges, what structure to build (if TASK_BUILD)
	float TaskStartedTime = 0.0f; // When the bot started this task. Helps time-out if the bot gets stuck trying to complete it
	bool bIssuedByCommander = false; // Was this task issued by the commander? Top priority if so
	bool bTargetIsPlayer = false; // Is the TaskTarget a player?
	bool bTaskIsUrgent = false; // Determines whether this task is prioritised over others if bot has multiple
	bool bIsWaitingForBuildLink = false; // If true, Gorge has sent the build impulse and is waiting to see if the building materialised
	float LastBuildAttemptTime = 0.0f; // When did the Gorge last try to place a structure?
	int BuildAttempts = 0; // How many attempts the Gorge has tried to place it, so it doesn't keep trying forever
	AvHMessageID Evolution = MESSAGE_NULL; // Used by TASK_EVOLVE to determine what to evolve into
	float TaskLength = 0.0f; // If a task has gone on longer than this time, it will be considered completed
};

struct AvHAIStuckTracker
{
	float LastStuckCheckTime = 0.0f; // Last time the bot checked if it had successfully moved
	float TotalStuckTime = 0.0f; // Total time the bot has spent stuck
	Vector LastBotPosition = g_vecZero;
	Vector MoveDestination = g_vecZero;
	bool bPathFollowFailed = false;

	void Clear()
	{
		LastStuckCheckTime = 0.0f;
		TotalStuckTime = 0.0f;
		LastBotPosition = g_vecZero;
		MoveDestination = g_vecZero;
		bPathFollowFailed = false;
	}
};



struct AvHAIPlayer
{
	AvHPlayer* Player = nullptr;
	edict_t* Edict = nullptr;
	AvHTeamNumber	Team = TEAM_IND;
	AvHAIMovementInput NextFrameMovementInput;
	byte			AdjustedMsec = 0;

	bool bIsPendingKill = false;
	bool bIsInactive = false;

	float LastUseTime = 0.0f;

	Vector SpawnLocation = g_vecZero;
	Vector DesiredMovementDir = g_vecZero;
	Vector CurrentEyePosition = g_vecZero;
	Vector CurrentFloorPosition = g_vecZero;

	Vector CollisionHullBottomLocation = g_vecZero;
	Vector CollisionHullTopLocation = g_vecZero;

	EAIWeaponId DesiredMoveWeapon = EAIWeaponId::WEAPON_INVALID;
	EAIWeaponId DesiredCombatWeapon = EAIWeaponId::WEAPON_INVALID;

	AvHBotViewFrustumPlane viewFrustum[6]; // Bot's view frustum. Essentially, their "screen" for determining visibility of stuff

	AvHAIEnemyStatus TrackedEnemies[32];
	int CurrentEnemy = -1;
	EAICombatStrategy CurrentCombatStrategy = EAICombatStrategy::COMBAT_STRATEGY_ATTACK;
	edict_t* CurrentEnemyRef = nullptr;

	AvHAIPlayerTask PrimaryBotTask;
	AvHAIPlayerTask SecondaryBotTask;
	AvHAIPlayerTask WantsAndNeedsTask;
	AvHAIPlayerTask CommanderTask; // Task assigned by the commander
	AvHAIPlayerTask* CurrentTask = &PrimaryBotTask; // Bot's current task they're performing

	float BotNextTaskEvaluationTime = 0.0f;

	AvHAISkillLevel BotSkillSettings;

	char PathStatus[128]; // Debug used to help figure out what's going on with a bot's path finding
	char MoveStatus[128]; // Debug used to help figure out what's going on with a bot's steering

	AvHAINavStatus BotNavInfo; // Bot's movement information, their current path, where in the path they are etc.

	AvHAIPendingMessageList PendingMessages;

	float LastCombatTime = 0.0f;

	AvHAIGuardInfo GuardInfo;

	float LastRequestTime = 0.0f; // When bot last used a voice line to request something. Prevents spam

	float LastTeleportTime = 0.0f; // Last time the bot teleported somewhere

	AvHAIViewInfo ViewInfo;

	Vector DesiredLookDirection = g_vecZero; // What view angle is the bot currently turning towards
	Vector InterpolatedLookDirection = g_vecZero; // Used to smoothly interpolate the bot's view rather than snap instantly like an aimbot
	edict_t* LookTarget = nullptr; // Used to work out what view angle is needed to look at the desired entity
	Vector LookTargetLocation = g_vecZero; // This is the bot's current desired look target. Could be an enemy (see LookTarget), or point of interest
	Vector MoveLookLocation = g_vecZero; // If the bot has to look somewhere specific for movement (e.g. up for a ladder or wall-climb), this will override LookTargetLocation so the bot doesn't get distracted and mess the move up
	bool bSnapView = false; // Use for rapid, precise snapping of the bot's view to the target. Useful if the bot requires more precise view angles for movement or other reasons
	float LastTargetTrackUpdate = 0.0f; // Add a delay to how frequently a bot can track a target's movements
	float ViewInterpolationSpeed = 0.0f; // How fast should the bot turn its view? Depends on distance to turn
	float ViewInterpStartedTime = 0.0f; // Used for interpolation

	float ViewUpdateRate = 0.2f; // How frequently the bot can react to new sightings of enemies etc.
	float LastViewUpdateTime = 0.0f; // Used to throttle view updates based on ViewUpdateRate

	Vector ViewForwardVector = g_vecZero; // Bot's current forward unit vector
	Vector LastSafeLocation = g_vecZero;

	int ExperiencePointsAvailable = 0; // How much experience the bot has to spend
	AvHMessageID NextCombatModeUpgrade = MESSAGE_NULL;

	float ThinkDelta = 0.0f; // How long since this bot last ran AIPlayerThink
	float LastThinkTime = 0.0f; // When the bot last ran AIPlayerThink

	float ServerUpdateDelta = 0.0f; // How long since we last called RunPlayerMove
	float LastServerUpdateTime = 0.0f; // When we last called RunPlayerMove

	float HearingThreshold = 0.0f; // How loud does a sound need to be before the bot detects it? This is set when hearing a sound so that louder sounds drown out quieter ones, and decrements quickly

	int DebugValue = 0; // Used for debugging the bot

	Vector DebugDestination = g_vecZero;

	bool IsValid() const { return Player != nullptr && !FNullEnt(Edict) && !Edict->free; }
	bool HasValidPath() const;
	const NavAgentProfile* GetNavProfile() const { return &BotNavInfo.NavProfile; }
	bool IsOnGround() const;
	bool IsOnLadder() const;
	bool IsInWater() const { return (Edict->v.flags & FL_INWATER); }
	bool CanCrouch() const;
	bool IsCrouching() const { return (Edict->v.flags & FL_DUCKING); }
	enum_hull GetPlayerHull(bool bIsCrouching) const;
	float GetPlayerRadius() const;
	float GetPlayerHeight() const;
	void AddMovementTask(AvHAIMoveTask& NewTask);
	EAINavMoveResult MoveTo(const Vector& DesiredLocation);
	EAINavMoveResult MoveToWithoutNav(const Vector& DesiredLocation);
	EAINavMoveResult ProgressMovementTasks();
	Vector GetLocation() const { return Edict->v.origin; }
	Vector GetVelocity() const { return Edict->v.velocity; }
	Vector GetEyePosition() const;
	void Jump(bool bDuckJump);
	void Suicide();
	bool IsDead() const;
	float GetDesiredMovementSpeed(bool bShouldWalk) const;
	Vector GetBottomOfHitbox() const;
	Vector GetTopOfHitbox() const;
	void CheckAndSendMessages();
	void Think(float DeltaTime);
	void StartThink(float DeltaTime);
	void EndThink(float DeltaTime);
	void UpdateView(float DeltaTime);
	void BotUpdateDesiredViewRotation();
	void InterpolateView(float DeltaTime);
	void LookAt(const Vector& LocationTarget);
	void LookAt(const edict_t* Target);
	void UpdateViewFrustum();
	bool IsObjectInFOV(const edict_t* Object) const;
	bool UseObject(edict_t* Object, bool bUseContinuously = false);
	void DropWeapon();
	void ReloadWeapon();
	void InterruptReload();
	EAIWeaponId GetCurrentWeapon() const;
	void LeaveCommChair();
	void UpdateReceivedOrders();
	void OnReceiveMoveOrder(const Vector& TargetLocation);
	void OnReceiveBuildOrder(const edict_t* TargetObject);
	void SwitchToWeapon(EAIWeaponId NewWeaponId);
	void RequestEvolveUpgrade(EAIAlienUpgrade DesiredUpgrade);
	void RequestEvolveLifeform(EAIAlienLifeform DesiredLifeform);
	void Say(const char* ThingToSay, bool bTeamSay, float Delay = 0.0f);
	bool ShouldThink() const;
	void HearEnemy(const edict_t* EmittingEdict, float Volume);
	void OnNavMeshModified(EAINavMeshIndex ModifiedMeshIndex);
	void TakeDamage(float DamageAmount, const edict_t* Inflictor);
	void UpdateNavProfile();

	EAINavMoveResult FollowPath(AvHAIPath* Path);
	bool NextMove(AvHAIPath* Path);
	bool NextSwimMove(AvHAIPath* Path);

	bool NewGroundMove(AvHAIPath* Path);
	bool NewFallMove(AvHAIPath* Path);
	bool NewJumpMove(AvHAIPath* Path);
	bool NewLadderMove(AvHAIPath* Path);
	bool NewPlatformMove(AvHAIPath* Path);
	bool NewPhaseGateMove(AvHAIPath* Path);
	bool NewMountLadderMove(AvHAIPath* Path);

	void HandlePlayerAvoidance(const AvHAIPathNode* CurrentPathNode);

	EAINavMoveResult ProgressMoveTask(AvHAIMoveTask* MoveTask);
};


Vector GetVisiblePointOnPlayerFromObserver(edict_t* Observer, edict_t* TargetPlayer);

#endif