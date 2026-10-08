
#include "AvHAIPlayer.h"
#include "AvHAIPlayerUtil.h"
#include "AvHAIHelper.h"
#include "AvHAIMath.h"
#include "AvHAINavigation.h"
#include "AvHAIWeaponHelper.h"
#include "AvHAITactical.h"
#include "AvHAITask.h"
#include "AvHAICommander.h"
#include "AvHAIPlayerManager.h"
#include "AvHAIConfig.h"

#include "AvHGamerules.h"
#include "AvHMessage.h"
#include "AvHTurret.h"


bool AvHAIPlayer::HasValidPath() const
{
	return BotNavInfo.MovementTasks.size() > 0;
}

bool AvHAIPlayer::IsOnGround() const
{
	return (!FNullEnt(Edict) && ((Edict->v.flags & FL_ONGROUND) || IsOnLadder()));
}

bool AvHAIPlayer::IsOnLadder() const
{
	return (!FNullEnt(Edict) && Edict->v.movetype == MOVETYPE_FLY);
}

bool AvHAIPlayer::CanCrouch() const
{
	if (!IsValid()) { return false; }

	switch (Edict->v.iuser3)
	{
		case AVH_USER3_ALIEN_PLAYER1:
		case AVH_USER3_ALIEN_PLAYER2:
		case AVH_USER3_ALIEN_PLAYER3:
			return false;
		default:
			return true;
	}
}

enum_hull AvHAIPlayer::GetPlayerHull() const
{
	if (!IsValid()) { return point_hull; }

	AvHUser3 PlayerClass = (AvHUser3)Edict->v.iuser3;

	switch (PlayerClass)
	{
		case AVH_USER3_MARINE_PLAYER: // Regular/heavy marine
		case AVH_USER3_ALIEN_PLAYER4: // Fade
			return (IsCrouching()) ? head_hull : human_hull;
		case AVH_USER3_COMMANDER_PLAYER:
			return head_hull;
		case AVH_USER3_ALIEN_EMBRYO: // Gestating
			return head_hull;
		case AVH_USER3_ALIEN_PLAYER1: // Skulk
		case AVH_USER3_ALIEN_PLAYER2: // Gorge
		case AVH_USER3_ALIEN_PLAYER3:// Lerk
			return head_hull;
		case AVH_USER3_ALIEN_PLAYER5: // Onos
			return (IsCrouching()) ? human_hull : large_hull;
		default:
			return head_hull;
	}
}

float AvHAIPlayer::GetPlayerRadius() const
{
	if (!IsValid()) { return 0.0f; }

	enum_hull PlayerHull = GetPlayerHull();

	switch (PlayerHull)
	{
		case human_hull:
		case head_hull:
			return 16.0f;
		case large_hull:
			return 32.0f;
		default:
			return 16.0f;
	}
}

float AvHAIPlayer::GetPlayerHeight() const
{
	if (!IsValid()) { return 0.0f; }

	enum_hull PlayerHull = GetPlayerHull();

	switch (PlayerHull)
	{
		case head_hull:
			return 36.0f;
		case human_hull:
			return 72.0f;
		case large_hull:
			return 108.0f;
		default:
			return 72.0f;
	}
}

Vector AvHAIPlayer::GetEyePosition() const
{
	if (FNullEnt(Edict)) { return ZERO_VECTOR; }

	return (Edict->v.origin + Edict->v.view_ofs);
}

void AvHAIPlayer::AddMovementTask(AvHAIMoveTask& NewTask)
{
	BotNavInfo.MovementTasks.push_back(NewTask);
}

EAINavMoveResult AvHAIPlayer::MoveTo(const Vector& DesiredLocation)
{
	// If the destination is close enough to our core movement task, then we are continuing with our existing movement tasks
	if (BotNavInfo.MovementTasks.size() > 0)
	{
		AvHAIMoveTask& CoreTask = BotNavInfo.MovementTasks.at(0);

		if (vEquals(CoreTask.TaskLocation, DesiredLocation, 18.0f))
		{
			return ProgressMovementTasks();
		}
	}

	// This is a brand new destination
	BotNavInfo.ClearPath();

	AvHAIMoveTask NewDestinationTask;

	if (AINAV_AddMoveMovementTask(GetNavProfile(), GetBottomOfHitbox(), DesiredLocation, nullptr, NewDestinationTask))
	{
		BotNavInfo.MovementTasks.push_back(NewDestinationTask);
		return EAINavMoveResult::NAV_MOVE_SUCCESS;
	}

	return EAINavMoveResult::NAV_MOVE_NOTASK;
}

EAINavMoveResult AvHAIPlayer::MoveToWithoutNav(const Vector& DesiredLocation)
{
	return EAINavMoveResult::NAV_MOVE_SUCCESS;
}

void AvHAIPlayer::InterruptReload()
{
	if (IsPlayerReloading(Player))
	{
		NextFrameMovementInput.Button |= IN_ATTACK;
	}
}

EAINavMoveResult AvHAIPlayer::ProgressMovementTasks()
{
	if (BotNavInfo.MovementTasks.size() == 0)
	{
		return EAINavMoveResult::NAV_MOVE_NOTASK;
	}

	AvHAIMoveTask* CurrentMoveTask = &BotNavInfo.MovementTasks.at(BotNavInfo.MovementTasks.size() - 1);

	switch (CurrentMoveTask->TaskType)
	{
		case EAIMovementTaskType::MOVE_TASK_MOVE:
			return ProgressMoveTask(CurrentMoveTask);
		default:
			return EAINavMoveResult::NAV_MOVE_NOPATH;
	}
}

void AvHAIPlayer::Jump(bool bDuckJump)
{
	if (IsOnGround())
	{
		if (gpGlobals->time - BotNavInfo.LandedTime >= 0.1f)
		{
			NextFrameMovementInput.Button |= IN_JUMP;
			NextFrameMovementInput.bHasAttemptedJump = true;
		}
	}
	else
	{
		if (bDuckJump)
		{
			NextFrameMovementInput.Button |= IN_DUCK;
		}
	}
}

void AvHAIPlayer::Suicide()
{
	if (!bIsPendingKill && !IsDead())
	{
		Player->Suicide();
		bIsPendingKill = true;
	}
}

bool AvHAIPlayer::IsDead() const
{
	return (Edict->v.deadflag != DEAD_NO || Edict->v.health <= 0.0f);
}

float AvHAIPlayer::GetDesiredMovementSpeed(bool bShouldWalk) const
{
	float MaxSpeed = fminf(CVAR_GET_FLOAT("cl_forwardspeed"), CVAR_GET_FLOAT("sv_maxspeed"));
	return (NextFrameMovementInput.bShouldWalk) ? MaxSpeed * 0.5f : MaxSpeed;
}

Vector AvHAIPlayer::GetBottomOfHitbox() const
{
	if (FNullEnt(Edict)) { return ZERO_VECTOR; }

	const float Height = GetPlayerHeight();

	return GetLocation() - Vector(0.0f, 0.0f, Height * 0.5f);
}

Vector AvHAIPlayer::GetTopOfHitbox() const
{
	if (FNullEnt(Edict)) { return ZERO_VECTOR; }

	const float Height = GetPlayerHeight();

	return GetLocation() + Vector(0.0f, 0.0f, Height * 0.5f);
}

void AvHAIPlayer::Think(float DeltaTime)
{
	NextFrameMovementInput.Clear();

	if (ShouldThink())
	{
		UpdateNavProfile();

		if (!vIsZero(DebugDestination))
		{
			MoveTo(DebugDestination);
		}
	}
}

void AvHAIPlayer::CheckAndSendMessages()
{
	AvHAIPendingMessageList::iterator MessageToDeliver = PendingMessages.end();
	float OldestMessage = -1.0f;

	for (auto MsgIt = PendingMessages.begin(); MsgIt != PendingMessages.end(); MsgIt++)
	{
		const AvHAIBotMsg* ThisPendingMessage = &(*MsgIt);

		if (ThisPendingMessage->SendTime > gpGlobals->time) { continue; }

		const float MessageAge = gpGlobals->time - ThisPendingMessage->SendTime;

		if (MessageAge > OldestMessage)
		{
			MessageToDeliver = MsgIt;
			OldestMessage = MessageAge;
		}
	}

	if (MessageToDeliver != PendingMessages.end())
	{
		AIPlayer_Say(Edict, MessageToDeliver->bIsTeamSay, MessageToDeliver->Message);
		PendingMessages.erase(MessageToDeliver);
	}
}

void AvHAIPlayer::StartThink(float DeltaTime)
{
	CheckAndSendMessages();
}

void AvHAIPlayer::UpdateView(float DeltaTime)
{
	BotUpdateDesiredViewRotation();
	InterpolateView(DeltaTime);
	UpdateViewFrustum();
}

void AvHAIPlayer::EndThink(float DeltaTime)
{
	NextFrameMovementInput.GenerateMovementOutputs(Edict->v.v_angle, Edict->v.maxspeed);

	const EAIWeaponId NewSwitchWeapon = (NextFrameMovementInput.RequiredWeapon != EAIWeaponId::WEAPON_INVALID)
		? NextFrameMovementInput.RequiredWeapon
		: NextFrameMovementInput.DesiredWeapon;

	if (NewSwitchWeapon != EAIWeaponId::WEAPON_INVALID && NewSwitchWeapon != GetCurrentWeapon())
	{
		SwitchToWeapon(NewSwitchWeapon);
	}

	UpdateView(DeltaTime);

	// Thanks to The Storm (ePODBot) for this one, finally fixed the bot running speed!
	int AdjustedTimeMS = (int)roundf((DeltaTime) * 1000.0f);

	if (AdjustedTimeMS > 255)
	{
		AdjustedTimeMS = 255;
	}

	// Simulate PM_PlayerMove so client prediction and stuff can be executed correctly.
	RUN_AI_MOVE(Edict, Edict->v.v_angle, NextFrameMovementInput.ForwardMove,
		NextFrameMovementInput.SideMove, NextFrameMovementInput.UpMove, NextFrameMovementInput.Button, NextFrameMovementInput.Impulse, (byte)AdjustedTimeMS);

	LastServerUpdateTime = gpGlobals->time;
}

void AvHAIPlayer::BotUpdateDesiredViewRotation()
{
	// If we are in the process of interpolating towards a current view target, don't interrupt and let it finish
	if (!vIsZero(ViewInfo.InterpolatingViewTarget)) { return; }

	const bool bIsRequiredView = !vIsZero(NextFrameMovementInput.RequiredLookLocation);

	const Vector NewDesiredTargetView = (bIsRequiredView)
		? NextFrameMovementInput.RequiredLookLocation
		: (!vIsZero(ViewInfo.LookTargetLocation)) ? ViewInfo.LookTargetLocation : NextFrameMovementInput.DesiredLookLocation;

	if (vIsZero(NewDesiredTargetView)) { return; }

	const Vector DesiredViewForwardVector = UTIL_GetVectorNormal(NewDesiredTargetView - GetEyePosition());

	ViewInfo.InterpolatingViewTarget = UTIL_VecToAngles(DesiredViewForwardVector);

	if (!vIsValid(ViewInfo.InterpolatingViewTarget))
	{
		ViewInfo.InterpolatingViewTarget = ZERO_VECTOR;
	}

	vClampViewAngles(ViewInfo.InterpolatingViewTarget);

	if (ViewInfo.bSnapView)
	{
		ViewInfo.ViewInterpolationSpeed = 1000.0f;
		ViewInfo.ViewInterpStartedTime = gpGlobals->time;

		return;
	}

	Vector ViewInterpolationDelta = ViewInfo.InterpolatingViewTarget - Edict->v.v_angle;

	// Now figure out how far we have to turn to reach our desired target
	vClampViewAngles(ViewInterpolationDelta);

	const float MaxViewDelta = fmaxf(fabsf(ViewInterpolationDelta.y), fabsf(ViewInterpolationDelta.x));

	float motion_tracking_skill = (IsPlayerMarine(Edict)) ? BotSkillSettings.marine_bot_motion_tracking_skill : BotSkillSettings.alien_bot_motion_tracking_skill;
	float bot_view_speed = (IsPlayerMarine(Edict)) ? BotSkillSettings.marine_bot_view_speed : BotSkillSettings.alien_bot_view_speed;
	float bot_aim_skill = (IsPlayerMarine(Edict)) ? BotSkillSettings.marine_bot_aim_skill : BotSkillSettings.alien_bot_aim_skill;

	ViewInfo.ViewInterpolationSpeed = (MaxViewDelta >= 45.0f)
		? 350.0f
		: (MaxViewDelta >= 25.0f) ? 175.0f
			: (MaxViewDelta >= 5.0f) ? 50.0f : 35.0f;

	ViewInfo.ViewInterpolationSpeed *= bot_view_speed;

	if (!bIsRequiredView)
	{
		const float AimOffset = (MaxViewDelta >= 45.0f)
			? frandrange(10.0f, 20.0f)
			: (MaxViewDelta >= 25.0f)
				? frandrange(5.0f, 10.0f)
				: (MaxViewDelta >= 5.0f)
					? frandrange(2.0f, 5.0f)
					: 0.0f;

		const float xOffset = AimOffset * (randbool()) ? -1.0f : 1.0f;
		const float yOffset = AimOffset * (randbool()) ? -1.0f : 1.0f;

		ViewInfo.InterpolatingViewTarget.x += xOffset;
		ViewInfo.InterpolatingViewTarget.y += yOffset;

		vClampViewAngles(ViewInfo.InterpolatingViewTarget);
	}

	ViewInfo.ViewInterpStartedTime = gpGlobals->time;
}

void AvHAIPlayer::InterpolateView(float DeltaTime)
{
	if (vIsZero(ViewInfo.InterpolatingViewTarget)) { return; }

	const Vector CurrentViewAngle = Edict->v.v_angle;
	Vector InterpDelta = ViewInfo.InterpolatingViewTarget - CurrentViewAngle;

	vClampViewAngles(InterpDelta);

	Vector InterpolatedFrameAngle = CurrentViewAngle;

	InterpolatedFrameAngle.x = fInterpConstantTo(CurrentViewAngle.x, ViewInfo.InterpolatingViewTarget.x, DeltaTime, ViewInfo.ViewInterpolationSpeed);

	const float YawDeltaInterp = fInterpConstantTo(0.0f, InterpDelta.y, DeltaTime, ViewInfo.ViewInterpolationSpeed);

	InterpolatedFrameAngle.y += YawDeltaInterp;

	vClampViewAngles(InterpolatedFrameAngle);

	if (vEquals2D(InterpolatedFrameAngle, ViewInfo.InterpolatingViewTarget) || (gpGlobals->time - ViewInfo.ViewInterpStartedTime > 2.0f))
	{
		ViewInfo.InterpolatingViewTarget = ZERO_VECTOR;
	}

	Edict->v.v_angle.x = InterpolatedFrameAngle.x;
	Edict->v.v_angle.y = InterpolatedFrameAngle.y;

	// set the body angles to point the gun correctly
	Edict->v.angles.x = Edict->v.v_angle.x / 3;
	Edict->v.angles.y = Edict->v.v_angle.y;
	Edict->v.angles.z = 0;

	// adjust the view angle pitch to aim correctly (MUST be after body v.angles stuff)
	Edict->v.v_angle.x = -Edict->v.v_angle.x;
	// Paulo-La-Frite - END

	Edict->v.ideal_yaw = Edict->v.v_angle.y;

	if (Edict->v.ideal_yaw > 180)
		Edict->v.ideal_yaw -= 360;

	if (Edict->v.ideal_yaw < -180)
		Edict->v.ideal_yaw += 360;
}

void AvHAIPlayer::LookAt(const Vector& LocationTarget)
{
	ViewInfo.LookTargetLocation = LocationTarget;
}

void AvHAIPlayer::LookAt(const edict_t* Target)
{
	if (FNullEnt(Target)) { return; }

	ViewInfo.LookTargetLocation = Target->v.origin;
}

void AvHAIPlayer::UpdateViewFrustum()
{
	MAKE_VECTORS(Edict->v.v_angle);
	Vector up = gpGlobals->v_up;
	Vector forward = gpGlobals->v_forward;
	Vector right = gpGlobals->v_right;

	Vector fc = (Edict->v.origin + Edict->v.view_ofs) + (forward * BOT_MAX_VIEW);

	Vector fbl = fc + (up * f_ffheight * 0.5f) - (right * f_ffwidth * 0.5f);
	Vector fbr = fc + (up * f_ffheight * 0.5f) + (right * f_ffwidth * 0.5f);
	Vector ftl = fc - (up * f_ffheight * 0.5f) - (right * f_ffwidth * 0.5f);
	Vector ftr = fc - (up * f_ffheight * 0.5f) + (right * f_ffwidth * 0.5f);

	Vector nc = (Edict->v.origin + Edict->v.view_ofs) + (forward * BOT_MIN_VIEW);

	Vector nbl = nc + (up * f_fnheight * 0.5f) - (right * f_fnwidth * 0.5f);
	Vector nbr = nc + (up * f_fnheight * 0.5f) + (right * f_fnwidth * 0.5f);
	Vector ntl = nc - (up * f_fnheight * 0.5f) - (right * f_fnwidth * 0.5f);
	Vector ntr = nc - (up * f_fnheight * 0.5f) + (right * f_fnwidth * 0.5f);

	UTIL_SetFrustumPlane(&ViewInfo.ViewFrustumPlanes[FRUSTUM_PLANE_TOP], ftl, ntl, ntr);
	UTIL_SetFrustumPlane(&ViewInfo.ViewFrustumPlanes[FRUSTUM_PLANE_BOTTOM], fbr, nbr, nbl);
	UTIL_SetFrustumPlane(&ViewInfo.ViewFrustumPlanes[FRUSTUM_PLANE_LEFT], fbl, nbl, ntl);
	UTIL_SetFrustumPlane(&ViewInfo.ViewFrustumPlanes[FRUSTUM_PLANE_RIGHT], ftr, ntr, nbr);
	UTIL_SetFrustumPlane(&ViewInfo.ViewFrustumPlanes[FRUSTUM_PLANE_NEAR], nbr, ntr, ntl);
	UTIL_SetFrustumPlane(&ViewInfo.ViewFrustumPlanes[FRUSTUM_PLANE_FAR], fbl, ftl, ftr);
}

bool AvHAIPlayer::IsObjectInFOV(const edict_t* Object) const
{
	if (!UTIL_IsEdictActive(Object) || (Object->v.effects & EF_NODRAW)) { return false; }

	if (IsEdictPlayer(Object) && !IsPlayerActiveInGame(Object)) { return false; }

	// To make things a little more accurate, we're going to treat players as cylinders rather than boxes
	for (int i = 0; i < 6; i++)
	{
		// Our cylinder must be inside all planes to be visible, otherwise return false
		if (!UTIL_CylinderInsidePlane(&ViewInfo.ViewFrustumPlanes[i], Object->v.origin - Vector(0.0f, 0.0f, 5.0f), 60.0f, 16.0f))
		{
			return false;
		}
	}

	return true;
}

bool AvHAIPlayer::UseObject(edict_t* Object, bool bUseContinuously)
{
	if (FNullEnt(Object)) { return false; }

	CBaseEntity* UsedObject = CBaseEntity::Instance(Object);

	if (!UsedObject) { return false; }

	Vector ClosestPoint = UTIL_GetClosestPointOnEntityToLocation(Edict->v.origin, Object);
	Vector TargetCentre = UTIL_GetCentreOfEntity(Object);

	Vector AimPoint = ClosestPoint;

	const bool bIsUsingStructure = AITAC_IsEdictStructure(Object);

	if (bIsUsingStructure)
	{
		AimPoint = TargetCentre;
		AimPoint.z = ClosestPoint.z;
	}

	LookAt(AimPoint);

	if (!bUseContinuously && ((gpGlobals->time - LastUseTime) < min_player_use_interval)) { return false; }

	Vector AimDir = UTIL_GetForwardVector2D(Edict->v.v_angle);
	Vector TargetAimDir = UTIL_GetVectorNormal2D(AimPoint - GetEyePosition());

	float AimDot = UTIL_GetDotProduct2D(AimDir, TargetAimDir);

	if (AimDot >= 0.95f)
	{
		LastUseTime = gpGlobals->time;

		if (UsedObject)
		{
			UsedObject->Use(Player, Player, USE_TOGGLE, 0);
		}

		return true;
	}

	return false;
}

EAIWeaponId AvHAIPlayer::GetCurrentWeapon() const
{
	if (!Player) { return EAIWeaponId::WEAPON_INVALID; }

	AvHBasePlayerWeapon* theBasePlayerWeapon = dynamic_cast<AvHBasePlayerWeapon*>(Player->m_pActiveItem);

	if (theBasePlayerWeapon)
	{
		return static_cast<EAIWeaponId>(theBasePlayerWeapon->m_iId);
	}

	return EAIWeaponId::WEAPON_INVALID;
}

void AvHAIPlayer::LeaveCommChair()
{
	if (!IsValid() || !IsPlayerCommander(Edict)) { return; }

	Player->SetUser3(AVH_USER3_MARINE_PLAYER);

	// Cheesy way to make sure player class change is sent to everyone
	Player->EffectivePlayerClassChanged();
}

void AvHAIPlayer::UpdateReceivedOrders()
{
	OrderListType ActiveOrders = Player->GetActiveOrders();

	for (auto it = ActiveOrders.begin(); it != ActiveOrders.end(); it++)
	{
		if (it->GetOrderActive() && it->GetReceiver() && ENTINDEX(Edict) == it->GetReceiver())
		{
			Vector OrderLocation = g_vecZero;
			it->GetLocation(OrderLocation);

			switch (it->GetOrderType())
			{
			case ORDERTYPEL_MOVE:
				OnReceiveMoveOrder(OrderLocation);
				break;
			case ORDERTYPET_BUILD:
				OnReceiveBuildOrder(INDEXENT(it->GetTargetIndex()));
				break;
			default:
				break;
			}
		}
	}
}

void AvHAIPlayer::OnReceiveMoveOrder(const Vector& TargetLocation)
{

}

void AvHAIPlayer::OnReceiveBuildOrder(const edict_t* TargetObject)
{
	if (!UTIL_IsEdictActive(TargetObject)) { return; }
}

void AvHAIPlayer::SwitchToWeapon(EAIWeaponId NewWeaponId)
{
	if (NewWeaponId == EAIWeaponId::WEAPON_INVALID) { return; }

	if (!UTIL_PlayerHasWeapon(Player, NewWeaponId)) { return; }

	const char* WeaponName = UTIL_WeaponTypeToClassname(NewWeaponId);
	Player->SwitchWeapon(WeaponName);
}

void AvHAIPlayer::RequestEvolveUpgrade(EAIAlienUpgrade DesiredUpgrade)
{
	AvHMessageID NewImpulse = UTIL_GetEvolveUpgradeImpulse(DesiredUpgrade);

	if (NewImpulse != MESSAGE_NULL)
	{
		NextFrameMovementInput.Impulse = NewImpulse;
	}
}

void AvHAIPlayer::RequestEvolveLifeform(EAIAlienLifeform DesiredLifeform)
{
	AvHMessageID NewImpulse = UTIL_GetEvolveLifeformImpulse(DesiredLifeform);

	if (NewImpulse != MESSAGE_NULL)
	{
		NextFrameMovementInput.Impulse = NewImpulse;
	}
}

void AvHAIPlayer::Say(const char* ThingToSay, bool bTeamSay, float Delay)
{
	AvHAIBotMsg NewMessage;
	NewMessage.bIsTeamSay = bTeamSay;
	NewMessage.SendTime = gpGlobals->time + Delay;
	std::sprintf(NewMessage.Message, ThingToSay);

	PendingMessages.push_back(NewMessage);
}

void AvHAIPlayer::DropWeapon()
{
	// Look straight ahead so we don't accidentally drop the weapon right at our feet and pick it up again instantly

	Vector AimDir = UTIL_GetForwardVector(Edict->v.v_angle);
	Vector TargetAimDir = Vector(AimDir.x, AimDir.y, 0.0f);

	Vector LookLoc = GetEyePosition() + (TargetAimDir * 100.0f);

	LookAt(LookLoc);

	float AimDot = UTIL_GetDotProduct(AimDir, TargetAimDir);

	if (AimDot >= 0.95f)
	{
		NextFrameMovementInput.Impulse = WEAPON_DROP;
	}
}

void AvHAIPlayer::ReloadWeapon()
{
	const EAIWeaponId CurrentWeapon = GetCurrentWeapon();

	if (CurrentWeapon == EAIWeaponId::WEAPON_INVALID) { return; }

	if (!UTIL_WeaponCanBeReloaded(CurrentWeapon)) { return; }

	if (!IsPlayerReloading(Player))
	{
		if (gpGlobals->time - LastUseTime > 1.0f)
		{
			NextFrameMovementInput.Button |= IN_RELOAD;
			LastUseTime = gpGlobals->time;
		}
	}
}

bool AvHAIPlayer::ShouldThink() const
{
	return AIMESH_IsNavMeshLoaded()
		&& GetGameRules()->GetGameStarted()
		&& !AIMGR_HasMatchEnded()
		&& (IsPlayerActiveInGame(Edict) || IsPlayerCommander(Edict))
		&& !IsPlayerGestating(Edict);
}

void AvHAIPlayer::HearEnemy(const edict_t* EmittingEdict, float Volume)
{

}

void AvHAIPlayer::OnNavMeshModified(EAINavMeshIndex ModifiedMeshIndex)
{
	for (auto MoveTaskIt = BotNavInfo.MovementTasks.begin(); MoveTaskIt != BotNavInfo.MovementTasks.end(); MoveTaskIt++)
	{
		AvHAIMoveTask* ThisTask = &(*MoveTaskIt);

		if (!ThisTask || !ThisTask->HasPath()) { continue; }

		if (ThisTask->TaskPath.UsedNavMesh == ModifiedMeshIndex)
		{
			// This will force a recalculation when the bot next wants to handle this movement task
			ThisTask->TaskPath.Clear();
		}
	}
}

void AvHAIPlayer::TakeDamage(float DamageAmount, const edict_t* Inflictor)
{

}

void AvHAIPlayer::UpdateNavProfile()
{
	if (IsPlayerMarine(Player))
	{
		BotNavInfo.NavProfile = *GetBaseAgentProfile(EAINavProfileIndex::NAV_PROFILE_MARINE);
	}
}

EAINavMoveResult AvHAIPlayer::FollowPath(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return EAINavMoveResult::NAV_MOVE_NOPATH; }

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();
	const AvHAIPathNode* NextPathNode = Path->GetNextPathNode();

	if (!CurrentPathNode || !CurrentPathNode->IsValidMove()) { return EAINavMoveResult::NAV_MOVE_NOPATH; }

	if (AINAV_IsPathPointComplete(GetNavProfile(), Edict, CurrentPathNode, NextPathNode))
	{
		// We have reached the end of our path. Job done.
		if (!NextPathNode)
		{
			return EAINavMoveResult::NAV_MOVE_PATH_COMPLETE;
		}

		Path->OnPathNodeComplete();

		CurrentPathNode = Path->GetCurrentPathNode();
		NextPathNode = Path->GetNextPathNode();
	}

	if (IsInWater())
	{
		TraceResult Hit;

		AvHAIMutablePathNodeList FutureNodeList = Path->GetMutableFuturePathNodeList();

		for (AvHAIPathNode* ThisNode : FutureNodeList)
		{
			if (!UTIL_IsPointInSwimArea(ThisNode->ToLocation)) { break; }

			UTIL_TraceHull(GetLocation(), ThisNode->ToLocation, ignore_monsters, head_hull, nullptr, &Hit);

			if (!Hit.fAllSolid && !Hit.fStartSolid && Hit.flFraction >= 1.0f)
			{
				Path->JumpToPathNode(ThisNode);
				ThisNode->FromLocation = GetLocation();
			}
		}

		CurrentPathNode = Path->GetCurrentPathNode();
		NextPathNode = Path->GetNextPathNode();
	}

	if (IsPlayerStandingOnPlayer(Edict) && CurrentPathNode->MovementFlag != EAINavMovementFlag::NAV_FLAG_LADDER)
	{
		if (GetVelocity().Length2D() > 10.0f)
		{
			NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(-Edict->v.groundentity->v.velocity);
			return EAINavMoveResult::NAV_MOVE_SUCCESS;
		}

		return MoveToWithoutNav(CurrentPathNode->ToLocation);
	}

	AvHPlayer* RidingPlayer = AINAV_GetPlayerRidingOnBot(Edict);

	if (RidingPlayer)
	{
		// TODO: Something here
	}

	if (AINAV_IsOffPathNode(GetNavProfile(), Edict, CurrentPathNode))
	{
		const bool bSucceededRegen = AINAV_FindPathClosestToPoint(GetNavProfile(), UTIL_GetFloorUnderEntity(Edict), Path->GetFinalDestination(), Path, GetPlayerRadius());

		if (!bSucceededRegen)
		{
			return EAINavMoveResult::NAV_MOVE_NOPATH;
		}
	}

	AvHAIMoveTask NewMoveTask;

	if (AINAV_CheckAndAddRequiredMovementTasks(GetNavProfile(), Path, NewMoveTask))
	{
		AddMovementTask(NewMoveTask);
		return EAINavMoveResult::NAV_MOVE_SUCCESS;
	}

	if (IsInWater())
	{
		NextSwimMove(Path);
	}
	else
	{
		NextMove(Path);
	}

	HandlePlayerAvoidance(CurrentPathNode);

	if (vIsZero(NextFrameMovementInput.RequiredLookLocation) && vIsZero(NextFrameMovementInput.DesiredLookLocation))
	{
		Vector FurthestView = AINAV_GetFurthestVisiblePointOnPath(GetEyePosition(), Path);

		if (vIsZero(FurthestView) || vDist2DSq(FurthestView, GetEyePosition()) < sqrf(200.0f))
		{
			FurthestView = CurrentPathNode->ToLocation;

			Vector LookNormal = UTIL_GetVectorNormal2D(FurthestView - GetEyePosition());

			FurthestView = FurthestView + (LookNormal * 1000.0f);
		}

		NextFrameMovementInput.DesiredLookLocation = FurthestView;
	}

	return EAINavMoveResult::NAV_MOVE_SUCCESS;
}

void AvHAIPlayer::HandlePlayerAvoidance(const AvHAIPathNode* CurrentPathNode)
{

}

EAINavMoveResult AvHAIPlayer::ProgressMoveTask(AvHAIMoveTask* MoveTask)
{
	if (!MoveTask || !MoveTask->IsValid()) { return EAINavMoveResult::NAV_MOVE_NOTASK; }

	if (!MoveTask->HasPath() || MoveTask->TaskPath.UsedNavMesh != GetNavProfile()->MeshIndex)
	{
		const bool bSuccess = AINAV_FindPathClosestToPoint(GetNavProfile(), UTIL_GetFloorUnderEntity(Edict), MoveTask->TaskLocation, &MoveTask->TaskPath, GetPlayerRadius());

		if (!bSuccess)
		{
			return EAINavMoveResult::NAV_MOVE_NOPATH;
		}
	}

	return FollowPath(&MoveTask->TaskPath);
}

bool AvHAIPlayer::NextSwimMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	return true;
}

bool AvHAIPlayer::NextMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();

	bool bMoveSuccess = false;

	switch (CurrentPathNode->MovementFlag)
	{
	case EAINavMovementFlag::NAV_FLAG_WALK:
		return NewGroundMove(Path);
		break;
	case EAINavMovementFlag::NAV_FLAG_FALL:
		return NewFallMove(Path);
		break;
	case EAINavMovementFlag::NAV_FLAG_JUMP:
		return NewJumpMove(Path);
		break;
	case EAINavMovementFlag::NAV_FLAG_LADDER:
		return NewLadderMove(Path);
		break;
	case EAINavMovementFlag::NAV_FLAG_PLATFORM:
		return NewPlatformMove(Path);
		break;
	case EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM1:
	case EAINavMovementFlag::NAV_FLAG_PHASEGATE_TEAM2:
		return NewPhaseGateMove(Path);
	default:
		return NewGroundMove(Path);
		break;
	}
}

bool AvHAIPlayer::NewGroundMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();

	const Vector CurrentPos = (IsOnGround()) ? GetLocation() : UTIL_GetFloorUnderEntity(Edict);

	const Vector vForward = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - CurrentPos);
	// Same goes for the right vector, might not be the same as the bot's right
	const Vector vRight = UTIL_GetVectorNormal(UTIL_GetCrossProduct(vForward, UP_VECTOR));

	bool bAdjustingForCollision = false;

	const float PlayerRadius = GetPlayerRadius() + 2.0f;

	Vector stTrcLft = CurrentPos - (vRight * PlayerRadius);
	Vector stTrcRt = CurrentPos + (vRight * PlayerRadius);
	Vector endTrcLft = stTrcLft + (vForward * 24.0f);
	Vector endTrcRt = stTrcRt + (vForward * 24.0f);

	bool bumpLeft = !AINAV_IsPointDirectlyReachable(GetNavProfile(), stTrcLft, endTrcLft);
	bool bumpRight = !AINAV_IsPointDirectlyReachable(GetNavProfile(), stTrcRt, endTrcRt);

	NextFrameMovementInput.DesiredMoveDirection = vForward;

	if (bumpRight && !bumpLeft)
	{
		NextFrameMovementInput.DesiredMoveDirection = NextFrameMovementInput.DesiredMoveDirection - vRight;
	}
	else if (bumpLeft && !bumpRight)
	{
		NextFrameMovementInput.DesiredMoveDirection = NextFrameMovementInput.DesiredMoveDirection + vRight;
	}
	else if (bumpLeft && bumpRight)
	{
		stTrcLft.z = Edict->v.origin.z;
		stTrcRt.z = Edict->v.origin.z;
		endTrcLft.z = Edict->v.origin.z;
		endTrcRt.z = Edict->v.origin.z;

		if (!UTIL_QuickTrace(Edict, stTrcLft, endTrcLft))
		{
			NextFrameMovementInput.DesiredMoveDirection = NextFrameMovementInput.DesiredMoveDirection + vRight;
		}
		else
		{
			NextFrameMovementInput.DesiredMoveDirection = NextFrameMovementInput.DesiredMoveDirection - vRight;
		}
	}
	else
	{
		const float DistFromLine = vDistanceFromLine2D(CurrentPathNode->FromLocation, CurrentPathNode->ToLocation, CurrentPos);

		if (DistFromLine > 18.0f)
		{
			float modifier = (float)vPointOnLine(CurrentPathNode->FromLocation, CurrentPathNode->ToLocation, CurrentPos);
			NextFrameMovementInput.DesiredMoveDirection = NextFrameMovementInput.DesiredMoveDirection + (vRight * modifier);
		}
	}

	NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(NextFrameMovementInput.DesiredMoveDirection);

	if (CanCrouch())
	{
		if (EnumHasAnyFlags(CurrentPathNode->MovementFlag, EAINavMovementFlag::NAV_FLAG_CROUCH))
		{
			NextFrameMovementInput.bShouldCrouch = true;
		}
		else
		{
			Vector HeadLocation = GetPlayerTopOfCollisionHull(Edict, false);

			// Crouch if we have something in our way at head height
			if (!UTIL_QuickTrace(Edict, HeadLocation, (HeadLocation + (NextFrameMovementInput.DesiredMoveDirection * 50.0f))))
			{
				NextFrameMovementInput.bShouldCrouch = true;
			}
		}
	}

	return true;
}

bool AvHAIPlayer::NewFallMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();

	const Vector AIPlayerLocation = GetLocation();
	const Vector vBotOrientation = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - AIPlayerLocation);
	const Vector vForward = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - CurrentPathNode->FromLocation);

	if (!IsOnGround())
	{
		NextFrameMovementInput.DesiredMoveDirection = vBotOrientation;
		return true;
	}

	if (vDist2DSq(AIPlayerLocation, CurrentPathNode->ToLocation) > sqrf(GetPlayerRadius()))
	{
		NextFrameMovementInput.DesiredMoveDirection = vBotOrientation;
	}
	else
	{
		NextFrameMovementInput.DesiredMoveDirection = vForward;
	}

	if (!CanCrouch()) { return true; }

	const Vector HeadLocation = GetPlayerTopOfCollisionHull(Edict, false);

	if (!UTIL_QuickTrace(Edict, HeadLocation, (HeadLocation + (NextFrameMovementInput.DesiredMoveDirection * 50.0f))))
	{
		NextFrameMovementInput.bShouldCrouch = true;
	}

	return true;
}

bool AvHAIPlayer::NewJumpMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();
	const AvHAIPathNode* NextPathNode = Path->GetNextPathNode();

	Vector vForward = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - GetLocation());

	if (vIsZero(vForward))
	{
		vForward = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - CurrentPathNode->FromLocation);
	}

	const Vector CurrentVelocity = GetVelocity();
	const Vector CurrentVelocity2D = UTIL_GetVectorNormal2D(CurrentVelocity);

	NextFrameMovementInput.DesiredMoveDirection = vForward;

	float Dot = UTIL_GetDotProduct2D(vForward, CurrentVelocity2D);

	// Yes this is cheating, but I'm up against millions of years of human evolution here...
	if (IsOnGround() && Dot < 0.95f)
	{
		float MoveSpeed = vSize2D(GetVelocity());
		Vector NewVelocity = vForward * fmaxf(MoveSpeed, GetDesiredMovementSpeed(false));
		NewVelocity.z = CurrentVelocity.z;

		NextFrameMovementInput.VelocityOverride = NewVelocity;
	}

	Jump(true);

	if (!CanCrouch()) { return true; }

	Vector HeadLocation = GetPlayerTopOfCollisionHull(Edict, false);

	if (!UTIL_QuickTrace(Edict, HeadLocation, (HeadLocation + (NextFrameMovementInput.DesiredMoveDirection * 50.0f))))
	{
		NextFrameMovementInput.bShouldCrouch = true;
	}

	return true;
}

bool AvHAIPlayer::NewLadderMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();

	bool bIsGoingUpLadder = (CurrentPathNode->FromLocation.z < CurrentPathNode->ToLocation.z);
	bool bAtAppropriateClimbHeight = (bIsGoingUpLadder) ? (GetLocation().z >= CurrentPathNode->RequiredClimbZ) : (GetLocation().z <= CurrentPathNode->RequiredClimbZ);

	const NavAgentProfile* NavProfile = GetNavProfile();

	if (!IsOnLadder())
	{
		if (!IsOnGround() || AINAV_IsPointDirectlyReachable(NavProfile, GetBottomOfHitbox(), CurrentPathNode->ToLocation))
		{
			return MoveToWithoutNav(CurrentPathNode->ToLocation) == EAINavMoveResult::NAV_MOVE_SUCCESS;
		}
		else
		{
			return NewMountLadderMove(Path);
		}
	}

	const Vector BotLocation = GetLocation();
	const Vector BotEyePosition = GetEyePosition();
	const Vector CollisionBottomLocation = GetBottomOfHitbox();
	const Vector CollisionTopLocation = GetTopOfHitbox();
	const float PlayerRadius = GetPlayerRadius();

	edict_t* CurrentLadder = UTIL_GetNearestLadderAtPoint(BotLocation);
	Vector LadderTop = UTIL_GetCentreOfEntity(CurrentLadder);
	LadderTop.z = CurrentLadder->v.absmax.z;

	// We're on the ladder and actively climbing

	Vector LadderNormalCheck = CollisionBottomLocation + Vector(0.0f, 0.0f, 18.0f);
	LadderNormalCheck = LadderNormalCheck + UTIL_GetVectorNormal2D(LadderNormalCheck - LadderTop);

	Vector CurrentLadderNormal = UTIL_GetNearestLadderNormal(LadderNormalCheck);

	CurrentLadderNormal = UTIL_GetVectorNormal2D(CurrentLadderNormal);

	if (vIsZero(CurrentLadderNormal))
	{

		if (CurrentPathNode->ToLocation.z > CurrentPathNode->FromLocation.z)
		{
			CurrentLadderNormal = UTIL_GetVectorNormal2D(CurrentPathNode->FromLocation - CurrentPathNode->ToLocation);
		}
		else
		{
			CurrentLadderNormal = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - CurrentPathNode->FromLocation);
		}
	}

	const Vector LadderRightNormal = UTIL_GetVectorNormal(UTIL_GetCrossProduct(CurrentLadderNormal, UP_VECTOR));

	Vector ClimbRightNormal = (bIsGoingUpLadder) ? -LadderRightNormal : LadderRightNormal;

	Vector ClimbDir = (bIsGoingUpLadder) ? -CurrentLadderNormal : CurrentLadderNormal;

	Vector DisembarkDir = UTIL_GetVectorNormal2D(CurrentPathNode->ToLocation - GetLocation());
	float DisembarkDot = UTIL_GetDotProduct2D(CurrentLadderNormal, DisembarkDir);

	float DesiredClimbHeight = CurrentPathNode->RequiredClimbZ;

	if (DisembarkDot > 0.75f)
	{
		float JumpDist = vDist2DSq(GetLocation(), CurrentPathNode->ToLocation);

		float ExtraClimbHeight = (JumpDist > sqrf(2.0f)) ? 100.0f : 50.0f;

		DesiredClimbHeight = fminf((CurrentPathNode->RequiredClimbZ + ExtraClimbHeight), LadderTop.z + GetPlayerOriginOffsetFromFloor(Edict, true).z);
	}

	// First check if we should dismount the ladder

	bIsGoingUpLadder = BotLocation.z < DesiredClimbHeight;

	if (bIsGoingUpLadder)
	{
		// We've reached the end of the ladder, try and make it to the disembark point if we can
		if (LadderTop.z - CollisionBottomLocation.z <= 2.0f || BotLocation.z >= DesiredClimbHeight)
		{
			MoveToWithoutNav(CurrentPathNode->ToLocation);

			const Vector DesiredLookTarget = (BotEyePosition + (DisembarkDir * 50.0f)) + Vector(0.0f, 0.0f, 50.0f);

			NextFrameMovementInput.RequiredLookLocation = DesiredLookTarget;

			if (DisembarkDot > 0.75f)
			{
				Jump(true);
			}

			return true;
		}
	}
	else
	{
		bool bDesiredGoingDownLadder = CurrentPathNode->FromLocation.z > CurrentPathNode->ToLocation.z;

		if (bDesiredGoingDownLadder && (BotLocation.z <= DesiredClimbHeight || (CollisionBottomLocation.z - CurrentPathNode->ToLocation.z < 100.0f)))
		{
			// We're close enough to the end that we can jump off the ladder
			if (UTIL_QuickTrace(Edict, CollisionTopLocation, CurrentPathNode->ToLocation))
			{
				MoveToWithoutNav(CurrentPathNode->ToLocation);
				Jump(true);
				return true;
			}
		}
	}

	// Still climbing

	Vector TraceStartPosition = (bIsGoingUpLadder) ? CollisionTopLocation : CollisionBottomLocation;

	Vector StartLeftTrace = TraceStartPosition - (ClimbRightNormal * PlayerRadius);
	Vector StartRightTrace = TraceStartPosition + (ClimbRightNormal * PlayerRadius);

	Vector EndLeftTrace = (bIsGoingUpLadder) ? StartLeftTrace + Vector(0.0f, 0.0f, 2.0f) : StartLeftTrace - Vector(0.0f, 0.0f, 2.0f);
	Vector EndRightTrace = (bIsGoingUpLadder) ? StartRightTrace + Vector(0.0f, 0.0f, 2.0f) : StartRightTrace - Vector(0.0f, 0.0f, 2.0f);

	bool bBlockedLeft = !UTIL_QuickTrace(Edict, StartLeftTrace, EndLeftTrace);
	bool bBlockedRight = !UTIL_QuickTrace(Edict, StartRightTrace, EndRightTrace);

	// Look up at the top of the ladder

	// If we are blocked going up the ladder, face the ladder and slide left/right to avoid blockage
	if (bBlockedLeft && !bBlockedRight)
	{
		Vector LookLocation = BotLocation - (CurrentLadderNormal * 50.0f);
		LookLocation.z = CurrentPathNode->RequiredClimbZ + 100.0f;

		NextFrameMovementInput.RequiredLookLocation = LookLocation;
		NextFrameMovementInput.DesiredMoveDirection = ClimbRightNormal;

		return true;
	}

	if (bBlockedRight && !bBlockedLeft)
	{
		Vector LookLocation = BotLocation - (CurrentLadderNormal * 50.0f);
		LookLocation.z = CurrentPathNode->RequiredClimbZ + 100.0f;

		NextFrameMovementInput.RequiredLookLocation = LookLocation;
		NextFrameMovementInput.DesiredMoveDirection = -ClimbRightNormal;

		return true;
	}

	if (CanCrouch())
	{
		Vector HeadTraceLocation = CollisionTopLocation;

		bool bHittingHead = !UTIL_QuickTrace(Edict, HeadTraceLocation, HeadTraceLocation + Vector(0.0f, 0.0f, 2.0f));

		if (bHittingHead)
		{
			NextFrameMovementInput.bShouldCrouch = true;
		}
	}

	NextFrameMovementInput.DesiredMoveDirection = ClimbDir;

	Vector LookTarget = CurrentPathNode->ToLocation;

	if (bIsGoingUpLadder)
	{
		LookTarget = LadderTop + (ClimbDir * 50.0f);
		LookTarget.z = DesiredClimbHeight + 50.0f;
	}

	NextFrameMovementInput.RequiredLookLocation = LookTarget;

	return true;
}

bool AvHAIPlayer::NewMountLadderMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();

	edict_t* MountLadder = UTIL_GetNearestLadderAtPoint(CurrentPathNode->FromLocation);

	if (FNullEnt(MountLadder))
	{
		return MoveToWithoutNav(CurrentPathNode->ToLocation) == EAINavMoveResult::NAV_MOVE_SUCCESS;
	}

	const Vector BotCurrentLocation = GetLocation();

	Vector LadderCentre = UTIL_GetCentreOfEntity(MountLadder);

	Vector MountPoint = AINAV_GetLadderMountPoint(MountLadder, CurrentPathNode->ToLocation);

	bool bMountingFromTop = BotCurrentLocation.z > MountLadder->v.absmax.z;

	if (!vEquals(MountPoint, LadderCentre) && !bMountingFromTop)
	{
		Vector AnglePlayerToLadder = UTIL_GetVectorNormal2D(LadderCentre - BotCurrentLocation);
		Vector AngleMountPointToLadder = UTIL_GetVectorNormal2D(LadderCentre - MountPoint);

		if (UTIL_GetDotProduct2D(AnglePlayerToLadder, AngleMountPointToLadder) > 0.9f)
		{
			MountPoint = LadderCentre;
		}
	}

	if (bMountingFromTop)
	{
		NextFrameMovementInput.bShouldWalk = true;
	}

	NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(MountPoint - BotCurrentLocation);

	return true;
}

bool AvHAIPlayer::NewPlatformMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	return true;
}

bool AvHAIPlayer::NewPhaseGateMove(AvHAIPath* Path)
{
	if (!Path || !Path->IsValidPath()) { return false; }

	const AvHAIPathNode* CurrentPathNode = Path->GetCurrentPathNode();

	StructureSearchFilter PGFilter;
	PGFilter.DeployableTeam = (AvHTeamNumber)Edict->v.team;
	PGFilter.DeployableTypes = EAIStructureType::STRUCTURE_MARINE_PHASEGATE;
	PGFilter.MaxSearchRadius = UTIL_MetresToGoldSrcUnits(2.0f);
	PGFilter.IncludeStatusFlags = EAIStructureStatus::STRUCTURE_STATUS_COMPLETED;

	const AvHAIBuildableStructure* NearestPhaseGate = AITAC_FindSingleMatchingStructure(CurrentPathNode->FromLocation, &PGFilter, EAIStructureSortType::FIND_STRUCTURE_NEAREST);

	if (!NearestPhaseGate || !NearestPhaseGate->IsValid()) { return true; }

	if (IsPlayerInUseRange(Edict, NearestPhaseGate->Edict))
	{
		NextFrameMovementInput.RequiredLookLocation = NearestPhaseGate->Location;
		NextFrameMovementInput.DesiredMoveDirection = g_vecZero;
		UseObject(NearestPhaseGate->Edict, false);

		if (vDist2DSq(GetLocation(), NearestPhaseGate->Location) < sqrf(16.0f))
		{
			NextFrameMovementInput.DesiredMoveDirection = UTIL_GetForwardVector2D(Edict->v.angles);
		}

		return true;
	}
	else
	{
		NextFrameMovementInput.DesiredMoveDirection = UTIL_GetVectorNormal2D(NearestPhaseGate->Location - GetLocation());
	}

	return true;
}

void AvHAIMovementInput::GenerateMovementOutputs(const Vector& CurrentViewAngles, float MaxSpeed)
{
	ClearMovementOutputs();

	if (vIsZero(DesiredMoveDirection)) { return; }

	UTIL_NormalizeVector2D(&DesiredMoveDirection);

	float CurrentYaw = CurrentViewAngles.y;
	float MoveDelta = UTIL_VecToAngles(DesiredMoveDirection).y;
	float AngleDelta = CurrentYaw - MoveDelta;

	float BotSpeed = MaxSpeed;

	if (bShouldWalk)
	{
		BotSpeed *= 0.4f;
		Button |= IN_WALK;
	}

	if (AngleDelta < -180.0f)
	{
		AngleDelta += 360.0f;
	}
	else if (AngleDelta > 180.0f)
	{
		AngleDelta -= 360.0f;
	}

	if (AngleDelta >= -22.5f && AngleDelta < 22.5f)
	{
		ForwardMove = BotSpeed;
		SideMove = 0.0f;
		Button |= IN_FORWARD;
	}
	else if (AngleDelta >= 22.5f && AngleDelta < 67.5f)
	{
		ForwardMove = BotSpeed;
		SideMove = BotSpeed;
		Button |= IN_FORWARD;
		Button |= IN_MOVERIGHT;
	}
	else if (AngleDelta >= 67.5f && AngleDelta < 112.5f)
	{
		ForwardMove = 0.0f;
		SideMove = BotSpeed;
		Button |= IN_MOVERIGHT;
	}
	else if (AngleDelta >= 112.5f && AngleDelta < 157.5f)
	{
		ForwardMove = -BotSpeed;
		SideMove = BotSpeed;
		Button |= IN_BACK;
		Button |= IN_MOVERIGHT;
	}
	else if (AngleDelta >= 157.5f || AngleDelta <= -157.5f)
	{
		ForwardMove = -BotSpeed;
		SideMove = 0.0f;
		Button |= IN_BACK;
	}
	else if (AngleDelta >= -157.5f && AngleDelta < -112.5f)
	{
		ForwardMove = -BotSpeed;
		SideMove = -BotSpeed;
		Button |= IN_BACK;
		Button |= IN_MOVELEFT;
	}
	else if (AngleDelta >= -112.5f && AngleDelta < -67.5f)
	{
		ForwardMove = 0.0f;
		SideMove = -BotSpeed;
		Button |= IN_MOVELEFT;
	}
	else if (AngleDelta >= -67.5f && AngleDelta < -22.5f)
	{
		ForwardMove = BotSpeed;
		SideMove = -BotSpeed;
		Button |= IN_FORWARD;
		Button |= IN_MOVELEFT;
	}
}

Vector GetVisiblePointOnPlayerFromObserver(edict_t* Observer, edict_t* TargetPlayer)
{
	Vector TargetCentre = UTIL_GetCentreOfEntity(TargetPlayer);

	TraceResult hit;
	UTIL_TraceLine(GetPlayerEyePosition(Observer), TargetCentre, ignore_monsters, ignore_glass, Observer->v.pContainingEntity, &hit);

	if (hit.flFraction >= 1.0f) { return TargetCentre; }

	AvHUser3 TargetClass = (AvHUser3)TargetPlayer->v.iuser3;

	// Only check the head and feet if we're not a short-arse (i.e. marine, fade or onos)
	if (TargetClass == AVH_USER3_MARINE_PLAYER || TargetClass == AVH_USER3_ALIEN_PLAYER4 || TargetClass == AVH_USER3_ALIEN_PLAYER5)
	{

		UTIL_TraceLine(GetPlayerEyePosition(Observer), GetPlayerEyePosition(TargetPlayer), ignore_monsters, ignore_glass, Observer->v.pContainingEntity, &hit);

		if (hit.flFraction >= 1.0f) { return GetPlayerEyePosition(TargetPlayer); }

		UTIL_TraceLine(GetPlayerEyePosition(Observer), GetPlayerBottomOfCollisionHull(TargetPlayer) + Vector(0.0f, 0.0f, 5.0f), ignore_monsters, ignore_glass, Observer->v.pContainingEntity, &hit);

		if (hit.flFraction >= 1.0f) { return GetPlayerBottomOfCollisionHull(TargetPlayer) + Vector(0.0f, 0.0f, 5.0f); }
	}

	// Skulks and Onos are long bois, so check to make sure they don't have their boots or snoots poking out round a corner...
	if (TargetClass == AVH_USER3_ALIEN_PLAYER1 || TargetClass == AVH_USER3_ALIEN_PLAYER5)
	{
		float Length = (TargetClass == AVH_USER3_ALIEN_PLAYER1) ? 55.0f : 110.0f;

		Vector ForwardVector = UTIL_GetForwardVector(TargetPlayer->v.angles);

		Vector MinLoc = TargetCentre - (ForwardVector * Length);

		UTIL_TraceLine(GetPlayerEyePosition(Observer), MinLoc, ignore_monsters, ignore_glass, Observer->v.pContainingEntity, &hit);

		if (hit.flFraction >= 1.0f) { return MinLoc; }

		Vector MaxLoc = TargetCentre + (ForwardVector * Length);

		UTIL_TraceLine(GetPlayerEyePosition(Observer), MaxLoc, ignore_monsters, ignore_glass, Observer->v.pContainingEntity, &hit);

		return (hit.flFraction >= 1.0f) ? MaxLoc : ZERO_VECTOR;
	}

	return ZERO_VECTOR;
}
