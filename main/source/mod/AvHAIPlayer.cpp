
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
			return AINAV_ProgressMoveTask(this, CurrentMoveTask, NextFrameMovementInput);
		default:
			return EAINavMoveResult::NAV_MOVE_NOPATH;
	}
}

void AvHAIPlayer::Jump(AvHAIMovementInput& Outputs, bool bDuckJump) const
{
	if (IsOnGround())
	{
		if (gpGlobals->time - BotNavInfo.LandedTime >= 0.1f)
		{
			Outputs.Button |= IN_JUMP;
			Outputs.bHasAttemptedJump = true;
		}
	}
	else
	{
		if (bDuckJump)
		{
			Outputs.Button |= IN_DUCK;
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
	if (ShouldThink())
	{
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
	NextFrameMovementInput.Clear();

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

	// Thanks to The Storm (ePODBot) for this one, finally fixed the bot running speed!
	int AdjustedTimeMS = (int)roundf((gpGlobals->time - LastServerUpdateTime) * 1000.0f);

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
