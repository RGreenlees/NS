#include "AvHAISoundQueue.h"
#include "AvHAIPlayerManager.h"

std::vector<AvHAISound> PendingSounds;

void AISND_RegisterNewSound(int EntIndex, float* NewLocation, EAISoundType NewSoundType, float Volume)
{
	if (!AIMGR_IsBotEnabled() || Volume < 0.01f) { return; }

	AvHAISound NewSound;
	NewSound.EntIndex = EntIndex;
	NewSound.SoundLocation[0] = NewLocation[0];
	NewSound.SoundLocation[1] = NewLocation[1];
	NewSound.SoundLocation[2] = NewLocation[2];
	NewSound.Volume = Volume;
	NewSound.SoundType = NewSoundType;

	PendingSounds.push_back(NewSound);
}

AvHAISound AISND_PopSound()
{
	if (PendingSounds.size() == 0) { return AvHAISound(); }

	AvHAISound Result = PendingSounds.back();

	PendingSounds.pop_back();

	return Result;
}

void AISND_ClearSounds()
{
	PendingSounds.clear();
}