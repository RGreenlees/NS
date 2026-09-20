#ifndef AVH_AI_SOUND_QUEUE
#define AVH_AI_SOUND_QUEUE


#include <vector>

// Sound types affect how audible they are to bots, how easy it is to pinpoint the location of the sound etc
enum class EAISoundType
{
	AI_SOUND_NONE = 0,		// Blank sound
	AI_SOUND_FOOTSTEP, 		// Footstep sound
	AI_SOUND_LANDING,		// Landing sound, THUD
	AI_SOUND_SHOOT,			// Pew pew
	AI_SOUND_VOICELINE,		// Player played a voice line (e.g. "Need a medpack")
	AI_SOUND_OTHER			// Miscellaneous sound e.g. building
};

struct AvHAISound
{
	int EntIndex = 0;
	float SoundLocation[3] = {0.0f, 0.0f, 0.0f};
	float Volume = 1.0f;
	EAISoundType SoundType = EAISoundType::AI_SOUND_NONE;
};


void AISND_RegisterNewSound(int EntIndex, float* NewLocation, EAISoundType NewSoundType, float Volume = 1.0f);

AvHAISound AISND_PopSound();

void AISND_ClearSounds();


#endif