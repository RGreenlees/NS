
#include "AvHAIConfig.h"
#include "AvHAIMath.h"

#include "AvHServerUtil.h"

#include <unordered_map>
#include <algorithm>
#include <random>

BotFillTiming CurrentBotFillTiming = FILLTIMING_ALLHUMANS;

float MaxAIMatchTimeMinutes = 90.0f;

float RelocationChance = 0.1f;

std::unordered_map<std::string, TeamSizeDefinitions> TeamSizeMap;

bot_skill BotSkillLevels[4];

std::vector<AvHMessageID> ChamberSequence;

std::default_random_engine rng;
bool bRNGSeeded = false;

string DefaultBotNames[MAX_PLAYERS] = { "MrRobot",
                                    "Wall-E",
                                    "BeepBoop",
                                    "Robotnik",
                                    "JonnyAutomaton",
                                    "Burninator",
                                    "SteelDeath",
                                    "Meatbag",
                                    "Undertaker",
                                    "Botini",
                                    "Robottle",
                                    "Rusty",
                                    "HeavyMetal",
                                    "Combot",
                                    "BagelLover",
                                    "Screwdriver",
                                    "LoveBug",
                                    "iSmash",
                                    "Chippy",
                                    "Baymax",
                                    "BoomerBot",
                                    "Jarvis",
                                    "Marvin",
                                    "Data",
                                    "Scrappy",
                                    "Mortis",
                                    "TerrorHertz",
                                    "Omicron",
                                    "Herbie",
                                    "Robogeddon",
                                    "Velociripper",
                                    "TerminalFerocity"
};

vector<string> BotNames;
int CurrentNameIndex = 0;

char BotPrefix[32] = "";

extern cvar_t avh_botskill;
extern cvar_t avh_botallowlerk;
extern cvar_t avh_botallowfade;
extern cvar_t avh_botallowonos;
extern cvar_t avh_botcommanderwait;
extern cvar_t avh_botlerkcooldown;
extern cvar_t avh_botmaxstucktime;

float CONFIG_GetCommanderWaitTime()
{
    return avh_botcommanderwait.value;
}

float CONFIG_GetLerkCooldown()
{
    return avh_botlerkcooldown.value;
}

bool CONFIG_IsLerkAllowed()
{
    return avh_botallowlerk.value > 0;
}

bool CONFIG_IsFadeAllowed()
{
    return avh_botallowfade.value > 0;
}

bool CONFIG_IsOnosAllowed()
{
    return avh_botallowonos.value > 0;
}

bool CONFIG_IsRelocationAllowed()
{
    return RelocationChance > 0.0f;
}

float CONFIG_GetRelocationChance()
{
    return RelocationChance;
}

float CONFIG_GetMaxStuckTime()
{
    return avh_botmaxstucktime.value;
}

float CONFIG_GetMaxAIMatchTimeMinutes()
{
    return MaxAIMatchTimeMinutes;
}

string CONFIG_GetBotPrefix()
{
    return string(BotPrefix);
}

int CONFIG_GetTeamASizeForMap(const char* MapName)
{
    std::string s = MapName;
    std::unordered_map<std::string, TeamSizeDefinitions>::const_iterator got = TeamSizeMap.find(s);

    if (got == TeamSizeMap.end())
    {
        return TeamSizeMap["default"].TeamASize;
    }
    else
    {
        return got->second.TeamASize;
    }
}

int CONFIG_GetTeamBSizeForMap(const char* MapName)
{
    std::string s = MapName;
    std::unordered_map<std::string, TeamSizeDefinitions>::const_iterator got = TeamSizeMap.find(s);

    if (got == TeamSizeMap.end())
    {
        return TeamSizeMap["default"].TeamBSize;
    }
    else
    {
        return got->second.TeamBSize;
    }
}

AvHMessageID CONFIG_GetHiveTechAtIndex(const int Index)
{
    if (Index < 0 || Index > 2) { return MESSAGE_NULL; }

    return ChamberSequence[Index];
}

bot_skill CONFIG_GetBotSkillLevel()
{
    int index = clampi((int)avh_botskill.value, 0, 3);

    return BotSkillLevels[index];
}

void CONFIG_PopulateBotNames()
{
    BotNames.clear();

    string BotConfigFile = string(getModDirectory()) + "/botnames.txt";

    const char* filename = BotConfigFile.c_str();

    std::ifstream cFile(filename);
    if (cFile.is_open())
    {
        std::string line;
        while (getline(cFile, line))
        {
            if (line[0] == '/' || line.empty())
                continue;

            BotNames.push_back(line);
        }
    }

    if (BotNames.size() > 2)
    {
        std::shuffle(begin(BotNames), end(BotNames), rng);
    }

    vector<string> DefaultNames;

    // Ensure we have 32 names for all bots
    for (int i = BotNames.size(); i < MAX_PLAYERS; i++)
    {
        DefaultNames.push_back(DefaultBotNames[i]);
    }

    if (DefaultNames.size() > 2)
    {
        std::shuffle(begin(DefaultNames), end(DefaultNames), rng);
    }

    BotNames.insert(BotNames.end(), DefaultNames.begin(), DefaultNames.end());

    CurrentNameIndex = 0;
    
}

string CONFIG_GetNextBotName()
{
    if (BotNames.size() == 0) { return "Bot"; }

    string Result = BotNames[CurrentNameIndex];

    CurrentNameIndex++;

    if (CurrentNameIndex >= BotNames.size())
    {
        CurrentNameIndex = 0;
    }

    return Result;
}

void CONFIG_ParseConfigFile()
{

    BotSkillLevels[0].marine_bot_reaction_time = 0.4f;
    BotSkillLevels[0].marine_bot_aim_skill = 0.1f;
    BotSkillLevels[0].marine_bot_motion_tracking_skill = 0.1f;
    BotSkillLevels[0].marine_bot_view_speed = 0.5f;

    BotSkillLevels[0].alien_bot_reaction_time = 0.4f;
    BotSkillLevels[0].alien_bot_aim_skill = 0.2f;
    BotSkillLevels[0].alien_bot_motion_tracking_skill = 0.2f;    
    BotSkillLevels[0].alien_bot_view_speed = 0.75f;

    BotSkillLevels[1].marine_bot_reaction_time = 0.2f;
    BotSkillLevels[1].marine_bot_aim_skill = 0.5f;
    BotSkillLevels[1].marine_bot_motion_tracking_skill = 0.4f;
    BotSkillLevels[1].marine_bot_view_speed = 1.0f;

    BotSkillLevels[1].alien_bot_reaction_time = 0.2f;
    BotSkillLevels[1].alien_bot_aim_skill = 0.5f;
    BotSkillLevels[1].alien_bot_motion_tracking_skill = 0.5f;
    BotSkillLevels[1].alien_bot_view_speed = 1.3f;

    BotSkillLevels[2].marine_bot_reaction_time = 0.2f;
    BotSkillLevels[2].marine_bot_aim_skill = 0.6f;
    BotSkillLevels[2].marine_bot_motion_tracking_skill = 0.6f;
    BotSkillLevels[2].marine_bot_view_speed = 1.5f;

    BotSkillLevels[2].alien_bot_reaction_time = 0.2f;
    BotSkillLevels[2].alien_bot_aim_skill = 0.8f;
    BotSkillLevels[2].alien_bot_motion_tracking_skill = 0.8f;
    BotSkillLevels[2].alien_bot_view_speed = 1.5f;

    BotSkillLevels[3].marine_bot_reaction_time = 0.1f;
    BotSkillLevels[3].marine_bot_aim_skill = 1.0f;
    BotSkillLevels[3].marine_bot_motion_tracking_skill = 1.0f;
    BotSkillLevels[3].marine_bot_view_speed = 2.0f;

    BotSkillLevels[3].alien_bot_reaction_time = 0.1f;
    BotSkillLevels[3].alien_bot_aim_skill = 1.0f;
    BotSkillLevels[3].alien_bot_motion_tracking_skill = 1.0f;
    BotSkillLevels[3].alien_bot_view_speed = 2.0f;

    ChamberSequence.clear();
    ChamberSequence.push_back(ALIEN_BUILD_DEFENSE_CHAMBER);
    ChamberSequence.push_back(ALIEN_BUILD_MOVEMENT_CHAMBER);
    ChamberSequence.push_back(ALIEN_BUILD_SENSORY_CHAMBER);

    if (!bRNGSeeded)
    {
        srand(time(0));
        rng.seed(time(0));
        bRNGSeeded = true;
    }

    std::shuffle(std::begin(ChamberSequence), std::end(ChamberSequence), rng);

    string BotConfigFile = string(getModDirectory()) + "/nsbots.ini";

    const char* filename = BotConfigFile.c_str();

    std::ifstream cFile(filename);
    if (cFile.is_open())
    {
        std::string line;
        int CurrSkillIndex = -1;
        bool bSkillLevelWarningGiven = false;

        while (getline(cFile, line))
        {
            line.erase(std::remove_if(line.begin(), line.end(), ::isspace),
                line.end());
            if (line[0] == '#' || line.empty())
                continue;
            auto delimiterPos = line.find("=");
            auto key = line.substr(0, delimiterPos);
            auto value = line.substr(delimiterPos + 1);

            const char* keyChar = key.c_str();

            if (!stricmp(keyChar, "TeamSize"))
            {
                auto mapDelimiterPos = value.find(":");

                if (mapDelimiterPos == std::string::npos)
                {
                    continue;
                }

                auto mapName = value.substr(0, mapDelimiterPos);
                auto teamSizes = value.substr(mapDelimiterPos + 1);
                auto sizeDelimiterPos = teamSizes.find("/");
                if (sizeDelimiterPos == std::string::npos)
                {
                    continue;
                }
                auto marineSize = teamSizes.substr(0, sizeDelimiterPos);
                auto alienSize = teamSizes.substr(sizeDelimiterPos + 1);

                int iMarineSize = atoi(marineSize.c_str());
                int iAlienSize = atoi(alienSize.c_str());

                if (iMarineSize >= 0 && iMarineSize <= 32 && iAlienSize >= 0 && iAlienSize <= 32)
                {
                    TeamSizeMap[mapName].TeamASize = atoi(marineSize.c_str());
                    TeamSizeMap[mapName].TeamBSize = atoi(alienSize.c_str());
                }

                continue;
            }

            if (!stricmp(keyChar, "Prefix"))
            {
                sprintf(BotPrefix, value.c_str());

                continue;
            }

            if (!stricmp(keyChar, "RelocationChance"))
            {
                float RelocationValue = std::stof(value.c_str());
                RelocationChance = RelocationValue;

                continue;
            }

            if (!stricmp(keyChar, "MaxAIMatchTime"))
            {
                float MaxMinutes = std::stof(value.c_str());
                MaxAIMatchTimeMinutes = MaxMinutes;

                continue;
            }

            if (!stricmp(keyChar, "BotFillTiming"))
            {                
                int FillSetting = atoi(value.c_str());
                FillSetting = clampi(FillSetting, 0, 2);
                CurrentBotFillTiming = (BotFillTiming)FillSetting;
                
                continue;
            }

            if (!stricmp(keyChar, "BotSkillLevel") || !stricmp(keyChar, "BotSkillName"))
            {
                CurrSkillIndex = std::stoi(value.c_str());
                CurrSkillIndex = clampi(CurrSkillIndex, 0, 3);

                continue;
            }

            if (!stricmp(keyChar, "MarineReactionTime"))
            {
                if (CurrSkillIndex > -1)
                {
                    float NewValue = std::stof(value.c_str());

                    BotSkillLevels[CurrSkillIndex].marine_bot_reaction_time = clampf(NewValue, 0.0f, 1.0f);
                }
                else
                {
                    if (!bSkillLevelWarningGiven)
                    {
                        ALERT(at_console, "No skill level has been defined in the config file. Use BotSkillLevel= followed by 0-3 before setting skill values");
                        bSkillLevelWarningGiven = true;
                    }
                }
                continue;
            }

            if (!stricmp(keyChar, "AlienReactionTime"))
            {
                if (CurrSkillIndex > -1)
                {
                    float NewValue = std::stof(value.c_str());

                    BotSkillLevels[CurrSkillIndex].alien_bot_reaction_time = clampf(NewValue, 0.0f, 1.0f);
                }
                else
                {
                    if (!bSkillLevelWarningGiven)
                    {
                        ALERT(at_logged, "No skill level has been defined in the config file. Use BotSkillLevel= followed by 0-3 before setting skill values");
                        bSkillLevelWarningGiven = true;
                    }
                }

                continue;
            }

            if (!stricmp(keyChar, "MarineAimSkill"))
            {
                if (CurrSkillIndex > -1)
                {
                    float NewValue = std::stof(value.c_str());

                    BotSkillLevels[CurrSkillIndex].marine_bot_aim_skill = clampf(NewValue, 0.0f, 1.0f);
                }
                else
                {
                    if (!bSkillLevelWarningGiven)
                    {
                        ALERT(at_logged, "No skill level has been defined in the config file. Use BotSkillLevel= followed by 0-3 before setting skill values");
                        bSkillLevelWarningGiven = true;
                    }
                }

                continue;
            }

            if (!stricmp(keyChar, "AlienAimSkill"))
            {
                if (CurrSkillIndex > -1)
                {
                    float NewValue = std::stof(value.c_str());

                    BotSkillLevels[CurrSkillIndex].alien_bot_aim_skill = clampf(NewValue, 0.0f, 1.0f);
                }
                else
                {
                    if (!bSkillLevelWarningGiven)
                    {
                        ALERT(at_console, "No skill level has been defined in the config file. Use BotSkillLevel= followed by 0-3 before setting skill values");
                        bSkillLevelWarningGiven = true;
                    }
                }

                continue;
            }

            if (!stricmp(keyChar, "MarineMovementTracking"))
            {
                if (CurrSkillIndex > -1)
                {
                    float NewValue = std::stof(value.c_str());

                    BotSkillLevels[CurrSkillIndex].marine_bot_motion_tracking_skill = clampf(NewValue, 0.0f, 1.0f);
                }
                else
                {
                    if (!bSkillLevelWarningGiven)
                    {
                        ALERT(at_console, "No skill level has been defined in the config file. Use BotSkillLevel= followed by 0-3 before setting skill values");
                        bSkillLevelWarningGiven = true;
                    }
                }

                continue;
            }

            if (!stricmp(keyChar, "AlienMovementTracking"))
            {
                if (CurrSkillIndex > -1)
                {
                    float NewValue = std::stof(value.c_str());

                    BotSkillLevels[CurrSkillIndex].alien_bot_motion_tracking_skill = clampf(NewValue, 0.0f, 1.0f);
                }
                else
                {
                    if (!bSkillLevelWarningGiven)
                    {
                        ALERT(at_console, "No skill level has been defined in the config file. Use BotSkillLevel= followed by 0-3 before setting skill values");
                        bSkillLevelWarningGiven = true;
                    }
                }

                continue;
            }

            if (!stricmp(keyChar, "MarineViewSpeed"))
            {
                if (CurrSkillIndex > -1)
                {
                    float NewValue = std::stof(value.c_str());

                    BotSkillLevels[CurrSkillIndex].marine_bot_view_speed = clampf(NewValue, 0.0f, 5.0f);
                }
                else
                {
                    if (!bSkillLevelWarningGiven)
                    {
                        ALERT(at_console, "No skill level has been defined in the config file. Use BotSkillLevel= followed by 0-3 before setting skill values");
                        bSkillLevelWarningGiven = true;
                    }
                }

                continue;
            }

            if (!stricmp(keyChar, "AlienViewSpeed"))
            {
                if (CurrSkillIndex > -1)
                {
                    float NewValue = std::stof(value.c_str());

                    BotSkillLevels[CurrSkillIndex].alien_bot_view_speed = clampf(NewValue, 0.0f, 5.0f);
                }
                else
                {
                    if (!bSkillLevelWarningGiven)
                    {
                        ALERT(at_console, "No skill level has been defined in the config file. Use BotSkillLevel= followed by 0-3 before setting skill values");
                        bSkillLevelWarningGiven = true;
                    }
                }

                continue;
            }

            if (!stricmp(keyChar, "ChamberSequence"))
            {
                auto firstTechDelimiter = value.find("/");

                if (firstTechDelimiter == std::string::npos)
                {
                    continue;
                }

                auto FirstTech = value.substr(0, firstTechDelimiter);
                auto NextTechs = value.substr(firstTechDelimiter + 1);

                auto SecondTechDelimiter = NextTechs.find("/");

                if (SecondTechDelimiter == std::string::npos)
                {
                    continue;
                }

                auto SecondTech = NextTechs.substr(0, SecondTechDelimiter);
                auto ThirdTech = NextTechs.substr(SecondTechDelimiter + 1);

                const char* FirstTechChar = FirstTech.c_str();
                const char* SecondTechChar = SecondTech.c_str();
                const char* ThirdTechChar = ThirdTech.c_str();

                if (!stricmp(FirstTechChar, "defense"))
                {
                    auto Element = std::find(ChamberSequence.begin(), ChamberSequence.end(), ALIEN_BUILD_DEFENSE_CHAMBER);
                    int Index = Element - ChamberSequence.begin();
                    std::swap(ChamberSequence[0], ChamberSequence[Index]);
                }
                else if (!stricmp(FirstTechChar, "movement"))
                {
                    auto Element = std::find(ChamberSequence.begin(), ChamberSequence.end(), ALIEN_BUILD_MOVEMENT_CHAMBER);
                    int Index = Element - ChamberSequence.begin();
                    std::swap(ChamberSequence[0], ChamberSequence[Index]);
                }
                else if (!stricmp(FirstTechChar, "sensory"))
                {
                    auto Element = std::find(ChamberSequence.begin(), ChamberSequence.end(), ALIEN_BUILD_SENSORY_CHAMBER);
                    int Index = Element - ChamberSequence.begin();
                    std::swap(ChamberSequence[0], ChamberSequence[Index]);
                }


                if (!stricmp(SecondTechChar, "defense"))
                {
                    auto Element = std::find(ChamberSequence.begin(), ChamberSequence.end(), ALIEN_BUILD_DEFENSE_CHAMBER);
                    int Index = Element - ChamberSequence.begin();
                    std::swap(ChamberSequence[1], ChamberSequence[Index]);
                }
                else if (!stricmp(SecondTechChar, "movement"))
                {
                    auto Element = std::find(ChamberSequence.begin(), ChamberSequence.end(), ALIEN_BUILD_MOVEMENT_CHAMBER);
                    int Index = Element - ChamberSequence.begin();
                    std::swap(ChamberSequence[1], ChamberSequence[Index]);
                }
                else if (!stricmp(SecondTechChar, "sensory"))
                {
                    auto Element = std::find(ChamberSequence.begin(), ChamberSequence.end(), ALIEN_BUILD_SENSORY_CHAMBER);
                    int Index = Element - ChamberSequence.begin();
                    std::swap(ChamberSequence[1], ChamberSequence[Index]);
                }

                if (!stricmp(ThirdTechChar, "defense"))
                {
                    auto Element = std::find(ChamberSequence.begin(), ChamberSequence.end(), ALIEN_BUILD_DEFENSE_CHAMBER);
                    int Index = Element - ChamberSequence.begin();
                    std::swap(ChamberSequence[2], ChamberSequence[Index]);
                }
                else if (!stricmp(ThirdTechChar, "movement"))
                {
                    auto Element = std::find(ChamberSequence.begin(), ChamberSequence.end(), ALIEN_BUILD_MOVEMENT_CHAMBER);
                    int Index = Element - ChamberSequence.begin();
                    std::swap(ChamberSequence[2], ChamberSequence[Index]);
                }
                else if (!stricmp(ThirdTechChar, "sensory"))
                {
                    auto Element = std::find(ChamberSequence.begin(), ChamberSequence.end(), ALIEN_BUILD_SENSORY_CHAMBER);
                    int Index = Element - ChamberSequence.begin();
                    std::swap(ChamberSequence[2], ChamberSequence[Index]);
                }

                continue;
            }
        }
    }
    else
    {
        ALERT(at_console, "nsbots.ini was not found in the NS mod folder. You can regenerate it with the console command 'sv_regenbotini'");
    }
}

BotFillTiming CONFIG_GetBotFillTiming()
{
    return CurrentBotFillTiming;
}

void CONFIG_RegenerateIniFile()
{
    string BotConfigFile = string(getModDirectory()) + "/nsbots.ini";

    const char* filename = BotConfigFile.c_str();

    FILE* NewConfigFile = fopen(filename, "w+");

    if (!NewConfigFile)
    {
        char ErrMsg[256];
        sprintf(ErrMsg, "Unable to write to %s, please ensure the user has privileges\n", filename);
        g_engfuncs.pfnServerPrint(ErrMsg);
        return;
    }

    fprintf(NewConfigFile, "### General bot settings ###\n\n");

    fprintf(NewConfigFile, "# What prefix to put in front of a bot's name (can leave blank)\n");
    fprintf(NewConfigFile, "Prefix=[BOT]\n\n");

    fprintf(NewConfigFile, "# After this many minutes into a match, the bots will leave the game if there are no humans playing\n");
    fprintf(NewConfigFile, "# Helps prevent stalemates and bugs that happen after extremely long matches\n");
    fprintf(NewConfigFile, "# Default = 90 minutes\n");
    fprintf(NewConfigFile, "MaxAIMatchTime=90\n\n");

    fprintf(NewConfigFile, "# When should the server start adding bots? Note: bots will always be added after round start regardless\n");
    fprintf(NewConfigFile, "# 0 = On map load (after 5 second grace period)\n");
    fprintf(NewConfigFile, "# 1 = When all humans have joined a team (i.e. no more humans left in ready room)\n");
    fprintf(NewConfigFile, "# 2 = When the round has started (after countdown)\n");
    fprintf(NewConfigFile, "BotFillTiming=1\n\n");

    fprintf(NewConfigFile, "# Chance the AI Commander will try to relocate at the beginning of the game\n");
    fprintf(NewConfigFile, "# Value is a decimal between 0.0 and 1.0, with 0 being never and 1 being always\n");
    fprintf(NewConfigFile, "RelocationChance=0.1\n\n\n");


    fprintf(NewConfigFile, "### Skill Settings ###\n\n");

    fprintf(NewConfigFile, "# Bot skill settings. There are 4 settings from 0 - 3 for easiest - hardest\n");
    fprintf(NewConfigFile, "# Use BotSkillName=<index> to start defining a skill level, then the following:\n");
    fprintf(NewConfigFile, "# ReactionTime = How quickly in seconds the bot will react to sighting enemies\n");
    fprintf(NewConfigFile, "# AimSkill = How accurately the bot can lock sights on you after seeing you (0.0 - 1.0)\n");
    fprintf(NewConfigFile, "# MovementTracking = How accurately the bot can follow a moving target (0.0 - 1.0)\n");
    fprintf(NewConfigFile, "# ViewSpeed = How fast the bot can swivel its view (0.1 - 2.0)\n");
    fprintf(NewConfigFile, "# Set the difficulty using the 'mp_botskill' cvar (0 - 3)\n\n");

    fprintf(NewConfigFile, "BotSkillLevel=0\n");
    fprintf(NewConfigFile, "MarineReactionTime=0.4\n");
    fprintf(NewConfigFile, "MarineAimSkill=0.1\n");
    fprintf(NewConfigFile, "MarineMovementTracking=0.1\n");
    fprintf(NewConfigFile, "MarineViewSpeed=0.5\n");
    fprintf(NewConfigFile, "AlienReactionTime=0.4\n");
    fprintf(NewConfigFile, "AlienAimSkill=0.2\n");
    fprintf(NewConfigFile, "AlienMovementTracking=0.2\n");
    fprintf(NewConfigFile, "AlienViewSpeed=0.75\n\n");

    fprintf(NewConfigFile, "BotSkillLevel=1\n");
    fprintf(NewConfigFile, "MarineReactionTime=0.2\n");
    fprintf(NewConfigFile, "MarineAimSkill=0.5\n");
    fprintf(NewConfigFile, "MarineMovementTracking=0.4\n");
    fprintf(NewConfigFile, "MarineViewSpeed=1.0\n");
    fprintf(NewConfigFile, "AlienReactionTime=0.2\n");
    fprintf(NewConfigFile, "AlienAimSkill=0.5\n");
    fprintf(NewConfigFile, "AlienMovementTracking=0.5\n");
    fprintf(NewConfigFile, "AlienViewSpeed=1.3\n\n");

    fprintf(NewConfigFile, "BotSkillLevel=2\n");
    fprintf(NewConfigFile, "MarineReactionTime=0.2\n");
    fprintf(NewConfigFile, "MarineAimSkill=0.6\n");
    fprintf(NewConfigFile, "MarineMovementTracking=0.6\n");
    fprintf(NewConfigFile, "MarineViewSpeed=1.5\n");
    fprintf(NewConfigFile, "AlienReactionTime=0.2\n");
    fprintf(NewConfigFile, "AlienAimSkill=0.8\n");
    fprintf(NewConfigFile, "AlienMovementTracking=0.8\n");
    fprintf(NewConfigFile, "AlienViewSpeed=1.5\n\n");

    fprintf(NewConfigFile, "BotSkillLevel=3\n");
    fprintf(NewConfigFile, "MarineReactionTime=0.1\n");
    fprintf(NewConfigFile, "MarineAimSkill=1.0\n");
    fprintf(NewConfigFile, "MarineMovementTracking=1.0\n");
    fprintf(NewConfigFile, "MarineViewSpeed=2.0\n");
    fprintf(NewConfigFile, "AlienReactionTime=0.1\n");
    fprintf(NewConfigFile, "AlienAimSkill=1.0\n");
    fprintf(NewConfigFile, "AlienMovementTracking=1.0\n");
    fprintf(NewConfigFile, "AlienViewSpeed=2.0\n\n\n");


    fprintf(NewConfigFile, "# Desired team sizes. Only used if bot fill mode is 'fillteams'\n");
    fprintf(NewConfigFile, "# Format is TeamSize=mapname:nummarines/numaliens\n");
    fprintf(NewConfigFile, "# 'default' will be used if playing a map not listed below\n");
    fprintf(NewConfigFile, "TeamSize=default:7/7\n");
    fprintf(NewConfigFile, "TeamSize=ns_machina:8/8\n");
    fprintf(NewConfigFile, "TeamSize=ns_ragnarok:8/8\n");
    fprintf(NewConfigFile, "TeamSize=ns_askr_b5:8/8\n");
    fprintf(NewConfigFile, "TeamSize=ns_askr_b6:8/8\n");
    fprintf(NewConfigFile, "TeamSize=co_faceoff:4/4\n");
    fprintf(NewConfigFile, "TeamSize=co_core:4/4\n");
    fprintf(NewConfigFile, "TeamSize=co_pulse:6/6\n");
    fprintf(NewConfigFile, "TeamSize=co_ulysses:6/6\n");
    fprintf(NewConfigFile, "TeamSize=co_niveus:5/5\n");
    fprintf(NewConfigFile, "TeamSize=co_kestrel:5/5\n\n\n");


    fprintf(NewConfigFile, "### Alien Settings ###\n\n");

    fprintf(NewConfigFile, "# Preferred chamber sequence. Valid entries are 'defense', 'movement' and 'sensory'. Separate sequence with forward slash\n");
    fprintf(NewConfigFile, "# You can also use ? for random, so if you want movement always first but then defense and sensory at random, use\n");
    fprintf(NewConfigFile, "# ChamberSequence:movement/?/?\n");
    fprintf(NewConfigFile, "# Or if you want sensory always last, but movement and defence random, use\n");
    fprintf(NewConfigFile, "# ChamberSequence=?/?/sensory\n");
    fprintf(NewConfigFile, "ChamberSequence=?/?/?\n");

    fflush(NewConfigFile);
    fclose(NewConfigFile);

    char ErrMsg[256];
    sprintf(ErrMsg, "Regenerated %s\n", filename);
    g_engfuncs.pfnServerPrint(ErrMsg);

}