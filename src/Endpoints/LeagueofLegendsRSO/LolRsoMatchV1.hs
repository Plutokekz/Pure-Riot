{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Endpoints.LeagueofLegendsRSO.LolRsoMatchV1 where

import           Data.Aeson          (FromJSON (..), defaultOptions,
                                      fieldLabelModifier, genericParseJSON)
import           Data.Aeson.Types    (withObject, (.:))
import           Data.Map            (Map)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

data MatchDto = MatchDto
  { {- | Match metadata.-}
    metadata :: MetadataDto,
    {- | Match info.-}
    info     :: InfoDto
  }
  deriving (Show, Eq, Generic)

instance FromJSON MatchDto

data MetadataDto = MetadataDto
  { {- | Match data version.-}
    dataVersion  :: String,
    {- | Match id.-}
    matchId      :: String,
    {- | A list of participant PUUIDs.-}
    participants :: [String]
  }
  deriving (Show, Eq, Generic)

instance FromJSON MetadataDto

data InfoDto = InfoDto
  { {- | Refer to indicate if the game ended in termination.-}
    endOfGameResult    :: String,
    {- | Unix timestamp for when the game is created on the game server (i.e., the loading screen).-}
    gameCreation       :: Int,
    {- | Prior to patch 11.20, this field returns the game length in milliseconds calculated from gameEndTimestamp - gameStartTimestamp. Post patch 11.20, this field returns the max timePlayed of any participant in the game in seconds, which makes the behavior of this field consistent with that of match-v4. The best way to handling the change in this field is to treat the value as milliseconds if the gameEndTimestamp field isn't in the response and to treat the value as seconds if gameEndTimestamp is in the response.-}
    gameDuration       :: Int,
    {- | Unix timestamp for when match ends on the game server. This timestamp can occasionally be significantly longer than when the match "ends". The most reliable way of determining the timestamp for the end of the match would be to add the max time played of any participant to the gameStartTimestamp. This field was added to match-v5 in patch 11.20 on Oct 5th, 2021.-}
    gameEndTimestamp   :: Int,
    gameId             :: Int,
    {- | Refer to the Game Constants documentation.-}
    gameMode           :: String,
    gameName           :: String,
    {- | Unix timestamp for when match starts on the game server.-}
    gameStartTimestamp :: Int,
    gameType           :: String,
    {- | The first two parts can be used to determine the patch a game was played on.-}
    gameVersion        :: String,
    {- | Refer to the Game Constants documentation.-}
    mapId              :: Int,
    participants       :: [ParticipantDto],
    {- | Platform where the match was played.-}
    platformId         :: String,
    {- | Refer to the Game Constants documentation.-}
    queueId            :: Int,
    teams              :: [TeamDto],
    {- | Tournament code used to generate the match. This field was added to match-v5 in patch 11.13 on June 23rd, 2021.-}
    tournamentCode     :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON InfoDto

data ParticipantDto = ParticipantDto
  { {- | Yellow crossed swords-}
    allInPings                     :: Int,
    {- | Green flag-}
    assistMePings                  :: Int,
    assists                        :: Int,
    baronKills                     :: Int,
    bountyLevel                    :: Int,
    champExperience                :: Int,
    champLevel                     :: Int,
    {- | Prior to patch 11.4, on Feb 18th, 2021, this field returned invalid championIds. We recommend determining the champion based on the championName field for matches played prior to patch 11.4.-}
    championId                     :: Int,
    championName                   :: String,
    {- | Blue generic ping (ALT+click)-}
    commandPings                   :: Int,
    {- | This field is currently only utilized for Kayn's transformations. (Legal values: 0 - None, 1 - Slayer, 2 - Assassin)-}
    championTransform              :: Int,
    consumablesPurchased           :: Int,
    challenges                     :: ChallengesDto,
    damageDealtToBuildings         :: Int,
    damageDealtToObjectives        :: Int,
    damageDealtToTurrets           :: Int,
    damageSelfMitigated            :: Int,
    deaths                         :: Int,
    detectorWardsPlaced            :: Int,
    doubleKills                    :: Int,
    dragonKills                    :: Int,
    eligibleForProgression         :: Bool,
    {- | Yellow questionmark-}
    enemyMissingPings              :: Int,
    {- | Red eyeball-}
    enemyVisionPings               :: Int,
    firstBloodAssist               :: Bool,
    firstBloodKill                 :: Bool,
    firstTowerAssist               :: Bool,
    firstTowerKill                 :: Bool,
    {- | This is an offshoot of the OneStone challenge. The code checks if a spell with the same instance ID does the final point of damage to at least 2 Champions. It doesn't matter if they're enemies, but you cannot hurt your friends.-}
    gameEndedInEarlySurrender      :: Bool,
    gameEndedInSurrender           :: Bool,
    holdPings                      :: Int,
    {- | Yellow circle with horizontal line-}
    getBackPings                   :: Int,
    goldEarned                     :: Int,
    goldSpent                      :: Int,
    {- | Both individualPosition and teamPosition are computed by the game server and are different versions of the most likely position played by a player. The individualPosition is the best guess for which position the player actually played in isolation of anything else. The teamPosition is the best guess for which position the player actually played if we add the constraint that each team must have one top player, one jungle, one middle, etc. Generally the recommendation is to use the teamPosition field over the individualPosition field.-}
    individualPosition             :: String,
    inhibitorKills                 :: Int,
    inhibitorTakedowns             :: Int,
    inhibitorsLost                 :: Int,
    item0                          :: Int,
    item1                          :: Int,
    item2                          :: Int,
    item3                          :: Int,
    item4                          :: Int,
    item5                          :: Int,
    item6                          :: Int,
    itemsPurchased                 :: Int,
    killingSprees                  :: Int,
    kills                          :: Int,
    lane                           :: String,
    largestCriticalStrike          :: Int,
    largestKillingSpree            :: Int,
    largestMultiKill               :: Int,
    longestTimeSpentLiving         :: Int,
    magicDamageDealt               :: Int,
    magicDamageDealtToChampions    :: Int,
    magicDamageTaken               :: Int,
    missions                       :: MissionsDto,
    {- | neutralMinionsKilled = mNeutralMinionsKilled, which is incremented on kills of kPet and kJungleMonster-}
    neutralMinionsKilled           :: Int,
    {- | Green ward-}
    needVisionPings                :: Int,
    nexusKills                     :: Int,
    nexusTakedowns                 :: Int,
    nexusLost                      :: Int,
    objectivesStolen               :: Int,
    objectivesStolenAssists        :: Int,
    {- | Blue arrow pointing at ground-}
    onMyWayPings                   :: Int,
    participantId                  :: Int,
    playerScore0                   :: Int,
    playerScore1                   :: Int,
    playerScore2                   :: Int,
    playerScore3                   :: Int,
    playerScore4                   :: Int,
    playerScore5                   :: Int,
    playerScore6                   :: Int,
    playerScore7                   :: Int,
    playerScore8                   :: Int,
    playerScore9                   :: Int,
    playerScore10                  :: Int,
    playerScore11                  :: Int,
    pentaKills                     :: Int,
    perks                          :: PerksDto,
    physicalDamageDealt            :: Int,
    physicalDamageDealtToChampions :: Int,
    physicalDamageTaken            :: Int,
    placement                      :: Int,
    playerAugment1                 :: Int,
    playerAugment2                 :: Int,
    playerAugment3                 :: Int,
    playerAugment4                 :: Int,
    playerSubteamId                :: Int,
    {- | Green minion-}
    pushPings                      :: Int,
    profileIcon                    :: Int,
    puuid                          :: String,
    quadraKills                    :: Int,
    riotIdGameName                 :: String,
    riotIdTagline                  :: String,
    role                           :: String,
    sightWardsBoughtInGame         :: Int,
    spell1Casts                    :: Int,
    spell2Casts                    :: Int,
    spell3Casts                    :: Int,
    spell4Casts                    :: Int,
    subteamPlacement               :: Int,
    summoner1Casts                 :: Int,
    summoner1Id                    :: Int,
    summoner2Casts                 :: Int,
    summoner2Id                    :: Int,
    summonerId                     :: String,
    summonerLevel                  :: Int,
    summonerName                   :: String,
    teamEarlySurrendered           :: Bool,
    teamId                         :: Int,
    {- | Both individualPosition and teamPosition are computed by the game server and are different versions of the most likely position played by a player. The individualPosition is the best guess for which position the player actually played in isolation of anything else. The teamPosition is the best guess for which position the player actually played if we add the constraint that each team must have one top player, one jungle, one middle, etc. Generally the recommendation is to use the teamPosition field over the individualPosition field.-}
    teamPosition                   :: String,
    timeCCingOthers                :: Int,
    timePlayed                     :: Int,
    totalAllyJungleMinionsKilled   :: Int,
    totalDamageDealt               :: Int,
    totalDamageDealtToChampions    :: Int,
    totalDamageShieldedOnTeammates :: Int,
    totalDamageTaken               :: Int,
    totalEnemyJungleMinionsKilled  :: Int,
    {- | Whenever positive health is applied (which translates to all heals in the game but not things like regeneration), totalHeal is incremented by the amount of health received. This includes healing enemies, jungle monsters, yourself, etc-}
    totalHeal                      :: Int,
    {- | Whenever positive health is applied (which translates to all heals in the game but not things like regeneration), totalHealsOnTeammates is incremented by the amount of health received.  This is post modified, so if you heal someone missing 5 health for 100 you will get +5 totalHealsOnTeammates-}
    totalHealsOnTeammates          :: Int,
    {- | totalMillionsKilled = mMinionsKilled, which is only incremented on kills of kTeamMinion, kMeleeLaneMinion, kSuperLaneMinion, kRangedLaneMinion and kSiegeLaneMinion-}
    totalMinionsKilled             :: Int,
    totalTimeCCDealt               :: Int,
    totalTimeSpentDead             :: Int,
    totalUnitsHealed               :: Int,
    tripleKills                    :: Int,
    trueDamageDealt                :: Int,
    trueDamageDealtToChampions     :: Int,
    trueDamageTaken                :: Int,
    turretKills                    :: Int,
    turretTakedowns                :: Int,
    turretsLost                    :: Int,
    unrealKills                    :: Int,
    visionScore                    :: Int,
    visionClearedPings             :: Int,
    visionWardsBoughtInGame        :: Int,
    wardsKilled                    :: Int,
    wardsPlaced                    :: Int,
    win                            :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON ParticipantDto

data ChallengesDto = ChallengesDto
  { assistStreakCount12                       :: Int,
    baronBuffGoldAdvantageOverThreshold       :: Int,
    controlWardTimeCoverageInRiverOrEnemyHalf :: Double,
    earliestBaron                             :: Int,
    earliestDragonTakedown                    :: Int,
    earliestElderDragon                       :: Int,
    earlyLaningPhaseGoldExpAdvantage          :: Int,
    fasterSupportQuestCompletion              :: Int,
    fastestLegendary                          :: Int,
    hadAfkTeammate                            :: Int,
    highestChampionDamage                     :: Int,
    highestCrowdControlScore                  :: Int,
    highestWardKills                          :: Int,
    junglerKillsEarlyJungle                   :: Int,
    killsOnLanersEarlyJungleAsJungler         :: Int,
    laningPhaseGoldExpAdvantage               :: Int,
    legendaryCount                            :: Int,
    maxCsAdvantageOnLaneOpponent              :: Double,
    maxLevelLeadLaneOpponent                  :: Int,
    mostWardsDestroyedOneSweeper              :: Int,
    mythicItemUsed                            :: Int,
    playedChampSelectPosition                 :: Int,
    soloTurretsLategame                       :: Int,
    takedownsFirst25Minutes                   :: Int,
    teleportTakedowns                         :: Int,
    thirdInhibitorDestroyedTime               :: Int,
    threeWardsOneSweeperCount                 :: Int,
    visionScoreAdvantageLaneOpponent          :: Double,
    infernalScalePickup                       :: Int,
    fistBumpParticipation                     :: Int,
    voidMonsterKill                           :: Int,
    abilityUses                               :: Int,
    acesBefore15Minutes                       :: Int,
    alliedJungleMonsterKills                  :: Double,
    baronTakedowns                            :: Int,
    blastConeOppositeOpponentCount            :: Int,
    bountyGold                                :: Int,
    buffsStolen                               :: Int,
    completeSupportQuestInTime                :: Int,
    controlWardsPlaced                        :: Int,
    damagePerMinute                           :: Double,
    damageTakenOnTeamPercentage               :: Double,
    dancedWithRiftHerald                      :: Int,
    deathsByEnemyChamps                       :: Int,
    dodgeSkillShotsSmallWindow                :: Int,
    doubleAces                                :: Int,
    dragonTakedowns                           :: Int,
    legendaryItemUsed                         :: [Int],
    effectiveHealAndShielding                 :: Double,
    elderDragonKillsWithOpposingSoul          :: Int,
    elderDragonMultikills                     :: Int,
    enemyChampionImmobilizations              :: Int,
    enemyJungleMonsterKills                   :: Double,
    epicMonsterKillsNearEnemyJungler          :: Int,
    epicMonsterKillsWithin30SecondsOfSpawn    :: Int,
    epicMonsterSteals                         :: Int,
    epicMonsterStolenWithoutSmite             :: Int,
    firstTurretKilled                         :: Int,
    firstTurretKilledTime                     :: Double,
    flawlessAces                              :: Int,
    fullTeamTakedown                          :: Int,
    gameLength                                :: Double,
    getTakedownsInAllLanesEarlyJungleAsLaner  :: Int,
    goldPerMinute                             :: Double,
    hadOpenNexus                              :: Int,
    immobilizeAndKillWithAlly                 :: Int,
    initialBuffCount                          :: Int,
    initialCrabCount                          :: Int,
    jungleCsBefore10Minutes                   :: Double,
    junglerTakedownsNearDamagedEpicMonster    :: Int,
    kda                                       :: Double,
    killAfterHiddenWithAlly                   :: Int,
    killedChampTookFullTeamDamageSurvived     :: Int,
    killingSprees                             :: Int,
    killParticipation                         :: Double,
    killsNearEnemyTurret                      :: Int,
    killsOnOtherLanesEarlyJungleAsLaner       :: Int,
    killsOnRecentlyHealedByAramPack           :: Int,
    killsUnderOwnTurret                       :: Int,
    killsWithHelpFromEpicMonster              :: Int,
    knockEnemyIntoTeamAndKill                 :: Int,
    kTurretsDestroyedBeforePlatesFall         :: Int,
    landSkillShotsEarlyGame                   :: Int,
    laneMinionsFirst10Minutes                 :: Int,
    lostAnInhibitor                           :: Int,
    maxKillDeficit                            :: Int,
    mejaisFullStackInTime                     :: Int,
    moreEnemyJungleThanOpponent               :: Double,
    {- | This is an offshoot of the OneStone challenge. The code checks if a spell with the same instance ID does the final point of damage to at least 2 Champions. It doesn't matter if they're enemies, but you cannot hurt your friends.-}
    multiKillOneSpell                         :: Int,
    multikills                                :: Int,
    multikillsAfterAggressiveFlash            :: Int,
    multiTurretRiftHeraldCount                :: Int,
    outerTurretExecutesBefore10Minutes        :: Int,
    outDoubleedKills                          :: Int,
    outDoubleedNexusKill                      :: Int,
    perfectDragonSoulsTaken                   :: Int,
    perfectGame                               :: Int,
    pickKillWithAlly                          :: Int,
    poroExplosions                            :: Int,
    quickCleanse                              :: Int,
    quickFirstTurret                          :: Int,
    quickSoloKills                            :: Int,
    riftHeraldTakedowns                       :: Int,
    saveAllyFromDeath                         :: Int,
    scuttleCrabKills                          :: Int,
    shortestTimeToAceFromFirstTakedown        :: Double,
    skillshotsDodged                          :: Int,
    skillshotsHit                             :: Int,
    snowballsHit                              :: Int,
    soloBaronKills                            :: Int,
    sWARM_DefeatAatrox                        :: Int,
    sWARM_DefeatBriar                         :: Int,
    sWARM_DefeatMiniBosses                    :: Int,
    sWARM_EvolveWeapon                        :: Int,
    sWARM_Have3Passives                       :: Int,
    sWARM_KillEnemy                           :: Int,
    sWARM_PickupGold                          :: Double,
    sWARM_ReachLevel50                        :: Int,
    sWARM_Survive15Min                        :: Int,
    sWARM_WinWith5EvolvedWeapons              :: Int,
    soloKills                                 :: Int,
    stealthWardsPlaced                        :: Int,
    survivedSingleDigitHpCount                :: Int,
    survivedThreeImmobilizesInFight           :: Int,
    takedownOnFirstTurret                     :: Int,
    takedowns                                 :: Int,
    takedownsAfterGainingLevelAdvantage       :: Int,
    takedownsBeforeJungleMinionSpawn          :: Int,
    takedownsFirstXMinutes                    :: Int,
    takedownsInAlcove                         :: Int,
    takedownsInEnemyFountain                  :: Int,
    teamBaronKills                            :: Int,
    teamDamagePercentage                      :: Double,
    teamElderDragonKills                      :: Int,
    teamRiftHeraldKills                       :: Int,
    tookLargeDamageSurvived                   :: Int,
    turretPlatesTaken                         :: Int,
    {- | Any player who damages a tower that is destroyed within 30 seconds of a Rift Herald charge will receive credit. A player who does not damage the tower will not receive credit.-}
    turretsTakenWithRiftHerald                :: Int,
    turretTakedowns                           :: Int,
    twentyMinionsIn3SecondsCount              :: Int,
    twoWardsOneSweeperCount                   :: Int,
    unseenRecalls                             :: Int,
    visionScorePerMinute                      :: Double,
    wardsGuarded                              :: Int,
    wardTakedowns                             :: Int,
    wardTakedownsBefore20M                    :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ChallengesDto where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = customFieldMapping}

customFieldMapping :: String -> String
customFieldMapping "assistStreakCount12" = "12AssistStreakCount"
customFieldMapping "InfernalScalePickup" = "infernalScalePickup"
customFieldMapping "SWARM_DefeatAatrox" = "    sWARM_DefeatAatrox"
customFieldMapping "SWARM_DefeatBriar" = "    sWARM_DefeatBriar"
customFieldMapping "SWARM_DefeatMiniBosses" = "    sWARM_DefeatMiniBosses"
customFieldMapping "SWARM_EvolveWeapon" = "    sWARM_EvolveWeapon"
customFieldMapping "SWARM_Have3Passives" = "    sWARM_Have3Passives"
customFieldMapping "SWARM_KillEnemy" = "    sWARM_KillEnemy"
customFieldMapping "SWARM_PickupGold" = "    sWARM_PickupGold"
customFieldMapping "SWARM_ReachLevel50" = "    sWARM_ReachLevel50"
customFieldMapping "SWARM_Survive15Min" = "    sWARM_Survive15Min"
customFieldMapping "SWARM_WinWith5EvolvedWeapons" = "    sWARM_WinWith5EvolvedWeapons"
customFieldMapping other = other

data MissionsDto = MissionsDto
  { playerScore0  :: Int,
    playerScore1  :: Int,
    playerScore2  :: Int,
    playerScore3  :: Int,
    playerScore4  :: Int,
    playerScore5  :: Int,
    playerScore6  :: Int,
    playerScore7  :: Int,
    playerScore8  :: Int,
    playerScore9  :: Int,
    playerScore10 :: Int,
    playerScore11 :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON MissionsDto

data PerksDto = PerksDto
  { statPerks :: PerkStatsDto,
    styles    :: [PerkStyleDto]
  }
  deriving (Show, Eq, Generic)

instance FromJSON PerksDto

data PerkStatsDto = PerkStatsDto
  { defense :: Int,
    flex    :: Int,
    offense :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON PerkStatsDto

data PerkStyleDto = PerkStyleDto
  { description :: String,
    selections  :: [PerkStyleSelectionDto],
    style       :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON PerkStyleDto

data PerkStyleSelectionDto = PerkStyleSelectionDto
  { perk :: Int,
    var1 :: Int,
    var2 :: Int,
    var3 :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON PerkStyleSelectionDto

data TeamDto = TeamDto
  { bans       :: [BanDto],
    objectives :: ObjectivesDto,
    teamId     :: Int,
    win        :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON TeamDto

data BanDto = BanDto
  { championId :: Int,
    pickTurn   :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON BanDto

data ObjectivesDto = ObjectivesDto
  { baron      :: ObjectiveDto,
    champion   :: ObjectiveDto,
    dragon     :: ObjectiveDto,
    horde      :: ObjectiveDto,
    inhibitor  :: ObjectiveDto,
    riftHerald :: ObjectiveDto,
    tower      :: ObjectiveDto
  }
  deriving (Show, Eq, Generic)

instance FromJSON ObjectivesDto

data ObjectiveDto = ObjectiveDto
  { first :: Bool,
    kills :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ObjectiveDto

data TimelineDto = TimelineDto
  { {- | Match metadata.-}
    metadata :: MetadataTimeLineDto,
    {- | Match info.-}
    info     :: InfoTimeLineDto
  }
  deriving (Show, Eq, Generic)

instance FromJSON TimelineDto

data MetadataTimeLineDto = MetadataTimeLineDto
  { {- | Match data version.-}
    dataVersion  :: String,
    {- | Match id.-}
    matchId      :: String,
    {- | A list of participant PUUIDs.-}
    participants :: [String]
  }
  deriving (Show, Eq, Generic)

instance FromJSON MetadataTimeLineDto

data InfoTimeLineDto = InfoTimeLineDto
  { {- | Refer to indicate if the game ended in termination.-}
    endOfGameResult :: String,
    frameInterval   :: Int,
    gameId          :: Int,
    participants    :: [ParticipantTimeLineDto],
    frames          :: [FramesTimeLineDto]
  }
  deriving (Show, Eq, Generic)

instance FromJSON InfoTimeLineDto

data ParticipantTimeLineDto = ParticipantTimeLineDto
  { participantId :: Int,
    puuid         :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON ParticipantTimeLineDto

data FramesTimeLineDto = FramesTimeLineDto
  { events            :: [EventsTimeLineDto],
    participantFrames :: ParticipantFramesDto,
    timestamp         :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON FramesTimeLineDto

data EventsTimeLineDto = EventsTimeLineDto
  { timestamp     :: Int,
    realTimestamp :: Int,
    eventType     :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON EventsTimeLineDto where
  parseJSON = withObject "EventsTimeLineDto" $ \v ->
    EventsTimeLineDto
      <$> v .: "timestamp"
      <*> v .: "realTimestamp"
      <*> v .: "type"

newtype ParticipantFramesDto = ParticipantFramesDto (Map Int ParticipantFrameDto)
  deriving (Show, Eq, Generic)

instance FromJSON ParticipantFramesDto

data ParticipantFrameDto = ParticipantFrameDto
  { championStats            :: ChampionStatsDto,
    currentGold              :: Int,
    damageStats              :: DamageStatsDto,
    goldPerSecond            :: Int,
    jungleMinionsKilled      :: Int,
    level                    :: Int,
    minionsKilled            :: Int,
    participantId            :: Int,
    position                 :: PositionDto,
    timeEnemySpentControlled :: Int,
    totalGold                :: Int,
    xp                       :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ParticipantFrameDto

data ChampionStatsDto = ChampionStatsDto
  { abilityHaste         :: Int,
    abilityPower         :: Int,
    armor                :: Int,
    armorPen             :: Int,
    armorPenPercent      :: Int,
    attackDamage         :: Int,
    attackSpeed          :: Int,
    bonusArmorPenPercent :: Int,
    bonusMagicPenPercent :: Int,
    ccReduction          :: Int,
    cooldownReduction    :: Int,
    health               :: Int,
    healthMax            :: Int,
    healthRegen          :: Int,
    lifesteal            :: Int,
    magicPen             :: Int,
    magicPenPercent      :: Int,
    magicResist          :: Int,
    movementSpeed        :: Int,
    omnivamp             :: Int,
    physicalVamp         :: Int,
    power                :: Int,
    powerMax             :: Int,
    powerRegen           :: Int,
    spellVamp            :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ChampionStatsDto

data DamageStatsDto = DamageStatsDto
  { magicDamageDone               :: Int,
    magicDamageDoneToChampions    :: Int,
    magicDamageTaken              :: Int,
    physicalDamageDone            :: Int,
    physicalDamageDoneToChampions :: Int,
    physicalDamageTaken           :: Int,
    totalDamageDone               :: Int,
    totalDamageDoneToChampions    :: Int,
    totalDamageTaken              :: Int,
    trueDamageDone                :: Int,
    trueDamageDoneToChampions     :: Int,
    trueDamageTaken               :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON DamageStatsDto

data PositionDto = PositionDto
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON PositionDto

type MatchId = String

matchesIds :: Platform -> IO (TypedRequest [string])
matchesIds platform = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/rso-match/v1/matches/ids"
  return $ TypedRequest request

matches :: Platform -> MatchId -> IO (TypedRequest MatchDto)
matches platform matchId = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/rso-match/v1/matches/" ++ matchId
  return $ TypedRequest request

matchesTimeline :: Platform -> MatchId -> IO (TypedRequest TimelineDto)
matchesTimeline platform matchId = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/rso-match/v1/matches/" ++ matchId ++ "/timeline"
  return $ TypedRequest request
