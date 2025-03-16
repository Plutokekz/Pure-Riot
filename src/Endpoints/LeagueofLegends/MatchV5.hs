{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Endpoints.LeagueofLegends.MatchV5 where

import           Data.Aeson            (FromJSON (..), defaultOptions,
                                        fieldLabelModifier, genericParseJSON)
import           Data.Aeson.Types      (withObject, (.:), (.:?))
import qualified Data.Bifunctor
import qualified Data.ByteString.Char8 as BC
import           Data.Map              (Map)
import           GHC.Generics
import           Network.HTTP.Client   (parseRequest, setQueryString)
import           PureRiot              (TypedRequest (TypedRequest))
import           Regions               (Region (..), regionToUrl)

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
    playerScore0                   :: Maybe Int,
    playerScore1                   :: Maybe Int,
    playerScore2                   :: Maybe Int,
    playerScore3                   :: Maybe Int,
    playerScore4                   :: Maybe Int,
    playerScore5                   :: Maybe Int,
    playerScore6                   :: Maybe Int,
    playerScore7                   :: Maybe Int,
    playerScore8                   :: Maybe Int,
    playerScore9                   :: Maybe Int,
    playerScore10                  :: Maybe Int,
    playerScore11                  :: Maybe Int,
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
    baronBuffGoldAdvantageOverThreshold       :: Maybe Int,
    controlWardTimeCoverageInRiverOrEnemyHalf :: Maybe Double,
    earliestBaron                             :: Maybe Int,
    earliestDragonTakedown                    :: Maybe Int,
    earliestElderDragon                       :: Maybe Int,
    earlyLaningPhaseGoldExpAdvantage          :: Maybe Int,
    fasterSupportQuestCompletion              :: Maybe Int,
    fastestLegendary                          :: Maybe Double,
    hadAfkTeammate                            :: Maybe Int,
    highestChampionDamage                     :: Maybe Int,
    highestCrowdControlScore                  :: Maybe Int,
    highestWardKills                          :: Maybe Int,
    junglerKillsEarlyJungle                   :: Maybe Int,
    killsOnLanersEarlyJungleAsJungler         :: Maybe Int,
    laningPhaseGoldExpAdvantage               :: Maybe Int,
    legendaryCount                            :: Maybe Int,
    maxCsAdvantageOnLaneOpponent              :: Maybe Double,
    maxLevelLeadLaneOpponent                  :: Maybe Int,
    mostWardsDestroyedOneSweeper              :: Maybe Int,
    mythicItemUsed                            :: Maybe Int,
    playedChampSelectPosition                 :: Maybe Int,
    soloTurretsLategame                       :: Maybe Int,
    takedownsFirst25Minutes                   :: Maybe Int,
    teleportTakedowns                         :: Maybe Int,
    thirdInhibitorDestroyedTime               :: Maybe Int,
    threeWardsOneSweeperCount                 :: Maybe Int,
    visionScoreAdvantageLaneOpponent          :: Maybe Double,
    infernalScalePickup                       :: Maybe Int,
    fistBumpParticipation                     :: Maybe Int,
    voidMonsterKill                           :: Maybe Int,
    abilityUses                               :: Maybe Int,
    acesBefore15Minutes                       :: Maybe Int,
    alliedJungleMonsterKills                  :: Maybe Double,
    baronTakedowns                            :: Maybe Int,
    blastConeOppositeOpponentCount            :: Maybe Int,
    bountyGold                                :: Maybe Double,
    buffsStolen                               :: Maybe Int,
    completeSupportQuestInTime                :: Maybe Int,
    controlWardsPlaced                        :: Maybe Int,
    damagePerMinute                           :: Maybe Double,
    damageTakenOnTeamPercentage               :: Maybe Double,
    dancedWithRiftHerald                      :: Maybe Int,
    deathsByEnemyChamps                       :: Maybe Int,
    dodgeSkillShotsSmallWindow                :: Maybe Int,
    doubleAces                                :: Maybe Int,
    dragonTakedowns                           :: Int,
    legendaryItemUsed                         :: [Int],
    effectiveHealAndShielding                 :: Maybe Double,
    elderDragonKillsWithOpposingSoul          :: Maybe Int,
    elderDragonMultikills                     :: Maybe Int,
    enemyChampionImmobilizations              :: Maybe Int,
    enemyJungleMonsterKills                   :: Maybe Double,
    epicMonsterKillsNearEnemyJungler          :: Maybe Int,
    epicMonsterKillsWithin30SecondsOfSpawn    :: Maybe Int,
    epicMonsterSteals                         :: Maybe Int,
    epicMonsterStolenWithoutSmite             :: Maybe Int,
    firstTurretKilled                         :: Maybe Int,
    firstTurretKilledTime                     :: Maybe Double,
    flawlessAces                              :: Maybe Int,
    fullTeamTakedown                          :: Maybe Int,
    gameLength                                :: Maybe Double,
    getTakedownsInAllLanesEarlyJungleAsLaner  :: Maybe Int,
    goldPerMinute                             :: Maybe Double,
    hadOpenNexus                              :: Maybe Int,
    immobilizeAndKillWithAlly                 :: Maybe Int,
    initialBuffCount                          :: Maybe Int,
    initialCrabCount                          :: Maybe Int,
    jungleCsBefore10Minutes                   :: Maybe Double,
    junglerTakedownsNearDamagedEpicMonster    :: Int,
    kda                                       :: Maybe Double,
    killAfterHiddenWithAlly                   :: Maybe Int,
    killedChampTookFullTeamDamageSurvived     :: Maybe Int,
    killingSprees                             :: Maybe Int,
    killParticipation                         :: Maybe Double,
    killsNearEnemyTurret                      :: Maybe Int,
    killsOnOtherLanesEarlyJungleAsLaner       :: Maybe Int,
    killsOnRecentlyHealedByAramPack           :: Maybe Int,
    killsUnderOwnTurret                       :: Maybe Int,
    killsWithHelpFromEpicMonster              :: Maybe Int,
    knockEnemyIntoTeamAndKill                 :: Maybe Int,
    kTurretsDestroyedBeforePlatesFall         :: Maybe Int,
    landSkillShotsEarlyGame                   :: Maybe Int,
    laneMinionsFirst10Minutes                 :: Maybe Int,
    lostAnInhibitor                           :: Maybe Int,
    maxKillDeficit                            :: Maybe Int,
    mejaisFullStackInTime                     :: Maybe Int,
    moreEnemyJungleThanOpponent               :: Maybe Double,
    {- | This is an offshoot of the OneStone challenge. The code checks if a spell with the same instance ID does the final point of damage to at least 2 Champions. It doesn't matter if they're enemies, but you cannot hurt your friends.-}
    multiKillOneSpell                         :: Maybe Int,
    multikills                                :: Maybe Int,
    multikillsAfterAggressiveFlash            :: Maybe Int,
    multiTurretRiftHeraldCount                :: Maybe Int,
    outerTurretExecutesBefore10Minutes        :: Maybe Int,
    outDoubleedKills                          :: Maybe Int,
    outDoubleedNexusKill                      :: Maybe Int,
    perfectDragonSoulsTaken                   :: Maybe Int,
    perfectGame                               :: Maybe Int,
    pickKillWithAlly                          :: Maybe Int,
    poroExplosions                            :: Maybe Int,
    quickCleanse                              :: Maybe Int,
    quickFirstTurret                          :: Maybe Int,
    quickSoloKills                            :: Maybe Int,
    riftHeraldTakedowns                       :: Maybe Int,
    saveAllyFromDeath                         :: Maybe Int,
    scuttleCrabKills                          :: Maybe Int,
    shortestTimeToAceFromFirstTakedown        :: Maybe Double,
    skillshotsDodged                          :: Maybe Int,
    skillshotsHit                             :: Maybe Int,
    snowballsHit                              :: Maybe Int,
    soloBaronKills                            :: Maybe Int,
    sWARM_DefeatAatrox                        :: Maybe Int,
    sWARM_DefeatBriar                         :: Maybe Int,
    sWARM_DefeatMiniBosses                    :: Maybe Int,
    sWARM_EvolveWeapon                        :: Maybe Int,
    sWARM_Have3Passives                       :: Maybe Int,
    sWARM_KillEnemy                           :: Maybe Int,
    sWARM_PickupGold                          :: Maybe Double,
    sWARM_ReachLevel50                        :: Maybe Int,
    sWARM_Survive15Min                        :: Maybe Int,
    sWARM_WinWith5EvolvedWeapons              :: Maybe Int,
    soloKills                                 :: Maybe Int,
    stealthWardsPlaced                        :: Maybe Int,
    survivedSingleDigitHpCount                :: Maybe Int,
    survivedThreeImmobilizesInFight           :: Maybe Int,
    takedownOnFirstTurret                     :: Maybe Int,
    takedowns                                 :: Maybe Int,
    takedownsAfterGainingLevelAdvantage       :: Maybe Int,
    takedownsBeforeJungleMinionSpawn          :: Maybe Int,
    takedownsFirstXMinutes                    :: Maybe Int,
    takedownsInAlcove                         :: Maybe Int,
    takedownsInEnemyFountain                  :: Maybe Int,
    teamBaronKills                            :: Maybe Int,
    teamDamagePercentage                      :: Maybe Double,
    teamElderDragonKills                      :: Maybe Int,
    teamRiftHeraldKills                       :: Maybe Int,
    tookLargeDamageSurvived                   :: Maybe Int,
    turretPlatesTaken                         :: Maybe Int,
    {- | Any player who damages a tower that is destroyed within 30 seconds of a Rift Herald charge will receive credit. A player who does not damage the tower will not receive credit .-}
    turretsTakenWithRiftHerald                :: Maybe Int,
    turretTakedowns                           :: Maybe Int,
    twentyMinionsIn3SecondsCount              :: Maybe Int,
    twoWardsOneSweeperCount                   :: Maybe Int,
    unseenRecalls                             :: Maybe Int,
    visionScorePerMinute                      :: Maybe Double,
    wardsGuarded                              :: Maybe Int,
    wardTakedowns                             :: Maybe Int,
    wardTakedownsBefore20M                    :: Maybe Int
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
  { playerScore0  :: Maybe Int,
    playerScore1  :: Maybe Int,
    playerScore2  :: Maybe Int,
    playerScore3  :: Maybe Int,
    playerScore4  :: Maybe Int,
    playerScore5  :: Maybe Int,
    playerScore6  :: Maybe Int,
    playerScore7  :: Maybe Int,
    playerScore8  :: Maybe Int,
    playerScore9  :: Maybe Int,
    playerScore10 :: Maybe Int,
    playerScore11 :: Maybe Int
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
    realTimestamp :: Maybe Int,
    eventType     :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON EventsTimeLineDto where
  parseJSON = withObject "EventsTimeLineDto" $ \v ->
    EventsTimeLineDto
      <$> v .: "timestamp"
      <*> v .:? "realTimestamp"
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

type Puuid = String

newtype Puuids = Puuids [String] deriving (Show, Eq, Generic)

instance FromJSON Puuids

{- | Epoch timestamp in seconds. The matchlist started storing timestamps on June 16th, 2021. Any matches played before June 16th, 2021 won't be included in the results if the startTime filter is set.-}
type StartTime = Integer

-- | Epoch timestamp in seconds.
type EndTime = Integer

-- |Filter the list of match ids by a specific queue id. This filter is mutually inclusive of the type filter meaning any match ids returned must match both the queue and type filters.
type Queue = Int

-- | Filter the list of match ids by the type of match. This filter is mutually inclusive of the queue filter meaning any match ids returned must match both the queue and type filters.
type Type = String

-- | Defaults to 0. Start index.
type Start = Int

-- | Defaults to 20. Valid values: 0 to 100. Number of match ids to return.
type Count = Int

packQueryParams :: [(String, String)] -> [(BC.ByteString, Maybe BC.ByteString)]
packQueryParams = map (Data.Bifunctor.bimap BC.pack (Just . BC.pack))

matchesByPuuidId :: Region -> Puuid -> StartTime -> EndTime -> Queue -> Type -> Start -> Count -> IO (TypedRequest Puuids)
matchesByPuuidId region puuid startTime endTime queue qtype start count = do
  let baseUrl = regionToUrl region ++ "/lol/match/v5/matches/by-puuid/" ++ puuid ++ "/ids"
  request <- parseRequest baseUrl
  let params = packQueryParams
          [ ("startTime", show startTime),
            ("endTime", show endTime),
            ("queue", show queue),
            ("type", show qtype),
            ("start", show start),
            ("count", show count)
          ]
  return $ TypedRequest (setQueryString params request)

matches :: Region -> MatchId -> IO (TypedRequest MatchDto)
matches region matchId = do
  request <- parseRequest $ regionToUrl region ++ "/lol/match/v5/matches/" ++ matchId
  return $ TypedRequest request

matchesTimeline :: Region -> MatchId -> IO (TypedRequest TimelineDto)
matchesTimeline region matchId = do
  request <- parseRequest $ regionToUrl region ++ "/lol/match/v5/matches/" ++ matchId ++ "/timeline"
  return $ TypedRequest request
