# Pure Riot

Pure Riot is an api client for the riot api like all the other clients. It has types to all existing endpoints and functions to create requests. The request then can be consumed by a manager to produce a response with a type. 

```haskell
import           Endpoints.LeagueofLegends.MatchV5 (matchesTimeline)
import           PureRiot                          (createManager, fetchTyped)
import           Regions                           (Region (EUROPE))

main :: IO ()
main = do
  manager <- createManager API-KEY -- creating the http manager
  request <- matchesTimeline EUROPE "EUW1_7334877218" -- creating a request that will get the timeline if the given match id
  timelineDto <- fetchTyped manager request -- execute the request and return the timelineDto record
  print timelineDto
```

For the future, a Priority Queue Manager is planned that takes an infinite priority queue to scrape the api for endpoints. But if a user comes with a request, the user request should be fetched as fast as possible. Therefore, the Rate Limits should always be reached, and no user has to wait a long time for their request to be finished.

# Setup

Clone the reposetory

```
git clone https://github.com/Plutokekz/Pure-Riot.git
```
Build all endpoints

```
cabal build
```

Build only the selected endpoints. Fore example, this command would build the Account and League of Legends endpoints. All available flags can be found in the `pure-riot.cabal`

```
cabal build --flag=league-of-legends --flag=account --flag=-all-endpoints
```

# TODO 

- [ ] Rate Limiting
- [ ] Tests
    - [ ] life 
    - [ ] stubbed 
- [ ] release to Hackage
- [ ] Priority Queue Manager
