
library(dplyr)
temp <- match1v1
temp$randomBit <- sample(0:100, size=nrow(temp), replace = TRUE)

match1v1Clean <- group_by(temp, MatchId) %>%
  arrange(randomBit, .by_group=TRUE) %>%
  summarise(
            matchId = first(MatchId),
            matchMods = first(MatchMods),
            matchMap = first(MatchMap),
            matchPlayerId = first(MatchPlayerId),
            playerCiv=first(MatchPlayerCivName),
            opponentPlayerId = last(MatchPlayerId),
            opponentCiv=last(MatchPlayerCivName),
            winner=first(MatchPlayerWinner),
            eloGap=first(MatchPlayerPreRating) - last(MatchPlayerPreRating),
            playerElo = first(MatchPlayerPreRating),
            opponentElo = last(MatchPlayerPreRating)
            ) %>%
  mutate(
      matchMods = as.list(strsplit(matchMods, ","))
    ) %>%
  filter(playerCiv != "VooblyCivError" & opponentCiv !="VooblyCivError")
