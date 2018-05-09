
library(dplyr)
temp <- match1v1
set.seed(42);
temp$randomBit <- sample(0:100, size=nrow(temp), replace = TRUE)

match1v1Clean <- group_by(temp, MatchId) %>%
  arrange(randomBit, .by_group=TRUE) %>%
  summarise(
            MatchDate = first(MatchDate),
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
      upVersion = ifelse(grepl("1.5 Beta", matchMods, fixed=TRUE), "1.5" , "1.4"),
      uplatest = grepl("v1.5 Beta R6", matchMods),
      wk = ifelse(grepl("WololoKingdoms", matchMods, fixed=TRUE), TRUE , FALSE)
    ) %>%
  filter(playerCiv != "VooblyCivError" & opponentCiv !="VooblyCivError")

match1v1CleanInverted <- mutate(match1v1Clean,
    oldOpponentCiv = opponentCiv,
    oldOpponentElo = opponentElo,
    oldOpponentPlayerId = opponentPlayerId,
    winner = !winner,
    eloGap = 0 - eloGap,
    opponentCiv = playerCiv,
    opponentElo = playerElo,
    opponentPlayerId = matchPlayerId,
    playerCiv = oldOpponentCiv,
    playerElo = oldOpponentElo,
    matchPlayerId = oldOpponentPlayerId
  ) %>% select(-one_of(c('oldOpponentCiv', 'oldOpponentElo', 'oldOpponentPlayerId')))

match1v1Clean <- rbind(match1v1Clean, match1v1CleanInverted)
rm(match1v1CleanInverted)

View(match1v1Clean)