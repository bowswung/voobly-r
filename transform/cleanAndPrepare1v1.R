
library(dplyr)
library(stringr)
temp <- match1v1
#temp <- head(match1v1)
set.seed(42);
temp$randomBit <- sample(0:100, size=nrow(temp), replace = TRUE)

# match1v1Clean <- group_by(temp, MatchId) %>%
#   arrange(randomBit, .by_group=TRUE) %>%
#   summarise(
#             MatchDate = first(MatchDate),
#             MatchDuration = first(MatchDuration),
#             matchId = first(MatchId),
#             matchMods = first(MatchMods),
#             matchMap = first(MatchMap),
#             matchPlayerId = first(MatchPlayerId),
#             playerCiv=first(MatchPlayerCivName),
#             opponentPlayerId = last(MatchPlayerId),
#             opponentCiv=last(MatchPlayerCivName),
#             winner=first(MatchPlayerWinner),
#             eloGap=first(MatchPlayerPreRating) - last(MatchPlayerPreRating),
#             playerElo = first(MatchPlayerPreRating),
#             opponentElo = last(MatchPlayerPreRating)
#             ) %>%
#   mutate(
#       upVersion = ifelse(grepl("1.5 Beta", matchMods, fixed=TRUE), "1.5" , "1.4"),
#       uplatest = grepl("v1.5 Beta R7", matchMods),
#       upReleaseVersion = sapply(matchMods, function(x) (str_match(x, regex("v1\\.5 Beta (R[0-9]+)"))[2])),
#       wk = ifelse(grepl("WololoKingdoms", matchMods, fixed=TRUE), TRUE , FALSE)
#     ) %>%
#   filter(playerCiv != "VooblyCivError" & opponentCiv !="VooblyCivError")

# match1v1CleanInverted <- mutate(match1v1Clean,
#     oldOpponentCiv = opponentCiv,
#     oldOpponentElo = opponentElo,
#     oldOpponentPlayerId = opponentPlayerId,
#     winner = !winner,
#     eloGap = 0 - eloGap,
#     opponentCiv = playerCiv,
#     opponentElo = playerElo,
#     opponentPlayerId = matchPlayerId,
#     playerCiv = oldOpponentCiv,
#     playerElo = oldOpponentElo,
#     matchPlayerId = oldOpponentPlayerId
#   ) %>% select(-one_of(c('oldOpponentCiv', 'oldOpponentElo', 'oldOpponentPlayerId')))

# match1v1Clean <- rbind(match1v1Clean, match1v1CleanInverted)
# rm(match1v1CleanInverted)


tempCivFixed <- match1v1Clean
tempCivFixed <- distinct(tempCivFixed, matchId, .keep_all = TRUE)


civList <- sort(unique(tempCivFixed$playerCiv))
tempCivFixed <- rowwise(tempCivFixed) %>% mutate(
    playerCivFixed = ifelse(which(civList == playerCiv) < which(civList == opponentCiv), playerCiv, opponentCiv),
    opponentCivFixed = ifelse(which(civList == playerCiv) < which(civList == opponentCiv), opponentCiv, playerCiv),
    matchup = paste(playerCiv, opponentCiv, sep="-"),
    matchupFixed = ifelse(which(civList == playerCiv) < which(civList == opponentCiv), paste(playerCiv, opponentCiv, sep="-"), paste(opponentCiv, playerCiv , sep="-")),

    eloGapFixed = ifelse(which(civList == playerCiv) < which(civList == opponentCiv), eloGap, -eloGap),
    winnerFixed = ifelse(which(civList == playerCiv) < which(civList == opponentCiv), winner, !winner)) %>% ungroup()
tempCivFixed$matchupFixed <- factor(tempCivFixed$matchupFixed)
tempCivFixed <- select(tempCivFixed, -matchMods, -playerCiv, -opponentCiv, -matchPlayerId, -opponentPlayerId, -eloGap, -winner, -matchup, -MatchId)
match1v1CleanMatchupFixed <- tempCivFixed

rm(tempCivFixed)
