library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

set.seed(12)

temp <- match1v1Clean
temp <- arrange(temp, matchId)
gamesFClean <- temp %>%
  group_by(matchPlayerId) %>%
  nest()
# we should remove first 25 games from all 1600 players, and require everyone to have at least 15 games after that


gamesFClean <- rowwise(gamesFClean) %>% mutate (
    firstElo = first(data$playerElo),
    numberGames = length(data$matchId)
  ) %>%
  filter(firstElo == 1600 & numberGames > 40)


sortOutData <- function (x) {
  x$gameNumber <- 1:(length(x$matchId))
  head(x,400)
}

gamesF <- gamesFClean
gamesF <- rowwise(gamesF) %>% mutate (
    eloAfter25Games = nth(data$playerElo, 21),
    eloGapAfter25Games = eloAfter25Games - firstElo,
    eloAfter40Games = nth(data$playerElo, 41),
    eloGapAfterMoreGames = eloAfter40Games - eloAfter25Games,

  ) %>% filter(eloGapAfter25Games > 100 & eloGapAfterMoreGames > 100)

gamesF <- rowwise(gamesF) %>% do(matchPlayerId=.$matchPlayerId, eloAfter25Games=.$eloAfter25Games, numberGames=.$numberGames, data = sortOutData(.$data)) %>%
  mutate (matchPlayerId = matchPlayerId[1], eloAfter25Games = eloAfter25Games[1])

#gamesF <- sample_n(gamesF, 500)
gamesF <- unnest(gamesF, data)
gamesF$matchPlayerId <- factor(gamesF$matchPlayerId)

png(filename="images/eloHistory.png", width=2000, height=800)
p <- ggplot(data=gamesF, aes(x=gameNumber, y=playerElo, group=matchPlayerId)) +
  geom_line(aes(color=matchPlayerId))
p
dev.off()
p

# this is a way of getting a list of matches to remove

# smurfs <- arrange(temp, matchId)


# smurfs <- temp %>%
#   group_by(matchPlayerId) %>%
#   nest()



# smurfs <- map_dfr(smurfs$data, function(data) {
#   data.frame(stripMatches = ifelse(first(data$playerElo) < 1700, head(data$MatchId, 50), c(-123132131)))

#   })
# smurfs <- distinct(smurfs, stripMatches)
# View(smurfs)
# stop("asdf")

# smurfs <- filter(gamesF, eloGapAfter20Games > 0)
# smurfs <- sample_n(smurfs, 100)
# smurfs <- unnest(smurfs, data)
# smurfs$matchPlayerId <- factor(smurfs$matchPlayerId)
# ggplot(data=smurfs, aes(x=gameNumber, y=playerElo, group=matchPlayerId)) +
#   geom_line(aes(color=matchPlayerId))

# stop("asdf")


# noobs <- filter(gamesF, eloGapAfter20Games < 0)
# noobs <- head (noobs, 100)
# noobs <- unnest(noobs, data)
# noobs$matchPlayerId <- factor(noobs$matchPlayerId)
# ggplot(data=noobs, aes(x=gameNumber, y=playerElo, group=matchPlayerId)) +
#   geom_line(aes(color=matchPlayerId))

