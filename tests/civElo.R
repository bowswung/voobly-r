library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(lubridate)

temp <- match1v1Clean
temp <- distinct(temp, MatchId, .keep_all = TRUE)
temp <- filter(temp, playerCiv != opponentCiv)
temp <- filter(temp, matchMap == "Arabia")
temp <- filter(temp, (upReleaseVersion == "R6" | upReleaseVersion == "R7" & wk))
temp <- filter(temp, ((playerElo + opponentElo) /2) > 2000)
temp <- arrange(temp, MatchDate)


temp$playerCiv <- factor(temp$playerCiv)
temp$opponentCiv <- factor(temp$opponentCiv)



calculateEloChange <- function(winnerRating, loserRating) {

  rW<- 10^(winnerRating / 400)
  rL<- 10^(loserRating / 400)

  eW <- rW / (rW + rL)
  eL <- rL / (rW + rL)

  afterW <- 32 * (1 - eW)
  afterL <- 32 * (0 - eL)
  c(afterW, afterL)

}

lookupLastElo <- function(targetCiv, eloData) {
  if (length(eloData[eloData$civ == targetCiv, ]$eloAfter) > 0) {
    tail(eloData[eloData$civ == targetCiv, ], 1)$eloAfter[1]
    } else {
      1600
    }
}

elo <- data.frame(matchDate=as.Date(character()),
                 civ = factor(character(), levels=unique(temp$playerCiv)),
                 eloBefore=numeric(),
                 eloChange=numeric(),
                 eloAfter=numeric()
                 )
# allCivs <-  tidyr::expand(temp, playerCiv)


# for (i in 1:nrow(temp)) {
#   if (i %% 500 == 0) {
#     print(i)
#   }
#   if (temp$winner[i]) {
#     winCiv <- temp$playerCiv[i]
#     winPlayerElo <- temp$playerElo[i]
#     loseCiv <- temp$opponentCiv[i]
#     losePlayerElo <- temp$opponentElo[i]
#   } else {
#     winCiv <- temp$opponentCiv[i]
#     winPlayerElo <- temp$opponentElo[i]
#     loseCiv <- temp$playerCiv[i]
#     losePlayerElo <- temp$playerElo[i]
#   }

#   winCivPreviousElo <- lookupLastElo(winCiv, elo)
#   loseCivPreviousElo <- lookupLastElo(loseCiv, elo)

#   afterElos <- calculateEloChange(winCivPreviousElo + winPlayerElo, loseCivPreviousElo + losePlayerElo)

#   elo <- rbind(elo, data.frame(matchDate=temp$MatchDate[i], civ=winCiv, eloBefore=winCivPreviousElo, eloChange=afterElos[1]/2, eloAfter=winCivPreviousElo + afterElos[1]/2))
#   elo <- rbind(elo, data.frame(matchDate=temp$MatchDate[i], civ=loseCiv, eloBefore=loseCivPreviousElo, eloChange=afterElos[2]/2, eloAfter=loseCivPreviousElo + afterElos[2]/2))


# }
# elo.withPlayer.2000 <- elo
# stop("ADASADS")


eloToWorkWith <- elo.withPlayer.2000

eloAverageAllTime <- group_by(eloToWorkWith, civ) %>%
  summarise(elo = mean(eloAfter))
View(eloAverageAllTime)


currentCivElos <- rowwise(currentCivElos) %>% mutate(
  currentElo = lookupLastElo(playerCiv, elo)
  )


eloForPlot <- eloToWorkWith
#eloForPlot <- tail(eloForPlot, -)
#eloDailyAverage$date <- floor_date(as.Date(eloDailyAverage$matchDate), "week")
#eloDailyAverage$date <- as.Date(eloDailyAverage$matchDate)

#eloDailyAverage <- group_by(eloDailyAverage, civ, date) %>%
#                     summarise(elo = mean(eloAfter))

eloForPlot$date <- eloForPlot$matchDate
eloForPlot$elo <- eloForPlot$eloAfter

eloForPlot <- filter(eloForPlot, (civ == "Franks" | civ == "Khmer" | civ == "Aztecs" | civ == "Koreans" | civ == "Celts" | civ =="Burmese" | civ == "Vietnamese" | civ=="Teutons"))
png(filename="temp/images/civEloHistory.png", width=1920, height=1080)
p <- ggplot(data=eloForPlot, aes(x=date, y=elo, group=civ)) +
        geom_point(aes(color=civ), size=0.1) +
        stat_smooth(aes(color=civ, fill=civ), alpha=0.1) +
        theme_bw(base_size=14)+
        theme(panel.border = element_blank(),
         axis.line = element_line(size = 0.5, linetype = "solid", colour = "#999999")
         )
p
dev.off()
p




