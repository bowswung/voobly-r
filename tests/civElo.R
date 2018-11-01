library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(lubridate)
library(compiler)

temp <- match1v1Clean
temp <- distinct(temp, MatchId, .keep_all = TRUE)
temp <- filter(temp, playerCiv != opponentCiv)
temp <- filter(temp, matchMap == "Arabia")
#temp <- filter(temp, (upReleaseVersion == "R6" | upReleaseVersion == "R7" & wk))
temp <- filter(temp, (wk))
temp <- filter(temp, ( playerElo > 1700 & opponentElo > 1700 & (playerElo + opponentElo) /2) < 2000)
temp <- arrange(temp, MatchDate)


temp$playerCiv <- factor(temp$playerCiv)
temp$opponentCiv <- factor(temp$opponentCiv)



calculateEloChange <- function(winnerRating, loserRating) {

  rW<- 10^(winnerRating / 400)
  rL<- 10^(loserRating / 400)

  eW <- rW / (rW + rL)
  eL <- rL / (rW + rL)

  afterW <- 16 * (1 - eW)
  afterL <- 16 * (0 - eL)
  c(afterW, afterL)

}
calculateEloChangeCmp <- cmpfun(calculateEloChange)

allCivs <- levels(temp$playerCiv)

ratings <- as.list(rep(1600, times=length(allCivs)))
names(ratings) <- allCivs


# lookupLastElo <- function(targetCiv, eloData) {
#   if (length(eloData[eloData$civ == targetCiv, ]$eloAfter) > 0) {
#     tail(eloData[eloData$civ == targetCiv, ], 1)$eloAfter[1]
#     } else {
#       1600
#     }
# }


# allCivs <-  tidyr::expand(temp, playerCiv)
# nDate <- as.POSIXct("2018-09-11 00:00:00")
# elo.matchDates <- rep(nDate, nrow(temp)*2)
# elo.civ <- rep(temp$playerCiv[1], nrow(temp)*2)
# elo.eloBefore <- rep(0, nrow(temp)*2)
# elo.eloChange <- rep(0, nrow(temp)*2)
# elo.eloAfter <- rep(0, nrow(temp)*2)
# tempToWorkWith <- temp
# tempToWorkWith$playerCiv <- as.character(tempToWorkWith$playerCiv)
# tempToWorkWith$opponentCiv <- as.character(tempToWorkWith$opponentCiv)
# tempToWorkWith$winCiv <- ifelse(tempToWorkWith$winner, tempToWorkWith$playerCiv, tempToWorkWith$opponentCiv)
# tempToWorkWith$winPlayerElo <- ifelse(tempToWorkWith$winner, tempToWorkWith$playerElo, tempToWorkWith$opponentElo)
# tempToWorkWith$loseCiv <- ifelse(tempToWorkWith$winner, tempToWorkWith$opponentCiv, tempToWorkWith$playerCiv)
# tempToWorkWith$losePlayerElo <- ifelse(tempToWorkWith$winner, tempToWorkWith$opponentElo, tempToWorkWith$playerElo)

# tempToWorkWith$winCiv <- factor(tempToWorkWith$winCiv)
# tempToWorkWith$loseCiv <- factor(tempToWorkWith$loseCiv)


# for (i in 1:nrow(tempToWorkWith)) {
#   if (i %% 500 == 0) {
#     print(i)
#   }
#   j <- 1 + ((i -1)*2)
#   k <- j+1
#   winCivPreviousElo <- as.numeric(ratings[tempToWorkWith$winCiv[i]])
#   loseCivPreviousElo <- as.numeric(ratings[tempToWorkWith$loseCiv[i]])


#   afterElos <- calculateEloChangeCmp(winCivPreviousElo + tempToWorkWith$winPlayerElo[i], loseCivPreviousElo + tempToWorkWith$losePlayerElo[i])
#   winCivEloAfter <- winCivPreviousElo + afterElos[1]/2
#   loseCivEloAfter <- loseCivPreviousElo + afterElos[2]/2


#   elo.matchDates[j] = tempToWorkWith$MatchDate[i]
#   elo.civ[j] = tempToWorkWith$winCiv[i]
#   elo.eloBefore[j] = winCivPreviousElo
#   elo.eloChange[j] = afterElos[1]/2
#   elo.eloAfter[j] = winCivEloAfter

#   elo.matchDates[k] = tempToWorkWith$MatchDate[i]
#   elo.civ[k] = tempToWorkWith$loseCiv[i]
#   elo.eloBefore[k] = loseCivPreviousElo
#   elo.eloChange[k] = afterElos[2]/2
#   elo.eloAfter[k] = loseCivEloAfter


#   ratings[tempToWorkWith$winCiv[i]] = winCivEloAfter
#   ratings[tempToWorkWith$loseCiv[i]] = loseCivEloAfter
# }
# elo <- data.frame
# elo.withPlayer.1700 <- data.frame(matchDate = elo.matchDates, civ=elo.civ, eloBefore=elo.eloBefore, eloChange = elo.eloChange, eloAfter = elo.eloAfter)
# stop("ADASADS")


eloToWorkWith <- elo.withPlayer.1700
eloToWorkWith <- filter(eloToWorkWith, matchDate > "2018-01-01 00:00:00")
eloAverageAllTime <- group_by(eloToWorkWith, civ) %>%
  summarise(elo = mean(eloAfter))
#View(eloAverageAllTime)


eloForPlot <- eloToWorkWith
#eloForPlot <- tail(eloForPlot, -)
#eloDailyAverage$date <- floor_date(as.Date(eloDailyAverage$matchDate), "week")
#eloDailyAverage$date <- as.Date(eloDailyAverage$matchDate)

#eloDailyAverage <- group_by(eloDailyAverage, civ, date) %>%
#                     summarise(elo = mean(eloAfter))

eloForPlot$date <- eloForPlot$matchDate
eloForPlot$elo <- eloForPlot$eloAfter

eloForPlot <- filter(eloForPlot, (civ == "Franks" | civ == "Khmer" | civ == "Aztecs" | civ == "Koreans" | civ == "Celts" | civ =="Mayans" | civ == "Vietnamese"))
png(filename="temp/images/civEloHistory.png", width=1920, height=1080)
p <- ggplot(data=eloForPlot, aes(x=date, y=elo, group=civ)) +
        geom_point(aes(color=civ), size=0.5, alpha=0.1) +
        stat_smooth(method="loess", span=0.3, aes(color=civ, fill=civ), alpha=0.1) +
        theme_bw(base_size=14)+
        theme(panel.border = element_blank(),
         axis.line = element_line(size = 0.5, linetype = "solid", colour = "#999999")
         ) +
        scale_x_datetime(date_breaks="1 month")
p
dev.off()
p




