library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(lubridate)
library(compiler)
library(hues)

temp <- match1v1Clean
temp <- distinct(temp, MatchId, .keep_all = TRUE)
temp <- filter(temp, playerCiv != opponentCiv)
temp <- filter(temp, matchMap == "Arabia")
#temp <- filter(temp, (upReleaseVersion == "R6" | upReleaseVersion == "R7" & wk))
temp <- filter(temp, (wk))
#temp <- filter(temp, ( playerElo > 1700 & opponentElo > 1700 & (playerElo + opponentElo) /2) < 2000)
temp <- filter(temp, ( (playerElo > 2000 & opponentElo > 2000)))
temp <- arrange(temp, MatchDate)


temp$playerCiv <- factor(temp$playerCiv)
temp$opponentCiv <- factor(temp$opponentCiv)



# calculateEloChange <- function(winnerRating, loserRating) {

#   rW<- 10^(winnerRating / 400)
#   rL<- 10^(loserRating / 400)

#   eW <- rW / (rW + rL)
#   eL <- rL / (rW + rL)

#   afterW <- 16 * (1 - eW)
#   afterL <- 16 * (0 - eL)
#   c(afterW, afterL)

# }
# calculateEloChangeCmp <- cmpfun(calculateEloChange)

# allCivs <- levels(temp$playerCiv)

# ratings <- as.list(rep(1600, times=length(allCivs)))
# names(ratings) <- allCivs


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
# elo.withPlayer.2000 <- data.frame(matchDate = elo.matchDates, civ=elo.civ, eloBefore=elo.eloBefore, eloChange = elo.eloChange, eloAfter = elo.eloAfter)
# stop("ADASADS")



temp <- match1v1Clean

# secretIds <- data.frame(
#   pid = c(123211439, 123999216, 817, 12926, 123497926, 124236797, 124043892, 124147336),
#   pname = c("TheViper", "TaToH", "JorDan_23", "Our Lord and Savior", "slam", "T90Official", "DraCont", "Lierrey")
#   )
targetIds <- data.frame(
  pid = c(12926, 123211439, 124236797, 124043892, 124147336, 123303397, 123636492, 124954867),
  pname = c("Our Lord and Savior", "TheViper",  "T90", "DraCont", "Lierrey", "Fat Dragon", "Yo", "Hera")
  )

targetIds <- arrange(targetIds, pname)

eloToWorkWith <- match1v1Clean
eloToWorkWith <- filter(eloToWorkWith, wk)
eloToWorkWith <- filter(eloToWorkWith, MatchDate > "2018-01-01 00:00:00")

eloToWorkWith <- filter(eloToWorkWith, matchPlayerId %in% targetIds$pid)

eloToWorkWith <- mutate(eloToWorkWith, playerElo = ifelse(matchPlayerId == 12926, 2800, playerElo))

eloToWorkWith$matchPlayerId <-factor(eloToWorkWith$matchPlayerId, levels=targetIds$pid, labels=targetIds$pname)
#View(eloAverageAllTime)
eloToWorkWith$matchPlayerId <-relevel(eloToWorkWith$matchPlayerId, ref="Our Lord and Savior")

eloForPlot <- eloToWorkWith
#eloForPlot <- tail(eloForPlot, -)
#eloDailyAverage$date <- floor_date(as.Date(eloDailyAverage$matchDate), "week")
#eloDailyAverage$date <- as.Date(eloDailyAverage$matchDate)

#eloDailyAverage <- group_by(eloDailyAverage, civ, date) %>%
#                     summarise(elo = mean(eloAfter))

eloForPlot$date <- eloForPlot$MatchDate
eloForPlot$elo <- eloForPlot$playerElo
eloForPlot <- select(eloForPlot, date, elo, matchPlayerId)
eloForPlot <- rbind(eloForPlot, list(date = "2018-01-01 01:00:00", elo=2800, matchPlayerId = "Our Lord and Savior"))

sapFioriColors = c("#de7b45", "#5ba3d8", "#4d9f79", "#de6780", "#877cce", "#40a3b3", "#3582f1", "#b15f9f", "#708993", "#e2776e", "#2e6c96")


png(filename="temp/images/secretEloHistory.png", width=1920, height=1080)
p <- ggplot(data=eloForPlot, aes(x=date, y=elo, group=matchPlayerId)) +
        geom_point(aes(color=matchPlayerId), size=0.5, alpha=0.5) +
        stat_smooth(method="loess", level=0.95, span=0.3, aes(color=matchPlayerId, fill=matchPlayerId), alpha=0.1) +
        theme_bw(base_size=20)+
        theme(panel.border = element_blank(),
         axis.line = element_line(size = 0.5, linetype = "solid", colour = "#999999")
         ) +
        scale_x_datetime(date_breaks="1 month") +
        scale_y_continuous(breaks=((16:28)*100)) +
        scale_fill_manual(values=sapFioriColors, name = "Player") +
        scale_colour_manual(values=sapFioriColors, name = "Player") +
        ggtitle(paste("Elo over time | Our Lord and Savior + some scrubs"  , sep="")) +
        xlab('Date') +
        ylab('Player Elo')
p
dev.off()
p
stop("Adas")





# LIMITED CIV PLOTS

allCivsToWorkOn <- sort(unique(match1v1Clean$playerCiv))
sapFioriColors = c("#5ba3d8", "#de7b45", "#4d9f79", "#de6780", "#877cce", "#40a3b3", "#3582f1", "#b15f9f", "#708993", "#e2776e", "#2e6c96")
for (i in c(1,9,17,25)) {

  civsToWorkOn <- allCivsToWorkOn[i:(i+7 + ifelse(i == 25, -1, 0))]
  eloToWorkWith <- elo.withPlayer.1700
  eloToWorkWith <- filter(eloToWorkWith, matchDate > "2018-01-01 00:00:00")
  eloToWorkWithTotalLength <- length(eloToWorkWith$matchDate)/2

  eloToWorkWith <- filter(eloToWorkWith, civ %in% civsToWorkOn)

  eloAveragesLastX <- filter(eloToWorkWith, matchDate > "2018-09-01 00:00:00") %>%
    group_by(civ) %>%
    summarise(elo = mean(eloAfter))

  legendLabels <- paste(eloAveragesLastX$civ, " - ", round(eloAveragesLastX$elo), sep = "")

  eloForPlot <- eloToWorkWith
  eloForPlot$date <- eloForPlot$matchDate
  eloForPlot$elo <- eloForPlot$eloAfter
  set.seed(2)
  #colors <- unname(iwanthue(31, hmin=0, hmax=360, cmin=0, cmax=100, lmin=30, lmax=100, random = TRUE))

  fName <- paste("temp/images/2000-",civsToWorkOn[1],"-",civsToWorkOn[length(civsToWorkOn)], ".png", sep="")
  png(filename=fName, width=1920, height=1080)
  p <- ggplot(data=eloForPlot, aes(x=date, y=elo, group=civ)) +
        geom_point(aes(color=civ), size=0.5, alpha=0.2) +
        stat_smooth(method="loess", level=0.99, span=0.3, aes(color=civ, fill=civ), alpha=0.2) +
        theme_bw(base_size=20)+
        theme(panel.border = element_blank(),
         axis.line = element_line(size = 0.5, linetype = "solid", colour = "#999999")
         ) +
        scale_x_datetime(date_breaks="1 month") +
        scale_fill_manual(values=sapFioriColors, name = "Civ", labels=legendLabels) +
        scale_colour_manual(values=sapFioriColors, name = "Civ", labels=legendLabels) +
        coord_cartesian(ylim = c(1450, 1750)) +
        ggtitle(paste("Simulated Civ Elo | Non-mirror WK | Arabia | 1700-2000 | ", eloToWorkWithTotalLength, " matches"  , sep="")) +
        xlab('Date') +
        ylab('Simulated Civ Elo')

  print(p)
  dev.off()
}
stop("sadf")



# /////////////////









## CIV ELO PLOT OPTIMISED FOR ALL CIVS
eloToWorkWith <- elo.withPlayer.2000
eloToWorkWith <- filter(eloToWorkWith, matchDate > "2018-01-01 00:00:00")
eloAveragesAllTime <- group_by(eloToWorkWith, civ) %>%
  summarise(elo = mean(eloAfter), eloVar = var(eloAfter))

eloAveragesLastX <- filter(eloToWorkWith, matchDate > "2018-09-01 00:00:00") %>%
  group_by(civ) %>%
  summarise(elo = mean(eloAfter))

legendLabels <- paste(eloAveragesLastX$civ, " - ", round(eloAveragesLastX$elo), sep = "")

#View(eloAverageAllTime)


eloForPlot <- eloToWorkWith
#eloForPlot <- tail(eloForPlot, -)
#eloDailyAverage$date <- floor_date(as.Date(eloDailyAverage$matchDate), "week")
#eloDailyAverage$date <- as.Date(eloDailyAverage$matchDate)

#eloDailyAverage <- group_by(eloDailyAverage, civ, date) %>%
#                     summarise(elo = mean(eloAfter))

eloForPlot$date <- eloForPlot$matchDate
eloForPlot$elo <- eloForPlot$eloAfter



set.seed(2)
colors <- unname(iwanthue(31, hmin=0, hmax=360, cmin=0, cmax=100, lmin=30, lmax=100, random = TRUE))
#eloForPlot <- filter(eloForPlot, (civ == "Goths" | civ == "Burmese" | civ == "Berbers" | civ == "Teutons"))
#eloForPlot <- head(eloForPlot, 1000)
png(filename="temp/images/civEloHistory.png", width=3000, height=2000)
p <- ggplot(data=eloForPlot, aes(x=date, y=elo, group=civ)) +
        #geom_point(aes(color=civ), size=0.5, alpha=0.1) +
        stat_smooth(method="loess", geom="line", level=0.99, span=0.3, aes(color=civ, fill=civ), alpha=0.7, size=4) +
        theme_bw(base_size=20)+
        theme(panel.border = element_blank(),
         axis.line = element_line(size = 0.5, linetype = "solid", colour = "#999999")
         ) +
        scale_x_datetime(date_breaks="1 month") +
        scale_fill_manual(values=colors, name = "Civ") +
        scale_colour_manual(values=colors, name = "Civ", labels=legendLabels) +
        coord_cartesian(ylim = c(1400, 1750)) +
        ggtitle(paste("Simulated Civ Elo | Non-mirror WK | Arabia | 2000+ | ", length(eloForPlot$date)/2, " matches"  , sep="")) +
        xlab('Date') +
        ylab('Simulated Civ Elo')

p
dev.off()
