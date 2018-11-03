library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(rms)
library(purrr)
library(DescTools)
library(corrplot)
source("tests/singleCivMatchup.R")

#temp <- match1v1CleanMatchupFixed



# temp <- filter(match1v1Clean, playerCiv == "Saracens")
# temp <- filter(temp, playerCiv != opponentCiv)
# #temp <- tempGoths
# temp <- filter(temp, matchMap == "Arabia")
# #temp <- filter(temp, wk)
# temp <- filter(temp, (playerElo > 1650 & opponentElo > 1650))
# #temp <- filter(temp, (upReleaseVersion == "R6" | upReleaseVersion == "R7" & wk))

# #temp <- mutate(temp, cutoff = ifelse(((playerElo + opponentElo) /2) >=2000, "2000+", "1700-2000"))
# #temp$cutoff <- factor(temp$cutoff)

# temp<-filter(temp, MatchDuration < 7200 & MatchDuration > 500)

# temp <- arrange(temp, MatchDuration)
# temp$groupingVar <- rep(seq.int(0, length(temp$matchId)), each=100, length.out=length(temp$matchId))

# tempPlot <- temp %>%
#     group_by(groupingVar) %>%
#     summarise(expected = mean(1/(1+10^((-(eloGap))/400))), meanWinning = mean(winner), count=n(), meanDuration = mean(MatchDuration))

# models.1v1.civMatchups.logit <- glm(winner ~ eloGap + MatchDuration, data=temp, family = "binomial")
# summary(models.1v1.civMatchups.logit)
# PseudoR2(models.1v1.civMatchups.logit, which="Tjur")
# anova(models.1v1.civMatchups.logit, test="Chisq")

# drange <-  tidyr::expand(temp, MatchDuration = 0:7200)
# drange$eloGap = 0
# dplot <- cbind(drange, predict(models.1v1.civMatchups.logit, newdata = drange, type = "link", se = TRUE))
# dplot <- within(dplot, {
#     PredictedProb <- plogis(fit)
#     LL <- plogis(fit - (1.96 * se.fit))
#     UL <- plogis(fit + (1.96 * se.fit))
#   })

# png(filename="temp/images/1v1-civMatchup-byDuration.png", width=1600, height=600)

# models.1v1.civMatchups.logit <- ggplot(dplot, aes(x = MatchDuration, y = PredictedProb)) +
#   labs(x = "Match length", y = "Probability of winning match") +
#   geom_ribbon(data=dplot, aes(x=MatchDuration, y=PredictedProb, ymin = LL, ymax = UL), alpha = 0.5, fill="#ff0000", color="#ff0000") +
#   geom_point(data=tempPlot, aes(x=meanDuration, y=meanWinning), alpha=1) +
#   ggtitle(paste("Non-mirror WK | Arabia | 1.5 R7 |", length(unique(temp$matchId)) ,"matches")) +
#   theme_bw(base_size=14) +
#   theme(panel.border = element_blank(),
#      axis.line = element_line(size = 0.5, linetype = "solid", colour = "#999999")
#     )

# models.1v1.civMatchups.logit
# dev.off()
# stop("ASFAS")




# THIS IS FOR A CORR MATRIZ
tempToFocusOn <- match1v1CleanMatchupFixed
#tempToFocusOn <- filter(tempToFocusOn, abs(eloGapFixed) >= 100)
tempToFocusOn <- filter(tempToFocusOn, ( playerElo > 1700 & opponentElo > 1700 & ((playerElo + opponentElo) /2) < 2000))
#tempToFocusOn <- filter(tempToFocusOn, (playerElo > 1700 & opponentElo > 1700))

tempToFocusOn <- filter(tempToFocusOn, MatchDate > "2018-06-01 00:00:00")
tempToFocusOn <- filter(tempToFocusOn, wk & matchMap == "Arabia")
#tempToFocusOn <- filter(tempToFocusOn, playerCivFixed != opponentCivFixed)
#tempToFocusOn <- filter(tempToFocusOn, upReleaseVersion == "R7")
#empToFocusOn <- filter(tempToFocusOn, MatchDate > "2018-01-01 00:00:00")

# singleCivMatchup(tempToFocusOn, "Goths-Mayans")
# stop("asdf")
crange <-  tidyr::expand(tempToFocusOn, matchupFixed)
cplot <- rowwise (crange) %>% do (
    binTest = singleCivMatchup(tempToFocusOn, .$matchupFixed),
    matchupFixed = as.character(.$matchupFixed),
    civs = unlist(strsplit(as.character(.$matchupFixed), "-", TRUE)),
    countMatches = length(unique(tempToFocusOn[tempToFocusOn$matchupFixed == .$matchupFixed, ]$matchId))
  )

cplotSide1 <- rowwise(cplot) %>% mutate(
  civ1 = civs[1],
  civ2 = civs[2],
  winRate = (binTest$estimate),
  winRateProb = binTest$civP
  )

cplotSide2 <- rowwise(cplot) %>% mutate(
  civ1 = civs[2],
  civ2 = civs[1],
  winRate = -(binTest$estimate),
  winRateProb = binTest$civP
  )
cplotSide2 <- filter(cplotSide2, civ1 != civ2)
cplot <- rbind(cplotSide1, cplotSide2)
cplot$civ1 <- factor(cplot$civ1)
cplot$civ2 <- factor(cplot$civ2)
cMatrix <- xtabs(winRate ~ civ1 + civ2, data=cplot)
cPvals <- xtabs(winRateProb ~ civ1 + civ2, data=cplot)

png(filename="temp/images/civCorrPlotWithMirrors.png", width=1200, height=1200)

cplotDone <- corrplot(cMatrix,
  title=paste("Matchup Balance | WK after June 2018 | Arabia | 1700-2000 | ", length(unique(tempToFocusOn$matchId)), " matches", sep=""),
  mar=c(0,0,2,0),
  p.mat=cPvals,
  insig = "n",
  sig.level=c(.001, .01, .05), pch.cex = .9, pch.col = "white",
  method = "circle",
  is.corr=FALSE,
  tl.col = "#333333",
  col=colorRampPalette(c("#cf3e25", "white", "#5d995b"))(100),
  cl.lim =c(-0.4,0.4),
  number.cex=1,
  tl.cex = 1.2,
  cl.cex=1.2
  )

dev.off()
png(filename="temp/images/civCorrPlotWithMirrorsNumber.png", width=1600, height=1600)


cplotDone <- corrplot(cMatrix,
  title=paste("Matchup Balance | WK after June 2018 | Arabia | 1700-2000 | ", length(unique(tempToFocusOn$matchId)), " matches", sep=""),
  mar=c(0,0,2,0),
  p.mat=cPvals,
  insig = "n",
  sig.level=c(.001, .01, .05), pch.cex = .9, pch.col = "white",
  method = "color",
  is.corr=FALSE,
  tl.col = "#333333",
  col=colorRampPalette(c("#cf3e25", "white", "#5d995b"))(100),
  cl.lim =c(-0.4,0.4),
  number.cex=1,
  addCoef.col="#333333",
  tl.cex = 1.5,
  cl.cex=1.5
  )


dev.off()


stop("asdf")




# flipMatchup <- function(x) {
#   s <- unlist(strsplit(x, "-", TRUE))
#   paste(s[2], "-", s[1], sep="")
# }

dplot <- rowwise(dplot) %>% mutate(
    winRate = ifelse(binTest$estimate > 0.5, binTest$estimate, 1-binTest$estimate),
    LL = ifelse(binTest$estimate > 0.5, binTest$estimateLwr, 1-binTest$estimateLwr),
    UL = ifelse(binTest$estimate > 0.5, binTest$estimateUpr, 1-binTest$estimateUpr),
    winRateProb = binTest$civP,
    matchupOriginal = matchupFixed,
    matchupFixed = ifelse(binTest$estimate > 0.5, matchupFixed, flipMatchup(matchupFixed)),
    shouldNotFlip = binTest$estimate > 0.5
  )

# dplot <- within(dplot, {
#     winRate <- plogis(fit)
#     shouldFlip <- winRate < 0.5
#     LL <- plogis(fit - (1.96 * se.fit))
#     UL <- plogis(fit + (1.96 * se.fit))
#   })
dplot <- dplot[c("matchupFixed", "winRate", "LL", "UL", "countMatches", "type", "shouldNotFlip", "matchupOriginal", "winRateProb")]

dplot$matchupFixed <- as.character(dplot$matchupFixed)
dplot$matchupFixed <- factor(dplot$matchupFixed)

drangeObserved <- filter(tempCivFixed, eloGap > -50 & eloGap < 50)
drangeObserved <- group_by(drangeObserved, matchupFixed) %>%
                    summarise(winRate = mean(winnerFixed), LL=mean(winnerFixed), UL=mean(winnerFixed), countMatches = n())
drangeObserved$type = "Observed"

dplot <- arrange(dplot, matchupOriginal)
drangeObserved <- arrange(drangeObserved, matchupFixed)
drangeObserved$shouldNotFlip <- dplot$shouldNotFlip



dplot <- dplot[c("matchupFixed", "winRate", "LL", "UL", "countMatches", "type", "shouldNotFlip", "winRateProb")]

drangeObserved$matchupFixed <- as.character(drangeObserved$matchupFixed)
drangeObserved <- rowwise(drangeObserved) %>% mutate(
    winRate = ifelse(shouldNotFlip, winRate, (1 - winRate)),
    LL = ifelse(shouldNotFlip, LL, (1 - LL)),
    UL = ifelse(shouldNotFlip, UL, (1 - UL)),
    matchupFixed = ifelse(shouldNotFlip, matchupFixed, flipMatchup(matchupFixed)),
    winRateProb = 1
  )
drangeObserved$matchupFixed <- as.character(drangeObserved$matchupFixed)

drangeObserved$matchupFixed <- factor(drangeObserved$matchupFixed)


dplotFinal <- rbind(dplot, drangeObserved)
dplotFinal$matchupFixed <- as.character(dplotFinal$matchupFixed)


dplotFinal$matchupFixed <- as.character(dplotFinal$matchupFixed)
dplotFinal <- mutate(dplotFinal, probForOrder = ifelse(type =="Predicted", winRate, 0))
dplotFinal$matchupFixed <- reorder(dplotFinal$matchupFixed, dplotFinal$probForOrder)
dplotFinal$type <- as.character(dplotFinal$type)

sigLevel = 0.001
includeMatchups <- filter(dplotFinal, winRateProb < sigLevel)$matchupFixed
dplotFinal <- filter(dplotFinal, matchupFixed %in% includeMatchups)

dplotFinal$winRate = dplotFinal$winRate * 100
dplotFinal$LL = dplotFinal$LL * 100
dplotFinal$UL = dplotFinal$UL * 100


dplotFinal <- rowwise(dplotFinal) %>% mutate(
  barLabel = ifelse(type == "Predicted", paste("n = ", as.character(countMatches), ",", " p = ", prettyNum(winRateProb, digits=3), sep="") , paste("n = ", as.character(countMatches), sep="")),
  labelColor = ifelse(type == "Predicted", "#FFFFFF", "#000000")

)



png(filename="temp/images/1v1-1700-2000-civMatchup.png", width=1600, height=600)

models.1v1.civMatchups.plot <- ggplot(dplotFinal, aes(fill=type, x = matchupFixed, y = winRate)) +
  labs(x = "matchup", y = "Win rate percent") +
  geom_bar( width=0.7, position=position_dodge(width=0.7), stat="identity", alpha=1) +
  geom_errorbar(aes(ymin=LL, ymax=UL, color=type, alpha=ifelse(type == "Predicted", 1, 0)), show.legend = FALSE,
                  width=.2,
                  position=position_dodge(0.7)) +
  geom_text(vjust=0.5, hjust=0,  position=position_dodge(0.7), angle=90, aes(label=barLabel, y = 3 ), show.legend = FALSE, size=3.5) +
  scale_x_discrete(labels = function(x){x}) +
  ggtitle(paste("Non-mirror WK | Arabia | 1.5 R6/R7 | 1700-2000 Elo | ", length(unique(temp$matchId)), " matches")) +
  theme_bw(base_size=14)+
  theme(panel.border = element_blank(),
     axis.line = element_line(size = 0.5, linetype = "solid", colour = "#999999")
    )+
  scale_fill_manual(values=c("#b6d8f4", "#5ba3d8"), name = "Type") +
  scale_colour_manual(values=c("#78b3e0", "#0974b3")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

models.1v1.civMatchups.plot
dev.off()

models.1v1.civMatchups.plot



stop("asdfs")

temp <- arrange(temp, eoGap)

temp <- mutate(temp, eloGapStepped = eloGap)

temp$groupingVar <- rep(seq.int(0, length(temp$matchId)), each=1000, length.out=length(temp$matchId))

tempPlot <- temp %>%
    group_by(groupingVar) %>%
    summarise(meanWinning = mean(winner), meanElogap = mean(eloGap))


m <- lm(meanWinning ~ meanElogap, data = tempPlot)
summary(m)

ggplot(tempPlot, aes(x=meanElogap, y = meanWinning)) +
  geom_point()

stop("asdfsf")

m <- glm(meanWinning ~ meanElogap, data=temp)

summary(models.1v1.eloPlusSkillGap.logit)
confint.default(models.1v1.eloPlusSkillGap.logit)

anova(models.1v1.eloPlusSkillGap.logit, test="Chisq")

View(tempPlot)
stop("asfsdf")



m <- glm(winner ~ eloGap, data=temp, family = "binomial")
drange <-  tidyr::expand(temp, eloGap = -800:800)
dplot <- cbind(drange, predict(m, newdata = drange, type = "link", se = TRUE))
dplot <- within(dplot, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })


ggplot(tempPlot, aes(x=eloGapStepped, y = meanWinning)) +
  geom_point(aes(size=countWins)) +
  scale_x_continuous(limits = c(-500, 500), breaks = round(seq(-500, 500, by = 50))) +
  geom_bar(data = tempPlot, fill="red", stat="identity", aes(x=eloGapStepped, y = countWins), alpha=0.3) +

  scale_size_continuous(range = c(0.1, 2))+
  geom_ribbon(data=dplot, aes(x=eloGap, y=PredictedProb, ymin = LL,
    ymax = UL), alpha = 1, color="#ff0000")


stop("ADS")

# #model
# models.1v1.elo.logit <- glm(winner ~ eloGap:cutoff, data=tempWithCutoff, family = "binomial")
# summary(models.1v1.elo.logit)
# confint.default(models.1v1.elo.logit)

# # #accuracy
# # tempWithCutoff$pred <- predict(models.1v1.elo.logit, newdata=tempWithCutoff, type="response")
# # tempWithCutoff$predBool <- ifelse(tempWithCutoff$pred > 0.5, TRUE, FALSE)
# # models.1v1.elo.accuracy <- mean(temp$winner == temp$predBool)

# png(filename="images/1v1-1800-eloGap.png", width=800, height=600)

# drange <-  tidyr::expand(tempWithCutoff, cutoff, eloGap = -500:500)
# dplot <- cbind(drange, predict(models.1v1.elo.logit, newdata = drange, type = "link", se = TRUE))
# dplot <- within(dplot, {
#     PredictedProb <- plogis(fit)
#     LL <- plogis(fit - (1.96 * se.fit))
#     UL <- plogis(fit + (1.96 * se.fit))
#   })

# models.1v1.elo.plot <- ggplot(dplot, aes(x = eloGap, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
#     ymax = UL, fill=cutoff), alpha = 0.2) + labs(x = "Elo difference", y = "Probability of winning match") + geom_line(aes(colour = cutoff),
#     size = 1)
# models.1v1.elo.plot
# dev.off()
# models.1v1.elo.plot
# stop("asdfsadf")


# #model
# models.1v1.eloPlusCiv.logit <- glm(winner ~ eloGap + playerCiv, data=temp, family = "binomial")
# summary(models.1v1.eloPlusCiv.logit)
# confint.default(models.1v1.eloPlusCiv.logit)

# #acccuracy
# temp$pred <- predict(models.1v1.eloPlusCiv.logit, newdata=temp, type="response")
# temp$predBool <- ifelse(temp$pred > 0.5, TRUE, FALSE)
# models.1v1.eloPlusCiv.accuracy <- mean(temp$winner == temp$predBool)


# # test whether civ increases model fit
# anova(models.1v1.eloPlusCiv.logit, test="Chisq")










#model
tempNoNewCivs <- filter(temp, !(as.character(playerCiv) %in% c('Khmer', 'Italians', 'Magyars', 'Indians', 'Malay', 'Slavs', 'Vietnamese', 'Incas', 'Burmese', 'Ethiopian', 'Portuguese', 'Berbers', 'Malian')))
tempNoNewCivs$playerCiv <- droplevels(tempNoNewCivs$playerCiv)

models.1v1.eloPlusCivPlusWK.logit <- glm(winner ~ eloGap + playerCiv + playerCiv:wk, data=tempNoNewCivs, family = "binomial")
summary(models.1v1.eloPlusCivPlusWK.logit)
confint.default(models.1v1.eloPlusCivPlusWK.logit)

#acccuracy
tempNoNewCivs$pred <- predict(models.1v1.eloPlusCivPlusWK.logit, newdata=tempNoNewCivs, type="response")
tempNoNewCivs$predBool <- ifelse(tempNoNewCivs$pred > 0.5, TRUE, FALSE)
models.1v1.eloPlusCivPlusWK.accuracy <- mean(tempNoNewCivs$winner == tempNoNewCivs$predBool)


# test model fit
anova(models.1v1.eloPlusCivPlusWK.logit, test="Chisq")


#plot of elo influence


drange <-  tidyr::expand(tempNoNewCivs, playerCiv, wk)
drange$eloGap = 0
dplot <- cbind(drange, predict(models.1v1.eloPlusCivPlusWK.logit, newdata = drange, type = "link", se = TRUE))
dplot <- within(dplot, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
dplot <- mutate(dplot, probForOrder = ifelse(wk, 0, PredictedProb))
dplot$playerCiv <- reorder(dplot$playerCiv, dplot$probForOrder)

png(filename="images/1v1-1800-civPlusWk.png", width=1200, height=600)

models.1v1.eloPlusCivPlusWK.plot <- ggplot(dplot, aes(fill=wk, x = playerCiv, y = PredictedProb)) +
  labs(x = "Player civ", y = "Probability of winning match") +
  geom_bar(width=0.7, position=position_dodge(width=0.7), stat="identity", alpha=0.8) +
  geom_errorbar(aes(ymin=LL, ymax=UL),
                  width=.2,
                  color="#666666",
                  position=position_dodge(0.7)) +
  scale_x_discrete(labels = function(x) toupper(substr(x, 0, 3)))

models.1v1.eloPlusCivPlusWK.plot
dev.off()
models.1v1.eloPlusCivPlusWK.plot







png(filename="images/1v1-1800-eloGap.png", width=800, height=600)

drange <- data.frame(eloGap = -500:500)
dplot <- cbind(drange, predict(models.1v1.elo.logit, newdata = drange, type = "link", se = TRUE))
dplot <- within(dplot, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })

models.1v1.elo.plot <- ggplot(dplot, aes(x = eloGap, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
    ymax = UL), alpha = 0.5, fill = "darkred") + labs(x = "Elo difference", y = "Probability of winning match")
models.1v1.elo.plot
dev.off()

drange <- data.frame(eloGap = 0, playerCiv = levels(temp$playerCiv))
dplot <- cbind(drange, predict(models.1v1.eloPlusCiv.logit, newdata = drange, type = "link", se = TRUE))
dplot <- within(dplot, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
dplot$playerCiv <- reorder(dplot$playerCiv, dplot$PredictedProb)

png(filename="images/1v1-1800-civ.png", width=1200, height=600)

models.1v1.eloPlusCiv.plot <- ggplot(dplot, aes(x = playerCiv, y = PredictedProb)) +
  geom_bar(width=0.7, position=position_dodge(width=0.6), stat="identity", fill="darkred", alpha=0.6) +
  labs(x = "Player civ", y = "Probability of winning match") +
  geom_errorbar(aes(ymin=LL, ymax=UL),
                  color = "darkred",
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
  scale_x_discrete(labels = function(x) toupper(substr(x, 0, 3)))

models.1v1.eloPlusCiv.plot
dev.off()


# for no new civs
# tempNoNewCivs <- filter(temp, !(as.character(playerCiv) %in% c('Khmer', 'Italians', 'Magyars', 'Indians', 'Malay', 'Slavs', 'Vietnamese', 'Incas', 'Burmese', 'Ethiopian', 'Portuguese')))
# tempNoNewCivs$playerCiv <- droplevels(tempNoNewCivs$playerCiv)


# this is arabia vs arena

# temp <- filter(temp, matchMap == "Arabia" | matchMap == "Arena")
# temp <- filter(temp, wk == TRUE)
# models.1v1.eloPlusMap.logit <- glm(winner ~ eloGap + playerCiv + playerCiv:matchMap, data=temp, family = "binomial")

# anova(models.1v1.eloPlusMap.logit, test="Chisq")

# drange <-  tidyr::expand(temp, playerCiv, matchMap)
# drange$eloGap = 0
# dplot <- cbind(drange, predict(models.1v1.eloPlusMap.logit, newdata = drange, type = "link", se = TRUE))
# dplot <- within(dplot, {
#     PredictedProb <- plogis(fit)
#     LL <- plogis(fit - (1.96 * se.fit))
#     UL <- plogis(fit + (1.96 * se.fit))
#   })
# dplot <- mutate(dplot, probForOrder = ifelse(matchMap =="Arena", 0, PredictedProb))
# dplot$playerCiv <- reorder(dplot$playerCiv, dplot$probForOrder)

# png(filename="images/1v1-1800-civPlusMap.png", width=1200, height=600)

# models.1v1.eloPlusCivPlusMap.plot <- ggplot(dplot, aes(fill=matchMap, x = playerCiv, y = PredictedProb)) +
#   labs(x = "Player civ", y = "Probability of winning match") +
#   geom_bar(width=0.7, position=position_dodge(width=0.7), stat="identity", alpha=0.8) +
#   geom_errorbar(aes(ymin=LL, ymax=UL),
#                   width=.2,
#                   color="#666666",
#                   position=position_dodge(0.7)) +
#   scale_x_discrete(labels = function(x) toupper(substr(x, 0, 3)))

# models.1v1.eloPlusCivPlusMap.plot
# dev.off()

# models.1v1.eloPlusCivPlusMap.plot

# stop("asdf")



# #this is arabia vs arena

# temp <- filter(temp, matchMap == "Arabia" | matchMap == "Arena")
# temp$matchMap <- factor(temp$matchMap)


# models.1v1.eloPlusMap.logit <- glm(winner ~ eloGap + playerCiv + playerCiv:matchMap, data=temp, family = "binomial")

# anova(models.1v1.eloPlusMap.logit, test="Chisq")

# drange <-  tidyr::expand(temp, playerCiv, matchMap)
# drange$eloGap = 0
# dplot <- cbind(drange, predict(models.1v1.eloPlusMap.logit, newdata = drange, type = "link", se = TRUE))
# dplot <- within(dplot, {
#     PredictedProb <- plogis(fit)
#     LL <- plogis(fit - (1.96 * se.fit))
#     UL <- plogis(fit + (1.96 * se.fit))
#   })
# #dplot <- mutate(dplot, probForOrder = ifelse(matchMap =="Arena", 0, PredictedProb))
# dplot$playerCiv <- reorder(dplot$playerCiv, dplot$PredictedProb)

# png(filename="images/1v1-1800-civPlusMap.png", width=1200, height=600)

# models.1v1.eloPlusCivPlusMap.plot <- ggplot(dplot, aes(fill=matchMap, x = playerCiv, y = PredictedProb)) +
#   labs(x = "Player civ", y = "Probability of winning match") +
#   geom_bar(width=0.7, position=position_dodge(width=0.7), stat="identity", alpha=0.8) +
#   geom_errorbar(aes(ymin=LL, ymax=UL),
#                   width=.2,
#                   color="#666666",
#                   position=position_dodge(0.7)) +
#   scale_x_discrete(labels = function(x) toupper(substr(x, 0, 3)))

# models.1v1.eloPlusCivPlusMap.plot
# dev.off()

# models.1v1.eloPlusCivPlusMap.plot

# stop("asdfsdf")

# # FINAL CUTOFF code

# tempWithCutoff <- mutate(temp, cutoff = ifelse(((playerElo + opponentElo) /2 >=2200), "2200+", "1800-2200"))

# models.1v1.eloPlusSkillGap.logit <- glm(winner ~ eloGap + playerCiv + cutoff + cutoff:playerCiv, data=tempWithCutoff, family = "binomial")
# summary(models.1v1.eloPlusSkillGap.logit)
# confint.default(models.1v1.eloPlusSkillGap.logit)

# anova(models.1v1.eloPlusSkillGap.logit, test="Chisq")


# uniqueMatches =
# drange <-  tidyr::expand(tempWithCutoff, playerCiv, cutoff)
# drange$eloGap = 0
# drange <- rowwise(drange) %>%
#   mutate(countMatches = length(unique(tempWithCutoff[tempWithCutoff$playerCiv == playerCiv & tempWithCutoff$cutoff == cutoff, ]$matchId)))
# dplot <- cbind(drange, predict(models.1v1.eloPlusSkillGap.logit, newdata = drange, type = "link", se = TRUE))
# dplot <- within(dplot, {
#     oPlayerCiv <- playerCiv
#     PredictedProb <- plogis(fit)
#     LL <- plogis(fit - (1.96 * se.fit))
#     UL <- plogis(fit + (1.96 * se.fit))
#   })
# dplot <- mutate(dplot, probForOrder = ifelse(cutoff =="2200+", 0, PredictedProb))
# dplot$playerCiv <- reorder(dplot$playerCiv, dplot$probForOrder)

# png(filename="images/1v1-1800-civPlusCutoff.png", width=1600, height=600)

# models.1v1.eloPlusCivPlusCutoff.plot <- ggplot(dplot, aes(fill=cutoff, x = playerCiv, y = PredictedProb)) +
#   labs(x = "Player civ", y = "Probability of winning match") +
#   geom_bar(width=0.7, position=position_dodge(width=0.7), stat="identity", alpha=0.8) +
#   geom_errorbar(aes(ymin=LL, ymax=UL),
#                   width=.2,
#                   color="#666666",
#                   position=position_dodge(0.7)) +
#   geom_label(label.padding = unit(0.15, "lines"), position=position_dodge(0.7), aes(label=countMatches), size=3.5, label.size=0) +
#   scale_x_discrete(labels = function(x) toupper(substr(x, 0, 3))) +
#   ggtitle(paste("Non-mirror WK | 1.5 R6 | ", length(unique(tempWithCutoff$matchId)), " matches"))

# models.1v1.eloPlusCivPlusCutoff.plot
# dev.off()

# models.1v1.eloPlusCivPlusCutoff.plot
# stop("asdfs")

# # FINAL VERSIONED

# tempNoNewCivs <- filter(temp, !(as.character(playerCiv) %in% c('Khmer', 'Italians', 'Magyars', 'Indians', 'Malay', 'Slavs', 'Vietnamese', 'Incas', 'Burmese', 'Ethiopian', 'Portuguese', 'Berbers', 'Malian')))
# tempNoNewCivs <- filter(tempNoNewCivs, !(as.character(opponentCiv) %in% c('Khmer', 'Italians', 'Magyars', 'Indians', 'Malay', 'Slavs', 'Vietnamese', 'Incas', 'Burmese', 'Ethiopian', 'Portuguese', 'Berbers', 'Malian')))

# tempNoNewCivs$playerCiv <- droplevels(tempNoNewCivs$playerCiv)

# tempNoNewCivs <- mutate(tempNoNewCivs,
#     version = ifelse(wk, ifelse(uplatest, "WK - 1.5R6", "WK - Other"), "Non-WK")
#   )
# tempNoNewCivs$version <- factor(tempNoNewCivs$version, levels= c("Non-WK", "WK - Other", "WK - 1.5R6"))

# models.1v1.balanceCompare.logit <- glm(winner ~ eloGap + playerCiv + version + playerCiv:version, data=tempNoNewCivs, family = "binomial")
# summary(models.1v1.balanceCompare.logit)
# confint.default(models.1v1.balanceCompare.logit)

# anova(models.1v1.balanceCompare.logit, test="Chisq")


# drange <-  tidyr::expand(tempNoNewCivs, playerCiv, version)
# drange$eloGap = 0
# drange <- rowwise(drange) %>%
#   mutate(countMatches = length(unique(tempNoNewCivs[tempNoNewCivs$playerCiv == playerCiv & tempNoNewCivs$version == version, ]$matchId)))
# dplot <- cbind(drange, predict(models.1v1.balanceCompare.logit, newdata = drange, type = "link", se = TRUE))
# dplot <- within(dplot, {
#     PredictedProb <- plogis(fit)
#     LL <- plogis(fit - (1.96 * se.fit))
#     UL <- plogis(fit + (1.96 * se.fit))
#   })
# dplot <- mutate(dplot, probForOrder = ifelse(version =="Non-WK", PredictedProb, 0))
# dplot$playerCiv <- reorder(dplot$playerCiv, dplot$probForOrder)

# png(filename="images/1v1-1800-civPlusVersion.png", width=1600, height=600)

# models.1v1.eloPlusVersion.plot <- ggplot(dplot, aes(fill=version, x = playerCiv, y = PredictedProb)) +
#   labs(x = "Player civ", y = "Probability of winning match") +
#   geom_bar(width=0.7, position=position_dodge(width=0.7), stat="identity", alpha=0.8) +
#   geom_errorbar(aes(ymin=LL, ymax=UL),
#                   width=.2,
#                   color="#666666",
#                   position=position_dodge(0.7)) +
#   geom_label(label.padding = unit(0.15, "lines"), position=position_dodge(0.7), aes(label=countMatches), size=3.5, label.size=0) +
#   scale_x_discrete(labels = function(x) toupper(substr(x, 0, 3))) +
#   ggtitle(paste("Non-mirror 1800+ Arabia | Balance comparison (WK civs excluded as opponents) | ", length(unique(tempNoNewCivs$matchId)), " matches"))

# models.1v1.eloPlusVersion.plot
# dev.off()

# models.1v1.eloPlusVersion.plot
# stop("asdfs")

# # MAP
# temp$matchMap <- factor(temp$matchMap)

# models.1v1.eloPlusMap.logit <- glm(winner ~ eloGap + playerCiv + playerCiv:matchMap, data=temp, family = "binomial")

# anova(models.1v1.eloPlusMap.logit, test="Chisq")

# drange <-  tidyr::expand(temp, playerCiv, matchMap)
# drange$eloGap = 0
# drange <- rowwise(drange) %>%
#   mutate(countMatches = length(unique(temp[temp$playerCiv == playerCiv & temp$matchMap == matchMap, ]$matchId)))

# dplot <- cbind(drange, predict(models.1v1.eloPlusMap.logit, newdata = drange, type = "link", se = TRUE))
# dplot <- within(dplot, {
#     PredictedProb <- plogis(fit)
#     LL <- plogis(fit - (1.96 * se.fit))
#     UL <- plogis(fit + (1.96 * se.fit))
#   })
# dplot <- mutate(dplot, probForOrder = ifelse(matchMap =="Arena", PredictedProb, 0))
# dplot$playerCiv <- reorder(dplot$playerCiv, dplot$probForOrder)

# png(filename="images/1v1-1800-civPlusMap.png", width=1200, height=600)

# models.1v1.eloPlusCivPlusMap.plot <- ggplot(dplot, aes(fill=matchMap, x = playerCiv, y = PredictedProb)) +
#   labs(x = "Player civ", y = "Probability of winning match") +
#   geom_bar(width=0.7, position=position_dodge(width=0.7), stat="identity", alpha=0.8) +
#   geom_errorbar(aes(ymin=LL, ymax=UL),
#                   width=.2,
#                   color="#666666",
#                   position=position_dodge(0.7)) +
#   geom_label(label.padding = unit(0.15, "lines"), position=position_dodge(0.7), aes(label=countMatches), size=3.5, label.size=0) +
#   scale_x_discrete(labels = function(x) toupper(substr(x, 0, 4))) +
#   ggtitle(paste("Non-mirror WK 1800+ | 1.5 R6 | ", length(unique(temp$matchId)), " matches"))

# models.1v1.eloPlusCivPlusMap.plot
# dev.off()

# models.1v1.eloPlusCivPlusMap.plot