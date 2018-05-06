library(ggplot2)
library(dplyr)
library(tidyr)



temp <- match1v1Clean
temp <- filter(temp, playerElo > 1800 & opponentElo > 1800)
temp <- filter(temp, playerCiv != opponentCiv)
temp <- filter(temp, wk == TRUE)
temp <- filter(temp, uplatest == TRUE)

temp$playerCiv <- factor(temp$playerCiv)
temp$playerCiv <- relevel(temp$playerCiv, ref="Vietnamese")
temp$opponentCiv <- factor(temp$opponentCiv)
temp$opponentCiv <- relevel(temp$opponentCiv, ref="Vietnamese")
temp$upVersion <- factor(temp$upVersion)


tempWithCutoff <- mutate(temp, cutoff = ifelse(((playerElo + opponentElo) /2 >=2200), "2200+", "1800-2200"))

models.1v1.eloPlusSkillGap.logit <- glm(winner ~ eloGap + playerCiv + cutoff + cutoff:playerCiv, data=tempWithCutoff, family = "binomial")
summary(models.1v1.eloPlusSkillGap.logit)
confint.default(models.1v1.eloPlusSkillGap.logit)

anova(models.1v1.eloPlusSkillGap.logit, test="Chisq")


uniqueMatches =
drange <-  tidyr::expand(tempWithCutoff, playerCiv, cutoff)
drange$eloGap = 0
drange <- rowwise(drange) %>%
  mutate(countMatches = length(unique(tempWithCutoff[tempWithCutoff$playerCiv == playerCiv & tempWithCutoff$cutoff == cutoff, ]$matchId)))
dplot <- cbind(drange, predict(models.1v1.eloPlusSkillGap.logit, newdata = drange, type = "link", se = TRUE))
dplot <- within(dplot, {
    oPlayerCiv <- playerCiv
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
dplot <- mutate(dplot, probForOrder = ifelse(cutoff =="2200+", 0, PredictedProb))
dplot$playerCiv <- reorder(dplot$playerCiv, dplot$probForOrder)

png(filename="images/1v1-1800-civPlusCutoff.png", width=1600, height=600)

models.1v1.eloPlusCivPlusCutoff.plot <- ggplot(dplot, aes(fill=cutoff, x = playerCiv, y = PredictedProb)) +
  labs(x = "Player civ", y = "Probability of winning match") +
  geom_bar(width=0.7, position=position_dodge(width=0.7), stat="identity", alpha=0.8) +
  geom_errorbar(aes(ymin=LL, ymax=UL),
                  width=.2,
                  color="#666666",
                  position=position_dodge(0.7)) +
  geom_label(label.padding = unit(0.15, "lines"), position=position_dodge(0.7), aes(label=countMatches), size=3.5, label.size=0) +
  scale_x_discrete(labels = function(x) toupper(substr(x, 0, 3))) +
  ggtitle(paste("Non-mirror WK | 1.5 R6 | ", length(unique(tempWithCutoff$matchId)), " matches"))

models.1v1.eloPlusCivPlusCutoff.plot
dev.off()

models.1v1.eloPlusCivPlusCutoff.plot
stop("asdfs")


stop("asdfs")
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


tempWithCutoff <- mutate(tempWithCutoff, cutoff = ifelse(((playerElo + opponentElo) /2 >=2200), "2200+", "1800-2200"))
tempWithCutoff <- mutate(tempWithCutoff, uplatest = grepl("v1.5 Beta R6", matchMods))
tempWithCutoff <- filter(tempWithCutoff, uplatest == TRUE)
View(tempWithCutoff)
tempWithCutoff$cutoff <- factor(tempWithCutoff$cutoff)



models.1v1.eloPlusSkillGap.logit <- glm(winner ~ eloGap + playerCiv + playerCiv:cutoff, data=tempWithCutoff, family = "binomial")

anova(models.1v1.eloPlusSkillGap.logit, test="Chisq")

drange <-  tidyr::expand(tempWithCutoff, playerCiv, cutoff)
drange$eloGap = 0
dplot <- cbind(drange, predict(models.1v1.eloPlusSkillGap.logit, newdata = drange, type = "link", se = TRUE))
dplot <- within(dplot, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
dplot <- mutate(dplot, probForOrder = ifelse(cutoff =="2200+", 0, PredictedProb))
dplot$playerCiv <- reorder(dplot$playerCiv, dplot$probForOrder)

png(filename="images/1v1-1800-civPlusCutoff.png", width=1200, height=600)

models.1v1.eloPlusCivPlusCutoff.plot <- ggplot(dplot, aes(fill=cutoff, x = playerCiv, y = PredictedProb)) +
  labs(x = "Player civ", y = "Probability of winning match") +
  geom_bar(width=0.7, position=position_dodge(width=0.7), stat="identity", alpha=0.8) +
  geom_errorbar(aes(ymin=LL, ymax=UL),
                  width=.2,
                  color="#666666",
                  position=position_dodge(0.7)) +
  scale_x_discrete(labels = function(x) toupper(substr(x, 0, 3)))

models.1v1.eloPlusCivPlusCutoff.plot
dev.off()

models.1v1.eloPlusCivPlusCutoff.plot
stop("asdfs")







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
