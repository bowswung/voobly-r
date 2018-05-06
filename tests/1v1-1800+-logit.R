
temp <- match1v1Clean
temp <- filter(temp, playerElo > 1800 & opponentElo > 1800)
temp <- filter(temp, playerCiv != opponentCiv)
#temp <- filter(temp, matchMap == "Arena")
temp <- filter(temp, wk == TRUE)
temp$playerCiv <- factor(temp$playerCiv)
temp$playerCiv <- relevel(temp$playerCiv, ref="Vietnamese")
temp$matchMap <- factor(temp$matchMap)
temp$upVersion <- factor(temp$upVersion)


#model
models.1v1.elo.logit <- glm(winner ~ eloGap, data=temp, family = "binomial")
summary(models.1v1.elo.logit)
confint.default(models.1v1.elo.logit)

#accuracy
temp$pred <- predict(models.1v1.elo.logit, newdata=temp, type="response")
temp$predBool <- ifelse(temp$pred > 0.5, TRUE, FALSE)
models.1v1.elo.accuracy <- mean(temp$winner == temp$predBool)



#model
models.1v1.eloPlusCiv.logit <- glm(winner ~ eloGap + playerCiv, data=temp, family = "binomial")
summary(models.1v1.eloPlusCiv.logit)
confint.default(models.1v1.eloPlusCiv.logit)

#acccuracy
temp$pred <- predict(models.1v1.eloPlusCiv.logit, newdata=temp, type="response")
temp$predBool <- ifelse(temp$pred > 0.5, TRUE, FALSE)
models.1v1.eloPlusCiv.accuracy <- mean(temp$winner == temp$predBool)


# test whether civ increases model fit
anova(models.1v1.eloPlusCiv.logit, test="Chisq")



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
library(ggplot2)


drange <-  tidyr::expand(tempNoNewCivs, playerCiv, wk)
drange$eloGap = 0
drange$orderwk = FALSE
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