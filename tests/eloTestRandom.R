library(ggplot2)
library(dplyr)
library(tidyr)



set.seed(100)


temp <- match1v1Clean
temp <- filter(temp, playerCiv == opponentCiv)
temp <- filter(temp, (playerElo + opponentElo) / 2  < 1900)
temp <- select(temp, winner, eloGap)
temp$withRandom = FALSE
temp$winner <- sample(c(FALSE, TRUE), size=nrow(temp), replace = TRUE)

# tempCopy <- temp
# tempWithRandom <- sample_n(temp, 100000)
# tempWithRandom <- rbind(tempWithRandom, tempCopy)
# tempWithRandom$withRandom = TRUE
# tempWithRandom$winner <- sample(c(FALSE, TRUE), size=nrow(tempWithRandom), replace = TRUE)
# temp$winner <- sample(c(FALSE, TRUE), size=nrow(temp), replace = TRUE)


tempFinal <- rbind(temp)

models.1v1.elo.logit <- glm(winner ~ eloGap:withRandom, data=tempFinal, family = "binomial")
summary(models.1v1.elo.logit)
confint.default(models.1v1.elo.logit)

# #accuracy
# tempWithCutoff$pred <- predict(models.1v1.elo.logit, newdata=tempWithCutoff, type="response")
# tempWithCutoff$predBool <- ifelse(tempWithCutoff$pred > 0.5, TRUE, FALSE)
# models.1v1.elo.accuracy <- mean(temp$winner == temp$predBool)

png(filename="images/1v1-1800-eloGap.png", width=800, height=600)

drange <-  tidyr::expand(tempFinal, withRandom, eloGap = -500:500)
dplot <- cbind(drange, predict(models.1v1.elo.logit, newdata = drange, type = "link", se = TRUE))
dplot <- within(dplot, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })

models.1v1.elo.plot <- ggplot(dplot, aes(x = eloGap, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL,
    ymax = UL, fill=withRandom), alpha = 0.2) +
  labs(x = "Elo difference", y = "Probability of winning match") +
  geom_line(aes(colour = withRandom), size = 1) +
  theme_bw(base_size=14)+
  ggtitle(paste("Elo gap | Mirror civ, all balance, all maps | ", length(unique(tempFinal$matchId)), " matches"))


models.1v1.elo.plot
dev.off()
models.1v1.elo.plot
stop("asdfsadf")