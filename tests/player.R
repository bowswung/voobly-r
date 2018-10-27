library(ggplot2)
library(dplyr)
library(tidyr)

#viper
playerId <- "123211439"
#daut
playerId <- "12926"

temp <- match1v1Clean
temp <- filter(temp, matchPlayerId == playerId | opponentPlayerId == playerId)
temp <- filter(temp, playerCiv != opponentCiv)
temp <- filter(temp, matchMap == "Arabia")
temp <- filter(temp, wk==TRUE)


temp$playerCiv <- factor(temp$playerCiv)


models.1v1.player.logit <- glm(winner ~ eloGap + playerCiv , data=temp, family = "binomial")
anova(models.1v1.player.logit, test="Chisq")

drange <-  tidyr::expand(temp, playerCiv)
drange$eloGap = 0


drange <- rowwise(drange) %>%
  mutate(countMatches = length(unique(temp[as.character(temp$playerCiv) == as.character(playerCiv), ]$matchId)))
dplot <- cbind(drange, predict(models.1v1.player.logit, newdata = drange, type = "link", se = TRUE))

dplot <- within(dplot, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
dplot$playerCiv <- reorder(dplot$playerCiv, dplot$PredictedProb)
png(filename="images/1v1-1800-civPlusPlayer.png", width=1200, height=600)

models.1v1.player.plot <- ggplot(dplot, aes(x = playerCiv, y = PredictedProb)) +
  labs(x = "Player civ", y = "Probability of winning match") +
  geom_bar(width=0.7, position=position_dodge(width=0.7), stat="identity",   alpha=0.8, fill="#da373b") +
  geom_errorbar(aes(ymin=LL, ymax=UL),
                  width=.2,
                  color="#666666",
                  position=position_dodge(0.7)) +
  geom_label(label.padding = unit(0.15, "lines"), position=position_dodge(0.7), aes(label=countMatches), size=3.5, label.size=0) +
  scale_x_discrete(labels = function(x) toupper(substr(x, 0, 4))) +
  ggtitle(paste("Viper on Arabia civ win chance based on ",  length(unique(temp$matchId)), " matches"))

models.1v1.player.plot
dev.off()

models.1v1.player.plot
