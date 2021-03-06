library(ggplot2)
library(dplyr)
library(tidyr)



temp <- match1v1Clean
temp <- filter(temp, playerCiv != opponentCiv)
temp <- filter(temp, playerElo < 1800 & opponentElo < 1800)
temp <- filter(temp, matchMap == "Arabia")
temp <- filter(temp, ((upReleaseVersion == "R6" | upReleaseVersion == "R7") & wk))

tempWithCutoff <- mutate(temp, cutoff = (ifelse(MatchDate < "2018-09-11 00:00:00", "Old balance", "New balance")))
tempWithCutoff$cutoff <- factor(tempWithCutoff$cutoff, levels=c("Old balance", "New balance"))


models.1v1.upBalance.logit <- glm(winner ~ eloGap + playerCiv + cutoff + cutoff:playerCiv, data=tempWithCutoff, family = "binomial")
summary(models.1v1.upBalance.logit)
confint.default(models.1v1.upBalance.logit)

anova(models.1v1.upBalance.logit, test="Chisq")


drange <-  tidyr::expand(tempWithCutoff, playerCiv, cutoff)
drange$eloGap = 0
drange <- rowwise(drange) %>%
  mutate(countMatches = length(unique(tempWithCutoff[tempWithCutoff$playerCiv == playerCiv & tempWithCutoff$cutoff == cutoff, ]$matchId)))
dplot <- cbind(drange, predict(models.1v1.upBalance.logit, newdata = drange, type = "link", se = TRUE))
dplot <- within(dplot, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
dplot <- mutate(dplot, probForOrder = ifelse(cutoff =="New balance", PredictedProb, 0))
dplot$playerCiv <- reorder(dplot$playerCiv, dplot$probForOrder)

png(filename="images/1v1-1800-upBalance.png", width=1600, height=600)

models.1v1.upBalance.plot <- ggplot(dplot, aes(fill=cutoff, x = playerCiv, y = PredictedProb)) +
  labs(x = "Player civ", y = "Probability of winning match") +
  geom_bar( width=0.7, position=position_dodge(width=0.7), stat="identity", alpha=1) +
  geom_errorbar(aes(ymin=LL, ymax=UL, color=cutoff), , show.legend = FALSE,
                  width=.2,
                  position=position_dodge(0.7)) +
  geom_text(vjust=0.5, position=position_dodge(0.7), angle=90, aes(label=countMatches, y = 0.03 ), show.legend = FALSE, size=3.5) +
  scale_x_discrete(labels = function(x) toupper(substr(x, 0, 4))) +
  ggtitle(paste("Non-mirror WK > 1800 | Arabia | UP 1.5 | ", length(unique(tempWithCutoff$matchId)), " matches")) +

  theme_bw(base_size=14)+
  theme(panel.border = element_blank(),
     axis.line = element_line(size = 0.5, linetype = "solid", colour = "#999999")
    )+
  scale_fill_manual(values=c("#b6d8f4", "#5ba3d8"), name = "Balance") +
  scale_colour_manual(values=c("#78b3e0", "#0974b3"))


models.1v1.upBalance.plot
dev.off()

models.1v1.upBalance.plot



stop("asdfs")