library(ggplot2)
library(dplyr)
library(tidyr)



temp <- match1v1Clean
temp <- filter(temp, playerElo > 1800 & opponentElo > 1800)
temp <- filter(temp, playerCiv != opponentCiv)
temp <- filter(temp, matchMap == "Arabia")
temp <- filter(temp, wk==TRUE)


civToExamine = "Burmese"
tempCiv <- filter(temp, opponentCiv == civToExamine)
temp <- filter(temp, opponentCiv != civToExamine & playerCiv != civToExamine)

temp$playerCiv <- factor(temp$playerCiv)
tempCiv$playerCiv <- factor(tempCiv$playerCiv)



models.1v1.overall.logit <- glm(winner ~ eloGap + playerCiv , data=temp, family = "binomial")
models.1v1.civCounter.logit <- glm(winner ~ eloGap + playerCiv , data=tempCiv, family = "binomial")

drange <-  tidyr::expand(temp, playerCiv)
drange <- filter(drange, playerCiv != civToExamine)
drange$playerCiv <- droplevels(drange$playerCiv)
drange$eloGap = 0
drange$matchup <- factor(c("Other", civToExamine), levels=c("Other", civToExamine))


drangeOverall <- rowwise(drange) %>%
  mutate(matchup="Other", countMatches = length(unique(temp[as.character(temp$playerCiv) == as.character(playerCiv), ]$matchId)))
dplotOverall <- cbind(drangeOverall, predict(models.1v1.overall.logit, newdata = drangeOverall, type = "link", se = TRUE))


drangeCiv <- rowwise(drange) %>%
  mutate(matchup=civToExamine, countMatches = length(unique(tempCiv[tempCiv$playerCiv == playerCiv, ]$matchId)))
dplotCiv <- cbind(drangeCiv, predict(models.1v1.civCounter.logit, newdata = drangeCiv, type = "link", se = TRUE))

dplot <- rbind(dplotCiv, dplotOverall)

dplot <- within(dplot, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
dplot <- mutate(dplot, probForOrder = ifelse(matchup == "Other", 0, PredictedProb))
dplot$playerCiv <- reorder(dplot$playerCiv, dplot$probForOrder)
str(factor(dplot$matchup))

dplot$matchup <- factor(dplot$matchup, levels=c("Other", civToExamine))



png(filename=paste("images/civMatchup",civToExamine,".png",sep=""), width=1200, height=600)

models.1v1.civMatchup.plot <- ggplot(dplot, aes(fill=matchup, x = playerCiv, y = PredictedProb)) +
  labs(x = "Player civ", y = paste("Probability of winning match vs ", civToExamine, sep="")) +
  geom_bar(width=0.7, position=position_dodge(width=0.7), stat="identity",   alpha=1) +
  geom_errorbar(aes(ymin=LL, ymax=UL, color=matchup), show.legend = FALSE,
                  width=.2,
                  position=position_dodge(0.7)) +
  geom_text(vjust=0.5, position=position_dodge(0.7), angle=90, aes(label=countMatches, y = 0.03 ), show.legend = FALSE, size=3.5) +
  scale_x_discrete(labels = function(x) toupper(substr(x, 0, 4))) +
  ggtitle(paste(civToExamine, " counter civs | 1800+ Arabia WK | ",  length(unique(tempCiv$matchId)), " ", civToExamine, " matches | " , length(unique(temp$matchId)), " other matches"))+
  theme_bw(base_size=14)+
  theme(panel.border = element_blank(),
     axis.line = element_line(size = 0.5, linetype = "solid", colour = "#999999")
    )+
  scale_fill_manual(values=c("#b6d8f4", "#5ba3d8"), name = "Civ") +
  scale_colour_manual(values=c("#78b3e0", "#0974b3"))



models.1v1.civMatchup.plot
dev.off()

models.1v1.civMatchup.plot