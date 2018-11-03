library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(rms)
library(purrr)
library(DescTools)
library(corrplot)


temp <- filter (matchDump, MatchLadder == "DM - 1v1" )
#temp <- matchTeam

temp <- mutate(temp,
    upVersion = ifelse(grepl("1.5 Beta", MatchMods, fixed=TRUE), "1.5" , "1.4"),
    wk = ifelse(grepl("WololoKingdoms", MatchMods, fixed=TRUE), TRUE , FALSE)
  )

#temp <- filter(temp, MatchMap == "Arabia" & wk)

temp <- group_by(temp, MatchId) %>%
   mutate(
    numPlayers = n(),
    type = ifelse(numPlayers == 4, "2v2", ifelse(numPlayers == 6, "3v3", ifelse(numPlayers == 8, "4v4", ifelse(numPlayers == 2, "1v1", "Unknown"))))
    ) %>% ungroup()

temp <- group_by(temp, MatchId, MatchPlayerWinner) %>%
   mutate(
      teamElo = mean(MatchPlayerPreRating)
    ) %>% ungroup()


temp <- group_by(temp, MatchId) %>%
   arrange(MatchPlayerWinner, .by_group=TRUE) %>%
   mutate(
      teamEloGap = ifelse(MatchPlayerWinner, last(teamElo) - first(teamElo), first(teamElo) - last(teamElo))
    ) %>% ungroup()


temp$type <- factor(temp$type)
tempClean <- filter(temp, type != "Unknown")

tempClean <- filter(tempClean, MatchPlayerCivName != "VooblyCivError")

# stop("ASd")

models.team.eloPlusSkillGap.logit <- glm(MatchPlayerWinner ~ teamEloGap + MatchPlayerCivName, data=tempClean, family = "binomial")
summary(models.team.eloPlusSkillGap.logit)
confint.default(models.team.eloPlusSkillGap.logit)

anova(models.team.eloPlusSkillGap.logit, test="Chisq")
PseudoR2(models.team.eloPlusSkillGap.logit, which="Tjur")
tempClean$type <- droplevels(tempClean$type)

drange <-  tidyr::expand(tempClean, MatchPlayerCivName, type)
drange <- rowwise(drange) %>%
  mutate(countMatches = length(unique(tempClean[tempClean$MatchPlayerCivName == MatchPlayerCivName & tempClean$type == type, ]$MatchId)))
drange$teamEloGap = 0
dplot <- cbind(drange, predict(models.team.eloPlusSkillGap.logit, newdata = drange, type = "link", se = TRUE))
dplot <- within(dplot, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
dplot <- mutate(dplot, probForOrder = ifelse(type =="2v2", PredictedProb, 0))
dplot$MatchPlayerCivName <- reorder(dplot$MatchPlayerCivName, dplot$probForOrder)

sapFioriColors = c("#5ba3d8", "#de7b45", "#4d9f79", "#de6780", "#877cce", "#40a3b3", "#3582f1", "#b15f9f", "#708993", "#e2776e", "#2e6c96")


png(filename="temp/images/team-all-civPlusType.png", width=1600, height=600)

models.team.eloPlusSkillGap.plot <- ggplot(dplot, aes(fill=type, x = MatchPlayerCivName, y = PredictedProb)) +
  labs(x = "Player civ", y = "Probability of winning match") +
  geom_bar( width=0.7, position=position_dodge(width=0.7), stat="identity", alpha=1) +
  geom_errorbar(aes(ymin=LL, ymax=UL, color=type), show.legend = FALSE,
                  width=.2,
                  position=position_dodge(0.7)) +
  geom_text(vjust=0.5, position=position_dodge(0.7), angle=90, aes(label=countMatches, y = 0.03 ), show.legend = FALSE, size=3.5) +
  scale_x_discrete(labels = function(x) toupper(substr(x, 0, 4))) +
  ggtitle(paste("Non-mirror WK | Arabia | 1.5 R7 | ", length(unique(tempClean$MatchId)), " matches")) +
  theme_bw(base_size=14)+
  theme(panel.border = element_blank(),
     axis.line = element_line(size = 0.5, linetype = "solid", colour = "#999999")
    )+
  scale_fill_manual(values=sapFioriColors, name = "ELO cutoff") +
  scale_colour_manual(values=sapFioriColors)

models.team.eloPlusSkillGap.plot
dev.off()
