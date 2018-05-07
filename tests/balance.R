library(ggplot2)
library(dplyr)
library(tidyr)

temp <- match1v1Clean
temp <- filter(temp, playerCiv != opponentCiv)
temp <- filter(temp, playerElo > 1800 & opponentElo > 1800)
temp <- filter(temp, matchMap == "Arabia")

tempNoNewCivs <- filter(temp, !(as.character(playerCiv) %in% c('Khmer', 'Italians', 'Magyars', 'Indians', 'Malay', 'Slavs', 'Vietnamese', 'Incas', 'Burmese', 'Ethiopian', 'Portuguese', 'Berbers', 'Malian')))
tempNoNewCivs <- filter(tempNoNewCivs, !(as.character(opponentCiv) %in% c('Khmer', 'Italians', 'Magyars', 'Indians', 'Malay', 'Slavs', 'Vietnamese', 'Incas', 'Burmese', 'Ethiopian', 'Portuguese', 'Berbers', 'Malian')))

tempNoNewCivs$playerCiv <- factor(tempNoNewCivs$playerCiv)

tempNoNewCivs <- mutate(tempNoNewCivs,
    version = ifelse(wk, ifelse(uplatest, "WK - 1.5R6", "WK - Other"), "Non-WK")
  )
tempNoNewCivs$version <- factor(tempNoNewCivs$version, levels= c("Non-WK", "WK - Other", "WK - 1.5R6"))

models.1v1.balanceCompare.logit <- glm(winner ~ eloGap + playerCiv + version + playerCiv:version, data=tempNoNewCivs, family = "binomial")
summary(models.1v1.balanceCompare.logit)
confint.default(models.1v1.balanceCompare.logit)

anova(models.1v1.balanceCompare.logit, test="Chisq")


drange <-  tidyr::expand(tempNoNewCivs, playerCiv, version)
drange$eloGap = 0
drange <- rowwise(drange) %>%
  mutate(countMatches = length(unique(tempNoNewCivs[tempNoNewCivs$playerCiv == playerCiv & tempNoNewCivs$version == version, ]$matchId)))
dplot <- cbind(drange, predict(models.1v1.balanceCompare.logit, newdata = drange, type = "link", se = TRUE))
dplot <- within(dplot, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
dplot <- mutate(dplot, probForOrder = ifelse(version =="Non-WK", PredictedProb, 0))
dplot$playerCiv <- reorder(dplot$playerCiv, dplot$probForOrder)

png(filename="images/1v1-1800-civPlusVersion.png", width=1600, height=600)

models.1v1.eloPlusVersion.plot <- ggplot(dplot, aes(fill=version, x = playerCiv, y = PredictedProb)) +
  labs(x = "Player civ", y = "Probability of winning match") +
  geom_bar(width=0.7, position=position_dodge(width=0.7), stat="identity", alpha=0.8) +
  geom_errorbar(aes(ymin=LL, ymax=UL),
                  width=.2,
                  color="#666666",
                  position=position_dodge(0.7)) +
  geom_label(label.padding = unit(0.15, "lines"), position=position_dodge(0.7), aes(label=countMatches), size=3.5, label.size=0) +
  scale_x_discrete(labels = function(x) toupper(substr(x, 0, 4))) +
  ggtitle(paste("Non-mirror 1800+ Arabia | Balance comparison (WK civs excluded as opponents) | ", length(unique(tempNoNewCivs$matchId)), " matches")) +
  theme_bw(base_size=14)

models.1v1.eloPlusVersion.plot
dev.off()

models.1v1.eloPlusVersion.plot
