
temp <- match1v1Clean
#temp <- filter(temp, playerCiv == opponentCiv)

tempWithEloGapSeparate <- filter (temp, winner & (eloGap < 300 & eloGap >-300))
tempWithEloGapSeparate$eloGroup <- cut(tempWithEloGapSeparate$eloGap, seq(-300, 300, 25))

tempWithEloGapSeparate <- mutate(tempWithEloGapSeparate,
  cutoff = ifelse(playerElo >= 2200 & opponentElo >=2200, "2200+", ifelse(playerElo >= 1800 & opponentElo >= 1800, "1800-2200", "< 1800")))

tempFreqs <- count(tempWithEloGapSeparate, eloGroup, cutoff)
tempFreqs <- rowwise(tempFreqs) %>%
  mutate(
    relFreq = round(100 * n / length(tempWithEloGapSeparate[tempWithEloGapSeparate$cutoff == cutoff, ]$matchId), 2)

  )




png(filename="images/eloGapGrouped.png", width=1600, height=600)


models.eloGapGroupedPlot <- ggplot(tempFreqs, aes(fill=cutoff,x = eloGroup, y = relFreq)) +
  labs(x = "Elo gap", y = "Frequency winning match") +
  geom_bar(width=0.7, position=position_dodge(width=0.7), stat="identity", alpha=0.8) +
  geom_label(label.padding = unit(0.15, "lines"), position=position_dodge(0.7), aes(label=n), size=3.5, label.size=0) +

  ggtitle(paste("Elo gap groups | Mirror civ, all balance, all maps | ", length(unique(tempWithEloGapSeparate$matchId)), " matches")) +
  theme_bw(base_size=14)


models.eloGapGroupedPlot
dev.off()
models.eloGapGroupedPlot





stop ("Aasdas")

temp <- match1v1Clean
temp <- filter(temp, playerCiv == opponentCiv)
tempWithCutoff <- mutate(temp,
  cutoff = ifelse(playerElo >= 2200 & opponentElo >=2200, "2200+", ifelse(playerElo >= 1800 & opponentElo >= 1800, "1800-2200", "< 1800")))


models.1v1.elo.logit <- glm(winner ~ eloGap:cutoff, data=tempWithCutoff, family = "binomial")
summary(models.1v1.elo.logit)
confint.default(models.1v1.elo.logit)

# #accuracy
# tempWithCutoff$pred <- predict(models.1v1.elo.logit, newdata=tempWithCutoff, type="response")
# tempWithCutoff$predBool <- ifelse(tempWithCutoff$pred > 0.5, TRUE, FALSE)
# models.1v1.elo.accuracy <- mean(temp$winner == temp$predBool)

png(filename="images/1v1-1800-eloGap.png", width=800, height=600)

drange <-  tidyr::expand(tempWithCutoff, cutoff, eloGap = -500:500)
dplot <- cbind(drange, predict(models.1v1.elo.logit, newdata = drange, type = "link", se = TRUE))
dplot <- within(dplot, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })

models.1v1.elo.plot <- ggplot(dplot, aes(x = eloGap, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL,
    ymax = UL, fill=cutoff), alpha = 0.2) +
  labs(x = "Elo difference", y = "Probability of winning match") +
  geom_line(aes(colour = cutoff), size = 1) +
  theme_bw(base_size=14)+
  ggtitle(paste("Elo gap | Mirror civ, all balance, all maps | ", length(unique(tempWithCutoff$matchId)), " matches"))


models.1v1.elo.plot
dev.off()
models.1v1.elo.plot
stop("asdfsadf")