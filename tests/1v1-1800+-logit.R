temp <- match1v1Clean

temp <- filter(temp, playerElo > 1800 & opponentElo > 1800)


logit <- glm(winner ~ eloGap + playerCiv, data=temp, family = "binomial")
summary(logit)



temp$pred <- predict(logit, newdata=temp, type="response")
temp$predBool <- ifelse(temp$pred > 0.5, TRUE, FALSE)
mean(temp$winner == temp$predBool)


# eloPredictions1800 <- data.frame(eloGap = -300:300)
# eloPredictions1800$winP <- predict(logit, newdata=eloPredictions1800, type="response")
# eloPredictions1800


# eloWithCivProbabilityPredictions1900 <- data.frame(eloGap = 0, playerCiv = levels(temp$playerCiv))
# rm(temp)
# eloWithCivProbabilityPredictions1900$winP <- predict(logit, newdata=eloWithCivProbabilityPredictions1900, type="response")
# eloWithCivProbabilityPredictions1900


# temp$playerCiv <- factor(temp$playerCiv)
# temp$opponentCiv <- factor(temp$opponentCiv)

# factorMap <- data.frame(factor = levels (temp$playerCiv), value = 1:(nlevels (temp$playerCiv)))

# temp <-transmute(temp,


#     newPlayerCiv = ifelse ((factorMap$value[factor = playerCiv] > factorMap$value[factor = opponentCiv]), as.character(playerCiv), as.character(opponentCiv)),
#     newOpponentCiv = ifelse ((factorMap$value[factor = playerCiv] > factorMap$value[factor = opponentCiv]), as.character(opponentCiv), as.character(playerCiv)),
#     newWinner = ifelse ((factorMap$value[factor = playerCiv] > factorMap$value[factor = opponentCiv]), winner, !winner),
#     newEloGap = ifelse ((factorMap$value[factor = playerCiv] > factorMap$value[factor = opponentCiv]), eloGap, 0 - eloGap),
#   )

# temp <- mutate(temp,
#     civInteraction = paste(as.character(newPlayerCiv), "vs", as.character(newOpponentCiv))
#   )
# temp$civInteraction <- factor(temp$civInteraction)
# levels(temp$civInteraction)
# #temp$opponentCiv

# View(temp)


