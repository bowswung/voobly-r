library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(rms)
library(purrr)
library(DescTools)
require(MASS)

rateFromElo <- function(elo1, elo2) {

  #P(A) = 1/(1+10^m) where m is the rating difference (rating(B)-rating(A)) divided by 400
  1/(1+10^((elo2-elo1)/400))
}

singleCivMatchup <- function(data, matchupTarget) {
  # s <- unlist(strsplit(matchupTarget, "-", TRUE))
  # civ1 <- s[1]
  # civ2 <- s[2]
  dataF <- data[data$matchupFixed == matchupTarget, ]

  dataF <- mutate(dataF, expectedRate = 1/(1+10^((-eloGapFixed)/400)))
  expectedRate <- mean(dataF$expectedRate)



  nWins <- length(dataF[dataF$winnerFixed, ]$matchId)
  nTotal <- length(dataF$matchId)


  print(as.character(matchupTarget))
  print(expectedRate)
  print(nWins)
  print(nTotal)
  if (!(nTotal > 0)) {
    list(civP = 1, estimate=0, estimateLwr=0, estimateUpr=0)
  } else {
  res <-binom.test(nWins, nTotal, expectedRate)
  ci <- BinomCI(nWins, nTotal)

  # m <- glm(winner ~ eloGap + playerCiv, data=dataF, family = binomial)
  # print(exp(confint(m)))

  # print(res)
  # print(ci)
  estimateDiff <- ci[1] - expectedRate
  # this is just for the sake more comparable charts
  estimateDiff <- ifelse(estimateDiff > 0.5, 0.5, estimateDiff)
  estimateDiff <- ifelse(estimateDiff < -0.5, -0.5, estimateDiff)


  list(civP =res$p.value, estimate = estimateDiff, estimateLwr = ci[2] - expectedRate, estimateUpr = ci[3] - expectedRate)

}
}