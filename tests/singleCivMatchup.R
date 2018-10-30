library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(rms)
library(purrr)
library(DescTools)
require(MASS)


singleCivMatchup <- function(data, matchupTarget) {
  # s <- unlist(strsplit(matchupTarget, "-", TRUE))
  # civ1 <- s[1]
  # civ2 <- s[2]
  dataF <<- data
  dataF <- dataF[dataF$matchupFixed == matchupTarget, ]
  nWins <- length(dataF[dataF$winnerFixed, ]$matchId)
  nTotal <- length(dataF$matchId)
  res <- binom.test(nWins, nTotal, 0.5)
  ci <- BinomCI(nWins, nTotal)

  # m <- glm(winner ~ eloGap + playerCiv, data=dataF, family = binomial)
  # print(exp(confint(m)))

  # print(res)
  # print(ci)

  list(civP =res$p.value, estimate = ci[1], estimateLwr = ci[2], estimateUpr = ci[3])

}