# import all data
library(readr)
library(dplyr)
matchDump <- read_csv("data/matchDump.csv",
    col_types = cols(MatchDate = col_datetime(format = "%Y-%m-%dT%H:%M:%S"),
        MatchPlayerCivId = col_skip(), MatchPlayerId = col_integer(),
        MatchPlayerWinner = col_logical(),
        MatchUrl = col_skip()))

match1v1 <- filter (matchDump, MatchLadder == "RM - 1v1")
matchTeam <- filter (matchDump, MatchLadder == "RM - Team")