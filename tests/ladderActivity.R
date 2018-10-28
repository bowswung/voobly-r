library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)





temp <- matchDump

temp <- distinct(temp, MatchId, .keep_all = TRUE)
temp$matchDay <- floor_date(as.Date(temp$MatchDate), "week")
temp <- filter (temp, matchDay < "2018-10-20")
temp <- filter (temp, matchDay > "2017-08-14")

temp <- temp %>%
    group_by(MatchLadder, matchDay) %>%

    summarise(mPerDay = n())


#temp <- filter (temp, MatchLadder == "RM - 1v1")
png(filename="temp/images/ladderActivity.png", width=1280, height=720)

p <- ggplot(temp, aes(x=matchDay, y=mPerDay, fill=MatchLadder)) +
  geom_area() +
  ggtitle("Matches played on Voobly ladders") +
  labs(x = "Week", y = "No of matches") +

  theme_bw(base_size=14)+
  theme(panel.border = element_blank(),
     axis.line = element_line(size = 0.5, linetype = "solid", colour = "#999999")
    ) +
  scale_fill_manual(values=c("#4d9f79","#de6780",  "#5ba3d8", "#de7b45"), name = "Ladder")

p
dev.off()
p


temp <- matchDump
temp <- filter (temp, MatchLadder == "RM - 1v1" | MatchLadder == "RM - Team")

temp <- distinct(temp, MatchId, .keep_all = TRUE)
temp$matchDay <- floor_date(as.Date(temp$MatchDate), "week")
temp <- filter (temp, matchDay < "2018-10-20")
temp <- filter (temp, matchDay > "2017-08-14")


temp <- mutate(temp, eloGroup = paste(MatchLadder, ifelse(MatchPlayerPreRating >= 2200, "2200+", ifelse(MatchPlayerPreRating >= 1800, "1800-2200", "< 1800"))))

temp <- temp %>%
    group_by(matchDay, eloGroup) %>%

    summarise(mPerDay = n())


#temp <- filter (temp, MatchLadder == "RM - 1v1")
png(filename="temp/images/ladderActivityByElo.png", width=1280, height=720)

p <- ggplot(temp, aes(x=matchDay, y=mPerDay, fill=eloGroup)) +
  geom_area() +
  ggtitle("Matches played on Voobly ladders") +
  labs(x = "Week", y = "No of matches") +

  theme_bw(base_size=14)+
  theme(panel.border = element_blank(),
     axis.line = element_line(size = 0.5, linetype = "solid", colour = "#999999")
    ) +
  scale_fill_manual(values=c("#97c5e9", "#5ba3d8", "#0974b3", "#eead8b", "#de7b45", "#c25021"), name = "Ladder")

p
dev.off()
p

