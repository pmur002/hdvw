
## https://www.kaggle.com/datasets/lylebegbie/international-rugby-union-results-from-18712022/data

## CC BY-NC-SA 4.0

## Only includes "tier one" sides
## 
##  [1] "New Zealand"  "Australia"    "France"       "Ireland"      "Argentina"
##  [6] "England"      "Scotland"     "Wales"        "South Africa" "Italy"

results <- read.csv("results.csv")
results$year <- as.numeric(substring(results$date, 1, 4))
results$date <- as.Date(results$date)

resultsWC <- subset(results, world_cup == "True")

teamsWC <- c(resultsWC$home_team, resultsWC$away_team)
pointsWC <- c(resultsWC$home_score, resultsWC$away_score)
againstWC <- c(resultsWC$away_score, resultsWC$home_score)
yearWC <- c(resultsWC$year, resultsWC$year)
matchWC <- rep(1:nrow(resultsWC), 2)

write.csv(data.frame(year=yearWC, team=teamsWC,
                     scored=pointsWC, conceded=againstWC,
                     match=matchWC),
          row.names=FALSE,
          "rwc-all-time.csv")
