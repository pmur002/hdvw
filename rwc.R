
dataPath <- "Data/RWC"

## https://www.kaggle.com/datasets/lylebegbie/international-rugby-union-results-from-18712022/data
## CC BY-NC-SA 4.0

hemi <- read.csv(file.path(dataPath, "rwc-2023-hemisphere.csv"))
rwcAll <- merge(read.csv(file.path(dataPath, "rwc-all-time.csv")),
                hemi, by.x="team", by.y="country")
rwcAll$hemisphere <- factor(rwcAll$hemisphere, levels=c("South", "North"))

## Source:
## https://www.rugbyworldcup.com/2023/stats/

tables <- c("hemisphere", "yellowcards", "redcards",
            "cleanbreaks", "tackles",
            "points", "conversions", "offloads", "tries",
            "runs", "matches")

statsList <- lapply(tables,
                    function(x) {
                        df <- read.csv(file.path(dataPath,
                                                 paste0("rwc-2023-", x,
                                                        ".csv")))
                        names(df) <- c("country", x)
                        df
                    })

RWC <- Reduce(function(x, y) {
                  merge(x, y, by="country", all=TRUE)
              },
              statsList)
RWC[is.na(RWC)] <- 0
RWC$hemisphere <- factor(RWC$hemisphere, levels=c("South", "North"))
names(RWC) <- gsub("yellowcards", "ycards",
                   gsub("redcards", "rcards",
                        gsub("cleanbreaks", "breaks",
                             gsub("hemisphere", "sphere",
                                  gsub("conversions", "converts",
                                       names(RWC))))))

## Make counts "per game"
RWCperGame <- do.call(data.frame,
                      lapply(RWC[-c(1:2, ncol(RWC))],
                             function(x, n) {
                                 x/n
                             },
                             RWC$matches))
RWCperGame <- cbind(country=RWC$country, sphere=RWC$sphere,
                    RWCperGame)
## Order by 'cleanbreaks' (for qualitative.Rmd)
RWCperGame <- RWCperGame[order(RWCperGame$breaks), ]
## Normalise all variables (0, max()) -> (0, 1)
RWCnorm <- do.call(data.frame,
                   lapply(RWCperGame[-c(1:2, ncol(RWCperGame))],
                          function(x) {
                              x/max(x)
                          }))
RWCnorm <- cbind(country=RWCperGame[[1]], sphere=RWCperGame[[2]],
                 RWCnorm)

## Work with just top 8 nations
topNations <- c("South Africa", "New Zealand", "England", "Argentina",
                "Ireland", "France", "Wales", "Fiji")

RWCtop <- RWC[RWC$country %in% topNations, ]
RWCtop$country <- factor(RWCtop$country, levels=topNations)

RWCtopPerGame <- RWCperGame[RWCperGame$country %in% topNations, ]
RWCtopPerGame$country <- factor(RWCtopPerGame$country, levels=topNations)

RWCtopNorm <- RWCnorm[RWCnorm$country %in% topNations, ]
RWCtopNorm$country <- factor(RWCtopNorm$country, levels=topNations)

## Wikipedia page
wikiRWC <- read.csv(file.path(dataPath, "WordCloud", "wiki-rwc.csv"))
