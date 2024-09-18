
## Source:
## https://www.rugbyworldcup.com/2023/stats/

dataPath <- "Data/RWC"

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

## Make counts "per game"
RWCperGame <- do.call(data.frame,
                      lapply(RWC[-c(1:2, ncol(RWC))],
                             function(x, n) {
                                 x/n
                             },
                             RWC$matches))
RWCperGame <- cbind(country=RWC$country, hemisphere=RWC$hemisphere,
                    RWCperGame)
## Order by 'cleanbreaks' (for qualitative.Rmd)
RWCperGame <- RWCperGame[order(RWCperGame$cleanbreaks), ]
## Normalise all variables (0, max()) -> (0, 1)
RWCnorm <- do.call(data.frame,
                   lapply(RWCperGame[-c(1:2, ncol(RWCperGame))],
                          function(x) {
                              x/max(x)
                          }))
RWCnorm <- cbind(country=RWCperGame[[1]], hemisphere=RWCperGame[[2]],
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
