
################################################################################
## Offender demographics (age/sex/region)

## NOTE that all rows include exactly 1 record

crime <- read.csv("/media/pmur002/ExternalStorage/scratch/NZPolice/CrimeWave/nzpolice-age-sex-boundary.csv")

## Extract incidents from 2021
crime2021 <- subset(crime, grepl("2021$", crime$Year.Month))
## Drop "unknowns"
keep <- crime2021$Police.District != "Not Specified (District)" &
    crime2021$Police.District != "PNHQ Licensing (District)" &
    crime2021$SEX != "Not Applicable" &
    crime2021$SEX != "Not Stated"
crime2021 <- crime2021[keep, ]

## Dates
crime2021$Date <- as.Date(crime2021$Year.Month, format="%m/%d/%Y")

## Bin age groups
crime2021$youth <- crime2021$Age.Group %in% c("0-4", "10-14", "15-19")
crime2021$youthLabel <- c("adult", "youth")[crime2021$youth + 1]

## Bin crime type
crime2021$minor <- crime2021$ANZSOC.Division %in%
    c("Traffic and Vehicle Regulatory Offences",
      "Theft and Related Offences",
      "Public Order Offences",
      "Property Damage and Environmental Pollution",
      "Miscellaneous Offences")
crime2021$minorLabel <- c("major", "minor")[crime2021$minor + 1]

## Bin court action
crime2021$court <- crime2021$Mop.Division == "Court Action"
crime2021$courtLabel <- c("no action", "action")[crime2021$court + 1]

cols <- c("Police.District", "Police.Area",
          "ANZSOC.Division", "ANZSOC.Group", "ANZSOC.Subdivision",
          "Mop.Division", "Mop.Group", "Mop.Subdivision",
          "Age.Group", "SEX", "Date",
          "youth", "youthLabel", "minor", "minorLabel", "court", "courtLabel")
write.csv(crime2021[cols], "nzpolice-offenders-2021.csv", row.names=FALSE)

notrun <- function() {
    temp <- aggregate(crime2021[c("youth", "minor")],
                      list(district=crime2021$Police.Area),
                      mean)
    
    plot(temp$youth, temp$minor)
    
    temp <- aggregate(crime2021[c("youth", "court")],
                      list(district=crime2021$Police.Area),
                      mean)
    plot(temp$youth, temp$court)
}

################################################################################
## Offender demographics (age/sex/ethnicity)

crime <- read.csv("/media/pmur002/ExternalStorage/scratch/NZPolice/CrimeWave/nzpolice-age-sex-ethnicity.csv")

## Extract incidents from 2021
crime2021 <- subset(crime, grepl("2021$", crime$Year.Month))
## Drop "unknowns"
keep <- crime2021$Ethnic.Group != "Not Stated" &
    crime2021$Ethnic.Group != "Organisation" &
    crime2021$Ethnic.Group != "Other Ethnicities" &
    crime2021$SEX != "Not Applicable" &
    crime2021$SEX != "Not Stated"
crime2021 <- crime2021[keep, ]

## Dates
crime2021$Date <- as.Date(paste0("01", crime2021$Year.Month), format="%d%b%Y")

cols <- c("Ethnic.Group",
          "ANZSOC.Division", "ANZSOC.Group", "ANZSOC.Subdivision",
          "Mop.Division", "Mop.Group", "Mop.Subdivision",
          "SEX", "Date")
write.csv(crime2021[cols], "nzpolice-offenders-ethnicity-2021.csv",
          row.names=FALSE)

################################################################################
## Victimisation time and day

victims <- read.csv("/media/pmur002/ExternalStorage/scratch/NZPolice/CrimeWave/nzpolice-victimisation-day-time.csv")

## Missing hour-of-day is coded as 99
table(victims$Occurrence.Hour.Of.Day)
victims <- subset(victims, Occurrence.Hour.Of.Day != 99)

## Missing day-of-week is coded as either '.' or UNKNOWN
table(victims$Occurrence.Day.Of.Week)
victims <- subset(victims, Occurrence.Day.Of.Week != "." &
                           Occurrence.Day.Of.Week != "UNKNOWN")

victims$Date <- as.Date(paste0("01 ", victims$Year.Month),
                        format="%d %b %Y")
victims$month <- as.numeric(factor(months(victims$Date), levels=month.name))
victims$year <- as.numeric(gsub("[^0-9]*", "", victims$Year.Month))
daynames <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
              "Saturday", "Sunday")
victims$day <- as.numeric(factor(victims$Occurrence.Day.Of.Week,
                                 levels=daynames))

cols <- c("year", "month", "day")
write.csv(victims[cols], "nzpolice-victims.csv", row.names=FALSE)

library(ggplot2)

coord_radar <- function (theta = "x", start = 0, direction = 1) {
    theta <- match.arg(theta, c("x", "y"))
    r <- if (theta == "x") "y" else "x"
    ggproto("CoordRadar", CoordPolar, theta = theta, r = r, start = start, 
            direction = sign(direction),
            is_linear = function(coord) TRUE)
}

## Polar plot day of week
temp <- as.data.frame(table(victims$day, victims$month, victims$year))
temp$day <- as.numeric(as.character(temp$Var1))
temp$yearmon <- paste(temp$Var2, temp$Var3)

ggplot(subset(temp, Freq > 0)) + # , Var3 != 2019 & Var3 != 2023)) +
    geom_path(aes(day, Freq), na.rm=TRUE) + # , colour=Var2)) +
    scale_x_continuous(limits=c(1, 8), breaks=1:7, labels=daynames) +
    scale_y_continuous(limits=c(0, NA)) +
    coord_radar()

## Facetted by year
ggplot(subset(temp, Freq > 0)) + # , Var3 != 2019 & Var3 != 2023)) +
    geom_path(aes(day, Freq), na.rm=TRUE) + # , colour=Var2)) +
    scale_x_continuous(limits=c(1, 8), breaks=1:7, labels=daynames) +
    scale_y_continuous(limits=c(0, NA)) +
    facet_wrap("Var3") +
    coord_radar()

## Polar plot scaled to match clock!
temp <- as.data.frame(table(victims$Occurrence.Hour.Of.Day, victims$year))
## 12-hour not 24-hour
temp$hour <- as.numeric(as.character(temp$Var1)) %% 12
temp$night <- as.numeric(as.character(temp$Var1)) >= 12
temp$year <- as.numeric(as.character(temp$Var2))
temp$clock <- paste(temp$night, temp$year)

ggplot(subset(temp, year != 2019)) +
    geom_path(aes(hour, Freq,
                  colour=as.factor(year), group=year)) +
    scale_y_continuous(limits=c(0, NA)) +
    scale_x_continuous(limits=c(0, 12)) +
    coord_radar()




