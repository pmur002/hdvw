
ratesTable <-
    read.csv("Data/YouthCrime/crime-rates.csv", check.names=FALSE)
library(reshape2)
rates <- melt(ratesTable, id="Age", variable.name="Year", value.name="Rate")
rates <- rates[-grep("Total", rates$Age), ]

crimeLevel <- read.csv("Data/YouthCrime/crime-level.csv")
crimeLevel$level <- 
    factor(crimeLevel$level, 
           levels=c("Low", "Low-Medium", "Medium", "Medium-High", "High"),
           ordered=TRUE)
crimeLevel$levelNumeric <- as.numeric(crimeLevel$level)
crimeLevel$yearDate <- as.Date(paste0(crimeLevel$year, "-06-30"))
crimeLevelTotal <- group_by(crimeLevel, level) |>
    summarise(total = sum(count)) |>
    mutate(prop = total/sum(total))
crimeLevel2021 <- subset(crimeLevel, year == 2021)
crimeYear <- group_by(crimeLevel, yearDate) |>
    summarise(total = sum(count), year = unique(year))

crimeAge <- read.csv("Data/YouthCrime/crime-age.csv")
crimeAge <- subset(crimeAge,
                   age %in% c("14", "15", "16"))
crimeAge$age <- as.numeric(crimeAge$age)
crimeAge$ageFactor <- factor(crimeAge$age)
crimeAge$yearDate <- as.Date(paste0(crimeAge$year, "-06-30"))
crimeAgeTotal <- select(crimeAge, -rate) |>
    group_by(age) |>
    summarise(total = sum(count),
              avgPop = mean(pop))

crimeGender <- read.csv("Data/YouthCrime/crime-gender.csv")
crimeGender <- subset(crimeGender,
                      gender != "Other/Unknown")
crimeGender$genderFactor <- factor(crimeGender$gender)
crimeGender$yearDate <- as.Date(paste0(crimeGender$year, "-06-30"))
crimeGenderTotal <- group_by(crimeGender, gender) |>
    summarise(total = sum(count)) |>
    mutate(prop = total/sum(total))
crimeGender2021 <- subset(crimeGender, year == 2021)

crimeType <- read.csv("Data/YouthCrime/crime-type.csv")
crimeType$typeFactor <- factor(crimeType$type)
crimeType$yearDate <- as.Date(paste0(crimeType$year, "-06-30"))
crimeTypeTotal <- group_by(crimeType, type) |>
    summarise(total = sum(count)) |>
    mutate(prop = total/sum(total))
crimeType2021 <- subset(crimeType, year == 2021)

crimeDivision <- read.csv("Data/YouthCrime/crime-type.csv")
other <- !(crimeDivision$type %in%
           c("Theft", "Causing injury", "Unlawful entry, burglary",
             "Dangerous acts", "Public disorder", "Property damage"))
crimeDivision$type[other] <- "Other"
crimeDivision <- aggregate(crimeDivision["count"],
                           list(year=crimeDivision$year,
                                type=crimeDivision$type),
                           sum)
crimeDivision <- subset(crimeDivision, year == 2021)
crimeDivision$prop <- crimeDivision$count/sum(crimeDivision$count)
crimeDivision$typeFactor <- factor(crimeDivision$type)


crimeDistrict <- read.csv("Data/YouthCrime/crime-district.csv") |>
    ## For joining with map data
    mutate(district = gsub("Bay Of Plenty", "Bay of Plenty",
                           gsub("Counties Manukau", "Counties/Manukau",
                                district)))
crimeDistrict <- subset(crimeDistrict,
                        district != "Outside New Zealand (District)")
crimeDistrict$yearDate <- as.Date(paste0(crimeDistrict$year, "-06-30"))
crimeDistrictTotal <- select(crimeDistrict, -rate) |>
    group_by(district) |>
    summarise(total = sum(count),
              avgPop = mean(pop),
              rate = total/avgPop)
crimeDistrict2021 <- subset(crimeDistrict, year == 2021)

crimeGroup <- read.csv("Data/YouthCrime/crime-group.csv")
crimeGroup <- subset(crimeGroup,
                     group %in%
                     c("Māori","Pasifika", "European/Other", "Unknown"))
crimeGroup$yearDate <- as.Date(paste0(crimeGroup$year, "-06-30"))
crimeGroupTotal <- group_by(crimeGroup, group) |>
    summarise(total = sum(count))
crimeGroupTotal$groupFactor <- as.factor(crimeGroupTotal$group)
crimeGroupTotal$groupNumeric <- as.numeric(crimeGroupTotal$groupFactor)
crimeGroup2021 <- subset(crimeGroup, year == 2021)

crimeEthnicity <- read.csv("Data/YouthCrime/crime-ethnicity.csv")
crimeEthnicity$yearDate <- as.Date(paste0(crimeEthnicity$year, "-06-30"))
crimeEthnicityTotal <- group_by(crimeEthnicity, ethnicity) |>
    summarise(total = sum(count))
crimeEthnicityTotal$ethnicityFactor <- as.factor(crimeEthnicityTotal$ethnicity)
crimeEthnicityTotal$ethnicityNumeric <- as.numeric(crimeEthnicityTotal$ethnicityFactor)

districts <- 
    st_read("Data/YouthCrime/SHP/nz-police-district-boundaries-29-april-2021.shp",
            quiet=TRUE)
## Drop unnecessary Z dimension from some geometries
districts <- st_zm(districts)
centroids <- st_coordinates(st_centroid(st_geometry(districts)))
districts$X <- centroids[,1]
districts$Y <- centroids[,2]
districts <- inner_join(districts, subset(crimeDistrict, year == 2021),
                        by=join_by(D_MACRON == district))

offenders <- read.csv("Data/YouthCrime/nzpolice-offenders-2021.csv")
offenders$Date <- as.Date(offenders$Date)
offenders$Month <- months(offenders$Date)
offenders$MonthNum <- as.numeric(format(offenders$Date, "%m"))
offenders$Age.Group <- factor(offenders$Age.Group,
                              levels=c("0-4", "5-9", "10-14", "15-19",
                                       "20-24", "25-29", "30-34", "35-39",
                                       "40-44", "45-49", "50-54", "55-59", 
                                       "60-64", "65-69", "70-74", "75-79",
                                       "80yearsorover", "NotSpecified"))
offenders$Area <- gsub(" Area", "", offenders$Police.Area)
offenders$Police.District <- gsub("Of", "of", offenders$Police.District)
offendersByArea <- aggregate(offenders[c("youth", "minor", "court")],
                             list(area=offenders$Police.Area),
                             mean)
offendersByArea <- merge(unique(offenders[c("Police.District", "Police.Area")]),
                         offendersByArea, by.x="Police.Area", by.y="area")
offendersByArea$Area <- gsub(" Area", "", offendersByArea$Police.Area)
offendersByDistrict <- aggregate(offenders[c("youth", "minor", "court")],
                                 list(district=offenders$Police.District),
                                 mean)

offenderEthnicity <-
    read.csv("Data/YouthCrime/nzpolice-offenders-ethnicity-2021.csv")

victimisations <- read.csv("Data/YouthCrime/nzpolice-victims.csv")


