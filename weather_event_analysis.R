# Set up date processing
library("lubridate")
setClass('myDate')
setAs("numeric", "myDate", function(from) as.numeric(year(mdy_hms(from))))

colnames <- 
    c("STATE__",   "BGN_DATE",   "BGN_TIME",   "TIME_ZONE",  "COUNTY",     "COUNTYNAME",
      "STATE",     "EVTYPE",     "BGN_RANGE",  "BGN_AZI",    "BGN_LOCATI", "END_DATE",  
      "END_TIME",  "COUNTY_END", "COUNTYENDN", "END_RANGE",  "END_AZI",    "END_LOCATI",
      "LENGTH",    "WIDTH",      "F",          "MAG",        "FATALITIES", "INJURIES",  
      "PROPDMG",   "PROPDMGEXP", "CROPDMG",    "CROPDMGEXP", "WFO",        "STATEOFFIC",
      "ZONENAMES", "LATITUDE",   "LONGITUDE",  "LATITUDE_E", "LONGITUDE_", "REMARKS",
      "REFNUM")

readcols <-
    c("NULL",      "myDate",     "NULL",       "NULL",       "NULL",       "NULL",
      "NULL",      "character",  "NULL",       "NULL",       "NULL",       "NULL",  
      "NULL",      "NULL",       "NULL",       "NULL",       "NULL",       "NULL",
      "NULL",      "NULL",       "NULL",       "NULL",       "numeric",    "numeric",  
      "numeric",   "NULL",       "numeric",    "NULL",       "NULL",       "NULL",
      "NULL",      "NULL",       "NULL",       "NULL",       "NULL",       "NULL",
      "NULL")
head = read.csv("repdata-data-StormData.csv.bz2", header=TRUE, nrows=25)

validEventTypes = as.vector(sapply(
                             c("Astronomical Low Tide",
                               "Avalanche",
                               "Blizzard",
                               "Coastal Flood",
                               "Cold/Wind Chill", 
                               "Debris Flow", 
                               "Dense Fog", 
                               "Dense Smoke",
                               "Drought",
                               "Dust Devil",
                               "Dust Storm",
                               "Excessive Heat",
                               "Extreme Cold/Wind Chill",
                               "Flash Flood",
                               "Flood",
                               "Frost/Freeze",
                               "Funnel Cloud",
                               "Freezing Fog",
                               "Hail",
                               "Heat",
                               "Heavy Rain",
                               "Heavy Snow",
                               "High Surf",
                               "High Wind",
                               "Hurricane (Typhoon)",
                               "Ice Storm",
                               "Lake-Effect Snow",
                               "Lakeshore Flood",
                               "Lightning",
                               "Marine Hail",
                               "Marine High Wind",
                               "Marine Strong Wind",
                               "Marine Thunderstorm Wind",
                               "Rip Current",
                               "Seiche",
                               "Sleet",
                               "Storm Surge/Tide",
                               "Strong Wind",
                               "Thunderstorm Wind",
                               "Tornado",
                               "Tropical Depression",
                               "Tropical Storm",
                               "Tsunami",
                               "Volcanic Ash",
                               "Waterspout",
                               "Wildfire",
                               "Winter Storm",
                               "Winter Weather"), toupper))

test = read.csv("repdata-data-StormData.csv", header=TRUE, colClasses=readcols, comment.char="")
test <- test[test$BGN_DATE >= 1996, ]
test <- test[test$FATALITIES != 0 | test$INJURIES != 0 | test$PROPDMG != 0 | test$CROPDMG != 0, ]
test$EVTYPE <- as.vector(sapply(test$EVTYPE, toupper))

# List out all of the invalid EVTYPEs originally 141 types
#badTypes <- test[!(test$EVTYPE %in% validEventTypes), ]
#unique(badTypes$EVTYPE)

# Get specific bad events you're searching for
#tstmIdx <- grep("TSTM", unique(badTypes$EVTYPE))
#unique(badTypes$EVTYPE)[tstmIdx]

# Clean up code
#badTypes$EVTYPE <- as.vector(sapply(badTypes$EVTYPE, function(x) if (grepl("NON.*TSTM WIND", x)) "HIGH WIND" else x))
#badTypes$EVTYPE <- as.vector(sapply(badTypes$EVTYPE, function(x) if (grepl("MARINE TSTM WIND", x)) "MARINE THUNDERSTORM WIND" else x))
#badTypes$EVTYPE <- as.vector(sapply(badTypes$EVTYPE, function(x) if (grepl("TSTM WIND", x)) "THUNDERSTORM WIND" else x))
#badTypes$EVTYPE <- as.vector(sapply(badTypes$EVTYPE, function(x) if (grepl("GUSTY WIND", x)) "HIGH WIND" else x))
#badTypes$EVTYPE <- as.vector(sapply(badTypes$EVTYPE, function(x) if (grepl("COLD", x)) "COLD/WIND CHILL" else x))
#badTypes$EVTYPE <- as.vector(sapply(badTypes$EVTYPE, function(x) if (grepl("FOG", x)) "DENSE FOG" else x))
#badTypes$EVTYPE <- as.vector(sapply(badTypes$EVTYPE, function(x) if (grepl("ROAD", x)) "BLIZZARD" else x))
#badTypes$EVTYPE <- as.vector(sapply(badTypes$EVTYPE, function(x) if (grepl("FLASH", x)) "FLASH FLOOD" else x))
#badTypes$EVTYPE <- as.vector(sapply(badTypes$EVTYPE, function(x) if (grepl("LAKE EFFECT SNOW", x)) "LAKE-EFFECT SNOW" else x))
#badTypes$EVTYPE <- as.vector(sapply(badTypes$EVTYPE, function(x) if (grepl("SNOW", x)) "HEAVY SNOW" else x))
#badTypes$EVTYPE <- as.vector(sapply(badTypes$EVTYPE, function(x) if (grepl("SURF", x)) "HIGH SURF" else x))
#badTypes$EVTYPE <- as.vector(sapply(badTypes$EVTYPE, function(x) if (grepl("FREEZING SPRAY", x)) "HIGH SURF" else x))
#badTypes$EVTYPE <- as.vector(sapply(badTypes$EVTYPE, function(x) if (grepl("FREEZ", x)) "FROST/FREEZE" else x))
#badTypes$EVTYPE <- as.vector(sapply(badTypes$EVTYPE, function(x) if (grepl("FROST", x)) "FROST/FREEZE" else x))
#badTypes$EVTYPE <- as.vector(sapply(badTypes$EVTYPE, function(x) if (grepl("FREEZING RAIN", x)) "SLEET" else x))








