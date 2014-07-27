# Set up date processing
library("lubridate")
setClass("myDate")
setClass("typeUpper")
setAs("character", "myDate", function(from) as.numeric(year(mdy_hms(from))))
setAs("character", "typeUpper", function(from) toupper(from))

#colnames <- 
#    c("STATE__",   "BGN_DATE",   "BGN_TIME",   "TIME_ZONE",  "COUNTY",     "COUNTYNAME",
#      "STATE",     "EVTYPE",     "BGN_RANGE",  "BGN_AZI",    "BGN_LOCATI", "END_DATE",  
#      "END_TIME",  "COUNTY_END", "COUNTYENDN", "END_RANGE",  "END_AZI",    "END_LOCATI",
#      "LENGTH",    "WIDTH",      "F",          "MAG",        "FATALITIES", "INJURIES",  
#      "PROPDMG",   "PROPDMGEXP", "CROPDMG",    "CROPDMGEXP", "WFO",        "STATEOFFIC",
#      "ZONENAMES", "LATITUDE",   "LONGITUDE",  "LATITUDE_E", "LONGITUDE_", "REMARKS",
#      "REFNUM")

#readcols <-
#    c("NULL",      "myDate",     "NULL",       "NULL",       "NULL",       "NULL",
#      "NULL",      "character",  "NULL",       "NULL",       "NULL",       "NULL",  
#      "NULL",      "NULL",       "NULL",       "NULL",       "NULL",       "NULL",
#      "NULL",      "NULL",       "NULL",       "NULL",       "numeric",    "numeric",  
#      "numeric",   "character",  "numeric",    "character",  "NULL",       "NULL",
#      "NULL",      "NULL",       "NULL",       "NULL",       "NULL",       "NULL",
#      "NULL")

#f <- function()
#{
columnsToRead = c("NULL",
                  "myDate",
                  rep("NULL", 5),
                  "typeUpper",
                  rep("NULL", 14),
                  "numeric",    "numeric",  "numeric",   "character",  "numeric",    "character",
                  rep("NULL", 9))

eventData = read.csv("repdata-data-StormData.csv.bz2", header=TRUE, colClasses=columnsToRead, comment.char="")
colnames(eventData) <- 
    c("year", "type", "fatalities", "injuries", "propertyDamage", "propertyDamageScale", "cropDamage", "cropDamageScale")

eventData <- eventData[eventData$year >= 1996, ]
eventData <- eventData[eventData$fatalities != 0 | eventData$fatalities != 0 | 
                       eventData$propertyDamage != 0 | eventData$cropDamage != 0, ]

# return actual numeric value for damage data
# expects a two-column dataframe that contains the damage and damage scale columns
getNumericValue <- function(damageColumns)
{
    if ("" == damageColumns[2]) return(as.numeric(damageColumns[1]))
    if ("K" == damageColumns[2]) return(1000 * as.numeric(damageColumns[1]))
    if ("M" == damageColumns[2]) return(1000000 * as.numeric(damageColumns[1]))
    if ("B" == damageColumns[2]) return(1000000000 * as.numeric(damageColumns[1]))
}

propertyDamage <- apply(eventData[, c("propertyDamage", "propertyDamageScale")], 1, getNumericValue)
cropDamage <- apply(eventData[, c("cropDamage", "cropDamageScale")], 1, getNumericValue)
eventData <- cbind(eventData[, 1:4], propertyDamage, cropDamage)

print(head(eventData))


validEventTypes <-
    as.vector(sapply(c("Astronomical Low Tide",   "Avalanche",           "Blizzard",
                       "Coastal Flood",           "Cold/Wind Chill",     "Debris Flow", 
                       "Dense Fog",               "Dense Smoke",         "Drought",
                       "Dust Devil",              "Dust Storm",          "Excessive Heat",
                       "Extreme Cold/Wind Chill", "Flash Flood",         "Flood",
                       "Frost/Freeze",            "Funnel Cloud",        "Freezing Fog",
                       "Hail",                    "Heat",                "Heavy Rain",
                       "Heavy Snow",              "High Surf",           "High Wind",
                       "Hurricane (Typhoon)",     "Ice Storm",           "Lake-Effect Snow",
                       "Lakeshore Flood",         "Lightning",           "Marine Hail",
                       "Marine High Wind",        "Marine Strong Wind",  "Marine Thunderstorm Wind",
                       "Rip Current",             "Seiche",              "Sleet",
                       "Storm Surge/Tide",        "Strong Wind",         "Thunderstorm Wind",
                       "Tornado",                 "Tropical Depression", "Tropical Storm",
                       "Tsunami",                 "Volcanic Ash",        "Waterspout",
                       "Wildfire",                "Winter Storm",        "Winter Weather"), toupper))

# Display invalid event types
invalidEventTypes <- eventData[!(eventData$type %in% validEventTypes), ]
print(paste("The percentage of events that have invalid event types = ", 
            round(nrow(invalidEventTypes) / nrow(eventData) * 100, 2)))
print(unique(invalidEventTypes$type))

#}

# Remove invalid event types
convertToValidEventType <- function(eventType)
{
    if (grepl("MARINE TSTM WIND", eventType)) return("MARINE THUNDERSTORM WIND")
    if (grepl("TSTM WIND", eventType)) return("THUNDERSTORM WIND")
    if (grepl("THUNDERSTORM", eventType)) return("THUNDERSTORM WIND")
    if (grepl("GUSTY WIND", eventType)) return("HIGH WIND")
    if (!(grepl("^EXTREME", eventType)) & grepl("COLD", eventType)) return("COLD/WIND CHILL")
    if (grepl("^EXTREME", eventType)) return("EXTREME COLD/WIND CHILL")
    if (grepl("EXPOSURE", eventType)) return("EXTREME COLD/WIND CHILL")
    if (grepl("^FOG", eventType)) return("DENSE FOG")
    if (grepl("ROAD", eventType)) return("WINTER WEATHER")
    if (grepl("WEATHER", eventType)) return("WINTER WEATHER")
    if (grepl("BLACK ICE", eventType)) return("WINTER WEATHER")
    if (grepl("LAKE EFFECT SNOW", eventType)) return("LAKE-EFFECT SNOW")
    if (!(grepl("^LAKE", eventType)) & grepl("SNOW", eventType)) return("HEAVY SNOW")        
    if (grepl("SURF", eventType)) return("HIGH SURF")
    if (grepl("FREEZING SPRAY", eventType)) return("HIGH SURF")
    if (grepl("FREEZING RAIN", eventType)) return("SLEET")
    if (grepl("MIX", eventType)) return("SLEET")
    if (grepl("FREEZ", eventType) & !(grepl("FOG", eventType))) return("FROST/FREEZE")
    if (grepl("FROST", eventType)) return("FROST/FREEZE")
    if (grepl("ASTRONOMICAL", eventType)) return("ASTRONOMICAL LOW TIDE")
    if (grepl("COAST", eventType)) return("COASTAL FLOOD")
    if (grepl("EROSION", eventType)) return("COASTAL FLOOD")
    if (grepl("FLASH", eventType)) return("FLASH FLOOD")
    if (grepl("HURRICANE", eventType)) return("HURRICANE (TYPHOON)")
    if (grepl("TYPHOON", eventType)) return("HURRICANE (TYPHOON)")
    if (grepl("CURRENTS", eventType)) return("RIP CURRENT")
    if (grepl("FIRE", eventType)) return("WILDFIRE")
    if (grepl("TIDAL", eventType)) return("STORM SURGE/TIDE")
    if (grepl("SURGE", eventType)) return("STORM SURGE/TIDE")
    if (grepl("BURST", eventType)) return("HEAVY RAIN")
    if (grepl("RAIN", eventType)) return("HEAVY RAIN")
    if (grepl(" SEAS", eventType)) return("MARINE HIGH WIND")
    if (grepl("SWELLS", eventType)) return("MARINE HIGH WIND")
    if (grepl("WIND AND WAVE", eventType)) return("MARINE HIGH WIND")
    if (grepl("RIVER", eventType)) return("FLOOD")
    if (grepl("DAM", eventType)) return("FLOOD")
    if (grepl("HIGH WATER", eventType)) return("FLOOD")
    if (grepl("HEAT", eventType)) return("EXCESSIVE HEAT")
    if (grepl("BLOWING DUST", eventType)) return("DUST STORM")
    if (grepl("LANDSPOUT", eventType)) return("DUST DEVIL")
    if (grepl("^WIND", eventType)) return("HIGH WIND")
    if (grepl("WINDS", eventType)) return("HIGH WIND")
    if (grepl("GRADIENT WIND", eventType)) return("HIGH WIND")
    if (grepl("G40", eventType)) return("HIGH WIND")
    if (grepl("WHIRLWIND", eventType)) return("TORNADO")
    if (grepl("SMALL HAIL", eventType)) return("HAIL")
    if (grepl("ICE JAM", eventType)) return("OTHER")
    if (grepl("URBAN", eventType)) return("OTHER")
    if (grepl("SLIDE", eventType)) return("OTHER")
    if (grepl("SLUMP", eventType)) return("OTHER")
    if (grepl("GLAZE", eventType)) return("OTHER")
    if (grepl("DROWN", eventType)) return("OTHER")
    if (grepl("UNSEASONABLY", eventType)) return("OTHER")
    if (grepl("ACCIDENT", eventType)) return("OTHER")
    
    return(eventType)
}

eventData$type <- sapply(eventData$type, convertToValidEventType)
eventData <- eventData[eventData$type != "OTHER", ]

par(mfrow = c(1, 2))
boxplot(eventData$fatalities, eventData$injuries, 
        main="Distribution of Fatalities/Injuries",
        xlab="Fatalities | Injuries")

boxplot(eventData$propertyDamage, eventData$cropDamage,
        main="Distribution of Property/Crop Damage",
        xlab="Property | Crop",
        ylab="Number in Dollars")

maxIndex = which.max(eventData$propertyDamage)
print(eventData[maxIndex,])
eventData$propertyDamage[maxIndex] <- eventData$propertyDamage[maxIndex] / 1000


eventData$type <- factor(eventData$type)


q <- qplot(type,x,data=fatalities,geom="bar", stat="identity")
q + theme(axis.text.x = element_text(angle = -90, hjust = 0))

