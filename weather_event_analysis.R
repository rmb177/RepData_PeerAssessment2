# Set up date processing and convert all event types to upper case
library("lubridate")

setClass("myDate")
setClass("typeUpper")
setAs("character", "myDate", function(from) as.numeric(year(mdy_hms(from))))
setAs("character", "typeUpper", function(from) toupper(from))


# Filter out unnecessary columns
columnsToRead = c("NULL",
                  "myDate",
                  rep("NULL", 5),
                  "typeUpper",
                  rep("NULL", 14),
                  "numeric",    "numeric",  "numeric",   "character",  "numeric",    "character",
                  rep("NULL", 9))

# read data
eventData = read.csv("repdata-data-StormData.csv.bz2", header=TRUE, colClasses=columnsToRead, comment.char="")
colnames(eventData) <- 
    c("year", "type", "fatalities", "injuries", "propertyDamage", "propertyDamageScale", "cropDamage", "cropDamageScale")

# filter out data prior to 1996 and any events that do not have health/damage values
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
        xlab="Fatalities | Injuries",
        cex.main=0.75)

boxplot(eventData$propertyDamage, eventData$cropDamage,
        main="Distribution of Property/Crop Damage",
        xlab="Property | Crop",
        ylab="Number of Dollars",
        cex.main=0.75)

maxIndex = which.max(eventData$propertyDamage)
print(eventData[maxIndex,])
eventData$propertyDamage[maxIndex] <- eventData$propertyDamage[maxIndex] / 1000


library("ggplot2")
library("gridExtra")

# Create plot figure for health information
eventData$type <- factor(eventData$type)
healthDataSum <- setNames(aggregate(eventData$fatalities + eventData$injuries, 
                                    by=eventData[c("type")], 
                                    FUN=sum), 
                          c("type", "amount"))
healthDataSum <- healthDataSum[order(-healthDataSum$amount),][1:5,]

healthDataAvg <- setNames(aggregate(eventData$fatalities + eventData$injuries, 
                                    by=eventData[c("type")], 
                                    FUN=mean), 
                          c("type", "amount"))
healthDataAvg <- healthDataAvg[order(-healthDataAvg$amount),][1:5,]

p1 <- ggplot(healthDataSum,
             aes(reorder(healthDataSum$type, order(healthDataSum$amount, decreasing=TRUE)), 
                 healthDataSum$amount)) +
    xlab("") +
    ylab("Total Fatalities and Injuries") +
    geom_bar(stat="identity") +  
    theme(axis.text.x = element_text(angle = -60, hjust = 0)) 

p2 <- ggplot(healthDataAvg,
             aes(reorder(healthDataAvg$type, order(healthDataAvg$amount, decreasing=TRUE)), 
                 healthDataAvg$amount)) +
    xlab("") +
    ylab("Average Fatalities and Injuries") +
    geom_bar(stat="identity") +  
    theme(axis.text.x = element_text(angle = -60, hjust = 0)) 

grid.arrange(p1, p2, ncol=2, 
             main="Effects of Weather Events on Population Health")


# Create plot figure for damage information
damageDataSum <- setNames(aggregate(eventData$propertyDamage / 1000000 + eventData$cropDamage / 1000000, 
                                    by=eventData[c("type")], 
                                    FUN=sum), 
                          c("type", "amount"))
damageDataSum <- damageDataSum[order(-damageDataSum$amount),][1:5,]

damageDataAvg <- setNames(aggregate(eventData$propertyDamage / 1000000 + eventData$cropDamage / 1000000, 
                                    by=eventData[c("type")], 
                                    FUN=mean), 
                          c("type", "amount"))
damageDataAvg <- damageDataAvg[order(-damageDataAvg$amount),][1:5,]

p1 <- ggplot(damageDataSum,
             aes(reorder(damageDataSum$type, order(damageDataSum$amount, decreasing=TRUE)), 
                 damageDataSum$amount)) +
    xlab("") + 
    ylab("Total Damage") +
    geom_bar(stat="identity") +  
    theme(axis.text.x = element_text(angle = -60, hjust = 0))

p2 <- ggplot(damageDataAvg,
             aes(reorder(damageDataAvg$type, order(damageDataAvg$amount, decreasing=TRUE)), 
                 damageDataAvg$amount)) +
    xlab("") + 
    ylab("Average Damage") +
    geom_bar(stat="identity") +  
    theme(axis.text.x = element_text(angle = -60, hjust = 0))

grid.arrange(p1, p2, ncol=2, 
             main=paste("Crop and Property Damage Caused by Weather Events",
                        "(Damage in millions of dollars)",
                        sep="\n"))


