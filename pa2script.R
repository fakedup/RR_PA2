setwd ("/home/nikita/R/RR/PA2")

if (!file.exists("stormdata.bz2")){
        download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "stormdata.bz2", method = "curl")
}

stormdata <- read.table(bzfile("stormdata.bz2"), header = TRUE, sep=',', na.strings = "", stringsAsFactors = FALSE)

stormdata$BGN_DATE <- strptime (stormdata$BGN_DATE, format = "%m/%d/%Y")

write (levels(stormdata$EVTYPE), file = "evtypes.txt")

evtypesCorrection <- read.csv("edited_evtypes.csv", stringsAsFactors = FALSE)

for (i in (1:length(evtypesCorrection[,1]))){
        stormdata$EVTYPE[stormdata$EVTYPE==evtypesCorrection$wrong[i]] <- evtypesCorrection$right[i]
}

stormdata$EVTYPE <- as.factor(stormdata$EVTYPE)

table (stormdata$PROPDMGEXP)

costSign <- data.frame(sign = c('K','M','B'),val = c(10**3,10**6,10**9))

stormdata$PROPDMGEXP[!stormdata$PROPDMGEXP %in% costSign$sign] <- 0
stormdata$CROPDMGEXP[!stormdata$CROPDMGEXP %in% costSign$sign] <- 0

for (i in (1:3)){
        stormdata$PROPDMGEXP[stormdata$PROPDMGEXP == costSign$sign[i]] <- costSign$val[i]
        stormdata$CROPDMGEXP[stormdata$CROPDMGEXP == costSign$sign[i]] <- costSign$val[i]
        
}

stormdata$PROPDMGEXP <- as.numeric(stormdata$PROPDMGEXP)
stormdata$PROPDMG <- stormdata$PROPDMG*stormdata$PROPDMGEXP

stormdata$CROPDMGEXP <- as.numeric(stormdata$CROPDMGEXP)
stormdata$CROPDMG <- stormdata$CROPDMG*stormdata$CROPDMGEXP

stormdata <- subset(stormdata, select=-c(CROPDMGEXP, PROPDMGEXP))

boxplot(stormdata$PROPDMG[stormdata$PROPDMG != 0]) #detect outlier
stormdata[stormdata$PROPDMG==max(stormdata$PROPDMG),] #outlier

"""Remark:
Major flooding continued into the early hours of January 1st, before the Napa River finally fell below flood stage and the water receeded.
Flooding was severe in Downtown Napa from the Napa Creek and the City and Parks Department was hit with $6 million in damage alone.
The City of Napa had 600 homes with moderate damage, 150 damaged businesses with costs of at least $70 million.
"""
stormdata$PROPDMG[stormdata$PROPDMG==max(stormdata$PROPDMG)] <- 76000000

par(mar = c(7,4,2,2))

fatalities <- aggregate (FATALITIES~EVTYPE, stormdata, sum)
fatalities <- fatalities[fatalities$FATALITIES > quantile(fatalities$FATALITIES, probs = 0.9), ]
with (fatalities, {
        barplot (FATALITIES, names.arg = EVTYPE, las = 3, cex.names=0.7, main = "Top event types by total fatalities numbers")
})

injuries <- aggregate (INJURIES~EVTYPE, stormdata, sum)
injuries <- injuries[injuries$INJURIES > quantile(injuries$INJURIES, probs = 0.9), ]
with (injuries, {
        barplot (INJURIES, names.arg = EVTYPE, las = 3, cex.names=0.7, main = "Top event types by total non-fatal injuries numbers")
})

damageCost <- aggregate (cbind (PROPDMG, CROPDMG)~EVTYPE, stormdata, sum)
damageCost <- damageCost[(damageCost$PROPDMG + damageCost$CROPDMG > quantile (damageCost$PROPDMG + damageCost$CROPDMG, probs = 0.9)),]
with (damageCost, {
        barplot (PROPDMG+CROPDMG, names.arg = EVTYPE, las = 3, cex.names=0.7, main = "Event types with greatest economy impact")
})