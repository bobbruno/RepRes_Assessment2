Reproducible Research - Peer Assignment 2: Severity Analysis of Storms in the USA (1966-2010)
========================================================

## Synopsis

**Still have to write something interesting here. Probably on the lines of what is NOAA, what kinds of events are there, what analysis was made and what kinds of conclusions.

## Introduction

## Data Processing

### Loading the data
The data is presented as a single CSV, compacted into a bzip format, which can be directly read
throught the `read.csv` command into a dataframe.

```r
stormData = read.csv("Data/StormData.csv.bz2", stringsAsFactors=FALSE)
```

The resulting dataframe has the following structure:


### Processing the data

First, generate a usable and consistent exponent to damage property. Values of "", -, ? and + have been all considered to be 0:

```r
stormData$DMGEXP2[stormData$PROPDMGEXP %in% c("", "-", "?", "+", "0")]=0
stormData$DMGEXP2[stormData$PROPDMGEXP == "1"]=1
stormData$DMGEXP2[stormData$PROPDMGEXP %in% c("h", "H", "2")]=2
stormData$DMGEXP2[stormData$PROPDMGEXP %in% c("3", "K")]=3
stormData$DMGEXP2[stormData$PROPDMGEXP == "4"]=4
stormData$DMGEXP2[stormData$PROPDMGEXP == "5"]=5
stormData$DMGEXP2[stormData$PROPDMGEXP %in% c("M", "m", "6")]=6
stormData$DMGEXP2[stormData$PROPDMGEXP == "7"]=7
stormData$DMGEXP2[stormData$PROPDMGEXP == "8"]=8
stormData$DMGEXP2[stormData$PROPDMGEXP == "B"]=9
stormData$CDMGEXP2[stormData$CROPDMGEXP %in% c("", "?", "0")]=0
stormData$CDMGEXP2[stormData$CROPDMGEXP %in% c("M", "m")]=6
stormData$CDMGEXP2[stormData$CROPDMGEXP %in% c("K", "k")]=3
stormData$CDMGEXP2[stormData$CROPDMGEXP == "B"]=9
stormData$CDMGEXP2[stormData$CROPDMGEXP == "2"]=2
```

Having these numbers allows us to calculate which events generate the most impact. The following cutoff rules will be applied:
- Event types will be ordered in descending order by the %of total impact they generate on the entire database;
- Fatalities and injury events will be treated independently and grouped on a single data frame for health impacts
- for health impact, the events which generate 90% of impact on either fatalities or injuries will be considered;
- Property and crop damage will be treated independently and grouped on a single dataframe for economic impact;
- For economic impact, the events which generate 90% of impact on either property or damage will be considered.

```r
require(plyr)
```

```
## Loading required package: plyr
```

```
## Warning: package 'plyr' was built under R version 3.0.3
```

```r
quantifEvs = data.frame(event=stormData$EVTYPE, fatalities=stormData$FATALITIES,
               injuries=stormData$INJURIES, 
               propDmg = stormData$PROPDMG*10^stormData$DMGEXP2,
               cropDmg= stormData$CROPDMG*10^stormData$CDMGEXP2)
quantifEvs$event=as.character(quantifEvs$event)
sumEvs=ddply(quantifEvs, "event", summarize, 
        sumFatal=sum(fatalities), cntFatal=sum(fatalities>0), 
        sumInj=sum(injuries), cntInj=sum(injuries>0),
        sumPropDmg = sum(propDmg), cntPropDmg=sum(propDmg>0),
        sumCropDmg = sum(cropDmg), cntCropDmg=sum(cropDmg>0))
totFatal = sum(sumEvs$sumFatal)
totInj = sum(sumEvs$sumInj)
totPropDmg = sum(sumEvs$sumPropDmg)
totCropDmg = sum(sumEvs$sumCropDmg)
topFatal = order(sumEvs$sumFatal, decreasing=TRUE)
topInj = order(sumEvs$sumInj, decreasing=TRUE)
topPropDmg = order(sumEvs$sumPropDmg, decreasing=TRUE)
topCropDmg = order(sumEvs$sumCropDmg, decreasing=TRUE)
print(sprintf("The top %d event types with fatalities generate %0.1f%% of fatalities.", 22,
              sum(sumEvs$sumFatal[topFatal[1:22]])/totFatal*100))
```

```
## [1] "The top 22 event types with fatalities generate 90.5% of fatalities."
```

```r
fatalCutoff = sumEvs$event[topFatal[1:22]]
print(sprintf("The top %d event types with injuries generate %0.1f%% of injuries.", 11,
              sum(sumEvs$sumInj[topInj[1:11]])/totInj*100))
```

```
## [1] "The top 11 event types with injuries generate 90.3% of injuries."
```

```r
injCutoff = sumEvs$event[topInj[1:11]]
print(sprintf("The top %d event types with property damage generate %0.1f%% of losses.", 12,
              sum(sumEvs$sumPropDmg[topPropDmg[1:12]])/totPropDmg*100))
```

```
## [1] "The top 12 event types with property damage generate 90.7% of losses."
```

```r
propDmgCutoff = sumEvs$event[topPropDmg[1:12]]
print(sprintf("The top %d event types with crop damage generate %0.1f%% of losses.", 14,
              sum(sumEvs$sumCropDmg[topCropDmg[1:14]])/totCropDmg*100))
```

```
## [1] "The top 14 event types with crop damage generate 90.6% of losses."
```

```r
cropDmgCutoff = sumEvs$event[topCropDmg[1:14]]

healthImpEvs = subset(stormData, EVTYPE %in% fatalCutoff | EVTYPE %in% injCutoff)
econImpEvs = subset(stormData, EVTYPE %in% propDmgCutoff | EVTYPE %in% cropDmgCutoff)
print("The list of relevante event types then is limited the the following:")
```

```
## [1] "The list of relevante event types then is limited the the following:"
```

```r
unique(c(healthImpEvs$EVTYPE, econImpEvs$EVTYPE))
```

```
##  [1] "TORNADO"                 "TSTM WIND"              
##  [3] "HAIL"                    "WINTER STORM"           
##  [5] "HEAVY RAIN"              "LIGHTNING"              
##  [7] "THUNDERSTORM WIND"       "RIP CURRENT"            
##  [9] "FLASH FLOOD"             "HEAT"                   
## [11] "FLOOD"                   "EXTREME COLD"           
## [13] "HIGH WIND"               "BLIZZARD"               
## [15] "HEAVY SNOW"              "ICE STORM"              
## [17] "AVALANCHE"               "EXTREME HEAT"           
## [19] "EXCESSIVE HEAT"          "HIGH SURF"              
## [21] "STRONG WIND"             "HEAT WAVE"              
## [23] "RIP CURRENTS"            "EXTREME COLD/WIND CHILL"
## [25] "RIVER FLOOD"             "DROUGHT"                
## [27] "STORM SURGE"             "TROPICAL STORM"         
## [29] "WILDFIRE"                "HURRICANE"              
## [31] "FROST/FREEZE"            "HURRICANE/TYPHOON"
```

The list of relevant events clearly has some quality problems, namely different spellings for the same event type. The following corrections are needed:
- Replace"EXTREME COLD" by "EXTREME COLD/WIND CHILL"
- Replace "EXTREME HEAT" by "EXCESSIVE HEAT"
- Replace "RIVER FLOOD" by "FLOOD"
- Replace "HEAT WAVE" by "HEAT"
- Replace "HURRICANE" by "HURRICANE/TYPHOON"
- Replace "RIP CURRENTS" by "RIP CURRENT"
- Replace "TSTM WIND" by "THUNDERSTORM WIND"

```r
econImpEvs$EVTYPE[econImpEvs$EVTYPE == "EXTREME COLD"] = "EXTREME COLD/WIND CHILL"
econImpEvs$EVTYPE[econImpEvs$EVTYPE == "EXTREME HEAT"] = "EXCESSIVE HEAT"
econImpEvs$EVTYPE[econImpEvs$EVTYPE == "RIVER FLOOD"] = "FLOOD"
econImpEvs$EVTYPE[econImpEvs$EVTYPE == "HEAT WAVE"] = "HEAT"
econImpEvs$EVTYPE[econImpEvs$EVTYPE == "HURRICANE"] = "HURRICANE/TYPHOON"
econImpEvs$EVTYPE[econImpEvs$EVTYPE == "RIP CURRENTS"] = "RIP CURRENT"
econImpEvs$EVTYPE[econImpEvs$EVTYPE == "TSTM WIND"] = "THUNDERSTORM WIND"

healthImpEvs$EVTYPE[healthImpEvs$EVTYPE == "EXTREME COLD"] = "EXTREME COLD/WIND CHILL"
healthImpEvs$EVTYPE[healthImpEvs$EVTYPE == "EXTREME HEAT"] = "EXCESSIVE HEAT"
healthImpEvs$EVTYPE[healthImpEvs$EVTYPE == "RIVER FLOOD"] = "FLOOD"
healthImpEvs$EVTYPE[healthImpEvs$EVTYPE == "HEAT WAVE"] = "HEAT"
healthImpEvs$EVTYPE[healthImpEvs$EVTYPE == "HURRICANE"] = "HURRICANE/TYPHOON"
healthImpEvs$EVTYPE[healthImpEvs$EVTYPE == "RIP CURRENTS"] = "RIP CURRENT"
healthImpEvs$EVTYPE[healthImpEvs$EVTYPE == "TSTM WIND"] = "THUNDERSTORM WIND"
```

- Blizzards of less than 3 hours shoul be reduced to "WINTER STORM"



.
