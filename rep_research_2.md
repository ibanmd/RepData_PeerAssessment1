# An Analysis of the NOAA Storm Data Set
#### Coursera - Reproducible Research - July 2014
#### Mario Ibanez

### *The purpose of this analysis is to answer two key questions: Which weather events were most harmful to the population health, and which weather events had the greatest economic consequences.  Harm to population health is measured in terms of injuries and fatalities, while economic consequences are measured in terms of dollar amounts. In order to come up with reasonable conclusions, a certain amount of cleaning and preparation of the data had to first be performed, which will also be included in this report.  The majority of preparation involved restricting our scope to just certain columns, analyzing only those weather events that occured in 1996 or more recently, and creating a few new columns in order to help with analysis later on.*


# Data Processing

The data was obtained through a link provided within the Coursera course called Reproducible Research (July, 2014).  The unzipped file is approximately 500mb and consists of 37 columns and 902297 rows.


```r
## Load the package "stringdist" for use later.
library(stringdist)
```

```
## Loading required package: parallel
```

```r
require(stringdist)
```


```r
## Download the file from the link given by the Reproducible Research Coursera course.
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destinationFile <- "/Users/Mario/StormData.csv.bz2"
download.file(fileURL, destinationFile, method="curl")

## Read the .csv file into a data frame.
data <- read.csv(destinationFile)
```

As mentioned, there are 37 columns and here are their column names:


```r
## Returns the names of the columns in the data frame.
names(data)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```

In order to keep this analysis focused, only this subset of columns will be kept in the data frame: 

1. "STATE__"        
2. "BGN_DATE"       
3. "BGN_TIME"       
7. "STATE"          
8. "EVTYPE"         
21. "F"             
22. "MAG"           
23. "FATALITIES"    
24. "INJURIES"      
25. "PROPDMG"       
26. "PROPDMGEXP"    
27. "CROPDMG"       
28. "CROPDMGEXP"    

Please see the Appendix at the end of this document for more information about the meaning of each of these.  

So we will next go ahead and subset the data frame to keep just these columns.


```r
## Subset the data frame to include only the columns of interest.  The new data frame will be called "data_c" to represent the fact that it has been subsetted by columns.
data_c <- data[c("STATE__","BGN_DATE","BGN_TIME","STATE","EVTYPE","F","MAG","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]
```

The next thing to do is create the "dictionary" of the 48 official event types.  The 900+ event types in the original data set will be mapped to one of these 48 official event types using the amatch() function in the "stringdist" pachage, using the Levenshtein distance to compare the similarities and differences between strings.


```r
## Create the dictionary
## These 48 event types can be found on pages 2, 3, and 4 at this url: http://www.nws.noaa.gov/directives/sym/pd01016005curr.pdf
## To view the original set of event types, use: levels(factor(data$EVTYPE)) .

dictionary <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Freezing Fog","Frost/Freeze","Funnel Cloud","Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind","Hurricane/Typhoon","Ice Storm","Lakeshore Flood","Lake-Effect Snow","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind","Marine Thunderstorm Wind","Rip Current","Seiche","Sleet","Storm Tide","Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather")
```

First make the dictionary and the EVTYPE column both lowercase, and then add the replacement column to the data frame.


```r
## Make both the dictionary and the EVTYPE column lowercase, to make the mapping more accurate.
dictionary <- tolower(dictionary)
data_c$EVTYPE <- tolower(data_c$EVTYPE)
data_c$EVTYPE_48 <- dictionary[amatch(data_c$EVTYPE,dictionary,method="lv", maxDist=20)]
```

I would however like to take a moment to talk about the validity of using this method to standardize the event types found in the column EVTYPE.  There are numerous non-standard entries such as "?", "dust devil waterspout", "freezing drizzle", "gustnado and", "lack of snow", "no severe weather", "none", "summary of june 6", and even "excessive".  Under the assumption that these represent a minority of cases, and that events like "no severe weather" and "none" did not lead to casualties or property damage, I decided that it was okay to let the function amatch() decide what to assign to each of these values.  I will also not be looking at averages, so this is another reason that this method is sufficient.  This would not be a good method to use if, for example, one wanted to know how much damage tornados caused on average, per occurence. 

Next, since the full range of 48 event types were not included into the data base until 1996, we'll subset the data frame to include only years greater than or equal to 1996.  In reality this step could have been done before running the amatch() function.  It would have saved a little time, but the amatch() function excuted rather quickly even on the full data set.


```r
## Creates a column of dates for ease of use.
data_c$BGN_DATE_Stan <- as.Date(data_c$BGN_DATE,format="%m/%d/%Y")

## Creates a new data frame.  The "d" added to the name signifies that it has been subsetted according to date. (Starting January 1st, 1996 an onwards, inclusive)
data_cd <- data_c[data_c$BGN_DATE_Stan >= "1996-01-01",]
```

Further, the data will be subsetted to include only events that had either casualties or property damage.  This is due to the fact that we will only be looking at totals rather than averages.


```r
## Removes all rows that have a 0 in each of the 4 columns FATALITIES, INJURIES, PROPDMG, CROPDMG.  
## In other words, if a weather event led to no casualites or economic damage, we will ignore it from this point on.
data_cd0 <- data_cd[data_cd$FATALITIES != 0 | data_cd$INJURIES != 0 | data_cd$PROPDMG != 0 | data_cd$CROPDMG != 0,]
```

Before we begin to look at results, let's do a little sanity check and compare the 8 most common event types in the column EVTYPE to the 8 most common event types in EVTYPE_48.  Remember that EVTYPE has many nonstandard and misspelled entries, while the latter only contains the standard 48 event types.


```r
## These return the top 8 most common event types in the columns EVTYPE and EVTYPE_48.
head(sort(table(data_cd0$EVTYPE),decreasing=TRUE),n=8)
```

```
## 
##         tstm wind thunderstorm wind              hail       flash flood 
##             61776             43097             22679             19011 
##           tornado         lightning             flood         high wind 
##             12366             11152              9513              5402
```

```r
head(sort(table(data_cd0$EVTYPE_48),decreasing=TRUE),n=8)
```

```
## 
##         high wind thunderstorm wind              hail       flash flood 
##             67244             43100             22683             19100 
##           tornado         lightning             flood       strong wind 
##             12366             11294              9744              3414
```

Pleasently, we see no immediately cause for concern with the way the function amatch() has cleaned up our EVTYPE column.  "tstm wind" means "thunderstorm wind", and it is a little unfortunate that "tstm wind" was apparently mapped to "high wind", but the meanings are similar enough that we can accept this.  The important thing is, we know that in the original data wind, hail, floods, and tornados were the most common event types, and they are as well in our new column of standard event types. 

Another step of processing that will be done is to combine the data on fatalities and injuries into a total number of casualities.  Instead of simply adding the two figures however, a weight of 10 will be given to each fatality.  This is an arbitrary weight, though more reasonable than considering a fatality and an injury to be of equal importance.  This will be added to a column called CASUALTIES.


```r
## Adds a column called CASUALITIES to the data frame, which is a weighted combination of fatalties and injuries.
data_cd0$CASUALTIES <- 10*data_cd0$FATALITIES + data_cd0$INJURIES
```

The last step of processing the data is to find out what the total dollar amounts of property damage and crop damage are, as well as an overall total.  This will require a bit of care because the amount of damage is found in one column as the base value, and in the adjacent column as an exponent.  Luckily, at this point, the only values left in the exponents column are "K", "M", and "B", whereas before we had done some subsetting, there were many nonstandard values.


```r
## To have a look at what type of exponents we are dealing with, look at: table(data_cd0$PROPDMGEXP) and table(data_cd0$CROPDMGEXP).
## It was seen at the time of this analysis that the only values were blanks, "K"'s, "M"'s, and "B"'s.
## The following are two for loops to find the total amount of property damage per event.
## ******* These for loops take about 3 minutes each, they should be optimized somehow ****************
for (i in 1:length(data_cd0$PROPDMG)) {
  if (data_cd0$PROPDMGEXP[i] == "K") {
    data_cd0$PROPDMG_TOTAL[i] <- data_cd0$PROPDMG[i]*10^3
  }
  else if (data_cd0$PROPDMGEXP[i] == "M") {
    data_cd0$PROPDMG_TOTAL[i] <- data_cd0$PROPDMG[i]*10^6
  }
  else if (data_cd0$PROPDMGEXP[i] == "B") {
    data_cd0$PROPDMG_TOTAL[i] <- data_cd0$PROPDMG[i]*10^9
  }
  else data_cd0$PROPDMG_TOTAL[i] <- 0
}

## For loop to find the total amount of crop damage per event.
for (i in 1:length(data_cd0$CROPDMG)) {
  if (data_cd0$CROPDMGEXP[i] == "K") {
    data_cd0$CROPDMG_TOTAL[i] <- data_cd0$CROPDMG[i]*10^3
  }
  else if (data_cd0$CROPDMGEXP[i] == "M") {
    data_cd0$CROPDMG_TOTAL[i] <- data_cd0$CROPDMG[i]*10^6
  }
  else if (data_cd0$CROPDMGEXP[i] == "B") {
    data_cd0$CROPDMG_TOTAL[i] <- data_cd0$CROPDMG[i]*10^9
  }
  else data_cd0$CROPDMG_TOTAL[i] <- 0
}

## Now lets find the total monetary damage, giving equal weight to property and crop damage.
data_cd0$TOTALDMG <- data_cd0$CROPDMG_TOTAL + data_cd0$PROPDMG_TOTAL
```


```r
## One last step, let's create a data table from our data frame.
library(data.table)
datatable <- data.table(data_cd0)
```

Now that we have totals for the amount of casualties that occured in each event, as well as how much economic damage occured in terms of property, crops, and in total, we can begin to pull some results from the data.

# Results

Again, our primary goal here is find out which weather events caused the most damage in terms of casualties, as well as in terms of economic damage.

Let us start by totalling the amount of monetary damage according to each event type, and ordering it from greatest to least.  Below are the top 10 most costly categories of event types.  Floods caused $149,539,674,950, hurricanes caused $71,913,712,800, and storm tides caused $47,835,729,000 in total damage to property and crops.


```r
total_by_EV <- datatable[,sum(TOTALDMG),by=EVTYPE_48]
head(total_by_EV[order(V1,decreasing=TRUE)],n=10)
```

```
##             EVTYPE_48        V1
##  1:             flood 1.495e+11
##  2: hurricane/typhoon 7.191e+10
##  3:        storm tide 4.784e+10
##  4:           tornado 2.490e+10
##  5:              hail 1.707e+10
##  6:       flash flood 1.671e+10
##  7:            seiche 1.456e+10
##  8:           drought 1.442e+10
##  9:         high wind 1.092e+10
## 10:          wildfire 8.508e+09
```

Here we have a bar plot of the top 5 most costly categories, in order to get a visual idea of how the compare to each other in relative terms.  Notice that not only does "flood" appear first, but it appears again as "flash flood" at the sixth position on the plot.  Floods in all forms are almost surely the most costly form of event type as a whole.


```r
plot <- head(total_by_EV[order(V1,decreasing=TRUE)],n=6)
## On the plot, I manually input the event types labels, since "hurricane/typhoon" was too long of a label to appear.
barplot(plot$V1, xlab="6 Most Costly Event Types",names.arg=c("flood","hurricane","storm tide","tornado","hail","flash flood"),ylab="Amount of Property and Crop Damage in Dollars",main="Plot of Monetary Damage by 6 Most Costly Event Types")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 

Now we do look at casualties in a similar manner.  Remember, the number found in the CASUALTIES column is a score, rather than a number that represents directly the amount of people that were hurt.  It is a score calculated by giving 1 point for each injury, and 10 points for each fatality. Here is a ranking of the top 10 most dangerous events in terms of their scores:



```r
total2_by_EV <- datatable[,sum(CASUALTIES),by=EVTYPE_48]
head(total2_by_EV[order(V1,decreasing=TRUE)],n=10)
```

```
##          EVTYPE_48    V1
##  1:        tornado 35777
##  2: excessive heat 24363
##  3:          flood 12948
##  4:      lightning 10663
##  5:    flash flood 10565
##  6:      high wind  9567
##  7:    rip current  5923
##  8:           heat  3686
##  9:   winter storm  3289
## 10:       wildfire  2796
```

We see that "tornado" is at the top of the list.  This is likely do to the fact that they arrive with little to no warning, while in the case of hurricanes, there is generally a lot of time to evacuate.  Though as we saw, hurricanes lead to a lot of property damage, more than tornados.  Now lets see a bar plot of these results:


```r
plot2 <- head(total2_by_EV[order(V1,decreasing=TRUE)],n=6)
## On the plot, I manually input the event types labels, since some labels were too long to appear.
barplot(plot2$V1, xlab="6 Most Dangerous Event Types (Injuries & Fatalities)",names.arg=c("tornado","ex. heat","flood","lightning","fl. flood","high wind"),ylab="Casualty Score (10*Fatality + Injury)",main="Plot of Casualty Score by top 6 Event Types")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 

One last interesting thing to look at is what sort of correlation there is between property/crop damage and damage to human life.  It would be reasonable to expect at least a weak positive correlation, since severe events do not discrimate in the damage they inflict.  Though as noted earlier, some events occur without warning, while some allow time to evacuate.  


```r
plot(total2_by_EV$V1,total_by_EV$V1,xlab="Casualty Score (0 - 12000)",ylab="Total Monetary Damage ($0 - $20B)",main="Monetary Damage by Casualty Score (For each event type)",plot.window(xlim=c(0,12000),ylim=c(0,20000000000)))
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17.png) 

Within the plot, since there are some extreme outliers in the data and I wanted to focus in on how the majority of the data was behaving, the x-axis and y-axis were adjusted to give a better view, and ignore the few outlying values.  As can be seen, there is a slight positive correlation though it is indeed weak.  This is likely due to the fact that even in this window, many event types are still being included that were not significant in terms of damage or casualties.  One new question that presents itself is what sort of events lead to high economic damage with low casualties, and which lead to high casualties with low economic damage.  As can be seen in the plot, there are a few events that hug the x-axis and y-axis quite tightly, even as their casualty scores or monetary damage numbers increase, respectively.

# Appendix

### Session Information

```r
## Returns information about the computing environment.
sessionInfo()
```

```
## R version 3.1.0 (2014-04-10)
## Platform: x86_64-apple-darwin13.1.0 (64-bit)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
## [1] data.table_1.9.2 stringdist_0.7.3 knitr_1.6       
## 
## loaded via a namespace (and not attached):
## [1] digest_0.6.4   evaluate_0.5.5 formatR_0.10   plyr_1.8.1    
## [5] Rcpp_0.11.1    reshape2_1.4   stringr_0.6.2  tools_3.1.0
```

### Data dictionary

This is to provide some idea of the purpose of each of the 37 columns, along with url links for further information.

https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf
https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf
http://www.ncdc.noaa.gov/stormevents/details.jsp
http://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf

1. "STATE__"      State number (1 = Alabama)
2. "BGN_DATE"     Begin date
3. "BGN_TIME"     Begin time
4. "TIME_ZONE"    Time zone
5. "COUNTY"       County number
6. "COUNTYNAME"   County name
7. "STATE"        State, 2 letter abbreviation
8. "EVTYPE"       Type of event (storm, flood, etc)
9. "BGN_RANGE"    Beginning range
10. "BGN_AZI"     Beginning azimuth
11. "BGN_LOCATI"  Beginning location
12. "END_DATE"    End date
13. "END_TIME"    End time
14. "COUNTY_END"  County where event ended? (name or number?)
15. "COUNTYENDN"  County where event ended? (name or number?)
16. "END_RANGE"   Ending range
17. "END_AZI"     Ending azimuth
18. "END_LOCATI"  Ending location
19. "LENGTH"      Length of tornado path (in yards?)
20. "WIDTH"       Maximum width of tornado's path in yards
21. "F"           Fujita tornado intensity scale
22. "MAG"         Hail in inches (implied hundreths)
23. "FATALITIES"  Number of fatalities
24. "INJURIES"    Number of injuries
25. "PROPDMG"     Property damage in dollars
26. "PROPDMGEXP"  K=thousands, M=millions, B=billions
27. "CROPDMG"     Crop damage in dollars
28. "CROPDMGEXP"  H=hundreds, K=thousands, M=millions, B=billions
29. "WFO"         Weather Forecast Office
30. "STATEOFFIC"  State office?
31. "ZONENAMES"   Zone names?
32. "LATITUDE"    Latitude
33. "LONGITUDE"   Longitude
34. "LATITUDE_E"  ?
35. "LONGITUDE_"  ?
36. "REMARKS"     Remarks
37. "REFNUM"      Reference number?
