library(dplyr)
library(ggplot2)

#read in csv files and normalize column names
Beers <- read.csv("Beers.csv", sep = ",")
colnames(Beers) <- c("Name", "Beer_id", "ABV", "IBU", "Brewery_id", "Style", "Ounces")

Breweries <- read.csv("Breweries.csv", sep = ",")
colnames(Breweries) <- c("Brewery_id", "Name", "City", "State")

#cleanup
namestotest <- Breweries$Name

duptable = data.frame(Index=as.numeric(),Name=as.character())

for (i in 1:length(namestotest)){
  # Find potential duplicates
  dup = agrep(namestotest[i],namestotest[-i],ignore.case = T, value = T, max.distance = .1, useBytes=FALSE)
  name = ifelse(length(dup)>0,dup,"OK")
  dupoccurance = data.frame(Index=i,Name = name)
  duptable = rbind(duptable,dupoccurance)
}

# Remove rows that are "OK"
duptable=subset(duptable,duptable$Name!="OK")
duptable = duptable[order(duptable$Name),] # Sort to find true duplicates
head(duptable)

# a. How many breweries in each state?
summary(Breweries)
table(Breweries$State)

# b. Merge beer data with the breweries data. Print the first 6 observations and the last six observations to check the merged file
brewDataMerged <- merge(Beers, Breweries, by = "Brewery_id", all = TRUE)
colnames(brewDataMerged) <- c("Brewery_id", "Beer_name", "Beer_id", "ABV", "IBU", "Style", "Ounces", "Brewery_name", "City", "State")
head(brewDataMerged, 6)
tail(brewDataMerged, 6)

# c. Report the number of NA's in Each Column
sapply(brewDataMerged, function(x) sum(length(which(is.na(x)))))

# d. Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare.
stateGrouped <- group_by(brewDataMerged, State)
summarizedGroup <- summarize(stateGrouped, Median_alcohol = median(ABV, na.rm = TRUE), Median_IBU = median(IBU, na.rm = TRUE))
ggplot(data = summarizedGroup) + geom_bar(mapping = aes(x= State, y= Median_alcohol), stat="identity")
ggplot(data = summarizedGroup) + geom_bar(mapping = aes(x= State, y= Median_IBU), stat="identity")

# e.
#state with most alcoholic beer?
brewDataMerged[which.max(brewDataMerged$ABV),]

#state witb most bitter beer
brewDataMerged[which.max(brewDataMerged$IBU),]

#f. summary for ABV statistic
summary(brewDataMerged$ABV)

#g. relationship between the bitterness of the beer and its alcoholic content? Draw a scatter plot.
ggplot(brewDataMerged, aes(IBU, ABV))+ 
  ggtitle("International Bitterness Units (IBU) vs. Alcohol Content (ABV)") +
  geom_point(color="green") + 
  geom_smooth(method=lm,  se=FALSE, color="blue")
                                                      


