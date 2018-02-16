#read in csv files and normalize column names
Beers <- read.csv("Beers.csv", sep = ",")
colnames(Beers) <- c("Name", "Beer_id", "ABV", "IBU", "Brewery_id", "Style", "Ounces")

Breweries <- read.csv("Breweries.csv", sep = ",")
colnames(Breweries) <- c("Brewery_id", "Name", "City", "State")

# a. How many breweries in each state?
summary(Breweries)
table(Breweries$State)

# b. Merge beer data with the breweries data. Print the first 6 observations and the last six observations to check the merged file
brewDataMerged <- merge(Beers, Breweries, by = "Brewery_id", all = TRUE)
head(brewDataMerged, 6)
tail(brewDataMerged, 6)

# c. Report the number of NA's in Each Column
sapply(brewDataMerged, function(x) sum(length(which(is.na(x)))))

# d. Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare.



