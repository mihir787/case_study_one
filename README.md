# Case Study: Beer and Breweries

In the late 1970s the United States began a renaissance of craft brewing that would turn into a national phenomenon. In 2017 there was recorded a total of over 6,000 breweries and it is estimated that 83% of 21+ adults now live within 10 miles of a brewery. The industry was measured as contributing 67.8 billion dollars to the U.S. economy in 2016. The code below begins with a raw data dump of prominenet breweries and beers with the goal of analysis. As shown in the initial data exploration, the data is not entirely clean. However, once the data has been appropriately cleaned meaningful insight can be gleaned.

Using the Beers.csv and Breweries.csv data sets.

1. How many breweries are present in each state?
2. Merge beer data with the breweries data. Print the first 6 observations and the last six observations to check the merged file.
3. Report the number of NA’s in each column.
4. Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare.
5. Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?
6. Summary statistics for the ABV variable.
7. Is there an apparent relationship between the bitterness of the beer and its alcoholic content? Draw a scatter plot. You are welcome to use the ggplot2 library for graphs. Please ignore missing values in your analysis. Make your best judgment of a relationship and EXPLAIN your answer.

#Description
The Beers dataset contains a list of 2410 US craft beers and Breweries dataset contains 558 US breweries. The datasets descriptions are as follows.

Beers.csv:
Name: Name of the beer.
Beer ID: Unique identifier of the beer.
ABV: Alcohol by volume of the beer.
IBU: International Bitterness Units of the beer. Brewery ID: Brewery id associated with the beer. Style: Style of the beer.
Ounces: Ounces of beer.

Breweries.csv:
Brew ID: Unique identifier of the brewery. Name: Name of the brewery.
City: City where the brewery is located.
State: U.S. State where the brewery is located.
