
library(UsingR)
x <- father.son$fheight

# The simplest way to think of a distribution is as a compact description of many numbers. 
hist(x) # This summarizes the data.

#----------------------------------
library(gapminder)
library(dplyr)
data(gapminder)
head(gapminder)

# Create a vector x of the life expectancies of each country for the year 1952. 
# Plot a histogram of these life expectancies to see the spread of the different countries.

par(mfrow = c(2,2))
year <- filter(gapminder, year==1952)

x <- vector("numeric")
for (c in unique(gapminder$country)) {
  by_country <- filter(year,country==c) %>% dplyr::select(lifeExp) %>% unlist
  x <- c(x, by_country)
}
hist(x,main='Life expectancy ')

mean(x <= 40)

prop = function(q) {
  mean(x <= q)
}
prop(40)

qs = seq(from=min(x), to=max(x), length=20)
props = sapply(qs, prop)
props = sapply(qs, function(q) mean(x <= q))
plot(props)
plot(ecdf(x))
