#---------------------------------------

# Boxplots
head(InsectSprays)

# Two ways to do the same boxplots
boxplot(split(InsectSprays$count, InsectSprays$spray))
boxplot(InsectSprays$count ~ InsectSprays$spray)

#------------------------------------
library(dplyr)
data(nym.2002, package="UsingR")
head(nym.2002)

boxplot(nym.2002$time ~ nym.2002$gender)

# Examine data with QQ plots

males <- nym.2002[nym.2002$gender == 'Male',]
females <- nym.2002[nym.2002$gender == 'Female',]
qqnorm(males$time, main='Males')
qqline(males$time)

qqnorm(females$time, main='Females')
qqline(females$time)

print ('Mean males')
print (mean(males$time))

print ('Mean Females')
print (mean(females$time))
 