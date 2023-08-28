load("skew.rdata")

# Examine data with QQ plots
dim(dat)
par(mfrow = c(3,3))
for (i in 1:9) {
  qqnorm(dat[,i],main=paste0("i=",i), ylim=c(-6,6))
  qqline(dat[,i])
}


# Examine columns 4 and 9 as they are skewed
hist(dat[,4], main = 'i = 4')
hist(dat[,9], main = 'i = 9')
