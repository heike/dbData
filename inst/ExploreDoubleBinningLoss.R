library(multicore)
library(devtools)
library(ggplot2bayes)

bin.once <-  lossRandom(d1, binning=c(2,2), newData = TRUE)
bin.twice <- lossRandom(bin.once$NewData, binning=c(4,4), newData=TRUE)
bin.once$percent.loss
bin.twice$percent.loss

bin.once2 <- lossRandom(d1, binning=c(2, 2), newData=TRUE)
bin.twice2 <- lossRandom(d1, binning=c(4, 4), newData=TRUE)
bin.once2$percent.loss
bin.twice2$percent.loss

bin.orig <- lossRandom(d1, binning=c(4,4), newData=TRUE)

compare <- merge(bin.twice$NewData[order(bin.twice$NewData[,1], bin.twice$NewData[,2]),], 
                 bin.twice2$NewData[order(bin.twice2$NewData[,1], bin.twice2$NewData[,2]),], 
                 by = c("G", "SO"))
names(compare) <- c("G", "SO", "Freq.1", "Freq.2")
compare <- merge(bin.orig$NewData[order(bin.orig$NewData[,1], bin.orig$NewData[,2]),],
                 compare, by=c("G", "SO"))

plotcompare <- data.frame(rbind(cbind(bin.twice$NewData, p='Double Bin (2x2, 4x4)(a)'), 
                     cbind(bin.twice2$NewData, p='Double Bin (2x2, 4x4)(b)'), 
                     cbind(bin.orig$NewData, p='Single Bin')))
ggplot(data=plotcompare, aes(x=G, y=SO, fill=log10(Freq), facets=p)) + geom_tile() + facet_wrap(~p)

rbind(c("Twice", bin.twice$percent.loss), c("Twice", bin.twice2$percent.loss), c("Once", bin.orig$percent.loss))


N <- 100
temp <- mclapply(1:N, function(i) {lossRandom(d1, binning=c(2,2), newData=TRUE)})
temp2 <- mclapply(1:N, function(i) {lossRandom(temp[[i]]$NewData, binning=c(4,4), newData=FALSE)})
temp3 <- mclapply(1:N, function(i) {lossRandom(temp[[1]]$NewData, binning=c(4,4), newData=FALSE)})


pl <- NULL
for(i in 1:N){
  pl <- rbind(pl, temp3[[i]]$percent.loss)
}
pl2 <- rbind(data.frame(x=pl$G, var="G"), data.frame(x=pl$SO, var="SO"), data.frame(x=pl$Freq, var="Freq"))
qplot(data=pl2, x=x, geom="density") + facet_wrap(~var, scales="free")


# Conclusion: Repeating binning doesn't produce homogeneous loss for the random function,
#            unless the 1st random binning is held constant.