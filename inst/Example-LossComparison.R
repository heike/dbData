load("/home/susan/Dropbox/dbaccess/dbData/data/7-23-2012.RData")

connect <- dbConnect(dbDriver("MySQL"), user="2009Expo", 
password="R R0cks", port=3306, dbname="baseball", 
host="headnode.stat.iastate.edu")
pitch <- new("dataDB", co=connect, table="Pitching")
d1 <- dbData(pitch, vars=c( "G", "SO")) # Games, Strike Outs
#	qplot(G,SO, fill=log10(Freq), data=d1, geom="tile")+scale_fill_gradient2()
#	lossRandom(d1, binning=c(1,5))
#	d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(1,5))
#	qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+scale_fill_gradient2()
#	lossRandom(d1, binning=c(1,10))
#	lossRandom(d2, binning=c(1,2))
#	d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(1,10))
#	qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+scale_fill_gradient2()
## some more exploration of loss
bins <- expand.grid(x=c(1:10), y=c(1:10))
losses3 <- mdply(bins, function(x,y) unlist(lossRandom(data=d1, binning=c(x, y))), .progress="text")
grid.arrange(qplot(x, percent.loss.G, group=y, data=losses3, geom="line"),qplot(x, percent.loss.SO, group=y, data=losses3, geom="line"), qplot(x, percent.loss.Freq, group=y, data=losses3, geom="line"))

connect <- dbConnect(dbDriver("MySQL"), user="2009Expo", 
password="R R0cks", port=3306, dbname="baseball", 
host="headnode.stat.iastate.edu")
pitch <- new("dataDB", co=connect, table="Pitching")
d1 <- dbData(pitch, vars=c( "G", "SO"))
#	qplot(G,SO, fill=log10(Freq), data=d1, geom="tile")+scale_fill_gradient2()
#	lossMean(d1, binning=c(2,5))
#	lossMean(d1, binning=c(1,5))
#	d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(1,5))
#	qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+scale_fill_gradient2()
#	lossMean(d1, binning=c(1,10))
#	lossMean(d2, binning=c(1,2))
#	d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(1,10))
#	qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+scale_fill_gradient2()
## some more exploration of loss
bins <- expand.grid(x=c(1:10), y=c(1:10))
losses2 <- mdply(bins, function(x,y) unlist(lossMean(data=d1, binning=c(x, y))), .progress="text")
grid.arrange(qplot(x, percent.loss.G, group=y, data=losses2, geom="line"),qplot(x, percent.loss.SO, group=y, data=losses2, geom="line"), qplot(x, percent.loss.Freq, group=y, data=losses2, geom="line"))

## Comparison of Random and Mean Functions ##
p <- arrangeGrob(
	qplot(x, percent.loss.G, group=y, data=losses3, geom="line", main="Random", xlab="Bin Width"), 
	qplot(x, percent.loss.G, group=y, data=losses2, geom="line", main = "Numerical Loss - Mean", xlab="Bin Width"),
	qplot(x, percent.visualLoss.G, group=y, data=losses2, geom="line", main = "Visual Loss - Mean", xlab="Bin Width"),
	qplot(x, percent.loss.SO, group=y, data=losses3, geom="line", main="Random", xlab="Bin Width"),
	qplot(x, percent.loss.SO, group=y, data=losses2, geom="line", main = "Numerical Loss - Mean", xlab="Bin Width"), 
	qplot(x, percent.visualLoss.SO, group=y, data=losses2, geom="line", main = "Visual Loss - Mean", xlab="Bin Width"),
	qplot(x, percent.loss.Freq, group=y, data=losses3, geom="line", main="Freq Loss - Random", xlab="Bin Width"),
	qplot(x, percent.loss.Freq, group=y, data=losses2, geom="line", main = "Freq Loss - Mean", xlab="Bin Width")
)
# ggsave(plot = p, "LossFunctionComparison.png", width=8, height=12)
p
png("PercentLossComparison.png", width=650, height=800, units="px")
p
dev.off()