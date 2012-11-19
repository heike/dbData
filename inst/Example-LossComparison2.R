connect <- dbConnect(dbDriver("MySQL"), user="2009Expo", 
                     password="R R0cks", port=3306, dbname="baseball", 
                     host="headnode.stat.iastate.edu")
pitch <- new("dataDB", co=connect, table="Pitching")
d1 <- dbData(pitch, vars=c( "G", "SO"))
qplot(G,SO, fill=log10(Freq), data=d1, geom="tile")+
  scale_fill_gradient2()
lossCalc(d1, binning=c(2,5))
lossCalc(d1, binning=c(2,5), type="standard")
lossCalc(d1, binning=c(1,5))
lossCalc(d1, binning=c(1,5), type="standard")
d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(1,5))
qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+
  scale_fill_gradient2()
lossCalc(d1, binning=c(1,10))
lossCalc(d2, binning=c(1,2))
d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(1,10))
qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+
  scale_fill_gradient2()
## some more exploration of loss
bins <- expand.grid(x=2*c(1:4), y=2*c(1:4)-1)

lossesRdm <- mdply(bins, function(x,y) lossCalc(data=d1, binning=c(x, y)))
lossesRdm <- mdply(bins, function(x,y) lossCalc(data=d1, binning=c(x, y), type="standard"))
lossesRdm <- mclapply(1:nrow(bins), function(i) lossCalc(data=d1, binning=c(bins[i,1], bins[i,2])))
lossesStd <- mclapply(1:nrow(bins), function(i) lossCalc(data=d1, binning=c(bins[i,1], bins[i,2]), type="standard"))
qplot(x, percent.loss, group=y, data=losses, geom="line")
qplot(y, percent.loss, group=x, data=losses, geom="line")
d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(10,10))
                     