library(dbData)
library(ggplot2)
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
bins <- expand.grid(x=c(1:10, seq(15, 100, 5)), y=c(1:10, seq(15, 100, 5)))

library(multicore)
lossesRdm <- do.call("rbind", mclapply(1:nrow(bins), function(i) lossCalc(data=d1, binning=c(bins[i,1], bins[i,2]))))
lossesStd <- do.call("rbind", mclapply(1:nrow(bins), function(i) lossCalc(data=d1, binning=c(bins[i,1], bins[i,2]), type="standard")))

lossesRdm <- cbind(bins, lossesRdm)
lossesStd <- cbind(bins, lossesStd)

losses <- rbind(cbind(type="random", lossesRdm), cbind(type="standard", lossesStd))
write.csv(losses, "/home/susan/Documents/R Projects/dbData/data/losses.csv", row.names=FALSE)


qplot(x=x, y=TotalLoss.G, group=interaction(y, type), data=losses, geom="line", colour=type)
qplot(x=y, y=TotalLoss.SO, group=interaction(x, type), data=losses, geom="line", colour=type)

lossfreqdiff <- ddply(losses, .(x,y), summarise, diff = TotalLoss.LogFreq[type=="random"]-TotalLoss.LogFreq[type=="standard"])
qplot(x=x, y=diff, group=y, data=lossfreqdiff, geom="line", colour=y) + 
  scale_colour_gradient(low="#51A7EA", high="#132B43", trans="log") 
# generally random has less frequency loss, but not always
                    
