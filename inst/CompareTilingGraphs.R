library(ggplot2)
library(gridExtra)

load("/home/susan/Dropbox/dbaccess/dbData/data/7-23-2012.RData")

connect <- dbConnect(dbDriver("MySQL"), user="2009Expo", 
                     password="R R0cks", port=3306, dbname="baseball", 
                     host="headnode.stat.iastate.edu")
pitch <- new("dataDB", co=connect, table="Pitching")

d1 <- dbData(pitch, vars=c( "G", "SO")) # Games, Strike Outs


LossTable <- NULL

bins <- rbind(c(1,1), c(2,2), c(2,5), c(5, 2), c(4,4), c(3, 5), c(5, 3), c(5, 5), c(5, 25), c(25, 5), c(2, 50), c(50, 2), c(5, 50))

setwd("/home/susan/Dropbox/dbaccess/NewWriteUp")
computeLoss <- function(binning, data){
  lRandom <- lossRandom(data, binning=binning, newData = TRUE)
  lMean <- lossMean(data, binning=binning, newData=TRUE)
  qplot(G, SO, fill = log10(Freq), geom="tile", data = lRandom$NewData, 
        main=paste("Random Binning (", binning[1], ", ", binning[2],")", sep="")) + 
        scale_fill_gradient2()
  ggsave(paste("Random", binning[1], binning[2], ".png", sep=""))
}