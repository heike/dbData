library(ggplot2)
library(gridExtra)
library(dbData)
library(multicore)
library(devtools)
library(xtable)

connect <- dbConnect(dbDriver("MySQL"), user="2009Expo", 
                     password="R R0cks", port=3306, dbname="baseball", 
                     host="headnode.stat.iastate.edu")
pitch <- new("dataDB", co=connect, table="Pitching")

d1 <- dbData(pitch, vars=c( "G", "SO")) # Games, Strike Outs


LossTable <- NULL

bins <- rbind(c(1,1), c(2,2), c(2,5), c(5, 2), c(4,4), c(3, 5), c(5, 3), c(5, 5), c(5, 25), c(25, 5), c(2, 50), c(50, 2), c(5, 50))

setwd("/home/susan/Dropbox/GraphicsGroup/dbaccess/NewWriteUp")
computeLoss <- function(binning, data){
  lRandom <- lossRandom(data, binning=binning, newData = TRUE)
  lMean <- lossMean(data, binning=binning, newData=TRUE)
  qplot(G, SO, fill = log10(Freq), geom="tile", data = lRandom$NewData, 
        main=paste("Random Binning (", binning[1], ", ", binning[2],")", sep="")) + 
        scale_fill_gradient2()
  ggsave(paste("Random", binning[1], binning[2], ".png", sep=""), width=5, height=4.5, units="in")
  qplot(G, SO, fill = log10(Freq), geom="tile", data = lMean$NewData, 
        main=paste("Standard Binning (", binning[1], ", ", binning[2],")", sep="")) + 
          scale_fill_gradient2()
  ggsave(paste("Standard", binning[1], binning[2], ".png", sep=""), width=5, height=4.5, units="in")
  return(rbind(cbind(binX = binning[1], binY = binning[2], Function = "Random", lRandom$percent.loss), 
                            cbind(binX = binning[1], binY = binning[2], Function = "Standard", lMean$percent.loss)))
}

LossTable <- do.call("rbind", mclapply(1:nrow(bins), function(i) computeLoss(bins[i,], d1)))


LossTable <- as.data.frame(LossTable)
names(LossTable) <- c("binX", "binY", "Function", "G", "SO", "Freq")
LossTable[order(LossTable[,3], LossTable[,1]),]

Diff <- LossTable[LossTable$Function == "Random", 4:6] - LossTable[LossTable$Function == "Standard",4:6]
Diff

colMeans(Diff)

tab <- xtable(LossTable[order(LossTable[,3], LossTable[,1]),], digits=c(0, 0, 0, 0,8, 8, 8))

print(tab, include.rownames=FALSE)

tab <- xtable(LossTable[order(LossTable[,3], LossTable[,2]),], digits=c(0, 0, 0, 0,8, 8, 8))

print(tab, include.rownames=FALSE)
  ggsave(paste("Random", binning[1], binning[2], ".png", sep=""))

