library(ggplot2)
library(gridExtra)

load("/home/susan/Dropbox/dbaccess/dbData/data/7-23-2012.RData")

connect <- dbConnect(dbDriver("MySQL"), user="2009Expo", 
                     password="R R0cks", port=3306, dbname="baseball", 
                     host="headnode.stat.iastate.edu")
pitch <- new("dataDB", co=connect, table="Pitching")

d1 <- dbData(pitch, vars=c( "G", "SO")) # Games, Strike Outs


LossTable <- NULL

# loss.rand11 <-  lossRandom(d1, binning=c(1,1), newData = TRUE)
# loss.mean11 <-	lossMean(d1, binning=c(1,1), newData = TRUE)
tile11 <- arrangeGrob(
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.rand11$NewData, main="Random Binning (1, 1)")+scale_fill_gradient2(), 
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.mean11$NewData, main="Standard Binning (1, 1)")+scale_fill_gradient2()
)
tile11
png("Tiling11.png", width=550, height=440, units="px", pointsize=12)
tile11
dev.off()

LossTable <- rbind(LossTable, cbind(binX = 1, binY = 1, Function = "Random", loss.rand11$percent.loss))
LossTable <- rbind(LossTable, cbind(binX = 1, binY = 1, Function = "Mean", loss.mean11$percent.loss))

loss.rand22 <-  lossRandom(d1, binning=c(2,2), newData = TRUE)
loss.mean22 <-	lossMean(d1, binning=c(2,2), newData = TRUE)
tile22 <- arrangeGrob(
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.rand22$NewData, main="Random Binning  (2, 2)")+scale_fill_gradient2(), 
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.mean22$NewData, main="Standard Binning (2, 2)")+scale_fill_gradient2()
)
tile22 

png("Tiling22.png", width=550, height=440, units="px", pointsize=12)
tile22
dev.off()

LossTable <- rbind(LossTable, cbind(binX = 2, binY = 2, Function = "Random", loss.rand22$percent.loss))
LossTable <- rbind(LossTable, cbind(binX = 2, binY = 2, Function = "Mean", loss.mean22$percent.loss))

loss.rand25 <-	lossRandom(d1, binning=c(2,5), newData = TRUE)
loss.mean25 <-	lossMean(d1, binning=c(2,5), newData = TRUE)
tile25 <- arrangeGrob(
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.rand25$NewData, main="Random Binning  (2, 5)")+scale_fill_gradient2(), 
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.mean25$NewData, main="Standard Binning (2, 5)")+scale_fill_gradient2()
)
tile25 

png("Tiling25.png", width=550, height=440, units="px", pointsize=12)
tile25
dev.off()

LossTable <- rbind(LossTable, cbind(binX = 2, binY = 5, Function = "Random", loss.rand25$percent.loss))
LossTable <- rbind(LossTable, cbind(binX = 2, binY = 5, Function = "Mean", loss.mean25$percent.loss))

loss.rand52 <-  lossRandom(d1, binning=c(5,2), newData = TRUE)
loss.mean52 <-	lossMean(d1, binning=c(5,2), newData = TRUE)
tile52 <- arrangeGrob(
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.rand52$NewData, main="Random Binning  (5, 2)")+scale_fill_gradient2(), 
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.mean52$NewData, main="Standard Binning (5, 2)")+scale_fill_gradient2()
)
tile52 

png("Tiling52.png", width=550, height=440, units="px", pointsize=12)
tile52
dev.off()

LossTable <- rbind(LossTable, cbind(binX = 5, binY = 2, Function = "Random", loss.rand52$percent.loss))
LossTable <- rbind(LossTable, cbind(binX = 5, binY = 2, Function = "Mean", loss.mean52$percent.loss))

loss.rand44 <-  lossRandom(d1, binning=c(4,4), newData = TRUE)
loss.mean44 <-	lossMean(d1, binning=c(4,4), newData = TRUE)
tile44 <- arrangeGrob(
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.rand44$NewData, main="Random Binning  (4, 4)")+scale_fill_gradient2(), 
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.mean44$NewData, main="Standard Binning  (4, 4)")+scale_fill_gradient2()
)
tile44

png("Tiling44.png", width=550, height=440, units="px", pointsize=12)
tile44
dev.off()

loss.rand35 <-	lossRandom(d1, binning=c(3,5), newData = TRUE)
loss.mean35 <-	lossMean(d1, binning=c(3,5), newData = TRUE)
tile35 <- arrangeGrob(
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.rand35$NewData, main="Random Binning  (3, 5)")+scale_fill_gradient2(), 
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.mean35$NewData, main="Standard Binning  (3, 5)")+scale_fill_gradient2()
)
tile35

png("Tiling35.png", width=550, height=440, units="px", pointsize=12)
tile35
dev.off()

LossTable <- rbind(LossTable, cbind(binX = 3, binY = 5, Function = "Random", loss.rand35$percent.loss))
LossTable <- rbind(LossTable, cbind(binX = 3, binY = 5, Function = "Mean", loss.mean35$percent.loss))

loss.rand55 <-  lossRandom(d1, binning=c(5,5), newData = TRUE)
loss.mean55 <-	lossMean(d1, binning=c(5,5), newData = TRUE)
tile55 <- arrangeGrob(
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.rand55$NewData, main="Random Binning (5, 5)")+scale_fill_gradient2(), 
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.mean55$NewData, main="Standard Binning (5, 5)")+scale_fill_gradient2()
)
tile55

png("Tiling55.png", width=550, height=440, units="px", pointsize=12)
tile55
dev.off()

LossTable <- rbind(LossTable, cbind(binX = 5, binY = 5, Function = "Random", loss.rand55$percent.loss))
LossTable <- rbind(LossTable, cbind(binX = 5, binY = 5, Function = "Mean", loss.mean55$percent.loss))

loss.rand525 <-	lossRandom(d1, binning=c(5,25), newData = TRUE)
loss.mean525 <-	lossMean(d1, binning=c(5,25), newData = TRUE)
tile525 <- arrangeGrob(
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.rand525$NewData, main="Random Binning (5, 25)")+scale_fill_gradient2(), 
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.mean525$NewData, main="Standard Binning  (5, 25)")+scale_fill_gradient2()
)
tile525

png("Tiling525.png", width=550, height=440, units="px", pointsize=12)
tile525
dev.off()

LossTable <- rbind(LossTable, cbind(binX = 5, binY = 25, Function = "Random", loss.rand525$percent.loss))
LossTable <- rbind(LossTable, cbind(binX = 5, binY = 25, Function = "Mean", loss.mean525$percent.loss))


loss.rand250 <-  lossRandom(d1, binning=c(2,50), newData = TRUE)
loss.mean250 <-  lossMean(d1, binning=c(2,50), newData = TRUE)
tile250 <- arrangeGrob(
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.rand250$NewData, main="Random Binning (2, 50)")+scale_fill_gradient2(), 
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.mean250$NewData, main="Standard Binning (2, 50)")+scale_fill_gradient2()
)
tile250

png("Tiling250.png", width=550, height=440, units="px", pointsize=12)
tile250
dev.off()

LossTable <- rbind(LossTable, cbind(binX = 2, binY = 50, Function = "Random", loss.rand250$percent.loss))
LossTable <- rbind(LossTable, cbind(binX = 2, binY = 50, Function = "Mean", loss.mean250$percent.loss))


loss.rand550 <-  lossRandom(d1, binning=c(5,50), newData = TRUE)
loss.mean550 <-	lossMean(d1, binning=c(5,50), newData = TRUE)
tile550 <- arrangeGrob(
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.rand550$NewData, main="Random Binning (5, 50)")+scale_fill_gradient2(), 
  qplot(G, SO, fill = log10(Freq), geom="tile", data = loss.mean550$NewData, main="Standard Binning (5, 50)")+scale_fill_gradient2()
)
tile550

png("Tiling550.png", width=550, height=440, units="px", pointsize=12)
tile550
dev.off()

LossTable <- rbind(LossTable, cbind(binX = 5, binY = 50, Function = "Random", loss.rand550$percent.loss))
LossTable <- rbind(LossTable, cbind(binX = 5, binY = 50, Function = "Mean", loss.mean550$percent.loss))

LossTable <- as.data.frame(LossTable)
names(LossTable) <- c("binX", "binY", "Function", "G", "SO", "Freq")
LossTable[order(LossTable[,3], LossTable[,1]),]

Diff <- LossTable[LossTable$Function == "Random", 4:6] - LossTable[LossTable$Function == "Mean",4:6]
Diff

colMeans(Diff)

tab <- xtable(LossTable[order(LossTable[,3], LossTable[,1]),], digits=c(0, 0, 0, 0,8, 8, 8))

print(tab, include.rownames=FALSE)

tab <- xtable(LossTable[order(LossTable[,3], LossTable[,2]),], digits=c(0, 0, 0, 0,8, 8, 8))

print(tab, include.rownames=FALSE)
