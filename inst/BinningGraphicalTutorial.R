library(mvtnorm)
library(ggplot2)
setwd("/home/susan/Dropbox/GraphicsGroup/dbaccess/NewWriteUp")
set.seed(33)
d <- rmvnorm(200, c(5, 25), matrix(c(100, 50, 50, 100), nrow=2))
d <- data.frame(d)
names(d) <- c("x", "y")
d1 <- d
d$bin <- (d$x>=5)&(d$x<15)&(d$y>=15)&(d$y<=25)
d$a <- .25 + .75*d$bin
qplot(data=d, x=x, y=y, geom="point") # Data
ggsave("Binning-Data.png", width=5, height=4.5, units="in")
qplot(data=d, x=x, y=y, geom="point", alpha=a) + 
  geom_polygon(aes(x=c(5, 5, 15, 15), y=c(15, 25, 25, 15)), fill="blue", alpha=.25) + 
  scale_alpha_identity()
# Data + bin
ggsave("Binning-DataAndBin.png", width=5, height=4.5, units="in")

d$l <- "Data"
d$size <- 2
d <- rbind(d, data.frame(x=10, y=20, l="Visual Center", size=5, a=1, bin=TRUE), 
              data.frame(x=mean(d$x[d$bin]), y=mean(d$y[d$bin]), 
                         l="Numerical Center", size=5, a=1, bin=TRUE))


poly <- data.frame(x=c(5, 5, 15, 15), y=c(15, 25, 25, 15), l=NA)
qplot(data=poly, x=x, y=y, xlim=c(3, 17), ylim=c(13, 27), geom="polygon", fill=I("blue"), alpha=I(.25)) + 
  geom_point(data=d, aes(x=x, y=y, size = l, shape=l, colour=l, alpha=a)) + 
  scale_shape_manual(name="", values=c("Data" = 16, "Numerical Center" = 15, "Visual Center" = 18))+
  scale_colour_manual(name="", values=c("Data" = "black", "Numerical Center" = "yellow", "Visual Center" = "red"))+
  scale_size_manual(name="", values=c("Data" = 2, "Numerical Center" = 4, "Visual Center" = 4))+
  scale_alpha_identity()
ggsave("Binning-DataVisualNumericalCenter.png", width=7, height=4.5, units="in")

library(dbData)
nl <- ncol(d1)
binning <- c(10, 10)
for(x in 1:(nl-1)){
  dnew[,x] <- binning[x]*(floor(data[,x]/binning[x]) + 
    sapply((data[,x]%%binning[x])/binning[x], function(p) rbinom(1, 1, p)))
  vars[,x] <- data$Freq*(dnew[,x] - data[,x])^2
}

dnew$id <- interaction(dnew[,1:(nl-1)])
res <- ddply(dnew, .(id), summarize, n=length(id), fsum=sum(Freq))

qplot(data=d1, x=x, y=y, geom="point") + 