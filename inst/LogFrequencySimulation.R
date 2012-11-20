library(dbData)
cdfbreaks <- function(df){
  df <- df[order(df$type, df$breaks),]
  maxbreaks <- max(df$breaks)
  df2 <- ddply(df, .(type), function(x) 
                              data.frame(breaks=c(x$breaks[-1]-.000001, maxbreaks), 
                                         sum=x$sum,
                                         type=x$type))
  df2 <- rbind(df, df2)
  df2 <- df2[order(df2$type, df2$breaks),]
  return(df2)
}

sim.breaks <- function(data, nbreaks=6, binfcn=binStd, binning = c(1, 1)){
  dbin <- ddply(binfcn(data, c(1, 1)), .(x,y), summarize,  Freq=sum(Freq))
  breaks.linear <- hist(dbin$Freq, breaks=nbreaks, plot=FALSE)$breaks
  breaks.log <- exp(hist(log(dbin$Freq), breaks=nbreaks, plot=FALSE)$breaks)
  breaks.full <- c(0, unique(sort(dbin$Freq)))
  cdf.breaks <- 
    rbind(data.frame(breaks = breaks.linear, 
                     sum = colSums(sapply(breaks.linear, function(i) 
                       dbin$Freq<i)), 
                     type="linear"),
          data.frame(breaks = breaks.log, 
                     sum = colSums(sapply(breaks.log, function(i)
                       dbin$Freq<i)),
                     type="log"), 
          data.frame(breaks = breaks.full,
                     sum = colSums(sapply(breaks.full, function(i)
                       dbin$Freq<i)),
                     type="actual"))
  
#   qplot(data=data, x=x, y=y, geom="point")
#   qplot(data=dbin, x=x, y=y, fill=Freq, geom="tile")+ 
#     scale_fill_gradient(low="#51A7EA", high="#132B43") 
#   qplot(data=dbin, x=x, y=y, fill=Freq, geom="tile")+ 
#     scale_fill_gradient(low="#51A7EA", high="#132B43", trans="log") 
#   qplot(dbin$Freq, geom="density")
#   qplot(log(dbin$Freq), geom="density")
  
  cdf.breaks <- cdfbreaks(cdf.breaks)
}

cdf.breaks.unif <- sim.breaks(data=data.frame(x=runif(10000, -8, 8), y=runif(10000, -8, 8), Freq=1))
qplot(data=cdf.breaks.unif, x=breaks, y=sum, geom="line", colour=type, group=type)


cdf.breaks.norm <- sim.breaks(data=data.frame(x=rnorm(10000, 0, 2), y=rnorm(10000, 0, 2), Freq=1))
qplot(data=cdf.breaks.norm, x=breaks, y=sum, geom="line", colour=type, group=type)

cdf.breaks.exp <- sim.breaks(data.frame(x=rexp(10000, 1/2), y=rexp(10000, 1/2), Freq=1))
qplot(data=cdf.breaks.exp, x=breaks, y=sum, geom="line", colour=type, group=type)

library(multicore)
expsim <- do.call("rbind", mclapply(1:500, function(i) cbind(sim.breaks(data.frame(x=rexp(10000, 1/2), y=rexp(10000, 1/2), Freq=1)), rep=i)))
qplot(data=expsim, x=breaks, y=sum, geom="line", group=interaction(type, rep), colour=type, alpha=I(.05)) + guides(colour = guide_legend(override.aes = list(alpha = 1))) + facet_grid(.~type)

normsim <- do.call("rbind", mclapply(1:500, function(i) cbind(sim.breaks(data=data.frame(x=rnorm(10000, 0, 2), y=rnorm(10000, 0, 2), Freq=1)), rep=i)))
qplot(data=normsim, x=breaks, y=sum, geom="line", group=interaction(type, rep), colour=type, alpha=I(.05)) + guides(colour = guide_legend(override.aes = list(alpha = 1))) + facet_grid(.~type)

unifsim <- do.call("rbind", mclapply(1:500, function(i) cbind(sim.breaks(data=data.frame(x=runif(10000, -8, 8), y=runif(10000, -8, 8), Freq=1)), rep=i)))
qplot(data=unifsim, x=breaks, y=sum, geom="line", group=interaction(type, rep), colour=type, alpha=I(.05)) + guides(colour = guide_legend(override.aes = list(alpha = 1))) + facet_grid(.~type)


d <- do.call("rbind", mclapply(1:100, function(i) data.frame(x=runif(10000, -8, 8), y=runif(10000, -8, 8), Freq=1, rep=i)))

