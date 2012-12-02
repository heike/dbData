#' Standard Binning Algorithm
#'
#' Bins data based on the Standard binning algorithm, that is, points are allocated to the closest bin with weight 1.
#' 
#' @param d1 data frame
#' @param binning vector of binwidths
#' @param newData binned data available?
#' @export
#' @examples
#' connect <- dbConnect(dbDriver("MySQL"), user="2009Expo", 
#' password="R R0cks", port=3306, dbname="baseball", 
#' host="headnode.stat.iastate.edu")
#' pitch <- new("dataDB", co=connect, table="Pitching")
#' d1 <- dbData(pitch, vars=c( "G", "SO"))
#' qplot(G,SO, fill=log10(Freq), data=d1, geom="tile")+scale_fill_gradient2()
#' d1binned <- binStd(d1, binning=c(2,5))
#' d1binned.reduced <- ddply(d1binned, .(G, SO), summarise, Freq=sum(Freq))
#' qplot(G,SO, fill=log10(Freq), data=d1binned.reduced, geom="tile")+scale_fill_gradient2()
#' d1binned <- binStd(d1, binning=c(1,5))
#' d1binned.reduced <- ddply(d1binned, .(G, SO), summarise, Freq=sum(Freq))
#' qplot(G,SO, fill=log10(Freq), data=d1binned.reduced, geom="tile")+scale_fill_gradient2()
binStd <- function(data, binning){
  dnew <- data
  nl <- ncol(data)
  if(length(binning) != nl)
    binning <- rep(binning, length=nl-1)
  for(x in 1:(nl-1)){
    dnew[,x] <- binning[x]*round(data[,x]/binning[x],0)
  }
  return(dnew)
}

#' Random Binning Algorithm
#'
#' Bins data based on the Random binning algorithm, that is, points are allocated to the closest bin using a bernoulli trial with p inversely proportional to the distance from the point to the bin.
#' 
#' @param d1 data frame
#' @param binning vector of binwidths
#' @param newData binned data available?
#' @export
#' @examples
#' connect <- dbConnect(dbDriver("MySQL"), user="2009Expo", 
#' password="R R0cks", port=3306, dbname="baseball", 
#' host="headnode.stat.iastate.edu")
#' pitch <- new("dataDB", co=connect, table="Pitching")
#' d1 <- dbData(pitch, vars=c( "G", "SO"))
#' qplot(G,SO, fill=log10(Freq), data=d1, geom="tile")+scale_fill_gradient2()
#' d1binned <- binRdm(d1, binning=c(2,5))
#' d1binned.reduced <- ddply(d1binned, .(G, SO), summarise, Freq=sum(Freq))
#' qplot(G,SO, fill=log10(Freq), data=d1binned.reduced, geom="tile")+scale_fill_gradient2()
#' d1binned <- binRdm(d1, binning=c(1,5))
#' d1binned.reduced <- ddply(d1binned, .(G, SO), summarise, Freq=sum(Freq))
#' qplot(G,SO, fill=log10(Freq), data=d1binned.reduced, geom="tile")+scale_fill_gradient2()
binRdm <- function(data, binning){
    dnew <- data
    nl <- ncol(data)
    if(length(binning) != nl)
      binning <- rep(binning, length=nl-1)
    for(x in 1:(nl-1)){
      dnew[,x] <- binning[x]*(floor(data[,x]/binning[x]) + 
                                sapply((data[,x]%%binning[x])/binning[x], 
                                       function(p) rbinom(1, 1, p)))
    }
    return(dnew)
}

#' Partitioned Loss function
#'
#' Based on a given data set, loss is computed from comparing the original data to the data binned at the specified level using either the Random (default) or Standard binning algorithm.
#' Loss is computed as sum of squared differences between all values at the original data and the mean of the data in the new bin.
#' Percent loss compares loss to the total sum of squares in the original data.
#' 
#' @param d1 data frame
#' @param binning vector of binwidths
#' @export
#' @examples
#' connect <- dbConnect(dbDriver("MySQL"), user="2009Expo", 
#' password="R R0cks", port=3306, dbname="baseball", 
#' host="headnode.stat.iastate.edu")
#' pitch <- new("dataDB", co=connect, table="Pitching")
#' d1 <- dbData(pitch, vars=c( "G", "SO"))
#' qplot(G,SO, fill=log10(Freq), data=d1, geom="tile")+scale_fill_gradient2()
#' lossCalc(d1, binning=c(2,5))
#' lossCalc(d1, binning=c(2,5), type="standard")
#' lossCalc(d1, binning=c(1,5))
#' lossCalc(d1, binning=c(1,5), type="standard")
#' d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(1,5))
#' qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+scale_fill_gradient2()
#' lossCalc(d1, binning=c(1,10))
#' lossCalc(d2, binning=c(1,2))
#' d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(1,10))
#' qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+scale_fill_gradient2()
#' ## some more exploration of loss
#' bins <- expand.grid(x=c(1:10), y=c(1:10))
#' losses <- mdply(bins, function(x,y) lossCalc(data=d1, binning=c(x, y)))
#' qplot(x, percent.loss, group=y, data=losses, geom="line")
#' qplot(y, percent.loss, group=x, data=losses, geom="line")
#' d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(10,10))
#' 
lossCalc <- function(data, binning, type="standard", newData=FALSE){
#   browser()
  if(sum(grepl("freq", tolower(names(data)), fixed=TRUE))==0) data$Freq <- 1
  nl <- ncol(data)
  
  min.bin <- sapply(1:(nl-1), function(i){
    diffs <- diff(sort(data[,i]))
    diffs <- subset(diffs, diffs>0)
    min(diffs)
  })
  numMinimalBins <- prod(sapply(1:(nl-1), function(i) diff(range(data[,i]))/min.bin[i]))
  
  data.old <- data
  data <- ddply(data, c(1:(nl-1)), summarise, Freq=sum(Freq))
  
  if(type=="random"){dnew <- binRdm(data, binning)
  }else if(type=="standard"){dnew <- binStd(data, binning)
  }else{
    warning("Type not 'standard' or 'random' - proceeding with standard binning")
    dnew <- binStd(data, binning)
  }
  
  if(type=="random"){dmin <- binRdm(data, min.bin)
  }else if(type=="standard"){dmin <- binStd(data, min.bin)
  }else{
    warning("Type not 'standard' or 'random' - proceeding with standard binning")
    dmin <- binStd(data, min.bin)
  }
  
  dmin <- ddply(dmin, c(1:(nl-1)), Freq=sum(Freq))

  
  vars <- data*0
  names(vars) <- paste("Var", names(data), sep="")

  data$id <- dnew$id <- interaction(dnew[,1:(nl-1)], sep=".split.")
  total.loss <- as.data.frame(data$Freq*(data[,1:(nl-1)]-dnew[,1:(nl-1)])^2)
  names(total.loss) <- names(data)[1:(nl-1)]
  total.loss$id <- data$id
  
  
  mfun <- function(v){
    nl <- ncol(v)-1
    # Calculate EX
    t <- data.frame(t(sapply(1:(nl-1), function(i) sum(v[,i]*v[,nl])/sum(v[,nl])))) 
    # t <- data.frame(t(sapply(1:(nl-1), function(i) weighted.mean(v[,i], v[,nl]))))
    t$fsum <- sum(v[,nl])
    # Calculate EX^2
    tsq <- data.frame(t(sapply(1:(nl-1), function(i) sum(v[,i]^2*v[,nl])/sum(v[,nl])))) 
    # tsq <- data.frame(t(sapply(1:(nl-1), function(i) weighted.mean(v[,i]^2, v[,nl]))))
    tsq <- tsq - t^2 # VarX = EX^2 - (EX)^2
    tsq <- tsq * t$fsum # Get SSQ from Freq
    names(t) <- c(names(v)[-c(nl, nl+1)], "fsum")
    t$id <- unique(v$id)
    t$n <- length(v$id)
    names(tsq) <- paste(names(t)[1:(nl-1)], "Var", sep="")
    t <- cbind(t, tsq)
    return(t)
  }
  
  res <- ddply(data, .(id), mfun)
  dnew2 <- merge(res, dnew[,-c(1:(nl-1))], by="id")

  TotalLoss <- colSums(total.loss[,1:(nl-1)])
  
  emptybinstop <- prod(binning)/prod(min.bin)
  NumLoss <- c(colSums(res[,(ncol(res)-(nl-2)):ncol(res)]), sum(
      (log(dnew2$Freq+1) - log((dnew2$fsum+1)/emptybinstop))^2) + sum(
      (emptybinstop-dnew2$n)*(log(dnew2$fsum+1)/emptybinstop)^2
    ))
  names(NumLoss) <- paste(c("", "", "Log"), names(data[,-(nl+1)]), sep="")
#   rm(list="dnew2")
  TotalLoss <- c(TotalLoss, NumLoss[nl])
  NumLoss <- NumLoss[1:(nl-1)]
  
  VisLoss <- TotalLoss[1:(nl-1)] - NumLoss
  
  nbins <- prod(sapply(1:(nl-1), function(i) ceiling(diff(range(data[,i]))+1)/binning[i]))*emptybinstop
  numEmptyBins <- nbins-nrow(dmin)
  TSS <- as.data.frame(t(c(
    sapply(1:(nl-1), function(i) 
      sum(data$Freq*(data[,i] - weighted.mean(data[,i], data[,nl]))^2)), 
    sum((log(dmin$Freq+1)-log(sum(dmin$Freq)/nbins))^2)+NumEmptyBins*(log(sum(dmin$Freq)/nbins))^2)))
  names(TSS) <- paste(c("", "", "Log"), names(data[,-(nl+1)]), sep="")
  
  if(newData){
    dnew3 <- as.data.frame(cbind(do.call("rbind", lapply(strsplit(as.character(res$id), ".split.", fixed=TRUE), as.numeric)), res$fsum))
    names(dnew3) <- names(data[,1:(nl)])
  }

  LossAll <- data.frame(c(NumLoss/TSS[1:2], VisLoss/TSS[1:2], TotalLoss/TSS))
  names(LossAll) <- c(paste("NumLoss.", names(NumLoss), sep=""),
                      paste("VisLoss.", names(VisLoss), sep=""),
                      paste("TotalLoss.", names(TotalLoss), sep=""))
  if(newData)
    return(list(Loss=LossAll, NewData = dnew2[,c(2:(nl+1))]))
  else return(LossAll)
}
