#' Random Loss function
#'
#' Based on a given data set, loss is computed from comparing the original data to the data binned at the specified level.
#' Loss is computed as sum of squared differences between all values at the original data and the binned data. 
#' Data that are "in beween" two bins are randomly assigned to one bin or the other by a bernoulli trial with success 
#' probability equal to the distance between the bin's center and the point divided by the binning level. 
#' Percent loss compares loss to the total sum of squares in the original data.
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
#' lossRandom(d1, binning=c(2,5))
#' lossRandom(d1, binning=c(1,5))
#' d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(1,5))
#' qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+scale_fill_gradient2()
#' lossRandom(d1, binning=c(1,10))
#' lossRandom(d2, binning=c(1,2))
#' d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(1,10))
#' qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+scale_fill_gradient2()
#' ## some more exploration of loss
#' bins <- expand.grid(x=c(1:10), y=c(1:10))
#' losses <- mdply(bins, function(x,y) lossRandom(data=d1, binning=c(x, y)))
#' qplot(x, percent.loss, group=y, data=losses, geom="line")
#' qplot(y, percent.loss, group=x, data=losses, geom="line")
#' d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(10,10))
#' 
lossRandom <- function(data, binning, newData=FALSE){
#	browser()
	dnew <- data
	vars <- data*0
	names(vars) <- paste("Var", names(data), sep="")
	nl <- ncol(data)
	if(length(binning) != nl)
		binning <- rep(binning, length=nl-1)
	for(x in 1:(nl-1)){
		dnew[,x] <- binning[x]*(floor(data[,x]/binning[x]) + 
      sapply((data[,x]%%binning[x])/binning[x], function(p) rbinom(1, 1, p)))
		vars[,x] <- data$Freq*(dnew[,x] - data[,x])^2
	}

	dnew$id <- interaction(dnew[,1:(nl-1)])
	res <- ddply(dnew, .(id), summarize, n=length(id), fsum=sum(Freq))
	
	dnew <- merge(dnew, res, by = "id")
	vars$VarFreq <- with(dnew, (log(Freq)-log(fsum)/n)^2)
	vars$id <- dnew$id

	vfun <- function(v){
		t <- sapply(1:ncol(v), function(i) is.numeric(v[,i]))
		return(colSums(v[t]))
	}
	var.res <- ddply(vars, .(id), vfun)
	dcond <- merge(res, var.res, by = "id")
	namesdcond <- names(dcond)
	dcond <- cbind(do.call("rbind", lapply(strsplit(as.character(dcond$id), ".", fixed=TRUE), as.numeric)), dcond[,-1])
	names(dcond) <- c(names(dnew)[2:(nl)], namesdcond[-1])

	Loss <-  colSums(dcond[,(nl+2):ncol(dcond)])
	names(Loss) <- paste(c("", "", "Log"), names(data[,-(nl+1)]), sep="")
	TSS <- as.data.frame(t(c(sapply(1:(nl-1), function(i) sum(data$Freq*(data[,i] - mean(data[,i]))^2)), sum((log(dnew$Freq)-mean(log(dnew$Freq)))^2))))
	names(TSS) <- names(data)
	retdata <- dcond[,c(1:(nl-1), (nl+1))]
	names(retdata) <- names(data)
	if(newData)
		return(list(loss = Loss, totalssq = TSS, percent.loss = Loss/TSS*100, NewData = retdata))
	else return(list(loss = Loss, totalssq = TSS, percent.loss = Loss/TSS*100))
}


#' Partitioned Standard Loss function
#'
#' Based on a given data set, loss is computed from comparing the original data to the data binned at the specified level.
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
#' lossMean(d1, binning=c(2,5))
#' lossMean(d1, binning=c(1,5))
#' d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(1,5))
#' qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+scale_fill_gradient2()
#' lossMean(d1, binning=c(1,10))
#' lossMean(d2, binning=c(1,2))
#' d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(1,10))
#' qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+scale_fill_gradient2()
#' ## some more exploration of loss
#' bins <- expand.grid(x=c(1:10), y=c(1:10))
#' losses <- mdply(bins, function(x,y) lossMean(data=d1, binning=c(x, y)))
#' qplot(x, percent.loss, group=y, data=losses, geom="line")
#' qplot(y, percent.loss, group=x, data=losses, geom="line")
#' d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(10,10))
#' 
lossMean <- function(data, binning, newData=FALSE){
	dnew <- data
	vars <- data*0
	names(vars) <- paste("Var", names(data), sep="")
	nl <- ncol(data)
	if(length(binning) != nl)
		binning <- rep(binning, length=nl-1)
	for(x in 1:(nl-1)){
		dnew[,x] <- binning[x]*round(data[,x]/binning[x],0)
	}
  
	data$id <- dnew$id <- interaction(dnew[,1:(nl-1)])
	vloss <- as.data.frame(data$Freq*(data[,1:(nl-1)]-dnew[,1:(nl-1)])^2)
	names(vloss) <- names(data)[1:(nl-1)]
	vloss$id <- data$id
	
	mfun <- function(v){
		nl <- ncol(v)-1
		t <- data.frame(t(sapply(1:(nl-1), function(i) sum(v[,i]*v[,nl])/sum(v[,nl])))) # Calculate EX
		t$fsum <- sum(v[,nl])
		tsq <- data.frame(t(sapply(1:(nl-1), function(i) sum(v[,i]^2*v[,nl])/sum(v[,nl])))) # Calculate EX^2
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
	dnew2 <- merge(res, dnew[-c(1:(nl-1))], by="id")
	
	Loss <- c(colSums(res[,(ncol(res)-(nl-2)):ncol(res)]), sum((log(dnew2$Freq) - log(dnew2$fsum)/dnew2$n)^2))
	names(Loss) <- paste(c("", "", "Log"), names(data[,-(nl+1)]), sep="")
	
	visLoss <- colSums(vloss[,1:(nl-1)])
	
	TSS <- as.data.frame(t(c(sapply(1:(nl-1), function(i) sum(data$Freq*(data[,i] - mean(data[,i]))^2)), sum((log(dnew$Freq)-mean(log(dnew$Freq)))^2))))
	names(TSS) <- names(data[,-(nl+1)])
	dnew3 <- as.data.frame(cbind(do.call("rbind", lapply(strsplit(as.character(res$id), ".", fixed=TRUE), as.numeric)), res$fsum))
	names(dnew3) <- names(data[,1:(nl)])
	if(newData)
		 return(list(math.loss = Loss, visualLoss = visLoss, loss = Loss + c(visLoss,0), totalssq = TSS, percent.mathLoss = Loss/TSS*100,  percent.visualLoss = 100*visLoss/TSS[1:(nl-1)],  percent.loss = (Loss + c(visLoss, 0))/TSS*100, NewData = dnew3))
	else return(list(math.loss = Loss, visualLoss = visLoss, loss = Loss + c(visLoss,0), totalssq = TSS, percent.mathLoss = Loss/TSS*100,  percent.visualLoss = 100*visLoss/TSS[1:(nl-1)],  percent.loss = (Loss + c(visLoss, 0))/TSS*100))
}

#' Standard Loss function (efficient, no partitioning)
#'
#' Based on a given data set, loss is computed from comparing the original data to the data binned at the specified level.
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
#' lossStandard(d1, binning=c(2,5))
#' lossStandard(d1, binning=c(1,5))
#' d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(1,5))
#' qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+scale_fill_gradient2()
#' lossStandard(d1, binning=c(1,10))
#' lossStandard(d2, binning=c(1,2))
#' d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(1,10))
#' qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+scale_fill_gradient2()
#' ## some more exploration of loss
#' bins <- expand.grid(x=c(1:10), y=c(1:10))
#' losses <- mdply(bins, function(x,y) lossStandard(data=d1, binning=c(x, y)))
#' qplot(x, percent.loss, group=y, data=losses, geom="line")
#' qplot(y, percent.loss, group=x, data=losses, geom="line")
#' d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(10,10))
#'
lossStandard <- function(data, binning, newData=FALSE){
  dnew <- data
  vars <- data*0
  names(vars) <- paste("Var", names(data), sep="")
  nl <- ncol(data)
  if(length(binning) != nl)
    binning <- rep(binning, length=nl-1)
  for(x in 1:(nl-1)){
    dnew[,x] <- binning[x]*round(data[,x]/binning[x],0)
    vars[,x] <- data$Freq*(dnew[,x] - data[,x])^2
  }
  
  dnew$id <- interaction(dnew[,1:(nl-1)])
  res <- ddply(dnew, .(id), summarize, n=length(id), fsum=sum(Freq))
  
  dnew <- merge(dnew, res, by = "id")
  vars$VarFreq <- with(dnew, (log(Freq)-log(fsum)/n)^2)
  vars$id <- dnew$id
  
  vfun <- function(v){
    t <- sapply(1:ncol(v), function(i) is.numeric(v[,i]))
    return(colSums(v[t]))
  }
  var.res <- ddply(vars, .(id), vfun)
  dcond <- merge(res, var.res, by = "id")
  namesdcond <- names(dcond)
  dcond <- cbind(do.call("rbind", lapply(strsplit(as.character(dcond$id), ".", fixed=TRUE), as.numeric)), dcond[,-1])
  names(dcond) <- c(names(dnew)[2:(nl)], namesdcond[-1])
  
  Loss <-  colSums(dcond[,(nl+2):ncol(dcond)])
  names(Loss) <- paste(c("", "", "Log"), names(data[,-(nl+1)]), sep="")
  TSS <- as.data.frame(t(c(sapply(1:(nl-1), function(i) sum(data$Freq*(data[,i] - mean(data[,i]))^2)), sum((log(dnew$Freq)-mean(log(dnew$Freq)))^2))))
  names(TSS) <- names(data)
  retdata <- dcond[,c(1:(nl-1), (nl+1))]
  names(retdata) <- names(data)
  if(newData)
    return(list(loss = Loss, totalssq = TSS, percent.loss = Loss/TSS*100, NewData = retdata))
  else return(list(loss = Loss, totalssq = TSS, percent.loss = Loss/TSS*100))
}


