#' union class to a database connection
#' 
#' A database connection can point to either a MySQL database or a Monet database.
#' @export
setClassUnion("dbConnection", c("MySQLConnection", "MonetDBConnection"))

#' Wrapper class to a database object
#'
#' @param co of class MySQLConnection, connection to a MySQL database 
#' @param table character string of table name
#' @export
#' @rdname dataDB-methods

setClass("dataDB", representation(co="dbConnection", 
                                  table="character"))

#' Method for dataDB object: retrieve first n rows from the data set
#'
#' @param x dataDB object
#' @param n number of rows shown 
#' @export
#' @rdname dataDB-methods

setMethod("head", "dataDB",
          function(x="dataDB", n = 6L, ...) {
            dbGetQuery(x@co, sprintf("select * from %s limit %d", x@table, n))
          }
)

#' Method for dataDB object: show names of table
#'
#' @param x dataDB object
#' @export
#' @rdname dataDB-methods

setMethod("names", "dataDB",
          function(x="dataDB") {
            dbListFields(x@co, x@table)
          }
)

#' Function to get sufficient statistics of variables from an SQL database
#'
#' @param data dataDB object
#' @param vars list of variable names 
#' @param binwidth vector of bin sizes for each variable. -1 for minimal binwidth 
#' @param where character string with conditional statement for SQL query
#' @export
#' @examples
#' connect <- dbConnect(dbDriver("MySQL"), user="dbaccess", 
#' port=3306, dbname="baseball", host="mysql2.stat.iastate.edu")
#' pitch <- new("dataDB", co=connect, table="Pitching")
#' names(pitch)
#' head(pitch, n=10)[,1:8]
#' pitch.stats <- dbData(vars=list("H", "SO"), pitch)
#' 
#' require(ggplot2)
#' qplot(H, SO, alpha=Freq, data=pitch.stats)
#' qplot(H, SO, fill=Freq, data=dbData(pitch, list("SO", "H"), 
#'   binwidth=c(10,50)), geom="tile")
#' qplot(H, SO, fill=Freq, data=dbData(pitch, list("SO", "H", "yearID"), 
#'   binwidth=c(10,50, -1)), facets=~yearID, geom="tile")
#' qplot(H, SO, fill=Freq, data=dbData(pitch, list("SO", "H", "yearID"), 
#'   binwidth=c(10,50, -1), where="yearID > 1990"), facets=~yearID, geom="tile")
#' 
## Not run:
## connect <- dbConnect(dbDriver("MySQL"), user="dbaccess", 
## port=3306, dbname="data_expo_2009", 
## host="mysql2.stat.iastate.edu")
## ontime <- new("dataDB", co=connect, table="ontime")
## qplot(DepTime, ArrDelay, data=dbData(ontime, list("DepTime","ArrDelay")))
## End(Not run)

dbData <- function(data, vars=list(), binwidth=-1, where = "") {
  varlist <- paste(unlist(vars), sep=",", collapse=",")
  
  if (all(binwidth==-1)) glist <- varlist
  else {
    binwidth <- rep(binwidth, length=length(vars))
    gvars <- unlist(vars)
    idx <- which(binwidth!=-1)    
    gvars[idx] <- sprintf("round(%s/%f, 0)",  gvars[idx], binwidth[idx])
    glist <- paste(gvars, sep=",", collapse=",")
    gvars[idx] <- sprintf("%f*round(%s/%f, 0) as %s", binwidth[idx], unlist(vars)[idx], binwidth[idx], unlist(vars)[idx])
    varlist <- paste(gvars, sep=",", collapse=",")
  }
  
  query <- sprintf("select %s, count(*) as Freq from %s __cond__ group by %s", 
                   varlist, data@table, glist)
  if (where != "") where <- sprintf("where %s", where)
  query <- gsub("__cond__", where, query)
  cat(query)
  dbGetQuery(data@co, query)
}

#' Determine number of distinct values of variables in a database
#'
#' For margin = TRUE the function return the number of distinct levels each variable has. For numeric variables this number will be identical to the number of rows in case there are no ties.
#' If margin = FALSE, the function returns the number of distinct tuples.
#' If binwidth is specified, the number of distinct tuples of the binned data is returned.
#' As the number of variables increases, the number of tuples will grow to at most the product of the marginal number of categories, but not beyond the number of records in the data table.
#' 
#' @param data dataDB object
#' @param vars list of variable names 
#' @param binwidth vector of bin sizes for each variable. -1 for minimal binwidth
#' @param margin logical value: should marginal distributions be considered or joint distribution?
#' @export
#' @examples
#' connect <- dbConnect(dbDriver("MySQL"), user="dbaccess", 
#' port=3306, dbname="baseball", host="mysql2.stat.iastate.edu")
#' pitch <- new("dataDB", co=connect, table="Pitching")
#' dbNBins(pitch, vars=list("G", "SO", "yearID"))
#' dbNBins(pitch, vars=list("G", "SO", "yearID"), binwidth=c(10,10,1))
#' dbNBins(pitch, vars=list("G", "SO", "yearID"), binwidth=c(10,10,1), margin=FALSE)
#' dbNBins(pitch, vars=list("yearID", "playerID"), margin=FALSE)

dbNBins <- function(data, vars=list(), binwidth=-1, margin=TRUE) {
  binwidth <- rep(binwidth, length=length(vars))
  
  gvars <- unlist(vars)
  idx <- which(binwidth!=-1)
  gvars[idx] <- sprintf("round(%s/%f, 0)",  gvars[idx], binwidth[idx])
  
  if (margin) {
    bins <- ldply(1:length(vars), function(x) {
      s <- vars[[x]]
      query <- sprintf("select count(distinct(%s)) as count, min(%s) as min, 
                     max(%s) as max from %s", gvars[x],s,s,data@table)
      dbGetQuery(data@co, query)
    })
    return(bins)
  } else {      
    ## distinct values of joint distribution
    
    gdistinct <- paste(gvars, sep=",", collapse=",")
    query <- sprintf("select count(distinct( concat(%s))) as count 
                         from %s", gdistinct,data@table)
      return(dbGetQuery(data@co, query))
  }
}

#' Loss function
#'
#' @param d1 data frame
#' @param d2 data frame
#' @export
#' @examples
#' connect <- dbConnect(dbDriver("MySQL"), user="dbaccess", 
#' port=3306, dbname="baseball", host="mysql2.stat.iastate.edu")
#' pitch <- new("dataDB", co=connect, table="Pitching")
#' d1 <- dbData(pitch, vars=c( "G", "SO"))
#' d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(2, 5))
#' qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+scale_fill_gradient2()
#' qplot(G,SO, fill=log10(Freq), data=d1, geom="tile")+scale_fill_gradient2()
#' loss(d1, d2)

loss <- function(d1, d2) {
  # assumptions:
  # both data sets have the same variables
  # the last variable is a count
  # the dataset with the higher number of rows is at the higher resolution
  stopifnot(length(setdiff(names(d1), names(d2))) == 0)
  
  resolution <- function (x) {
    x <- unique(as.numeric(x))
    if (length(x) == 1) return(x)
    min(diff(sort(x)))
  }
  
  if (nrow(d1) < nrow(d2)) {
    dlarge <- d2
    dsmall <- d1
  } else {
    dlarge <- d1
    dsmall <- d2    
  }
  
  nl <- ncol(dlarge)
  bws <- ldply(dsmall[,1:(nl-1)],resolution)$V1
  bwl <- ldply(dlarge[,names(dsmall)[-nl]],resolution)$V1
  radius <- bws/bwl
  
  dm <- dlarge
  for (x in 1:(nl-1)) {
    dm[,x] <- radius[x]*round(dlarge[,x]/radius[x],0)
  }
  dsmall$id <- 1:nrow(dsmall)
  dm <- merge(dm, dsmall, all=T, by=names(dm)[-nl])
  dm$Freq.y[is.na(dm$Freq.y)] <- 0
  dm$Freq.x[is.na(dm$Freq.x)] <- 0
  res <- ddply(dm, .(id), summarize, n=length(id), xsum=sum(Freq.x), ysum=sum(Freq.y))
  dm <- merge(dm, res, by="id")
  ## leads to losses of more than 100% because of binning issues
  ##dm$loss <- with(dm, (Freq.x-Freq.y/n)^2) 
  dm$loss <- with(dm, (Freq.x-xsum/n)^2)
  TSS <- sum((dm$Freq.x-mean(dm$Freq.x))^2)
  browser()
  c(TSS=TSS, loss=sum(dm$loss, na.rm=T), percent.loss=sum(dm$loss, na.rm=T)/TSS*100)
}


#' Loss function
#'
#' Based on a given data set, loss is computed from comparing the original data to the data binned at the specified level.
#' Loss is computed as sum of squared differences between all values at the original data and the binned data.
#' Percent loss compares loss to the total sum of squares in the original data.
#' 
#' @param d1 data frame
#' @param binning vector of binwidths
#' @export
#' @examples
#' connect <- dbConnect(dbDriver("MySQL"), user="dbaccess", 
#' port=3306, dbname="baseball", host="mysql2.stat.iastate.edu")
#' pitch <- new("dataDB", co=connect, table="Pitching")
#' d1 <- dbData(pitch, vars=c( "G", "SO"))
#' qplot(G,SO, fill=log10(Freq), data=d1, geom="tile")+scale_fill_gradient2()
#' loss2(d1, binning=c(2,5))
#' loss2(d1, binning=c(1,5))
#' d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(1,5))
#' qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+scale_fill_gradient2()
#' loss2(d1, binning=c(1,10))
#' loss2(d2, binning=c(1,2))
#' d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(1,10))
#' qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+scale_fill_gradient2()
#' ## some more exploration of loss
#' bins <- expand.grid(x=c(1:10), y=c(1:10))
#' losses <- mdply(bins, function(x,y) loss2(data=d1, binning=c(x, y)))
#' qplot(x, percent.loss, group=y, data=losses, geom="line")
#' qplot(y, percent.loss, group=x, data=losses, geom="line")
#' d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(10,10))
#' 

loss2 <- function(data, binning) {
  # assumptions:
  # the last variable in data is a count
  # what if scenario: loss we would experience, if we used the binning given by binning  
  
  dnew <- data
  nl <- ncol(data)
  binning <- rep(binning, length=nl-1)
  for (x in 1:(nl-1)) {
    dnew[,x] <- binning[x]*round(data[,x]/binning[x],0)
  }
  dnew$id <- interaction(dnew[,1:(nl-1)])
  res <- ddply(dnew, .(id), summarize, n=length(id), fsum=sum(Freq))
  dnew <- merge(dnew, res, by="id")
  dnew$loss <- with(dnew, (Freq-fsum/n)^2)
  TSS <- sum((dnew$Freq-mean(dnew$Freq))^2)
  c(TSS=TSS, loss=sum(dnew$loss, na.rm=T), percent.loss=sum(dnew$loss, na.rm=T)/TSS*100)
}



#' Method for dataDB object: retrieve dimensions of table
#'
#' @param x dataDB object
#' @export
#' @rdname dataDB-methods
#' @examples
#' connect <- dbConnect(dbDriver("MySQL"), user="dbaccess", 
#' port=3306, dbname="baseball", host="mysql2.stat.iastate.edu")
#'
#' pitch <- new("dataDB", co=connect, table="Pitching")
#' d1 <- dbData(pitch, vars=c( "G", "SO"))


setMethod("dim", "dataDB",
          function(x="dataDB") {
            ncol <- drop(length(dbListFields(x@co, x@table)))
            nrow <- dbGetQuery(x@co, sprintf("select count(*) from %s",x@table))[1,1]
            c(nrow, ncol)
          }
)
