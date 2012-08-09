\name{loss2}
\alias{loss2}
\title{Loss function}
\usage{
  loss2(data, binning)
}
\arguments{
  \item{d1}{data frame}

  \item{binning}{vector of binwidths}
}
\description{
  Based on a given data set, loss is computed from
  comparing the original data to the data binned at the
  specified level. Loss is computed as sum of squared
  differences between all values at the original data and
  the binned data. Percent loss compares loss to the total
  sum of squares in the original data.
}
\examples{
connect <- dbConnect(dbDriver("MySQL"), user="2009Expo",
password="R R0cks", port=3306, dbname="baseball",
host="headnode.stat.iastate.edu")
pitch <- new("dataDB", co=connect, table="Pitching")
d1 <- dbData(pitch, vars=c( "G", "SO"))
qplot(G,SO, fill=log10(Freq), data=d1, geom="tile")+scale_fill_gradient2()
loss2(d1, binning=c(2,5))
loss2(d1, binning=c(1,5))
d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(1,5))
qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+scale_fill_gradient2()
loss2(d1, binning=c(1,10))
loss2(d2, binning=c(1,2))
d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(1,10))
qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+scale_fill_gradient2()
## some more exploration of loss
bins <- expand.grid(x=c(1:10), y=c(1:10))
losses <- mdply(bins, function(x,y) loss2(data=d1, binning=c(x, y)))
qplot(x, percent.loss, group=y, data=losses, geom="line")
qplot(y, percent.loss, group=x, data=losses, geom="line")
d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(10,10))
}

