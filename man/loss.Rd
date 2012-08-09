\name{loss}
\alias{loss}
\title{Loss function}
\usage{
  loss(d1, d2)
}
\arguments{
  \item{d1}{data frame}

  \item{d2}{data frame}
}
\description{
  Loss function
}
\examples{
connect <- dbConnect(dbDriver("MySQL"), user="2009Expo",
password="R R0cks", port=3306, dbname="baseball",
host="headnode.stat.iastate.edu")
pitch <- new("dataDB", co=connect, table="Pitching")
d1 <- dbData(pitch, vars=c( "G", "SO"))
d2 <- dbData(pitch, vars=c( "G", "SO"), binwidth=c(2, 5))
qplot(G,SO, fill=log10(Freq), data=d2, geom="tile")+scale_fill_gradient2()
qplot(G,SO, fill=log10(Freq), data=d1, geom="tile")+scale_fill_gradient2()
loss(d1, d2)
}

