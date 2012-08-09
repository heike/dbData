\name{dbData}
\alias{dbData}
\title{Function to get sufficient statistics of variables from an SQL database}
\usage{
  dbData(data, vars = list(), binwidth = -1, where = "")
}
\arguments{
  \item{data}{dataDB object}

  \item{vars}{list of variable names}

  \item{binwidth}{vector of bin sizes for each variable. -1
  for minimal binwidth}

  \item{where}{character string with conditional statement
  for SQL query}
}
\description{
  Function to get sufficient statistics of variables from
  an SQL database
}
\examples{
connect <- dbConnect(dbDriver("MySQL"), user="2009Expo",
password="R R0cks", port=3306, dbname="baseball",
host="headnode.stat.iastate.edu")
pitch <- new("dataDB", co=connect, table="Pitching")
names(pitch)
head(pitch, n=10)[,1:8]
pitch.stats <- dbData(vars=list("H", "SO"), pitch)

require(ggplot2)
qplot(H, SO, alpha=Freq, data=pitch.stats)
qplot(H, SO, fill=Freq, data=dbData(pitch, list("SO", "H"),
  binwidth=c(10,50)), geom="tile")
qplot(H, SO, fill=Freq, data=dbData(pitch, list("SO", "H", "yearID"),
  binwidth=c(10,50, -1)), facets=~yearID, geom="tile")
qplot(H, SO, fill=Freq, data=dbData(pitch, list("SO", "H", "yearID"),
  binwidth=c(10,50, -1), where="yearID > 1990"), facets=~yearID, geom="tile")
}

