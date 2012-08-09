\name{dbNBins}
\alias{dbNBins}
\title{Determine number of distinct values of variables in a database}
\usage{
  dbNBins(data, vars = list(), binwidth = -1,
    margin = TRUE)
}
\arguments{
  \item{data}{dataDB object}

  \item{vars}{list of variable names}

  \item{binwidth}{vector of bin sizes for each variable. -1
  for minimal binwidth}

  \item{margin}{logical value: should marginal
  distributions be considered or joint distribution?}
}
\description{
  For margin = TRUE the function return the number of
  distinct levels each variable has. For numeric variables
  this number will be identical to the number of rows in
  case there are no ties. If margin = FALSE, the function
  returns the number of distinct tuples. If binwidth is
  specified, the number of distinct tuples of the binned
  data is returned. As the number of variables increases,
  the number of tuples will grow to at most the product of
  the marginal number of categories, but not beyond the
  number of records in the data table.
}
\examples{
connect <- dbConnect(dbDriver("MySQL"), user="2009Expo",
password="R R0cks", port=3306, dbname="baseball",
host="headnode.stat.iastate.edu")
pitch <- new("dataDB", co=connect, table="Pitching")
dbNBins(pitch, vars=list("G", "SO", "yearID"))
dbNBins(pitch, vars=list("G", "SO", "yearID"), binwidth=c(10,10,1))
dbNBins(pitch, vars=list("G", "SO", "yearID"), binwidth=c(10,10,1), margin=FALSE)
dbNBins(pitch, vars=list("yearID", "playerID"), margin=FALSE)
}

