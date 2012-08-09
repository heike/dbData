\name{dataDB}
\alias{dataDB}
\alias{dim}
\alias{head}
\alias{names}
\title{Wrapper class to a database object}
\arguments{
  \item{co}{of class MySQLConnection, connection to a MySQL
  database}

  \item{table}{character string of table name}

  \item{x}{dataDB object}

  \item{n}{number of rows shown}

  \item{x}{dataDB object}

  \item{x}{dataDB object}
}
\description{
  Wrapper class to a database object

  Method for dataDB object: retrieve first n rows from the
  data set

  Method for dataDB object: show names of table

  Method for dataDB object: retrieve dimensions of table
}
\examples{
connect <- dbConnect(dbDriver("MySQL"), user="2009Expo",
password="R R0cks", port=3306, dbname="data_expo_2009",
host="headnode.stat.iastate.edu")

ontime <- new("dataDB", co=connect, table="ontime")
dim(ontime)
dim(new("dataDB", co=connect, table="weather"))
pitch <- new("dataDB", co=connect, table="Pitching")
d1 <- dbData(pitch, vars=c( "G", "SO"))
}

