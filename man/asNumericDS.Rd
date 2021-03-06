% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/asNumericDS.R
\name{asNumericDS}
\alias{asNumericDS}
\title{Coerces an R object into class numeric}
\usage{
asNumericDS(x.name)
}
\arguments{
\item{x.name}{the name of the input object to be coerced to class
numeric. Must be specified in inverted commas. But this argument is
usually specified directly by <x.name> argument of the clientside function
\code{ds.asNumeric}.}
}
\value{
the object specified by the <newobj> argument (or its default name
<x.name>.num) which is written to the serverside. For further
details see help on the clientside function \code{ds.asNumeric}.
}
\description{
This function is based on the native R function \code{as.numeric}.
}
\details{
See help for function \code{as.numeric} in native R, and details section
in the help file of the clientside function \code{ds.asNumeric}.
}
\author{
Amadou Gaye, Paul Burton, Demetris Avraam, for DataSHIELD Development Team
}
