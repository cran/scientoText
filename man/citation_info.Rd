% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/citation_info.R
\name{citation_info}
\alias{citation_info}
\title{Citations and Cited Instances}
\usage{
citation_info(citations, pub_years)
}
\arguments{
\item{citations}{A numeric vector containing citations}

\item{pub_years}{A numeric vector containing publication years}
}
\value{
return year-wise total instances (tp), cited instances and total citations (tc)
}
\description{
Citations and Cited Instances
}
\examples{
citation_info(c(1,3,0,4,2,3,1,0),c(2012,2012,2012,2013,2012,2011,2014,2014))
}

