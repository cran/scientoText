% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/collaboration.R
\name{international_colmat}
\alias{international_colmat}
\title{International Collaboration Matrix}
\usage{
international_colmat(affiliations, pub_years = NULL, countries = NULL)
}
\arguments{
\item{affiliations}{A text vector containing affiliation (country) information}

\item{pub_years}{A numeric vector containing publication years}

\item{countries}{A list of countries (optional)}
}
\value{
Collaboration adjacent matrix(es)
}
\description{
Calculate Internationally Collaborated Matrix(es)
}
\details{
It finds the collaboration network at internationla level in terms of
adjacent matrix so affiliation fields must have country information
}
\examples{
affiliations<-c("Stanford University, Stanford, CA, United States; Montreal, QC, Canada",
"Stanford University, United States; Google Inc., United States",
"University of Michigan, Ann Arbor, MI 48109-2122, United States;
Tsinghua University, Beijing 100084, China",
"Imperial College London, London, SW7 2BZ, United Kingdom;
ENSTA, Ecole Polytechnique, Palaiseau, 91761, France")

pub_years<-c(2012,2012,2013,2014)

international_colmat( affiliations, pub_years)
international_colmat(affiliations)
}

