% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/authorship_pattern.R
\name{authorship_pattern}
\alias{authorship_pattern}
\title{Co-authorship Matrix and Average co-authorship}
\usage{
authorship_pattern(authors, pub_years, sep)
}
\arguments{
\item{authors}{A character vector containing author names}

\item{pub_years}{A numeric vector containing publication years}

\item{sep}{A character pattern separating author names}
}
\value{
A list with co-authorship matrix and average co-authorship values
}
\description{
It finds year-wise co-authorship matrix and average co-authorship values
}
\examples{
authors<-c("Wolf W.R., Lele S.K.",
"Shin D., Yeh X., Khatib O.",
"Aukes D., Heyneman B., Duchaine V., Cutkosky M.R.")
years<-c(2011,2012,2012)
authorship_pattern(authors,years,',')
}

