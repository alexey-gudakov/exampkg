% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exampkg.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Return list with data frame objects}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{A vector of the years that you need}
}
\value{
Returns a list with data.frame objects
which contain columns MONTH and year
}
\description{
Return list with data frame objects
}
\examples{
\dontrun{
  years_range <- 2013:2015
  result <- fars_read_years(years_range)
}

}
