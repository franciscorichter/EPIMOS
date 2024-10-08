% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oasi_fetch_data.R
\name{process_locations}
\alias{process_locations}
\title{Process Locations Data}
\usage{
process_locations(domain)
}
\arguments{
\item{domain}{A character string representing the domain for which to process locations data.}
}
\value{
A data frame containing the locations that have the "Q" parameter available. If the data fetching fails, returns NULL.
}
\description{
This function processes location data by separating locations based on the availability of the "Q" parameter.
It fetches the locations data, checks each location for the "Q" parameter, and categorizes the locations into
two groups: those with the "Q" parameter and those without. It then prints and returns the data for locations
with the "Q" parameter.
}
\examples{
\dontrun{
q_locations <- process_locations("example_domain")
print(q_locations)
}
}
