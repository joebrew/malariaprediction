% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/juice_the_market.R
\name{juice_the_market}
\alias{juice_the_market}
\title{Juice the market}
\usage{
juice_the_market(n = 1000, ids = c(0, -1, -2), overwrite = FALSE)
}
\arguments{
\item{n}{The number of random offers to make}

\item{ids}{the user id(s)}

\item{overwrite}{Whether to overwrite or add}
}
\value{
A row added to the offers table
}
\description{
Make a bunch of random offers from one person to get the market started
}

