% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_offer.R
\name{make_offer}
\alias{make_offer}
\title{Make an offer to sell or buy a statement}
\usage{
make_offer(users = NULL, transactions = NULL, user_id = 1, event_id = 1,
  shares = 1, price = 50, yes = 1)
}
\arguments{
\item{users}{The users table}

\item{user_id}{A user id}

\item{event_id}{An event id}

\item{shares}{Number of shares}

\item{price}{The price per share}

\item{yes}{Boolean. Whether the offer is for or against}
}
\value{
A row added to the offers table
}
\description{
Make an offer to sell or buy a statement.
}

