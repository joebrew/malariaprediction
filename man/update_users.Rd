% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_users.R
\name{update_users}
\alias{update_users}
\title{Update users}
\usage{
update_users(users = NULL, transactions = NULL, offers = NULL)
}
\arguments{
\item{users}{A users dataset}

\item{transactions}{A transactions dataset}

\item{offers}{An offers dataset}
}
\value{
transactions, users and offers tables updated
}
\description{
Update the users table as a function of transactions
This does not actually modify the users datafame; it simply gives an in
memory update
}

