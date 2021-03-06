% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/twitToken.R
\name{twitToken}
\alias{twitToken}
\title{Twitter Token Generator}
\usage{
twitToken(consumer_key, consumer_secret, access_token, access_secret)
}
\arguments{
\item{consumer_key}{Character. The consumer key for your app as a string}

\item{consumer_secret}{Character. The consumer secret for your app as a string}

\item{access_token}{Character. The access token for your app as a string}

\item{access_secret}{Character. The access secret for your app as a string}
}
\description{
Generates a twitter token to access the API. 
You are assumed to have set up an app at apps.twitter.com.
This function requires the httr package
}
\examples{
twitToken(consumer_key, consumer_secret, access_token, access_secret)
}

