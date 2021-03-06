% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_datagov_apikey.R
\name{save_datagov_apikey}
\alias{save_datagov_apikey}
\title{Save a data.gov API key}
\usage{
save_datagov_apikey(key, overwrite = FALSE, install = FALSE)
}
\arguments{
\item{key}{Your API Key}

\item{overwrite}{TRUE or FALSE.}

\item{install}{TRUE or FALSE}
}
\description{
It's best practice not to include your API key in scripts. Save it interactively with this function, then it will be available with Sys.getenv("DATAGOV_API_KEY"). All 'api_key' arguments in tidyfec functions default to Sys.getenv("DATAGOV_API_KEY").
}
\details{
Note: The code for save_datagov_apikey is lifted from Kyle Walker's tidycensus package (MIT License).
}
