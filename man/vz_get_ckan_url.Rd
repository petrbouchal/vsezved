% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shared.R
\name{vz_get_ckan_url}
\alias{vz_get_ckan_url}
\title{Get URL of file in CKAN dataset}
\usage{
vz_get_ckan_url(package_id = NULL, base_url = NULL)
}
\arguments{
\item{package_id}{package ID. Leave as NULL for whole-country school register.}

\item{base_url}{CKAN base URL. Leave as NULL for MSMT CKAN.}
}
\value{
a URL, character of length 1
}
\description{
Currently assumes we are getting register XML data
}
