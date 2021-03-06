% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/school_directory.R
\name{vz_get_search_page}
\alias{vz_get_search_page}
\title{Get search page for directory search}
\usage{
vz_get_search_page(base_url = NULL)
}
\arguments{
\item{base_url}{If left unset, defaults to internally recorded base URL}
}
\value{
an rvest_session object containing the session for the search page.
Can be passed on to \code{vz_get_search_form()}.
}
\description{
Get search page for directory search
}
