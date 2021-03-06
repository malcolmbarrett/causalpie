% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/causify.R
\name{causes}
\alias{causes}
\alias{sc}
\alias{causify}
\title{Create sufficient causes}
\usage{
sc(...)

causify(..., add_u = TRUE)
}
\arguments{
\item{...}{components of the cause}

\item{add_u}{logical. Should component "U" be added to the sufficient cause?
Default is \code{TRUE}.}
}
\value{
\code{sc()}: a named vector; \code{causify()}: a \code{tibble} with sufficient causes.
}
\description{
Create sufficient causes
}
\examples{

causify(sc(A = 1, B = 0), sc(A = 1, E = 1, C = 0))

causify(sc(F = 1, Q = 0), sc(A = 1), add_u = FALSE)

}
