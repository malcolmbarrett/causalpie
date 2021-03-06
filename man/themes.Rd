% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/causalpie.R
\name{themes}
\alias{themes}
\alias{theme_causal_pie}
\alias{theme_causal_pie_grid}
\title{Minimalist themes for causal pies}
\usage{
theme_causal_pie(base_size = 12, base_family = "", ...)

theme_causal_pie_grid(base_size = 12, base_family = "", ...)
}
\arguments{
\item{base_size}{base font size}

\item{base_family}{base font family}

\item{...}{additional arguments passed to \code{theme()}}
}
\description{
Minimalist themes for causal pies
}
\examples{

causify(sc(A = 1, B = 0), sc(A = 1, E = 1, C = 0)) \%>\%
  causal_pie_necessary() +
  theme_causal_pie()
}
