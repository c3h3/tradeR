% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forwardSamplingTools.R
\name{Pt2Rt}
\alias{Pt2Rt}
\title{Pt2Rt}
\usage{
Pt2Rt(Xt, XtColumnSeletor = Cl)
}
\arguments{
\item{Xt}{a xts object}

\item{XtColumnSeletor}{Xt's single column selector}
}
\description{
function which can help you transform your price xts to an accumulate return xts
}
\examples{
Xt = getSymbols('2330.TW', auto.assign = F)
Xt \%>\% Pt2Rt \%>\% head
}
