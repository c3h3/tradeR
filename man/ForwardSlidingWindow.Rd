% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forwardSamplingTools.R
\name{ForwardSlidingWindow}
\alias{ForwardSlidingWindow}
\title{ForwardSlidingWindow}
\usage{
ForwardSlidingWindow(accRt, win_size)
}
\arguments{
\item{accRt}{an accumulate return xts object}

\item{win_size}{the size of forward sampling slide window}
}
\description{
a funtion which an help you do forward sampling easier
}
\examples{
Xt = getSymbols('2330.TW', auto.assign = F)
Xt \%>\% Pt2Rt \%>\% ForwardSlidingWindow(20) \%>\% head
}
