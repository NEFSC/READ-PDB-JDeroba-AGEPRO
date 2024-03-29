% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ApplyBB_CR.R
\name{AgeProRun}
\alias{AgeProRun}
\title{Iteratively apply agepro}
\usage{
AgeProRun(direct = "missing", proj.fname = "missing",
  FracBmsyThreshHi = 0, FracBmsyThreshLo = 0, FracFtarg = 1,
  SSBmsy = "missing", Fmsy = "missing", decimals = 3,
  domsy = FALSE, msy.name = "missing", CIwantLow = 0.05,
  CIwantHi = 0.95, fmsyold = 999, SSBmsyold = 999, msyold = 999,
  recrold = 999, nyr.avg = 10)
}
\arguments{
\item{direct}{is the directory location of a functional agepro .INP file; proj.fname}

\item{proj.fname}{is a functional age pro input file without the .INP at the end.  First year should be setup as "bridge year" with catch specified.}

\item{FracBmsyThreshHi}{is the fraction of SSBmsy above which fishing is held constant at FracFtarg.  Below this value, F declines linearly to FracBmsyThreshLo.  (Default is 0.0 and produces constant F if FracBmsyThreshLo also 0.0)}

\item{FracBmsyThreshLo}{is the fraction of SSBmsy below which no fishing occurs.  Above this value, F increases linearly to FracBmsyThreshHi.  (Default is 0.0 and produces constant F if FracBmsyThreshHi also 0.0)}

\item{FracFtarg}{is the proportion of Fmsy that defines the maximum F desired.  Enter the fraction between 0 and 1, not the desired F rate.}

\item{SSBmsy}{is spawning stock biomass at msy level}

\item{Fmsy}{is exactly that; Fmsy.}

\item{decimals}{is the number of decimals for output values}

\item{domsy}{do longterm MSY ref point run: TRUE or FALSE.  If TRUE, creates LaTex code for groundfish short report.}

\item{msy.name}{needed only if domsy=TRUE. name of functional agepro run for longterm MSY ref point projection}

\item{fmsyold}{needed only if domsy=TRUE. old fmsy value to compare to new projection}

\item{SSBmsyold}{needed only if domsy=TRUE. is old SSBmsy value to compare to new projection}

\item{msyold}{needed only if domsy=TRUE. is old msy value to compare to new projection}

\item{recrold}{needed only if domsy=TRUE. is old recruitment at MSY value to compare to new projection}

\item{nyr.avg}{needed only if domsy=TRUE. is the number of years from the end of the longterm projection (e.g., last 10 years) to be averaged to define ref points (e.g., Bmsy)}
}
\description{
Does iterative agepro runs
}
\examples{
AgeProRun()
}
\keyword{AgePro}
