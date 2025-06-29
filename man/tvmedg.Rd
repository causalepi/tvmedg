% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tvmedg.R
\name{tvmedg}
\alias{tvmedg}
\title{Time-varying mediation analysis using g-computation}
\usage{
tvmedg(
  data,
  fix,
  expo,
  med,
  tvar,
  lag,
  outc,
  time,
  norev = NULL,
  LM = FALSE,
  boot = FALSE,
  seed = 0,
  mreg = "binomial",
  lreg = c("binomial", "gaussian", "gaussian"),
  yreg = "binomial",
  dof = 3,
  montecarlo = 10000,
  length = 12,
  parallel = TRUE,
  nboot = 1,
  ci = 0.95
)
}
\arguments{
\item{data}{input data}

\item{fix}{time-fixed variable}

\item{expo}{exposure variable}

\item{med}{mediator variable}

\item{tvar}{time-varying variable}

\item{lag}{number of lag}

\item{outc}{outcome variable}

\item{time}{time variable}

\item{norev}{non-reversible variable (among expo,med,tvar)}

\item{LM}{L before M assumption}

\item{boot}{doing boostrap}

\item{seed}{set seed}

\item{mreg}{regression model for mediator}

\item{lreg}{regression model for time-varying variable}

\item{yreg}{regression model for outcome variable}

\item{dof}{degree of freedom}

\item{montecarlo}{number of repeated samples for accept-reject algorithm}

\item{length}{time to follow up}

\item{parallel}{run parallel}

\item{nboot}{bootstraping times}

\item{ci}{confident interval}
}
\value{
Q11, Q10, Q00
}
\description{
Time-varying mediation analysis using g-computation
}
