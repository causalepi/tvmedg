% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_data.R
\name{process_data}
\alias{process_data}
\title{Process data}
\usage{
process_data(
  fix,
  expo,
  med,
  tvar,
  lag,
  outc,
  time,
  norev = NULL,
  LM = FALSE,
  data
)
}
\arguments{
\item{fix}{time-fixed variable}

\item{expo}{exposure variable}

\item{med}{mediator variable}

\item{tvar}{time-varying variable}

\item{lag}{number of lag}

\item{outc}{outcome variable}

\item{time}{time variable}

\item{norev}{non-reversible variable (among expo,med,tvar)}

\item{LM}{L before M assumption}

\item{data}{input data}
}
\value{
a data frame with lag of exposure, mediator, time-varying variable
}
\description{
Process data
}
