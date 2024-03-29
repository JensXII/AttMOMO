% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AttMOMO_estimationCut.R
\name{AttMOMO_estimationCut}
\alias{AttMOMO_estimationCut}
\title{Run AttMOMO base and baseline adjusted models and return estimated means and variances.}
\usage{
AttMOMO_estimationCut(
  country,
  StartWeek,
  EndWeek,
  groups,
  pooled = NULL,
  indicators,
  indicatorCuts,
  death_data,
  population_data = NULL,
  ET_data,
  lags = 3,
  ptrend = 0.05,
  p26 = 0.05,
  p52 = 0.1
)
}
\arguments{
\item{country}{Country name.}

\item{StartWeek}{ISOweek format (YYYY-WXX)}

\item{EndWeek}{ISOweek format (YYYY-WXX)}

\item{groups}{list of group names}

\item{pooled}{list of group names to be pooled (default = NULL)
Must be part of groups.}

\item{indicators}{list if indicator variables.
One file for each must be available in memory: IndicatorName_data}

\item{indicatorCuts}{list of cut-weeks for each indicator in indicators}

\item{death_data}{Name of deaths data file in memory.}

\item{population_data}{Name of population data file in memory. NULL if not available (default)}

\item{ET_data}{Name of ET data file in memory.}

\item{lags}{weeks of lagged effect (default = 3, max = 9)}

\item{ptrend}{significance of trend to be included (default = 0.05)}

\item{p26}{significance of half year-sine be included (default = 0.05)}

\item{p52}{significance of year-sine be included (default = 0.10)}
}
\value{
data with weekly estimated means and variances
}
\description{
Run AttMOMO base and baseline adjusted models and return estimated means and variances.
}
