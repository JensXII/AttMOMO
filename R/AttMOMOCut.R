#' Run AttMOMO base and baseline adjusted models.
#' Read input data, create output directories and return csv-file with estimates.
#' Demanded input files must be available in wdir/data
#'
#' @param country Country name.
#' @param wdir Working directory.
#' @param StartWeek ISOweek format
#' @param EndWeek ISOweek format
#' @param groups list of group names
#' @param pooled list of group names to be pooled (default = NULL)
#' Must be part of groups.
#' @param indicators list if indicator variables.
#' One file for each must be available: IndicatorName_data.txt
#' @param indicatorCuts list of cut-weeks for each indicator in indicators
#' @param lags weeks of lagged effect (default = 2, max = 9)
#' @param ptrend significance of trend to be included (default = 0.05)
#' @param p26 significance of half year-sine be included (default = 0.05)
#' @param p52 significance of year-sine be included (default = 0.10)
#' @param Rdata Return data to R (TRUE/FALSE - default FALSE).
#' @import data.table
#' @return Write a ;-separated file AttData_indicators.txt in output directory/output.
#' @export
AttMOMOCut <- function(country, wdir, StartWeek, EndWeek, groups, pooled = NULL, indicators, indicatorCuts,
                    lags = 2, ptrend = 0.05, p26 = 0.05, p52 = 0.10, Rdata = FALSE) {
  read.table <- write.table <- quasipoisson <- df.residuals <- predict.glm <- residuals <- NULL

  # country <- "Denmark"
  # wdir <- "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/AttMOMO_DK"
  # StartWeek <- StartWeek
  # EndWeek <- EndWeek
  # groups <- c('00to14', '15to44', '45to64', '65to74', '75to84', '85P', 'Total')
  # pooled <- c('00to14', '15to44', '45to64', '65to74', '75to84', '85P')
  # indicators <- c("RSVPosInc", "InflPosInc", "COVID19PosInc")
  # indicatorCuts <- list(`RSVPosInc` = c("2015-W21", "2016-W21", "2017-W21", "2018-W21", "2019-W21", "2020-W21", "2021-W21", "2022-W21"),
  #                      `InflPosInc` = c("2015-W40", "2016-W40", "2017-W40", "2018-W40", "2019-W40", "2020-W40", "2021-W40", "2022-W40"),
  #                      `COVID19PosInc` = c("2020-W01", "2021-W01", "2021-W26", "2021-W52", "2022-W27"))
  # lags <- 3
  # ptrend <- 0.05
  # p26 <- 0.05
  # p52 <- 0.10
  # Rdata <- FALSE


  # Directory setup ---------------------------------------------------------
  # Create general output dir
  if (!dir.exists(paste0(wdir, "/AttMOMO_", EndWeek))) { dir.create(paste0(wdir, "/AttMOMO_", EndWeek)) }
  # Copy data directory - directory where input data are stored
  indir <- paste0(wdir, "/AttMOMO_", EndWeek, "/data")
  if (!dir.exists(indir)) dir.create(indir)
  file.copy(from = list.files(paste0(wdir,"/data"), all.files = TRUE, full.names = TRUE, no.. = TRUE),
            to = indir, overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
  #file.remove(list.files(paste0(wdir,"/data"), all.files = TRUE, full.names = TRUE, no.. = TRUE))

  # Create output directory - directory where output are created
  outdir <- paste0(wdir,"/AttMOMO_", EndWeek, "/output")
  if (!dir.exists(outdir)) dir.create(outdir)

  # Read and merge data -----------------------------------------------------
  # death data
  death_data <- try(read.table(paste0(indir, "/death_data.txt"), sep=";", dec=".", header = TRUE, as.is = TRUE)[,c("group", "ISOweek", "deaths")])
  if (inherits(death_data, "try-error")) {
    stop(paste0("Could not read ", indir, "/death_data.txt"))
  }

  # Population data
  if (file.exists(paste0(indir, "/population_data.txt"))) {
    population_data <- try(read.table(paste0(indir, "/population_data.txt"), sep=";", dec=".", header = TRUE, as.is = TRUE)[,c("group", "ISOweek", "N")])
    if (inherits(population_data, "try-error")) {
      stop(paste0("Could not read ", indir, "/population_data.txt"))
    }
  }
  if (file.exists(paste0(indir, "/Population_data.txt"))) {
    population_data <- try(read.table(paste0(indir, "/Population_data.txt"), sep=";", dec=".", header = TRUE, as.is = TRUE)[,c("group", "ISOweek", "N")])
    if (inherits(population_data, "try-error")) {
      stop(paste0("Could not read ", indir, "/Population_data.txt"))
    }
  }

  # Extreme temperature data
  ET_data <- try(read.table(paste0(indir,"/ET_data.txt"), header = TRUE, sep = ";", dec = ".", as.is =  TRUE))[, c("ISOweek", "ET")]
  if (inherits(ET_data,"try-error")) {
    stop(paste0("Could not read ", indir, "/ET_data.txt"))
  }

  # Indicator data
  if (length(indicators[duplicated(indicators)]) > 0) {
    stop("Duplicated indicators")
  }
  indicators <- lapply(indicators, sort)
  for (i in indicators) {
    X <- try(read.table(paste0(indir,"/", i, "_data.txt"), header = TRUE, sep = ";", dec = ".", as.is =  TRUE)[, c("group", "ISOweek", i)])
    if (inherits(X, "try-error")) {
      stop(paste0("Could not read  ", indir, "/", i, "_data.txt"))
    }
    # assign(paste0(i, '_data'), X)
    assign(paste0(i, '_data'), X, envir = .GlobalEnv)
  }

  # Indicator week-cuts
  if (length(indicatorCuts[duplicated(indicatorCuts)]) > 0) {
    stop("Duplicated indicatorCuts")
  }
  indicatorCuts <- lapply(indicatorCuts, sort)
  if (max(match(indicators,names(indicatorCuts))) != length(indicators)) {
    stop("Not week-cuts for all or too many indicators")
  }
  rm(X)

  # source('R/AttMOMO_estimationCut.R')
  AttData <- AttMOMO::AttMOMO_estimationCut(country, StartWeek, EndWeek, groups, pooled, indicators, indicatorCuts, death_data, ET_data, lags, ptrend, p26, p52)

  write.table(AttData, file = paste0(outdir, "/AttData_", paste(indicators, collapse = '_'), ".txt"), sep = ";", row.names = FALSE, col.names = TRUE)

  if (Rdata) {
    return(AttData)
  } else {
    return(invisible(NULL))
  }
}
