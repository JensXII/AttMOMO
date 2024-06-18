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
#' @param Rdata Return list with data and R-square values to R (TRUE/FALSE - default FALSE).
#' @import data.table
#' @return Write two ;-separated files AttData_indicators.txt and R2_indicators.txt in wdir/output.
#' @export
AttMOMOCut <- function(country, wdir, StartWeek, EndWeek, groups, pooled = NULL, indicators, indicatorCuts,
                    lags = 2, ptrend = 0.05, p26 = 0.05, p52 = 0.10, Rdata = FALSE) {
  read.table <- write.table <- quasipoisson <- df.residuals <- predict.glm <- residuals <- NULL

  # country <- paste0(Countries[c, .(CountryName)])
  # wdir <- paste0("CountryData_", StartWeek, "_", StudyWeek, "_", DataWeek, "/", Countries[c, .(CountryName)], "_", Countries[c, .(NUTScode)])
  # StartWeek <- StartWeek
  # EndWeek <- StudyWeek
  # groups <- c("00to14", "15to44", "45to64", "65to74", "75to84", "85P", "Total")
  # pooled <- c("00to14", "15to44", "45to64", "65to74", "75to84", "85P")
  # indicators <- ind[[i]]
  # indicatorCuts <- Cuts
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
  } else {
    if (file.exists(paste0(indir, "/Population_data.txt"))) {
      population_data <- try(read.table(paste0(indir, "/Population_data.txt"), sep=";", dec=".", header = TRUE, as.is = TRUE)[,c("group", "ISOweek", "N")])
      if (inherits(population_data, "try-error")) {
        stop(paste0("Could not read ", indir, "/Population_data.txt"))
      }
    } else {
      population_data <- NULL
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
  indicatorCuts <- lapply(indicatorCuts, sort)
  if (max(match(indicators,names(indicatorCuts))) != length(indicators)) {
    stop("Not week-cuts for all or too many indicators")
  }
  rm(X)

  # source('H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/AttMOMO/R/AttMOMO_estimationCut.R')
  # AttData <- AttMOMO_estimationCut(country, StartWeek, EndWeek, groups, pooled, indicators, indicatorCuts, death_data, population_data, ET_data, lags, ptrend, p26, p52)
  AttData <- AttMOMO::AttMOMO_estimationCut(country, StartWeek, EndWeek, groups, pooled, indicators, indicatorCuts, death_data, population_data, ET_data, lags, ptrend, p26, p52)

  write.table(AttData$AttData, file = paste0(outdir, "/AttData_", paste(indicators, collapse = '_'), ".txt"), sep = ";", row.names = FALSE, col.names = TRUE)
  write.table(AttData$R2, file = paste0(outdir, "/R2_", paste(indicators, collapse = '_'), ".txt"), sep = ";", row.names = FALSE, col.names = TRUE)

  if (Rdata) {
    return(AttData)
  } else {
    return(invisible(NULL))
  }
}
