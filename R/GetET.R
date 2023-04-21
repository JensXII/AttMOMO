#' Excess Temperature data from weather data
#'
#' @param ET Data containing the variables: date, pop3, NUTS3, tvar
#' @param StartWeek Start week (ISO format)
#' @param EndWeek End week (ISO format)
#' @param tvar name of temperature variable in data
#' @import data.table
#' @import ISOweek
#' @return data.table with ISOweek and ET (excess temperature)
#' @export
GetET <- function(ET, StartWeek, EndWeek, tvar) {
  . <- temp <- pop3 <- NUTS3 <- pop3.sum <- wk <- ptemp <- ptmin <- ptmax <- predict <- glm <- read.table <- write.table <- ..cols <- NULL

  # source("C:/Users/NLS/Documents/GitHub/EuroMOMOnetwork/AttMOMO/R/GetWdata.R")
  # ET <- GetWdata('C:/Users/NLS/Bat_Files/wdata', 'DK')
  # StartWeek <- '2014-W27'
  # EndWeek <- '2020-W22'
  # tvar <- "mintemp"

  data.table::setDT(ET)
  # keep relevant colums
  cols <- c("date", "pop3", "NUTS3", tvar)
  ET <- ET[, ..cols]
  rm(cols)
  # renane tvar to temp
  setnames(ET, old = tvar, new = "temp")

  # time span
  ET <- merge(data.table::data.table(date = as.character(seq(as.Date(ISOweek::ISOweek2date(paste0(StartWeek, "-1"))),
                                                 as.Date(ISOweek::ISOweek2date(paste0(EndWeek, "-7"))), by = 'day'))),
              ET, by = 'date', all.x = TRUE)

  # Mean over NUTS3 and date
  ET <- ET[, .(temp = mean(temp, na.rm = TRUE), pop3 = mean(pop3, na.rm = TRUE)), keyby = .(NUTS3, date)]
  # add total population
  ET[, pop3.sum := sum(pop3, na.rm = TRUE), keyby = date]
  # by date weighted by population
  ET <- ET[, .(temp = sum(temp * pop3 / pop3.sum, na.rm =TRUE)), keyby = date]
  # mean by ISOweek
  ET <- ET[, .(temp = mean(temp, na.rm =TRUE), tmin = min(temp, na.rm =TRUE), tmax = max(temp, na.rm =TRUE)), keyby = .(ISOweek = ISOweek::ISOweek(as.Date(date)))]

  # regression parameters
  ET[, wk := as.numeric(as.factor(ISOweek))]
  ET[, `:=`(sin52 = sin((2*pi/(365.25/7)) * wk),
            cos52 = cos((2*pi/(365.25/7)) * wk))]
  # expected temperatures
  ET[, `:=`(ptemp = predict(glm(temp ~ sin52 + cos52, data=ET[!(is.na(ET$temp) | is.infinite(ET$temp)),]), ET),
            ptmin = predict(glm(tmin ~ sin52 + cos52, data=ET[!(is.na(ET$tmin) | is.infinite(ET$tmin)),]), ET),
            ptmax = predict(glm(tmax ~ sin52 + cos52, data=ET[!(is.na(ET$tmax) | is.infinite(ET$tmax)),]), ET))]
  # weeks with missing temperature measurements
  ET[, `:=`(temp = ifelse(is.na(temp) | is.infinite(temp), ptemp, temp), wk = NULL, sin52 = NULL, cos52 = NULL, tmin = NULL, tmax = NULL)]
  # excess temperatures
  ET[order(ISOweek), ET := (temp - ptmax)*(temp > ptmax) + (temp - ptmin)*(temp < ptmin)]

  return(ET)
}
