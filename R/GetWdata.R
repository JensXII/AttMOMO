#' Read weather data achieved from EuroMOMO
#'
#' @param dir Directory with data: dir/wdata_CountryCode.txt.
#' @param CountryCode NUTS country code.
#' @param NUTS A list of equal length NUTS by which to limit data (default = CountryCode).
#' @import data.table
#' @return data.table: date, pop3, NUTS3, temp, mintemp, maxtemp.
#' @export
GetWdata <- function(dir, CountryCode, NUTS = NULL) {
  NUTS3 <- read.table <- NULL

  if (is.null(NUTS)) {NUTS <- CountryCode}
  if (min(substr(NUTS,1,2) == CountryCode) == 0) {
    stop(paste("Two first characters in NUTS !=", CountryCode))
  }
  wdata <- try(data.table::data.table(read.table(paste0(dir, "/wdata_", CountryCode, ".txt"),
                                     header = TRUE, sep = ";", dec = ".", as.is = TRUE))
               [(substr(NUTS3, 1 , max(nchar(NUTS))) %in% NUTS), c("date", "pop3", "NUTS3", "temp", "mintemp", "maxtemp")])
  if (inherits(wdata, "try-error")) {
    stop(paste0("Cannot read wdata_", CountryCode, ".txt from ", dir))
  }
  wdata[order(NUTS3, date),]

  return(wdata)
}
