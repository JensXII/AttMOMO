#' Run AttMOMO base and baseline adjusted models and return estimated means and variances.
#'
#' @param country Country name.
#' @param StartWeek ISOweek format (YYYY-WXX)
#' @param EndWeek ISOweek format (YYYY-WXX)
#' @param groups list of group names
#' @param pooled list of group names to be pooled (default = NULL)
#' Must be part of groups.
#' @param indicators list if indicator variables.
#' One file for each must be available in memory: IndicatorName_data
#' @param indicatorCuts list of cut-weeks for each indicator in indicators
#' @param death_data Name of deaths data file in memory.
#' @param ET_data Name of ET data file in memory.
#' @param lags weeks of lagged effect (default = 3, max = 9)
#' @param ptrend significance of trend to be included (default = 0.05)
#' @param p26 significance of half year-sine be included (default = 0.05)
#' @param p52 significance of year-sine be included (default = 0.10)
#' @import data.table
#' @import glm2
#' @return data with weekly estimated means and variances
#' @export
AttMOMO_estimationCut <- function(country, StartWeek, EndWeek, groups, pooled = NULL, indicators, indicatorCuts, death_data, ET_data,
                                  lags = 3, ptrend = 0.05, p26 = 0.05, p52 = 0.10) {
  group <- ET <- summer <- winter <- EB <- EAB <- deaths <- VEB <- EET <- VEET <- VEAB <- wk <- . <- anova <- glm <- median <- residuals <- df.residual <- predict.glm <- quasipoisson <- NULL

  # country <- "Denmark"
  # StartWeek <- '2017-W38'
  # EndWeek <- '2023-W12'
  # groups = c('00to14', '15to44', '45to64', '65to74', '75to84', '85P', 'Total')
  # pooled <- c('00to14', '15to44', '45to64', '65to74', '75to84', '85P')
  # indicators <- c('GSIPLS', 'GSCLS')
  # indicatorCuts <- list(`GSIPLS` = c("2015-W40", "2016-W40", "2017-W40", "2018-W40", "2019-W40", "2020-W40", "2021-W40", "2022-W40", "2023-W40"),
  #                       `GSCLS` = c("2020-W01", "2021-W27", "2022-W27"))
  # death_data <- death_data
  # ET_data <- ET_data
  # lags <- 3
  # ptrend <- 0.05
  # p26 <- 0.05
  # p52 <- 0.10

  # Read and merge data -----------------------------------------------------
  # death data
  AttData <- data.table::data.table(death_data)
  if (sum(colnames(AttData) %in% c('group', 'ISOweek', 'deaths')) != 3) {
    stop('Columns group, ISOweek, deaths not in deaths_date')
  }
  for (g in groups) {
    if (!(g %in% unique(AttData$group))) {
      stop(paste("group", g, "not in death_data"))
    }
    if ((min(AttData[g == group,]$ISOweek) > StartWeek) | (max(AttData[g == group,]$ISOweek) < EndWeek)) {
      stop(paste("death_data", g, "do not cover", StartWeek, "to", EndWeek))
    }
  }
  AttData <- AttData[(StartWeek <= ISOweek) & (ISOweek <= EndWeek) & (group %in% groups),]
  AttData <- AttData[order(group, ISOweek),]

  # Population data
  if (exists("population_data")) {
    pop_data <- data.table::data.table(population_data)
    if (sum(colnames(pop_data) %in% c('group', 'ISOweek', 'N')) != 3) {
      stop('Columns group, ISOweek, N not in population_date')
    }
    if ((min(pop_data$ISOweek) > StartWeek) | (max(pop_data$ISOweek) < EndWeek)) {
      stop(paste("Population_data do not cover", StartWeek, "to", EndWeek))
    }
    AttData <- merge(AttData, pop_data[order(ISOweek), ], by = c("group", "ISOweek"))
  } else {
    AttData[, N := 1]
  }

  # Extreme temperature data
  ET_data <- data.table::data.table(ET_data)
  if (sum(colnames(ET_data) %in% c('ISOweek', 'ET')) != 2) {
    stop('Columns ISOweek, ET not in ET_date')
  }
  if ((min(ET_data$ISOweek) > StartWeek) | (max(ET_data$ISOweek) < EndWeek)) {
    stop(paste("ET_data do not cover", StartWeek, "to", EndWeek))
  }
  AttData <- merge(AttData, ET_data[order(ISOweek), ], by = "ISOweek")

  # Indicator data
  for (i in indicators) {
    X <- try(data.table::data.table(eval(parse(text = paste0(i, '_data')))))
    if (inherits(X, "try-error")) {
      stop(paste0("Could not read ", i, "_data"))
    }
    if (sum(colnames(X) %in% c('group', 'ISOweek', i)) != 3) {
      stop(paste0('Columns group, ISOweek, ', i, 'not in ', i, '_date'))
    }
    for (g in groups) {
      if (!(g %in% unique(X$group))) {
        stop(paste0("group ", g, " not in ", i, "_data"))
      }
      if ((min(X[g == group,]$ISOweek) > StartWeek) | (max(X[g == group,]$ISOweek) < EndWeek)) {
        stop(paste0(i, "_data ", g, " do not cover ", StartWeek, " to ", EndWeek))
      }
    }
    AttData <- merge(AttData, X[order(group, ISOweek),], by = c("group", "ISOweek"))
    rm(X)
  }

  # Prepare data ------------------------------------------------------------

  # Indicator week-cut
  AttData[, paste0(indicators, "period") := 0]
  for (i in indicators) {
    # Remove cut-points outside StartWeek to EndWeek
    indicatorCuts[i] <- list(unlist(indicatorCuts[i])[(StartWeek < unlist(indicatorCuts[i])) & (unlist(indicatorCuts[i]) < EndWeek)])
    for (s in unlist(indicatorCuts[i])) {
      AttData[ISOweek >= s, eval(parse(text = paste0(i, "period := ", i, "period + 1")))]
    }
  }

  # Warm/cold summer/winter
  AttData[, `:=`(summer = as.numeric((21 <= as.numeric(substr(ISOweek,7,8))) & (as.numeric(substr(ISOweek,7,8)) <= 39)),
                 winter = as.numeric((20 >= as.numeric(substr(ISOweek,7,8))) | (as.numeric(substr(ISOweek,7,8)) >= 40)))]
  AttData[, `:=`(cold_summer = -((ET < 0) * ET) * summer,
                 warm_summer = ((ET > 0) * ET) * summer,
                 cold_winter = -((ET < 0) * ET) * winter,
                 warm_winter = ((ET > 0) * ET) * winter)]
  AttData[, `:=`(summer = NULL, winter = NULL)]

  # lags
  for (i in c(indicators, c('cold_summer', 'warm_summer', 'cold_winter', 'warm_winter'))) {
    for (l in 0:lags) {
      expr <- parse(text = paste0(i, "_d", l, " := data.table::shift(", i, ", ", l, ", type = 'lag')"))
      AttData[, eval(expr), by = group]
    }
  }

  # indicator by indicator cut-weeks
  for (i in indicators) {
    for (l in 0:lags) {
      for (s in unique(AttData[, eval(parse(text = paste0(i, "period")))])) {
        expr <- parse(text = paste0(i, "_d", l, "_", s, " := ifelse(", s, " == ", i, "period", ", ", i, "_d", l, ", 0)"))
        AttData[, eval(expr), by = group]
      }
      expr <- parse(text = paste0(i, "_d", l, " := NULL"))
      AttData[, eval(expr)]
    }
  }
  AttData[, `:=`(cold_summer= NULL, warm_summer = NULL, cold_winter = NULL, warm_winter = NULL)]
  AttData[is.na(AttData)] <- 0

  # baseline
  AttData[, wk := as.numeric(as.factor(ISOweek))]
  AttData[, `:=`(const = 1,
                 sin52 = sin((2*pi/(365.25/7)) * wk),
                 cos52 = cos((2*pi/(365.25/7)) * wk),
                 sin26 = sin((4*pi/(365.25/7)) * wk),
                 cos26 = cos((4*pi/(365.25/7)) * wk))]

  # Prediction data ---------------------------------------------------------
  # baseline
  AttData.B <- data.table::copy(AttData)
  for (l in 0:lags) {
    for (i in c('cold_summer', 'warm_summer', 'cold_winter', 'warm_winter')) {
      expr <- parse(text = paste0(i, "_d", l, ":= 0"))
      AttData.B[, eval(expr)]
    }
    for (i in indicators) {
      for (s in unique(AttData[, eval(parse(text = paste0(i, "period")))])) {
        expr <- parse(text = paste0(i, "_d", l, "_", s, " := 0"))
        AttData.B[, eval(expr)]
      }
    }
  }
  # ET
  AttData.ET <- data.table::copy(AttData)
  AttData.ET[, `:=`(const = 0, wk = 0, sin52 = 0, cos52 = 0, sin26 = 0, cos26 = 0)]
  for (i in indicators) {
    for (l in 0:lags) {
      for (s in unique(AttData[, eval(parse(text = paste0(i, "period")))])) {
        expr <- parse(text = paste0(i, "_d", l, "_", s, " := 0"))
        AttData.ET[, eval(expr)]
      }
    }
  }
  # indicators
  for (i in indicators) {
    X <- data.table::copy(AttData)
    X[, `:=`(const = 0, wk = 0, sin52 = 0, cos52 = 0, sin26 = 0, cos26 = 0)]
    for (l in 0:lags) {
      for (ir in c('cold_summer', 'warm_summer', 'cold_winter', 'warm_winter')) {
        expr <- parse(text = paste0(ir, "_d", l, ":= 0"))
        X[, eval(expr)]
      }
      for (ir in indicators[indicators != i]) {
        for (s in unique(AttData[, eval(parse(text = paste0(i, "period")))])) {
          expr <- parse(text = paste0(ir, "_d", l, "_", s, " := 0"))
          X[, eval(expr)]
        }
      }
    }
    assign(paste0("AttData.", i), X)
  }
  rm(X)

  # Estimation --------------------------------------------------------------
  # non-baseline parameters
  parm <- paste(grep("_d[0-9]", names(AttData), value=TRUE), collapse = " + ")

  for (g in groups) {

    cat(paste("### Group", g, "###\n"))

    selection <- function(f, fa, p) {
      # m <- try(glm2::glm2(f, quasipoisson(identity), data = AttData[group == g,]), silent = TRUE)
      m <- try(glm2::glm2(f, quasipoisson(identity), data = AttData[group == g,], weights = N), silent = TRUE)
      if (!inherits(m, "try-error")) {
        if (m$converged) {
          ma <- try(glm2::glm2(fa, quasipoisson(identity), data = AttData[group == g,], weights = N), silent = TRUE)
          if (!inherits(ma, "try-error")) {
            if (ma$converged &
                (anova(m, ma, dispersion = max(1, sum(residuals(m, type = "deviance")^2)/df.residual(m)),
                       test="LRT")$`Pr(>Chi)`[2] > p)) {
              return(ma)
            } else {
              return(m)
            }
          } else {
            return(m)
          }
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    }
    suppressWarnings({
      m <- selection(f = paste(c("deaths/N ~ -1 + const + wk"), collapse = " + "),
                     fa = paste(c("deaths/N ~ -1 + const + wk", parm), collapse = " + "), 0)

      ma <- selection(f = paste(c("deaths /N~ -1 + const + wk", parm), collapse = " + "),
                      fa = paste(c("deaths/N ~ -1 + const + wk", "sin52 + cos52", parm), collapse = " + "), p52)
      if (!is.null(ma)) m <- ma

      ma <- selection(f = paste(c("deaths/N ~ -1 + const + wk", "sin52 + cos52", parm), collapse = " + "),
                      fa = paste(c("deaths/N ~ -1 + const + wk", "sin52 + cos52", "sin26 + cos26", parm), collapse = " + "), p26)
      if (!is.null(ma)) m <- ma
    })

    # Remove NA co-linearity
    f <- paste("deaths ~ -1 +", paste(names(m$coefficients[!is.na(m$coefficients)]), collapse = ' + '))
    m <- glm2::glm2(f, quasipoisson(identity), data = AttData[group == g,], weights = N)

    print(summary(m, dispersion = max(1, sum(residuals(m, type = "deviance")^2)/df.residual(m))))

    # Predictions -------------------------------------------------------------

    # Prediction baseline
    AttData[group == g, `:=`(EB = predict.glm(m, newdata = AttData.B[group == g,], se.fit=FALSE))]
    AttData[group == g, `:=`(VEB = (max(1, sum(residuals(m)^2)/df.residual(m)))*EB +
                               predict.glm(m, newdata = AttData.B[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)]
    #  Prediction ET
    AttData[group == g, `:=`(EET = predict.glm(m, newdata = AttData.ET[group == g,], se.fit=FALSE),
                             VEET = predict.glm(m, newdata = AttData.ET[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)]

    # Predictions indicators
    for (i in indicators) {
      expr <- parse(text = paste0("`:=`(E", i, " = predict.glm(m, newdata = AttData.", i, "[group == g,], se.fit=FALSE),
      VE", i, " = predict.glm(m, newdata = AttData.", i, "[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)"))
      AttData[group == g, eval(expr)]
    }

    # Adjusted baseline
    X <- data.table::copy(AttData)
    for (i in indicators) {
      for (l in 0:lags) {
        for (s in unique(AttData[, eval(parse(text = paste0(i, "period")))])) {
          expr <- parse(text = paste0(i, "_d", l, "_", s, " := ifelse(E", i," >= 0, 0,", i, "_d", l, "_", s,")"))
          X[, eval(expr)]
        }
      }
    }
    AttData[group == g, `:=`(EAB = predict.glm(m, newdata = X[group == g,], se.fit=FALSE))]
    AttData[group == g, `:=`(VEAB = (max(1, sum(residuals(m)^2)/df.residual(m)))*EAB +
                               predict.glm(m, newdata = X[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)]

    # Adjusted predictions indicators
    for (i in indicators) {
      X <- data.table::copy(AttData)
      X[, `:=`(const = 0, wk = 0, sin52 = 0, cos52 = 0, sin26 = 0, cos26 = 0)]
      for (l in 0:lags) {
        for (ir in c('cold_summer', 'warm_summer', 'cold_winter', 'warm_winter')) {
          expr <- parse(text = paste0(ir, "_d", l, ":= 0"))
          X[, eval(expr)]
        }
        for (s in unique(AttData[, eval(parse(text = paste0(i, "period")))])) {
          for (ir in indicators[indicators != i]) {
            expr <- parse(text = paste0(ir, "_d", l, "_", s, " := 0"))
            X[, eval(expr)]
          }
          expr <- parse(text = paste0(i, "_d", l, "_", s, " := ifelse(E", i," < 0, 0,", i, "_d", l, "_", s,")"))
          X[, eval(expr)]
        }
      }
      expr <- parse(text = paste0("`:=`(EA", i, " = pmax(0, predict.glm(m, newdata = X[group == g,], se.fit=FALSE)),
                                  VEA", i, " = predict.glm(m, newdata = X[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)"))
      AttData[group == g, eval(expr)]
    }
    rm(X)

  }

  # Clean up
  AttData[, `:=`(const = NULL, wk = NULL, sin52 = NULL, cos52 = NULL, sin26 = NULL, cos26 = NULL)]
  for (l in 0:lags) {
    for (i in c('cold_summer', 'warm_summer', 'cold_winter', 'warm_winter')) {
      expr <- parse(text = paste0(i, "_d", l, " := NULL"))
      AttData[, eval(expr)]
    }
    for (i in indicators) {
      for (s in unique(AttData[, eval(parse(text = paste0(i, "period")))])) {
        expr <- parse(text = paste0(i, "_d", l, "_", s, " := NULL"))
        AttData[, eval(expr)]
      }
    }
  }
  rm(expr, parm, g, i, l, ma, AttData.B, AttData.ET)

  # Pooled total ------------------------------------------------------------
  if (!is.null(pooled)) {
    pooledData <- AttData[group %in% pooled,
                          .(group = 'TotalPooled',
                            deaths = sum(deaths, na.rm = TRUE),
                            ET = mean(ET, na.rm = TRUE),
                            EB = sum(EB, na.rm = TRUE),
                            VEB = sum(VEB, na.rm = TRUE),
                            EET = sum(EET, na.rm = TRUE),
                            VEET = sum(VEET, na.rm = TRUE),
                            EAB = sum(EAB, na.rm = TRUE),
                            VEAB = sum(VEAB, na.rm = TRUE)
                          ), keyby = ISOweek]

    for (i in indicators) {
      expr <- parse(text = paste0(".(", i, " = NA,",
                                  " ", i, "period = mean(", i, "period, na.rm = TRUE),
                                  E", i, " = sum(E", i, ", na.rm = TRUE),
                                  VE", i, " = sum(VE", i, ", na.rm = TRUE),
                                  EA", i, " = sum(EA", i, ", na.rm = TRUE),
                                  VEA", i, " = sum(VEA", i, ", na.rm = TRUE))"))
      pooledData <- merge(pooledData, AttData[group %in% pooled, eval(expr), keyby = ISOweek], by = "ISOweek", all.x = TRUE)
    }
    AttData <- rbind(AttData, pooledData)
  }

  AttData <- cbind(country, AttData)
  AttData <- AttData[order(country, group, ISOweek)]

  return(AttData)
}
