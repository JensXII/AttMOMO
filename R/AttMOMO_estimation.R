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
#' @param death_data Name of deaths data file in memory.
#' @param ET_data Name of ET data file in memory.
#' @param lags weeks of lagged effect (default = 3, max = 9)
#' @param ptrend significance of trend to be included (default = 0.05)
#' @param p26 significance of half year-sine be included (default = 0.05)
#' @param p52 significance of year-sine be included (default = 0.10)
#' @import data.table
#' @return data with weekly estimated means and variances
#' @export
AttMOMO_estimation <- function(country, StartWeek, EndWeek, groups, pooled = NULL, indicators, death_data, ET_data,
                               lags = 3, ptrend = 0.05, p26 = 0.05, p52 = 0.10) {
  group <- ET <- summer <- winter <- EB <- EAB <- season <- deaths <- VEB <- EET <- VEET <- VEAB <- wk <- . <- anova <- glm <- median <- residuals <- df.residual <- predict.glm <- quasipoisson <- NULL
  library(data.table)

  # country <- "Denmark"
  # StartWeek <- '2014-W27'
  # EndWeek <- '2020-W22'
  # groups = c('00to14', '15to44', '45to64', '65to74', '75to84', '85P', 'Total')
  # pooled <- c('00to14', '15to44', '45to64', '65to74', '75to84', '85P')
  # indicators <- c('GSIPLS', 'GSCLS')
  # death_data <- death_data
  # ET_data <- ET_data
  # lags <- 3
  # ptrend <- 0.05
  # p26 <- 0.05
  # p52 <- 0.10


  # Read and merge data -----------------------------------------------------
  # death data
  AttData <- data.table(death_data)
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

  # Extreme temperature data
  ET_data <- data.table(ET_data)
  if (sum(colnames(ET_data) %in% c('ISOweek', 'ET')) != 2) {
    stop('Columns ISOweek, deaths not in ET_date')
  }
  if ((min(ET_data$ISOweek) > StartWeek) | (max(ET_data$ISOweek) < EndWeek)) {
    stop(paste("ET_data do not cover", StartWeek, "to", EndWeek))
  }
  AttData <- merge(AttData, ET_data[order(ISOweek), ], by = "ISOweek")

  # Indicator data
  for (i in indicators) {
    X <- try(data.table(eval(parse(text = paste0(i, '_data')))))
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
  AttData[, `:=`(season = as.numeric(substr(ISOweek,1,4)) - (as.numeric(substr(ISOweek,7,8)) < 27),
                 summer = as.numeric((21 <= as.numeric(substr(ISOweek,7,8))) & (as.numeric(substr(ISOweek,7,8)) <= 39)),
                 winter = as.numeric((20 >= as.numeric(substr(ISOweek,7,8))) | (as.numeric(substr(ISOweek,7,8)) >= 40)))]
  # Warm/cold summer/winter
  AttData[, `:=`(cold_summer = -((ET < 0) * ET) * summer,
                 warm_summer = ((ET > 0) * ET) * summer,
                 cold_winter = -((ET < 0) * ET) * winter,
                 warm_winter = ((ET > 0) * ET) * winter)]
  AttData[, `:=`(summer = NULL, winter = NULL)]
  # lags
  for (i in c(indicators, c('cold_summer', 'warm_summer', 'cold_winter', 'warm_winter'))) {
    for (l in 0:lags) {
      expr <- parse(text = paste0(i, "_d", l, " := shift(", i, ", ", l, ", type = 'lag')"))
      AttData[, eval(expr), by = group]
    }
  }
  # indicator by season
  for (i in indicators) {
    for (l in 0:lags) {
      for (s in unique(AttData$season)) {
        expr <- parse(text = paste0(i, "_d", l, "_", s, " := ifelse(", s, " == season, ", i, "_d", l, ", 0)"))
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
  AttData.B <- copy(AttData)
  for (l in 0:lags) {
    for (i in c('cold_summer', 'warm_summer', 'cold_winter', 'warm_winter')) {
      expr <- parse(text = paste0(i, "_d", l, ":= 0"))
      AttData.B[, eval(expr)]
    }
    for (i in indicators) {
      for (s in unique(AttData$season)) {
        expr <- parse(text = paste0(i, "_d", l, "_", s, " := 0"))
        AttData.B[, eval(expr)]
      }
    }
  }
  # ET
  AttData.ET <- copy(AttData)
  AttData.ET[, `:=`(const = 0, wk = 0, sin52 = 0, cos52 = 0, sin26 = 0, cos26 = 0)]
  for (i in indicators) {
    for (l in 0:lags) {
      for (s in unique(AttData$season)) {
        expr <- parse(text = paste0(i, "_d", l, "_", s, " := 0"))
        AttData.ET[, eval(expr)]
      }
    }
  }
  # indicators
  for (i in indicators) {
    X <- copy(AttData)
    X[, `:=`(const = 0, wk = 0, sin52 = 0, cos52 = 0, sin26 = 0, cos26 = 0)]
    for (l in 0:lags) {
      for (ir in c('cold_summer', 'warm_summer', 'cold_winter', 'warm_winter')) {
        expr <- parse(text = paste0(ir, "_d", l, ":= 0"))
        X[, eval(expr)]
      }
      for (ir in indicators[indicators != i]) {
        for (s in unique(AttData$season)) {
          expr <- parse(text = paste0(ir, "_d", l, "_", s, " := 0"))
          X[, eval(expr)]
        }
      }
    }
    assign(paste0("AttData.", i), X)
  }
  rm(X)

  # Estimation --------------------------------------------------------------
  # V <- vcov(m) # covariance matrix
  # non-baseline parameters
  parm <- paste(grep("_d[0-9]", names(AttData), value=TRUE), collapse = " + ")

  for (g in groups) {
    print(paste("### Group", g, "###"))

    f <- paste(c("deaths ~ -1 + const + wk", "sin52 + cos52", "sin26 + cos26", parm), collapse = " + ")
    m <- try(glm(f, quasipoisson(identity), data = AttData[group == g,]), silent = TRUE)
    if ((!inherits(m, "try-error")) & (median(AttData[group == g,]$deaths) > 0)) {
      if (m$converged) {
        fa <- paste(c("deaths ~ -1 + const + wk", parm), collapse = " + ")
        ma <- glm(fa, quasipoisson(identity), data = AttData[group == g,])
        if (anova(m, ma, dispersion = max(1, sum(residuals(m, type = "deviance")^2)/df.residual(m)), test="LRT")$`Pr(>Chi)`[2] > max(p52, p26)) {
          m <- ma
        } else {
          fa <- paste(c("deaths ~ -1 + const + wk", "sin52 + cos52", parm), collapse = " + ")
          ma <- glm(fa, quasipoisson(identity), data = AttData[group == g,])
          if (anova(m, ma, dispersion = max(1, sum(residuals(m, type = "deviance")^2)/df.residual(m)), test="LRT")$`Pr(>Chi)`[2] > p26) {
            m <- ma
          } else {
            fa <- paste(c("deaths ~ -1 + const + wk", parm), collapse = " + ")
            ma <- glm(fa, quasipoisson(identity), data = AttData[group == g,])
            if (anova(m, ma, dispersion = max(1, sum(residuals(m, type = "deviance")^2)/df.residual(m)), test="LRT")$`Pr(>Chi)`[2] > p52) {
              m <- ma
            }
          }
        }
      } else {
        f <- paste(c("deaths ~ -1 + const + wk", parm), collapse = " + ")
        m <- try(glm(f, quasipoisson(identity), data = AttData[group == g,]), silent = TRUE)
        if ((inherits(m, "try-error")) | (!m$converged)) {
          print(paste("The model did not converge. A Simple model with only trend used i.e. no effect of indicators"))
          f <- paste(c("deaths ~ -1 + const + wk"))
          m <- glm(f, quasipoisson(identity), data = AttData[group == g,])
        }
      }
    } else {
      if (median(AttData[group == g,]$deaths) == 0) {msg <- "Zero inflated." } else {msg <- NULL}
      if (inherits(m, "try-error")) print(paste("Could not fit model.", msg, "Simple model with only trend used i.e. no effect of indicators"))
      f <- paste(c("deaths ~ -1 + const + wk"))
      m <- glm(f, quasipoisson(identity), data = AttData[group == g,])
    }

    # Remove NA colinearity
    f <- paste("deaths ~ -1 +", paste(names(m$coefficients[!is.na(m$coefficients)]), collapse = ' + '))

    m <- glm(f, quasipoisson(identity), data = AttData[group == g,])

    print(summary(m, dispersion = max(1, sum(residuals(m, type = "deviance")^2)/df.residual(m))))

    # Predictions -------------------------------------------------------------

    # Prediction baseline
    AttData[group == g, `:=`(EB = predict.glm(m, newdata = AttData.B[group == g,], se.fit=TRUE)$fit)]
    AttData[group == g, `:=`(VEB = (max(1, sum(residuals(m)^2)/df.residual(m)))*EB +
                               predict.glm(m, newdata = AttData.B[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)]
    #  Prediction ET
    AttData[group == g, `:=`(EET = predict.glm(m, newdata = AttData.ET[group == g,], se.fit=TRUE)$fit,
                             VEET = predict.glm(m, newdata = AttData.ET[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)]

    # Predictions indicators
    for (i in indicators) {
      expr <- parse(text = paste0("`:=`(E", i, " = predict.glm(m, newdata = AttData.", i, "[group == g,], se.fit=TRUE)$fit,
      VE", i, " = predict.glm(m, newdata = AttData.", i, "[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)"))
      AttData[group == g, eval(expr)]
    }

    # Adjusted baseline
    X <- copy(AttData)
    for (i in indicators) {
      for (l in 0:lags) {
        for (s in unique(AttData$season)) {
          expr <- parse(text = paste0(i, "_d", l, "_", s, " := ifelse(E", i," >= 0, 0,", i, "_d", l, "_", s,")"))
          X[, eval(expr)]
        }
      }
    }
    AttData[group == g, `:=`(EAB = predict.glm(m, newdata = X[group == g,], se.fit=TRUE)$fit)]
    AttData[group == g, `:=`(VEAB = (max(1, sum(residuals(m)^2)/df.residual(m)))*EAB +
                               predict.glm(m, newdata = X[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)]

    # Adjusted predictions indicators
    for (i in indicators) {
      X <- copy(AttData)
      X[, `:=`(const = 0, wk = 0, sin52 = 0, cos52 = 0, sin26 = 0, cos26 = 0)]
      for (l in 0:lags) {
        for (ir in c('cold_summer', 'warm_summer', 'cold_winter', 'warm_winter')) {
          expr <- parse(text = paste0(ir, "_d", l, ":= 0"))
          X[, eval(expr)]
        }
        for (l in 0:lags) {
          for (s in unique(AttData$season)) {
            for (ir in indicators[indicators != i]) {
              expr <- parse(text = paste0(ir, "_d", l, "_", s, " := 0"))
              X[, eval(expr)]
            }
            expr <- parse(text = paste0(i, "_d", l, "_", s, " := ifelse(E", i," < 0, 0,", i, "_d", l, "_", s,")"))
            X[, eval(expr)]
          }
        }
      }
      expr <- parse(text = paste0("`:=`(EA", i, " = predict.glm(m, newdata = X[group == g,], se.fit=TRUE)$fit,
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
      for (s in unique(AttData$season)) {
        expr <- parse(text = paste0(i, "_d", l, "_", s, " := NULL"))
        AttData[, eval(expr)]
      }
    }
  }
  AttData[, season := NULL]
  # rm(f, fa, expr, parm, g, i, l, m, ma, AttData.B, AttData.ET)
  rm(fa, expr, parm, g, i, l, ma, AttData.B, AttData.ET)

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
      expr <- parse(text = paste0(".(", i, " = NA,
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

# # Residual autocorellation and heteroskedasticity
# g <- 'Total'
# parm <- paste(grep("_d[0-9]", names(AttData), value=TRUE), collapse = " + ")
# f <- paste(c("deaths ~ 1 + wk", "sin52 + cos52", "sin26 + cos26", parm), collapse = " + ")
# m<- glm(f, quasipoisson(identity), data = AttData[group == g,])
# summary(m)
# res <- data.table(res = m$residuals)
# res$n <- 1:nrow(res)
# X <- acf(res$res)
# mod2 = lm(res ~ n, data = res)
# summary(mod2)
# plot(res$res ~ res$n)


# Get R2 - don't work in function
# print(logLik(m, REML = FALSE))
# X <- subset(AttData, group == "Total")
# m <- glm2::glm2(f, quasipoisson(identity), data = AttData[which(AttData$group == "Total"),])
# R2 <- rsq::rsq(m, adj = FALSE, type = 'v')
# Adj.R2 <- rsq::rsq(m, adj = TRUE, type = 'v')
# print(paste("R2:", R2))
# print(paste("Adj.R2:", Adj.R2))
