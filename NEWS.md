## AttMOMO 2024.06.18
AttMOMO_cut return a list of two elements: AttData (previously only return) and R2 containing R-square values for the estimations.

## AttMOMO 2023.06.26
Included and adjust for population size in the AttMOMOCut and AttMOMO_estimationCut.
If a ;-separated population_data.txt file is available in wdir/data

# AttMOMO 2023.04.11
Included parameter with cut-weeks for each indicator.
Two new funcions: AttMOMOCut and AttMOMO_estimationCut,
Where AttMOMOCut is a wrapper for AttMOMO_estimationCut
 
```{r eval = FALSE}
AttMOMOCut(
  country = "Denmark",
  wdir = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/AttMOMO_DK",
  StartWeek = StartWeek,
  EndWeek = EndWeek,
  groups = c('00to14', '15to44', '45to64', '65to74', '75to84', '85P', 'Total'),
  pooled = c('00to14', '15to44', '45to64', '65to74', '75to84', '85P'),
  indicators = c("RSVPosInc", "InflPosInc", "COVID19PosInc"),
  indicatorCuts = list(`RSVPosInc` = c("2015-W21", "2016-W21", "2017-W21", "2018-W21", "2019-W21", "2020-W21", "2021-W21", "2022-W21"),
                       `InflPosInc` = c("2015-W40", "2016-W40", "2017-W40", "2018-W40", "2019-W40", "2020-W40", "2021-W40", "2022-W40"),
                       `COVID19PosInc` = c("2020-W01", "2021-W01", "2021-W26", "2021-W52", "2022-W27")),
  lags = 3,
  ptrend = 0.05,
  p26 = 0.05,
  p52 = 0.10,
  Rdata = FALSE
)
```

## AttMOMO 2021.09.29
Removed library-statements.

## AttMOMO 2021.03.05
AttMOMO available as a R-package.
```{r eval = FALSE}
devtools::install_github("JensXII/AttMOMO")
```

