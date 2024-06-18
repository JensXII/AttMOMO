## AttMOMO - Attributable Mortality Model

Estimate number of deaths attributable to one or more pathogens, adjusted for excess temperatures

https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2021.26.8.2001646

# AttMOMO 2024.06.18
AttMOMO_cut return a list of two elements: AttData (previously only return) and R2 containing R-square values for the estimations.

# AttMOMO 2023.06.26
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

# AttMOMO 2021.09.29
Removed library-statements.

# AttMOMO 2021.03.05
AttMOMO available as a R-package.

```{r eval = FALSE}
devtools::install_github("JensXII/AttMOMO")
```

# AttMOMO
Wrapper for AttMOMO_estimation  
Read input data from ;-separated .txt files. Which all must be available in wdir/data  
Create output subdirectory wdir/AttMOMO_'EndWeek'  
with the subdirectories:  
 /data - a copy of input data  
 /output - contain output from AttMOMO_estimation  
Return estimates as a ;-separated .txt file in /output  
 if Rdata = TRUE also data.
 
```{r eval = FALSE}
AttData <- AttMOMO(
  country <- "Denmark"
  wdir <- "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark"
  StartWeek <- '2017-W38'
  EndWeek <- '2023-W12'
  groups = c('00to14', '15to44', '45to64', '65to74', '75to84', '85P', 'Total')
  pooled <- c('00to14', '15to44', '45to64', '65to74', '75to84', '85P')
  indicators <- c('GSIPLS', 'GSCLS')
  lags <- 3
  ptrend <- 0.05
  p26 <- 0.05
  p52 <- 0.10
  Rdata <- TRUE
)
```

# AttMOMO_estimation
Read and merge input data  
Prepare data for estimation e.g. create lags  
Make the estimations by groups  
Make pooled estimates over selected groups (optional)  
Return a data.table with input data, mean estimated number of attributable deaths and their variances  

Demand that the following data are available i R:  
death_data - containing the variables: group, ISOweek, deaths  
ET_data - containing the variables: ISOweek, ET (= excess temperature). Can be achieved via GetWdata and GetET  
One dataset for each pathogen indicator. Containing the variables: group, ISOweek, 'indicator name' (= indicators nominel value)  

```{r eval = FALSE}
AttData <- AttMOMO_estimation(
  country <- "Denmark"
  StartWeek <- '2014-W27'
  EndWeek <- '2020-W22'
  groups = c('00to14', '15to44', '45to64', '65to74', '75to84', '85P', 'Total')
  pooled <- c('00to14', '15to44', '45to64', '65to74', '75to84', '85P')
  indicators <- c('GSIPLS', 'GSCLS')
  death_data <- death_data
  ET_data <- ET_data
  lags <- 3
  ptrend <- 0.05
  p26 <- 0.05
  p52 <- 0.10
)
```

# GetWdata
Read weather data achieved from EuroMOMO.  
Select specified NUTS-codes  
Return data with: date, pop3, NUTS3, temp, mintemp, maxtemp  

# GetET
Excess Temperature data from weather data.  
Input data must contain: date, pop3, NUTS3 and the name of the variable to be used to calculate Excess Temperatures  
Return data.table with ISOweek and ET (excess temperature)  

# GetMOMOdata
Convert an A-MOMO complete file to an AttMOMO input file.  
