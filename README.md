## AttMOMO - Attributable Mortality Model

Estimate number of deaths attributable to one or more pathogens, adjusted for excess temparatures.

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
AttData <- AttMOMO_estimation(
  country <- "Denmark"
  wdir <- "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark"
  StartWeek <- '2014-W27'
  EndWeek <- '2020-W22'
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
