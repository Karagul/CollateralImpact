## Analysis of effect of collateral on account performance 
# save.image(here::here('PerfCollateral.RData'))

#### 0. Load workspace and required packages ----

### 0.1 Required packages ----
library(tidyverse)
library(dbplyr)
library(lubridate)
library(forcats)
library(odbc)
library(readxl)
library(here)
library(xlsx)

### 0.2 Parameters setting ----
database_LLD <- odbc::dbConnect(
  odbc::odbc(),
  driver = "SQL Server",
  server = "ALF-DATAWH-01",
  database = "LLD"
)
schema_lld <- 'lld'
table_lld <- 'lld_data_new_apr_2018'
schema_orig <- 'lld'
table_orig <- 'origination_data_apr_2018'

current_month <- '2018-06-01'
default_threshold <- 90 # 90+DPD within first 6 months as 'bad/default'

#### 1. Read and format data ----

### 1.1 Read in performance and origination data ----
alf_lld <- dplyr::tbl(database_LLD, dbplyr::in_schema(schema_lld, table_lld)) %>%
  collect()
alf_orig <- dplyr::tbl(database_LLD, dbplyr::in_schema(schema_orig, table_orig)) %>%
  collect()
alf_lld %>%
  top_n(1000) %>%
  View()

### 1.2 Gather collateral related information ----
alf_collateral <- alf_orig %>%
  select(
      AccountNumber
    , ActualInterestRatePct
    , SubsequentContractualPayment
    , ContractTerm
    , CreditScore
    , PTIPct
    , DTIPct
    , MonthlyIncomeAmount
    , ContractTerm
    , AmountFinanced
    , VehicleEstimatedValue
    , LTVPct
    , VehicleAge
    , Mileage
    , ProductName
  )

### 1.3 Define performance threshold as reaching 60+DPD within first six months ----

## 1.3.1 Consider loans that started performance at least six months ago ----
alf_lld_select <- alf_lld %>%
  mutate(
      Vintage = str_c(Vintage, '-01')
    , Vintage = ymd(Vintage)
    , MonthsSinceOrig = interval(Vintage, ymd(ReportingDate)) %/% months(1)
  ) %>%
  filter(
      Vintage <= ymd(current_month) %m-% months(6)
    , MonthsSinceOrig <= 6 # define default as reach 60+DPD within first 6 months
  ) 

## 1.3.2 Bad accounts based on performance threshold defined ----
alf_bad <- alf_lld_select %>%
  group_by(
    AccountNumber
  ) %>%
  mutate(
      CumDaysDelinquent = cummax(DaysDelinquent)
    , IsDefault = ifelse(CumDaysDelinquent > default_threshold, 1, 0)
  ) %>%
  filter(
    cumany(CumDaysDelinquent > default_threshold) 
  ) %>%
  filter(
    MonthsSinceOrig == min(MonthsSinceOrig)
  ) %>%
  ungroup()

## 1.3.3 Good accounts based on performance threshold defined ----
alf_good <- alf_lld_select %>%
  group_by(
    AccountNumber
  ) %>% 
  mutate(
      CumDaysDelinquent = cummax(DaysDelinquent)
    , IsDefault = ifelse(CumDaysDelinquent > default_threshold, 1, 0)
  ) %>%
  # filter(
  #     IsDefault == 0
  #   , MonthsSinceOrig == 6
  # ) %>%
  filter(
    !cumany(CumDaysDelinquent > default_threshold) 
  ) %>% View()
  filter(
    MonthsSinceOrig == max(MonthsSinceOrig)
  ) %>%
  ungroup()

## 1.3.4 Combine good and bad ----
alf_performance <- alf_bad %>%
  bind_rows(alf_good)

### 1.4 Joint performance data with collateral information ----
alf_collateral_perf <- alf_performance %>%
  mutate(AccountNumber = bit64::as.double.integer64(AccountNumber)) %>% 
  select(
      AccountNumber
    , Vintage
    , ReportingDate
    , MonthsSinceOrig
    , DaysDelinquent
    , IsBad = IsDefault
  ) %>%
  left_join(
    alf_collateral,
    by = c('AccountNumber' = 'AccountNumber')
  ) %>%
  mutate(
      IsBad = factor(IsBad, labels = c('Good', 'Bad'))
    , VehicleType = ifelse(str_detect(ProductName, regex('^used', ignore_case = TRUE)),
                           'Used', 'New')
  )

#### 2. Compare performance based on collateral info ----

### 2.1 Impact of 'VehicleType' on performance ----
table(alf_collateral_perf[, c('IsBad', 'VehicleType')])
round(
  prop.table(table(alf_collateral_perf[, c('IsBad', 'VehicleType')]), margin = 2) * 100, 2
)

new_perf <- alf_collateral_perf %>%
  filter(VehicleType == 'New')

used_perf <- alf_collateral_perf %>%
  filter(VehicleType == 'Used' )

### 2.2 Impact of 'VehicleAge' on performance of 'Used' ----
used_vage_tbl <- used_perf %>%
  group_by(VehicleAge) %>%
  summarise(
      Count = n()
    , CountBad = sum(IsBad == 'Bad')
    , BadRatio = round(CountBad / Count * 100, 2)
    , AvgDaysDelinq = mean(DaysDelinquent)
  )

## 2.2.1 Bar plot of bad ratios ---- 
used_vage_tbl %>%
  ggplot(aes(VehicleAge, BadRatio, fill = desc(BadRatio))) +
  geom_col() + 
  # coord_flip() + # rotate bars 
  scale_x_discrete(limits = vehicle_age_tbl$VehicleAge) +
  labs(
    title = '"Bad Ratio" of each vehicle age group',
    caption = '"Bad" is defined as reaching 120+DPD within first 6 months'
  ) + 
  theme(legend.position="none")

used_vage_tbl %>%
  ggplot(aes(VehicleAge, AvgDaysDelinq, fill = desc(AvgDaysDelinq))) +
  geom_col() +
  scale_x_discrete(limits = vehicle_age_tbl$VehicleAge) +
  labs(
    title = 'Average "Days Delinquent" of each vehicle age group'
  ) + 
  theme(legend.position="none")

### 2.3 Impact of 'Mileage' on performance of 'Used' ----
used_mileage_tbl <- used_perf %>%
  mutate(
    mileage_buckets = forcats::fct_explicit_na(
      cut(
        x = Mileage,
        breaks = unname(quantile(Mileage, probs = seq(0, 1, 0.125))),
        labels = str_c('<=', round(unname(quantile(Mileage, probs = seq(0, 1, 0.125))), 0))[-1],
        include.lowest = TRUE
      )
    )
  ) %>% 
  group_by(mileage_buckets) %>%
  summarise(
      Count = n()
    , CountBad = sum(IsBad == 'Bad')
    , BadRatio = round(CountBad / Count * 100, 2)
    , AvgDaysDelinq = mean(DaysDelinquent)
  )
  
## 2.3.1 Bar plot of bad ratios ----
used_mileage_tbl %>%
  ggplot(aes(mileage_buckets, BadRatio, fill = desc(BadRatio))) +
  geom_col() + 
  labs(
    title = '"Bad Ratio" of each mileage bucket',
    caption = '"Bad" is defined as reaching 60+DPD within first 6 months'
  ) + 
  theme(legend.position="none")

used_mileage_tbl %>%
  ggplot(aes(mileage_buckets, AvgDaysDelinq, fill = desc(AvgDaysDelinq))) +
  geom_col() +
  labs(
    title = 'Average "Days Delinquent" of each vehicle age group'
  ) + 
  theme(legend.position="none")

### 2.4 Impact of 'EstimatedValue' on performance of 'Used' ----
used_value_tbl <- used_perf %>%
  mutate(
    value_buckets = forcats::fct_explicit_na(
      cut(
        x = VehicleEstimatedValue,
        breaks = unname(quantile(VehicleEstimatedValue, probs = seq(0, 1, 0.125))),
        labels = str_c('<=', round(unname(quantile(VehicleEstimatedValue, probs = seq(0, 1, 0.125))), 0))[-1],
        include.lowest = TRUE
      )
    )
  ) %>% 
  group_by(value_buckets) %>%
  summarise(
      Count = n()
    , CountBad = sum(IsBad == 'Bad')
    , BadRatio = round(CountBad / Count * 100, 2)
    , AvgDaysDelinq = mean(DaysDelinquent)
  )

## 2.4.1 Bar plot of bad ratios ----
used_value_tbl %>%
  ggplot(aes(value_buckets, BadRatio, fill = desc(BadRatio))) +
  geom_col() + 
  labs(
    title = '"Bad Ratio" of each mileage bucket',
    caption = '"Bad" is defined as reaching 60+DPD within first 6 months'
  ) + 
  theme(legend.position="none")

### 2.5 Impact of 'LTVPct' on performance of 'Used' ----
used_ltv_tbl <- used_perf %>%
  mutate(
    ltv_buckets = forcats::fct_explicit_na(
      cut(
        x = LTVPct,
        breaks = unname(quantile(LTVPct, probs = seq(0, 1, 0.125))),
        labels = str_c('<=', round(unname(quantile(LTVPct, probs = seq(0, 1, 0.125))) * 100, 2))[-1],
        include.lowest = TRUE
      )
    )
  ) %>% 
  group_by(ltv_buckets) %>%
  summarise(
    Count = n()
    , CountBad = sum(IsBad == 'Bad')
    , BadRatio = round(CountBad / Count * 100, 2)
    , AvgDaysDelinq = mean(DaysDelinquent)
  )

used_perf %>%
  rename(Performance = IsBad) %>%
  ggplot(aes(VehicleEstimatedValue, fill = Performance)) +
  geom_density(alpha = .5, position = 'identity')

used_perf %>%
  rename(Performance = IsBad) %>%
  ggplot(aes(LTVPct, fill = Performance)) +
  geom_density(alpha = .5, position = 'identity')





