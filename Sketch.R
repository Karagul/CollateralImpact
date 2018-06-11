## sketch
#### 0. Define good and bad ----
accounts <- alf_lld_select %>%
  group_by(AccountNumber) %>%
  summarise(ReportingDate = min(ReportingDate)) %>%
  select(AccountNumber)

accounts_combine <- alf_lld_select %>% 
  filter(!is.na(DaysDelinquent)) %>%
  group_by(AccountNumber) %>%
  mutate(
      RowNumber = row_number()
    , CumMaxDaysDelinquent = cummax(DaysDelinquent)
  ) %>% 
  group_by(AccountNumber) %>%
  filter(
      RowNumber == n()
    , CumMaxDaysDelinquent > default_threshold
  ) %>%
  bind_rows(
    alf_lld_select %>% 
      filter(!is.na(DaysDelinquent)) %>%
      group_by(AccountNumber) %>%
      mutate(
          RowNumber = row_number()
        , CumMaxDaysDelinquent = cummax(DaysDelinquent)
      ) %>% 
      group_by(AccountNumber) %>%
      filter(
          RowNumber == n()
        , CumMaxDaysDelinquent <= default_threshold
      )
  ) %>%
  select(AccountNumber)

dplyr::setdiff(bit64::as.character.integer64(accounts$AccountNumber),
               bit64::as.character.integer64(accounts_combine$AccountNumber))

alf_lld_select %>%
  filter(AccountNumber == 9100046904) %>%
  mutate(
    CumMaxDDays = cummax(DaysDelinquent)
  ) %>%
  View()

alf_bad <- alf_lld_select %>% 
  group_by(AccountNumber) %>%
  mutate(
      RowNumber = row_number()
    , CumMaxDaysDelinquent = cummax(DaysDelinquent)
  ) %>% 
  group_by(AccountNumber) %>%
  filter(
      RowNumber == n()
    , CumMaxDaysDelinquent <= default_threshold
  ) %>%
  View()
  

alf_lld_group <- alf_lld_select %>%
  group_by(
    AccountNumber
  ) %>%
  mutate(
      CumDaysDelinquent = cummax(DaysDelinquent)
    , IsDefault = ifelse(CumDaysDelinquent > default_threshold, 1, 0)
  )

alf_bad <- alf_lld_group %>%
  select(
      AccountNumber
    , Vintage
    , ReportingDate
    , DaysDelinquent
    , MonthsSinceOrig
    , CumDaysDelinquent
    , IsDefault
  ) %>% 
  filter(
    cumany(CumDaysDelinquent > default_threshold)
  ) %>% 
  filter(
    MonthsSinceOrig == min(MonthsSinceOrig)
  ) %>%
  ungroup()

alf_good <- alf_lld_group %>%
  select(
      AccountNumber
    , Vintage
    , ReportingDate
    , DaysDelinquent
    , MonthsSinceOrig
    , CumDaysDelinquent
    , IsDefault
  ) %>% 
  filter(
    cumany(CumDaysDelinquent > default_threshold)
  ) %>% 
  filter(
    MonthsSinceOrig == max(MonthsSinceOrig)
  ) %>%
  ungroup()




used_perf_byAge <- used_perf %>%
  group_by(VehicleAge) %>%
  arrange(VehicleAge) %>%
  nest()

names(used_perf_byAge$data) <- used_perf_byAge$VehicleAge

## FICO ----
used_perf_byAge %>%
  mutate(
    fico = map(data, 'CreditScore'),
    fico_noscorepct = map_dbl(
      fico,
      function(scores) {
        return(round( sum(scores == 0) / length(scores) * 100, 2 ))
      }
    ),
    fico_min = map_dbl(
      fico,
      function(scores) {
        return(summary(scores[scores != 0])[['Min.']])
      }
    ),
    fico_q1 = map_dbl(
      fico,
      function(scores) {
        return(summary(scores[scores != 0])[['1st Qu.']])
      }
    ),
    fico_median = map_dbl(
      fico,
      function(scores) {
        return(summary(scores[scores != 0])[['Median']])
      }
    ),
    fico_mean = map_dbl(
      fico,
      function(scores) {
        return(summary(scores[scores != 0])[['Mean']])
      }
    ),
    fico_q3 = map_dbl(
      fico,
      function(scores) {
        return(summary(scores[scores != 0])[['3rd Qu.']])
      }
    ),
    fico_max = map_dbl(
      fico,
      function(scores) {
        return(summary(scores[scores != 0])[['Max.']])
      }
    )
  )

used_perf %>%
  filter(
      VehicleAge == 0
    | VehicleAge == 1
  ) %>%
  mutate(
    VehicleAge = factor(VehicleAge)
  ) %>%
  ggplot(aes(CreditScore, fill = VehicleAge)) +
  geom_histogram(alpha = .5, position = 'identity') +
  labs(title = 'FICO distribution, "Used" age 0 and 1')

## Mileage ----
used_perf_byAge %>%
  mutate(
    mileage = map(data, 'Mileage'),
    mileage_min = map_dbl(
      mileage,
      function(series) {
        return(summary(series[series != 0])[['Min.']])
      }
    ),
    mileage_q1 = map_dbl(
      mileage,
      function(series) {
        return(summary(series[series != 0])[['1st Qu.']])
      }
    ),
    mileage_median = map_dbl(
      mileage,
      function(series) {
        return(summary(series[series != 0])[['Median']])
      }
    ),
    mileage_mean = map_dbl(
      mileage,
      function(series) {
        return(summary(series[series != 0])[['Mean']])
      }
    ),
    mileage_q3 = map_dbl(
      mileage,
      function(series) {
        return(summary(series[series != 0])[['3rd Qu.']])
      }
    ),
    mileage_max = map_dbl(
      mileage,
      function(series) {
        return(summary(series[series != 0])[['Max.']])
      }
    )
  ) %>%
  select(
    -data, -mileage
  )

used_perf %>%
  filter(
    VehicleAge == 0
    | VehicleAge == 1
  ) %>%
  mutate(
    VehicleAge = factor(VehicleAge)
  ) %>%
  ggplot(aes(Mileage, fill = VehicleAge)) +
  geom_histogram(alpha = .5, position = 'identity') +
  labs(title = 'Mileage distribution, "Used" age 0 and 1')

## PTIPct ----
used_perf_byAge %>%
  mutate(
    pti = map(data, 'PTIPct'),
    pti_min = map_dbl(
      pti,
      function(series) {
        return(summary(series[series != 0])[['Min.']])
      }
    ),
    pti_q1 = map_dbl(
      pti,
      function(series) {
        return(summary(series[series != 0])[['1st Qu.']])
      }
    ),
    pti_median = map_dbl(
      pti,
      function(series) {
        return(summary(series[series != 0])[['Median']])
      }
    ),
    pti_mean = map_dbl(
      pti,
      function(series) {
        return(summary(series[series != 0])[['Mean']])
      }
    ),
    pti_q3 = map_dbl(
      pti,
      function(series) {
        return(summary(series[series != 0])[['3rd Qu.']])
      }
    ),
    pti_max = map_dbl(
      pti,
      function(series) {
        return(summary(series[series != 0])[['Max.']])
      }
    )
  ) %>%
  select(-data, -pti) 
  
used_perf %>%
  filter(
    VehicleAge == 0
    | VehicleAge == 1
  ) %>%
  mutate(
    VehicleAge = factor(VehicleAge)
  ) %>%
  ggplot(aes(PTIPct, fill = VehicleAge)) +
  geom_histogram(alpha = .5, position = 'identity') +
  labs(title = 'PTI distribution, "Used" age 0 and 1')


## Interest Rate ----
used_perf %>%
  filter(
      ActualInterestRatePct >= 16.95
    , ActualInterestRatePct <= 25.95
  ) %>%
  group_by(VehicleAge) %>%
  nest() %>%
  mutate(
    rate = map(data, 'ActualInterestRatePct'),
    rate_min = map_dbl(
      rate,
      function(series) {
        return(summary(series[series != 0])[['Min.']])
      }
    ),
    rate_q1 = map_dbl(
      rate,
      function(series) {
        return(summary(series[series != 0])[['1st Qu.']])
      }
    ),
    rate_median = map_dbl(
      rate,
      function(series) {
        return(summary(series[series != 0])[['Median']])
      }
    ),
    rate_mean = map_dbl(
      rate,
      function(series) {
        return(summary(series[series != 0])[['Mean']])
      }
    ),
    rate_q3 = map_dbl(
      rate,
      function(series) {
        return(summary(series[series != 0])[['3rd Qu.']])
      }
    ),
    rate_max = map_dbl(
      rate,
      function(series) {
        return(summary(series[series != 0])[['Max.']])
      }
    )
  ) %>%
  select(-data, -rate) 

used_perf %>%
  filter(
    VehicleAge == 0
    | VehicleAge == 1
  ) %>%
  mutate(
    VehicleAge = factor(VehicleAge)
  ) %>%
  ggplot(aes(ActualInterestRatePct, fill = VehicleAge)) +
  geom_histogram(alpha = .5, position = 'identity') +
  labs(title = 'Interest rate distribution, "Used" age 0 and 1')

## Income ----
used_perf_byAge %>%
  mutate(
    income = map(data, 'MonthlyIncomeAmount'),
    income_min = map_dbl(
      income,
      function(series) {
        return(summary(series[series != 0])[['Min.']])
      }
    ),
    income_q1 = map_dbl(
      income,
      function(series) {
        return(summary(series[series != 0])[['1st Qu.']])
      }
    ),
    income_median = map_dbl(
      income,
      function(series) {
        return(summary(series[series != 0])[['Median']])
      }
    ),
    income_mean = map_dbl(
      income,
      function(series) {
        return(summary(series[series != 0])[['Mean']])
      }
    ),
    income_q3 = map_dbl(
      income,
      function(series) {
        return(summary(series[series != 0])[['3rd Qu.']])
      }
    ),
    income_max = map_dbl(
      income,
      function(series) {
        return(summary(series[series != 0])[['Max.']])
      }
    )
  ) %>%
  select(-data, -income) 


used_perf %>%
  filter(
    VehicleAge == 0
    | VehicleAge == 1
  ) %>%
  mutate(
    VehicleAge = factor(VehicleAge)
  ) %>%
  ggplot(aes(MonthlyIncomeAmount, fill = VehicleAge)) +
  geom_histogram(alpha = .5, position = 'identity') +
  labs(title = 'Income distribution, "Used" age 0 and 1')


## Amount Financed ----
used_perf %>%
  group_by(VehicleAge) %>%
  summarise(
      amount_min = min(AmountFinanced)
    , amount_q1 = quantile(AmountFinanced)[['25%']]
    , amount_median = median(AmountFinanced)
    , amount_mean = mean(AmountFinanced)
    , amount_q3 = quantile(AmountFinanced)[['75%']]
    , amount_max = max(AmountFinanced)
  )

## Payment ----
used_perf %>%
  group_by(VehicleAge) %>%
  summarise(
      payment_min = min(SubsequentContractualPayment)
    , payment_q1 = quantile(SubsequentContractualPayment)[['25%']]
    , payment_median = median(SubsequentContractualPayment)
    , payment_mean = mean(SubsequentContractualPayment)
    , payment_q3 = quantile(SubsequentContractualPayment)[['75%']]
    , payment_max = max(SubsequentContractualPayment)
  )

## Repo Status ----
alf_lld_select %>%
  select(
      AccountNumber
    , Vintage
    , ReportingDate
    , contains('repo')
    , MonthsSinceOrig
  ) %>%
  group_by(
    AccountNumber
  ) %>%
  filter(
    !is.na(RepoStatus)
  ) %>%
  filter(
    MonthsSinceOrig == min(MonthsSinceOrig)
  ) %>%
  View()

## Group by FICO, compate bad ratio between different vehicle age ----
used_perf_nestbyfico <- used_perf %>%
  group_by(FICOBuckets) %>%
  nest() %>%
  arrange(FICOBuckets)

names(used_perf_nestbyfico$data) <- used_perf_nestbyfico$FICOBuckets

# Define function 'alf_summarise' ----
alf_summarise <- function(df, group_var) {
  df %>% 
    group_by(!! group_var) %>%
    summarise(
        Count = n()
      , CountBad = sum(Performance == 'Bad')
      , BadRatio = round(CountBad / Count * 100, 2)
      , AvgDaysDelinq = mean(DaysDelinquent)
    )
}

alf_summarise(used_perf_nestbyfico$data[['NoScore']], quo(VehicleAge))

used_perf_fico_age <- used_perf_nestbyfico %>%
  mutate(
      FICOBucket_Count = map_int(data, nrow)
    , Summary_byVehicleAge = map(data, alf_summarise, quo(VehicleAge))
    , BadRatio_BarPlots = 
        map2(
          .x = Summary_byVehicleAge, .y = FICOBuckets,
          ~ ggplot(.x, aes(VehicleAge, BadRatio, fill = desc(BadRatio))) +
              geom_col() +
              scale_x_discrete(limits = .x$VehicleAge) +
              labs(
                title = str_c('"Bad Ratio" of each vehicle age group, FICO: ', .y),
                caption = str_c('"Bad" is defined as reaching ', default_threshold, 
                                '+DPD within first ', window_length, ' months')
              ) + 
              theme(legend.position="none")
        )
  )

bar_list <- used_perf_fico_age$BadRatio_BarPlots
names(bar_list)
  
multiplot(bar_list[['NoScore']], bar_list[['470-561']], bar_list[['561-582']],
          bar_list[['582-602']], bar_list[['602-629']], bar_list[['629-867']],
          layout = matrix(seq(1, 6, 1), ncol = 3, byrow = TRUE))















