## sketch
#### 0. Define good and bad ----
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









