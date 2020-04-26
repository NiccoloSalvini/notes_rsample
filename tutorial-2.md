rsample tutorial n°2
================
Niccolò Salvini
25/4/2020

# rsamples Time Series approach

## intro to rolling rolling origins technique

When the data are not independent cross-validation becomes more
difficult as leaving out an observation does not remove all the
associated information due to the correlations with other observations.
For time series forecasting, a cross-validation statistic is obtained as
follows:

1.  Fit the model to the data \[y_1,...,y_t\] and let \[\hat{y}_{t+1}\]
    denote the forcast of the next observation.Then compute the error
    \[\tilde{e}_{t+1} = y_{t+1} -\hat{y}_{t+1}\] for the forecast
    observation.
2.  Repeat step 1 for \[t = m,...,n-1\] where m is the minimum number of
    observations needed for fitting the model
3.  Compute the MSE from \[\tilde{e}_{m+1},...,\tilde{e}_n\]

The data are sales of alcoholic beverages originally from the Federal
Reserve Bank of St. Louis website.

``` r
# data = drinks
skimr::skim(drinks)
```

|                                                  |        |
| :----------------------------------------------- | :----- |
| Name                                             | drinks |
| Number of rows                                   | 309    |
| Number of columns                                | 2      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |        |
| Column type frequency:                           |        |
| Date                                             | 1      |
| numeric                                          | 1      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |        |
| Group variables                                  | None   |

Data summary

**Variable type:
Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
| :------------- | ---------: | -------------: | :--------- | :--------- | :--------- | --------: |
| date           |          0 |              1 | 1992-01-01 | 2017-09-01 | 2004-11-01 |       309 |

**Variable type:
numeric**

| skim\_variable | n\_missing | complete\_rate |    mean |      sd |   p0 |  p25 |  p50 |  p75 |  p100 | hist  |
| :------------- | ---------: | -------------: | ------: | ------: | ---: | ---: | ---: | ---: | ----: | :---- |
| S4248SM144NCEN |          0 |              1 | 7624.85 | 2721.47 | 3031 | 5117 | 7266 | 9553 | 14740 | ▇▇▇▅▁ |

``` r
roll_rs = rolling_origin(
  drinks, 
  initial = 12 * 20, 
  assess = 12,
  cumulative = FALSE
  )

nrow(roll_rs)
#> [1] 58
```

where:

  - *initial* = the number of samples used for **analysis**/modeling in
    the initial resample.
  - *assess* = The number of samples used for each **assessment**
    resample.
  - *cumulative* = A logical. Should the analysis resample grow beyond
    the size specified by initial at each resample?.

the idea behind cumulative is that of you do set it = T you are going to
to evaluate the first 5 elements in the time series and you are goin to
predict on the 6th, then you are going to take the new prediction and
put it in the analysis set and evaluate on the 7th and so on and so
forth. From here you are just defining the splitting, you are still not
permorming any model.

``` r
roll_rs %>% 
  pull(splits) %>%  
  pluck(1)
#> <Training/Validation/Total>
#> <240/12/309>
```

This indicates that there were **240** (initial) data points in the
**analysis** set, **12** (assess) instances were in the **assessment**
set, and that the original data contained **309** data points.

So the number of splitting will be:

``` r
total_data_points = 309
number_of_inital_points = 240
splits= total_data_points -1 - number_of_inital_points
splits
#> [1] 68
```

For plotting, let’s index each split by the first day of the assessment
set, we dont really need to know that:

``` r
get_date <- function(x) 
  min(assessment(x)$date)

start_date <- map(roll_rs$splits, get_date)
roll_rs$start_date <- do.call("c", start_date)
head(roll_rs$start_date)
#> [1] "2012-01-01" "2012-02-01" "2012-03-01" "2012-04-01" "2012-05-01"
#> [6] "2012-06-01"
```

This resampling scheme has 58 splits of the data so that there will be
58 **ARIMA** models that are fit. To create the models, the
`auto.arima()` function from the forecast package is used. The functions
`analysis()` and `assessment()` return the data frame, so another step
converts the data in to a ts object called mod\_dat using a function in
the `timetk` package.

``` r

library(forecast)  # for `auto.arima`
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
#> 
#> Attaching package: 'forecast'
#> The following object is masked from 'package:yardstick':
#> 
#>     accuracy
library(timetk)    # for `tk_ts`
library(zoo)       # for `as.yearmon`
#> 
#> Attaching package: 'zoo'
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric

fit_model <- function(x, ...) {
  # suggested by Matt Dancho:
  x %>%
    analysis() %>%
    # Since the first day changes over resamples, adjust it
    # based on the first date value in the data frame 
    tk_ts(start = .$date[[1]] %>% as.yearmon(), 
          freq = 12, 
          silent = TRUE) %>%
    auto.arima(...)
}
```

So at this point you have set in the previous a comulative = F,
therefore you are evaluating th 12 `assessement()` observations a data
point at-a-time. In other words what it happening is that you are
evaluating the following 12 obervations based on the the 240
observations initially contained in the `analysis()`. Then in the next
iteration you are taking 241 observations and forecasting the following
12, since once again your assess is 12. What you are really doing is
moving yourself along the timeline one obs at-a-time, starting from 240.
As a matter of fact you are having 58 splits, since you have 309-1 n’s.
The idea in the chunck above where the function is defined is: take
data, apply the splittings, then set as

Each model is saved in a new column:

\[should finish…\]
