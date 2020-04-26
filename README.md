rsample
================
Niccolò Salvini
25/4/2020

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rsample)](https://cran.r-project.org/package=rsample)
![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)

<img src="img/logo.png" alt="drawing" width="139"/>

## Gentle Introduction

-----

`rsample` contains a set of functions that can create different types of
resamples and corresponding classes for their analysis. The goal is to
have a modular set of methods that can be used across different R
packages for:

  - Traditional resampling techniques for estimating the sampling
    Distribution of a statistic and
  - Estimating model performance using a holdout set ( *tain test
    splitting* )

In addition it can be used to in combination with `recipes` and
`parnsip` to develop easy and reproducible model pipelines. it supports
train test splitting for a number of different data types: - Survival
analisys - Time series Anlisys

It can also be used in: - Tuning models in nested resampling -
Gridsearch and randomized search in `Keras` - Boostrap Interval
Estimation

The scope of `rsample` is to provide the basic building blocks for
creating and analyzing resamples of a data set but does not include code
for modeling or calculating statistics. The “Working with Resample Sets”
vignette gives demonstrations of how rsample tools can be
used.

## Terminology Alert… ([Josh Starmer](https://www.youtube.com/user/joshstarmer) called upon)

We define a resample as the result of a two-way split of a data set. For
example, when bootstrapping, one part of the resample is a sample with
replacement of the original data. The other part of the split contains
the instances that were not contained in the bootstrap sample.
Cross-validation is another type of resampling.

some quick YT reference to refresh[boostrapping
technique](http://www.youtube.com/watch?v=XNgt7F6FqDU&t=8m48s) by the
statquest. It automatically jumps to minute 8:48, don’t worry.

## Application

Everybody knows the mtcars dataset, if you don’t, don’t mind I will
introduce you.

``` r
# by the skimr package
skimr::skim(mtcars)
```

|                                                  |        |
| :----------------------------------------------- | :----- |
| Name                                             | mtcars |
| Number of rows                                   | 32     |
| Number of columns                                | 11     |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |        |
| Column type frequency:                           |        |
| numeric                                          | 11     |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |        |
| Group variables                                  | None   |

Data summary

**Variable type:
numeric**

| skim\_variable | n\_missing | complete\_rate |   mean |     sd |    p0 |    p25 |    p50 |    p75 |   p100 | hist  |
| :------------- | ---------: | -------------: | -----: | -----: | ----: | -----: | -----: | -----: | -----: | :---- |
| mpg            |          0 |              1 |  20.09 |   6.03 | 10.40 |  15.43 |  19.20 |  22.80 |  33.90 | ▃▇▅▁▂ |
| cyl            |          0 |              1 |   6.19 |   1.79 |  4.00 |   4.00 |   6.00 |   8.00 |   8.00 | ▆▁▃▁▇ |
| disp           |          0 |              1 | 230.72 | 123.94 | 71.10 | 120.83 | 196.30 | 326.00 | 472.00 | ▇▃▃▃▂ |
| hp             |          0 |              1 | 146.69 |  68.56 | 52.00 |  96.50 | 123.00 | 180.00 | 335.00 | ▇▇▆▃▁ |
| drat           |          0 |              1 |   3.60 |   0.53 |  2.76 |   3.08 |   3.70 |   3.92 |   4.93 | ▇▃▇▅▁ |
| wt             |          0 |              1 |   3.22 |   0.98 |  1.51 |   2.58 |   3.33 |   3.61 |   5.42 | ▃▃▇▁▂ |
| qsec           |          0 |              1 |  17.85 |   1.79 | 14.50 |  16.89 |  17.71 |  18.90 |  22.90 | ▃▇▇▂▁ |
| vs             |          0 |              1 |   0.44 |   0.50 |  0.00 |   0.00 |   0.00 |   1.00 |   1.00 | ▇▁▁▁▆ |
| am             |          0 |              1 |   0.41 |   0.50 |  0.00 |   0.00 |   0.00 |   1.00 |   1.00 | ▇▁▁▁▆ |
| gear           |          0 |              1 |   3.69 |   0.74 |  3.00 |   3.00 |   4.00 |   4.00 |   5.00 | ▇▁▆▁▂ |
| carb           |          0 |              1 |   2.81 |   1.62 |  1.00 |   2.00 |   2.00 |   4.00 |   8.00 | ▇▂▅▁▁ |

``` r

# using mtcars
set.seed(123)
bt_resamples = bootstraps(mtcars, times = 3)
bt_resamples
#> # Bootstrap sampling 
#> # A tibble: 3 x 2
#>   splits          id        
#>   <list>          <chr>     
#> 1 <split [32/11]> Bootstrap1
#> 2 <split [32/9]>  Bootstrap2
#> 3 <split [32/10]> Bootstrap3
```

## Individual Resamples are *rsplit* Objects

The resamples are stored in the splits column in an object that has
**class** : *rsplit*.

In this package we use the following terminology for the two partitions
that comprise a resample:

  - The **analysis** data are those that we selected in the resample.
    For a bootstrap, this is the sample with replacement. For 10-fold
    cross-validation, this is the 90% of the data. These data are often
    used to fit a model or calculate a statistic in traditional
    bootstrapping.(aka the *training* set even if authors do not reaaly
    want you to say that)

  - The **assessment** data are usually the section of the original data
    not covered by the analysis set. Again, in 10-fold CV, this is the
    10% held out. These data are often used to evaluate the performance
    of a model that was fit to the analysis data. aka the *testing* set
    even if authors do not reaaly want you to say that)

ed. I am using a Tidyverse approach for the selection of elements in the
`first_resample`, in particular the `pull()` selection to extract the
column split, then the `pluck()` to pick the first element of the list
of the list. (bt\_resamples$splits\[\[1\]\])

``` r

first_resample = 
  bt_resamples %>%
  pull(splits) %>% # pull selection 
  pluck(1)
  
first_resample
#> <Training/Validation/Total>
#> <32/11/32>
```

This indicates that there were **32** data points in the *analysis* set,
**14** instances were in the assessment set, and that the original data
contained **32** data points. These results can also be determined using
the dim function on an rsplit object.

## How to really call data

``` r

as.data.frame(first_resample) %>% 
  head()
#>                     mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Maserati Bora      15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
#> Cadillac Fleetwood 10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Honda Civic        30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Merc 450SLC        15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Datsun 710         22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Merc 280           19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
```

### alternatively, way better than this

``` r

analysis(first_resample) %>% dim()
#> [1] 32 11
assessment(first_resample) %>% dim()
#> [1] 11 11
```
