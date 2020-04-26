rsample tutorial n°1
================
Niccolò Salvini
25/4/2020

# New Application

**attrition** data is used in this. It comes from the R base.

``` r
# number of columns
names(attrition) %>% 
  length()
#> [1] 31

attrition %>%  
  select(Attrition) %>% 
  head()
#>   Attrition
#> 1       Yes
#> 2        No
#> 4       Yes
#> 5        No
#> 7        No
#> 8        No
```

We have a huge number of columns, our target variable is once again
$Attrition. The idea is at first building a logistic regression, say, in
the " **common framework** " so with the `glm()` and the family =
‘binomial’ and then try to build without a the same model with the
`parnsip`. So for now let’s fit the Logistics some random predictors
say: JobSatisfaction | Gender | MonthlyIncome

``` r

glm(Attrition ~ JobSatisfaction + Gender + MonthlyIncome, data = attrition, family = binomial)
#> 
#> Call:  glm(formula = Attrition ~ JobSatisfaction + Gender + MonthlyIncome, 
#>     family = binomial, data = attrition)
#> 
#> Coefficients:
#>       (Intercept)  JobSatisfaction.L  JobSatisfaction.Q  JobSatisfaction.C  
#>        -0.9992810         -0.5891416         -0.0063505         -0.1978366  
#>        GenderMale      MonthlyIncome  
#>         0.1543205         -0.0001279  
#> 
#> Degrees of Freedom: 1469 Total (i.e. Null);  1464 Residual
#> Null Deviance:       1299 
#> Residual Deviance: 1234  AIC: 1246
mod_form = as.formula(Attrition ~ JobSatisfaction + Gender + MonthlyIncome)
```

## build the sampling technique

To evaluate this model, we will use 10 repeats of 10-fold
cross-validation and use the 100 holdout samples to evaluate the overall
accuracy of the model.

First, let’s make the splits of the data:

``` r

set.seed(123)
rs_obj = vfold_cv(attrition, v = 10, repeats = 10)

## visualize one of them

rs_obj %>%  
  pull(splits) %>% 
  pluck(1)
#> <Training/Validation/Total>
#> <1323/147/1470>
```

those numbers are:

This indicates that there were **1323** data points in the **analysis**
set, **147** instances were in the **assessment** set, and that the
original data contained **1470** data points.

> quello che succede in italiano: dividi il dataset in 10 parti
> randomiche, eserciti il modello su 9/10 e lo valuti sul restante 1/10.
> Questo lo fai 10 volte all’interno della stessa partizione.
> Successivamente ridivi il dataset in altre 10 parti diverse da quelle
> allo step\_1 e riprovi il modello su 9/10 e lo valuti sul restante
> 1/10. E così via.

this has to be applied to the logistic model and the steps are:

1.  obtain the analysis data set (i.e. the 90% used for modeling)
2.  fit a logistic regression model (GLM)
3.  predict the assessment data (the other 10% not used for the model)
    using the `broom` package
4.  determine if each sample was predicted correctly.

here the function:

``` r

## splits will be the `rsplit` object with the 90/10 partition
holdout_results = function(splits, ...) {
  # Fit the model to the 90%
  mod = glm(..., data = analysis(splits), family = binomial)
  # Save the 10%
  holdout = assessment(splits)
  
  # `augment` will save the predictions with the holdout data set
  res = broom::augment(mod, newdata = holdout)
  # Class predictions on the assessment set from class probs
  lvls = levels(holdout$Attrition)
  predictions = factor(ifelse(res$.fitted > 0, lvls[2], lvls[1]),
                        levels = lvls)
  # Calculate whether the prediction was correct
  res$correct = predictions == holdout$Attrition
  # Return the assessment data set with the additional columns
  res
}
```

take a single split and apply a the `holdout_results()`

``` r

example = holdout_results(rs_obj$splits[[1]],  mod_form)
dim(example)
#> [1] 147  35

dim(assessment(rs_obj$splits[[1]]))
#> [1] 147  31

example[1:10, setdiff(names(example), names(attrition))]
#> # A tibble: 10 x 4
#>    .rownames .fitted .se.fit correct
#>    <chr>       <dbl>   <dbl> <lgl>  
#>  1 20         -1.85    0.208 TRUE   
#>  2 31         -0.892   0.172 FALSE  
#>  3 57         -1.88    0.187 TRUE   
#>  4 61         -1.20    0.185 TRUE   
#>  5 70         -1.76    0.197 TRUE   
#>  6 72         -1.90    0.187 TRUE   
#>  7 106        -2.57    0.204 TRUE   
#>  8 110        -1.36    0.180 TRUE   
#>  9 112        -2.20    0.173 TRUE   
#> 10 141        -3.18    0.351 TRUE
```

In this part I am creating a variable `example` and I am applying the
user defined first item is the single split, second item is the
mod\_form which is the formula (target and predictors). What you are
really doing is applying the model to the first splitting to test it.
For this model, the .fitted value is the linear predictor in log-odds
units. To compute this data set for each of the 100 resamples, we’ll use
the map function from the `purrr` package:

``` r

rs_obj$results = map(rs_obj$splits,
                      holdout_results,
                      mod_form)
rs_obj
#> #  10-fold cross-validation repeated 10 times 
#> # A tibble: 100 x 4
#>    splits             id       id2    results            
#>    <named list>       <chr>    <chr>  <named list>       
#>  1 <split [1.3K/147]> Repeat01 Fold01 <tibble [147 x 35]>
#>  2 <split [1.3K/147]> Repeat01 Fold02 <tibble [147 x 35]>
#>  3 <split [1.3K/147]> Repeat01 Fold03 <tibble [147 x 35]>
#>  4 <split [1.3K/147]> Repeat01 Fold04 <tibble [147 x 35]>
#>  5 <split [1.3K/147]> Repeat01 Fold05 <tibble [147 x 35]>
#>  6 <split [1.3K/147]> Repeat01 Fold06 <tibble [147 x 35]>
#>  7 <split [1.3K/147]> Repeat01 Fold07 <tibble [147 x 35]>
#>  8 <split [1.3K/147]> Repeat01 Fold08 <tibble [147 x 35]>
#>  9 <split [1.3K/147]> Repeat01 Fold09 <tibble [147 x 35]>
#> 10 <split [1.3K/147]> Repeat01 Fold10 <tibble [147 x 35]>
#> # ... with 90 more rows
```

Now we can compute the accuracy values for all of the assessment data
sets:

``` r

rs_obj$accuracy <- map_dbl(rs_obj$results, function(x) mean(x$correct))
summary(rs_obj$accuracy)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.7687  0.8231  0.8401  0.8388  0.8571  0.8980
```
