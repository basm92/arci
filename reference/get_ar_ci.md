# For a regression table, get Anderson-Rubin Confidence Intervals for fixest IV objects (Compatible with Fixed Effects)

This function displays, after calculating, Anderson-Rubin (AR)
confidence intervals for a specified endogenous variable from an IV
regression estimated with
[`fixest::feols`](https://lrberge.github.io/fixest/reference/feols.html).
It is robust to weak instruments and compatible with models containing
fixed effects.

## Usage

``` r
get_ar_ci(model)
```

## Arguments

- model:

  A 'fixest' object resulting from an IV estimation.

## Value

Returns a string representing the Anderson-Rubin confidence interval.
