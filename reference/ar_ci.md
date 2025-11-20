# Calculate Anderson-Rubin Confidence Intervals for fixest IV objects (Compatible with Fixed Effects)

This function calculates Anderson-Rubin (AR) confidence intervals for a
specified endogenous variable from an IV regression estimated with
[`fixest::feols`](https://lrberge.github.io/fixest/reference/feols.html).
It is robust to weak instruments and compatible with models containing
fixed effects.

## Usage

``` r
ar_ci(model, param, level = 0.95, grid_range = NULL, grid_length = 1000)
```

## Arguments

- model:

  A 'fixest' object resulting from an IV estimation.

- param:

  A character string specifying the name of the endogenous variable. If
  the model has only one, this can be omitted.

- level:

  The confidence level required (e.g., 0.95 for a 95% CI).

- grid_range:

  A numeric vector of length 2, `c(min, max)`, specifying the range of
  coefficient values to test. If NULL, a default is chosen.

- grid_length:

  An integer specifying the number of points in the grid to test.

## Value

Returns an object of class 'ar_ci' containing the confidence interval,
the grid of tested values, and their corresponding p-values.
