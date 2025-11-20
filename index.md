## ARCI

`ARCI` is an R package to create Anderson-Rubin confidence intervals for
instrumental variable regression models.

The function is compatible with models created with the `feols` function
from the `fixest` package.

The Anderson-Rubin method is robust to weak instruments and provides
valid inference even when the instruments are not strongly correlated
with the endogenous regressors.

For more technical background, check my blog post
[here](https://bas-m.netlify.app/posts/andersonrubin).
