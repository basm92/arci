#' For a regression table, get Anderson-Rubin Confidence Intervals for fixest IV objects
#' (Compatible with Fixed Effects)
#'
#' This function displays, after calculating, Anderson-Rubin (AR) confidence intervals for a specified
#' endogenous variable from an IV regression estimated with `fixest::feols`.
#' It is robust to weak instruments and compatible with models containing fixed effects.
#'
#' @param model A 'fixest' object resulting from an IV estimation.
#' @return Returns a string representing the Anderson-Rubin confidence interval.
#' @export
get_ar_ci <- function(model){
    interval <- ar_ci(model)$ci |> 
        purrr::pluck(1) |>
        sprintf("%.3f", x = _) |> 
        paste0(... = _, collapse = ", ")
    
    out <- paste0("[", interval, "]", collapse=NULL)
    return(out)
}