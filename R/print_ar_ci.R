#' Print method for ar_ci objects
#' @param x An object of class 'ar_ci' returned by the `ar_ci` function.
#' @param ... Additional arguments (not used).
#' @export
print.ar_ci <- function(x, ...) {
    cat("Anderson-Rubin", paste0(x$level * 100, "%"), "Confidence Set for '", x$param, "'\n\n", sep = "")
    if (is.character(x$ci)) {
        cat(x$ci, "\n")
    } else {
        cat("The confidence set is the union of the following interval(s):\n")
        for (interval in x$ci) {
            if(length(interval) == 2){
                cat(sprintf("  [%.4f, %.4f]\n", interval[1], interval[2]))
            }
        }
    }
}