#' Plot method for ar_ci objects
#' @param x An object of class 'ar_ci' returned by the `ar_ci` function.
#' @param ... Additional arguments passed to the plot function.
#' @export
plot.ar_ci <- function(x, ...) {
    alpha <- 1 - x$level
    plot_data <- data.frame(beta = x$grid_tested, p_value = x$p_values)
    
    plot(plot_data$beta, plot_data$p_value, type = "l",
         xlab = bquote(Hypothesized ~ beta[0] ~ "for coefficient '" ~ .(x$param) ~ "'"),
         ylab = "P-value of AR-test",
         main = "Anderson-Rubin Confidence Set",
         ylim = c(0, 1),
         col = "darkgray", lwd = 2, ...)
    
    # Shade the acceptance region
    accepted_betas <- x$grid_tested[x$p_values > alpha & !is.na(x$p_values)]
    points(accepted_betas, rep(alpha, length(accepted_betas)), pch="|", col="skyblue3")
    
    # Highlight the p-value curve inside the acceptance region
    lines(x$grid_tested[x$p_values > alpha], x$p_values[x$p_values > alpha], col="skyblue3", lwd=2.5)
    
    # Draw the significance level
    abline(h = alpha, col = "red", lty = 2)
    legend("top", inset=0.01,
           legend = c("AR-test p-values", "Acceptance Region", paste("Significance level (alpha) =", alpha)),
           col = c("darkgray", "skyblue3", "red"),
           lty = c(1, 1, 2), lwd=c(2,2,1), bty = "n")
}
