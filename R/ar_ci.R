# Make sure you have the 'fixest' package installed
# install.packages("fixest")
library(fixest)

#' Calculate Anderson-Rubin Confidence Intervals for fixest IV objects
#' (Compatible with Fixed Effects)
#'
#' This function calculates Anderson-Rubin (AR) confidence intervals for a specified
#' endogenous variable from an IV regression estimated with `fixest::feols`.
#' It is robust to weak instruments and compatible with models containing fixed effects.
#'
#' @param model A 'fixest' object resulting from an IV estimation.
#' @param param A character string specifying the name of the endogenous variable.
#'   If the model has only one, this can be omitted.
#' @param level The confidence level required (e.g., 0.95 for a 95% CI).
#' @param grid_range A numeric vector of length 2, `c(min, max)`, specifying the
#'   range of coefficient values to test. If NULL, a default is chosen.
#' @param grid_length An integer specifying the number of points in the grid to test.
#'
#' @return Returns an object of class 'ar_ci' containing the confidence interval,
#'   the grid of tested values, and their corresponding p-values.
#' @export
ar_ci <- function(model, param, level = 0.95, grid_range = NULL, grid_length = 1000) {
    
    # --- 1. Input Validation ---
    if (!inherits(model, "fixest") || is.null(model$iv_first_stage)) {
        stop("This function requires a 'fixest' object from an IV regression.")
    }
    
    endo_vars <- names(model$iv_first_stage)
    if (missing(param)) {
        if (length(endo_vars) == 1) param <- endo_vars
        else stop("Model has multiple endogenous variables. Please specify one using the 'param' argument.")
    }
    if (!param %in% endo_vars) {
        stop(paste0("Endogenous variable '", param, "' not found. Available: ", paste(endo_vars, collapse=", ")))
    }
    
    has_fixef <- !is.null(model$fixef_vars)
    
    # --- 2. Extract Data and Variable Matrices ---
    data <- eval(model$call$data, environment(model$fml))
    if (is.null(data)) {
        stop("Could not retrieve data from the model call. Please provide data explicitly, e.g., feols(..., data = mydata).")
    }
    
    # Correctly handle observation selection (e.g., dropped NAs)
    if (length(model$obs_selection) > 0) {
        obs_to_keep <- unlist(model$obs_selection)
        # The list can contain negative indices (rows to remove)
        if (any(obs_to_keep < 0)) {
            data <- data[obs_to_keep, ]
        } else {
            # Or positive indices (rows to keep)
            data <- data[obs_to_keep, ]
        }
    }
    
    y_name <- all.vars(model$fml_all$linear)[1]
    
    # Create a data frame of all necessary variables
    var_names <- c(y_name, param)
    exo_fml <- model$fml_all$linear; exo_fml[[2]] <- NULL
    inst_fml <- model$fml_all$iv; inst_fml[[2]] <- NULL
    
    # Ensure we only include variables that are actually in the data
    exo_vars <- intersect(all.vars(exo_fml), names(data))
    inst_vars <- intersect(all.vars(inst_fml), names(data))
    
    # Full matrix of variables
    all_vars_df <- data[, unique(c(y_name, param, exo_vars, inst_vars)), drop = FALSE]
    
    # --- 3. Partial Out Fixed Effects (and Finalize Variable Types) ---
    if (has_fixef) {
        message("Model contains fixed effects. Partialling them out using demean().")
        demeaned_vars <- demean(X = all_vars_df, f = data[model$fixef_vars], data = data)
        y <- demeaned_vars[, y_name, drop = TRUE]
        x_endo <- demeaned_vars[, param, drop = TRUE]
        x_exo <- demeaned_vars[, exo_vars, drop = FALSE]
        z <- demeaned_vars[, inst_vars, drop = FALSE]
    } else {
        # If no FEs, build matrices from the original data
        y <- model.matrix(reformulate(y_name, intercept = FALSE), all_vars_df)
        x_endo <- model.matrix(reformulate(param, intercept = FALSE), all_vars_df)
        
        if (length(exo_vars) > 0) {
            x_exo <- model.matrix(reformulate(exo_vars), all_vars_df)[, -1, drop = FALSE]
        } else {
            x_exo <- matrix(nrow = nrow(all_vars_df), ncol = 0)
        }
        
        if (length(inst_vars) > 0) {
            z <- model.matrix(reformulate(inst_vars), all_vars_df)[, -1, drop = FALSE]
        } else {
            stop("No instruments found in the model.")
        }
    }
    
    # **CRUCIAL FIX**: Ensure y and x_endo are simple numeric vectors,
    # not data.frames or matrices, before the loop. This prevents the
    # 'list' object error in lm.fit(y = ...).
    y <- as.numeric(y)
    x_endo <- as.numeric(x_endo)
    
    # --- 4. Set Up Grid for Beta ---
    if (is.null(grid_range)) {
        coef_name <- model$iv_endo_names_fit[model$iv_endo_names == param]
        est <- coef(model)[coef_name]; se <- se(model)[coef_name]
        if(is.na(est) || is.na(se)) stop("Could not extract 2SLS coefficient or standard error.")
        # Use a wider default for potentially weak IVs
        grid_range <- c(est - 30 * se, est + 30 * se)
    }
    beta_grid <- seq(grid_range[1], grid_range[2], length.out = grid_length)
    
    # --- 5. AR Test Function ---
    # --- AR Test Function (Final Correction for Matrix Coercion) ---
    get_ar_p_value <- function(beta_0) {
        y_star <- y - beta_0 * x_endo
        
        # --- Robustly construct design matrices for lm.fit ---
        x_list_ur <- list()
        if (!has_fixef) {
            x_list_ur$intercept <- 1
        }
        if (ncol(x_exo) > 0) {
            x_list_ur$x_exo <- x_exo
        }
        x_list_ur$z <- z
        
        # Coerce to matrix to prevent dimension dropping on single-regressor models
        x_ur <- as.matrix(do.call(cbind, x_list_ur))
        
        x_list_r <- list()
        if (!has_fixef) {
            x_list_r$intercept <- 1
        }
        if (ncol(x_exo) > 0) {
            x_list_r$x_exo <- x_exo
        }
        
        # --- Calculate residuals and SSR ---
        res_ur <- lm.fit(x = x_ur, y = y_star)$residuals
        
        if (length(x_list_r) > 0) {
            # Coerce to matrix here as well for robustness
            x_r <- as.matrix(do.call(cbind, x_list_r))
            res_r <- lm.fit(x = x_r, y = y_star)$residuals
        } else {
            # Occurs with FEs and no other exogenous regressors.
            # The model is y_star ~ 0; residuals are y_star.
            res_r <- y_star
        }
        
        ssr_r <- sum(res_r^2)
        ssr_ur <- sum(res_ur^2)
        
        # --- Calculate F-statistic and p-value ---
        df_num <- ncol(z)
        
        # Use the residual degrees of freedom from the original fixest model
        # as the most reliable measure, since it accounts for absorbed FEs.
        df_den <- df.residual(model)
        
        if (is.na(df_den) || df_den <= 0) return(NA)
        
        f_stat <- ((ssr_r - ssr_ur) / df_num) / (ssr_ur / df_den)
        p_value <- pf(f_stat, df_num, df_den, lower.tail = FALSE)
        
        return(p_value)
    }
    
    # --- 6. Apply over the grid ---
    p_values <- sapply(beta_grid, get_ar_p_value)
    
    # --- 7. Format and Return Output ---
    alpha <- 1 - level
    accepted_betas <- beta_grid[p_values > alpha & !is.na(p_values)]
    
    if (length(accepted_betas) > 0 && (min(accepted_betas) <= grid_range[1] || max(accepted_betas) >= grid_range[2])) {
        warning("Confidence interval may extend beyond the 'grid_range'. Consider expanding the range.")
    }
    
    if (length(accepted_betas) == 0) {
        ci <- "Empty set"
    } else if (all(p_values > alpha & !is.na(p_values))) {
        ci <- "(-Inf, Inf) - The data cannot rule out any value."
    } else {
        step_size <- beta_grid[2] - beta_grid[1]
        jumps <- c(0, which(diff(accepted_betas) > 1.5 * step_size), length(accepted_betas))
        intervals <- list()
        for (i in 1:(length(jumps) - 1)) {
            intervals[[i]] <- range(accepted_betas[(jumps[i] + 1):jumps[i+1]])
        }
        ci <- intervals
    }
    
    result <- list(ci = ci, param = param, level = level, grid_tested = beta_grid, p_values = p_values)
    class(result) <- "ar_ci"
    return(result)
}
