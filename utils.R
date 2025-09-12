# Helper functions for regression diagnostics
    
input2txt <- function(x) {
  if (!is.null(x$added)) {
    res <- sprintf("Outcome variable: %s
Predictor variable(s): %s", x$outcome, paste0((paste(x$predictor, collapse = " + ")), " + ", (paste(x$added, collapse = " + "))))
    return(res)
  } else {
    res1 <- sprintf("Outcome variable: %s
Predictor variable(s): %s", x$outcome, paste(x$predictor, collapse = " + "))
    return(res1)
  }
}

# Colour-vision–friendly mode
theme_consistent <- function() {
  theme_minimal(base_size = 12) +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank(),
      plot.title.position = "plot"
    )
}

cb_apply <- function(p, discrete = TRUE, enabled = TRUE) {
  if (!enabled) return(p)
  if (discrete) {
    p + scale_color_viridis_d(end = 0.9) + scale_fill_viridis_d(end = 0.9)
  } else {
    p + scale_color_viridis_c(end = 0.9) + scale_fill_viridis_c(end = 0.9)
  }
}

# Compute component+residuals (partial residuals) for one term
partial_residual_df <- function(fit, var, data = NULL) {
  stopifnot(inherits(fit, "lm"))
  mf <- if (is.null(data)) model.frame(fit) else data
  X  <- model.matrix(fit, data = mf)
  cn <- colnames(X)
  
  # safer: direct string matching rather than regex
  cols <- grepl(paste0("^", var), cn, fixed = FALSE)
  if (!any(cols)) {
    # fallback: fixed match
    cols <- grepl(var, cn, fixed = TRUE)
  }
  if (!any(cols)) stop(sprintf("No design-matrix columns matched '%s'.", var))
  
  beta  <- coef(fit)[cols]
  comp  <- as.numeric(X[, cols, drop = FALSE] %*% beta)
  pr    <- residuals(fit) + comp
  
  data.frame(
    x = mf[[var]],
    pr = pr
  )
}

# # ggplot for numeric vs factor predictors
pr_plot <- function(fit, var, palette_on = TRUE) {
  df <- partial_residual_df(fit, var)  # returns data.frame(x, pr)
  
  if (is.numeric(df$x)) {
    p <- ggplot(df, aes(x = x, y = pr)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm",    formula = y ~ x, se = TRUE,
                  aes(color = "Linear fit", linetype = "Linear fit")) +
      geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                  aes(color = "LOESS", linetype = "LOESS")) +
      labs(x = var, y = "Partial residual",
           title = paste("Partial residual plot for", var),
           color = NULL, linetype = NULL) +
      theme_consistent()
    # engage palette (discrete scale for the two line labels)
    p <- cb_apply(p, discrete = TRUE, enabled = palette_on)
    
  } else {
    # factor: fill boxes by level, color jitter by level
    p <- ggplot(df, aes(x = x, y = pr)) +
      geom_boxplot(aes(fill = x), outlier.alpha = 0.0) +
      geom_jitter(aes(color = x), width = 0.15, alpha = 0.35) +
      labs(x = var, y = "Partial residual",
           title = paste("Partial residual plot for", var),
           color = NULL, fill = NULL) +
      theme_consistent()
    # apply discrete viridis to both color and fill
    p <- cb_apply(p, discrete = TRUE, enabled = palette_on)
    # Optional: hide legend to avoid redundancy
    # p <- p + guides(color = "none", fill = "none")
  }
  
  p
}
