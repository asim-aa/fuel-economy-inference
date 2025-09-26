options(error = NULL) 
set.seed(1)
suppressWarnings({
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("Install ggplot2 for nicer plots: install.packages('ggplot2')")
  }
})
library(stats)

alpha         <- 0.10
R_per_setting <- 10000       
g_values      <- c(5, 10, 20, 50, 100)
n_values      <- c(10, 20, 30, 40, 50)
pairs_mode <- "auto"        
mu_grid_for_g <- function(g) {
  if (g <= 5)  return(seq(0, 1.0,  by = 0.1))
  if (g <= 10) return(seq(0, 1.2,  by = 0.1))
  if (g <= 20) return(seq(0, 1.5,  by = 0.1))
  if (g <= 50) return(seq(0, 2.0,  by = 0.2))
  return(seq(0, 2.5, by = 0.25))  # g = 100
}

sample_dataset <- function(g, n, mu) {
  grp <- gl(g, n, labels = seq_len(g))
  means <- c(mu, rep(0, g - 1))
  y <- rnorm(g * n, mean = rep(means, each = n), sd = 1)
  list(y = y, grp = grp)
}

global_p_from_pairwise <- function(y, grp, mode = "all") {
  g <- nlevels(grp)
  if (mode == "all") {
    pm <- pairwise.t.test(y, grp, p.adjust.method = "holm",
                          pool.sd = FALSE, paired = FALSE)$p.value
    return(min(pm, na.rm = TRUE))
  } else if (mode == "against1") {
    lev <- levels(grp)
    pvec <- vapply(2:g, function(j) {
      t.test(y[grp == lev[1]], y[grp == lev[j]],
             var.equal = FALSE, paired = FALSE)$p.value
    }, numeric(1))
    return(min(p.adjust(pvec, method = "holm")))
  } else {
    stop("Unknown mode for pairwise testing.")
  }
}
one_replicate <- function(g, n, mu, mode_pairwise) {
  dat <- sample_dataset(g, n, mu)
  sds <- tapply(dat$y, dat$grp, sd)
  if (any(!is.finite(sds)) || any(sds == 0)) return(c(welchF = NA, holm = NA))
  p_welchF <- tryCatch(
    oneway.test(dat$y ~ dat$grp, var.equal = FALSE)$p.value,
    error = function(e) NA_real_ )
  p_holm <- tryCatch(
    global_p_from_pairwise(dat$y, dat$grp, mode = mode_pairwise),
    error = function(e) NA_real_ )
  c(welchF = p_welchF, holm = p_holm)
}
mode_for_g <- function(g) {
  if (pairs_mode == "auto") {
    if (g <= 20) "all" else "against1"
  } else pairs_mode
}
run_sim <- function() {
  results <- list()
  idx <- 1
  total_steps <- sum(sapply(g_values, function(g) length(mu_grid_for_g(g)))) * length(n_values)
  pb <- txtProgressBar(min = 0, max = total_steps, style = 3)
  step <- 0
  
  for (g in g_values) {
    mu_grid <- mu_grid_for_g(g)
    pw_mode <- mode_for_g(g)
    message(sprintf("== g = %d (pairwise mode: %s) ==", g, pw_mode))
    
    for (n in n_values) {
      for (mu in mu_grid) {
        rejs <- replicate(R_per_setting, {
          p <- one_replicate(g, n, mu, pw_mode)
          c(welchF = as.numeric(!is.na(p["welchF"]) && p["welchF"] < alpha),
            holm   = as.numeric(!is.na(p["holm"])   && p["holm"]   < alpha))})
        if (is.null(dim(rejs))) rejs <- matrix(rejs, nrow = 2)
        pow_w <- mean(rejs[1, ], na.rm = TRUE)
        pow_h <- mean(rejs[2, ], na.rm = TRUE)
        results[[idx]] <- data.frame(
          g = g, n = n, mu = mu,
          test = c("Welch F", "Pairwise Holm"),
          power = c(pow_w, pow_h),
          stringsAsFactors = FALSE )
        idx <- idx + 1
        step <- step + 1
        setTxtProgressBar(pb, step)}}}
  close(pb)
  do.call(rbind, results)
}
res <- run_sim()
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  for (g_now in unique(res$g)) {
    dfp <- subset(res, g == g_now)
    p <- ggplot(dfp, aes(x = mu, y = power, color = test, group = test)) +
      geom_line() +
      geom_point(size = 0.9) +
      facet_wrap(~ n, labeller = label_both) +
      labs(
        title = paste0("Power vs ", expression(mu), " at α = ", alpha, " (g = ", g_now, ")"),
        subtitle = paste("Welch one-way F vs pairwise Welch t-tests + Holm",
                         if (mode_for_g(g_now) == "against1") " (only pairs with group 1)" else ""),
        x = expression(mu),
        y = "Power"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    print(p)
  }}else {
    print(head(res)) }