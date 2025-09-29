library(coin)
set.seed(42)

R      <- 1200
k      <- 3
n      <- 10
B      <- 2000     # we'll pass this as nresample
alpha  <- 0.05

sim_one <- function(k = 3, n = 10, effect = 0) {
  g  <- factor(rep(seq_len(k), each = n))
  mu <- c(rep(0, k - 1), effect)
  y  <- unlist(lapply(seq_len(k), function(j) rnorm(n, mean = mu[j], sd = 1)))
  list(y = y, g = g)
}

kw_pvals <- function(y, g, nresample = 2000) {
  k <- nlevels(g)
  
  p_asym <- pvalue(kruskal_test(y ~ g, distribution = asymptotic()))
  p_mc   <- pvalue(kruskal_test(y ~ g, distribution = approximate(nresample = nresample)))
  
  p_exact <- NA_real_
  if (k == 2) {
    p_exact <- pvalue(kruskal_test(y ~ g, distribution = exact()))
  }
  c(asymptotic = p_asym, exact = p_exact, monte_carlo = p_mc)
}

# Null: Type I error
pvals_null <- matrix(NA_real_, nrow = R, ncol = 3,
                     dimnames = list(NULL, c("asymptotic","exact","monte_carlo")))
for (i in seq_len(R)) {
  dat <- sim_one(k = k, n = n, effect = 0)
  pvals_null[i, ] <- kw_pvals(dat$y, dat$g, nresample = B)
}
type1 <- colMeans(pvals_null < alpha, na.rm = TRUE)

# Alternative: Power
pvals_alt <- matrix(NA_real_, nrow = R, ncol = 3,
                    dimnames = list(NULL, c("asymptotic","exact","monte_carlo")))
for (i in seq_len(R)) {
  dat <- sim_one(k = k, n = n, effect = 0.6)
  pvals_alt[i, ] <- kw_pvals(dat$y, dat$g, nresample = B)
}
power <- colMeans(pvals_alt < alpha, na.rm = TRUE)

cat("Groups k =", k, " | Repeats R =", R, " | MC B =", B, "\n")
cat("Type I error (H0, alpha=0.05):\n"); print(round(type1, 3))
cat("Power (H1, effect=0.6):\n");        print(round(power, 3))
