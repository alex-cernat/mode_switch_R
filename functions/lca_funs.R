# extra functions for LCA in R

# calculate entropy. Based on syntax from Daniel Oberski, ESRA course

lca_entropy <- function(x) {

  entropy <- function(p) sum(-p * log(p))
  error_prior <- entropy(x$P) # Class proportions
  error_post <- mean(apply(x$posterior, 1, entropy), na.rm = T)
  (R2_entropy  <- (error_prior - error_post) / error_prior)

}

# extract coefficients of interest

lca_fit_extract <- function(x,
                            coefs = c("llik", "Gsq", "Nobs", "npar",
                                      "aic", "bic")) {
  reduce(x[coefs], cbind) %>%
    tbl_df() %>%
    setNames(coefs) %>%
    mutate(entropy = lca_entropy(x))
}
