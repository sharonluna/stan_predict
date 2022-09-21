############################<TITTLE>##################################
## Descripción:    functions for as.matrix.stanreg lql
## Autor: Sharon Trejo
## Fecha: 2022-09-13
######################################################################.

collect_pars <- function (x, pars = NULL, regex_pars = NULL) {
  
  # browser()
  
  if (is.null(pars) && is.null(regex_pars)) 
    return(NULL)
  if (!is.null(pars)) 
    pars[pars == "varying"] <- "b"
  if (!is.null(regex_pars)) 
    pars <- c(pars, grep_for_pars(x, regex_pars))
  unique(pars)
}

STOP_no_draws <- function () stop("No draws found.", call. = FALSE)

used.optimizing <- function (x) {
  x$algorithm == "optimizing"
}

exclude_lp_and_ppd <- function (pars) {
  grep(pattern = "mean_PPD|log-posterior", 
       x = pars, 
       invert = TRUE, 
       value = TRUE)
}

as.matrix.stanreg <- function(x, ..., pars = NULL, regex_pars = NULL) {
  
  # browser()
  
  pars <- collect_pars(x, pars, regex_pars)
  user_pars <- !is.null(pars)
  
  if (used.optimizing(x)) {
    mat <- x$asymptotic_sampling_dist
    if (is.null(mat)) 
      STOP_no_draws()
    if (!user_pars) {
      aux <- c("sigma", "scale", "shape", "lambda", "reciprocal_dispersion")
      pars <- c(names(coef(x)), # return with coefficients first
                aux[which(aux %in% colnames(mat))])
    }
  } else { # used mcmc or vb
    mat <- as.matrix(x$stanfit)
    if (!user_pars)
      pars <- exclude_lp_and_ppd(colnames(mat))
  }
  if (user_pars)
    check_missing_pars(mat, pars)
  
  mat <- mat[, pars, drop = FALSE]
  if (!is.mer(x))
    return(mat)
  unpad_reTrms(mat)
}
