############################<TITTLE>##################################
## Descripción:   Función predict para stan 
## Autor: Sharon Trejo
## Fecha: 2022-09-13
######################################################################.


source('misc.R')
source('get_funcs.R')
source('pp_data_lql_clean.R')
source('as.matrix.stanreg.R')


library(dplyr)

predict.stanreg.lql <- function(object,
                            ...,
                            newdata = NULL,
                            type = c("link", "response"),
                            se.fit = FALSE) {
  
  # browser()
  
  if (is.mer(object)) {
    stop(
      "'predict' is not available for models fit with ",
      object$stan_function,
      ". Please use the 'posterior_predict' function instead.",
      call. = FALSE
    )
  }
  
  type <- match.arg(type)
  if (!se.fit && is.null(newdata)) {
    preds <- if (type == "link") 
      object$linear.predictors else object$fitted.values
    return(preds)
  }
  
  # if (isTRUE(object$stan_function == "stan_betareg") &&
  #     !is.null(newdata)) {
  #   # avoid false positive warnings about missing z variables in newdata
  #   zvars <- all.vars(object$terms$precision)
  #   for (var in zvars) {
  #     if (!var %in% colnames(newdata)) newdata[[var]] <- NA
  #   }
  # }
  
  dat <- pp_data(object, newdata)
  stanmat <- as.matrix.stanreg(object)
  beta <- stanmat[, seq_len(ncol(dat$x))]
  eta <- linear_predictor(beta, dat$x, dat$offset)
  
  if (type == "response") {
    inverse_link <- linkinv(object)
    eta <- inverse_link(eta)
    
    if (is(object, "polr") && ("alpha" %in% colnames(stanmat)))
      # eta <- apply(eta, 1L, FUN = `^`, e2 = stanmat[, "alpha"]) 
      stop("Object is type POLR, check alpha")
  }
  fit <- colMeans(eta)
  if (!se.fit)
    return(fit)
  
  se.fit <- apply(eta, 2L, sd)
  nlist(fit, se.fit)
}
