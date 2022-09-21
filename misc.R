############################<TITTLE>##################################
## Descripción:    Auxiliar functions for rstanarm
## Autor: Sharon Trejo
## Fecha: 2022 - 09 - 13
######################################################################.


# Check type of model -----------------------------------------------------

is.stanreg <- function(x){
  # browser()
  inherits(x, "stanreg")
  }

is.mer <- function (x) {
  
  # browser()
  
  stopifnot(is.stanreg(x))
  check1 <- inherits(x, "lmerMod")
  check2 <- !is.null(x$glmod)
  if (check1 && !check2) {
    stop("Bug found. 'x' has class 'lmerMod' but no 'glmod' component.")
  }
  else if (!check1 && check2) {
    stop("Bug found. 'x' has 'glmod' component but not class 'lmerMod'.")
  }
  isTRUE(check1 && check2)
}

is.nlmer <- function (x) {
  is.mer(x) && inherits(x, "nlmerMod")
}

is.jm <- function (x) {
  isTRUE(x$stan_function == "stan_jm")
}

is.mvmer <- function (x) {
  isTRUE(x$stan_function %in% c("stan_mvmer", "stan_jm"))
}


validate_stanreg_object <- function (x, call. = FALSE) {
  if (!is.stanreg(x)) 
    stop("Object is not a stanreg object.", call. = call.)
}


# Operators ---------------------------------------------------------------

`%ORifNULL%` <- function(a, b) {  if (is.null(a)) b else a}

`%ORifINF%` <- function(a, b) {  if (a == Inf) b else a}

nlist <- function(...) {
  m <- match.call()
  out <- list(...)
  no_names <- is.null(names(out))
  has_name <- if (no_names) FALSE else nzchar(names(out))
  if (all(has_name)) 
    return(out)
  nms <- as.character(m)[-1L]
  if (no_names) {
    names(out) <- nms
  } else {
    names(out)[!has_name] <- nms[!has_name]
  } 
  
  return(out)
}

list_nms <- function (object, M = NULL, stub = "Long") {
  ok_type <- is.null(object) || is.list(object) || is.vector(object)
  if (!ok_type) 
    stop("'object' argument should be a list or vector.")
  if (is.null(object)) 
    return(object)
  if (is.null(M)) 
    M <- length(object)
  nms <- paste0(stub, 1:M)
  if (length(object) > M) 
    nms <- c(nms, "Event")
  names(object) <- nms
  object
}

last_dimnames <- function (x) {
  ndim <- length(dim(x))
  dimnames(x)[[ndim]]
}



# Drop the extra reTrms from a matrix x ---------------------------------

unpad_reTrms <- function (x, ...) UseMethod("unpad_reTrms")

unpad_reTrms.default <- function(x, ...) {
  if (is.matrix(x) || is.array(x))
    return(unpad_reTrms.array(x, ...))
  keep <- !grepl("_NEW_", names(x), fixed = TRUE)
  x[keep]
}

unpad_reTrms.array <- function(x, columns = TRUE, ...) {
  ndim <- length(dim(x))
  if (ndim > 3)
    stop("'x' should be a matrix or 3-D array")
  
  nms <- if (columns) 
    last_dimnames(x) else rownames(x)
  keep <- !grepl("_NEW_", nms, fixed = TRUE)
  if (length(dim(x)) == 2) {
    x_keep <- if (columns) 
      x[, keep, drop = FALSE] else x[keep, , drop = FALSE]
  } else {
    x_keep <- if (columns) 
      x[, , keep, drop = FALSE] else x[keep, , , drop = FALSE]
  }
  return(x_keep)
}



#Methods for creating linear predictors ----------------------------------

linear_predictor <- function(beta, x, offset = NULL) {
  UseMethod("linear_predictor")
}
linear_predictor.default <- function(beta, x, offset = NULL) {
  eta <- as.vector(if (NCOL(x) == 1L) x * beta else x %*% beta)
  if (length(offset))
    eta <- eta + offset
  
  return(eta)
}
linear_predictor.matrix <- function(beta, x, offset = NULL) {
  if (NCOL(beta) == 1L) 
    beta <- as.matrix(beta)
  eta <- beta %*% t(x)
  if (length(offset)) 
    eta <- sweep(eta, 2L, offset, `+`)
  
  return(eta)
}

# Get inverse link function --------------
#
# @param x A stanreg object, family object, or string. 
# @param ... Other arguments passed to methods. For a \code{stanmvreg} object
#   this can be an integer \code{m} specifying the submodel.
# @return The inverse link function associated with x.

linkinv <- function(x, ...) UseMethod("linkinv")

linkinv.stanreg <- function(x, ...) {
  if (is(x, "polr")) polr_linkinv(x) else family(x)$linkinv
}

linkinv.stanmvreg <- function(x, m = NULL, ...) {
  ret <- lapply(family(x), `[[`, "linkinv")
  stub <- get_stub(x)
  if (!is.null(m)) ret[[m]] else list_nms(ret, stub = stub)
}

linkinv.family <- function(x, ...) {
  x$linkinv
}

linkinv.character <- function(x, ...) {
  stopifnot(length(x) == 1)
  polr_linkinv(x)
}

polr_linkinv <- function (x) {
  if (is.stanreg(x) && is(x, "polr")) {
    method <- x$method
  }
  else if (is.character(x) && length(x) == 1L) {
    method <- x
  }
  else {
    stop("'x' should be a stanreg object created by stan_polr ", 
         "or a single string.")
  }
  if (is.null(method) || method == "logistic") 
    method <- "logit"
  if (method == "loglog") 
    return(pgumbel)
  make.link(method)$linkinv
}







