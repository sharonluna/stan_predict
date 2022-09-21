############################<TITTLE>##################################
## Descripción:  pp_data lql clean  
## Autor: Sharon Trejo
## Fecha: 2022-09-13
######################################################################.



pp_data <-function(object,
                   newdata = NULL,
                   re.form = NULL,
                   offset = NULL,
                   m = NULL,
                   ...) {
  
  # browser()
  
  validate_stanreg_object(object)
  if (is.mer(object)) {
    if (is.nlmer(object))
      # out <- .pp_data_nlmer(object, newdata = newdata, re.form = re.form, m = m, ...) 
      stop("Is NLMER model, please check .pp_data_nlmer function")
    else
      # out <- .pp_data_mer(object, newdata = newdata, re.form = re.form, m = m, ...)
      stop("Is MER model, please check .pp_data_mer function")
    if (!is.null(offset)) out$offset <- offset
    return(out)
  }
  .pp_data(object, newdata = newdata, offset = offset, ...)
}

# for models without lme4 structure
.pp_data <- function(object, newdata = NULL, offset = NULL, ...) {
  
  # browser()
  
  if (is(object, "gamm4")) {
    requireNamespace("mgcv", quietly = TRUE)
    if (is.null(newdata))   x <- predict(object$jam, type = "lpmatrix")
    else x <- predict(object$jam, newdata = newdata, type = "lpmatrix")
    if (is.null(offset)) 
      offset <- object$offset %ORifNULL% rep(0, nrow(x))
    return(nlist(x, offset))
  }
  if (is.null(newdata)) {
    x <- get_x(object)
    if (is.null(offset)) {
      offset <- object$offset %ORifNULL% rep(0, nrow(x))
    }
    if (inherits(object, "betareg")) {
      return(nlist(x, offset, z_betareg = object$z))
    }
    
    return(nlist(x, offset))
  }
  
  offset <- .pp_data_offset(object, newdata, offset)
  Terms <- delete.response(terms(object))
  m <- model.frame(Terms, newdata, xlev = object$xlevels)
  if (!is.null(cl <- attr(Terms, "dataClasses"))) 
    .checkMFClasses(cl, m)
  x <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
  if (is(object, "polr") && !is_scobit(object)) 
    x <- x[,colnames(x) != "(Intercept)", drop = FALSE]
  
  if (inherits(object, "betareg")) {
    mf <- model.frame(delete.response(object$terms$precision), 
                      data = newdata, na.action = object$na.action, 
                      xlev = object$levels$precision)
    z_betareg <- model.matrix(object$terms$precision, mf, contrasts = object$contrasts$precision)
    return(nlist(x, offset, z_betareg))
  }
  
  return(nlist(x, offset))
}

null_or_zero <- function(x) {
  isTRUE(is.null(x) || all(x == 0))
}

.pp_data_offset <- function(object, newdata = NULL, offset = NULL) {
  
  # browser()
  
  if (is.null(newdata)) {
    # get offset from model object (should be null if no offset)
    if (is.null(offset)) 
      offset <- object$offset %ORifNULL% model.offset(model.frame(object))
  } else {
    if (!is.null(offset))
      stopifnot(length(offset) == nrow(newdata))
    else {
      # if newdata specified but not offset then confirm that model wasn't fit
      # with an offset (warning, not error)
      if (!is.null(object$call$offset) || 
          !null_or_zero(object$offset) || 
          !null_or_zero(model.offset(model.frame(object)))) {
        warning(
          "'offset' argument is NULL but it looks like you estimated ", 
          "the model using an offset term.", 
          call. = FALSE
        )
      }
      offset <- rep(0, nrow(newdata))
    }
  }
  return(offset)
}
