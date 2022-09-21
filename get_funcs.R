############################<TITTLE>##################################
## Descripción:  get_x  & get_y
## Autor: Sharon Trejo
## Fecha: 
######################################################################.

get_x <- function(object, ...) UseMethod("get_x")

get_x.default <- function(object, ...) {
  object[["x"]] %ORifNULL% model.matrix(object)
}

get_x.gamm4 <- function(object, ...) {
  as.matrix(object[["x"]])
}

get_x.lmerMod <- function(object, ...) {
  object$glmod$X %ORifNULL% stop("X not found")
}

get_stub <- function (object) {
  if (is.jm(object)) 
    "Long"
  else if (is.mvmer(object)) 
    "y"
  else NULL
}



