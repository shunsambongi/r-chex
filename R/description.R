description <- function(x, ...) {
  UseMethod("description")
}

`description<-` <- function(x, ...) {
  UseMethod("description<-")
}

description.default <- function(x, ...) {
  attr(x, "description")
}

`description<-.default` <- function(x, value) {
  attr(x, "description") <- value
  x
}

description.quosure <- function(x, ...) {
  as_label(x)
}
