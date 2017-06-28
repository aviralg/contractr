isFALSE <- function(x) identical(x, FALSE)

type <- function(val) {
  atomic <- typeof(val)
  if(is.vector(val)) {
    dims <- c(length(val))
    paste0(atomic, "[", dims , "]")
  } else if(is.array(val)) {
    dims <- dim(val)
    paste0(atomic, "[", paste(dims, collapse = ","), "]")
  } else {
    atomic
  }
}
