match_atomic <- function(datatype) {
  datatype <- as.character(datatype)
  switch(as.character(datatype),
         logical = list(contract = is.logical, expected = datatype),
         numeric = list(contract = is.numeric,  expected = datatype),
         integer = list(contract = is.integer, expected = datatype),
         double = list(contract = is.double, expected = datatype),
         complex = list(contract = is.complex, expected = datatype),
         character = list(contract = is.character, expected = datatype),
         raw = list(contract = is.raw, expected = datatype),
         any = list(contract = function(x) { TRUE }, expected = datatype),
         FALSE)
}

match_dimensions <- function(dimensions) {
  dimension_matcher <- function(formal) tryCatch({
    if(is.vector(formal)) actual <- length(formal)
    else actual <- dim(formal)
    ## print(actual)
    ## print(dimensions)
    if(is.null(actual)) return(FALSE)
    if(length(actual) != length(dimensions)) return(FALSE)
    for(i in 1:length(actual)) {
      value <- as.numeric(dimensions[[i]])
      if(value != actual[[i]]) return(FALSE)
    }
    TRUE
  },
  error = function(e) { FALSE }
  )
  list(contract = dimension_matcher,
       expected = dimensions)
}

match_array <- function(datatype) {
  left <- match_datatype(datatype[[2]])
  dimensions <- datatype[3:length(datatype)]
  right <- match_dimensions(dimensions)
  if(isFALSE(left) | isFALSE(right)) return(FALSE)
  list(contract = function(value) { left$contract(value) &
                                    right$contract(value) },
       expected = datatype)
}

match_union <- function(datatype) {
  left <- match_datatype(datatype[[2]])
  right <- match_datatype(datatype[[3]])
  if(isFALSE(left) | isFALSE(right)) return(FALSE)
  list(contract = function(value) { left$contract(value) |
                                      right$contract(value) },
       expected = datatype)
}

match_intersection <- function(datatype) {
  left <- match_datatype(datatype[[2]])
  right <- match_datatype(datatype[[3]])
  if(isFALSE(left) | isFALSE(right)) return(FALSE)
  list(contract = function(value) { left$contract(value) &
                                      right$contract(value) },
       expected = datatype)
}

match_group <- function(dataype) {
  match <- match_datatype(datatype[[2]])
  if(isFALSE(match)) return(FALSE)
  list(contract = match$contract,
       expected = datatype)
}

match_composite <- function(datatype) {
  switch(as.character(datatype[[1]]),
         "[" = match_array(datatype),
         "|" = match_union(datatype),
         "&" = match_intersection(datatype),
         "(" = match_group(datatype),
         FALSE)
}

match_datatype <- function(datatype) {
  if (is.symbol(datatype)) match_atomic(datatype)
  else match_composite(datatype)
}
