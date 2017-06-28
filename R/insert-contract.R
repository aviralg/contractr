failwith <- function (expr, error) {
  if (! expr) stop(error, call. = FALSE)
}

argument_message <- function(funname, formal, expected_type) {
  sprintf("argument type mismatch in function %s :: expected %s to receive a value of type %s but it received a value of type %s instead.",
          funname,
          as.character(substitute(formal)),
          deparse(substitute(expected_type)),
          type(formal))
}

return_message <- function(funname, result, expected_type) {
  sprintf("return type mismatch in function %s :: expected %s to return a value of type %s but it returned a value of type %s instead.",
          funname,
          funname,
          deparse(substitute(expected_type)),
          type(result))
}


# surround body with "{" for consistency
delimit_exprs <- function(funbody) {
  if(as.character(funbody[[1]]) != "{")
    substitute({ stmts }, list(stmts = funbody))
  else
    funbody
}

insert_return_type_contract <- function(match, funname, fun) {
  match$funbody <- delimit_exprs(body(fun))
  match$funname <- funname
  body(fun) <-
    substitute(
    {result <- funbody
      msg <- contractr:::return_message(funname, result, expected)
      contractr:::failwith((contract)(result), msg)
      result},
    match)
  fun
}

insert_argument_type_contract <- function(match, funname, fun, formal) {
  match$funbody <- delimit_exprs(body(fun))
  match$formal <- formal
  match$funname <- funname
  body(fun) <- substitute({
    msg <- contractr:::argument_message(funname, formal, expected)
    contractr:::failwith((contract)(formal), msg)
    funbody},
    match)
  fun
}
