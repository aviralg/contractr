library(annotatr)

namespace <- function() "contractr"

argument_type_handler <- create_handler("argument-type",
                                        match_datatype,
                                        insert_argument_type_contract,
                                        "once",
                                        TRUE)

return_type_handler <- create_handler("return-type",
                                      match_datatype,
                                      insert_return_type_contract,
                                      "once",
                                      TRUE)

.onAttach <- function(libname, pkgname) {

  register_annotation_handler(namespace(),
                              "function_formals",
                              argument_type_handler)

  register_annotation_handler(namespace(),
                              "function_body",
                              return_type_handler)
}
