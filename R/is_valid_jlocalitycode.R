#' @title Tests that its argument is a syntactically valid JIS
#' municipality or prefecture code
#'
#' This function does not check whether the code is really in use,
#' just whether it is well-formed (i.e., that it has the right number
#' of digits and a prefecture number between 1 and 47).
#'
#' @param code Character or numeric code
#' @return Logical
#' @export
is_valid_jlocalitycode = function(code) {
    (grepl("^\\d+$", code) &
     (nchar(code) %in% c(2, 5)) &
     (substr(code, 1, 2) < 48))
}
