#' Check whether arg is a bare name
#'
#' @param x Function argument
#'
#' @return Boolean. `TRUE` if `x` is a bare name; `FALSE` otherwise.
#'
#' @importFrom rlang is_symbol
#'
#' @noRd
is_bare_name <- function(x) {

    # check whether quoted expression `x` is a symbol
    is_name <- rlang::is_symbol(x)

    return(is_name)

}
