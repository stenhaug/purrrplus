#' Turn a named list or named vector into a one row data frame.
#'
#' @description A helper function.
#'
#' @param x A named list or data frame
#'
#' @importFrom magrittr %>%
#' @importFrom tibble enframe
#' @importFrom tidyr spread
#' @importFrom purrr set_names
#' @importFrom dplyr select
#' @export
#' @examples
#' x <- list(a = 1, b = 2, c = 3)
#' data_framize(x)

data_framize <- function(x){
    if(is.null(names(x))){
        stop("This function needs to return a named list or vector")
    }

    x %>%
        tibble::enframe() %>%
        tidyr::spread(name, value) %>%
        dplyr::select(names(x)) %>%
        purrr::set_names(paste0(names(x), "_result"))
}
