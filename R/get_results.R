#' Unnest results from the output of pmap_safely
#'
#' @description Allows for results to be easily analyzed.
#'
#' @param pmap_safely_output The data frame outputted by the function pmap_safely_output. More specifically, must have an error and a result column (which is a list containing named lists or vectors).
#'
#' @importFrom magrittr %>%
#' @importFrom purrr map simplify
#' @importFrom tidyr unnest
#' @importFrom dplyr select mutate filter mutate_at
#' @export

get_results <- function(pmap_safely_output){
    message(
        paste0(
            "Removed ", sum(!is.na(pmap_safely_output$error)),
            " errors out of ", length(pmap_safely_output$error), " rows."
        )
    )

    pmap_safely_output %>%
        dplyr::filter(is.na(error)) %>%
        dplyr::select(-error) %>%
        dplyr::mutate(result = result %>% purrr::map(data_framize)) %>%
        tidyr::unnest(result) %>%
        dplyr::mutate_at(dplyr::vars(contains("_result")), purrr::simplify)
}
