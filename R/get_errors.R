#' Get errors from pmap_safely_output for a specified variable.
#'
#' @description Allows for errors to be easily analyzed.
#'
#' @param pmap_safely_output The data frame outputted by the function pmap_safely_output. More specifically, must have an error and a result column (which is a list containing named lists or vectors).
#' @param var The input variable to analyzed errors based on.
#' @param specific Do you just want a summary or specific errors?
#'
#' @importFrom magrittr %>%
#' @importFrom purrr map simplify
#' @importFrom tidyr unnest
#' @importFrom dplyr select mutate filter arrange mutate_at group_by_at summarize n left_join
#' @export

get_errors_one_var <- function(pmap_safely_output, var, specific = FALSE){
    if(!specific){
        out <- pmap_safely_output %>%
            dplyr::group_by_at(var) %>%
            dplyr::summarize(n_errors = sum(!is.na(error)),
                      n_variable_value = dplyr::n(),
                      error_rate = mean(!is.na(error))) %>%
            tidyr::gather(variable, value, -n_errors, -n_variable_value, -error_rate) %>%
            dplyr::select(variable, value, n_errors, n_variable_value, error_rate) %>%
            dplyr::arrange(-error_rate) %>%
            dplyr::mutate(value = as.character(value))
    } else if(specific){
        counts <- pmap_safely_output %>% dplyr::group_by_at(var) %>% dplyr::summarize(n_variable_value = n())

        out <- pmap_safely_output %>%
            dplyr::group_by_at(c(var, "error")) %>%
            dplyr::summarize(n = n()) %>%
            dplyr::left_join(counts, by = var) %>%
            dplyr::mutate(rate = n / n_variable_value) %>%
            tidyr::gather(variable, value, -error, -n, -n_variable_value, -rate) %>%
            dplyr::select(variable, value, error, n, n_variable_value, rate) %>%
            dplyr::arrange(variable, value, -rate) %>%
            dplyr::mutate(value = as.character(value))
    }

    out
}

#' Get errors from pmap_safely_output for all variables.
#'
#' @description Allows for errors to be easily analyzed.
#'
#' @param pmap_safely_output The data frame outputted by the function pmap_safely_output. More specifically, must have an error and a result column (which is a list containing named lists or vectors).
#' @param specific Do you just want a summary or specific errors?
#'
#' @importFrom magrittr %>%
#' @importFrom purrr map_chr map_df
#' @importFrom tidyr unnest
#' @importFrom dplyr select
#' @export

get_errors <- function(pmap_safely_output, specific = FALSE){
    vars <- pmap_safely_output %>% dplyr::select(-error, result) %>% purrr::map_chr(typeof)
    columns <- names(vars[vars != "list"])
    columns %>% purrr::map_df(~ get_errors_one_var(pmap_safely_output, ., specific))
}
