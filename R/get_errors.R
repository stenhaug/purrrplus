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
#' @examples
#' # a function to apply
#' calculate_if_positive <- function(a, b){
#'     if(a < 0 & b < 0) {stop("Both numbers are negative.")}
#'     else if(a < 0) {stop("Just the first number is negative")}
#'     else if(b < 0) {stop("Just the second number is negative")}
#'
#'     list(add = a + b,
#'          subtract = a - b,
#'          multiply = a * b,
#'          divide = a / b)
#' }
#' # data frame to apply the function to by row
#' numbers <- data_frame(a = c(-1, 0, 1, 2),
#'                       b = c(2, 1, 0, -1),
#'                       irrelevant = c("minneapolis", "st_paul", "minneapolis", "st_paul"))
#'
#' # apply and get errors
#' output <- pmap_safely(numbers, calculate_if_positive)
#'
#' get_errors_one_var(output, var = "a")
#' get_errors_one_var(output, var = "a", specific = TRUE)
#' get_errors(output)
#' get_errors(output, specific = TRUE)
get_errors_one_var <- function(pmap_safely_output, var, specific = FALSE){
    if(!specific){
        out <- pmap_safely_output %>%
            dplyr::group_by_at(var) %>%
            dplyr::summarize(n_errors = sum(!is.na(error)),
                      count = dplyr::n(),
                      error_rate = mean(!is.na(error))) %>%
            tidyr::gather(variable, value, -n_errors, -count, -error_rate) %>%
            dplyr::select(variable, value, n_errors, count, error_rate) %>%
            dplyr::arrange(-error_rate) %>%
            dplyr::mutate(value = as.character(value))
    } else if(specific){
        counts <- pmap_safely_output %>% dplyr::group_by_at(var) %>% dplyr::summarize(count = n())

        out <- pmap_safely_output %>%
            dplyr::group_by_at(c(var, "error")) %>%
            dplyr::summarize(n = n()) %>%
            dplyr::left_join(counts, by = var) %>%
            dplyr::mutate(rate = n / count) %>%
            tidyr::gather(variable, value, -error, -n, -count, -rate) %>%
            dplyr::select(variable, value, error, n, count, rate) %>%
            dplyr::arrange(variable, value, -rate) %>%
            dplyr::mutate(value = as.character(value))
    }

    out
}

#' @rdname get_errors_one_var
#' @export

get_errors <- function(pmap_safely_output, specific = FALSE){
    vars <- pmap_safely_output %>% dplyr::select(-error, result) %>% purrr::map_chr(typeof)
    columns <- names(vars[vars != "list"])
    columns %>% purrr::map_df(~ get_errors_one_var(pmap_safely_output, ., specific))
}
