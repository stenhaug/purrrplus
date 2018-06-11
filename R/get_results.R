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
#' # apply and get results
#' output <- pmap_safely(numbers, calculate_if_positive)
#'
#' get_results(output)

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
