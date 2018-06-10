#' Safely apply a function to each row of a data frame.
#'
#' @description Easily keep track of inputs and outputs of a function safely applied to each row of a data frame. Output is rectangularized into the original data frame for easy analysis.
#'
#' @param .d A data frame with the inputs to .f.
#' @param .f A function to apply to each row of .d.
#'
#' @importFrom magrittr %>%
#' @importFrom purrr set_names map_chr map_lgl map_dbl map map2 safely
#' @importFrom dplyr select mutate
#' @export

pmap_safely <- function(.d, .f){
    .d_names <- names(.d)
    .f_args <- names(formals(.f))

    if(!all(.f_args %in% .d_names)){
        stop("Data frame does not not contain all function arguments")
    } else if(isTRUE(all.equal(sort(.f_args), sort(.d_names)))){
        message("Great! Perfect match between function arguments and variables.")
    } else{
        message(
            paste0("Note that the function does not use the following variables: ",
                   paste0(setdiff(.d_names, .f_args), collapse = ", ")
            )
        )
    }

    out <- .d %>%
        dplyr::mutate(output_safely = dplyr::select(., intersect(.d_names, .f_args)) %>%
                   purrr::pmap(purrr::safely(.f))) %>%
        dplyr::mutate(error = output_safely %>% purrr::map_chr(~ ifelse(is.null(.$error), NA_character_, .$error$message)),
               result = output_safely %>% purrr::map("result")) %>%
        dplyr::select(-output_safely)

    if(any(is.na(out$error) & purrr::map_lgl(out$result, is.null))){
        warning("Some rows don't have either an error or a result")
    } else if(any(!is.na(out$error) & !purrr::map_lgl(out$result, is.null))){
        warning("Some rows have both an error and a result")
    }

    out
}
