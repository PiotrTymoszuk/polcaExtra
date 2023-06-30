# Helper functions

# Modal assignment

#' Assign Observations to Classes by Nodal Vote.
#'
#' @description
#' Assigns observation to classes by a simple nodal probability vote.
#'
#' @param x a matrix with probabilities, observations in rows
#' and classes in columns.
#' @param resolve_ties logical, should ties in vote be resolved by random?
#' Defaults to `FALSE`.
#'
#' @return a tibble with the columns `observation`, `class` and `best_p`.
#'
#' @details
#' The column and rownames of the input matrix need to be specified.
#' Usually, if there are ties in voting, the first class of the tie is chosen.
#' Ties will be resolved by random if `resolve_ties` is set to `TRUE`.
#'
#' @export

  nodal_vote <- function(x, resolve_ties = FALSE) {

    ## entry control ------

    stopifnot(is.logical(resolve_ties))

    if(!is.matrix(x)) {

      stop('A matrix is required.', call. = FALSE)

    }

    if(is.null(colnames(x))) {

      stop("Column names of 'x' must be specified.", call. = FALSE)

    }

    if(is.null(rownames(x))) {

      stop("Row names of 'x' must be specified.", call. = FALSE)

    }

    observation <- NULL

    ## assignment ------

    max_list <-
      purrr::map(rownames(x), function(row) rlang::set_names(x[row, ],
                                                             colnames(x)))

    max_list <- rlang::set_names(max_list, rownames(x))

    max_list <- purrr::map(max_list, ~.x[.x == max(.x)])

    if(!resolve_ties) {

      assign_tbl <- purrr::map_dfr(max_list,
                                   ~tibble::tibble(class = names(.x)[1],
                                                   best_p = .x[1]))

    } else {

      indexes <-
        purrr::map(max_list,
                   function(x) if(length(x) == 1) 1 else sample(1:length(x),
                                                               size = 1,
                                                               replace = FALSE))

      assign_tbl <-
        purrr::map2_dfr(max_list,
                        indexes,
                        ~tibble::tibble(class = names(.x)[.y],
                                        best_p = .x[.y]))


    }

    assign_tbl <-
      dplyr::mutate(assign_tbl,
                    class = factor(class, colnames(x)),
                    observation = rownames(x))

    dplyr::relocate(assign_tbl, observation)

  }

# END -----
