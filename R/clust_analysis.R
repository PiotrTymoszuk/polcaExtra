# Functions for coercion to clust_analysis objects

# generic -------

#' Coerce to a clust_analysis Object
#'
#' @description
#' Coerces an object (currently `polcax` or `lca_assign` class instance) to a
#' \code{\link[clustTools]{clust_analysis}} object which may be
#' further investigated with a broad range
#' of diagnostic and visualization tools provided by
#' the \href{https://github.com/PiotrTymoszuk/clustTools}{clustTools} package.
#'
#' @details `lca2clust` is a S3 generic function. Distances between
#' observations are computed with \code{\link[clustTools]{calculate_dist}}.
#'
#' @param x as object.
#' @param data modeling data frame: required only for `lca_assign` objects.
#' @param distance_method distance between the observations. Allowed are:
#' `euclidean` (default), `manhattan`, `cosine`, `jaccard`,
#' `smc` (simple matching coefficient) and `dice`.
#' @param ... extra arguments passed to the methods.
#'
#' @export lca2clust.polcax
#' @export

  lca2clust.polcax <- function(x,
                               distance_method = c('euclidean',
                                                   'manhattan',
                                                   'cosine',
                                                   'jaccard',
                                                   'smc'), ...) {

    ## entry control --------

    stopifnot(is_polcax(x))

    distance_method <- match.arg(distance_method[1],
                                 c('euclidean',
                                   'manhattan',
                                   'cosine',
                                   'jaccard',
                                   'smc'))

    ## clustering data and distances ------

    clust_data <- components(x, type = 'data')

    clust_variables <- components(x, type = 'variables')

    clust_data[clust_variables] <-
      map_dfc(clust_data[clust_variables],
              ~as.numeric(.x) - 1)

    dist_mtx <- calculate_dist(clust_data, method = distance_method)

    assignment <-
      components(x, type = 'assignment')[c('observation', 'class')]

    assignment <- set_colnames(assignment, c('observation', 'clust_id'))

    ## cluster object ------

    clust_analysis(list(data = quo(clust_data),
                        dist_mtx = dist_mtx,
                        dist_method = distance_method,
                        clust_obj = NULL,
                        clust_fun = 'prediction',
                        clust_assignment = assignment,
                        dots = NULL))

  }

#' @rdname lca2clust.polcax
#' @export lca2clust.lca_assign
#' @export

  lca2clust.lca_assign <- function(x,
                                   data,
                                   distance_method = c('euclidean',
                                                       'manhattan',
                                                       'cosine',
                                                       'jaccard',
                                                       'smc'), ...) {

    ## entry control --------

    stopifnot(is_lca_assign(x))

    distance_method <- match.arg(distance_method[1],
                                 c('euclidean',
                                   'manhattan',
                                   'cosine',
                                   'jaccard',
                                   'smc'))

    if(!is.data.frame(data)) {

      stop("'data' hast to be a data frame.", call. = FALSE)

    }

    ## clustering data and distances ------

    clust_variables <- names(data)

    data[clust_variables] <-
      map_dfc(data[clust_variables], ~as.numeric(.x) - 1)

    dist_mtx <- calculate_dist(data, method = distance_method)

    assignment <- set_colnames(x[c('observation', 'class')],
                               c('observation', 'clust_id'))

    ## cluster object ------

    clust_analysis(list(data = quo(data),
                        dist_mtx = dist_mtx,
                        dist_method = distance_method,
                        clust_obj = NULL,
                        clust_fun = 'prediction',
                        clust_assignment = assignment,
                        dots = NULL))


  }

#' @rdname lca2clust.polcax
#' @export

  lca2clust <- function(x, ...) {

    UseMethod('lca2clust')

  }

# END ------
