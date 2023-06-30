# Tools for model tuning

# model list ------

#' Tune LCA Models.
#'
#' @description
#' Constructs a series of `poLCA` models and compares their convergence,
#' validity and fit stats.
#'
#' @details
#' The function fits a series of \code{\link[poLCA]{poLCA}} models
#' for a range of class numbers.
#' Parallel backend can be provided via \code{\link[future]{plan}}.
#'
#' @return a list with the following elements:
#'
#' * `models` a list of \code{\link[poLCA]{poLCA}} models with the requested
#' class numbers. The models are wrapped as `polcax` class instances
#'
#' * `stats` a data frame with fit statistics and indicators of model
#' convergence and validity, see: \code{\link{summary.polcax}} for details
#'
#' * `plots` a list of ggplot drawings visualizing log-likelihood,
#' AIC (Akaike Information Criterion), BIC (Bayesian Information Criterion),
#' deviance ratios and chi-squared statistics for the requested class
#' number range
#'
#' @param formula a model formula, see: \code{\link[poLCA]{poLCA}}
#' for specification.
#' @param data a modeling data frame.
#' @param class_range a vector of integers with the requested class number
#' to be tested.
#' @param metric tuning metric used for search of the optimal model:
#' `none` (default), `aic` or `bic`. If not `none`, the optimal model will be
#' indicated in the statistic output and in the plots.
#' @param seed a numeric vector for the seed of the random number generator.
#' If `NULL` (default), no seed is set.
#' @param color color of lines and points in the plots.
#' @param size size of points in the plots.
#' @param cust_theme custom ggplot theme for the plots.
#' @param ... extra arguments passed to \code{\link[poLCA]{poLCA}}.
#'
#' @importFrom rlang .data
#' @export

  tune_lca <- function(formula,
                       data,
                       class_range = 1:3,
                       metric = c('none', 'aic', 'bic'),
                       seed = NULL,
                       color = 'steelblue',
                       size = 2,
                       cust_theme = ggplot2::theme_classic(), ...) {

    ## entry control -------

    if(!rlang::is_formula(formula)) {

      stop("'formula' must be a valid R formula.", call. = FALSE)

    }

    if(!is.data.frame(data)) {

      stop("'data' hast to be a data frame.", call. = FALSE)

    }

    if(!is.numeric(class_range)) {

      stop("'class_range' has to be a numeric vector.", call. = FALSE)

    }

    class_range <- as.integer(class_range)

    metric <- match.arg(metric[1], c('none', 'aic', 'bic'))

    optimum <- NULL
    convergence <- NULL
    valid <- NULL
    k <- NULL
    alert <- NULL

    ## construction of the models --------

    if(!is.null(seed)) seed <- TRUE

    models <-
      furrr::future_map(class_range,
                        function(x) poLCA::poLCA(formula = formula,
                                                 data = data,
                                                 nclass = x, ...),
                        .options = furrr::furrr_options(seed = seed))

    models <- rlang::set_names(models, paste0('class_', class_range))

    models <- purrr::map(models, as_polcax)

    ## model stats ---------

    stats <- purrr::map_dfr(models, summary)

    if(metric == 'none') {

      stats <- dplyr::mutate(stats,
                             optimum = NA)

      opt_k <- NA

    } else {

      stats <- dplyr::mutate(stats,
                             optimum = ifelse(.data[[metric]] == min(.data[[metric]]),
                                              'yes', 'no'))

      opt_k <- dplyr::filter(stats, optimum == 'yes')[['k']][1]

    }

    stats <- dplyr::mutate(stats,
                           alert = ifelse(convergence & valid, 'no', 'yes'))

    ## tuning plots ---------

    alert_scale <- c(no = color,
                     yes = 'gray60')

    n_numbers <- nobs(models[[1]])

    plots <-
      purrr::pmap(list(x = c('llik', 'aic', 'bic', 'Gsq', 'Chisq'),
                       y = paste('LCA tuning:',
                                 c('log-likelihood', 'AIC', 'BIC',
                                   'deviance ratio', '\u03C7\u00B2')),
                       z = c('log-likelihood', 'AIC', 'BIC',
                             'deviance ratio', '\u03C7\u00B2')),
                  function(x, y, z) ggplot2::ggplot(stats,
                                                    ggplot2::aes(x = k,
                                                                 y = .data[[x]],
                                                                 color = alert)) +
                    ggplot2::geom_path(color = color) +
                    ggplot2::geom_point(shape = 16,
                                        size = size) +
                    ggplot2::scale_color_manual(values = alert_scale,
                                                name = 'Alerts') +
                    ggplot2::scale_x_continuous(breaks = class_range) +
                    cust_theme +
                    ggplot2::labs(title = y,
                                  subtitle = paste0('observations: n = ',
                                                    n_numbers[['observations']],
                                                    ', variables: n = ',
                                                    n_numbers[['variables']]),
                                  x = 'Class number, k',
                                  y = z))

    if(!is.na(opt_k)) {

      plots <- purrr::map(plots,
                          ~.x +
                            ggplot2::geom_vline(xintercept = opt_k,
                                                linetype = 'dashed'))

    }

    plots <- rlang::set_names(plots, c('llik', 'aic', 'bic', 'Gsq', 'Chisq'))

    ## output ------

    return(list(models = models,
                stats = stats,
                plots = plots))

  }


# END ------
