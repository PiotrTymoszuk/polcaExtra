# Summary, accessor and prediction methods for the 'polcax' class

# Summary --------

#' Statistic Summary of the polcax Object
#'
#' @description
#' Provides a summary of the fit statistics for a `polcax` model.
#'
#' @return
#' A tibble with the following columns:
#' * `k` number of classes
#' * `numiter` number of iterations required for the `poLCA` model to converge
#' * `maxiter` maximal number of iterations
#' * `convergence` a logical value indicating it model convergence was reached
#' * `Nvars` number of modeling variables
#' * `N` total number of cases
#' * `Nobs` number of complete cases
#' * `llik` maximum value of the log-likelihood
#' * `aic` Akaike Information Criterion
#' * `bic` Bayesian Information Criterion
#' * `Gsq` likelihood ratio/deviance statistic
#' * `Chisq` Pearson Chi-square goodness of fit statistic for fitted versus
#' observed multiway tables
#' * `df` number of degrees of freedom in the model
#' * `resid.df` number of residual degrees of freedom
#' * `valid` a logical value indicating if the model was properly parameterized,
#' i.e. if residual degrees of freedom were greater or equal than 0
#'
#' @param object an object of the `polcax` class.
#' @param ... extra arguments passed to the methods, currently none.
#' @export summary.polcax
#' @export

  summary.polcax <- function(object, ...) {

    stopifnot(is_polcax(object))

    k <- length(object[['P']])
    convergence <- object[['numiter']] < object[['maxiter']]
    valid <- object[['resid.df']] > 0

    summ <-
      c(list(k = k),
        object[c('numiter', 'maxiter')],
        list(convergence = convergence),
        list(Nvars = ncol(object[['y']])),
        object[c('N', 'Nobs',
                 'llik', 'aic', 'bic',
                 'Gsq', 'Chisq',
                 'npar', 'resid.df')],
        list(valid = valid))

    tibble::as_tibble(summ)

  }

# accessor --------

#' Access Components of the polcax Object
#'
#' @description
#' Accesses/extracts components of the `polcax` object.
#'
#' @details
#' By specifying the `type` arguments, the following components are retrieved:
#'
#' * `data` the modeling data
#'
#' * `class_names` names of the classes
#'
#' * `variables` names of the modeling variables
#'
#' * `class_numbers` numbers, fraction and percentages of observations assigned
#' to the classes in a tibble. The class assignment is provided
#' by \code{\link{nodal_vote}}
#'
#' * `prior_p` a tibble with mixing proportions (mean of priors) in the
#' LCA model with their standard deviations.
#'
#' * `posterior` a class `lca_assign` tibble with posterior class membership
#' probabilities and class assignment (nodal) computed
#' by \code{\link{nodal_vote}}. See also: \code{\link{plot.lca_assign}}
#'
#' * `assignment` as above
#'
#' * `fit_stats` a tibble with fit statistics as specified by
#' \code{\link{summary.polcax}}
#'
#' * `predcell` a tibble of observed versus predicted counts of observations.
#'
#' * `probs` a list of tibbles with estimated class-conditional
#' response probabilities.
#'
#' * `probs.se` a list of tibble with standard errors
#' of the conditional probabilities.
#'
#' @return a tibble or a list with components as specified in `Details`.
#'
#' @param object a `polcax` class object.
#' @param type component to be retrieved, see `Details`.
#' @param ... extra arguments for downstream functions
#' like \code{\link{nodal_vote}}, see `Details`.
#'
#' @importFrom generics components
#' @export components.polcax
#' @export

  components.polcax <- function(object,
                                type = c('data',
                                         'class_names',
                                         'variables',
                                         'class_numbers',
                                         'prior_p',
                                         'posterior',
                                         'assignment',
                                         'fit_stats',
                                         'predcell',
                                         'probs',
                                         'probs.se'), ...) {

    ## entry control -------

    stopifnot(is_polcax(object))

    type <- match.arg(type[1],
                      c('data',
                        'class_names',
                        'variables',
                        'class_numbers',
                        'prior_p',
                        'posterior',
                        'assignment',
                        'fit_stats',
                        'predcell',
                        'probs',
                        'probs.se'))

    n <- NULL
    fraction <- NULL

    ## accessing the components -------

    if(type == 'data') return(object[['y']])

    if(type == 'class_names') return(object[['class_names']])

    if(type == 'variables') return(colnames(object[['y']]))

    if(type == 'fit_stats') return(summary(object))

    if(type %in% c('posterior', 'assignment')) {

      post_p <- object[['posterior']]

      if(!is.null(rownames(object[['y']]))) {

        obs_id <- rownames(object[['y']])

      } else {

        obs_id <- paste0('obs_', 1:nrow(post_p))

      }

      post_p <- trafo::set_colnames(post_p,
                                    object[['class_names']])

      post_p <- trafo::set_rownames(post_p, obs_id)

      assignment <- nodal_vote(post_p, ...)

      post_p <- tibble::as_tibble(post_p)

      post_p <- tibble::as_tibble(cbind(assignment, post_p))

      return(lca_assign(post_p))

    }

    if(type == 'class_numbers') {

      assignment <- components(object, type = 'posterior', ...)

      class_n <- dplyr::count(assignment, class)

      return(dplyr::mutate(class_n,
                           fraction = n/sum(n),
                           percent = fraction * 100))

    }

    if(type == 'prior_p') {

      return(tibble::tibble(class = factor(object[['class_names']],
                                           object[['class_names']]),
                            p_prior = object[['P']],
                            se_prior = object[['P.se']]))

    }

    if(type == 'predcell') {

      mod_vars <- components(object, type = 'variables')

      preds <- tibble::as_tibble(object[['predcell']])

      levels <- purrr::map(object[['y']][mod_vars], levels)

      preds[mod_vars] <-
        purrr::map2_dfc(preds[mod_vars],
                        levels,
                        ~factor(.y[.x], .y))

      return(preds)

    }

    if(type %in% c('probs', 'probs.se')) {

      mod_vars <- components(object, type = 'variables')

      levels <- purrr::map(object[['y']][mod_vars], levels)

      probs <- object[[type]]

      probs <-
        purrr::map2(probs,
                    levels,
                    ~trafo::set_colnames(.x, .y))

      probs <- purrr::map(probs, trafo::set_rownames, object[['class_names']])

      probs <- purrr::map(probs,
                          ~tibble::rownames_to_column(as.data.frame(.x),
                                                     'class'))

      probs <- purrr::map(probs,
                          dplyr::mutate,
                          class = factor(class, object[['class_names']]))

      return(purrr::map(probs, tibble::as_tibble))

    }

  }

#' Numbers of Observations and Variables
#'
#' @description
#' Numbers of observations and variables in the `polcax` model.
#'
#' @return a list with the `observations` and `variables` elements.
#'
#' @param object an object of the `polcax` class.
#' @param ... extra arguments passed to methods, currently none.
#' @importFrom stats nobs
#' @export

  nobs.polcax <- function(object, ...) {

    list(observations = object[['Nobs']],
         variables = ncol(object[['y']]))

  }

#' Modeling Data of the polcax Object
#'
#' @description
#' Retrieves the modeling data of a `polcax` object.
#'
#' @return a data frame of matrix with the modeling data.
#'
#' @param formula a `polcax` class object.
#' @param ... extra arguments passed to methods, currently none.
#' @export

  model.frame.polcax <- function(formula, ...) {

    components(formula, type = 'data', ...)

  }

# prediction ------

#' Predict Class Assignment
#'
#' @description
#' Predicts class assignment based on the LCA model information stored in the
#' `polcax` object.
#'
#' @details
#' See: \code{\link[poLCA]{poLCA.posterior}} for details of prediction.
#' The class assignment is done by voting as specified
#' for \code{\link{nodal_vote}}.
#'
#' @param object a `polcax` class object.
#' @param newdata a vector or matrix containing series of responses on
#' the manifest variables in the LCA model.
#' @param x an optional vector or matrix of covariate values.
#' @param resolve_ties should ties in class assignment prediction
#' be randomly resolved? Defaults to `FALSE`.
#' @param ... extra arguments, currently none.
#'
#' @return a tibble of class `lca_assign` with
#' the \code{\link{plot.lca_assign}} method specified.
#'
#' @export predict.polcax
#' @export

  predict.polcax <- function(object,
                             newdata = NULL,
                             x = NULL,
                             resolve_ties = FALSE, ...) {

    ## entry control -------

    stopifnot(is_polcax(object))

    stopifnot(is.logical(resolve_ties))

    ## prediction --------

    if(is.null(newdata)) {

      return(components(object,
                        type = 'posterior',
                        resolve_ties = resolve_ties))

    }

    ## for some reasons, computation of the posterior matrix fails
    ## if the y argument is a multi-row matrix or data frame
    ## in turn, the posterior computation seems to work always
    ## for a single-row input so I'm implementing a backup in a loop

    suppressWarnings(

      post_p <-
        try(poLCA::poLCA.posterior(lc = object,
                                   y = newdata,
                                   x = x),
            silent = TRUE)

    )

    if(inherits(post_p, 'try-error')) {

      if(is.null(rownames(newdata))) {

        obs_id <- paste0('obs_', 1:nrow(newdata))

        newdata <- trafo::set_rownames(newdata, obs_id)

      } else {

        obs_id <- rownames(newdata)

      }

      row_lst <- purrr::map(obs_id, ~newdata[.x, ])

      post_p <-
        purrr::map(row_lst,
                   ~poLCA::poLCA.posterior(lc = object,
                                           y = .x,
                                           x = x))

      post_p <- do.call('rbind', post_p)

      post_p <- trafo::set_rownames(post_p, obs_id)

      post_p <- trafo::set_colnames(post_p, object[['class_names']])

    }

    assignment <- nodal_vote(post_p, resolve_ties = resolve_ties)

    post_p <- tibble::as_tibble(post_p)

    post_p <- tibble::as_tibble(cbind(assignment, post_p))

    return(lca_assign(post_p))

  }

# END -------
