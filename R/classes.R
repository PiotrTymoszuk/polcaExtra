# Implements the 'polcax' and 'lca_assign' classes and coercion functions

# The polcax class constructor --------

#' polcax Class
#'
#' @description
#' Constructs an instance of the `polcax` class, which extends the
#' diagnostic and graphical interface for latent class models fitted with
#' \code{\link[poLCA]{poLCA}}.
#'
#' @details
#' The output object contains the usual components of the
#' \code{\link[poLCA]{poLCA}} class and the `class_names` vector, which stores
#' the class names.
#' The input LCA models hast to have named observations, i.e. the modeling data
#' frame or matrix in the \code{\link[poLCA]{poLCA}} model
#' has to have row names.
#'
#' @return an object of the `polcax` class, which inherits from
#' the genuine `poLCA` models.
#'
#' @param x and object used to build the `polcax` class instance.
#' Currently only `poLCA` models are accepted.
#' @param class_names an optional argument specifying custom class names.
#' If `NULL`, class names are created with the 'class_1' ... 'class_n' scheme.
#' @param ... arguments for the methods, currently none.
#' @export

  polcax <- function(x, class_names = NULL, ...) {

    ## entry control

    if(!inherits(x, 'poLCA')) {

      stop("'x' must be a 'poLCA' model.", call. = FALSE)

    }

    if(is.null(class_names)) {

      class_names <- paste0('class_', 1:length(x[['P']]))

    }

    if(length(class_names) != length(x[['P']])) {

      err_txt <-
        paste("Invalid length of the 'class_names' argument.",
              "A vector with", length(x[['P']]), "names is required.")

      stop(err_txt, call. = FALSE)

    }

    if(is.null(rownames(x[['y']]))) {

      stop("The input poLCA model must contain named observations.",
           call. = FALSE)

    }

    input_classes <-
      map_lgl(x[['y']], is.factor)

    if(any(!input_classes)) {

      stop("The input poLCA model must contain named observations.",
           call. = FALSE)

    }

    ## construction

    model_classes <- c('polcax', class(x))

    structure(c(x, list(class_names = class_names)),
              class = model_classes)

  }

#' @rdname polcax
#' @export

  as_polcax <- function(x, ...) {

    UseMethod('as_polcax')

  }

#' @rdname polcax
#' @export

  as_polcax.poLCA <- function(x, class_names = NULL, ...) {

    polcax(x, class_names, ...)

  }

# The lca_assign class constructor ------

#' lca_assign Class
#'
#' @description
#' Constructs an instance of the `lca_assign` class on the top of a data frame.
#'
#' @details
#' The input object needs to be a data frame or a tibble with the following
#' minimal variable set:
#'
#' * `observation` with unique observation ID
#'
#' * `class` a factor with the class identifiers
#'
#' * `best_p` a numeric value with the probability of the class assignment
#'
#' See also: the `plot` method for the `lca_assign` class.
#'
#' @param x an object. Currently data frames and tibbles are accepted.
#' @param ... extra arguments, currently none.
#'
#' @export

  lca_assign <- function(x, ...) {

    ## entry control -------

    if(!is.data.frame(x)) {

      stop("'x' must be a data frame or a tibble.", call. = FALSE)

    }

    if(any(!c('observation', 'class', 'best_p') %in% names(x))) {

      err_txt <-
        paste("The input data frame has to contain the following columns:",
              "'observation', 'class' and 'best_p'")

      stop(err_txt, call. = FALSE)

    }

    if(!is.factor(x[['class']])) {

      stop("The variable 'class' has to be a factor.", call. = FALSE)

    }

    if(!is.numeric(x[['best_p']])) {

      stop("The variable 'best_p' must be numeric.", call. = FALSE)

    }

    if(any(x[['best_p']] > 1) | any(x[['best_p']] < 0)) {

      stop("Values of the [0,1] range are allowed for the 'best_p' variable.",
           call. = FALSE)

    }

    ## construction -------

    structure(x,
              class = c('lca_assign', class(x)))


  }

# Testing class inheritance ------

#' polcax Class Check
#'
#' @description
#' Checks if an object belongs to the `polcax` class.
#'
#' @return a logical value.
#' @param x as object.
#' @export

  is_polcax <- function(x) {

    inherits(x, 'polcax')

  }

#' lca_assign Class Check
#'
#' @description
#' Checks if an object belongs to the `lca_assign` class.
#'
#' @return a logical value.
#' @param x as object.
#' @export

  is_lca_assign <- function(x) {

    inherits(x, 'lca_assign')

  }

# END ------
