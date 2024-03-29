# Implementing the plot method for the 'polcax' and 'lca_assign' classes

# lca_assign -------

#' Graphics for the Latent Class Assignment
#'
#' @description
#' Plots class distribution or probability of class assignment.
#'
#' @details
#' Two types of plots can be generated as specified by the `type` argument:
#'
#' * `class_distribution` plot class distribution in a stack plot
#'
#' * `class_posterior` plots probabilities of class assignment for observations
#' split by the class
#'
#' The last plot type may be useful at assessment of the general
#' reliability of the model and identification of 'fence-sitter' observations
#' or classes with with doubtful observation assignment.
#'
#' @return a `ggplot` object.
#'
#' @param x a `lca_assign` class object.
#' @param type the requested plot type, see: `Details`.
#' @param signif_digits significant digits.
#' @param txt_size size of the text displayed in the plot labels.
#' @param txt_color color of the label text.
#' @param point_size point size.
#' @param point_alpha point alpha.
#' @param point_hjitter height of point jittering.
#' @param point_wjitter width of point jittering.
#' @param flip logical: flip the plot axes? Defaults to `FALSE`.
#' @param cust_theme custom ggplot graphic theme.
#' @param ... extra arguments, currently none.
#'
#' @export plot.lca_assign
#' @export

  plot.lca_assign <- function(x,
                              type = c('class_distribution',
                                       'class_posterior'),
                              signif_digits = 2,
                              txt_size = 2.75,
                              txt_color = 'black',
                              point_size = 2,
                              point_alpha = 0.8,
                              point_hjitter = 0.005,
                              point_wjitter = 0.005,
                              flip = FALSE,
                              cust_theme = ggplot2::theme_classic(), ...) {

    ## entry control ------

    stopifnot(is_lca_assign(x))
    stopifnot(is.numeric(signif_digits))

    signif_digits <- as.integer(signif_digits)

    stopifnot(is.numeric(txt_size))
    stopifnot(is.numeric(point_size))
    stopifnot(is.numeric(point_alpha))
    stopifnot(is.numeric(point_hjitter))
    stopifnot(is.numeric(point_wjitter))
    stopifnot(is.logical(flip))

    type <- match.arg(type[1],
                      c('class_distribution',
                        'class_posterior'))

    if(!ggplot2::is.theme(cust_theme)) {

      stop("'cust_theme' hast to be a valid ggplot theme object.",
           call. = FALSE)

    }

    n <- NULL
    percent <- NULL
    y_pos <- NULL
    best_p <- NULL
    observation <- NULL

    ## counts -----

    distr_tbl <- dplyr::count(x, class)

    ## class distribution -------

    if(type == 'class_distribution') {

      distr_tbl <- dplyr::arrange(distr_tbl, dplyr::desc(class))

      distr_tbl <- dplyr::mutate(distr_tbl,
                                 percent = n/sum(n) * 100,
                                 y_pos = cumsum(n) - 0.5 * n)

      if(flip) {

        out_plot <-
          ggplot2::ggplot(distr_tbl,
                          ggplot2::aes(y = 'Class',
                                       x = percent,
                                       fill = class)) +
          ggplot2::geom_bar(color = 'black',
                            stat = 'identity') +
          ggplot2::geom_label(ggplot2::aes(label = signif(percent, signif_digits),
                                           x = y_pos),
                              size = txt_size,
                              color = txt_color,
                              show.legend = FALSE) +
          ggplot2::labs(x = '% of the dataset') +
          cust_theme +
          ggplot2::theme(axis.title.y = ggplot2::element_blank())

      } else {

        out_plot <-
          ggplot2::ggplot(distr_tbl,
                          ggplot2::aes(x = 'Class',
                                       y = percent,
                                       fill = class)) +
          ggplot2::geom_bar(color = 'black',
                            stat = 'identity') +
          ggplot2::geom_label(ggplot2::aes(label = signif(percent, signif_digits),
                                           y = y_pos),
                              size = txt_size,
                              color = txt_color,
                              show.legend = FALSE) +
          ggplot2::labs(y = '% of the dataset') +
          cust_theme +
          ggplot2::theme(axis.title.x = ggplot2::element_blank())

      }

      out_plot <- out_plot +
        ggplot2::labs(title = 'Class distribution',
                      subtitle = paste('complete: n =', sum(distr_tbl$n)))

      return(out_plot)

    }

    ## posterior probabilities ------

    if(type == 'class_posterior') {

      n_legends <-
        purrr::map2_chr(distr_tbl[[1]],
                        distr_tbl[[2]],
                        paste, sep = '\nn = ')

      n_legends <- rlang::set_names(n_legends,
                                    distr_tbl[[1]])

      if(flip) {

        out_plot <-
          ggplot2::ggplot(x,
                          ggplot2::aes(y = best_p,
                                       x = stats::reorder(observation, best_p),
                                       fill = class)) +
          ggplot2::facet_grid(. ~ class,
                              scales = 'free',
                              space = 'free',
                              labeller = ggplot2::as_labeller(n_legends)) +
          cust_theme +
          ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank()) +
          ggplot2::labs(y = expression('p'[posterior]),
                        x = 'Observation')

      } else {

        out_plot <-
          ggplot2::ggplot(x,
                          ggplot2::aes(x = best_p,
                                       y = stats::reorder(observation, best_p),
                                       fill = class)) +
          ggplot2::facet_grid(class ~ .,
                              scales = 'free',
                              space = 'free',
                              labeller = ggplot2::as_labeller(n_legends)) +
          cust_theme +
          ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_blank()) +
          ggplot2::labs(x = expression('p'[posterior]),
                        y = 'Observation')


      }

      out_plot <- out_plot +
        ggplot2::geom_point(shape = 21,
                            size = point_size,
                            alpha = point_alpha,
                            position = ggplot2::position_jitter(width = point_wjitter,
                                                                height = point_hjitter)) +
        ggplot2::labs(title = 'Class assignment probability',
                      subtitle = paste('complete: n =', sum(distr_tbl$n)))

      return(out_plot)

    }

  }

# polcax ------

#' Graphics for the polcax Class
#'
#' @description
#' Plots for `polcax` class objects.
#'
#' @details
#' The method provides a variety of plots to visualize class distribution,
#' prior, posterior and conditional probabilities
#' specified by the `type` argument:
#'
#' * `poLCA` generates a traditional `poLCA` plot of conditional probabilities
#'
#' * `class_distribution` plots distribution of the classes expressed as
#' percentages of all complete observations
#'
#' * `prior_distribution` plots distribution of the prior probabilities
#'
#' * `prior_se` plots prior p values with their standard errors
#'
#' * `class_posterior` plots probabilities of class assignment for observations
#' split by the class
#'
#' * `conditional_probs` returns a list of plots with conditional
#' probabilities of the class assignment for each modeling variable
#'
#' @return a ggplot object or a list of ggplot objects.
#'
#' @param x a `polcax` object.
#' @param type type of the plot or plots, see: `Details`.
#' @param resolve_ties logical: should ties at class assignment be resolved
#' at random? Defaults to `FALSE`.
#' @param signif_digits significant digits.
#' @param txt_size size of the text displayed in the plot labels.
#' @param txt_color color of the label text.
#' @param txt_hjust horizontal justification of the label text.
#' @param txt_vjust vertical justification of the label text.
#' @param point_size point size.
#' @param point_alpha point alpha.
#' @param point_hjitter height of point jittering.
#' @param point_wjitter width of point jittering.
#' @param flip logical: flip the plot axes? Defaults to `FALSE`.
#' @param cust_theme custom ggplot graphic theme.
#' @param ... extra arguments, currently none.
#'
#' @export plot.polcax
#' @export

  plot.polcax <- function(x,
                          type = c('poLCA',
                                   'class_distribution',
                                   'prior_distribution',
                                   'prior_se',
                                   'class_posterior',
                                   'conditional_probs'),
                          resolve_ties = FALSE,
                          signif_digits = 2,
                          txt_size = 2.75,
                          txt_color = 'black',
                          txt_hjust = 0,
                          txt_vjust = -1.4,
                          point_size = 2,
                          point_alpha = 0.8,
                          point_hjitter = 0.005,
                          point_wjitter = 0.005,
                          flip = FALSE,
                          cust_theme = ggplot2::theme_classic(), ...) {

    ## entry control -------

    stopifnot(is_polcax(x))

    type <- match.arg(type[1],
                      c('poLCA',
                        'class_distribution',
                        'prior_distribution',
                        'prior_se',
                        'class_posterior',
                        'conditional_probs'))

    stopifnot(is.logical(resolve_ties))
    stopifnot(is.numeric(signif_digits))

    signif_digits <- as.integer(signif_digits)

    stopifnot(is.numeric(txt_size))
    stopifnot(is.numeric(point_size))
    stopifnot(is.numeric(point_alpha))
    stopifnot(is.numeric(point_hjitter))
    stopifnot(is.numeric(point_wjitter))
    stopifnot(is.logical(flip))

    if(!ggplot2::is.theme(cust_theme)) {

      stop("'cust_theme' must be a valid ggplot theme object.", call = FALSE)

    }

    p_prior <- NULL
    se_prior <- NULL
    y_pos <- NULL
    plot_lab <- NULL
    variable <- NULL
    p_conditional <- NULL

    ## numbers of observations per class ------

    class_n <- components(x, type = 'class_numbers')

    n_legends <-
      purrr::map2_chr(class_n[[1]], class_n[[2]],
                      paste, sep = '\nn = ')

    n_legends <- rlang::set_names(n_legends, class_n[[1]])

    ## poLCA conditional probability plot ---------

    if(type == 'poLCA') NextMethod()

    ## plotting the class distribution and posteriors -------

    if(type %in% c('class_distribution', 'class_posterior')) {

      post_p <- components(x,
                           type = 'posterior',
                           resolve_ties = resolve_ties)

      return(plot.lca_assign(post_p,
                             type = type,
                             signif_digits = signif_digits,
                             txt_size = txt_size,
                             txt_color = txt_color,
                             point_size = point_size,
                             point_alpha = point_alpha,
                             point_hjitter = point_hjitter,
                             point_wjitter = point_wjitter,
                             flip = flip,
                             cust_theme = cust_theme))

    }

    ## prior distribution ----------

    if(type %in% c('prior_distribution', 'prior_se')) {

      prior_p <- components(x, type = 'prior_p')

      prior_p <- dplyr::arrange(prior_p, dplyr::desc(class))

      prior_p <- dplyr::mutate(prior_p,
                               y_pos = cumsum(p_prior) - 0.5 * p_prior,
                               plot_lab = paste(signif(p_prior, signif_digits),
                                                signif(se_prior, signif_digits),
                                                sep = ' \u00B1 '))

    }

    if(type == 'prior_distribution') {

      if(flip) {

        out_plot <-
          ggplot2::ggplot(prior_p,
                           ggplot2::aes(x = p_prior,
                                        y = 'Class',
                                        fill = class)) +
          ggplot2::geom_bar(stat = 'identity',
                            color = 'black') +
          ggplot2::geom_label(ggplot2::aes(label = signif(p_prior, signif_digits),
                                           x = y_pos),
                              size = txt_size,
                              color = txt_color,
                              show.legend = FALSE) +
          cust_theme +
          ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
          ggplot2::labs(x = 'fraction of observations')

      } else {

        out_plot <-
          ggplot2::ggplot(prior_p,
                           ggplot2::aes(y = p_prior,
                                        x = 'Class',
                                        fill = class)) +
          ggplot2::geom_bar(stat = 'identity',
                            color = 'black') +
          ggplot2::geom_label(ggplot2::aes(label = signif(p_prior, signif_digits),
                                           y = y_pos),
                              size = txt_size,
                              color = txt_color) +
          cust_theme +
          ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
          ggplot2::labs(y = 'fraction of observations')

      }

      out_plot <- out_plot +
        ggplot2::labs(title = 'Prior probability distribution',
                      subtitle = paste('complete: n =', sum(class_n[['n']])))

      return(out_plot)

    }

    if(type == 'prior_se') {

      out_plot <-
        ggplot2::ggplot(prior_p,
                        ggplot2::aes(x = p_prior,
                                     y = class,
                                     color = class)) +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = p_prior - se_prior,
                                             xmax = p_prior + se_prior),
                                height = 0) +
        ggplot2::geom_point(shape = 16,
                            size = point_size) +
        ggplot2::geom_text(ggplot2::aes(label = plot_lab),
                           size = txt_size,
                           hjust = txt_hjust,
                           vjust = txt_vjust) +
        ggplot2::scale_y_discrete(labels = n_legends) +
        cust_theme +
        ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
        ggplot2::labs(title = 'Prior probability estimates',
                      subtitle = paste('complete: n =', sum(class_n[['n']])),
                      x = 'Fraction of observations')

      return(out_plot)

    }

    if(type == 'conditional_probs') {

      plot_tbl <- components(x, type = 'probs')

      levs <- purrr::map(plot_tbl, ~names(.x)[names(.x) != 'class'])

      plot_tbl <-
        purrr::map2(plot_tbl,
                    levs,
                    ~tidyr::pivot_longer(.x,
                                         cols = dplyr::all_of(.y),
                                         names_to = 'variable',
                                         values_to = 'p_conditional'))

      plot_tbl <-
        purrr::map2(plot_tbl,
                    levs,
                    ~dplyr::mutate(.x,
                                   variable = factor(variable, levels = .y)))

      plot_tbl <- purrr::map(plot_tbl, dplyr::arrange, dplyr::desc(variable))

      plot_tbl <- purrr::map(plot_tbl, trafo::blast, class)

      plot_tbl <-
        purrr::map(plot_tbl,
                   purrr::map_dfr,
                   dplyr::mutate,
                   y_pos = cumsum(p_conditional) - 0.5 * p_conditional)

      plot_lst <-
        purrr::pmap(list(x = plot_tbl,
                         y = names(plot_tbl)),
                    function(x, y) ggplot2::ggplot(x,
                                                   ggplot2::aes(x = class,
                                                                y = p_conditional,
                                                                fill = variable)) +
                      ggplot2::geom_bar(stat = 'identity',
                                        color = 'black') +
                      ggplot2::geom_label(ggplot2::aes(label = signif(p_conditional, signif_digits),
                                                       y = y_pos),
                                          size = txt_size,
                                          color = txt_color) +
                      ggplot2::scale_x_discrete(labels = n_legends) +
                      cust_theme +
                      ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
                      ggplot2::labs(title = y,
                                    subtitle = paste('complete: n = ', sum(class_n[['n']])),
                                    y = expression('p'[conditional])))

      return(plot_lst)

    }

  }

# END ------
