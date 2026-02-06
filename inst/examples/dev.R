# Development and trials

# tools ------

  library(tidyverse)
  library(rlang)
  library(trafo)
  library(poLCA)

# test data ------

  test_data <- read_tsv('./inst/examples/mds.tsv')

  test_vars <- names(test_data)[names(test_data) != 'ID']

  test_data[test_vars] <- test_data[test_vars] %>%
    map_dfc(factor, c('WT', 'mutated'))

  test_data <- test_data %>%
    column_to_rownames('ID')

  test_formula <- paste0('cbind(',
                         paste(test_vars, collapse = ', '),
                         ') ~ 1') %>%
    as.formula

  set.seed(12345)

  test_model <- poLCA(test_formula,
                      test_data,
                      nclass = 3)

# construction of the polcax class, summary, components -------

  test_model <- as_polcax(test_model)

  summary(test_model)

  components(test_model, type = 'class_names')

  components(test_model, type = 'fit_stats')

  components(test_model, type = 'posterior', resolve_ties = FALSE)

  rbind(c(0.5, 0.5, 0),
        c(0, 0.5, 0.5)) %>%
    set_rownames(c('obs_1', 'obs_2')) %>%
    set_colnames(c('class_1', 'class_2', 'class_3')) %>%
    nodal_vote(resolve_ties = TRUE)

  components(test_model, type = 'class_numbers')

  components(test_model, type = 'prior_p')

  components(test_model, type = 'variables')

  components(test_model, type = 'data')

  components(test_model, type = 'predcell')

  components(test_model, type = 'probs')

  components(test_model, type = 'probs.se')

# Predict method -------

  predict(test_model, newdata = NULL)

  predict(test_model, newdata = test_data[sample(1:109, 50), ])

# Plot for the 'lca_assign' class -------

  components(test_model, 'assignment') %>%
    plot(type = 'class_distribution',
         flip = TRUE)

  components(test_model, 'posterior') %>%
    plot(type = 'class_posterior',
         flip = TRUE)

# Plots for the 'polcax' class ------

  plot(test_model, type = 'poLCA')

  plot(test_model, type = 'class_distribution')

  plot(test_model, type = 'class_posterior', flip = TRUE)

  plot(test_model, type = 'prior_distribution', flip = TRUE)

  plot(test_model, type = 'prior_se')

  plot(test_model, type = 'conditional_probs')

# tuning ------

  future::plan('multisession')

  test_tune <-
    tune_lca(formula = test_formula,
             data = test_data,
             class_range = 1:6,
             nrep = 10,
             maxiter = 2000,
             seed = 12345,
             metric = 'bic')

  future::plan('sequential')

# coercion to a cluster analysis object -------

  ## starting with a polcax model

  test_clust <- test_model %>%
    lca2clust(distance_method = 'jaccard')

  test_clust %>%
    plot('heat_map')

  test_clust %>%
    clustTools::plot_clust_hm(discrete_fill = TRUE) +
    scale_fill_gradient(low = "steelblue",
                        high = "firebrick",
                        breaks = c(0, 1),
                        labels = c('0' = 'WT',
                                   '1' = 'mutated'),
                        name = '')

  ## starting with the class assignment

  test_clust_class2 <- test_tune$models$class_2 %>%
    components('assignment') %>%
    lca2clust(data = test_data,
              distance_method = 'jaccard')

  test_clust_class2 %>%
    clustTools::plot_clust_hm(discrete_fill = TRUE) +
    scale_fill_gradient(low = "steelblue",
                        high = "firebrick",
                        breaks = c(0, 1),
                        labels = c('0' = 'WT',
                                   '1' = 'mutated'),
                        name = '')

# END ----
