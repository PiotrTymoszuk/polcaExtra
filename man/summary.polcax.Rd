% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general_methods.R
\name{summary.polcax}
\alias{summary.polcax}
\title{Statistic Summary of the polcax Object}
\usage{
\method{summary}{polcax}(object, ...)
}
\arguments{
\item{object}{an object of the \code{polcax} class.}

\item{...}{extra arguments passed to the methods, currently none.}
}
\value{
A tibble with the following columns:
\itemize{
\item \code{k} number of classes
\item \code{numiter} number of iterations required for the \code{poLCA} model to converge
\item \code{maxiter} maximal number of iterations
\item \code{convergence} a logical value indicating it model convergence was reached
\item \code{Nvars} number of modeling variables
\item \code{N} total number of cases
\item \code{Nobs} number of complete cases
\item \code{llik} maximum value of the log-likelihood
\item \code{aic} Akaike Information Criterion
\item \code{bic} Bayesian Information Criterion
\item \code{Gsq} likelihood ratio/deviance statistic
\item \code{Chisq} Pearson Chi-square goodness of fit statistic for fitted versus
observed multiway tables
\item \code{df} number of degrees of freedom in the model
\item \code{resid.df} number of residual degrees of freedom
\item \code{valid} a logical value indicating if the model was properly parameterized,
i.e. if residual degrees of freedom were greater or equal than 0
}
}
\description{
Provides a summary of the fit statistics for a \code{polcax} model.
}
