% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/passthrough.R
\name{lms.passthrough.transform_residualize_lmfit}
\alias{lms.passthrough.transform_residualize_lmfit}
\title{lmfit-based function to residualize out covariates from a given dataset.}
\usage{
lms.passthrough.transform_residualize_lmfit(
  input_data,
  residualize_var,
  discrete_vars,
  continuous_vars,
  return_new_vars = F
)
}
\arguments{
\item{input_data}{The data containing the outcome and covariates (data.table).}

\item{residualize_var}{The name of the outcome variable (character).}

\item{discrete_vars}{A vector of discrete variable names (character).}

\item{continuous_vars}{A vector of continuous variable names (character).}

\item{return_new_vars}{If TRUE, returns new variables called _residual and _predicted.
If FALSE, overwrites the residualize_var with its residuals.}
}
\description{
This function regresses an outcome on a given set of covariates and
obtains and stores the residual for each observation.
}
