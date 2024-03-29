% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pglm.R
\name{pglm}
\alias{pglm}
\alias{ordinal}
\alias{negbin}
\title{Panel Estimators for Generalized Linear Models}
\usage{
pglm(
  formula,
  data,
  subset,
  na.action,
  effect = c("individual", "time", "twoways"),
  model = c("random", "pooling", "within", "between"),
  family,
  other = NULL,
  index = NULL,
  start = NULL,
  R = 20,
  method = c("bfgs", "newton"),
  trace = 0,
  ...
)

ordinal(link = c("probit", "logit"))

negbin(link = c("log"), vlink = c("nb1", "nb2"))
}
\arguments{
\item{formula}{a symbolic description of the model to be estimated,}

\item{data}{the data: a \code{pdata.frame} object or an ordinary
\code{data.frame},}

\item{subset}{an optional vector specifying a subset of
observations,}

\item{na.action}{a function which indicates what should happen when
the data contains \code{NA}s,}

\item{effect}{the effects introduced in the model, one of
\code{"individual"}, \code{"time"} or \code{"twoways"},}

\item{model}{one of \code{"pooling"}, \code{"within"}, \code{"between"},  \code{"random"},}

\item{family}{the distribution to be used,}

\item{other}{for developper's use only,}

\item{index}{the index,}

\item{start}{a vector of starting values,}

\item{R}{the number of function evaluation for the gaussian quadrature  method used,}

\item{method}{the optimization method, one of \code{"bfgs"} (the default) and \code{"newton"}}

\item{trace}{an integer}

\item{...}{further arguments.}

\item{link, vlink}{arguments of family functions}
}
\value{
An object of class \code{"miscr"}, a list with elements:
}
\description{
Estimation by maximum likelihood of glm (binomial and Poisson) and
'glm-like' models (Negbin and ordered) on longitudinal data
}
\examples{
## a binomial (probit) example
anb <- pglm(union ~ wage + exper + rural, union_wage, family = binomial('probit'),
            model = "pooling",  method = "bfgs", trace = 3, R = 5)

## a gaussian example on unbalanced panel data
ra <- pglm(mv ~ crim + zn + indus + nox + age + rm, hedonic, family = gaussian,
           model = "random", trace = 3, method = "newton", index = "townid")

## some count data models
la <- pglm(patents ~ lag(log(rd), 0:5) + scisect + log(capital72) + factor(year), patents_rd,
           family = negbin, model = "within", trace = 3, method = "newton",
           index = c('cusip', 'year'))
la <- pglm(patents ~ lag(log(rd), 0:5) + scisect + log(capital72) + factor(year), patents_rd,
           family = poisson, model = "pooling", index = c("cusip", "year"),
           tracen = 0, method="newton")

}
\author{
Yves Croissant
}
