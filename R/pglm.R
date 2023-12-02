#' Panel Estimators for Generalized Linear Models
#'
#' Estimation by maximum likelihood of glm (binomial and Poisson) and
#' 'glm-like' models (Negbin and ordered) on longitudinal data
#'
#' @name pglm
#' @param formula a symbolic description of the model to be estimated,
#' @param data the data: a `pdata.frame` object or an ordinary
#'     `data.frame`,
#' @param subset an optional vector specifying a subset of
#'     observations,
#' @param na.action a function which indicates what should happen when
#'     the data contains `NA`s,
#' @param effect the effects introduced in the model, one of
#'     `"individual"`, `"time"` or `"twoways"`,
#' @param model one of `"pooling"`, `"within"`, `"between"`,  `"random"`,
#' @param family the distribution to be used,
#' @param other for developper's use only,
#' @param index the index,
#' @param start a vector of starting values,
#' @param R the number of function evaluation for the gaussian quadrature  method used,
#' @param method the optimization method, one of `"bfgs"` (the default) and `"newton"`
#' @param trace an integer
#' @param link,vlink arguments of family functions
#' @param ... further arguments.
#' @return  An object of class `"miscr"`, a list with elements:
#' @importFrom stats .getXlevels coef deviance df.residual dnorm model.matrix model.response optim pnorm poisson qnorm resid sd var
#' @importFrom Formula as.Formula
#' @importFrom statmod gauss.quad
#' @importFrom plm pdata.frame
#' @import micsr
#' @examples
#' ## a binomial (probit) example
#' anb <- pglm(union ~ wage + exper + rural, union_wage, family = binomial('probit'),
#'             model = "pooling",  method = "bfgs", trace = 3, R = 5)
#' 
#' ## a gaussian example on unbalanced panel data
#' ra <- pglm(mv ~ crim + zn + indus + nox + age + rm, hedonic, family = gaussian,
#'            model = "random", trace = 3, method = "newton", index = "townid")
#' 
#' ## some count data models
#' la <- pglm(patents ~ lag(log(rd), 0:5) + scisect + log(capital72) + factor(year), patents_rd,
#'            family = negbin, model = "within", trace = 3, method = "newton",
#'            index = c('cusip', 'year'))
#' la <- pglm(patents ~ lag(log(rd), 0:5) + scisect + log(capital72) + factor(year), patents_rd,
#'            family = poisson, model = "pooling", index = c("cusip", "year"),
#'            tracen = 0, method="newton")
#' 
#' @author Yves Croissant
#' @export
pglm <-  function(formula, data, subset, na.action,
                  effect = c('individual', 'time', 'twoways'),
                  model  = c('random', 'pooling', 'within', 'between'),
                  family, other = NULL, index  = NULL, start = NULL, R = 20,
                  method = c("bfgs", "newton"), trace = 0, ...){
    .call <- match.call()
    .method <- match.arg(method)
    dots <- list(...)
    args <- list(model = model, effect = effect)
    if (is.character(family)){
        if (family %in% c("ordprobit", "ordlogit", "tobit")){
            if (family == "ordprobit") family <- list(family = "ordinal", link = "probit")
            if (family == "ordlogit") family <- list(family = "ordinal", link = "logit")
            if (family == "tobit") family <- list(family = "tobit", link = NULL)
        }
        else  family <- get(family, mode = "function")
    }
    if (is.function(family)) family <- family()
    
    link <- family$link
    if (family$family == "negbin") vlink <- family$vlink else vlink <- NULL
    family <- family$family
    
    # check and match the arguments
    effect <- match.arg(effect)
    if (! any(is.na(model))) model <- match.arg(model)

    if (inherits(data, "pdata.frame") && ! is.null(index))
        warning("the index argument is ignored because data is a pdata.frame")
    if (! inherits(data, "pdata.frame")) data <- pdata.frame(data, index)
    if (! inherits(formula, "Formula")) formula <- as.Formula(formula)

    # eval the model.frame
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
    mf <- mf[c(1,m)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    mf$formula <- data
    mf$data <- formula
    data <- eval(mf, parent.frame())
    mt <- attr(data, "terms")

    # return the model.frame or estimate the model
    if (is.na(model)){
        attr(data, "formula") <- formula
        return(data)
    }
    Kw <- NULL
    if (model != "pooling"){
        X <- model.matrix(data, rhs = 1, model = "pooling", effect = effect)
        if (model == "within" && family == "poisson"){
            Xw <- model.matrix(data, rhs = 1, model = "within", effect = effect)
            Kw <- colnames(Xw)
            X <- X[, Kw, drop = FALSE]
        }
        if (ncol(X) == 0) stop("empty model")
#        y <- pmodel.response(formula, data, model = "pooling", effect = effect)
        y <- model.response(data)
        id <- attr(data, "index")[[1]]
    }
    else{
        X <- model.matrix(data)
        y <- model.response(data)
        if (inherits(data, "pdata.frame")) id <- attr(data, "index")[[1]]
        else id <- NULL
    }
    # compute the nodes and the weights for the gaussian quadrature
    if (model == "random" && (! family %in% c("poisson", "negbin", "gaussian")))
        rn <- gauss.quad(R, kind = 'hermite')
    else rn <- NULL

    # check and eventually coerce the response for binomial and ordinal models
    if (family == "binomial"){
        if (is.character(y)) y <- factor(y)
        if (is.factor(y)) y <- as.numeric(y)
        if (length(unique(y)) != 2) stop("the response must have exactly 2 different values")
        if (min(y) != 0) y[y == min(y)] <- 0
        if (max(y) != 1) y[y == max(y)] <- 1
    }
    if (family == "ordinal"){
        if (is.character(y)) y <- factor(y)
        if (is.factor(y)) y <- as.numeric(y)
        if (length(unique(y)) < 3) stop("the response must have at least 3 different values")
    }

    # compute the starting vgalues
    start <- starting.values(family, link, vlink, rn, model, Kw, X, y, id, cl, start, other)

    if (family %in% c("tobit", "gaussian", "poisson")) args$other <- "other"
    if (family == "negbin") args$vlink <- "vlink"
    if (family == "negbin") fun <- lnl.negbin
    if (family == "poisson") fun <- lnl.poisson
    if (family == "gaussian") fun <- lnl.gaussian
    if (family == "binomial") fun <- lnl.binomial

    if (.method == "bfgs"){
        f_bfgs <- function(start) fun(start, y = y, X = X, id = id, vlink = vlink, link = link,
                                      other = other, rn = rn, neg = TRUE, model = model)
        g <- function(start) attr(fun(start, y = y, X = X, id = id, vlink = vlink, link = link,
                                      other = other, rn = rn, gradient = TRUE,
                                      neg = TRUE, model = model), "gradient")
        h <- function(start) attr(fun(start, y = y, X = X, id = id, vlink = vlink, link = link,
                                      other = other, rn = rn, hessian = TRUE,
                                      neg = TRUE, model = model), "hessian")
        z <- optim(start, f_bfgs, g, method = "BFGS", control = list(trace = trace))
        est_conv <- z$par
    }
    if (.method == "newton"){
        f <- function(start, gradient, hessian) fun(start, y = y, X = X, id = id, vlink = vlink, link = link,
                                                    other = other, rn = rn, neg = FALSE, model = model,
                                                    sum = TRUE, gradient = TRUE, hessian = TRUE)
        est_conv <- newton(f, start, trace = trace, direction = "max")
    }
    f_conv <- fun(est_conv, y = y, X = X, id = id, vlink = vlink, link = link,
                  other = other, rn = rn, neg = FALSE, model = model,
                  sum = FALSE, gradient = TRUE, hessian = TRUE, linpred = TRUE)
    value_conv <- sum(as.numeric(f_conv))
    g_conv <- attr(f_conv, "gradient")
    h_conv <- attr(f_conv, "hessian")
    linpred_conv <- attr(f_conv, "linpred")
    .K <- ncol(X)
    .L <- length(est_conv) - .K
    .npar <- structure(c(covariates = .K, vcov = .L), default = c("covariates", "vcov"))
    result <- list(coefficients = est_conv,
  #                 residuals = .resid,
                   linear.predictors = attr(f_conv, "linpred"),
                   model = data,
                   terms = mt,
                   logLik = as.numeric(f_conv),
                   value = structure(value_conv, nobs = nrow(data), df = length(est_conv), class = "logLik"),
                   npar = .npar,
                   gradient = attr(f_conv, "gradient"),
                   hessian = attr(f_conv, "hessian"),
#                   df.residual = N - K - L,
                   xlevels = .getXlevels(mt, mf),
                   na.action = attr(mf, "na.action"),
                   call = .call,
                   est_method = "ml"
                   )
    structure(result, class = "micsr")
}

#' @rdname pglm
#' @export
ordinal <- function(link = c('probit', 'logit')){
    link <- match.arg(link)
    list(family = 'ordinal', link = link)
}


#' @rdname pglm
#' @export
negbin <- function(link = c('log'), vlink = c('nb1', 'nb2')){
    link <- match.arg(link)
    vlink <- match.arg(vlink)
    list(family = 'negbin', link = link, vlink = vlink)
}

