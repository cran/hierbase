#' @importFrom stats glm lm anova pnorm
#' @importFrom glmnet cv.glmnet coef.glmnet

# pre-specified test functions

compMOD.NULL <- function(...) NULL

###########################
## F-test
###########################

# Those are the functions which are required to apply an F-test
test.func.F <- function(x, y, clvar, colnames.cluster,
                        arg.all, arg.all.fix, mod.large,
                        mod.small) {
  
  ### calculate smaller model ###
  # generate design matrices
  setdiff.cluster <- setdiff(colnames(x), colnames.cluster)
  
  # data.large <- cbind(clvar, x)
  data.small <- cbind(clvar, x[, setdiff.cluster]) # This results in a matrix although it might only have one column :-)
  if (ncol(data.small) == 0) {data.small <- rep(1, length(y))}
  
  # calculate smaller model
  mod.small <- lm(y ~ data.small, model = FALSE, qr = FALSE)
  
  ### compare the models ###
  # partial F test
  pval <- anova(mod.small,
                mod.large, # stats::lm(y ~ data.large),
                test = "F")$P[2]
  
  return(list("pval" = pval, "mod.small" = NULL))
}

# Smaller model is calculated for every test and is different
# for ever cluster. Therefore we do not return it in the function
# test.func and only use NULL as a plcae holder.
# => compMOD.NULL

compMOD.same.F <- function(x, y, clvar, arg.all, arg.all.fix) {
  
  data.large <- cbind(clvar, x)
  mod.large <- lm(y ~ data.large, model = FALSE, qr = FALSE)
  
  return(mod.large)
}

###########################
## LRT Test for logistic regression (other glm would be possbile if 
## correct family is chosen.)
###########################

# Those are the functions which are required to apply an LRT
test.func.LRT <- function(x, y, clvar, colnames.cluster,
                        arg.all, arg.all.fix, mod.large,
                        mod.small) {
  
  ### calculate smaller model ###
  # generate design matrices
  setdiff.cluster <- setdiff(colnames(x), colnames.cluster)
  
  # data.large <- cbind(clvar, x)
  data.small <- cbind(clvar, x[, setdiff.cluster]) # This results in a matrix although it might only have one column :-)
  if (ncol(data.small) == 0) {data.small <- rep(1, length(y))}
  
  # calculate smaller model
  mod.small <- glm(y ~ data.small, family = "binomial")
  
  ### compare the models ###
  # partial F test
  pval <- anova(mod.small, mod.large,
                test = "Chisq")$"Pr(>Chi)"[2]
  
  return(list("pval" = pval, "mod.small" = NULL))
}

# Smaller model is calculated for every test and is different
# for ever cluster. Therefore we do not return it in the function
# test.func and only use NULL as a plcae holder.
# => compMOD.NULL

compMOD.same.LRT <- function(x, y, clvar, arg.all, arg.all.fix) {

  data.large <- cbind(clvar, x)
  mod.large <- glm(y ~ data.large, family = "binomial")
  
  return(mod.large)
}

###########################
## debiased Lasso / hdi: build in test function!
###########################

# Compute output of lasso for each data set
compMOD.same.hdi <- function(x, y, clvar, arg.all, arg.all.fix) {
  standardize <- arg.all.fix$standardize
  fam <- arg.all.fix$family
  
  # if (!is.null(clvar)) {
  #   warning("You have specified control variables using the argument clvar. Those variables are included in the model and a L1-penalty is imposed on them as well for the initial lasso fit.")
  # }
  
  # argument x is the design matrix (without intercept) see help file of lasso.proj
  initial.lasso.fit <- hdi::lasso.proj(cbind(x, clvar), y, family = fam, 
                                       standardize = standardize)

  return(list(initial.lasso.fit = initial.lasso.fit))
} # {compMOD.same.hdi}

# Compute the p-value for a given cluster and given data set
#
# Compute the p-value for a given cluster (specified by the
# argument \code{colnames.cluster}) and given data set.
test.func.hdi <- function(x, y, clvar, arg.all, colnames.cluster,
                          arg.all.fix, mod.large, mod.small){
  
  fit.lasso <- mod.large$initial.lasso.fit

  # test.set is defines the set of variables to be test  
  test.set <- which(colnames(x) %in% colnames.cluster)
  
  # p-value
  pval <- fit.lasso$groupTest(test.set)
  
  return(list("pval" = pval, "mod.small" = mod.small)) # mod.small is anyway NULL
} # {test.func.hdi}

###########################
## group test: QF (Group inference in regression models can be measured with 
## respect to a weighted Quadratic Functional (QF) of the regression sub-vector 
## cor- responding to the group.)
###########################

# Compute output of lasso for each data set
compMOD.same.QF <- function(x, y, clvar, arg.all, arg.all.fix) {

  # In the previous version of SIHR::QF, there was an argument to set the intercept 
  # to TRUE and FALSE. We always want an initial Lasso fit with an intercept.
  intercept <- TRUE 
  lambda <- arg.all.fix$lambda
  
  # if (!is.null(clvar)) {
  #   warning("You have specified control variables using the argument clvar. Those variables are included in the model and a L1-penalty is imposed on them as well for the initial lasso fit.")
  # }
  
  # initial.lasso.fit <- SIHR:::Initialization.step(X = cbind(x, clvar), y = y, lambda = lambda,
  #                                                 intercept = intercept)

  # Alternative function call to glmnet / Lasso with CV selected lambda 
  # instead of calling SIHR:::Initialization.step
  # Case if is.null(lambda) in the above function call
  X <- cbind(x, clvar)
  n <- nrow(X)
  col.norm <- 1 / sqrt((1 / n) * diag(t(X) %*% X))
  Xnor <- X %*% diag(col.norm)

  outLas <- cv.glmnet(X, y, family = "gaussian", alpha = 1,
                      intercept = intercept)
  htheta <- as.vector(coef.glmnet(outLas, s = outLas$lambda.min))

    if (intercept == TRUE) {
    Xb <- cbind(rep(1, n), Xnor)
    col.norm <- c(1, col.norm)
  } else {
    Xb <- Xnor
  }
  htheta <- htheta * col.norm

  # We have to remove the intercept because only an initial estimate of 
  # the coefficient of X_1, ..., X_p is required and asked for as an 
  # input of the argument init.coef of the function SIHR::QF. 
  # We still want to estimate the initial Lasso fit with an intercept
  # because the initial Lasso fit could be biased if we estimate it 
  # without an intercept but there is a "true underlying" intercept 
  # required. 
  initial.lasso.fit <- list("lasso.est" = htheta[-1]) # remove intercept => [-1]
  
  return(initial.lasso.fit)
} # {compMOD.same.QF}

# Compute the p-value for a given cluster and given data set
#
# Compute the p-value for a given cluster (specified by the
# argument \code{colnames.cluster}) and given data set.
test.func.QF <- function(x, y, clvar, arg.all, colnames.cluster,
                         arg.all.fix, mod.large, mod.small){

  Cov.weight <- arg.all.fix$Cov.weight
  A <- arg.all.fix$A
  # previous version of SIHR: intercept <- arg.all.fix$intercept
  tau.vec <- arg.all.fix$tau.vec
  lambda <- arg.all.fix$lambda
  mu <- arg.all.fix$mu
  step <- arg.all.fix$step
  resol <- arg.all.fix$resol
  maxiter <- arg.all.fix$maxiter


  # Group Test
  # Group_Test takes three inputs
  # X is the covariates
  # y is the outcome
  # test.set is the set of variables to be tested.
  # Note that the intercept is not included in the test set.
  # For covariates X1, X2, ... Xp, the corresponding index would be 1,2, ... p
  ind.test.set <- which(colnames(x) %in% colnames.cluster)
  
  Est <- SIHR::QF(X = cbind(x, clvar), y = y,
                  G = ind.test.set,
                  Cov.weight = Cov.weight,
                  A = A, 
                  tau.vec = tau.vec, 
                  init.coef = mod.large$lasso.est, 
                  lambda = lambda, mu = mu,
                  step = step, resol = resol,
                  maxiter = maxiter)

  # output:
  # prop.point is the proposed point estimator
  # prop.sd is the standard deviation of the point estimator


  # se times \eta
  se.eta <- Est$se * 1.1

  # p-value
  pval <- pnorm(q = - Est$prop.est / se.eta)


  return(list("pval" = pval, "mod.small" = mod.small))
} # {test.func.QF}

