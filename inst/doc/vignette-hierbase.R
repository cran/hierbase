## ----eval=TRUE----------------------------------------------------------------
# load the packages
library(hierbase)
library(MASS) # for generating x

# random number generator (for parallel computing)
RNGkind("L'Ecuyer-CMRG")

# generate a toy data set
n <- 100
p <- 50 # 200
set.seed(3)                           # set a seed
x <- mvrnorm(n, mu = rep(0, p), Sigma = diag(p))  # data matrix x
colnames(x) <- paste0("Var", 1:p)     # column names
beta <- rep(0, p)                     # coefficients
beta[c(5, 20, 46)] <- 1               # three active covariates
y <- x %*% beta + rnorm(n)            # response

# estimate hierarchical tree
dendr1 <- cluster_vars(x = x)

# run hierarchical procedure
set.seed(4)
res <- advance_hierarchy(x, y, dendr = dendr1, test = "QF") 
res

## ----eval=FALSE---------------------------------------------------------------
#  # # estimate hierarchical tree
#  # dendr1 <- cluster_vars(x = x)
#  
#  # define test function
#  # low-dimensional partial F-Test
#  test.func.F <- function(x, y, clvar, colnames.cluster,
#                          arg.all, arg.all.fix, mod.large,
#                          mod.small) {
#    ## larger model
#    data.large <- cbind(clvar, x)
#    # estimate larger model
#    mod.large <- lm(y ~ data.large)
#  
#    ## smaller model
#    setdiff.cluster <- setdiff(colnames(x), colnames.cluster)
#    data.small <- cbind(clvar, x[, setdiff.cluster])
#    # special case if data.small is empty
#    if (ncol(data.small) == 0) {data.small <- rep(1, length(y))}
#  
#    # calculate smaller model
#    mod.small <- lm(y ~ data.small)
#  
#    ## compare the models
#    # partial F test
#    pval <- anova(mod.small, mod.large, test = "F")$P[2]
#  
#    return(list("pval" = pval, "mod.small" = NULL))
#  }
#  
#  # run hierarchical procedure
#  set.seed(4)
#  res2 <- run_hierarchy(x, y, dendr = dendr1, test.func = test.func.F)
#  res2

## ----eval=FALSE---------------------------------------------------------------
#  ## With block
#  # The user defines the second level of the hierarchical tree.
#  block <- data.frame("var.name" = paste0("Var", 1:p),
#                     "block" = rep(c(1, 2), each = p/2))
#  
#  # Estimate the hierarchical cluster tree in parallel.
#  # The argument block defines the second level of the tree.
#  dendr2 <- cluster_vars(x = x, block = block,
#                        # the following arguments have to be specified
#                        # for parallel computation
#                        parallel = "multicore",
#                        ncpus = 2)
#  
#  # Run the hierarchical procedure in parallel.
#  set.seed(76)
#  res2 <- advance_hierarchy(x = x, y = y, dendr = dendr2,
#                            test = "QF",
#                            # the following arguments have to be specified
#                            # for parallel computation
#                            parallel = "multicore",
#                            ncpus = 2)

