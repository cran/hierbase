# Compute the adjusted p-value of a given cluster
#
# Compute the adjusted p-value of a given cluster (specified by the argument
# \code{colnames.cluster}). This means that there is one adjusted p-value
# based on all data sets if multiple data sets are supplied. The p-values
# per data set are combined using Stouffer's method.
#
# @return adjusted p-value.
comp_cluster_pval <- function(x, y, clvar, test.func, arg.all, arg.all.fix,
                              colnames.cluster, minimal.pval, agg.method,
                              hier.adj, mt.adj, MD.factor = NULL, mod.large,
                              mod.small = NULL, stouffer.weights) {

  # compute a p-value for each of the phenotypes or phenotypes & corresponding
  # (distinct) genotypes and (distinct) control covariates
  pvals.data <- mapply(test.func, x = x, y = y, clvar = clvar,
                       arg.all = arg.all, mod.large = mod.large,
                       mod.small = mod.small,
                       MoreArgs = list(colnames.cluster = colnames.cluster,
                                       arg.all.fix = arg.all.fix))

  pvals.only <- do.call(c, pvals.data["pval", ])
  
  # Return an error if pvals.only is contains an invalid p-value or invalid 
  # p-values (for multiple data sets)  
  if (any(is.na(pvals.only)) | any(is.null(pvals.only)) | length(pvals.only) == 0) {
    stop("The group test function returned an invalid p-value (NA, NULL, or an object of length 0 like integer(0)). The problem occured for the cluster / group of size ", length(colnames.cluster), " containing the following variables: ", paste(colnames.cluster, collapse = ", "))
  }

  # hierarchical mc adjustment
  pvals.adj <- if (mt.adj == "none") {
    pvals.only
  } else if (mt.adj == "SBH") {
    # alpha * MD.factor  OR  pval / MD.factor
    pmin(pvals.only / MD.factor, 1)
  } else if (mt.adj == "dpBF") {
    # number of variables in each data set
    p.x <- vapply(X = x, FUN = ncol, FUN.VALUE = 1L)
    # alpha * (size of cluster) / p  OR  pval * p / (size of cluster)
    pmin(pvals.only * p.x / length(colnames.cluster), 1)
  }

  # aggregate the p-values over the different data sets
  pval <-
    if (length(pvals.adj) == 1) {
      # No aggregation method is applied because the user only specified one
      # data set.
      pvals.adj

    } else if (agg.method == "Tippett") {
      # Tippett's method: combine the p-values
      max(1 - (1 - min(pvals.adj))^(length(x)), .Machine$double.neg.eps)
      # We use max(., .Machine$double.neg.eps) because all smaller values
      # are set to zero, i.e. identical(1, 1 - 1e-17) => TRUE because of
      # rounding in floating point arithmetic.

      # # Alternative:
      # # Minimum p-value, Bonferroni corrected, i.e. m * min(p_i)
      # min(c(1, length(x) * min(pvals.adj)))

    } else if (agg.method == "Stouffer") {
      # Stouffer's method: combine the p-values
      stats::pnorm(sum(stouffer.weights * stats::qnorm(pvals.adj)))

    }
  # else if (agg.method == "max") {
  #   # Largest p-value
  #   max(pvals.adj)^(length(x))
  # }


  # hierarchical adjustment of the p-value (below Equation 4 on page 333 of
  # Mandozzi and Buehlmann (2016))
  if (hier.adj) {
    pval <- max(pval, minimal.pval)
  }

  return(list("cluster" = list(colnames.cluster = colnames.cluster,
                               pval = pval),
              "mod.small" = pvals.data["mod.small", ],
              "MD.factor" = MD.factor))
} # {comp_cluster_pval}

