#' Print Object of Class \code{hierBase}
#'
#' Print significant clusters or groups of variables of an object of class
#' \code{hierBase}.
#'
#' @param x an object of class \code{hierBase}
#' @param n.terms maximum number of column names or variables names to be
#' printed per cluster or group of variables.
#' @param digits number of significant digits to be used.
#' @param right logical value indicating whether the values should or should
#' not be right-aligned.
#' @param ... additional arguments to \code{\link{print.data.frame}}
#'
#' @details The function prints the significant clusters or groups of variables
#' of an object of class \code{hierBase}. By default, it prints at most the first
#' \code{n.terms} column or variable names per significant cluster and the
#' number of omitted column names are printed in square brackets (if any).
#'
#' @return The returned values is a invisible copy of the object \code{x}.
#'
#' @seealso \code{\link{invisible}}.
#'
#' @examples
#' n <- 200
#' p <- 100
#' library(MASS)
#' set.seed(3)
#' x <- mvrnorm(n, mu = rep(0, p), Sigma = diag(p))
#' colnames(x) <- paste0("Var", 1:p)
#' beta <- rep(0, p)
#' beta[c(5, 20, 46)] <- 1
#' y <- x %*% beta + rnorm(n)
#'
#' dendr <- cluster_vars(x = x)
#' sign.clusters <- advance_hierarchy(x = x, y = y, dendr = dendr,
#'                                    test = "F")
#'
#' # The argument n.terms is useful if there is one or multiple
#' # significant groups containing many variables.
#' print(sign.clusters, n.terms = 4)
#'
#' print(sign.clusters, right = TRUE)
#'
#' print(sign.clusters, digits = 6)
#'
#' @name print.hierBase
#' @export

print.hierBase <- function(x, n.terms = 5L, digits = max(3, getOption("digits") - 3),
                        right = FALSE, ...) {

  stopifnot((n.terms > 0) & (n.terms %% 1 == 0))
  stopifnot((digits > 0) & (digits %% 1 == 0))

  x.print <- x

  # Only print n.terms column names per significant cluster.
  len.cluster <- vapply(x.print$significant.cluster, FUN = length,
                        FUN.VALUE = 1)
  ind.long <- len.cluster > n.terms
  len.print <- len.cluster
  len.print[ind.long] <- n.terms
  x.print$significant.cluster <- mapply(FUN = helper_print,
                                        x = x.print$significant.cluster,
                                        len.cluster = len.cluster,
                                        len.print = len.print,
                                        ind.long = ind.long,
                                        MoreArgs = list(n.terms = n.terms),
                                        SIMPLIFY = FALSE)

  # Only print <.Machine$double.eps instead of small p-values because of
  # numerical reasons of Tippett's rule. I.e. identical(1 - 1e-17, 1) on
  # binary64 is TRUE.
  x.print$p.value <- format.pval(x.print$p.value, digits = digits)

  print.data.frame(x.print, ..., right = right)

  invisible(x)
}

# Help function for \code{print.hierBase}
#
# Shortens a vector to \code{len.print} elements and adds "... [number of not
# displayed elements]" if any.
helper_print <- function(x, len.cluster, len.print, ind.long, n.terms) {
  if (ind.long) {
    c(x[seq_len(len.print)], paste0("... [", len.cluster - n.terms, "]"))
  } else {
    x[seq_len(len.print)]
  }
}
