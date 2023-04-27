# bmi510.R

#' Sample from Vector or Dataframe
#'
#' This function is a wrapper around \code{\link{sample}} that tests whether the input
#' is an atomic vector or a dataframe-like object and returns either n samples or n rows
#' as appropriate.
#'
#' @param x an atomic vector or a dataframe
#' @param n number of samples or rows to be returned (default is 1)
#' @param replace logical, indicating whether or not sampling should be done with replacement (default is TRUE)
#'
#' @return a vector or a dataframe
#'
#' @examples
#' rando(1:10, 3)
#' rando(mtcars, 5)
#'
#' @export
rando <- function(x, n = 1, replace = T) {
  if (is.atomic(x)) {
    return(sample(x, n, replace = replace))
  } else if (is.data.frame(x)) {
    return(x[sample(nrow(x), n, replace = replace), ])
  } else {
    stop("Argument 'x' must be either an atomic vector or a dataframe.")
  }
}
#' Test if elements are minimum of a vector
#'
#' Returns a logical vector indicating whether the elements of the input vector are
#' equal to its minimum value.
#'
#' @param x an atomic vector of numeric, integer, or logical values
#' @param na.rm a logical value indicating whether missing values should be removed (default is TRUE)
#'
#' @return a logical vector of the same length as x
#'
#' @examples
#' is_min(c(1,2,3,1))
#' is_min(c(1,2,3,1), na.rm = FALSE)
#'
#' @export
is_min <- function(x, na.rm = T) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  x == min(x)
}

#' Test if elements are maximum of a vector
#'
#' Returns a logical vector indicating whether the elements of the input vector are
#' equal to its maximum value.
#'
#' @param x an atomic vector of numeric, integer, or logical values
#' @param na.rm a logical value indicating whether missing values should be removed (default is TRUE)
#'
#' @return a logical vector of the same length as x
#'
#' @examples
#' is_max(c(1,2,3,1))
#' is_max(c(1,2,3,1), na.rm = FALSE)
#'
#' @export
is_max <- function(x, na.rm = T) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  x == max(x)
  return(x)
}


#' Replicate matrix rows or columns
#'
#' Creates a matrix by replicating the rows or columns of a given matrix or data.frame
#' M (N) times.
#' rep_mat - replicate matrix or data frame by row and/or column
#'
#' @param x matrix or data frame to be replicated
#' @param M number of times to replicate the matrix by row
#' @param N number of times to replicate the matrix by column
#'
#' @return replicated matrix or data frame
#'
#' @examples
#' x <- matrix(1:4, ncol = 2)
#' rep_mat(x, 2, 1) # replicates the matrix x twice by row
#' rep_mat(x, 1, 2) # replicates the matrix x twice by column
#' rep_mat(x, 2, 2) # replicates the matrix x twice by row and twice by column
#'
#' @export
rep_mat = function(x, M=2, N=1) {
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop("x must be a matrix or dataframe.")
  }
  if (!is.numeric(M) || !is.numeric(N)) {
    stop("M and N must be numeric.")
  }
  if (M <= 0 || N <= 0) {
    stop("M and N must be positive.")
  }

  if (M >= 2 || N >=2) {
    # Repeat All Rows
    rows= c(1:nrow(x))
    cols = c(1:ncol(x))
    row_times = M
    cols_times = N
    x=x[rep(rows,row_times),rep(cols,cols_times)]
  }
  return(x)
}


#' Returns a character vector containing the classes of each variable in a tibble x.
#'
#' @param x A tibble or data frame
#' @return A character vector containing the classes of each variable in \code{x}
#' @examples
#' my_data <- tibble::tribble(
#'    ~name, ~age, ~height_cm,
#'    "Alice", 25, 165,
#'    "Bob", 30, 180,
#'    "Charlie", 35, 175
#' )
#' classes(my_data)
#' # Output: [1] "character" "numeric"   "numeric"
#' @export
classes <- function(x) {
  results<-sapply(x, class)
  return(results)
}

#' Scale numeric variables in a tibble
#'
#' @param x A tibble or data frame
#' @param center Logical. Should the variables be centered (subtract the mean)?
#' @param scale Logical. Should the variables be scaled (divide by the standard deviation)?
#' @return A tibble with scaled numeric variables
#' @examples
#' my_data <- tibble::tribble(
#'    ~name, ~age, ~height_cm,
#'    "Alice", 25, 165,
#'    "Bob", 30, 180,
#'    "Charlie", 35, 175
#' )
#' df_scale(my_data)
#' @export

df_scale = function(x, center = T, scale = T) {
  if (!is.data.frame(x)) {
    stop("x must be a data frame")
  }
  if (!is.logical(center) || !is.logical(scale)) {
    stop("center and scale must be logical")
  }

  # Select numeric columns
  numeric_cols = sapply(x, is.numeric)

  # Center and scale numeric columns
  if (sum(numeric_cols) > 0) {
    x[, numeric_cols] = scale(x[, numeric_cols], center = center, scale = scale)
  }

  return(x)
}

#' Calculate the log-likelihood of a normal distribution
#'
#' Calculates the log-likelihood of an observation or vector of observations given the
#' parameters of a normal distribution: the mean and standard deviation.
#'
#' @param x a numeric vector of observations
#' @param mean the mean of the normal distribution
#' @param sd the standard deviation of the normal distribution
#'
#' @return a numeric value representing the log-likelihood of the observations given the
#'         parameters of the normal distribution
#'
#' @examples
#' log_likelihood_norm(3, 0, 1)
#' log_likelihood_norm(c(1, 2, 3), 0, 1)
#'
#' @importFrom stats dnorm
#' @export
log_likelihood_norm <- function(x, mean, sd) {
  ll <- sum(log(dnorm(x, mean, sd)))
  return(ll)
}

#' Calculate the log-likelihood of a uniform distribution
#'
#' Calculates the log-likelihood of an observation or vector of observations given the
#' parameters of a uniform distribution: the minimum and maximum values.
#'
#' @param x a numeric vector of observations
#' @param min the minimum value of the uniform distribution
#' @param max the maximum value of the uniform distribution
#'
#' @return a numeric value representing the log-likelihood of the observations given the
#'         parameters of the uniform distribution
#'
#' @examples
#' log_likelihood_unif(0.5, 0, 1)
#' log_likelihood_unif(c(0.1, 0.2, 0.3), 0, 1)
#'
#' @importFrom stats dunif
#' @export
log_likelihood_unif <- function(x, min, max) {
  ll <- sum(log(dunif(x, min, max)))
  return(ll)
}


#' Calculate the log-likelihood of a chi-squared distribution
#'
#' Calculates the log-likelihood of an observation or vector of observations given the
#' degrees of freedom of a chi-squared distribution.
#'
#' @param x a numeric vector of observations
#' @param df the degrees of freedom of the chi-squared distribution
#'
#' @return a numeric value representing the log-likelihood of the observations given the
#'         degrees of freedom of the chi-squared distribution
#'
#' @examples
#' log_likelihood_chisq(3.5, 2)
#' log_likelihood_chisq(c(2, 4, 6), 3)
#'
#' @importFrom stats dchisq
#' @export
log_likelihood_chisq <- function(x, df) {
  ll <- sum(log(dchisq(x, df)))
  return(ll)
}


#' Calculate the log-likelihood of an F distribution with given parameters
#'
#' This function calculates the log-likelihood of an F distribution with given
#' degrees of freedom parameters df1 and df2, evaluated at a set of observations x.
#'
#' @param x A numeric vector of observations
#' @param df1 The degrees of freedom parameter for the numerator distribution
#' @param df2 The degrees of freedom parameter for the denominator distribution
#'
#' @return The log-likelihood of the F distribution with the given parameters and
#'         the observed data
#'
#' @examples
#' x <- rf(100, 5, 10)
#' log_likelihood_f(x, 5, 10)
#' @export
#'
log_likelihood_f <- function(x, df1, df2) {
  const <- lgamma((df1 + df2) / 2) - lgamma(df1 / 2) - lgamma(df2 / 2)
  ll <- const + ((df1 / 2) * log(df1 * x / (df1 * x + df2))) + ((df2 / 2) * log(df2 / (df1 * x + df2)))
  return(sum(ll, na.rm = TRUE))
}


#' Returns the log-likelihood of a sample x under the t-density
#'
#' @param x a numeric vector of sample data
#' @param df degrees of freedom
#'
#' @return the log-likelihood of the sample x under the t-density
#'
#' @details Computes the log-likelihood of the sample x under the t-density using the standard parameterization described in ?dt.
#'
#' @examples
#' # Generate a random sample from t-distribution
#' set.seed(123)
#' x <- rt(100, df = 5)
#'
#' # Compute the log-likelihood of the sample under the t-density
#' log_likelihood_t(x, df = 5)
#'
#' @export
log_likelihood_t <- function(x, df) {
  const <- lgamma((df + 1) / 2) - lgamma(df / 2) - 0.5 * log(df * pi)
  ll <- const - ((df + 1) / 2) * log(1 + (x^2 / df))
  return(sum(ll, na.rm = TRUE))
}


#' Calculate the sensitivity of a binary classifier.
#'
#' Sensitivity measures the proportion of true positive cases that were correctly identified
#' as positive by the binary classifier. It is calculated as the ratio of true positives to
#' all actual positives.
#'
#' @param pred A binary vector of predicted labels.
#' @param truth A binary vector of actual labels.
#' @return The sensitivity of the binary classifier.
#' @examples
#' pred <- c(1,0,1,1,0,0,1)
#' truth <- c(1,0,1,0,0,0,0)
#' sensitivity(pred, truth)
#' @export
sensitivity <- function(pred, truth) {
  true_positives <- pred & truth
  return(sum(true_positives) / sum(truth))

}


#' Calculate specificity based on predicted and true labels
#'
#' This function takes two vectors of binary values, the predicted and true labels, and returns the specificity, i.e., the proportion of negative cases correctly identified as negative.
#'
#' @param pred A vector of predicted binary values.
#' @param truth A vector of true binary values.
#' @return A numeric value representing the specificity.
#' @examples
#' specificity(c(1,1,0,0), c(1,0,1,0)) # Returns 0.5
#'
#' @export
specificity = function(pred,truth){
  return(sensitivity(!pred,!truth))
}


#' Calculate Precision
#'
#' Calculates the precision based on comparing predicted and truth labels.
#' @param pred A vector of predicted labels.
#' @param truth A vector of truth labels.
#' @return The precision value.
#' @examples
#' precision(c(1,1,0,1),c(1,0,0,1))
#' @export
precision = function(pred,truth){
  P = sum(truth)
  TP = sum(pred&truth)
  FP = sum(pred&!truth)
  return( TP / (TP + FP))
}


#' Calculate recall (same as sensitivity) based on comparing predicted and training labels
#'
#' @param pred A vector of predicted labels (0/1)
#' @param truth A vector of true labels (0/1)
#'
#' @return The recall (sensitivity) of the prediction
#'
#' @examples
#' truth <- c(1, 1, 0, 0, 1, 0, 1, 1, 1, 0)
#' pred <- c(1, 0, 0, 1, 1, 0, 1, 1, 1, 0)
#' recall(pred, truth)
#'
#' @export
recall <- function(pred, truth) {
  true_positives = pred&truth
  return(sum(true_positives)/sum(truth))
}


#' Calculate accuracy based on comparing predicted and true labels
#'
#' This function calculates the accuracy of a binary classification model based on the predicted and true labels.
#'
#' @param pred A vector of predicted labels (0 or 1)
#' @param truth A vector of true labels (0 or 1)
#'
#' @return The accuracy of the model (a value between 0 and 1)
#' @examples
#' pred <- c(1, 0, 1, 0, 1)
#' truth <- c(1, 1, 0, 0, 1)
#' accuracy(pred, truth)
#' # Output: 0.6
#'
#' @export
accuracy <- function(pred, truth) {
  return(sum(pred == truth) / length(truth))
}


#' Calculate the F1 score based on comparing predicted and true labels
#'
#' @param pred a binary vector of predicted labels
#' @param truth a binary vector of true labels
#' @return the F1 score
#' @examples
#' pred <- c(1, 0, 1, 1, 0)
#' truth <- c(0, 0, 1, 1, 1)
#' f1(pred, truth)
#' @export
f1 <- function(pred, truth) {
  p <- precision(pred, truth)
  r <- sensitivity(pred, truth)
  return( 2 * (p * r) / (p + r))
}


#' Calculate minimum n per group for a two-sample t-test
#'
#' @param d Expected Cohen's d
#' @param power Desired statistical power
#' @return Minimum n per group needed for a two-sample t-tesinslibrl
#' @examples
#' minimum_n_per_group(0.5)
#'
#' @export
minimum_n_per_group <- function(d, power = 0.8) {
  es <- d
  sig.level <- 0.05
  power <- power

  return(pwr::pwr.t.test(d = es, sig.level = sig.level, power = power, type = "two.sample")$n)
}


#' Calculate the R-squared statistic between predicted and ground truth continuous variables
#'
#' @param pred predicted values
#' @param truth ground truth values
#' @return the R-squared statistic
#'
#' @examples
#' r2(c(1, 2, 3), c(1, 3, 5))
#'
#' @export
r2 <- function(pred, truth) {
  SSres <- sum((truth - pred)^2)
  SStot <- sum((truth - mean(truth))^2)
  return( 1 - SSres/SStot)
}


#' Calculate the adjusted r-squared statistic between predicted and ground truth continuous variables.
#'
#' @param pred The predicted values.
#' @param truth The ground truth values.
#' @param n_p The number of model parameters, excluding the intercept.
#'
#' @return The adjusted r-squared statistic.
#'
#' @examples
#' # Generate some example data
#' set.seed(123)
#' pred <- rnorm(100, 0, 1)
#' truth <- 0.5*pred + rnorm(100, 0, 0.5)
#'
#' # Calculate the adjusted R-squared
#' adjR2(pred, truth, 1)
#'
#' @seealso \code{\link{r2}}
#'
#' @export
adjR2 <- function(pred, truth, n_p) {
  n <- length(truth)
  SSres <- sum((truth - pred)^2)
  SStot <- sum((truth - mean(truth))^2)
  R2 <- 1 - SSres/SStot
  adjR2 <- 1 - ((1-R2)*(n-1))/(n-n_p-1)
  return(adjR2)
}

