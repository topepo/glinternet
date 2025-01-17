glinternet <-
  function(X,
           Y,
           numLevels,
           lambda = NULL,
           nLambda = 50,
           lambdaMinRatio = 0.01,
           interactionCandidates = NULL,
           interactionPairs = NULL,
           screenLimit = NULL,
           numToFind = NULL,
           family = c("gaussian", "binomial"),
           tol = 1e-5,
           maxIter = 5000,
           verbose = FALSE,
           numCores = 1) {
    # get call and family
  thisCall <- match.call()
  family <- match.arg(family)

  # make sure inputs are valid
  n <- length(Y)
  pCat <- sum(numLevels > 1)
  pCont <- length(numLevels) - pCat
  stopifnot(n == nrow(X), pCat + pCont == ncol(X), family == "gaussian" || family == "binomial")
  if (family == "binomial" && !all(Y %in% 0:1)) {
    stop("Error:family=binomial but Y not in {0,1}")
  }
  for (i in 1:ncol(X)) {
    if (numLevels[i] > 1 && max(X[, i]) >= numLevels[i]) {
      stop(sprintf("Column %d of X is categorical, but not coded as {0, 1, ...}. Refer to glinternet help on what the X argument should be.", i))
    }
  }

  contIndices <- which(numLevels == 1)
  catIndices <- which(numLevels > 1)

  # specific interaction pairs
  if (!is.null(interactionPairs)) {
    # sanity check
    if (!is.matrix(interactionPairs)) {
      stop("interactionPairs must be either NULL or a n x 2 matrix of indices")
    }
    if (!is.null(interactionCandidates)) {
      stop("If interactionPairs is set, interactionCandidates must be NULL.")
    }
    pairs <- list(contcont = NULL, catcat = NULL, catcont = NULL)
    for (i in 1:nrow(interactionPairs)) {
      left <- interactionPairs[i, 1]
      right <- interactionPairs[i, 2]
      if (numLevels[left] == 1 && numLevels[right] == 1) {
        pairs$contcont <- c(pairs$contcont, which(contIndices %in% c(left, right)))
      } else if (numLevels[left] == 1) {
        pairs$catcont <- c(pairs$catcont, which(catIndices == right), which(contIndices == left))
      } else if (numLevels[right] == 1) {
        pairs$catcont <- c(pairs$catcont, which(catIndices == left), which(contIndices == right))
      } else {
        pairs$catcat <- c(pairs$catcat, which(catIndices %in% c(left, right)))
      }
    }
    # convert to matrices
    pairs <- lapply(pairs, function(x) {
      if (!is.null(x)) {
        return(matrix(x, ncol = 2, byrow = TRUE))
      } else {
        return(NULL)
      }
    })
    interactionPairs <- pairs
  }

  # separate into categorical and continuous parts
  if (pCont > 0) {
    continuousCandidates <- NULL
    Z <- as.matrix(apply(as.matrix(X[, contIndices]), 2, standardize))
    if (!is.null(interactionCandidates)) {
      continuousCandidates <- which(contIndices %in% interactionCandidates)
    }
  } else {
    Z <- NULL
    continuousCandidates <- NULL
  }
  if (pCat > 0) {
    categoricalCandidates <- NULL
    levels <- numLevels[catIndices]
    Xcat <- as.matrix(X[, catIndices])
    if (!is.null(interactionCandidates)) {
      categoricalCandidates <- which(catIndices %in% interactionCandidates)
    }
  } else {
    levels <- NULL
    Xcat <- NULL
    categoricalCandidates <- NULL
  }

  # compute variable norms
  res <- Y - mean(Y)
  candidates <- get_candidates(Xcat, Z, res, n, pCat, pCont, levels, interactionPairs, categoricalCandidates, continuousCandidates, screenLimit, numCores = numCores)

  # lambda grid if not user provided
  if (is.null(lambda)) {
    lambda <- get_lambda_grid(candidates, nLambda, lambdaMinRatio)
  } else {
    stopifnot(min(lambda) > 0)
    if (any(diff(lambda) > 0)) {
      stop("Error: input lambda sequence is not monotone decreasing.")
    }
    lambdaMax <- max(get_lambda_grid(candidates, nLambda, lambdaMinRatio))
    nLambda <- length(lambda)
    if (nLambda == 1) {
      lambda <- sort(c(lambda, lambdaMax), decreasing = TRUE)
      nLambda <- 2
    }
  }

  # initialize storage for results
  fitted <- matrix(mean(Y), n, nLambda)
  activeSet <- vector("list", nLambda)
  betahat <- vector("list", nLambda)
  betahat[[1]] <- ifelse(family == "gaussian", mean(Y), -log(1 / mean(Y) - 1))
  objValue <- rep(0, nLambda)
  objValue[1] <- ifelse(family == "gaussian", sum(res^2) / (2 * n), -mean(Y) * betahat[[1]] + log(1 / (1 - mean(Y))))

  # ever-active set + sequential strong rules + group lasso
  for (i in 2:nLambda) {
    if (verbose) {
      cat("lambda ", i, ": ", lambda[i], "\n")
    }
    activeSet[[i]] <- strong_rules(candidates, lambda[i], lambda[i - 1])
    betahat[[i]] <- initialize_betahat(activeSet[[i]], activeSet[[i - 1]], betahat[[i - 1]], levels)
    while (TRUE) {
      # group lasso on strong set
      solution <- group_lasso(Xcat, Z, Y, activeSet[[i]], betahat[[i]], levels, lambda[i], family, tol, maxIter, verbose)
      activeSet[[i]] <- solution$activeSet
      betahat[[i]] <- solution$betahat
      res <- solution$res
      objValue[i] <- solution$objValue
      # check kkt conditions on the rest
      check <- check_kkt(Xcat, Z, res, n, pCat, pCont, levels, candidates, activeSet[[i]], lambda[i], numCores)
      candidates$norms <- check$norms
      if (check$flag) {
        break
      }
      betahat[[i]] <- initialize_betahat(check$activeSet, activeSet[[i]], betahat[[i]], levels)
      activeSet[[i]] <- check$activeSet
    }
    # update the candidate set if necessary
    if (!is.null(screenLimit) && (screenLimit < pCat + pCont) && i < nLambda) {
      candidates <- get_candidates(Xcat, Z, res, n, pCat, pCont, levels, interactionPairs, categoricalCandidates, continuousCandidates, screenLimit, activeSet[[i]], candidates$norms, numCores)
    }
    # get fitted values
    fitted[, i] <- Y - res
    # compute total number of interactions found
    if (!is.null(numToFind)) {
      numFound <- sum(sapply(activeSet[[i]][3:5], function(x) ifelse(is.null(x), 0, nrow(x))))
      if (numFound >= numToFind) {
        break
      }
    }
  }

  # rescale betahat
  Z <- as.matrix(X[, numLevels == 1])
  betahatRescaled <- lapply(1:i, function(j) rescale_betahat(activeSet[[j]], betahat[[j]], Xcat, Z, levels, n))

  output <-
    list(
      call = thisCall,
      fitted = fitted[, 1:i],
      lambda = lambda[1:i],
      objValue = objValue,
      activeSet = activeSet[1:i],
      betahat = betahatRescaled[1:i],
      numLevels = numLevels,
      family = family
    )
  class(output) <- "glinternet"

  return(output)
}
