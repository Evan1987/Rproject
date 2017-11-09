garchFit<-
function (formula = ~garch(1, 1), data = dem2gbp, init.rec = c("mci", 
                                                               "uev"), delta = 2, skew = 1, shape = 4, cond.dist = c("norm", 
                                                                                                                     "snorm", "ged", "sged", "std", "sstd", "snig", "QMLE"), include.mean = TRUE, 
          include.delta = NULL, include.skew = NULL, include.shape = NULL, 
          leverage = NULL, trace = TRUE, algorithm = c("nlminb", "lbfgsb", 
                                                       "nlminb+nm", "lbfgsb+nm"), hessian = c("ropt", "rcd"), 
          control = list(), title = NULL, description = NULL, ...) 
{
  source('~/rstudio/!custom/(fun)user.garchInternalFit.r', echo=TRUE)
  DEBUG = FALSE
  init.rec = match.arg(init.rec)
  cond.dist = match.arg(cond.dist)
  hessian = match.arg(hessian)
  algorithm = match.arg(algorithm)
  CALL = match.call()
  Name = capture.output(substitute(data))
  if (is.character(data)) {
    eval(parse(text = paste("data(", data, ")")))
    data = eval(parse(text = data))
  }
  data <- as.data.frame(data)
  if (isUnivariate(data)) {
    colnames(data) <- "data"
  }
  else {
    uniqueNames = unique(sort(colnames(data)))
    if (is.null(colnames(data))) {
      stop("Column names of data are missing.")
    }
    if (length(colnames(data)) != length(uniqueNames)) {
      stop("Column names of data are not unique.")
    }
  }
  if (length(formula) == 3 && isUnivariate(data)) 
    formula[2] <- NULL
  if (length(formula) == 2) {
    if (isUnivariate(data)) {
      formula = as.formula(paste("data", paste(formula, 
                                               collapse = " ")))
    }
    else {
      stop("Multivariate data inputs require lhs for the formula.")
    }
  }
  robust.cvar <- (cond.dist == "QMLE")
  args = .garchArgsParser(formula = formula, data = data, trace = FALSE)
  if (DEBUG) 
    print(list(formula.mean = args$formula.mean, formula.var = args$formula.var, 
               series = args$series, init.rec = init.rec, delta = delta, 
               skew = skew, shape = shape, cond.dist = cond.dist, 
               include.mean = include.mean, include.delta = include.delta, 
               include.skew = include.skew, include.shape = include.shape, 
               leverage = leverage, trace = trace, algorithm = algorithm, 
               hessian = hessian, robust.cvar = robust.cvar, control = control, 
               title = title, description = description))
  ans = .garchFit(formula.mean = args$formula.mean, formula.var = args$formula.var, 
                  series = args$series, init.rec, delta, skew, shape, cond.dist, 
                  include.mean, include.delta, include.skew, include.shape, 
                  leverage, trace, algorithm, hessian, robust.cvar, control, 
                  title, description, ...)
  ans@call = CALL
  attr(formula, "data") <- paste("data = ", Name, sep = "")
  ans@formula = formula
  ans
}