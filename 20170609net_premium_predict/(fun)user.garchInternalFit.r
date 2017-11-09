.garchFit <-
  function(
    formula.mean = ~arma(0, 0),
    formula.var = ~garch(1, 1),
    series,
    init.rec = c("mci", "uev"),
    delta = 2,
    skew = 1,
    shape = 4,
    cond.dist = c("norm", "snorm", "ged", "sged", "std", "sstd", "QMLE"),
    include.mean = TRUE,
    include.delta = NULL,
    include.skew = NULL,
    include.shape = NULL,
    leverage = NULL,
    trace = TRUE,
    algorithm = c("sqp", "nlminb", "lbfgsb", "nlminb+nm", "lbfgsb+nm"),
    hessian = c("ropt", "rcd"),
    robust.cvar,
    control = list(),
    title = NULL,
    description = NULL,
    ...)
  {
    # A function implemented by Diethelm Wuertz
    
    # Description
    #   Fit parameters to a ARMA-GARCH model
    
    # Arguments:
    #   formula.mean - ARMA(m,n) mean specification
    #   formula.var - GARCH/APARCH(p,q) variance specification
    #   series - time series
    #   init.rec - names type of initialization of recurrence
    #       mci = mu-current-iteration, or
    #       uev = unconditional-expected-variances
    #   delta - numeric value of the exponent delta
    #   skew - optional skewness parameter
    #   shape - optional shape parameter
    #   cond.dist - name of the conditional distribution
    #   include.mean - should the mean value be estimated ?
    #   include.delta - should the exponent be estimated ?
    #   leverage - should the leverage factors be estimated ?
    #   trace - should the optimization be traced ?
    #   algorithm -
    #   control - list of additional control parameters for solver
    #   title - an optional title string
    #   description - an optional project description string
    
    # Note:
    #   This is the old version of garchFit, we keep it for backward
    #   compatibility.
    
    # FUNCTION:
    
    # Debug Mode:
    DEBUG <- FALSE
    
    # Allow only full formula specification:
    if(DEBUG) print("Formula Specification ...")
    fcheck = rev(all.names(formula.mean))[1]
    if (fcheck == "ma") {
      stop("Use full formula: arma(0,q) for ma(q)")
    } else if (fcheck == "ar") {
      stop("Use full formula expression: arma(p,0) for ar(p)")
    }
    
    # Check for Recursion Initialization:
    if(DEBUG) print("Recursion Initialization ...")
    if(init.rec[1] != "mci" & algorithm[1] != "sqp") {
      stop("Algorithm only supported for mci Recursion")
    }
    
    # Get Start Time:
    .StartFit <- Sys.time()
    
    # Generate Control List - Define Default Settings:
    if(DEBUG) print("Generate Control List ...")
    con <- .garchOptimizerControl(algorithm, cond.dist)
    con[(namc <- names(control))] <- control
    
    # Initialize Time Series Information - Save Globally:
    # keep copy of input data
    if(DEBUG) print("Initialize Time Series ...")
    data <- series
    # scale time series
    scale <- if (con$xscale) sd(series) else 1
    series <- series/scale
    .series <- .garchInitSeries(
      formula.mean = formula.mean,
      formula.var = formula.var,
      cond.dist = cond.dist[1],
      series = series,
      scale = scale,
      init.rec = init.rec[1],
      h.start = NULL,
      llh.start = NULL,
      trace = trace)
    .setfGarchEnv(.series = .series)
    
    # Initialize Model Parameters - Save Globally:
    if(DEBUG) print("Initialize Model Parameters ...")
    .params <- .garchInitParameters(
      formula.mean = formula.mean,
      formula.var = formula.var,
      delta = delta,
      skew = skew,
      shape = shape,
      cond.dist = cond.dist[1],
      include.mean = include.mean,
      include.delta = include.delta,
      include.skew = include.skew,
      include.shape = include.shape,
      leverage = leverage,
      algorithm = algorithm[1],
      control = con,
      trace = trace)
    .setfGarchEnv(.params = .params)
    
    # Select Conditional Distribution Function:
    if(DEBUG) print("Select Conditional Distribution ...")
    .setfGarchEnv(.garchDist = .garchSetCondDist(cond.dist[1]))
    
    # Estimate Model Parameters - Minimize llh, start from big value:
    if(DEBUG) print("Estimate Model Parameters ...")
    .setfGarchEnv(.llh = 1.0e99)
    .llh <- .getfGarchEnv(".llh")
    fit = .garchOptimizeLLH(hessian, robust.cvar, trace)
    
    # Add to Fit:
    if (DEBUG) print("Add to fit ...")
    .series <- .getfGarchEnv(".series")
    .params <- .getfGarchEnv(".params")
    names(.series$h) <- NULL
    fit$series = .series
    fit$params = .params
    
    # Retrieve Residuals and Fitted Values:
    if (DEBUG) print("Retrieve Residuals and Fitted Values ...")
    residuals = .series$z
    fitted.values = .series$x - residuals
    h.t = .series$h
    if (.params$includes["delta"])
      deltainv = 1/fit$par["delta"]
    else
      deltainv = 1/fit$params$delta
    sigma.t = (.series$h)^deltainv
    
    # Standard Errors and t-Values:
    if (DEBUG) print("Standard Errors and t-Values ...")
    fit$cvar <-
      if (robust.cvar)
        (solve(fit$hessian,tol = 1e-50) %*% (t(fit$gradient) %*% fit$gradient) %*%
           solve(fit$hessian,tol = 1e-50))
    else
      - solve(fit$hessian,tol = 1e-50)
    fit$se.coef = sqrt(diag(fit$cvar))
    fit$tval = fit$coef/fit$se.coef
    fit$matcoef = cbind(fit$coef, fit$se.coef,
                        fit$tval, 2*(1-pnorm(abs(fit$tval))))
    dimnames(fit$matcoef) = list(names(fit$tval), c(" Estimate",
                                                    " Std. Error", " t value", "Pr(>|t|)"))
    
    # Add Title and Description:
    if (DEBUG) print("Add Title and Description ...")
    if(is.null(title)) title = "GARCH Modelling"
    if(is.null(description)) description = description()
    
    # Total Execution Time:
    Time =  Sys.time() - .StartFit
    if(trace) {
      cat("\nTime to Estimate Parameters:\n ")
      print(Time)
    }
    
    # Return Value:
    new("fGARCH",
        call = as.call(match.call()),
        formula = as.formula(paste("~", formula.mean, "+", formula.var)),
        method = "Max Log-Likelihood Estimation",
        data = data,
        fit = fit,
        residuals = residuals,
        fitted = fitted.values,
        h.t = h.t,
        sigma.t = as.vector(sigma.t),
        title = as.character(title),
        description = as.character(description)
    )
  }