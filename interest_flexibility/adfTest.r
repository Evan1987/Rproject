function (x, lags = 1, type = c("nc", "c", "ct"), title = NULL, 
          description = NULL) 
{
  CALL = match.call()
  test = list()
  DNAME = deparse(substitute(x))
  test$data.name = DNAME
  if (class(x) == "timeSeries") 
    x = series(x)
  x = as.vector(x)
  if (lags < 0) 
    stop("Lags are negative")
  doprint = FALSE
  type = type[1]
  lags = lags + 1
  y = diff(x)
  n = length(y)
  z = embed(y, lags)
  y.diff = z[, 1]
  y.lag.1 = x[lags:n]
  tt = lags:n
  if (lags > 1) {
    y.diff.lag = z[, 2:lags]
    if (type == "nc") {
      res = lm(y.diff ~ y.lag.1 - 1 + y.diff.lag)
    }
    if (type == "c") {
      res = lm(y.diff ~ y.lag.1 + 1 + y.diff.lag)
    }
    if (type == "ct") {
      res = lm(y.diff ~ y.lag.1 + 1 + tt + y.diff.lag)
    }
  }
  else {
    if (type == "nc") {
      res = lm(y.diff ~ y.lag.1 - 1)
    }
    if (type == "c") {
      res = lm(y.diff ~ y.lag.1 + 1)
    }
    if (type == "ct") {
      res = lm(y.diff ~ y.lag.1 + 1 + tt)
    }
  }
  res.sum = summary(res)
  if (doprint) 
    print(res.sum)
  if (type == "nc") 
    coefNum = 1
  else coefNum = 2
  STAT = res.sum$coefficients[coefNum, 1]/res.sum$coefficients[coefNum, 
                                                               2]
  names(STAT) = "Dickey-Fuller"
  test$statistic = STAT
  if (type == "nc") 
    table = cbind(
      c(-2.66, -2.26, -1.95, -1.60, +0.92, +1.33, +1.70, +2.16), 
      c(-2.62, -2.25, -1.95, -1.61, +0.91, +1.31, +1.66, +2.08), 
      c(-2.60, -2.24, -1.95, -1.61, +0.90, +1.29, +1.64, +2.03), 
      c(-2.58, -2.23, -1.95, -1.62, +0.89, +1.29, +1.63, +2.01), 
      c(-2.58, -2.23, -1.95, -1.62, +0.89, +1.28, +1.62, +2.00), 
      c(-2.58, -2.23, -1.95, -1.62, +0.89, +1.28, +1.62, +2.00)
    )
  if (type == "c") 
    table = cbind(c(-3.75, -3.33, -3, -2.63, -0.37, +0, +0.34, 
                    +0.72), c(-3.58, -3.22, -2.93, -2.6, -0.4, -0.03, 
                              +0.29, +0.66), c(-3.51, -3.17, -2.89, -2.58, -0.42, 
                                               -0.05, +0.26, +0.63), c(-3.46, -3.14, -2.88, -2.57, 
                                                                       -0.42, -0.06, +0.24, +0.62), c(-3.44, -3.13, -2.87, 
                                                                                                      -2.57, -0.43, -0.07, +0.24, +0.61), c(-3.43, -3.12, 
                                                                                                                                            -2.86, -2.57, -0.44, -0.07, +0.23, +0.6))
  if (type == "ct") 
    table = cbind(c(-4.38, -3.95, -3.6, -3.24, -1.14, -0.8, 
                    -0.5, -0.15), c(-4.15, -3.8, -3.5, -3.18, -1.19, 
                                    -0.87, -0.58, -0.24), c(-4.04, -3.73, -3.45, -3.15, 
                                                            -1.22, -0.9, -0.62, -0.28), c(-3.99, -3.69, -3.43, 
                                                                                          -3.13, -1.23, -0.92, -0.64, -0.31), c(-3.98, -3.68, 
                                                                                                                                -3.42, -3.13, -1.24, -0.93, -0.65, -0.32), c(-3.96, 
                                                                                                                                                                             -3.66, -3.41, -3.12, -1.25, -0.94, -0.66, -0.33))
  table = t(table)
  tablen = dim(table)[2]
  tableT = c(25, 50, 100, 250, 500, 1e+05)
  tablep = c(0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99)
  tableipl = numeric(tablen)
  for (i in (1:tablen)) tableipl[i] = approx(tableT, table[, 
                                                           i], n, rule = 2)$y
  PVAL = approx(tableipl, tablep, STAT, rule = 2)$y
  if (is.na(approx(tableipl, tablep, STAT, rule = 1)$y)) {
    if (PVAL == min(tablep)) {
      warning("p-value smaller than printed p-value")
    }
    else {
      warning("p-value greater than printed p-value")
    }
  }
  names(PVAL) = ""
  test$p.value = PVAL
  PARAMETER = lags - 1
  names(PARAMETER) = "Lag Order"
  test$parameter = PARAMETER
  if (is.null(title)) 
    title = "Augmented Dickey-Fuller Test"
  if (is.null(description)) 
    description = date()
  test$lm = res
  new("fHTEST", call = CALL, data = list(x = x), test = test, 
      title = as.character(title), description = description())
}