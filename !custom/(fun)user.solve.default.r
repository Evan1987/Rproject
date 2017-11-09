solve.default <-
  function(a, b, tol = .Machine$double.eps, LINPACK = FALSE, ...)
  {
    if(is.complex(a) || (!missing(b) && is.complex(b))) {
      a <- as.matrix(a)
      if(missing(b)) {
        b <- diag(1.0+0.0i, nrow(a))
        colnames(b) <- rownames(a)
      }
      return(.Internal(La_solve_cmplx(a, b)))
    }
    
    if(inherits(a, "qr")) {
      warning("solve.default called with a \"qr\" object: use 'qr.solve'")
      return(solve.qr(a, b, tol))
    }
    
    a <- as.matrix(a)
    if(missing(b)) {
      b <- diag(1.0, nrow(a))
      colnames(b) <- rownames(a)
    }
    .Internal(La_solve(a, b, tol))
  }