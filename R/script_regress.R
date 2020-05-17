mlereg <- function(y, x, ...){

  n <- length(y)
  p <- ncol(x)

  mu_beta <- 0
  sigma_beta <- 10
  a_sigma <- 0.1
  b_sigma <- 0.1

  stan_data <- list(y=y, x=x, n=n, p=p, mu_beta=mu_beta, sigma_beta=sigma_beta,
                    a_sigma=a_sigma, b_sigma=b_sigma, approach=0)

  fit <- rstan::optimizing(stanmodels$regress, data=stan_data, hessian=TRUE)

  return(fit)
}


bayesreg <- function(y, x, ...){

  n <- length(y)
  p <- ncol(x)

  mu_beta <- 0
  sigma_beta <- 10
  a_sigma <- 0.1
  b_sigma <- 0.1

  stan_data <- list(y=y, x=x, n=n, p=p, mu_beta=mu_beta, sigma_beta=sigma_beta,
                    a_sigma=a_sigma, b_sigma=b_sigma, approach=1)

  fit <- rstan::sampling(stanmodels$regress, data=stan_data)

  return(fit)

}

lm_regbayes <- function(formula, data, approach=c("mle","bayes"), mu_beta=0, sigma_beta=10, a_sigma=0.1, b_sigma=0.1, ...){

  mf <- stats::model.frame(formula=formula, data=data)
  x <- model.matrix(attr(mf, "terms"), data=mf)
  y <- model.response(mf)
  n <- length(y)
  p <- ncol(x)


  if(approach=="mle"){
      fit <- mlereg(y=y, x=x, ...)

      output <- list(fit=fit)

      output$n <- n
      output$p <- p

      output$call <- match.call()
      output$formula <- formula
      output$terms <- stats::terms.formula(formula)
      output$mf <- mf
      output$labels <- labels
      output$approach <- approach
      output$fitted.values <- x%*%output$fit$par[2:(p+1)]
      output$residuals <- y - x%*%output$fit$par[2:(p+1)]


  }else{
      fit <- bayesreg(y=y, x=x, n=n, p=p, mu_beta=mu_beta, sigma_beta=sigma_beta,
                      a_sigma=a_sigma, b_sigma=b_sigma, ...)

      output <- list(fit=fit)

      output$n <- n
      output$p <- p

      output$call <- match.call()
      output$formula <- formula
      output$terms <- stats::terms.formula(formula)
      output$mf <- mf
      output$labels <- labels
      output$approach <- approach
  }

  class(output) <- "lm_regbayes"
  return(output)
}



