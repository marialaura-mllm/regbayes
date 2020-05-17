# regbayes
An R package for linear regression with bayesian and classical inference.

How to install
-------
```r
library(devtools)
install_github("marialaura-mllm/regbayes")
```

How to use
---------
Use the function **lm_regbayes(formula, data, approach=c("mle","bayes"), mu_beta=0, sigma_beta=10, a_sigma=0.1, b_sigma=0.1, ...)** 
to adjust a liner regrssion model. With the *approach* parameter you can choose the estimation method, **mle** for maximum likelihood 
estimation and **bayes** for estimation via bayesian inference.


Example:

```r
#load package
libray(regbayes) 
#adjusting the model
mod <- lm_regbayes(Sepal.Length~Petal.Length, approach="bayes", data = iris)
#observing the model adjust result
summary(mod)
```
