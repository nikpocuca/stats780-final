# Stats 790 Final Project

## Q1
Diabetes is a chronic ailment which induces a kidney to have little to no insulin production. In five years, the estimated Canada will have an increase of 5 million new diabetics or 12.5% of Canadians. This project contains a set of analyses for the purpose of classifying diabetes based on various predictors. The Pima dataset (Diabetes of Pima Indian Women) taken from the MASS package (Venables and Ripley, 2002) in the programming langauge R (R Core Team, 2020) contains 532 complete records after dropping the (mainly missing) data on serum insulin. The dataset contains 7 covariates and 1 response variable described in Table 1. A variable of particular interest is the pedigree function (ped) which provides a numeric feature based on the genetic history of relatives and their relationships to the subject (Smith et al., 1988). Some interesting questions that may arise is not only the effect of predictors on the response but non-linearity and particularly the skewness of some predictors.


## Q2

### a)
The multivariate Gaussian Mixture Model is one of the most popular approaches to mixture modelling due to its  
mathematical tractability and computational feasibility. The dirchletprocess package (Ross et. al. 2020) by R incorporates the classic mixture model approach with the robustness of a Dirchlet process. The Infinite Multivariate Gaussian Mixture Model is an extension of the original finite mixture model in the following definition:

Let $X$ be a random variable whose density is formulated as
 $ f(x|ϑ) = \sum_{g=1}^∞  $
