# xtivdml

**Double Machine Learning for Static Panel Models with Fixed Effects and Endogenous Treatment**

<!-- badges: start -->
[![License: GPL v2](https://img.shields.io/badge/License-GPL_v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)
[![R >= 3.5.0](https://img.shields.io/badge/R-%3E%3D%203.5.0-blue.svg)](https://www.r-project.org/)
<!-- badges: end -->

## Overview

`xtivdml` is an R package that estimates the structural parameter in **partially linear panel data models** with **fixed effects** and an **endogenous treatment** using the Double Machine Learning (DML) framework of Chernozhukov et al. (2018), extended to the instrumental-variables panel setting.

The nuisance functions are estimated with any learner from the [`mlr3`](https://mlr3.mlr-org.com/) ecosystem (random forests, gradient boosting, lasso, neural nets, etc.), and the structural parameter is recovered through Neyman-orthogonal scores and cross-fitting, yielding √N-consistent and asymptotically Gaussian estimates that are robust to regularization bias in the first stage.

The package implements the methodology developed in:

> Baiardi, A., Clark, P. S., Naghi, A., and Polselli, A. (2026). *Double Machine Learning for Static Panel Data with Instrumental Variables: New Method and Applications.* [arXiv:2603.20464](https://arxiv.org/abs/2603.20464)

It builds on the CRAN packages [`DoubleML`](https://docs.doubleml.org/r/stable/) (Bach et al., 2024) and `xtdml` (Polselli, 2025).

## The Model
The `xtivdml` estimates the structural (causal) parameter from panel data models with endogenous treatment: 
```math
  Y_{it} =  \big\{D_{it}- r_0( X_{it})\big\} \theta_0 + l_0(X_{it}) + \alpha_i + U_{it}
```
```math  
  D_{it} =  \mathbf{V}_{it}'\mathbf{\pi}_0 + r_0(X_{it}) + \zeta_i + R_{it},
```
```math  
  Z_{it} =  \mathbf{m}_0(X_{it}) + \gamma_i  + \mathbf{V}_{it},
```

where 
  * $Y_{it}$ is the output, $D_{it}$ an endogenous treatment,  $Z_{it}$ a set of valid instruments, and $X_{it}$ the covariates
  * $(l_0, r_0, \mathbf{m}_0)$ are (possibly nonlinear) nuisance functions to *learn* 
  * $(\theta_0, \pi_0)$ are the parameters to *estimate* 
  * ($\alpha_i, \zeta_i, \gamma_i$) are the unobserved individual heterogeneity correlated with the included covariates;
  * ($U_{it}, R_{it}, \mathbf{V}_{it}$) are disturbances.

The package:

1. Estimates the high-dimensional nuisance functions `l(·)= E[Y | X]`, `m(·) = E[D | X]`, and `r(·) = E[Z | X]` via *block-k-fold* cross-fitted machine learning.
2. Solves a Neyman-orthogonal moment condition to recover the structural parameter `θ` with valid standard errors.

> [!CAUTION]
> **Manual data preparation required.** This version of the package does **not** transform the data automatically — you must prepare it before applying the panel IV DML method, according to the approach you intend to use:
>
> - **First-difference (exact) approach.** Create first-order lags of $X_{it}$, and apply the first-difference transformation to $(Y_{it}, D_{it}, Z_{it})$.
> - **Within-group (approximation) approach.** Time-demean $(Y_{it}, D_{it}, Z_{it}, X_{it})$.
> - **Correlated random effects approach.** Construct the individual means of $(D_{it}, Z_{it}, X_{it})$.

> [!NOTE]
> **Two additional requirements on the input data:**
>
> - **No missing values.** `mlr3` learners cannot handle `NA`s. Drop incomplete rows before passing the data to the estimator.
> - **Sort the data by $(i, t)$** (panel ID first, then time) before estimation.

## Installation

The package is not yet on CRAN. Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("POLSEAN/xtivdml")
```

If `remotes` is not available, `devtools::install_github("POLSEAN/xtivdml")` works equivalently.

## Quick start

```r
library(xtivdml)
library(mlr3)
library(mlr3learners)

# 1. Simulate a panel dataset with an endogenous treatment and an instrument
set.seed(1)
df <- make_plpr_data(n_obs = 1000, t_per = 5, dim_x = 30,
                     theta = 0.5, pi = 0.8,
                     sigma_ur = 0.6, rho=0.8)

# 2. Build the xtivdml data backend
x_cols <- paste0("X", 1:30)
obj_xtivdml_data = xtivdml_data_from_data_frame(df,
                 x_cols = x_cols,
                 y_col = "y",
                 d_cols = "d",
                 z_cols = "z",
                 cluster_cols = "id")

# Print data environment
obj_xtivdml_data$print()

# 3. Choose ML learners for the nuisance functions
learner <- lrn("regr.rpart")
ml_l <- learner$clone()
ml_m <- learner$clone()
ml_r <- learner$clone()

# 4. Set up DML estimation environment
obj_xtivdml = xtivdml_plr$new(obj_xtivdml_data,
                          ml_l = ml_l, ml_m = ml_m, ml_r = ml_r,
                          score = "orth-PO", n_folds = 3)
# 4.1 Set up a list of parameter grids
param_grid = list("ml_l" = ps(cp = p_dbl(lower = 0.01, upper = 0.02),
                           maxdepth = p_int(lower = 2, upper = 10)),
                 "ml_m" = ps(cp = p_dbl(lower = 0.01, upper = 0.02),
                           maxdepth = p_int(lower = 2, upper = 10)),
                 "ml_r" = ps(cp = p_dbl(lower = 0.01, upper = 0.02),
                           maxdepth = p_int(lower = 2, upper = 10)))
# 4.2 Tune Hyperparameters
tune_settings = list(n_folds_tune = 3,
                  rsmp_tune = mlr3::rsmp("cv", folds = 3),
                  terminator = mlr3tuning::trm("evals", n_evals = 5),
                  tuner = tnr("grid_search", resolution = 10))
obj_xtivdml$tune(param_set = param_grid, tune_settings = tune_settings)

# 4.3 Fit the DML-IV panel model
obj_xtivdml$fit()

# 5. Inspect the structural parameter
summary(obj_xtivdml)
obj_xtivdml$print()

# 6. Extract Estimation objects

# Second-stage estimates
obj_xtivdml$coef_theta
obj_xtivdml$se_theta
obj_xtivdml$pval_theta

# First-stage estimates
obj_xtivdml$coef_pi
obj_xtivdml$se_pi
obj_xtivdml$pval_pi

# Weak-IV tests
obj_xtivdml$F_stat
obj_xtivdml$ARchi2
obj_xtivdml$ARchi2p
obj_xtivdml$ARcset

# Quality of learning (RMSEs) 
obj_xtivdml$model_rmse
obj_xtivdml$rmses["ml_l"])
obj_xtivdml$rmses["ml_r"])
obj_xtivdml$rmses["ml_m"])

# Selected hyperparameter values
obj_xtivdml$params
```

## Main functions

| Function | Purpose |
|----------|---------|
| `xtivdml` | R6 class implementing DML for partially linear panel models with an endogenous treatment and instruments. |
| `xtivdml_plr` | R6 class implementing DML for partially linear panel regression models with endogenous treatment variable. |
| `xtivdml_data` | Constructor for the data backend used by the DML estimator. |
| `xtivdml_data_from_data_frame` | Wrapper that builds an `xtivdml_data` object directly from a `data.frame`. |
| `make_plpr_data` | Simulates panel data from a partially linear model with fixed effects and endogenous variable as in Baiardi et al. (2026). |

Full documentation is available via `?xtivdml`, `?xtivdml_plr`, etc.

## Supported learners

Any regression or classification learner registered in `mlr3` can be plugged in for the nuisance functions, including for example:

- `ranger` / `rpart` — random forests and trees
- `xgboost` / `lightgbm` — gradient boosting
- `glmnet` / `cv_glmnet` — lasso, ridge, elastic net
- `lm` / `kknn` — linear regression, k-nearest neighbors
- ect.

Hyperparameter tuning is supported through `mlr3tuning`.

## Dependencies

`xtivdml` depends on `R (>= 3.5.0)` and imports `R6`, `data.table`, `mlr3`, `mlr3tuning`, `mlr3learners`, `mlr3misc`, `mvtnorm`, `clusterGeneration`, `readstata13`, `dplyr`, `magrittr`, `MASS`, `MLmetrics`, `checkmate`, and `sparseinv`. Suggested packages: `rpart`, `mlr3pipelines`.

## Citation

If you use `xtivdml` in academic work, please cite the methodological paper and the package:

```bibtex
@article{baiardi2026double,
  title={Double Machine Learning for Static Panel Data with Instrumental Variables: New Method and Applications},
  author={Baiardi, Anna and Clarke, Paul S and Naghi, Andrea A and Polselli, Annalivia},
  journal={arXiv preprint arXiv:2603.20464},
  year={2026}
}

@manual{polselli2026xtivdml,
  title  = {xtivdml: Double Machine Learning for Static Panel Models with Fixed Effects and Endogenous Treatment},
  author = {Polselli, Annalivia},
  year   = {2026},
  note   = {R package version 0.1.3},
  url    = {https://github.com/POLSEAN/xtivdml}
}
```

You can also generate an up-to-date entry from R with `citation("xtivdml")` once the package is loaded.

## References

- Bach, P., Chernozhukov, V., Kurz, M. S., and Spindler, M. (2024). DoubleML — An Object-Oriented Implementation of Double Machine Learning in R. *Journal of Statistical Software*, 108(3). <https://doi.org/10.18637/jss.v108.i03>
- Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C., Newey, W., and Robins, J. (2018). Double/debiased machine learning for treatment and structural parameters. *The Econometrics Journal*, 21(1), C1–C68.
- Clarke S. P., and Polselli A., (2026). Double machine learning for static panel models with fixed effects, The Econometrics Journal, Volume 29, Issue 1, January 2026, Pages 69–86. <https://doi.org/10.1093/ectj/utaf011>
- Lang, M. et al. (2019). mlr3: A modern object-oriented machine learning framework in R. *Journal of Open Source Software*, 4(44), 1903. <https://doi.org/10.21105/joss.01903>
- Polselli, A. (2025). xtdml: Double Machine Learning for Static Panel Models with Fixed Effects. <https://arxiv.org/abs/2512.15965>


