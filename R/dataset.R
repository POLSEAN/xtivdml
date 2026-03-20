#' @title Simulated Data Frame
#' @name make_plpr_data
#'
#' @description
#'  Generates data from a partially linear panel regression (PLPR) model with
#'  instrumental variables as in Baiardi et al. (2026).
#'
#'  The data generating process is defined as
#'
#' \eqn{Y_{it} = \theta D_{it} + g_0(X_{it}) + \alpha_i + U_{it},}
#' \eqn{D_{it} = r_0(X_{it}) + \zeta_i + R_{it},}
#' \eqn{Z_{it} = m_0(X_{it}) + \gamma_i + V_{it},}

#' where \eqn{U_{it} \sim \mathcal{N}(0,1)}, \eqn{R_{it} \sim \mathcal{N}(0,1)},
#' \eqn{V_{it}  \sim \mathcal{N}(0,0.25^2)}, \eqn{\sigma_{ur} = 0.6},
#' \eqn{\alpha_i = \rho A_i + \sqrt{1-\rho^2} B_i} with
#' \eqn{Gamma_i\sim \mathcal{N}(3,3)}, \eqn{A_i\sim \mathcal{N}(0,1)}, and  \eqn{B_i\sim \mathcal{N}(0,5)}.
#'
#' The covariates are distributed as \eqn{X_{it,p} \sim A_i + \mathcal{N}(0, 5)},
#' where \eqn{p} is the number of covariates.
#'
#' The nuisance functions are given by
#'
#' m_0 = 0.5 * X_1 + 0.5 *  X_1 * as.numeric(X_1>0) + 0.5 *  X_3
#' r_0 = 0.5 * X_1 + 0.5 *  X_1 * as.numeric(X_1>0) + 0.5 *  X_3
#' g_0 = 0.5 * X_1 + 0.5 *  X_1 * as.numeric(X_1>0) + 0.5 *  X_3
#'
#' @param n_obs (`integer(1)`) \cr
#' The number of cross-sectional observations (i) to simulate.
#'
#' @param t_per (`integer(1)`) \cr
#' The number of time periods (t) to simulate.
#'
#' @param theta (`numeric(1)`) \cr
#' The value of the causal parameter.
#'
#' @param pi (`numeric(1)`) \cr
#' The value of the first-stage parameter.
#'
#' @param dim_x (`integer(1)`) \cr
#' The number of covariates.
#'
#' @param sigma_ur (`numeric(1)`) \cr
#' Correlation between error tems U and R. The parameter governs model endogeneity.
#'
#' @param rho (`numeric(1)`) \cr
#' Parameter governing the relationship between the covariates and the unobserved
#' individual heterogeneity. The value is chosen between 0 (pure random effect)
#' and 1 (pure fixed effects).
#'
#' @return A data object.
#'
#' @examples
#' df = make_plpr_data(n_obs = 500, t_per = 10, dim_x = 20,
#'                     theta = 0.5, pi = 0.8,
#'                     sigma_ur = 0.6, rho=0.8)
#'
#' @export
make_plpr_data = function(n_obs = 500, t_per = 10, dim_x = 20,
                          theta = 0.5, pi = 0.8,
                          sigma_ur = 0.6, rho=0.8){
  assert_count(n_obs)
  assert_count(dim_x)
  assert_numeric(theta, len = 1)
  assert_number(rho, lower = 0, upper = 1)

  a = 0.5

  time = rep(1:t_per, times=n_obs, each=1)
  id   = rep(1:n_obs, times=1, each=t_per)
  nt = n_obs * t_per
  df = data.frame(id, time)

  df = df %>%
    dplyr::group_by(id) %>%
    mutate(Ai = rnorm(1,3,3),
           Bi = rnorm(1,0,1),
           Gammai = rnorm(1,0,5))

  alphai = rho*df$Ai + sqrt(1 - rho^2)*df$Bi

  # covariates
  X = replicate(dim_x, df$Ai + rnorm(nt, 0, 1))

  # generate correlated error terms
  mean_vector <- c(0, 0)
  cov_matrix  <- matrix(c(1, sigma_ur, sigma_ur, 1), nrow = 2)
  errors <- mvrnorm(n = nt, mu = mean_vector, Sigma = cov_matrix)
  rit = errors[, 1]
  uit = errors[, 2]
  vit = rnorm(n_obs,0,0.25)

  # nuisance functions
  m0 <- 0.5 * X[[1]] + 0.5 * X[[1]]*as.numeric(X[[1]]>0) + 0.5 * X[[3]]  # for Z
  r0 <- 0.5 * X[[1]] + 0.5 * X[[1]]*as.numeric(X[[1]]>0) + 0.5 * X[[3]]  # for D
  g0 <- 0.5 * X[[1]] + 0.5 * X[[1]]*as.numeric(X[[1]]>0) + 0.5 * X[[3]]  # for Y


  # treatment and output variables
  z = as.matrix(1 + m0 + df$Bi + vit)
  d = as.matrix(1 + pi * z + r0 +  0.5*alphai + rit)
  y = as.matrix(1 + theta * d + g0 + alphai + uit)

  colnames(X) = paste0("X", 1:dim_x)
  colnames(y) = "y"
  colnames(d) = "d"
  colnames(z) = "z"

  data = data.frame(id,time,X, y, d, z)
  return(data)
}
