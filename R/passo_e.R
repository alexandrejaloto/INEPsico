#' @title Probabilidade esperada
#' @name passo_e
#'
#' @description Gera as probabilidades esperadas (etapa E do algoritmo EM)
#'
#' @param a valor do parâmetro a
#' @param b valor do parâmetro b
#' @param c valor do parâmetro c
#' @param nodes pontos de quadratura
#' @param n_subjects quantidade de sujeitos
#' @param resp padrão de resposta dos sujeitos
#' @param mu média do grupo
#' @param sigma desvio padrão do grupo
#'
#' @return Lista com quantidade esperada de sujeitos em cada
#' ponto de quadratura e proporção esperada de acertos por item em
#' cada ponto
#'
#' @author Alexandre Jaloto
#'
#' @export

passo_e <- function(a, b, c, nodes = seq(-4, 4, length.out = 40), n_subjects, resp, mu = 0, sigma = 1)
{

  A <- dnorm(nodes, mean = mu, sd = sigma)
  A <- A / sum(A)
  q <- length(nodes)
  n_items <- length(a)
  P_jk <- matrix(0, nrow = n_items, ncol = q)
  for (j in 1:n_items) {
    P_jk[j, ] <- c[j] + (1 - c[j]) / (1 + exp(-a[j] * (nodes - b[j])))
  }

  P_jk <- pmin(pmax(P_jk, 1e-10), 1 - 1e-10)  # ANTES de calcular log

  logL <- resp %*% log(P_jk) + (1 - resp) %*% log(1 - P_jk)

  post <- matrix(0, nrow = n_subjects, ncol = q)
  for (i in 1:n_subjects) {
    v <- logL[i, ] + log(A)
    vmax <- max(v)
    w <- exp(v - vmax)
    post[i, ] <- w / sum(w)
  }
  N_k <- colSums(post)

  # evitar que algum nó fique com 0
  eps <- 1e-8
  N_k[N_k < eps] <- eps

  r_jk <- matrix(0, nrow = n_items, ncol = q)
  for (j in 1:n_items) {
    r_jk[j, ] <- colSums(post * resp[, j])
  }
  p_hat <- r_jk / matrix(rep(N_k, each = n_items), nrow = n_items)

  return(list(p_hat = p_hat, N_k = N_k))
}

