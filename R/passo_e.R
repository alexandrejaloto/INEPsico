#' @title Probabilidade esperada
#' @name passo_e
#'
#' @description Gera as probabilidades esperadas (etapa E do algoritmo EM)
#'
#' @param a valor do parâmetro a
#' @param b valor do parâmetro b
#' @param c valor do parâmetro c
#' @param nodes pontos de quadratura
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

passo_e <- function(a, b, c, nodes = seq(-4, 4, length.out = 40), resp, mu = 0, sigma = 1) {

  resp <- as.matrix(resp)
  n_subjects <- nrow(resp)
  n_items <- ncol(resp)
  q <- length(nodes)

  # Pesos dos nós
  A <- dnorm(nodes, mean = mu, sd = sigma)
  A <- A / sum(A)

  # Matriz de probabilidades P_jk
  P_jk <- matrix(0, nrow = n_items, ncol = q)
  for (j in 1:n_items) {
    P_jk[j, ] <- c[j] + (1 - c[j]) / (1 + exp(-a[j] * (nodes - b[j])))
  }
  P_jk <- pmin(pmax(P_jk, 1e-10), 1 - 1e-10)

  # Calcular log-verossimilhança ignorando NAs
  logL <- matrix(0, nrow = n_subjects, ncol = q)

  logP    <- log(P_jk)
  log1mP  <- log(1 - P_jk)

  for (i in seq_len(n_subjects)) {

    yi <- resp[i, ]
    ok <- !is.na(yi)

    if (any(ok)) {

      y_ok <- yi[ok]

      logL[i, ] <-
        crossprod(y_ok, logP[ok, , drop = FALSE]) +
        crossprod(1 - y_ok, log1mP[ok, , drop = FALSE])

    } else {
      logL[i, ] <- 0
    }
  }

  # log do prior
  logA <- log(A)

  # soma log-verossimilhança + log-prior
  V <- sweep(logL, 2, logA, "+")

  # softmax estável por linha
  V <- V - apply(V, 1, max)
  W <- exp(V)

  post <- W / rowSums(W)

  N_k <- colSums(post)
  N_k[N_k < 1e-8] <- 1e-8

  # Calcular r_jk (número esperado de acertos) ignorando NAs
  r_jk <- matrix(0, nrow = n_items, ncol = q)
  for (j in 1:n_items) {
    # Apenas sujeitos que responderam o item j
    sujeitos_que_responderam <- which(!is.na(resp[, j]))

    r_jk[j, ] <- colSums(post[sujeitos_que_responderam, , drop = FALSE] *
                           resp[sujeitos_que_responderam, j])
  }

  # Calcular p_hat: r_jk / N_k_ajustado
  # Onde N_k_ajustado é o número esperado de sujeitos no nó k que responderam o item j
  p_hat <- matrix(0, nrow = n_items, ncol = q)

  for (j in 1:n_items) {
    sujeitos_que_responderam <- which(!is.na(resp[, j]))

    # if (length(sujeitos_que_responderam) > 0) {
    N_k_ajustado <- colSums(post[sujeitos_que_responderam, , drop = FALSE])
    N_k_ajustado[N_k_ajustado < 1e-8] <- 1e-8
    p_hat[j, ] <- r_jk[j, ] / N_k_ajustado
    p_hat[j, ] <- pmin(pmax(p_hat[j, ], 0), 1)
    # } else {
    #   p_hat[j, ] <- rep(0.5, q)  # Item nunca respondido
    # }
  }

  return(list(p_hat = p_hat, N_k = N_k, post = post))
}
