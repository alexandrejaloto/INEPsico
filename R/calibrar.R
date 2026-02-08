#' @title Calibração de itens por TRI via algoritmo EM
#'
#' @description
#' Realiza a calibração de itens dicotômicos pelo modelo logístico de três
#' parâmetros (3PL) utilizando o algoritmo EM com quadratura discreta.
#' A função alterna entre o passo E, no qual são calculadas as quantidades
#' esperadas condicionais à habilidade, e o passo M, no qual os parâmetros
#' dos itens são atualizados por maximização da verossimilhança esperada.
#'
#' Opcionalmente, permite a fixação de parâmetros de itens para fins de
#' identificação ou equalização, bem como a atualização da métrica do grupo
#' (média e desvio padrão da habilidade).
#'
#' @param resp Matriz de respostas dos sujeitos aos itens, com sujeitos nas
#'   linhas e itens nas colunas. Os valores devem ser 0 (erro) ou 1 (acerto).
#' @param nodes Vetor numérico com os pontos de quadratura da habilidade
#'   (\eqn{\theta}) utilizados na integração discreta.
#' @param max_iter Número máximo de iterações do algoritmo EM.
#' @param tol Critério de convergência. O algoritmo é interrompido quando a
#'   maior mudança absoluta nos parâmetros dos itens entre iterações é menor
#'   que esse valor.
#' @param fixed_params Objeto opcional contendo os parâmetros fixados de
#'   determinados itens. Deve conter as colunas \code{item_idx}, \code{a},
#'   \code{b} e \code{c}. Quando especificado, esses itens não são atualizados
#'   no passo M e são utilizados para definir a métrica da habilidade.
#'
#' @return
#' Uma lista com os seguintes elementos:
#' \describe{
#'   \item{items}{Matriz com os parâmetros estimados dos itens
#'   (\eqn{a}, \eqn{b}, \eqn{c}).}
#'   \item{group_stats}{Vetor nomeado contendo a média e o desvio padrão
#'   estimados da distribuição da habilidade.}
#'   \item{iterations}{Número de iterações realizadas até a convergência
#'   (ou até atingir \code{max_iter}).}
#' }
#'
#' @details
#' A calibração é realizada segundo o seguinte esquema:
#'
#' \enumerate{
#'   \item \strong{Passo E:} cálculo das probabilidades a posteriori dos sujeitos
#'   nos pontos de quadratura, bem como das proporções esperadas de acerto por
#'   item e por ponto de habilidade.
#'   \item \strong{Atualização da métrica:} quando parâmetros de itens são
#'   fixados, a média e o desvio padrão da habilidade são atualizados a partir
#'   da distribuição posterior esperada.
#'   \item \strong{Passo M:} atualização dos parâmetros dos itens livres por
#'   maximização da verossimilhança completa esperada.
#' }
#'
#' Na ausência de itens fixos, a métrica da habilidade é mantida em média zero
#' e desvio padrão um, garantindo a identificação padrão do modelo.
#'
#' @seealso
#' \code{\link{passo_e}}, \code{\link{passo_m}}
#'
#' @author Alexandre Jaloto
#'
#' @export

calibrar <- function(resp, nodes = seq(-4, 4, length.out = 40),
                     max_iter = 100, tol = 0.001, fixed_params = NULL) {

  resp <- as.matrix(resp)
  n_items <- ncol(resp)
  n_subjects <- nrow(resp)

  cat("Itens:", n_items, " | Sujeitos:", n_subjects, "\n")
  cat("Dados válidos:", sum(!is.na(resp)), "/", prod(dim(resp)),
      sprintf("(%.1f%%)", 100*sum(!is.na(resp))/prod(dim(resp))), "\n")

  # verificar itens sem resposta
  itens_sem_resposta <- which(colSums(!is.na(resp)) == 0)

  if (length(itens_sem_resposta) > 0) {
    stop(sprintf(
      "Calibração interrompida. Os seguintes itens estão sem resposta: %s",
      paste(itens_sem_resposta, collapse = ", ")
    ))
  }

  # valor inicial do a
  a_init <- 1.0

  # valor inicial do c
  c_init <- 0.2

  p_j <- colMeans(resp, na.rm = TRUE)

  # Cálculo de b_init
  mask <- p_j > c_init & p_j < (1 - 1e-4)
  b_init <- numeric(n_items)
  b_init[mask] <- -log((p_j[mask] - c_init) / (1 - p_j[mask]))
  b_init <- pmin(pmax(b_init, -3), 3)
  b_init[!mask] <- 0

  current_params <- matrix(c(a_init, 0, c_init),
                           nrow = n_items, ncol = 3, byrow = TRUE)

  current_params[,2] <- b_init

  colnames(current_params) <- c("a", "b", "c")

  # Itens fixos (âncora)
  is_fixed <- rep(FALSE, n_items)
  if (!is.null(fixed_params)) {
    indices <- fixed_params$item_idx
    current_params[indices, ] <- as.matrix(fixed_params[, c("a", "b", "c")])
    is_fixed[indices] <- TRUE
    cat("Itens fixos (âncora):", indices, "\n")
  }

  # Algoritmo EM
  current_mu <- 0
  current_sigma <- 1

  for (iter in 1:max_iter) {
    old_params <- current_params

    # Etapa E
    e_step <- passo_e(
      a = current_params[, 1],
      b = current_params[, 2],
      c = current_params[, 3],
      nodes = nodes,
      resp = resp,
      mu = current_mu,
      sigma = current_sigma
    )

    # Etapa M
    for (j in 1:n_items) {
      if (!is_fixed[j]) {
        current_params[j, ] <- passo_m(
          item_idx = j,
          p_hat = e_step$p_hat,
          nodes = nodes,
          N_k = e_step$N_k,
          start_params = current_params[j, ],
          prior_c = c(5, 17)
        )
      }
    }

    # Atualizar métrica se houver âncoras
    if (!is.null(fixed_params)) {
      N_k <- e_step$N_k
      current_mu <- sum(N_k * nodes) / sum(N_k)
      current_sigma <- sqrt(sum(N_k * (nodes - current_mu)^2) / sum(N_k))
    }

    # Verificar convergência
    diff <- max(abs(current_params - old_params), na.rm = TRUE)
    cat(sprintf("Iteração %3d | Mudança: %.6f\n", iter, diff))

    if (diff < tol) {
      cat("Convergência atingida!\n")
      break
    }
  }

  # Informações adicionais
  n_respondentes <- colSums(!is.na(resp))

  return(list(
    items = current_params,
    group_stats = c(mean = current_mu, sd = current_sigma),
    iterations = iter,
    n_respondentes = n_respondentes,
    p_hat = e_step$p_hat
  ))
}
