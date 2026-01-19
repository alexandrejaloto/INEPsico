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

calibrar <- function(resp, nodes = seq(-4, 4, length.out = 40), max_iter = 100, tol = 0.001, fixed_params = NULL) {

  resp <- as.matrix(resp)
  n_items <- ncol(resp)
  n_subjects <- nrow(resp)

  # Valores iniciais
  current_params <- matrix(c(rep(1, n_items),
                             rep(0, n_items),
                             rep(0.1, n_items)),
                           nrow = n_items, ncol = 3)
  colnames(current_params) <- c("a", "b", "c")

  # Inicialização do Grupo (Métrica)
  current_mu <- 0
  current_sigma <- 1

  # Identificar itens fixos
  # Criamos um vetor lógico para saber quem otimizar
  is_fixed <- rep(FALSE, n_items)

  if (!is.null(fixed_params)) {
    # fixed_params deve ser uma lista/df com: item_idx, a, b, c
    indices <- fixed_params$item_idx
    current_params[indices, "a"] <- fixed_params$a
    current_params[indices, "b"] <- fixed_params$b
    current_params[indices, "c"] <- fixed_params$c
    is_fixed[indices] <- TRUE
  }

  for (iter in 1:max_iter) {
    old_params <- current_params

    # --- ETAPA E ---
    e_step <- passo_e(a = current_params[,1], b = current_params[,2], c = current_params[,3],
                             nodes = nodes, n_subjects = n_subjects, resp = resp,
                             mu = current_mu, sigma = current_sigma)

    # --- ATUALIZAÇÃO DA MÉTRICA DO GRUPO ---
    # Só atualizamos a métrica se houver itens fixos (equalização)
    # Caso contrário, fixamos em 0 e 1 para identificação padrão
    if (!is.null(fixed_params)) {
      N_k <- e_step$N_k
      current_mu <- sum(N_k * nodes) / sum(N_k)
      current_sigma <- sqrt(sum(N_k * (nodes - current_mu)^2) / sum(N_k))
    }

    # --- ETAPA M ---
    for (j in 1:n_items) {
      if (!is_fixed[j])
        current_params[j,] <- passo_m(item_idx = j,
                                          p_hat = e_step$p_hat,
                                          nodes = nodes,
                                          N_k = e_step$N_k,
                                          start_params = current_params[j,])
    }

    # Verificar convergência
    diff <- max(abs(current_params - old_params))
    cat("Iteração:", iter, "| Mudança Máxima:", round(diff, 6), "\n")

    if (diff < tol) {
      cat("Convergência atingida!\n")
      break
    }
  }
  # return(current_params)
  return(list(
    items = current_params,
    group_stats = c(mean = current_mu, sd = current_sigma),
    iterations = iter
  ))
}
