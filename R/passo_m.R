#' @title Atualização dos parâmetros do item (passo M)
#' @name passo_m
#'
#' @description
#' Executa o passo M do algoritmo EM para um item específico no modelo
#' logístico de três parâmetros (3PL). A função atualiza os parâmetros
#' \eqn{a}, \eqn{b} e \eqn{c} do item maximizando a verossimilhança esperada,
#' calculada a partir das proporções esperadas de acerto por ponto de
#' quadratura obtidas no passo E. Opcionalmente, incorpora uma penalização
#' Bayesiana do tipo Beta sobre o parâmetro de acerto ao acaso (\eqn{c}).
#'
#' @param item_idx Índice do item a ser estimado.
#' @param p_hat Matriz de proporções esperadas de acerto por item e por ponto
#'   de quadratura, conforme retornado pelo passo E.
#' @param nodes Vetor de pontos de quadratura da habilidade (\eqn{\theta}).
#' @param N_k Vetor com o número esperado de sujeitos em cada ponto de
#'   quadratura.
#' @param start_params Vetor numérico de valores iniciais para os parâmetros
#'   do item (\eqn{a}, \eqn{b}, \eqn{c}).
#' @param prior_c Vetor numérico de comprimento 2 com os hiperparâmetros
#'   \eqn{(\alpha, \beta)} da distribuição Beta utilizada como prior para o
#'   parâmetro \eqn{c}. Se \code{NULL}, nenhuma penalização é aplicada.
#'
#' @return
#' Vetor numérico de comprimento 3 contendo os parâmetros estimados do item:
#' discriminação (\eqn{a}), dificuldade (\eqn{b}) e acerto ao acaso (\eqn{c}).
#'
#' @details
#' A função maximiza a verossimilhança completa esperada do item:
#'
#' \deqn{
#' \sum_k \left[
#' r_{jk} \log P_j(\theta_k) +
#' (N_k - r_{jk}) \log(1 - P_j(\theta_k))
#' \right]
#' }
#'
#' onde \eqn{r_{jk} = \hat{p}_{jk} N_k} representa o número esperado de acertos
#' do item \eqn{j} no ponto de quadratura \eqn{\theta_k}. Quando especificado,
#' um termo de penalização \eqn{\log \text{Beta}(c \mid \alpha, \beta)} é somado
#' à log-verossimilhança.
#'
#' A otimização é realizada via \code{\link[stats]{optim}} com o método
#' \code{"L-BFGS-B"}, impondo restrições de positividade para \eqn{a} e limites
#' plausíveis para \eqn{c}.
#'
#' @author Alexandre Jaloto
#'
#' @export

passo_m <- function(item_idx, p_hat, nodes, N_k, start_params,
                        prior_c = c(5, 17))
{

  # prior_c = c(5, 17)

  ll_item <- function(params) {
    a_i <- params[1]
    b_i <- params[2]
    c_i <- params[3]

    # Restrições básicas
    if (a_i <= 1e-8 | c_i < 0 | c_i > 1)
      return(1e10)

    # Cálculo seguro da probabilidade
    z <- -a_i * (nodes - b_i)

    # Evitar overflow
    z <- pmin(pmax(z, -30), 30)

    exp_z <- exp(z)
    P <- c_i + (1 - c_i) / (1 + exp_z)

    # Garantir que P esteja em (0,1)
    P <- pmin(pmax(P, 1e-10), 1 - 1e-10)

    r_j <- p_hat[item_idx, ] * N_k

    # Log-verossimilhança com proteção
    ll <- sum(
      r_j * log(P) +
        (N_k - r_j) * log(1 - P)
    )

    if (!is.finite(ll))
      return(1e10)

    # --- Penalização Beta para c ---
    if (!is.null(prior_c)) {
      alpha <- prior_c[1]
      beta  <- prior_c[2]

      if (c_i > 1e-8 && c_i < (1 - 1e-8)) {
        log_prior_c <- (alpha - 1) * log(c_i) +
          (beta - 1) * log(1 - c_i)
        ll <- ll + log_prior_c
      }
    }
    return(-ll)  # negativo para minimização
  }

  opt <- optim(
    par = start_params,
    fn = ll_item,
    method = "L-BFGS-B",
    lower = c(0.01, -Inf, 1e-4),
    upper = c(Inf, Inf, 0.35)
  )

  return(opt$par)
}
