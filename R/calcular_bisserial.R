#' @title Calcular correlação bisserial alterada
#' @name calcular_bisserial
#'
#' @description Calcula correlação bisserial quando a variável dicotômica
#' é tratada como contínua com uma distribuição normal subjacente
#'
#' @param resp.item objeto com as respostas ao item
#' @param escore escore dos sujeitos
#' @param gabarito gabarito do item
#' @param pop TRUE se for população, FALSE (padrão) se for uma amostra. Essa escolha interfere nas contas que envolvem o cálculo da
#' variância ou do desvio padrão.
#'
#' @return Valor da correlação bisserial alterada
#'
#' @author Alexandre Jaloto
#' @keywords internal

calcular_bisserial <- function(resp.item, escore, gabarito, pop) {
  acertou <- escore[resp.item == gabarito]
  tentativas <- escore[!is.na(resp.item) & resp.item != "9"]
  Mp2 <- mean(acertou, na.rm = TRUE)
  M2 <- mean(tentativas)
  S2 <- ifelse(pop, sqrt(mean((tentativas - M2)^2, na.rm = TRUE)), sd(tentativas))
  p2 <- sum(!is.na(acertou)) / length(tentativas)
  hp2 <- exp((-qnorm(p2)^2) / 2) / sqrt(2 * pi)
  ((Mp2 - M2) / S2) * (p2 / hp2)
}
