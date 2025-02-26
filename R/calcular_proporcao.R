#' @title Calcular proporção de acerto
#' @name calcular_proporcao
#'
#' @description Calcula a proporção de acerto a um item
#'
#' @param resp.item objeto com as respostas ao item
#' @param gabarito gabarito do item
#'
#' @return Valor da proporção de acerto ao item
#'
#' @author Alexandre Jaloto
#' @keywords internal

calcular_proporcao <- function(resp.item, gabarito) {
  acertou <- resp.item[resp.item == gabarito]
  tentativas <- resp.item[!is.na(resp.item) & resp.item != "9"]
  sum(!is.na(acertou)) / length(tentativas)
}
