#' @title Altera priori dos parâmetros
#' @name pars.priori
#'
#' @description Altera valores iniciais e distribuição prévia dos parâmetros dos itens na tabela de parâmetros
#'
#' @param values tabela de parâmetros gerado pela função `mirt` ou `multipleGroup` com argumento `pars = 'values'`
#'
#' @details Distribuição do a é lognormal com média 0 e desvio 0.5,
#' com valor inicial 1.7. Distribuição do c é beta com alpha 5 e beta 17,
#' com valor inicial 0.2.
#'
#' @return Tabela de parâmetros com valores alterados
#'
#' @author Alexandre Jaloto
#'
#' @examples
#' pars.priori (parametros)
#'
#' @export
pars.priori <- function(values){

  values[values$name == 'a1', 'prior.type'] <- 'lnorm'
  values[values$name == 'a1', 'prior_1'] <- 0
  values[values$name == 'a1', 'prior_2'] <- 0.5
  values[values$name == 'a1', 'value'] <- 1.7

  values[values$name == 'g', 'prior.type'] <- 'expbeta'
  values[values$name == 'g', 'prior_1'] <- 5
  values[values$name == 'g', 'prior_2'] <- 17
  values[values$name == 'g', 'value'] <- 0.2

  return(values)

}
