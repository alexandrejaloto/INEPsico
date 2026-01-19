#' @title Filtrar itens pelos parâmetros
#' @name filtro.par
#'
#' @description Filtra os itens de acordo com os intervalos estabelecidos para os parâmetros da TRI
#'
#' @param pars `data.frame` com os parâmetros dos itens
#' @param a vetor com valores mínimo e máximo do intervalo aceitável para o parâmetro `a`
#' @param b vetor com valores mínimo e máximo do intervalo aceitável para o parâmetro `b`
#' @param c valor máximo aceitável para o parâmetro `c`
#'
#' @details
#' Para a análise, são excluídos automaticamente os itens que apresentem pelo menos um dos seguintes critérios:
#' \itemize{
#' \item a < .5 ou a > 4
#' \item nenhum parâmetro b entre -2.5 e 2.5
#' \item c > .45
#' \item Problema de ajuste
#' \item Existência de DIF
#' }
#'
#' @return A função mostra a comparação na tela.
#'
#' @author Alexandre Jaloto
#'
#' @examples
#' compara.sim.tri.v1 (banco = banco.sim.3PL, tab.pars = values, objeto.mirt = fit1)
#'
#' @export

filtro.par <- function(pars, a = c(0.5, 4), b = c(-2.5, 2.5), c = .45)
{

  eliminados <- list()

  pars <- subset(pars, !is.na(pars$a))
  problema.a <- rownames(pars)[pars$a < a[1] | pars$a > a[2]]
  eliminados$a <- c(eliminados$a, problema.a)
  problema.b <- dplyr::select(pars, dplyr::starts_with("b"))
  problema.b <- apply(problema.b, 1, function(x) any(x < b[2] &  x > b[1]))
  problema.b <- rownames(pars[!problema.b, ])
  eliminados$b <- c(eliminados$b, problema.b)
  problema.c <- rownames(pars)[pars$g > c]
  eliminados$c <- c(eliminados$c, problema.c)

  return(eliminados)

}
