#' @title Gerar banco para comparação
#' @name gera.banco.compara
#'
#' @description Gera o banco aberto para as análises de TCT e TRI dos bancos simulados
#'
#' @param banco lista com o banco simulado (respostas e gabarito)
#'
#' @return Banco aberto
#'
#' @author Alexandre Jaloto
#'
#' @examples
#' gera.banco.compara (banco = banco.sim.3PL)
#'
#'
gera.banco.compara <- function(banco){

  if (any(all.equal(banco, banco.sim.3PL) != TRUE)) {

    usa.normit <- TRUE

    if (all(all.equal(banco, banco.sim.BIB.aberto) == TRUE))
      banco.aberto <- banco.sim.BIB.aberto
    if (all(all.equal(banco, banco.sim.BIB.fechado) == TRUE))
    {
      banco.fechado <- banco.sim.BIB.fechado
      resps <- abre.resp(banco.fechado$respostas$TX_RESPOSTA)
      banco.aberto <- cbind(banco.fechado$respostas, resps)
      banco.aberto <- abre.banco(banco = banco.aberto[, -c(1, 3)],
                                 itens = banco.fechado$itens, bib = banco.fechado$BIB,
                                 disc = "LP", disc.cad = 1)
      banco.aberto$respostas$ID <- banco.fechado$respostas$ID
      banco.aberto$respostas <- dplyr::arrange(banco.aberto$respostas, ID)
      banco.aberto$respostas <- dplyr::select(banco.aberto$respostas, -ID)
    }
  } else {

    usa.normit <- FALSE

    banco.aberto <- banco

  }
  return(banco.aberto)
}
