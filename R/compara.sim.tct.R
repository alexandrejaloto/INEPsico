#' @title Comparar análise pela TCT de bancos simulados
#' @name compara.sim.tct
#'
#' @description Compara resultados da análise pela TCT de bancos simulados.
#'
#' @param banco lista com o banco simulado (respostas e gabarito)
#' @param resultado.tct objeto com resultado da TCT gerado pela função `tct`
#'
#' @return A função mostra a comparação na tela.
#'
#' @author Alexandre Jaloto
#'
#' @examples
#' compara.sim.tct (banco = banco.sim.3PL, resultado.tct = tct.3PL)
#'
#' @export
compara.sim.tct <- function(banco, resultado.tct){

  # data(banco.sim.3PL, package = "INEPsico")

  tipo <- attr(banco, "tipo")

  if (is.null(tipo)) {
    stop("O objeto 'banco' precisa ser um banco simulado do pacote INEPsico.")
  }

  if (tipo != "3PL")
    # if (any(all.equal(banco, banco.sim.3PL) != TRUE))
  {
    usa.normit <- TRUE
  } else {
    usa.normit <- FALSE
  }

  banco.aberto <- gera.banco.compara(banco)

  tct.sim <- tct(banco.aberto = banco.aberto$respostas, gab.aberto = banco.aberto$gabarito,
                 alt = c("A", "B", "C", "D", "E", ".", "*"), usa.normit = usa.normit)
  if(usa.normit)
    tct.sim <- tct.sim$tct

  if (ncol(tct.sim) != ncol(resultado.tct))
    stop("O número de colunas do seu objeto de resultado da análise pela TCT não é igual ao oficial. É possível que você tenha especificado quantidade diferente de alternativas")
  if (nrow(tct.sim) != nrow(resultado.tct))
    stop("O número de itens do seu objeto de resultado da análise pela TCT não é igual ao oficial.")

  print("Comparação de cada coluna do seu objeto de resultado (precisa ser TRUE ou 0)")

  problema <- compara.tabelas(resultado.tct, tct.sim)

  if(problema)
  {
    print("Sugestões de verificação caso haja problema em uma dessas variáveis:")
    print("DIFI ou Prop_X: problemas ao abrir o banco")
    print("DISC, ABAI ou ACIM: escolha diferente do método de calcular o percentil")
    print("DISC, ABAI, ACIM, BISE ou Bis_X: uso do normit")
  }
}
