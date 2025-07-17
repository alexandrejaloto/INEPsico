#' @title Comparar análise pela TCT de bancos simulados
#' @name compara.sim.tct
#'
#' @description Compara resultados da análise pela TCT de bancos simulados.
#'
#' @param banco lista com o banco simulado (respostas e gabarito)
#' @param resultado objeto com resultado da TCT gerado pela função `tct`
#'
#' @return A função mostra a comparação na tela.
#'
#' @author Alexandre Jaloto
#'
#' @examples
#' compara.sim.tct (banco = banco.sim.3PL, resultado = tct.3PL)
#'
#' @export
compara.sim.tct <- function(banco, resultado){

  if(all.equal(banco, banco.sim.BIB.fechado))
  {
    resps <- abre.resp(banco.sim.BIB.fechado$respostas$TX_RESPOSTA)
    banco.aberto <- cbind(banco.sim.BIB.fechado$respostas, resps)

    banco.aberto <- abre.banco(banco = banco.aberto[,-c(1, 3)],
                                itens = banco.sim.BIB.fechado$itens,
                                bib = banco.sim.BIB.fechado$BIB,
                                disc = 'LP',
                                disc.cad = 1)

    banco.aberto$respostas$ID <- banco.sim.BIB.fechado$respostas$ID
    banco.aberto$respostas <- dplyr::arrange(banco.aberto$respostas, ID)
    banco.aberto$respostas <- dplyr::select(banco.aberto$respostas,
                                            -ID)

  } else {
    banco.aberto <- banco
  }

  tct.sim <- tct(banco.aberto = banco.aberto$respostas, gab.aberto = banco.aberto$gabarito, alt = c("A", "B", "C", "D", "E", ".", "*"), usa.normit = FALSE)

  if(ncol(tct.sim) != ncol(resultado))
    stop('O número de colunas do seu objeto de resultado da análise pela TCT não é igual ao oficial. É possível que você tenha especificado quantidade diferente de alternativas')

  if(nrow(tct.sim) != nrow(resultado))
    stop('O número de itens do seu objeto de resultado da análise pela TCT não é igual ao oficial.')

  print('Comparação de cada coluna do seu objeto de resultado')
  for(i in 1:ncol(tct.sim))
  {
    print(paste0(names(tct.sim)[i],
                 ': ',
                 all.equal(resultado[,i], tct.sim[,i])))
  }

  if(all.equal(resultado, tct.sim) != TRUE)
  {print('Sugestões de verificação caso haja problema em uma dessas variáveis:')
    print('DIFI ou Prop_X: problemas ao abrir o banco')
    print('DISC, ABAI ou ACIM: escolha diferente do método de calcular o percentil')
    print('DISC, ABAI, ACIM, BISE ou Bis_X: uso do normit')}

}
