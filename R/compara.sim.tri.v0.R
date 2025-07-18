#' @title Comparar rodada V0 da análise pela TRI de bancos simulados
#' @name compara.sim.tri.v0
#'
#' @description Compara resultados da V0 da análise pela TRI de bancos simulados.
#'
#' @param banco lista com o banco simulado (respostas e gabarito)
#' @param tab.pars tabela de parâmetros gerado pela função `mirt` ou `multipleGroup` com argumento `pars = 'values'`
#' @param objeto.mirt objeto mirt da calibração
#'
#' @return A função mostra a comparação na tela.
#'
#' @author Alexandre Jaloto
#'
#' @examples
#' compara.sim.tri.v0 (banco = banco.sim.3PL, tab.pars = values, objeto.mirt = fit1)
#'
#' @export
compara.sim.tri.v0 <- function(banco, tab.pars, objeto.mirt){

  # data(banco.sim.3PL)
  # banco <- banco.sim.3PL

  if(!all.equal(banco, banco.sim.3PL))
  {

    if(all.equal(banco, banco.sim.BIB.aberto))
      banco.fechado <- banco.sim.BIB.aberto

    if(all.equal(banco, banco.sim.BIB.fechado))
      banco.fechado <- banco.sim.BIB.fechado

    resps <- abre.resp(banco.fechado$respostas$TX_RESPOSTA)
    banco.aberto <- cbind(banco.fechado$respostas, resps)

    banco.aberto <- abre.banco(banco = banco.aberto[,-c(1, 3)],
                               itens = banco.fechado$itens,
                               bib = banco.fechado$BIB,
                               disc = 'LP',
                               disc.cad = 1)

    banco.aberto$respostas$ID <- banco.fechado$respostas$ID
    banco.aberto$respostas <- dplyr::arrange(banco.aberto$respostas, ID)
    banco.aberto$respostas <- dplyr::select(banco.aberto$respostas,
                                            -ID)

    usa.normit = TRUE

  } else {
    banco.aberto <- banco

    usa.normit = FALSE
  }

  tct.sim <- tct(banco.aberto = banco$respostas, gab.aberto = banco$gabarito, alt = c("A", "B", "C", "D", "E", ".", "*"), usa.normit = usa.normit)

  if(usa.normit)
    tct.sim <- tct.sim$tct

  itens.problema.bis <- tct.sim$Item[tct.sim$BISE < 0]

  data <- mirt::key2binary(banco.aberto$respostas[,-1], banco.aberto$gabarito$Gabarito)

  data <- data[,which(!colnames(data) %in% itens.problema.bis)]

  tab.sim <- mirt::mirt(data, 1, '3PL', pars = 'values')

  tab.sim <- pars.priori(tab.sim)

  if(nrow(tab.sim) != nrow(tab.pars))
    stop('O número de linhas da sua tabela de parâmetros não é igual ao oficial. Talvez a quantidade de itens seja diferente')

  print('Comparação de cada coluna da sua tabela de parâmetros')
  for(i in 1:ncol(tab.sim))
  {
    print(paste0(names(tab.sim)[i],
                 ': ',
                 all.equal(tab.pars[,i], tab.sim[,i])))
  }

  if(all.equal(tab.pars, tab.sim) != TRUE)
  {print('Sugestões de verificação caso haja problema em uma dessas variáveis:')
    print('item: problemas ao nomear os itens')
    print('value: valores iniciais dos parâmetros errados ou parâmetros dos itens comuns errados')
    print('est: itens comuns não devem ser calibrados')
    print('prior.type, priot_1 ou prior_2: distribuição prévia dos parâmetros errada')
    stop('Tabela de parâmetros com divergência')}

  fit.sim <- mirt::mirt(data, 1, '3PL', pars = tab.sim, TOL = .001, SE = TRUE)

  # objeto.mirt <- fit.sim

  all.equal(fit.sim, objeto.mirt)

  pars.sim <- data.frame(mirt::coef(fit.sim, IRTpars = TRUE, simplify = TRUE)$items)
  pars.fit <- data.frame(mirt::coef(objeto.mirt, IRTpars = TRUE, simplify = TRUE)$items)

  print('Comparação de cada parâmetro')
  for(i in 1:ncol(pars.sim))
  {
    print(paste0(names(pars.sim)[i],
                 ': ',
                 all.equal(pars.fit[,i], pars.sim[,i])))
  }

  if(all.equal(pars.fit, pars.sim) != TRUE)
    print(paste('Os parâmetros não estão iguais.',
                'É possível que o comando utilizado para calibração esteja errado.',
                'Outra possibilidade é que o ordenamento do banco esteja diferente.', sep = ' '))

}
