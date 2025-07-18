#' @title Comparar rodada V1 da análise pela TRI de bancos simulados
#' @name compara.sim.tri.v1
#'
#' @description Compara resultados da V1 da análise pela TRI de bancos simulados.
#'
#' @param banco lista com o banco simulado (respostas e gabarito)
#' @param tab.pars tabela de parâmetros gerado pela função `mirt` ou `multipleGroup` com argumento `pars = 'values'`
#' @param objeto.mirt objeto mirt da calibração
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
compara.sim.tri.v1 <- function(banco, tab.pars, objeto.mirt){

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

  } else {
    banco.aberto <- banco
  }

  data <- mirt::key2binary(banco.aberto$respostas[,-1], banco.aberto$gabarito$Gabarito)

  tab.sim <- mirt::mirt(data, 1, '3PL', pars = 'values')

  tab.sim <- pars.priori(tab.sim)

  fit.sim <- mirt::mirt(data, 1, '3PL', pars = tab.sim, TOL = .001, SE = FALSE)

  dif.ajuste <- dif.mirt(
    fit.atual = fit.sim,
    limite.rmsd = Inf)

  pars.sim <- data.frame(mirt::coef(fit.sim, IRTpars = TRUE, simplify = TRUE)$items)

  problema.b <- dplyr::select(pars.sim, dplyr::starts_with('b'))
  problema.b <- apply(problema.b, 1, function(x) any(x < 2.5 & x > -2.5))

  itens.problema <- c(
    rownames(pars.sim)[pars.sim$a < .5 | pars.sim$a > 4],
    rownames(pars.sim[!problema.b,]),
    rownames(pars.sim)[pars.sim$g > .45],
    dif.ajuste$itens
  )

  itens.problema <- unique(itens.problema)

  tab.sim <- mirt::mirt(data[,which(!colnames(data) %in% itens.problema)], 1, '3PL', pars = 'values', TOL = .001, SE = FALSE)

  tab.sim <- pars.priori(tab.sim)

  fit.sim <- mirt::mirt(data[,which(!colnames(data) %in% itens.problema)], 1, '3PL', pars = tab.sim, TOL = .001, SE = FALSE)

  # tab.pars <- tab.sim
  # objeto.mirt <- fit.sim

  pars.sim <- data.frame(mirt::coef(fit.sim, IRTpars = TRUE, simplify = TRUE)$items)
  pars.fit <- data.frame(mirt::coef(objeto.mirt, IRTpars = TRUE, simplify = TRUE)$items)

  if(nrow(tab.sim) != nrow(tab.pars))
    stop('O número de linhas da sua tabela de parâmetros não é igual ao oficial. Talvez a quantidade de itens seja diferente, ou a quantidade de parâmetros de algum(ns) item(ns). Verifique se os nomes dos itens são os mesmos, se os itens excluídos são os mesmos e se o tipo de cada item é o mesmo.')

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

  all.equal(fit.sim, objeto.mirt)


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
