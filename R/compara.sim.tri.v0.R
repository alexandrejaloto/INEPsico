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

  tipo <- attr(banco, "tipo")

  if (is.null(tipo)) {
    stop("O objeto 'banco' precisa ser um banco simulado do pacote INEPsico.")
  }

  if (tipo != "3PL")
  {
    usa.normit <- TRUE
  } else {
    usa.normit <- FALSE
  }

  banco.aberto <- gera.banco.compara(banco)

  tct.sim <- tct(banco.aberto = banco.aberto$respostas, gab.aberto = banco.aberto$gabarito, alt = c("A", "B", "C", "D", "E", ".", "*"), usa.normit = usa.normit)

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

  problema <- compara.tabelas(tab.pars, tab.sim)

  if (problema)
  {
    print('Sugestões de verificação caso haja problema em uma dessas variáveis:')
    print('item: problemas ao nomear os itens')
    print('value: valores iniciais dos parâmetros errados ou parâmetros dos itens comuns errados')
    print('est: itens comuns não devem ser calibrados')
    print('prior.type, priot_1 ou prior_2: distribuição prévia dos parâmetros errada')
    stop('Tabela de parâmetros com divergência')
  }

  print('Verificando a calibração dos itens')

  fit.sim <- mirt::mirt(data, 1, '3PL', pars = tab.sim, TOL = .001, verbose = FALSE)

  pars.sim <- data.frame(mirt::coef(fit.sim, IRTpars = TRUE, simplify = TRUE)$items)
  pars.fit <- data.frame(mirt::coef(objeto.mirt, IRTpars = TRUE, simplify = TRUE)$items)

  print('Comparação de cada parâmetro')

  problema <- compara.tabelas(pars.fit, pars.sim)

  if(problema)
    print(paste('Os parâmetros não estão iguais.',
                'É possível que o comando utilizado para calibração esteja errado.',
                'Outra possibilidade é que o ordenamento do banco esteja diferente.', sep = ' '))

}
