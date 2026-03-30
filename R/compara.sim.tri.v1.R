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
#' \item Problema de ajuste pelo RMSD
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

  tipo <- attr(banco, "tipo")

  if (is.null(tipo)) {
    stop("O objeto 'banco' precisa ser um banco simulado do pacote INEPsico.")
  }

  # verificar se tem BIB para usar normit
  if (tipo != "3PL")
  {
    usa.normit <- TRUE
  } else {
    usa.normit <- FALSE
  }

  # gerar o banco para comparação
  banco.aberto <- gera.banco.compara(banco)

  # TCT
  tct.sim <- tct(banco.aberto = banco.aberto$respostas, gab.aberto = banco.aberto$gabarito, alt = c("A", "B", "C", "D", "E", ".", "*"), usa.normit = usa.normit)

  if(usa.normit)
    tct.sim <- tct.sim$tct

  # excluir os itens com bisserial < 0
  itens.problema.bis <- tct.sim$Item[tct.sim$BISE < 0]

  banco.mirt <- mirt::key2binary(banco.aberto$respostas[,-1], banco.aberto$gabarito$Gabarito)

  banco.mirt <- banco.mirt[,which(!colnames(banco.mirt) %in% itens.problema.bis)]

  print('Verificando a calibração dos itens')

  # tabela inicial de parâmetros
  tab.sim <- mirt::mirt(banco.mirt, 1, '3PL', pars = 'values')

  tab.sim <- pars.priori(tab.sim)

  if (nrow(tab.sim) != nrow(tab.pars))
    stop("O número de linhas da sua tabela de parâmetros não é igual ao oficial. Talvez a quantidade de itens seja diferente, ou a quantidade de parâmetros de algum(ns) item(ns). Verifique se os nomes dos itens são os mesmos, se os itens excluídos são os mesmos e se o tipo de cada item é o mesmo.")

  # ciclo de análise de dif, ajuste e parâmetros
  itens.problema <- 1
  eliminados <- list(ajuste = c(),
                     a = c(),
                     b = c(),
                     c = c())

  while(length(itens.problema) != 0)
  {

    fit.sim <- mirt::mirt(banco.mirt, 1, "3PL", pars = tab.sim, TOL = 0.001, SE = FALSE, verbose = FALSE)

    dif.ajuste <- dif.mirt(fit.atual = fit.sim, aviso = FALSE)

    eliminados$ajuste <- c(eliminados$ajuste, dif.ajuste$rmsd.pisa)

    pars.sim <- data.frame(mirt::coef(fit.sim, IRTpars = TRUE,
                                      simplify = TRUE)$items)

    itens.problema <- filtro.par(pars.sim)

    eliminados$a <- c(eliminados$a, itens.problema$a)
    eliminados$b <- c(eliminados$b, itens.problema$b)
    eliminados$c <- c(eliminados$c, itens.problema$c)

    itens.problema <- c(itens.problema$a, itens.problema$b, itens.problema$c)

    itens.problema <- unique(itens.problema, eliminados$ajuste)

    tab.sim[tab.sim$item %in% itens.problema & tab.sim$name == 'a1', 'value'] <- 0
    tab.sim[tab.sim$item %in% itens.problema, 'est'] <- FALSE
  }

  # pars.sim2 <- data.frame(mirt::coef(fit.sim2, IRTpars = TRUE, simplify = TRUE)$items)
  pars.sim <- data.frame(mirt::coef(fit.sim, IRTpars = TRUE, simplify = TRUE)$items)
  pars.fit <- data.frame(mirt::coef(objeto.mirt, IRTpars = TRUE, simplify = TRUE)$items)

  # Relatório

  print("Itens eliminados:")
  print(eliminados)

  print('Comparação de cada coluna da sua tabela inicial de parâmetros')

  problema <- compara.tabelas(tab.pars, tab.sim)

  if(problema)
  {
    print('Sugestões de verificação caso haja problema em uma dessas variáveis:')
    print('item: problemas ao nomear os itens')
    print('value: valores iniciais dos parâmetros errados; parâmetros dos itens comuns errados; itens excluídos na V1 devem ter a1=0')
    print('est: itens comuns devem ter est=FALSE; itens excluídos na V1 devem ter est=FALSE')
    print('prior.type, priot_1 ou prior_2: distribuição prévia dos parâmetros errada')
    stop('Tabela de parâmetros com divergência')
  }

  print('Comparação de cada parâmetro calibrado')

  problema <- compara.tabelas(pars.fit, pars.sim)

  if(problema)
    print(paste('Os parâmetros calibrados estão diferentes.',
                'É possível que o comando utilizado para calibração esteja errado.',
                'Outra possibilidade é que o ordenamento do banco esteja diferente.', sep = ' '))
}
