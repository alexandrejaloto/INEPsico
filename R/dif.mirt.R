#' @title Análise de DIF para o mirt
#' @name dif.mirt
#'
#' @description Verifica existência de DIF e a qualidade do ajuste de um item calibrado no mirt
#'
#' @param fit.antigo objeto mirt do tipo `SingleGroupClass` referente ao grupo em que os itens foram calibrados (grupo de referência).
#' @param fit.atual objeto mirt do tipo `SingleGroupClass` referente ao grupo da aplicação em voga (grupo focal).
#' @param comuns.antigo vetor com nomes dos itens comuns no banco antigo. Os nomes dos itens podem ser obtidos com `mirt::extract.mirt(fit.antigo, 'itemnames')`. Só é necessário se os nomes dos itens forem diferentes nos dois bancos.
#' @param comuns.atual vetor com nomes dos itens comuns no banco atual Os nomes dos itens podem ser obtidos com `mirt::extract.mirt(fit.atual, 'itemnames')`. Só é necessário se os nomes dos itens forem diferentes nos dois bancos.
#' @param int.qdpt intervalo dos pontos de quadratura. Esse intervalo será usado para a análise de MaxADif. O padrão é `c(-6, 6)`.
#' @param n.qdpt quantidade de pontos de quadratura.
#'
#' @return A função retorna uma lista com quatro elementos
#' \itemize{
#' \item `info` lista com os percentis dos grupos atual e antigo (se incluído), e os pontos de quadratura utilizados
#' \item `itens` vetor com os nomes dos itens que apresentaram DIF. Se os nomes nos bancos forem diferentes, os nomes do banco atual são apresentados
#' \item `maxajuste` `data.frame` com os itens que apresentaram desajuste pelo método MaxAjuste, com o nome do item, o ponto de quadratura em que a proporção superou `0.15` e as diferenças em cada categoria de resposta
#' \item `ajuste_rmsd` `data.frame` com os itens que apresentaram desajuste pelo método RMSD, com o nome do item e os valores de RMSD para cada categoria de resposta
#' \item `maxadif` `data.frame` com os itens que apresentaram DIF pelo método MaxADIF, com o nome do item, o ponto de quadratura em que a proporção superou `0.15` e as diferenças em cada categoria de resposta
#' \item `dif_rmsd` `data.frame` com os itens que apresentaram DIF pelo método RMSD, com o nome do item e os valores de RMSD para cada categoria de resposta
#' \item `regressao` lista com os itens que apresentaram DIF uniforme, não uniforme e misto com o método de regressão logística
#' }
#'
#' @author Alexandre Jaloto
#'
#'
#' @examples
#'
#' set.seed(1234)
#'
#' a <- rlnorm(60)
#' d <- rnorm(60)
#' data.atual <- data.frame(mirt::simdata(a, d, 1000, '2PL'))
#' data.antigo <- data.frame(mirt::simdata(a, d, 1000, '2PL'))
#'
#' names(data.antigo) <- c(paste0('IME_', 1:50), paste0('IRC_', 51:60))
#' names(data.atual) <- c(paste0('IME_', 61:85), paste0('IME_', 1:25), paste0('IRC_', 51:60))
#'
#' for(i in 51:60)
#' {data.antigo[,i] <- sample(3, 1000, TRUE)
#'  data.atual[,i] <- sample(3, 1000, TRUE)}
#' fit.antigo <- mirt::mirt(data.antigo, 1, TOL = .01)
#' fit.atual <- mirt::mirt(data.atual, 1, TOL = .01)
#' dif <- dif.mirt(fit.antigo = fit.antigo, fit.atual = fit.atual)
#'
#' # para itens com nomes diferentes
#' names(data.antigo) <- paste0('I', 1:ncol(data.antigo))
#'
#' comuns.antigo <- c(paste0('I', 1:25), paste0('I', 51:60))
#' comuns.atual <- c(paste0('IME_', 1:25), paste0('IRC_', 51:60))
#'
#' fit.antigo <- mirt::mirt(data.antigo, 1, TOL = .01)
#' fit.atual <- mirt::mirt(data.atual, 1, TOL = .01)
#'
#' dif <- dif.mirt(fit.antigo = fit.antigo, fit.atual = fit.atual, comuns.atual = comuns.atual, comuns.antigo = comuns.antigo)
#'
#' @export

dif.mirt <- function(fit.atual, fit.antigo = NULL,
                     comuns.atual = NULL, comuns.antigo = NULL,
                     int.teta = c(-6, 6), n.qdpt = 61)
{

  if(sum(is.null(comuns.antigo), is.null(comuns.atual)) == 1)
    stop('Se os nomes dos itens forem diferentes nos dois bancos, é preciso informar o nome dos itens comuns nos argumentos comuns.antigo e comuns.atual.')

  if(is.null(fit.antigo))
    warning('Como o grupo antigo não foi incluído, não será feita análise de DIF')

  # diferença entre cada ponto de quadratura
  diferenca.qdpt <- (int.teta[2] - int.teta[1])/(n.qdpt - 1)

  # caso o intervalo seja centrado no ponto de quadratura
  qdpt <- seq((int.teta[1] + diferenca.qdpt/2), (int.teta[2] - diferenca.qdpt/2), length.out = n.qdpt - 1)

  # caso o intervalo tenha início no ponto de quadratura
  # qdpt <- seq(int.teta[1], int.teta[2], length.out = n.qdpt)

  ### para o grupo atual

  # banco do grupo
  banco.atual <- data.frame(mirt::extract.mirt(fit.atual, 'data'))

  # itens
  itens.grupo.atual <- mirt::extract.mirt(fit.atual, 'itemnames')

  # teta do grupo
  banco.atual$teta <- mirt::fscores(fit.atual, mean = 0, cov = 1)

  # nível de cada sujeito
  banco.atual$cat <- cut(banco.atual$teta, c(-Inf, qdpt, Inf), right = TRUE)

  # p05 e p95 do grupo
  p05.atual <- quantile(banco.atual$teta, .05)
  p95.atual <- quantile(banco.atual$teta, .95)

  # parâmetros dos itens
  pars <- data.frame(mirt::coef(fit.atual, IRTpars = TRUE, simplify = TRUE)$items)

  # intervalo para análise do ajuste
  intervalo.analise <- c(p05.atual, p95.atual)

  # pontos de quadratura para análise. tem que somar diferenca.qdpt porque o objeto qdpt indica o ponto da direita do interavalo
  qdpt.analise <- qdpt > intervalo.analise[1] + diferenca.qdpt & qdpt < intervalo.analise[2]

  ### AJUSTE
  ajuste015 <- data.frame()
  ajustermsd <- data.frame()

  for(i in 1:length(itens.grupo.atual))
  {
    # i <- 1
    # i <- 31

    # porcentagem de cada categoria de resposta para cada ponto de quadratura em cada grupo
    item <- itens.grupo.atual[i]

    # proporções empíricas das categorias
    prop.atual <- prop.table(table(banco.atual$cat, banco.atual[[item]]), margin = 1)[qdpt.analise,]

    # calcular a proporção de sujeitos em cada ponto, mas somente daqueles que responderam o item (para RMSD)
    peso.atual <- prop.table(table(banco.atual[!is.na(banco.atual[[item]]),]$cat))[qdpt.analise]

    if (ncol(prop.atual) == 0)
      next

    # RMSD ajuste
    prob <- mirt::probtrace(mirt::extract.item(fit.atual, which(itens.grupo.atual == item)), qdpt[qdpt.analise])

    # calcular o peso para cada qdpt para o rmsd ajuste
    peso.ajuste <- peso.atual/(sum(peso.atual))

    # raiz do desvio quadrático médio (RMSD)
    ajustermsd.prov <- c()

    for(j in 2:ncol(prop.atual))
      ajustermsd.prov[j-1] <- data.frame(sqrt(weighted.mean(x = (prop.atual[,j]-prob[,j])^2, w = peso.ajuste)))

    ajustermsd.prov <- data.frame(ajustermsd.prov)

    names(ajustermsd.prov) <- paste0('P.', 1:(ncol(ajustermsd.prov)))

    ajustermsd.prov$item <- item

    ajustermsd <- data.table::rbindlist(
      list(
        ajustermsd,
        ajustermsd.prov
      ),
      fill = TRUE
    )

    # MaxAjuste

    ajuste015.prov <- as.data.frame.matrix(prop.atual-prob)
    ajuste015.prov <- data.frame(ajuste015.prov[,-1])

    ajuste015.prov$qdpt <- qdpt[qdpt.analise]

    ajuste015.prov <- ajuste015.prov[apply(data.frame(ajuste015.prov[,-ncol(ajuste015.prov)]), 1, function(x) any(abs(x) > .15)),]

    if (nrow(ajuste015.prov) != 0)
    {

      names(ajuste015.prov)[1:(ncol(ajuste015.prov)-1)] <- paste0('P.', 1:(ncol(ajuste015.prov)-1))

      ajuste015.prov$item <- item

      ajuste015 <- data.table::rbindlist(
        list(
          ajuste015,
          ajuste015.prov
        ),
        fill = TRUE
      )
    }

  }

  if(!is.null(fit.antigo))
  {
    # banco do grupo
    banco.antigo <- data.frame(mirt::extract.mirt(fit.antigo, 'data'))

    # verificar itens comuns

    itens.grupo.antigo <- mirt::extract.mirt(fit.antigo, 'itemnames')

    # caso o objeto mirt venha de um objeto de grupos múltiplos originalmente, os itens não comuns serão somente NA para o grupo que não respondeu esse item. esses itens devem sair desse grupo
    itens.grupo.atual <- itens.grupo.atual[apply(banco.atual, 2, function(x) sum(!is.na(x))) > 0]
    itens.grupo.antigo <- itens.grupo.antigo[apply(banco.antigo, 2, function(x) sum(!is.na(x))) > 0]

    # se os nomes dos itens são iguais nos dois bancos
    if(is.null(comuns.antigo))
    {
      itens.comuns <- itens.grupo.atual[which(itens.grupo.atual %in% itens.grupo.antigo)]

      # se os nomes dos itens são diferentes nos bancos
    } else {
      # Iterar sobre as listas de itens comuns
      for (i in seq_along(comuns.antigo)) {
        # Encontrar os índices dos itens comuns no banco.antigo
        indice_antigo <- which(names(banco.antigo) == comuns.antigo[i])
        # Encontrar os índices correspondentes no banco.atual
        indice_atual <- which(names(banco.atual) == comuns.atual[i])

        # Renomear os itens no banco.antigo para os nomes do banco.atual
        if (length(indice_antigo) > 0 && length(indice_atual) > 0) {
          names(banco.antigo)[indice_antigo] <- names(banco.atual)[indice_atual]
        }
      }

      itens.comuns <- comuns.atual
    }

    ## selecionar somente os pontos de quadratura entre o p05 e o p95 (qdpt) ----

    # teta do grupo
    banco.antigo$teta <- mirt::fscores(fit.antigo, mean = 0, cov = 1)

    # nível de cada sujeito
    banco.antigo$cat <- cut(banco.antigo$teta, c(-Inf, qdpt, Inf), right = TRUE)

    # p05 e p95 do grupo
    p05.antigo <- quantile(banco.antigo$teta, .05)
    p95.antigo <- quantile(banco.antigo$teta, .95)

    # intervalo para análise do DIF
    intervalo.analise <- c(max(p05.antigo, p05.atual), min(p95.antigo, p95.atual))

    # pontos de quadratura para análise. tem que somar diferenca.qdpt porque o objeto qdpt indica o ponto da direita do interavalo
    qdpt.analise <- qdpt > intervalo.analise[1] + diferenca.qdpt & qdpt < intervalo.analise[2]

    # parâmetros dos itens comuns
    pars <- pars[itens.comuns,]

    # DIF 015 e RMSD
    dif015 <- data.frame()
    difrmsd <- data.frame()

    for(i in 1:length(itens.comuns))
    {
      # i <- 1
      # i <- 31

      ## porcentagem de cada categoria de resposta para cada ponto de quadratura em cada grupo ----
      item <- itens.comuns[i]

      # proporções empíricas das categorias
      prop.atual <- prop.table(table(banco.atual$cat, banco.atual[[item]]), margin = 1)[qdpt.analise,]
      prop.antigo <- prop.table(table(banco.antigo$cat, banco.antigo[[item]]), margin = 1)[qdpt.analise,]

      # calcular a proporção de sujeitos em cada ponto, mas somente daqueles que responderam o item (para RMSD)
      peso.atual <- prop.table(table(banco.atual[!is.na(banco.atual[[item]]),]$cat))[qdpt.analise]
      peso.antigo <- prop.table(table(banco.antigo[!is.na(banco.antigo[[item]]),]$cat))[qdpt.analise]

      # calcular o peso para cada qdpt
      peso <- peso.atual*peso.antigo/(sum(peso.atual*peso.antigo))

      if (ncol(prop.atual) == 0 | ncol(prop.antigo) == 0)
        next

      # raiz do desvio quadrático médio (RMSD)
      rmsd.prov <- c()

      for(j in 2:ncol(prop.atual))
        rmsd.prov[j-1] <- data.frame(sqrt(weighted.mean(x = (prop.atual[,j]-prop.antigo[,j])^2, w = peso)))

      rmsd.prov <- data.frame(rmsd.prov)

      names(rmsd.prov) <- paste0('P.', 1:(ncol(rmsd.prov)))

      rmsd.prov$item <- item

      difrmsd <- data.table::rbindlist(
        list(
          difrmsd,
          rmsd.prov
        ),
        fill = TRUE
      )

      # MaxADIF

      dif015.prov <- as.data.frame.matrix(prop.atual-prop.antigo)
      dif015.prov <- data.frame(dif015.prov[,-1])

      dif015.prov$qdpt <- qdpt[qdpt.analise]

      dif015.prov <- dif015.prov[apply(data.frame(dif015.prov[,-ncol(dif015.prov)]), 1, function(x) any(abs(x) > .15)),]

      if (nrow(dif015.prov) != 0)
      {

        names(dif015.prov)[1:(ncol(dif015.prov)-1)] <- paste0('P.', 1:(ncol(dif015.prov)-1))

        dif015.prov$item <- item

        dif015 <- data.table::rbindlist(
          list(
            dif015,
            dif015.prov
          ),
          fill = TRUE
        )
      }

    }

    ## regressão logística ----

    dif.log <- list()
    itens.dif.log <- c()

    dados <- data.frame(data.table::rbindlist(list(banco.antigo, banco.atual), fill = TRUE))
    dados <- dados[,itens.comuns]

    grupos <- c(
      rep('G1', nrow(banco.antigo)),
      rep('G2', nrow(banco.atual))
    )

    tetas <- c(banco.antigo$teta, banco.atual$teta)

    dif.log <- lordif::rundif(item = itens.comuns,
                              resp = dados,
                              theta = tetas, gr = grupos,
                              criterion = 'CHISQR', alpha = .05, wt = NULL)

    dif.log <- list(
      uniforme = itens.comuns[dif.log$stats$chi12 < .05 & dif.log$stats$pseudo12.Nagelkerke > .035],
      nao_uniforme = itens.comuns[dif.log$stats$chi13 < .05 & dif.log$stats$pseudo13.Nagelkerke > .035],
      misto = itens.comuns[dif.log$stats$chi23 < .05 & dif.log$stats$pseudo23.Nagelkerke > .035]
    )

    itens.dif.log <- unique(c(itens.dif.log, dif.log$uniforme, dif.log$nao_uniforme, dif.log$misto))

  }

  ## se tiver item com DIF ou desajuste ----

  if(!is.null(fit.antigo))
  {
    if(nrow(dif015) > 1)
      dif015 <- dplyr::select(dif015, item, qdpt, dplyr::everything())

    if(nrow(difrmsd) > 1)
    {
      difrmsd <- dplyr::select(difrmsd, item, dplyr::everything())

      difrmsd <- difrmsd[apply(data.frame(difrmsd[,-1]), 1, function(x) any(x > .12, na.rm = TRUE)),]
    }
  }

  if(nrow(ajuste015) > 1)
    ajuste015 <- dplyr::select(ajuste015, item, qdpt, dplyr::everything())

  if(nrow(ajustermsd) > 1)
  {
    ajustermsd <- dplyr::select(ajustermsd, item, dplyr::everything())

    ajustermsd <- ajustermsd[apply(data.frame(ajustermsd[,-1]), 1, function(x) any(x > .12, na.rm = TRUE)),]
  }

  ## finalizar a função ----

  if(!is.null(fit.antigo))
  {
    itens <- c(dif015$item, difrmsd$item, ajuste015$item, ajustermsd$item, itens.dif.log)

    dif <- list(
      info = list(p.antigo = c(p05.antigo, p95.antigo),
                  p.atual = c(p05.atual, p95.atual),
                  qdpt.analise = qdpt[qdpt.analise]),
      itens = itens,
      maxajuste = data.frame(ajuste015),
      ajuste_rmsd = data.frame(ajustermsd),
      maxadif = data.frame(dif015),
      dif_rmsd = data.frame(difrmsd),
      regressao = dif.log
    )
  } else {
    itens <- c(ajuste015$item, ajustermsd$item)

    dif <- list(
      info = list(p.atual = c(p05.atual, p95.atual),
                  qdpt.analise = qdpt[qdpt.analise]),
      itens = itens,
      maxajuste = data.frame(ajuste015),
      ajuste_rmsd = data.frame(ajustermsd)
    )
  }

  return(dif)

}
