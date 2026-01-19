#' @title Análise de ajuste e DIF em um objeto mirt
#' @name dif.mirt
#'
#' @description Verifica existência de DIF e a qualidade do ajuste de um item calibrado no mirt
#'
#' @param fit.atual objeto mirt do tipo `SingleGroupClass` referente ao grupo da aplicação em voga (grupo focal).
#' @param fit.antigo objeto mirt do tipo `SingleGroupClass` referente ao grupo em que os itens foram calibrados (grupo de referência).
#' @param comuns.atual vetor com nomes dos itens comuns no banco atual. Os nomes dos itens podem ser obtidos com `mirt::extract.mirt(fit.atual, 'itemnames')`. Só é necessário se os nomes dos itens forem diferentes nos dois bancos.
#' @param comuns.antigo vetor com nomes dos itens comuns no banco antigo. Os nomes dos itens podem ser obtidos com `mirt::extract.mirt(fit.antigo, 'itemnames')`. Só é necessário se os nomes dos itens forem diferentes nos dois bancos.
#' @param int.teta intervalo dos pontos de quadratura. Esse intervalo será usado para a análise de MaxADif. O padrão é `c(-6, 6)`.
#' @param n.qdpt quantidade de pontos de quadratura.
#' @param int.cent lógico. Se TRUE, o intervalo dos pontos de quadratura é centrado. Se FALSE, o intervalo começa no ponto de quadratura. O padrão é TRUE.
#' @param limite.max.dif limite superior para as diferenças absolutas em MaxADIF. Itens com diferenças maiores que este valor são sinalizados. O padrão é `0.15`.
#' @param limite.rmsd limite superior para o RMSD (ajuste e DIF). Itens com RMSD maior que este valor são sinalizados. O padrão é `0.10`.
#' @param limite.rmsd.pisa limite superior para o RMSD PISA. Itens com RMSD maior que este valor são sinalizados. O padrão é `0.10`.
#' @param limite.pseudoR2.lordif limite superior para o pseudo R2 de Nagelkerke na análise de regressão logística (lordif). O padrão é `0.035`.
#'
#' @return A função retorna uma lista com quatro elementos
#' \itemize{
#' \item `info` lista com informação usada nos cálculos
#' \itemize{
#' \item `p.antigo.dif` percentis do grupo antigo para análise de DIF
#' \item `p.atual.dif` percentis do grupo atual para análise de DIF
#' \item `p.ajuste` percentis do grupo atual para análise de ajuste
#' \item `qdpt.analise.dif` pontos de quadratura utilizados na análise de DIF
#' }
#' \item `itens` itens que apresentaram problema. Se os nomes forem diferentes nos bancos atual e antigo, os nomes do banco atual são apresentados
#' \item `rmsd.ajuste` itens que apresentaram desajuste pelo método RMSD, com o nome do item e os valores de RMSD para cada categoria de resposta
#' \item `rmsd.pisa` itens que apresentaram desajuste pelo método RMSD do PISA, com o nome do item e os valores de RMSD para cada categoria de resposta
#' \item `maxadif` itens que apresentaram DIF pelo método MaxADIF, com o nome do item, o ponto de quadratura em que a proporção superou `limite.max.dif` e as diferenças em cada categoria de resposta
#' \item `rmsd.dif` itens que apresentaram DIF pelo método RMSD, com o nome do item e os valores de RMSD para cada categoria de resposta
#' \item `regressao` itens que apresentaram DIF uniforme, não uniforme e misto com o método de regressão logística
#' }
#'
#' @author Alexandre Jaloto
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
                     int.teta = c(-6, 6), n.qdpt = 61, int.cent = TRUE,
                     limite.max.dif = 0.15,
                     limite.rmsd = 0.10,
                     limite.rmsd.pisa = 0.10,
                     p.ajuste = c(0.05, 0.95),
                     p.dif = c(0.05, 0.95),
                     limite.pseudoR2.lordif = 0.035)
{

  # comuns.atual = NULL
  # comuns.antigo = NULL
  # int.teta = c(-6, 6)
  # n.qdpt = 61
  # int.cent = TRUE
  # limite.max.dif = 0.15
  # limite.rmsd = 0.12
  # p.ajuste = c(0.05, 0.95)
  # p.dif = c(0.05, 0.95)
  # limite.pseudoR2.lordif = 0.035


  # --- Funções Auxiliares (Definidas internamente para escopo local) ---

  # Função auxiliar para calcular RMSD de forma vetorizada para as categorias
  calcular_rmsd_item <- function(prop_observada, prop_comparacao, peso_qdpt) {
    if (ncol(prop_observada) != ncol(prop_comparacao)) {
      stop("As proporções para RMSD devem ter o mesmo número de categorias.")
    }
    # Aplica a função para cada coluna (categoria), vetorizando o cálculo
    # Usamos lapply e t() para garantir que o resultado seja um data.frame de 1 linha por item
    rmsd_valores <- lapply(2:ncol(prop_observada), function(j) {
      sqrt(weighted.mean(x = (prop_observada[,j] - prop_comparacao[,j])^2, w = peso_qdpt, na.rm = TRUE))
    })
    rmsd_df <- as.data.frame(t(unlist(rmsd_valores))) # Transforma a lista de resultados em um data.frame
    names(rmsd_df) <- paste0('P.', 1:length(rmsd_valores)) # Nomeia as colunas
    return(rmsd_df)
  }

  # Função auxiliar para calcular MaxADIF
  calcular_max_item <- function(prop_observada, prop_comparacao, qdpt_valores, limite_max_param) {
    # Calcula a matriz de diferenças entre as proporções de forma vetorizada
    diferencas_matrix <- as.data.frame.matrix(prop_observada - prop_comparacao)

    # Remove a primeira coluna (assumindo que é a categoria 0 ou a primeira opção)
    # Esta operação é vetorizada
    diferencas_filtradas <- diferencas_matrix[,-1, drop = FALSE] # Usar drop=FALSE para manter como DF se só 1 coluna sobrar

    # Se não houver colunas de dados para analisar (apenas qdpt ou está vazio), retorna um data.frame vazio
    if (ncol(diferencas_filtradas) == 0) {
      return(data.frame())
    }

    # Adiciona os pontos de quadratura como uma coluna
    diferencas_filtradas$qdpt <- qdpt_valores

    # Filtra as linhas onde qualquer diferença absoluta excede o limite
    # O apply com any(abs(x) > limite_max_param) já é uma forma vetorizada para verificar as linhas
    max_df_result <- diferencas_filtradas[apply(diferencas_filtradas[, -ncol(diferencas_filtradas), drop = FALSE], 1, function(x) any(abs(x) > limite_max_param, na.rm = TRUE)),, drop = FALSE]

    if (nrow(max_df_result) > 0) { # Apenas renomeia se houver linhas
      names(max_df_result)[1:(ncol(max_df_result)-1)] <- paste0('P.', 1:(ncol(max_df_result)-1))
    }

    return(max_df_result)
  }

  rmsd.pisa <- function (calib, flag = 0, probfun = TRUE, dentype = "norm")
  {

    dat <- mirt::extract.mirt(calib, "data")
    ret <- data.frame()

    nfact <- mirt::extract.mirt(calib, "nfact")
    stopifnot(nfact == 1L)
    sv <- mirt::mod2values(calib)
    sv$est <- FALSE
    Theta <- matrix(seq(-6, 6, length.out = 61))
    mod_g <- mirt::mirt(dat, nfact,
                        itemtype = mirt::extract.mirt(calib, "itemtype"),
                        pars = sv,
                        technical = list(storeEtable = TRUE, customTheta = Theta,
                                         customK = mirt::extract.mirt(calib, "K")))
    Etable <- mod_g@Internals$Etable[[1]]$r1
    if (dentype %in% c("norm", "snorm")) {
      mu <- sv$value[sv$name == "MEAN_1"]
      sigma2 <- sv$value[sv$name == "COV_11"]
      if (dentype == "snorm") {
        mu <- 0
        sigma2 <- 1
      }
      f_theta <- dnorm(Theta, mean = mu, sd = sqrt(sigma2))
      f_theta <- as.vector(f_theta/sum(f_theta))
    } else if (dentype == "empirical") {
      f_theta <- rowSums(Etable)/sum(Etable)
    } else stop("dentype not supported", call. = FALSE)

    itemloc <- mirt::extract.mirt(mod_g, "itemloc")
    which.items <- 1L:ncol(dat)
    ret2 <- vector("list", ncol(dat))
    names(ret2) <- mirt::extract.mirt(mod_g, "itemnames")
    for (i in seq_len(length(which.items))) {

      pick <- itemloc[which.items[i]]:(itemloc[which.items[i] +
                                                 1L] - 1L)
      O <- Etable[, pick]
      P_o <- O/rowSums(O)
      item <- mirt::extract.item(calib, which.items[i])
      P_e <- mirt::probtrace(item, Theta)
      ret2[[i]] <- if (probfun) {
        sqrt(colSums((P_o - P_e)^2 * f_theta))
      }
      else {
        S <- 1L:ncol(P_o) - 1L
        c(`S(theta)` = sqrt(sum((colSums(S * t(P_o -
                                                 P_e)))^2 * f_theta)))
      }
    }
    nms <- lapply(ret2, names)
    unms <- unique(do.call(c, nms))
    items <- matrix(NA, length(ret2), length(unms))
    rownames(items) <- names(ret2)
    colnames(items) <- unms
    for (i in seq_len(nrow(items))) items[i, nms[[i]]] <- ret2[[i]]
    if (flag > 0)
      items[items < flag] <- NA
    items[is.nan(items)] <- NA
    items <- as.data.frame(items)
    items <- mirt:::as.mirt_df(items)
    items
  }


  # --- Início do Código Principal da Função ---

  # 1. Validação inicial de parâmetros
  if(sum(is.null(comuns.antigo), is.null(comuns.atual)) == 1) {
    stop('Se os nomes dos itens forem diferentes nos dois bancos, é preciso informar o nome dos itens comuns nos argumentos comuns.antigo e comuns.atual.')
  }

  if(is.null(fit.antigo)) {
    warning('Como o grupo antigo não foi incluído, não será feita análise de DIF.')
  }

  # 2. Configuração dos Pontos de Quadratura (QDPT)
  diferenca.qdpt <- (int.teta[2] - int.teta[1]) / (n.qdpt - 1)

  if(int.cent) {
    qdpt <- seq(int.teta[1] + diferenca.qdpt/2, int.teta[2] - diferenca.qdpt/2, length.out = n.qdpt - 1)
  } else {
    qdpt <- seq(int.teta[1], int.teta[2], length.out = n.qdpt)
  }

  # 3. Processamento do Grupo Atual (Focal)
  # Extração de dados de forma eficiente
  banco.atual <- data.frame(mirt::extract.mirt(fit.atual, 'data'))
  itens.grupo.atual <- mirt::extract.mirt(fit.atual, 'itemnames')

  # Cálculo do teta e categorização dos sujeitos (operações vetorizadas)
  banco.atual$teta <- mirt::fscores(fit.atual, mean = 0, cov = 1)
  banco.atual$cat <- cut(banco.atual$teta, c(-Inf, qdpt, Inf), right = TRUE)

  # Quantis do teta
  p.menor.ajuste <- quantile(banco.atual$teta, p.ajuste[1])
  p.maior.ajuste <- quantile(banco.atual$teta, p.ajuste[2])

  # Intervalo de análise para ajuste
  intervalo.analise.ajuste <- c(p.menor.ajuste, p.maior.ajuste)
  qdpt.analise.ajuste <- qdpt[qdpt > intervalo.analise.ajuste[1] + diferenca.qdpt & qdpt < intervalo.analise.ajuste[2]]

  # 4. Análise de Ajuste (apenas RMSD de Ajuste agora)
  ajustermsd_results <- list()
  item_idx_ajuste <- 1 # Contador para os resultados válidos

  for(i in 1:length(itens.grupo.atual)) {
    item <- itens.grupo.atual[i]

    # Proporções empíricas das categorias (já é vetorizado pela `table` e `prop.table`)
    # Filtrar por `qdpt.analise.ajuste` para garantir que apenas os pontos relevantes sejam usados
    prop.atual <- prop.table(table(banco.atual$cat, banco.atual[[item]]), margin = 1)[qdpt > intervalo.analise.ajuste[1] + diferenca.qdpt & qdpt < intervalo.analise.ajuste[2],, drop = FALSE]

    # Calcular peso para RMSD de ajuste (vetorizado para `prop.table`)
    # Apenas para sujeitos que responderam ao item
    peso.atual <- prop.table(table(banco.atual[!is.na(banco.atual[[item]]),]$cat))[qdpt > intervalo.analise.ajuste[1] + diferenca.qdpt & qdpt < intervalo.analise.ajuste[2]]
    peso.ajuste <- peso.atual / sum(peso.atual, na.rm = TRUE) # Usar na.rm = TRUE para robustez

    if (ncol(prop.atual) == 0 || nrow(prop.atual) == 0 || sum(peso.ajuste, na.rm = TRUE) == 0) {
      next # Pular item se não houver dados válidos nas categorias ou pesos
    }

    # Proporções teóricas
    prob <- mirt::probtrace(mirt::extract.item(fit.atual, which(itens.grupo.atual == item)), qdpt.analise.ajuste)

    # Chamada da função auxiliar vetorizada para RMSD de Ajuste
    current_ajustermsd <- calcular_rmsd_item(prop_observada = prop.atual, prop_comparacao = prob, peso_qdpt = peso.ajuste)

    current_ajustermsd$item <- item
    ajustermsd_results[[item_idx_ajuste]] <- current_ajustermsd

    item_idx_ajuste <- item_idx_ajuste + 1
  }

  # Combinar resultados (uso de data.table::rbindlist é eficiente)
  ajustermsd_final <- if (length(ajustermsd_results) > 0) data.table::rbindlist(ajustermsd_results, fill = TRUE) else data.frame()

  # Reordenar colunas e filtrar por limite final
  if (nrow(ajustermsd_final) > 0) {
    ajustermsd_final <- dplyr::select(ajustermsd_final, item, dplyr::everything())
    ajustermsd_final <- ajustermsd_final[apply(ajustermsd_final[, -1, drop = FALSE], 1, function(x) any(x > limite.rmsd, na.rm = TRUE)),, drop = FALSE]
  }

  # 5. Processamento do Grupo Antigo (Referência) e Análise de DIF (se fit.antigo for fornecido)
  if(!is.null(fit.antigo)) {
    banco.antigo <- data.frame(mirt::extract.mirt(fit.antigo, 'data'))
    itens.grupo.antigo <- mirt::extract.mirt(fit.antigo, 'itemnames')

    # Filtra itens que são NA em todos os respondentes (otimização)
    itens.grupo.atual <- itens.grupo.atual[apply(banco.atual[, itens.grupo.atual, drop = FALSE], 2, function(x) sum(!is.na(x))) > 0]
    itens.grupo.antigo <- itens.grupo.antigo[apply(banco.antigo[, itens.grupo.antigo, drop = FALSE], 2, function(x) sum(!is.na(x))) > 0]

    # Identificar itens comuns e harmonizar nomes
    if(is.null(comuns.antigo)) {
      itens.comuns <- intersect(itens.grupo.atual, itens.grupo.antigo)
    } else {
      # Mapeamento e renomeação dos itens comuns (se nomes forem diferentes)
      # Usando `match` para vetorizar o processo de renomeação quando possível
      idx_antigo_map <- match(comuns.antigo, names(banco.antigo))
      idx_atual_map <- match(comuns.atual, names(banco.atual))

      valid_map <- !is.na(idx_antigo_map) & !is.na(idx_atual_map)
      names(banco.antigo)[idx_antigo_map[valid_map]] <- names(banco.atual)[idx_atual_map[valid_map]]
      itens.comuns <- comuns.atual[valid_map] # Garantir que apenas itens mapeados sejam considerados comuns
    }

    # Teta e categorização para banco.antigo (operações vetorizadas)
    banco.antigo$teta <- mirt::fscores(fit.antigo, mean = 0, cov = 1)
    banco.antigo$cat <- cut(banco.antigo$teta, c(-Inf, qdpt, Inf), right = TRUE)

    # Quantis do teta
    p.menor.dif.antigo <- quantile(banco.antigo$teta, p.dif[1])
    p.maior.dif.antigo <- quantile(banco.antigo$teta, p.dif[2])

    p.menor.dif.atual <- quantile(banco.atual$teta, p.dif[1])
    p.maior.dif.atual <- quantile(banco.atual$teta, p.dif[2])

    # Intervalo de análise para DIF (interseção dos intervalos dos grupos)
    intervalo.analise.dif <- c(max(p.menor.dif.antigo, p.menor.dif.atual),
                               min(p.maior.dif.antigo, p.maior.dif.atual))
    qdpt.analise.dif <- qdpt[qdpt > intervalo.analise.dif[1] + diferenca.qdpt & qdpt < intervalo.analise.dif[2]]

    # Análise de DIF (MaxADIF e RMSD de DIF)
    maxdif_results <- list()
    difrmsd_results <- list()
    item_idx_dif <- 1

    for(item in itens.comuns) { # Iterar apenas sobre os itens comuns
      # Proporções empíricas
      prop.atual.raw <- prop.table(table(banco.atual$cat, banco.atual[[item]]), margin = 1)[qdpt > intervalo.analise.dif[1] + diferenca.qdpt & qdpt < intervalo.analise.dif[2], , drop = FALSE]
      prop.antigo.raw <- prop.table(table(banco.antigo$cat, banco.antigo[[item]]), margin = 1)[qdpt > intervalo.analise.dif[1] + diferenca.qdpt & qdpt < intervalo.analise.dif[2], , drop = FALSE]

      # Identificar todas as categorias de resposta presentes em ambos os grupos para este item
      all_cats <- union(colnames(prop.atual.raw), colnames(prop.antigo.raw))

      # Garantir que ambas as tabelas tenham todas as categorias, preenchendo com 0 se ausente
      prop.atual <- matrix(0, nrow = nrow(prop.atual.raw), ncol = length(all_cats), dimnames = list(rownames(prop.atual.raw), all_cats))
      prop.atual[, colnames(prop.atual.raw)] <- prop.atual.raw

      prop.antigo <- matrix(0, nrow = nrow(prop.antigo.raw), ncol = length(all_cats), dimnames = list(rownames(prop.antigo.raw), all_cats))
      prop.antigo[, colnames(prop.antigo.raw)] <- prop.antigo.raw

      # Reordenar as colunas para garantir que estejam na mesma ordem
      prop.atual <- prop.atual[, order(as.numeric(gsub("V", "", colnames(prop.atual)))), drop = FALSE] # assume V1, V2, etc. ou 0, 1, 2
      prop.antigo <- prop.antigo[, order(as.numeric(gsub("V", "", colnames(prop.antigo)))), drop = FALSE] # assume V1, V2, etc. ou 0, 1, 2

      # Pesos para RMSD de DIF (apenas para respondentes válidos)
      peso.atual.item <- prop.table(table(banco.atual[!is.na(banco.atual[[item]]),]$cat))[qdpt > intervalo.analise.dif[1] + diferenca.qdpt & qdpt < intervalo.analise.dif[2]]
      peso.antigo.item <- prop.table(table(banco.antigo[!is.na(banco.antigo[[item]]),]$cat))[qdpt > intervalo.analise.dif[1] + diferenca.qdpt & qdpt < intervalo.analise.dif[2]]

      # Verificação de colunas e linhas válidas antes do cálculo de peso
      if (ncol(prop.atual) == 0 || nrow(prop.atual) == 0 ||
          ncol(prop.antigo) == 0 || nrow(prop.antigo) == 0 ||
          sum(peso.atual.item, na.rm = TRUE) == 0 || sum(peso.antigo.item, na.rm = TRUE) == 0) {
        next # Pular item se não houver dados válidos nas categorias ou pesos
      }

      # Cálculo do peso combinado
      peso <- (peso.atual.item * peso.antigo.item) / sum(peso.atual.item * peso.antigo.item, na.rm = TRUE)

      # Chamada das funções auxiliares vetorizadas
      current_difrmsd <- calcular_rmsd_item(prop_observada = prop.atual, prop_comparacao = prop.antigo, peso_qdpt = peso)
      current_maxdif <- calcular_max_item(prop_observada = prop.atual, prop_comparacao = prop.antigo, qdpt_valores = qdpt.analise.dif, limite_max_param = limite.max.dif)

      current_difrmsd$item <- item
      difrmsd_results[[item_idx_dif]] <- current_difrmsd

      if (nrow(current_maxdif) > 0) {
        current_maxdif$item <- item
        maxdif_results[[item_idx_dif]] <- current_maxdif
      }
      item_idx_dif <- item_idx_dif + 1
    }

    # Combinar resultados
    maxdif_final <- if (length(maxdif_results) > 0) data.table::rbindlist(maxdif_results, fill = TRUE) else data.frame()
    difrmsd_final <- if (length(difrmsd_results) > 0) data.table::rbindlist(difrmsd_results, fill = TRUE) else data.frame()

    # Reordenar colunas e filtrar por limite final
    if (nrow(maxdif_final) > 0) {
      maxdif_final <- dplyr::select(maxdif_final, item, qdpt, dplyr::everything())
    }
    if (nrow(difrmsd_final) > 0) {
      difrmsd_final <- dplyr::select(difrmsd_final, item, dplyr::everything())
      difrmsd_final <- difrmsd_final[apply(difrmsd_final[, -1, drop = FALSE], 1, function(x) any(x > limite.rmsd, na.rm = TRUE)),, drop = FALSE]
    }

    # 6. Regressão Logística (Lordif) - Implementação Original (com limite.pseudoR2.lordif)
    itens.dif.log <- c() # Inicializa vazio

    # Prepara dados para lordif (otimizado com drop=FALSE)
    dados <- data.table::rbindlist(list(banco.antigo, banco.atual), fill = TRUE)
    dados <- dados[, ..itens.comuns, drop = FALSE] # Correção: usando '..' para selecionar colunas por vetor

    grupos <- c(
      rep('G1', nrow(banco.antigo)),
      rep('G2', nrow(banco.atual))
    )

    tetas <- c(banco.antigo$teta, banco.atual$teta)

    dif.log.raw <- lordif::rundif(item = itens.comuns,
                                  resp = dados,
                                  theta = tetas, gr = grupos,
                                  criterion = 'CHISQR', alpha = .05, wt = NULL)

    # Filtragem com o novo parâmetro
    dif.log <- list(
      uniforme = itens.comuns[dif.log.raw$stats$chi12 < .05 & dif.log.raw$stats$pseudo12.Nagelkerke > limite.pseudoR2.lordif],
      nao_uniforme = itens.comuns[dif.log.raw$stats$chi13 < .05 & dif.log.raw$stats$pseudo13.Nagelkerke > limite.pseudoR2.lordif],
      misto = itens.comuns[dif.log.raw$stats$chi23 < .05 & dif.log.raw$stats$pseudo23.Nagelkerke > limite.pseudoR2.lordif]
    )

    itens.dif.log <- unique(c(itens.dif.log, dif.log$uniforme, dif.log$nao_uniforme, dif.log$misto))
  }

  # RMSD PISA ----

  rmsd.atual <- rmsd.pisa(fit.atual, flag = 0, probfun = TRUE, dentype = "norm")

  rmsd.atual <- data.frame(rmsd.atual[rowSums(rmsd.atual > limite.rmsd.pisa, na.rm = TRUE) > 0, ])

  # 7. Coleta Final de Itens com DIF/Desajuste e Estrutura de Retorno
  final_itens_com_problema <- ajustermsd_final$item # Inicia apenas com ajuste_rmsd

  if(!is.null(fit.antigo)) {
    final_itens_com_problema <- unique(c(final_itens_com_problema, rownames(rmsd.atual), maxdif_final$item, difrmsd_final$item, itens.dif.log))
  } else {
    final_itens_com_problema <- unique(final_itens_com_problema, rownames(rmsd.atual)) # Garantir que seja único mesmo sem DIF
  }

  if(!is.null(fit.antigo)) {
    dif_output <- list(
      info = list(p.antigo.dif = c(p.menor.dif.antigo, p.maior.dif.antigo),
                  p.atual.dif = c(p.menor.dif.atual, p.maior.dif.atual),
                  p.ajuste = c(p.menor.ajuste, p.maior.ajuste),
                  qdpt.analise.dif = qdpt.analise.dif), # Usar qdpt.analise.dif para o DIF
      itens = final_itens_com_problema,
      rmsd.ajuste = data.frame(ajustermsd_final),
      rmsd.pisa = rmsd.atual,
      maxadif = data.frame(maxdif_final),
      rmsd.dif = data.frame(difrmsd_final),
      regressao = dif.log
    )
  } else {
    dif_output <- list(
      info = list(p.ajuste = c(p.menor.ajuste, p.maior.ajuste),
                  qdpt.analise = qdpt.analise.ajuste), # Usar qdpt.analise.ajuste se não tiver DIF
      itens = final_itens_com_problema,
      rmsd.ajuste = data.frame(ajustermsd_final),
      rmsd.pisa = rmsd.atual
    )
  }

  return(dif_output)
}


