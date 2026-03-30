#' @title Comparar a tabela gerada com a simulada
#' @name compara.tabelas
#'
#' @description Compara uma tabela gerada pelo usuário com a tabela oficial gerada com dados simulados
#'
#' @param objeto.insumo tabela gerada pelo usuário
#' @param objeto.sim tabela oficial gerada com dados simulados
#'
#' @return Vetor lógico que indica existência de diferença (`TRUE`) ou ausência (`FALSE`).
#'
#' @author Alexandre Jaloto
#'
#'
compara.tabelas <- function(objeto.insumo, objeto.sim)
{
  problema <- FALSE

  for (i in 1:ncol(objeto.sim))
  {
    if(class(objeto.sim[,i]) %in% c('integer', 'numeric'))
    {

      # Tratar NaN como um caso especial
      pars_nan <- is.nan(objeto.insumo[, i])
      sim_nan <- is.nan(objeto.sim[, i])

      # Verificar se os NaN correspondem
      if(any(pars_nan != sim_nan, na.rm = TRUE)) {
        problema <- TRUE
        print(paste0(names(objeto.sim)[i], ": diferença em NaN"))
        next
      }

      # Verificar se os infinitos correspondem
      pars_inf_pos <- objeto.insumo[, i] == Inf
      pars_inf_neg <- objeto.insumo[, i] == -Inf
      sim_inf_pos <- objeto.sim[, i] == Inf
      sim_inf_neg <- objeto.sim[, i] == -Inf

      if(any(pars_inf_pos != sim_inf_pos, na.rm = TRUE) ||
         any(pars_inf_neg != sim_inf_neg, na.rm = TRUE))
      {
        problema <- TRUE
        print(paste0(names(objeto.sim)[i], ": diferença em valores infinitos"))
        next
      }


      # Para os valores finitos (não Inf, não NaN), comparar normalmente
      finitos <- is.finite(objeto.insumo[, i]) & is.finite(objeto.sim[, i])
      if(any(finitos, na.rm = TRUE)) {
        diferenca <- max(abs(objeto.insumo[finitos, i] - objeto.sim[finitos, i]), na.rm = TRUE)
        print(paste0(names(objeto.sim)[i], ": máxima diferença = ", diferenca))
        if(diferenca != 0)
          problema <- TRUE
      } else if(all(pars_nan & sim_nan) || all(is.infinite(objeto.insumo[, i]) & is.infinite(objeto.sim[, i]))) {
        print(paste0(names(objeto.sim)[i], ": todos os valores são NaN ou Inf e iguais"))
      } else {
        print(paste0(names(objeto.sim)[i], ": todos os valores são especiais (NaN/Inf) e iguais"))
      }


    } else {
      diferenca <- all.equal(objeto.insumo[, i], objeto.sim[, i])
      if(any(diferenca != TRUE))
        problema <- TRUE
      diferenca <- ifelse(any(diferenca != TRUE), 'diferente', 'igual')
      print(paste0(names(objeto.sim)[i], ": ", diferenca))
    }
  }

  problema
}
