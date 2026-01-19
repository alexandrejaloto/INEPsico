#' @title Calcular normit
#' @name calcular_normit
#'
#' @description Calcula o valor de normit para os sujeitos quando eles
#' são submetidos a formas diferentes de um teste
#'
#' @param banco.aberto banco aberto obtido em `abre.banco`
#' @param tot.cad máximo de cadernos
#' @param escore escore dos sujeitos
#'
#' @return Vetor numérico com os valores de normit dos sujeitos
#'
#' @author Alexandre Jaloto
#' @keywords internal

calcular_normit <- function(banco.aberto, tot.cad, escore) {
  normit <- escore
  for (j in 1:tot.cad) {
    caderno_j <- banco.aberto[banco.aberto$CADERNO == j, ]
    escores <- caderno_j$ACERTOS
    n <- length(escores)

    # Tratar escores 0 e 1 separadamente
    escore_0 <- sum(escores == 0)
    escore_1 <- sum(escores <= 1)
    normit_0 <- qnorm((escore_0 + escore_1 / 2) / (2 * n))
    normit_1 <- qnorm((escore_0 + escore_1) / (2 * n))
    normit[banco.aberto$CADERNO == j & escore == 0] <- normit_0
    normit[banco.aberto$CADERNO == j & escore == 1] <- normit_1

    # Tratar os demais escores
    escores_unicos <- unique(escores[escores > 1])
    for (i in escores_unicos) {
      escore_i_menor <- sum(escores <= i - 1)
      escore_i <- sum(escores <= i)
      normit_i <- qnorm((escore_i_menor + escore_i) / (2 * n))
      normit[banco.aberto$CADERNO == j & escore == i] <- normit_i
    }
  }
  return(normit)
}
