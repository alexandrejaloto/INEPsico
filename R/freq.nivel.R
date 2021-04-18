#' @title Frequência dos indivíduos nos níveis
#' da escala
#' @name freq.nivel
#'
#' @description Verificar a frequência dos indivíduos em cada nível
#' da escala
#' @param escore Vetor com os escores
#' @param peso Vetor com o peso de cada escore
#' @param niv Os níveis da escala
#' @param met O método de alocação, segundo o relatório de 2014 da ANA
#'
#' @details Detalhes
#'
#'
#' @return A função retorna um objeto do tipo data.frame com duas variáveis:
#' o nível da escala e a frequência relativa dos indivíduos.
#'
#' @author Alexandre Jaloto
#'
#'
#' @examples
#' set.seed(1000)
#' escore = rnorm (100)
#' freq.nivel (escore)
#' @export
#'
freq.nivel = function(escore, peso = 1, niv = seq (-2, 2, .25), met = 3)
{
# determinar os níveis dos pdrões de resposta
aloca = aloca.pessoa (escore, niv = niv, met = met)

# criar objeto com as informações necessárias
niveis. = data.frame (escore, peso, aloca$niveis[,1])
names (niveis.) = c ('Escore', 'Peso', 'Nivel')

niveis1 = dplyr::group_by (niveis., Nivel)
niveis = as.data.frame (dplyr::summarize (niveis1, Frequencia = sum(Peso)/ (sum(niveis.$Peso))))

return (niveis)
}
