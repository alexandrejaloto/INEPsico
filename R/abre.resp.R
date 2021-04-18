#' @title Separar o vetor de resposta
#' @name abre.resp
#'
#' @description Separa um único vetor de respostas (vários itens) em vetores compostos pela resposta a um único item
#'
#' @param unico Objeto com o vetor das respostas a todos os itens
#'
#' @details
#'
#' @return A função retorna uma matriz com número de colunas igual à quantidade de itens e número de linhas igual à quantidade de participantes.
#'
#' @author Alexandre Jaloto
#'
#' @examples
#' arq.par = 'Rodada6.PAR'
#' categorias = 5
#' p = .65
#' m = 0
#' dp = 1
#' int.dp = .25
#' int.nivel = c (-2, 2)
#' met = 3
#' nomes.cat = data.frame (a = paste0 ('a', 1:16), b = paste0 ('b', 1:16),
#'                c = paste0 ('c', 1:16), d = paste0 ('d', 1:16),
#'                e = paste0 ('e', 1:16))
#' aloca.item.multi (arq.par, categorias, p = .65, m = 0,
#'                dp = 1, int.dp = .25, int.nivel = c (-2, 2),
#'                met = 3, nomes.cat = nomes.cat)
#'
#' @export

abre.resp = function (unico)
{
  # separar a variável das respostas do caderno
  resp. = strsplit (as.character (as.matrix (unico)), NULL)

  # juntar os elementos da lista
  resp = do.call (rbind, resp.)
  return(resp)
}
