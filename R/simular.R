#' @title Simular padrões de resposta
#' @name simular
#'
#' @description Simular padrões de resposta para itens dicotômicos
#' segundo a Teoria de Resposta ao Item (modelos logístico e normal).
#' Verifique se a função simdata do pacote mirt contempla suas necessidades,
#' pois ela é mais rápida.
#' @param theta Vetor com as medidas de traço latente dos indivíduos
#' @param pars Objeto do tipo data.frame ou matrix com os parâmetros
#' dos itens. A primeira coluna deve apresentar os valores do parâmetro
#' de discrimação; a segunda, os valores de dificuldade (posição);
#' a terceira, os valores de acerto casual (pseudochute). Para modelos
#' de dois parâmetros ou um, veja a seção Details.
#' @param mod O modelo da distribuição de probabilidade. Use "log"
#' para logístico e "norm" para normal
#'
#' @details A simulação requer itens de três parâmetros. Para realizar
#' simulação de respostas a itens de dois parâmetros, arbitre o valor
#' do acerto casual para 0. Para realizar a simulação de respostas a
#' itens de um parâmetro, considere o acerto casual como 0 e a discriminação
#' como 1.
#'
#' @return A função retorna um objeto do tipo matrix que contém a
#' probabilidade de cada indivíduo acertar cada item. As linhas
#' correspondem aos indivíduos e as colunas, aos itens.
#'
#' @author Alexandre Jaloto
#'
#'
#' @examples
#' set.seed(1000)
#' theta = rnorm (50, 0, 1)
#' pars = matrix ( c (runif (20, .5, 2), rnorm (20, 0, 1),
#'                    runif (20, .05, .4)), nrow = 20, ncol = 3)
#' simular (theta = theta, pars = pars)
#' @export
simular = function (theta = seq(-3, 3, by = 0.1), pars, mod = "log")
{
  if (!is.null(dim(theta)) | mode(theta) != "numeric")
    stop("theta tem que ser um vetor numérico")
  if (mode(pars) != "numeric" & !inherits(pars, "matrix"))
    stop("pars tem que ser uma matriz com valores numéricos")
  if (dim(pars)[2] != 3)
    stop("o modelo requer três parâmetros")
  if (any(pars[, 3] >= 1) | any(pars[, 3] < 0))
    stop("o parâmetro 'c' (pseudochute) precisa estar entre 0 e 1")
  if (any(pars[, 1] < 0))
    warning("há pelo menos um valor negativo para o parâmetro 'a' (discriminação)")

  # se for modelo normal
  if (mod == 'norm')
  {
    sim = matrix(ncol = dim (pars)[1], nrow = length (theta))
    for (i in 1:length (theta))
    {
      sim [i,] = as.numeric (runif (dim(sim)[2]) < pars[,3] + (1 - pars[,3])/(1 + exp(-1.7*pars[,1]*(theta[i]-pars[,2]))))
    }
  } else {
    # se for modelo logístico
    if (mod == 'log')
    {
      sim = matrix(ncol = dim (pars)[1], nrow = length (theta))
      for (i in 1:length (theta))
      {
        sim [i,] = as.numeric (runif (dim(sim)[2]) < pars[,3] + (1 - pars[,3])/(1 + exp(-pars[,1]*(theta[i]-pars[,2]))))
      }
    }
  }
  return (sim)
}
