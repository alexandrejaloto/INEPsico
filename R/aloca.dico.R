#' @title Alocação de itens dicotômicos nos níveis da escala
#' @name aloca.dico
#'
#' @description Alocar itens dicotômicos em pontos ou níveis da escala
#'
#' @param par Objeto com os parâmetros dos itens estimados. Deve possuir três colunas,
#' uma para cada parâmetro (nesta ordem: a, b, c).
#' Só é necessário quando o método for um dos que seguem: `b`, `b65`, `n65` ou `(2+c)/3`.
#' @param met O método de alocação. As opções são: `b` (o parâmetro 'b' é utilizado para
#' alocar o item no nível); `b65` (o valor de theta em que a
#' probabilidade de acertar o item vale 0.65); `b65emp` (o valor de theta em que 65
#' porcento das pessoas acertaram o item; é a abcissa da
#' intercessão
#' da reta 0.65, com a interpolação linear entre a proporção empírica do nível e a do
#' nível anterior); `n65` (o nível para `b65`); `n65emp`
#' (o nível em que 65 porcento das pessoas acertaram o item); `(2+c)/3` (o valor de theta
#' em que a probabilidade de acertar o item vale (2+c)/3)
#' @param dp Desvio padrão da escala transformada
#' @param int.dp Os níveis variam de quanto em quanto, em relação ao `dp`?
#' @param int.nivel O nível mais baixo e o nível mais alto
#' @param centro `'centro'` se o nível for centrado no ponto; `'esquerda'` se o nível inicia
#' no ponto (fechado na esquerda)
#' @param banco Objeto do tipo `data.frame` com as respostas (0 = erro; 1 = acerto) e o
#' escore de cada indivíduo.
#' A última variável do banco é oe score. Esse objeto é necessário somente se
#' `met = 'b65emp'` ou `met = 'n65emp'`. ATENÇÃO: os nomes das colunas
#' dos itens devem começar com 'I' e o da nota deve ser 'NOTA'; para detalhes, veja o exemplo.
#' @param min.casos Número mínimo de respostas em um determinado nível para que o item
#' possa ser alocado nele. Usado somente quando o método
#' for `'b65emp'` ou `'n65emp'`
#' @param log Argumento lóico. A métrica é logística?
#'
#' @details Para esta função, ainda é necessário pensar sobre as transformações dos
#' parâmetros para alocação dos itens na escala.
#' Por exemplo, se de fato vai para escala (0,1) adequadamente e depois para a escala da
#' avaliação. É possível que haja algum erro
#' nessas transformações.
#'
#' @return
#'
#' @author Alexandre Jaloto
#'
#' @examples
#' set.seed(1000)
#' par = as.matrix(data.frame(a = rlnorm (30, 0, .5), b = rnorm (30, 0, 1), c = rbeta (30, 5, 17)))
#' pop = rnorm (5000, 0, 1)
#'
#'# precisa ser do tipo data.frame (não pode ser data.table nem matrix)
#'banco = data.frame(simular (pop, par))
#'
#'calib = mirt::mirt(banco, 1, itemtype = '3PL', TOL = 0.01)
#'nota = data.frame(mirt::fscores (calib))
#'names (nota) = 'NOTA'
#'par = mirt::coef(calib, simplify = TRUE, IRTpars = TRUE)$items[,1:3]
#'banco$NOTA = nota
#'
#'# atenção para a forma de nomear
#'names (banco) = c(paste0('I', 1:30), 'NOTA')
#'head (banco)
#'
#'int.nivel = c(-3, 3)
#'
#'centro = 'esquerda'
#'met = 'n65emp'
#'
#'aloca.dico(par = par, met = met, int.nivel = int.nivel, centro = centro, banco = banco, min.casos = 30)
#'
#'
#' @export
aloca.dico = function (par, met, dp = 1, int.dp = .5, int.nivel = c (-2, 2), centro = 'centro',
                       banco, min.casos = 50, log = TRUE)
{
  # os níveis
  if (centro == 'centro')
  {
    niveis = seq (int.nivel[1]-int.dp*dp/2, int.nivel[2]+int.dp*dp/2, int.dp*dp)
    nome.niveis = seq (int.nivel[1], int.nivel[2], int.dp*dp)
  } else if (centro == 'esquerda'){
    niveis = seq (int.nivel[1], int.nivel[2], int.dp*dp)
    nome.niveis = niveis
  }

  # se for normal, D = 1.7
  if (log == TRUE)
  {D = 1 } else if (log == FALSE)
  {D = 1.7}

  # caso o método para alocação seja n65
  if (met == 'n65')
  {

    # calcular o theta correspondente a P para acertar o item e alocar os itens nos níveis de acordo com a opção de método escolhida
    theta = list()

    # theta para P = .65
    theta$theta = qlogis( (.65 - par[,3])/(1-par[,3]),par[,2],1/(par[,1]*D))

    theta$niveis = cut (theta$theta, c (-Inf, niveis),
                        labels = seq (int.nivel[1], int.nivel[2], int.dp*dp), right = FALSE)

    # caso o método para alocação seja b65
  } else if (met == 'b65') {

    theta = qlogis( (.65 - par[,3])/(1-par[,3]),par[,2],1/(par[,1]*D))

    # caso o método para alocação seja b65emp
  } else if (met == 'b65emp') {

    # calcular a porcentagem de acerto em cada nível
    prop.niveis = data.frame()

    for (i in 1:length(niveis))
    {
      for (j in 1:(ncol(banco)-1))
      {

        # limite inferior e superior do nível
        inf = niveis[i]
        sup = niveis[i]+int.dp*dp

        #filtrar quem é do nível
        niv.selecao = subset (banco, NOTA >= inf & NOTA < sup)

        #ifelse (nrow (niv.selecao) > min.casos, round (mean ( niv.selecao[,paste0('I', j)]),2), NA)

        # calcular a porcentagem de acerto no nível
        prop.niveis[j,i] = ifelse (nrow (niv.selecao) > min.casos, round (mean ( niv.selecao[,paste0('I', j)]),2), NA)
      }
    }


    # indicar o nível de cada item (o nível em que 65% das pessoas acertam o item)
    for (i in 1:nrow(prop.niveis))
    {
      if (max (prop.niveis [i,-ncol(prop.niveis)], na.rm = TRUE) < .65)
      {print (paste0('O item ', i, ' não teve 65% de acertos em nenhum nível'))
        prop.niveis$Nivel[i] = NA } else {
          prop.niveis$Nivel[i] = nome.niveis [min (which (prop.niveis [i,] >= .65))]
        }
    }

    theta = c()

    for(i in 1:nrow(prop.niveis)) {

      # verificar o nível do item (na verdade é pra ver o Xésimo nível)
      x = which (nome.niveis == prop.niveis[i,"Nivel"])

      if (length(x)!=0) {

        # calcular a interpolação linear
        # y - y0
        y.y0 = 0.65 - prop.niveis[i,(x - 1)]

        # y1-y0
        y1.y0 = prop.niveis[i,x] - prop.niveis[i,(x - 1)]

        # x1-x0
        x1.x0 = niveis[x] - niveis[x-1]

        # x
        theta[i] = niveis[x-1] + (x1.x0*y.y0)/y1.y0

      } else {
        theta[i] = NA
      }
    }
    # caso o método para alocação seja n65emp
  } else if (met == 'n65emp') {

    # calcular a porcentagem de acerto em cada nível
    prop.niveis = data.frame()

    for (i in 1:length(niveis))
    {
      for (j in 1:(ncol(banco)-1))
      {

        # limite inferior e superior do nível
        inf = niveis[i]
        sup = niveis[i]+int.dp*dp

        #filtrar quem é do nível
        niv.selecao = subset (banco, NOTA >= inf & NOTA < sup)

        # calcular a porcentagem de acerto no nível
        prop.niveis[j,i] = ifelse (nrow (niv.selecao) > min.casos, round (mean ( niv.selecao[,paste0('I', j)]),2), NA)
      }
    }

    # indicar o nível de cada item (o nível em que 65% das pessoas acertam o item)
    for (i in 1:nrow(prop.niveis))
    {
      if (max (prop.niveis [i,-ncol(prop.niveis)], na.rm = TRUE) < .65)
      {print (paste0('O item ', i, ' não teve 65% de acertos em nenhum nível'))
        prop.niveis$Nivel[i] = NA } else {
          prop.niveis$Nivel[i] = nome.niveis [min (which (prop.niveis [i,] >= .65))]
        }
    }
    theta = prop.niveis$Nivel


    # caso o método para alocação seja (2+c)/3
  } else if (met == '(1+c)/2') {
    theta = qlogis( (((2+par[,3])/3) - par[,3])/(1-par[,3]),par[,2],1/(par[,1]*D))
  }

  return(theta)
}
