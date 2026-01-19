#' @title Curva de informação do teste
#' @name info.teste
#'
#' @description Produz a(s) curva(s) de informação do(s) teste(s)
#'
#' @param pars Objeto do tipo data.frame, matrix ou uma lista com
#' data.frame e/ou matrix. Este objeto contém os parâmetros dos
#' itens de cada instrumento. Se o objetivo é plotar a curva de
#' apenas um instrumento, o objeto deve ser uma data.frame ou
#' matrix; se a curva de mais de um instrumento for plotada, o
#' objeto deve ser uma lista em que cada elemento é uma data.frame
#' ou matrix com os parâmetros. A primeira coluna do objeto (ou
#' elemento, em caso de ser uma lista) contém os valores do
#' parâmetro 'a'; a segunda, os do parâmetro 'b'; a terceira, os
#' do parâmetro 'c'
#' @param testes Vetor com os nomes dos testes, para inserir a
#' legenda no gráfico
#' @param theta Vetor com os valores de traço latente para a
#' construção do gráfico
#' @param erro Valor lógico. Se TRUE, plota também a curva
#' de erro da medida
#' @param ajustar Valor lógico. Se TRUE, plota todas as
#' curvas de informação na mesma escala. Se FALSE, as curvas
#' serão plotadas em escalas diferentes no que diz respeito ao
#' eixo 'y'.
#'
#' @return A função retorna um gráfico com a curva de informação
#' de cada teste.
#'
#' @author Alexandre Jaloto
#'
#' @examples
#' # criar objeto com os parâmetros
#' set.seed(1000)
#' pars1 = data.frame (a = runif (50, .7, 1.3), b = runif (50, -3, 3), c = runif (50, 0, 1))
#' pars2 = data.frame (a = runif (50, .7, 1.3), b = runif (50, -3, 3), c = runif (50, 0, 1))
#' pars3 = data.frame (a = runif (50, .7, 1.3), b = runif (50, -3, 3), c = runif (50, 0, 1))
#'
#' pars = list (pars1, pars2, pars3)
#' testes = c ('T1', 'T2', 'T3')
#' theta = seq (-3, 3, .5)
#'
#' info.teste (pars = pars, theta = theta, testes = testes, erro = FALSE, ajustar = TRUE)
#'
#' info.teste (pars = pars[[1]], theta = theta, testes = testes[1], erro = TRUE)
#' @export

info.teste = function (pars = list (), testes = list (), theta = seq (-4, 4, .01), erro = FALSE, ajustar = TRUE)
{

if (class (pars) != 'list')
{
a = pars [,1]
b = pars [,2]
c = pars [,3]

p = data.frame()
info = data.frame()

for (i in 1:dim(pars)[1])
{
  for (j in 1:length(theta))
  {
#probabilidade de acerto
    p[i,j] = c[i] + (1 - c[i])/(1 +exp(-a[i]*(theta[j]-b[i])))

#informação de cada ponto
    info[i,j] = a[i]^2 * (1 - p[i,j]) * ((p[i,j] - c[i])/(1-c[i]))^2/p[i,j]
  }
}

#informação total de cada theta
info.total = apply (info, 2, sum)
names (info.total) = theta

#erro da medida
tab.erro = 1/sqrt (info.total)

grafico = data.frame (info = info.total, erro = tab.erro, theta = theta)

# verificar a relação entre a o erro e a informação, para plotar no mesmo gráfico
#transf = max (info.total)/max (tab.erro)

  if (erro == FALSE) {
    plot (theta, grafico$info, type = 'n', xlab = 'Proficiências', ylab = 'Informação', col = 1, lty = 1)
    lines (theta, grafico$info, col = 1, lty = 1)
    legend ("topleft", bg = "white", legend = testes,
            lty = c (1), col = c (1))

    } else {
      par(mar=c(4,4,4,4))
      plot (theta, grafico$info, type = 'n', xlab = 'Proficiências', ylab = 'Informação', col = 1, lty = 1)
      lines (theta, grafico$info, col = 1, lty = 1)
      par(new=T)
      plot (theta, grafico$erro, type = 'n', xlab = '', ylab = '', axes = FALSE)
      lines (theta, grafico$erro, col = 2, lty = 2)
      axis (4)
      mtext ('Erro', side = 4, line = 2)
      legend ("topleft", bg = "white", legend = testes,
              lty = c (1), col = c (1))
      }

# caso pars seja de classe list
} else {

  if (erro == TRUE)
    stop("Se 'pars' é 'list', então o erro não é plotado no gráfico")

  a = list()
  b = list()
  c = list()
  p = list()
  info = list()
  info.total = list()
  tab.erro = list()
  grafico = list()
k = 1
i = 1
j = 1
  for (k in 1:length (pars))
  {
  a [[k]]= pars [[k]][,1]
  b [[k]]= pars [[k]][,2]
  c [[k]] = pars [[k]][,3]
  p[[k]] = data.frame ()
  info[[k]] = data.frame ()
  for (i in 1:dim(pars[[k]])[1])
  {
    for (j in 1:length(theta))
    {
        #probabilidade de acerto
      p[[k]][i,j] = c[[k]][i] + (1 - c[[k]][i])/(1 +exp(-a[[k]][i]*(theta[j]-b[[k]][i])))

      #informação de cada ponto
      info[[k]][i,j] = a[[k]][i]^2 * (1 - p[[k]][i,j]) * ((p[[k]][i,j] - c[[k]][i])/(1-c[[k]][i]))^2/p[[k]][i,j]
    }
  }

  #informação total de cada theta
  info.total[[k]] = apply (info[[k]], 2, sum)
  names (info.total[[k]]) = theta

  #erro da medida
  tab.erro[[k]] = 1/sqrt (info.total[[k]])

  grafico[[k]] = data.frame (info = info.total[[k]], erro = tab.erro[[k]], theta = theta)

  # verificar a relação entre a o erro e a informação, para plotar no mesmo gráfico
#  transf[[k]] = max (info.total[[k]])/max (tab.erro[[k]])
  }

# se não for colocar todas as curvas na mesma escala do y
if (ajustar == TRUE)
{
  max.info. = c()
  for (k in 1:length (grafico))
  {
    max.info. = c (max.info., max (grafico[[k]]$info))
  }
  max.info = max (max.info.)
  plot (theta, grafico[[1]]$info, c (min (theta), max (theta)), c (0, max.info), type = 'n', xlab = 'Proficiências', ylab = 'Informação', col = 1, lty = 1)

  for (k in 1:length (grafico))
  {
  lines (theta, grafico[[k]]$info, col = k, lty = k)
  }
  legend ("topleft", bg = "white", legend = testes,
          lty = c (1:k), col = c (1:k))

# se não for colocar todas as curvas na mesma escala do y
} else {

  plot (theta, grafico[[1]]$info, type = 'n', xlab = 'Proficiências', ylab = 'Informação', yaxt="n", col = 1, lty = 1)
  for (k in 1:length (grafico))
  {
    par(new=T)
    plot (theta, grafico[[k]]$info, type = 'n', xlab = '', ylab = '', axes = FALSE)
    lines (theta, grafico[[k]]$info, col = k, lty = k)
  }
  legend ("topleft", bg = "white", legend = testes,
          lty = c (1:k), col = c (1:k))
}
}
}
