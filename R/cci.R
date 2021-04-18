#' @title Curva característica do item
#' @name cci
#'
#' @description Produz a curva característica de um item (CCI)
#'
#' @param a valor do parâmetro a
#' @param b valor do parâmetro b
#' @param c valor do parâmetro c
#' @param theta vetor com os valores de traço latente para a
#' construção do gráfico
#' @param info Valor lógico. Se TRUE, plota também a curva
#' de informação do item
#' @param ... Outros argumentos das funções plot e lines
#'
#' @return A função retorna um gráfico com a curva característica do item
#'
#'
#' @author Alexandre Jaloto
#'
#' @examples
#' # em uma escala (0,1)
#' a = 1.5
#' b = 0.3
#' c = .15
#'
#' cci (a, b, c)
#' cci (a, b, c, info = TRUE)
#'
#' # agora em uma escala (500, 100)
#' a = .01
#' b = 505
#' c = .15
#' theta = seq (100, 900, 10)
#' cci (a, b, c, theta)
#'
#' # alterando parâmtros do gráfico
#' cci (a, b, c, theta, col = 'red', lty = 2, main = 'CCI')
#'
#' @export
cci = function (a = 1.2, b = 0, c = .2, theta = seq (-4, 4, .01), info = FALSE, xlab = 'Proficiência (habilidade)', ylab = 'Probabiliade de acerto', ...)
{

  if (info == FALSE)
  {
  p = c()
  for (j in 1:length(theta))
  {
  #probabilidade de acerto
  p[j] = c + (1 - c)/(1 +exp(-a*(theta[j]-b)))
  }

  par(mar=c(4,4,4,4))
  (plotar = plot (theta, p, type = 'n', xlab = xlab, ylab = ylab, ...))
  (linhas = lines (theta, p, ...))
  } else {
    p = c()
    if (info == TRUE){curva.info = c()}
    for (j in 1:length(theta))
    {
      #probabilidade de acerto
      p[j] = c + (1 - c)/(1 +exp(-a*(theta[j]-b)))
      #informação de cada ponto
      if (info == TRUE){curva.info[j] = a^2 * (1 - p[j]) * ((p[j] - c)/(1-c))^2/p[j]}
    }

    par(mar=c(4,4,4,4))
    (plotar = plot (theta, p, type = 'n', xlab = xlab, ylab = ylab, ...))
    linhas = lines (theta, p, col = 1, lty = 1)
    par(new=T)
    plot (theta, curva.info, type = 'n', xlab = '', ylab = '', axes = FALSE)
    lines (theta, curva.info, col = 2, lty = 2)
    axis (4)
    mtext ('Informação', side = 4, line = 2)
    legend ("topleft", bg = "white", legend = c('Probabilidade', 'Informação'),
            lty = c (1,2), col = c (1,2))


  }
}
