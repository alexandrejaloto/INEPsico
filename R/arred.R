#' @title Arredondamento de números
#' @name arred
#'
#' @description Arredondar valores numéricos de acordo com a norma 5891:2014 da ABNT. O mesmo critério é adotado por pogramas como Excel e SPSS.
#'
#' @param x Valor a ser arredondado
#' @param n Quantidade de casas decimais
#'
#' @details O programa multiplica x por 10^n, trunca esse número e
#' diminui de x * 10^n. Se essa diferença for maior ou igual a 0.5,
#' então arredonda para cima; se essa diferença for menor do que 0.5,
#' então arredonda para baixo.
#'
#' ATENÇÃO: devido à forma como o número binário é convertido em número real,
#' é aconselhável utilizar o comando options(digits = 16). O padrão para essa opção é 7.
#'
#' @return A função retorna o valor arredondado de acordo com o critério determinado.
#'
#' @author Alexandre Jaloto
#'
#'
#' @examples
#' x = 100.123456
#' n = 5
#' arred (x, n)
#' options(digits=7)
#' arred (x, n)
#' options(digits=16)
#' arred (x, n)
#' options(digits=22)
#' arred (x, n)
#'
#'
#' @export

arred = function (x, n)
{
#if (x * (10 ^ n) - trunc (x * (10 ^ n)) >= .5)
#{trunc (x * (10 ^ n) + 1) / (10 ^ n)} else
#{trunc (x * (10 ^ n)) / (10 ^ n)}
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}
