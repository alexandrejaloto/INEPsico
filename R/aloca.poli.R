#' @title Alocação de itens politômicos nos níveis da escala
#' @name aloca.poli
#'
#' @description Alocar os itens com parâmetros estimados pelo MULTILOG nos níveis da escala
#'
#' @param arq.par Arquivo PAR gerado, pelo MULTILOG, com os parâmetros dos itens estimados
#' @param categorias O maior quantitativo de categorias de um mesmo item
#' @param p A probabilidade de obter determinado código (categoria) utilizada para
#' alocar o item em um nível
#' @param m Média da escala transformada (o padrão da função é não transformar)
#' @param dp Desvio padrão da escala transformada (o padrão da função é não transformar)
#' @param int.dp Os níveis variam de quanto em quanto, em relação ao \code{dp}?
#' @param int.nivel O nível mais baixo e o nível mais alto
#' @param met O método de alocação, podendo ser `1`ou `3`
#' @param nomes.cat objeto do tipo \code{data.frame} ou \code{matrix} indicando o nome
#' de cada
#' categoria de cada item (nesta \code{data.frame}, se a categoria não existir em
#' determinado item, indicar \code{NA})
#'
#' @details Para esta função, ainda é necessário pensar sobre as transformações dos
#' parâmetros para alocação dos itens na escala.
#' Por exemplo, se de fato vai para a escala (0,1) adequadamente e depois para a escala da
#' avaliação. É possível que haja algum erro
#' nessas transformações.
#'
#' Se `met = 1`, o posicionamento do item é feito da seguinte maneira: calcula-se a
#' proficiência que o participante deveria ter, dada a probabilidade de 0,65 de ele
#' marcar aquela categoria ou mais alta
#'
#' \deqn{\theta = (-1/a)*ln((1-p)/1)+b}
#'
#' onde `a` é a discriminação, `p` é a probabilidade de marcar a categoria ou
#' superior (no caso, 0,65) e `b` é o parâmetro de dificuldade.
#'
#' Se `met = 3`, o posicionamento do item é feito da seguinte maneira: calcula-se a
#' probabilidade de uma categoria ser selecionada dado que o theta é igual ao nível.
#' Seleciona-se o nível em que há 65% de probabilidade ou mais de um estudante com
#' proficiência naquele nível marcar uma categoria ou mais alta.
#'
#' @return A função retorna um objeto do tipo lista com três elementos:
#'
#' \code{$prof}: O escore que retorna a probabilidade 'p' do
#' indivíduo selecionar determinada categoria
#'
#' \code{$niveis}: O nível de cada categoria do item
#'
#' \code{$MaiorProb}: A categoria mais provável de ser selecionada em cada nível
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
#' aloca.poli (arq.par, categorias, p = .65, m = 0,
#'                dp = 1, int.dp = .25, int.nivel = c (-2, 2),
#'                met = 3, nomes.cat = nomes.cat)
#'
#' @export

aloca.poli = function (arq.par, categorias, p = .65, m = 0,
                             dp = 1, int.dp = .25, int.nivel = c (-2, 2),
                             met = 3, nomes.cat = c())
{
# ler arquivo PAR
par = ler.par(arq.par, prog = 'MLM', categorias)

# verificar número de itens
n.item = dim (par)[1]

# objeto com valores de parâmetro a
a = par [1:n.item,1]

# objeto com valores de parâmetro b
b = par [1:n.item,-1]

# calcular o theta correspondente a p para acertar o item e alocar os itens nos níveis de acordo com a opção de método escolhida
theta = list()
theta$prof = (- log ((1-p)/p)/a + b)*dp + m
prof.min = min (theta$prof [is.na(theta$prof)==F])
prof.max = max (theta$prof [is.na(theta$prof)==F])

theta$niveis = as.data.frame(matrix(nrow = nrow(theta$prof)))

# caso o método para alocação seja 1
if (met == 1)
{
for (i in 1:(categorias-1))
{
  theta$niveis[,i] = cut (theta$prof[,i], c (-Inf, seq (int.nivel[1]+int.dp*dp/2, int.nivel[2]+int.dp*dp/2, int.dp*dp)), labels = seq (int.nivel[1], int.nivel[2], int.dp*dp))
}

# caso o método para alocação seja 3
} else if (met == 3) {
  for (i in 1:(categorias-1))
{
  theta$niveis[,i] = cut (theta$prof[,i], c (-Inf, seq (int.nivel[1], int.nivel[2], int.dp*dp)), labels = seq (int.nivel[1], int.nivel[2], int.dp*dp))
}
}

# quantidade de níveis
n.niv = length(seq (int.nivel[1], int.nivel[2], int.dp*dp))

theta$MaiorProb = as.data.frame (matrix(ncol = n.niv, nrow = n.item))
names (theta$MaiorProb) = seq (int.nivel[1], int.nivel[2], int.dp*dp)

# a categoria de maior probabilidade será:
# se o nome da variável (que é o nível) for menor do que o nível da categoria mais fácil do item
# então será a 'cat1'; se não for, então será a 'cat' que tem nível igual ao nível em questão
# i é o item
for (i in 1:n.item)
{
# j é o nível
for (j in 1:n.niv)
{
  theta$MaiorProb [i,j] = ifelse (
    as.numeric (names (theta$MaiorProb)[j]) < as.numeric (min (theta$niveis[i,] [is.na(theta$niveis[i,])==F])),
    'cat1',
    paste('cat', ((which(theta$niveis[i,] == names (theta$MaiorProb)[j]) + 1)), sep = "")
    )
}
}
# trocar 'cat' pela categoria correspondente
for (i in 1:n.item)
{
  for (j in 1:n.niv)
  {

    theta$MaiorProb [i,j] = ifelse (theta$MaiorProb [i,j] == 'cat', theta$MaiorProb [i,(j-1)], theta$MaiorProb [i,j])
  }
}
# nomear as categorias
if (class (nomes.cat)=='data.frame' | class (nomes.cat)=='matrix')
{

  # número de algarismos da quantidade de categorias
  n.dig = ifelse (categorias<10, 1, 2)

  # i é o item
  for (i in 1:n.item)
  {

    # j é o nível
    for (j in 1:n.niv)
    {

      # retornar o número da categoria que será substituída nesta rodada do loop
      n.cat = as.numeric (substr(theta$MaiorProb [i,j],4,n.dig+4))

      # categoria correspondente ao item i, categoria n.cat
      cat. = as.character (nomes.cat [i, n.cat])

      # substituindo a 'cat' + i pela categoria correspondente da data.frame cat
      theta$MaiorProb [i,j] = cat.
    }
  }
}
return(theta)
}
