#' @title Alocação dos indivíduos nos níveis da escala
#' @name aloca.pessoa
#'
#' @description Alocar os indivíduos, de acordo com seu escore, nos níveis da escala
#'
#' @param escore Vetor com os escores dos indivíduos
#' @param niv Os níveis da escala
#' @param direita argumento lógico. Se `TRUE` (padrão), é fechado na direita e aberto na
#' esquerda. Se `FALSE`, é aberto na direita e fechado na esquerda.
#'
#' @details O nível é selecionado de acordo com o escore. O participante é posicionado no nível
#' em que o escore é maior do que o valor mínimo do intervalo e menor ou igual ao maior valor
#' do intervalo, se `direita = TRUE` Se `direita = FALSE`, o participante é posicionado no nível
#' em que o escore é maior ou igual ao valor mínimo do intervalo e menor do que o maior valor
#' do intervalo.
#'
#'
#' @return A função retorna um objeto do tipo lista com dois elementos:
#'
#' $theta: O escore dos indivíduos
#'
#' $niveis: O nível de cada indivíduo na escala
#'
#' @author Alexandre Jaloto
#'
#'
#' @examples
#' set.seed (12345)
#' escore = rnorm (100)
#' aloca.pessoa (escore)
#'
#' @export
aloca.pessoa = function (escore, niv = seq (-2, 2, .25), direita = TRUE)
{
  score = list()
  score$theta = escore

  # caso o método para alocação seja o da opção 1 do relatório da ANA 2014
  if (met == 1)
  {
    score$niveis = data.frame (cut (score$theta, c (-Inf, niv), labels = niv, right = direita))

    # caso o método para alocação seja o da opção 3 do relatório da ANA 2014
  } else if (met == 3) {
    score$niveis = data.frame (cut (score$theta, c (-Inf, niv), labels = niv, right = direita))
  }
  score$theta = data.frame (score$theta)

  names (score$theta) = "Escore"
  names (score$niveis) = "Nivel"
  return (score)
}
