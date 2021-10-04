#' @title Relatório TCT
#' @name relatorio.tct
#'
#' @description Elabora um relatório com informações das análises psicométricas
#' segundo a Teoria Clássica dos Testes (TCT)
#'
#' @param disc Sigla da disciplina / área
#' @param disc.extenso Nome da disciplina / área por extenso
#' @param teste Nome do teste
#' @param n.itens.comuns Quantidade de itens comuns
#' @param n.itens.novos Quantidade de itens novos
#' @param n.alt Número máximo de alternativas dos itens
#' @param tct Objeto com os resultados da análise clássica (no mesmo padrão do
#' objeto que a função tct retorna). Importante que o nome das variáveis deste
#' objeto seja no mesmo padrão do objeto que a função tct retorna
#'
#' @return A função retorna um arquivo HTML com o relatório das análises
#' psicométricas segundo a Teoria Clássica dos Testes (TCT).
#'
#' @author Alexandre Jaloto
#'
#'
#' @export

relatorio.tct = function (disc, disc.extenso, teste, n.itens.comuns, n.itens.novos,
                          n.alt, tct)
{
  rmarkdown::render (
    paste0(
      .libPaths()[1],
      '/INEPsico/relatorio_tct.rmd'
    ),
    output_format = 'html_document',
    output_file = paste0 (
      '/TCT_',
      disc,
      '.html'
    ),
    output_dir = getwd()
  )
}

