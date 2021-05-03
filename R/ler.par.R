#' @title Importação do arquivo PAR
#' @name ler.par
#'
#' @description Importar o arquivo `.PAR` produzido pelo BILOG-MG ou MULTILOG
#'
#' @param arq.par Arquivo `.PAR`
#' @param prog O programa que produziu o arquivo `.PAR`. Use `'BLM'` (padrão)
#' para BILOG-MG e `'MLM'` para MULTILOG.
#' @param categorias Somente se prog = `'MLM'`. O número máximo de categorias
#' de um item.
#'
#' @details Utilize este campo para escrever detalhes mais tecnicos da
#'     sua funcao (se necessario), ou para detalhar melhor como
#'     utilizar determinados argumentos.
#'
#' @return A função retorna um objeto do tipo data.frame com os dados do arquivo .PAR
#'
#' @author Alexandre Jaloto
#'
#' @examples
#' ler.par ('PTLCV3.PAR')
#' ler.par ('ANA16.PAR', prog = 'MLM', categorias = 4)
#' @export

ler.par = function (arq.par, prog = 'BLM', categorias)
{
if (prog == 'BLM')
{
  par = read.fwf (arq.par, skip = 4, widths = c (8, 8, rep (10, 12), 10, 4, 1, 1))
  names (par) = c ('ITEM', 'TESTE', 'INTERCEPTO', 'ERRO_INTERCEPTO', 'PAR_A', 'ERRO_A', 'PAR_B', 'ERRO_B', 'CARGA_FATORIAL', 'ERRO_CARGA', 'PAR_C', 'ERRO_C',
                   'DRIFT', 'ERRO_DRIFT', 'NA', 'NUM_BILOG', 'RESP', 'NA2')
  par = par [, c(-15, -18)]
} else if (prog == 'MLM')
{
  par. = read.fwf(arq.par, widths = rep (12, categorias))
  par = par. [-dim(par.)[1],]
}
  return (par)
}
