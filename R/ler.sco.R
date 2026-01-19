#' @title Importação do arquivo de escore
#' @name ler.sco
#'
#' @description Importar o arquivo de escore do BILOG-MG ou do MULTILOG
#'
#' @param arq.sco Arquivo SCO gerado pelo BILOG-MG ou pelo MULTILOG
#' @param prog Programa que gerou a base de dados ('BLM' para BILOG-MG e 'MLM' para MULTILOG)
#'
#' @details Utilize este campo para escrever detalhes mais tecnicos da
#'     sua funcao (se necessario), ou para detalhar melhor como
#'     utilizar determinados argumentos.
#'
#' @return A função retorna uma data.frame com os escores dos indivíduos
#'
#' @author Alexandre Jaloto
#'
#'
#' @examples
#' ler.sco (arq.sco = 'PTLCV3.SCO', prog = 'BLM')
#' @export

ler.sco = function (arq.sco, prog = 'BLM')
{

if (prog == 'MLM')
  {
  escore = data.table::fread (arq.sco, header = F, data.table = F)
  }

if (prog == 'BLM')
{
  # ler arquivo SCO
  dados2. = readLines(arq.sco)

  dados2 <- do.call(rbind,
                    lapply(split(dados2., rep(1:(length(dados2.)/2), each = 2)),
                           function(x)cbind(x[1], x[2])))

    dados2 = matrix (dados2[-1,], ncol = 2)

escore = data.frame (
  as.numeric (stringr::str_sub(dados2[,1], 1, 5)), # 5
  stringr::str_sub(dados2[,1], 6, 35), # 30
  as.numeric (stringr::str_sub(dados2[,2], 1, 6)), # 6
  stringr::str_sub(dados2[,2], 7, 7), # 1
  stringr::str_sub(dados2[,2], 8, 15), # 8
  as.numeric (stringr::str_sub(dados2[,2], 16, 20)), # 5
  as.numeric (stringr::str_sub(dados2[,2], 21, 25)), # 5
  as.numeric (stringr::str_sub(dados2[,2], 26, 35)), # 10
  as.numeric (stringr::str_sub(dados2[,2], 36, 47)), # 12
  as.numeric (stringr::str_sub(dados2[,2], 48, 59)), # 12
  stringr::str_sub(dados2[,2], 60, 60), # 1
  as.numeric (stringr::str_sub(dados2[,2], 61, 70)), # 10
  as.numeric (stringr::str_sub(dados2[,2], 71, 80)) # 10
  )

  names (escore) = c("Grupo", "Inscricao", "Peso", "Calibracao", "Teste", "Tentativa", "Acerto", "Porcentagem",
                     "Proficiencia", "Erro", "Estimacao do erro", "Ajuste do grupo", "Probabilidade do padrao")

}

return(escore)
}
