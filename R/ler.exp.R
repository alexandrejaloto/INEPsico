#' @title Importação do arquivo EXPECT
#' @name ler.exp
#'
#' @description Importar o arquivo .EXP produzido pelo BILOG-MG
#'
#' @param arq.exp Arquivo .EXP
#'
#' @details Utilize este campo para escrever detalhes mais tecnicos da
#'     sua funcao (se necessario), ou para detalhar melhor como
#'     utilizar determinados argumentos.
#'
#' @return A função retorna um objeto do tipo data.frame com os dados do arquivo .EXP
#'
#' @author Alexandre Jaloto
#'
#' @examples
#' ler.exp (arq.exp = 'PTLCV3.EXP')
#' @export

ler.exp = function (arq.exp)
{
dados. = readLines(arq.exp)

dados = dados. [3:length(dados.)]

tamanho = length(dados)

dados2 = character ((tamanho/8)*6)

for (i in 0:((tamanho/8)-1))
{
  dados2 [i*6+1] = dados [i*8+1]
  dados2 [i*6+2] = dados [i*8+2]
  dados2 [i*6+3] = dados [i*8+3]
  dados2 [i*6+4] = dados [i*8+4]
  dados2 [i*6+5] = dados [i*8+5]
  dados2 [i*6+6] = dados [i*8+7]
}

dados = data.frame (
  stringr::str_sub(dados2, 1, 8), # 8
  as.numeric (stringr::str_sub(dados2, 9, 17)), # 9
  stringr::str_sub(dados2, 18, 27), # 10
  stringr::str_sub(dados2, 28, 37), # 10
  stringr::str_sub(dados2, 38, 48), # 11
  stringr::str_sub(dados2, 49, 59), # 11
  stringr::str_sub(dados2, 60, 70), # 11
  stringr::str_sub(dados2, 71, 81) # 11
)
names (dados) = c ('Item', 'Grupo', 'Variável', paste('V', 1:5, sep = ''))

# transformar os valores que são ***** em NA

dados$V1 = stringr::str_replace(stringr::str_sub(dados$V1), '[*]', NA_character_)
dados$V2 = as.numeric (stringr::str_replace(stringr::str_sub(dados$V2), '[*]', NA_character_))
dados$V3 = as.numeric (stringr::str_replace(stringr::str_sub(dados$V3), '[*]', NA_character_))
dados$V4 = as.numeric (stringr::str_replace(stringr::str_sub(dados$V4), '[*]', NA_character_))
dados$V5 = as.numeric (stringr::str_replace(stringr::str_sub(dados$V5), '[*]', NA_character_))

return (dados)
}
