#' Banco simulado em BIB com formato fechado
#'
#'@details
#' Lista com quatro elementos:
#' \itemize{
#' \item `respostas` `data.frame` correspondente ao banco de respostas, com três variáveis:
#' \itemize{
#' \item `ID` identificação dos 10.000 sujeitos
#' \item `CADERNO` caderno que o sujeito respondeu
#' \item `TX_RESPOSTA` vetor de resposta aos 77 itens. As respostas
#' possíveis são "A", "B", "C", "D", "E", ".", "*"
#' }
#' \item `gabarito` gabarito dos 77 itens
#' \item `BIB` desenho do BIB
#' \item `itens` dicionário dos itens
#' }
"banco.sim.BIB.fechado"
