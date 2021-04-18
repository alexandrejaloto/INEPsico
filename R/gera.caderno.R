#' @title Gerar os cadernos de prova
#' @name gera.caderno
#'
#' @description Gerar objeto com a estrutura correspondente à composição de cada caderno de cada disciplina / área
#'
#' @param itens O(s) objeto(s) com os itens da(s) disciplina(s) que compõe(m) os cadernos no BIB (tem que ser todos, por enquanto; depois tem que melhorar a função)
#' @param disc.cad Quantidade de disciplinas em cada caderno (padrão: 2)
#' @param bib Objeto com o BIB
#'
#' @details Utilize este campo para escrever detalhes mais tecnicos da
#'     sua funcao (se necessario), ou para detalhar melhor como
#'     utilizar determinados argumentos.
#'
#' @return O que a função retorna?
#'
#' @author Alexandre Jaloto
#'
#'
#' @examples
#' set.seed(1000)
#' gab.lc = sample (LETTERS[1:4], 9, replace = TRUE)
#' gab.mt = sample (LETTERS[1:4], 9, replace = TRUE)
#' itens.lc = data.frame (Bloco = rep (1:3, c (3,3,3)), Posicao = rep (1:3, 3),
#'                        Item = sample (12345:54321, 9), Origem = 'NOVO',
#'                        Gabarito = gab.lc, Num_bilog = 201:209,
#'                        Nome_bilog = paste ('P', 0001:0009, sep = ''), Disciplina = 'LC')
#' itens.mt = data.frame (Bloco = rep (1:3, c (3,3,3)), Posicao = rep (1:3, 3),
#'                        Gabarito = gab.mt, Item = sample (12345:54321, 9),
#'                        Origem = 'NOVO', Num_bilog = 201:209,
#'                        Nome_bilog = paste ('P', 0001:0009, sep = ''), Disciplina = 'MT')
#'
#' bib = data.frame (Caderno = 1:3, Disciplina1 = rep ('LC', 3), Disciplina2 = rep ('MT', 3),
#'                   Bloco1 = 1:3, Bloco2 = c(2, 3, 1), Bloco3 = 1:3, Bloco4 = c(2, 3, 1))
#' itens = rbind (itens.lc, itens.mt)
#' cadernos = gera.caderno (itens, bib)
#' @export
gera.caderno = function (itens, bib, disc.cad = 2)
{
# nomes das disciplinas
#disciplinas = as.character (unique (unlist (bib[,2:(disc.cad+1)])))
disciplinas = as.character (unique (unlist (bib[,paste0('Disciplina', 1:disc.cad)])))

# número total de cadernos
tot.cad = max (bib$Caderno)

# quantidade de blocos por caderno
blocos.cad = sum (grepl("Bloco", names(bib)))

# quantidade de blocos de cada disciplina em cada caderno
blocos.disc = blocos.cad/disc.cad

# quantidade de blocos de cada disciplina
blocos = max (bib [, paste0 ('Bloco', 1:blocos.cad)])

################################# CHAMAR DETERMINADO CADERNO E COLOCAR OS ITENS CORRESPONDENTES (UMA ÚNICA TABELA POR DISCIPLINA COM TODA A INFORMAÇÃO) #################################

cad.disc2 = list()
cad.disc = list()
est.disc = list()

for (i in 1:length (disciplinas))
{
  for (x in 1:tot.cad)
  {
# selecionar os itens de cada disciplina
disc = subset (itens, Disciplina == disciplinas[i])

# selecionar os cadernos de cada disciplina
cad.disc2[[disciplinas[i]]] = subset (bib, bib[,2] == disciplinas[i])

# montar tabela com Caderno, Disciplina e Blocos da disciplina no caderno (considera sempre dois blocos por disciplina em cada caderno)
cad.disc[[disciplinas[i]]] = data.frame (cad.disc2[[disciplinas[i]]][,1], cad.disc2[[disciplinas[i]]][,2],
                                         cad.disc2[[disciplinas[i]]][,(2+disc.cad):(2+disc.cad+blocos.disc-1)])

cad.d = data.frame()
for (j in 1:disc.cad)
{
cad.disc2[[disciplinas[i]]] = subset (bib, bib[,1+j] == disciplinas[i])
cad.disc3 = data.frame (cad.disc2[[disciplinas[i]]][,1], cad.disc2[[disciplinas[i]]][,1+j],
                        cad.disc2[[disciplinas[i]]][,(2+disc.cad+(blocos.disc*(j-1))):(2+disc.cad+(blocos.disc*(j-1))+blocos.disc-1)])

names (cad.disc3) = c ('Caderno', 'Disciplina', paste0 ('Bloco', 1:blocos.disc))
cad.d = rbind (cad.d, cad.disc3)
}


# montar tabela com Caderno, Disciplina e Blocos da disciplina no caderno
cad.disc[[disciplinas[i]]]= cad.d

bl = data.frame()
for (k in 1:blocos.disc)
  {
  bl1 = subset (itens, Bloco == cad.disc [[disciplinas[i]]][x,(2+k)]
                 & Disciplina == disciplinas[i])	# primeiro bloco do caderno
bl = rbind (bl, bl1)
}

cad = cbind (bl, cad.disc [[disciplinas[i]]][x,1])	# adicionar uma coluna com o número do caderno

    cab = c (names (itens), "Caderno")
    names (cad) = cab
    nome.disc = disciplinas[i]

    est.disc [[nome.disc]] = rbind (est.disc[[nome.disc]], cad)	# montar a estrutura dos cadernos da disciplina

  }

  est.disc[[nome.disc]] = est.disc[[nome.disc]][order (est.disc[[nome.disc]]$Caderno),]

  names (est.disc[[nome.disc]]) = c (names (itens), "Caderno")
}

return (est.disc)
}
