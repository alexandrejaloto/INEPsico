#' @title Abrir o banco
#' @name abre.banco
#'
#' @description Construir um banco 'aberto' a partir de uma base com informações de respostas organizada por cadernos distintos.
#'
#' @param banco Objeto que tem somente a variável correspondente ao caderno e as variáveis correspondentes às espostas aos itens
#' @param itens O(s) objeto(s) com os itens da(s) disciplina(s) que compõe(m) os cadernos no BIB (tem que ser todos, por enquanto; depois tem que melhorar a função)
#' @param disc Qual será a disciplina desse banco?
#' @param disc.cad Quantidade de disciplinas em cada caderno
#' @param bib Objeto com o BIB
#'
#' @return A função retorna uma lista com dois elementos: 'respostas' e 'gabarito'
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

#' resp = matrix (sample (LETTERS[1:4], 180, replace = TRUE), ncol = 6)
#' banco = data.frame (CAD = seq(1:3), resp)
#' disc = 'LC'
#' aberto = abre.banco (banco = banco, itens = itens, bib = bib, disc = disc)
#' @export
abre.banco = function (banco, itens, bib, disc, disc.cad = 2)

{

  # disc.cad <- 2

  # estrutura dos cadernos
  # estrutura = gera.caderno (itens, bib, disc.cad)[[disc]]
  estrutura = gera.caderno (itens, bib, disc.cad)
  # estrutura$LC

  # número total de cadernos
  # tot.cad = max (estrutura$Caderno)
  tot.cad = max (estrutura[[disc]]$Caderno)

  # criar objeto 'aberto.' que será a base para o objeto final 'aberto'.
  # o que será feito: um loop para pegar os respondentes de cada caderno; nomeaçãoo das
  # variáveis de acordo com os códigos dos itens; rbind pelo data.table para juntar cada
  # objeto de cada loop

  # quais são as disciplinas do caderno?
  disciplinas = as.character (unique (unlist (bib[,paste0('Disciplina', 1:disc.cad)])))
  # disciplinas <- c('LC', 'MT')

  aberto. = data.table::as.data.table(data.frame())
  for (i in 1:tot.cad)
  {
    # i <- 1

    # selecionar os sujeitos do caderno i
    resp = data.table::as.data.table (subset (banco, banco[,1] == i))

    # caso não haja respondente do caderno, passa para o próximo
    if(nrow(resp) == 0)
      next

    # selecionar caderno i de cada disciplina

    # fazer loop com a quantidade de disc.cad


    # cad = list()
    disc.n = list()
    cad. = list()

    for(j in 1:disc.cad)
    {
      # j = 1

      # pegar a estrutura da disciplina j
      cad.[[j]] = subset (estrutura[[disciplinas[[j]]]], Caderno == i)
      names(cad.)[j] = disciplinas[[j]]

      # qual é a disciplina j?
      disc.n[[j]] = subset(bib, Caderno == i)[[paste0('Disciplina', j)]]

    }

    cad = data.frame()
    for(j in 1:disc.cad)
      # empilhar com a ordem do caderno
      cad = rbind(cad, cad.[[disc.n[[j]]]])


    # sem loop é assim
    # cad[[disciplinas[[1]]]] = subset (estrutura[[disciplinas[[1]]]], Caderno == i)
    # cad[[disciplinas[[2]]]] = subset (estrutura[[disciplinas[[2]]]], Caderno == i)


    # cad1 = subset (estrutura[[disciplinas[[1]]]], Caderno == i)
    # cad2 = subset (estrutura[[disciplinas[[2]]]], Caderno == i)


    # disc1 = subset(bib, Caderno == i)$Disciplina1
    # disc2 = subset(bib, Caderno == i)$Disciplina2

    # subset(bib, Caderno == i)$Disciplina1



    # cad = rbind(cad[[disc1]], cad[[disc2]])

    # cad = rbind(cad, cad.)


    names (resp) = c ('CADERNO', cad$Item)
    aberto. = rbind(aberto., resp, fill = TRUE)
  }
  aberto = list()
  aberto$respostas = data.frame (aberto.)
  names (aberto$respostas) = names (aberto.)
  rm (aberto.)

  cod.itens = itens %>%
    subset(Disciplina == disc) %>%
    dplyr::arrange(Bloco, Posicao) %>%
    dplyr::distinct(Item) %>%
    dplyr::pull()

  names(aberto$respostas)

  aberto[['respostas']][,c('CADERNO', cod.itens)]

  # selecionar os itens da disc
  aberto$respostas = aberto$respostas[,c('CADERNO', cod.itens)]

  # agora abrir o gabarito

  # criar objeto com os códigos dos itens (sem repetição)
  # aberto[['gabarito']] = data.frame (Item = unique(estrutura$Item))

  aberto$gabarito = data.frame (Item = cod.itens)

  # fazer um loop para pegar cada item desse objeto e fazer tipo um procv no objeto
  # 'estrutura'
  for (i in 1:length (aberto[['gabarito']]$Item))
  {
    # i = 1
    cod = aberto[['gabarito']]$Item[i]
    aberto[['gabarito']]$Gabarito[i] = as.character (estrutura[[disc]]$Gabarito [which(estrutura[[disc]]$Item == cod)[1]])
  }

  return (aberto)
}
