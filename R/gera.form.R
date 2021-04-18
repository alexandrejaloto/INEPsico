#' @title Gera os forms e os gabaritos para o BILOG-MG
#' @name gera.form
#'
#' @description A partir das informaçõs do BIB e dos itens, esta função gera
#' dois arquivos 'txt' para serem incorporados aos arquivos do BILOG-MG.
#' São os forms, que devem ser adicionados no BLM, e os gabaritos.
#'
#' @param itens O(s) objeto(s) com os itens da(s) disciplina(s) que compõe(m) os
#' cadernos no BIB (tem que ser todos, por enquanto; depois tem que melhorar a função).
#' ATENÇÃO: precisa ser data.frame
#' @param bib Objeto com o BIB. ATENÇÃO: precisa ser data.frame
#' @param disc.cad Quantidade de disciplinas em cada caderno
#' @param cad.1 Número do primeiro form, no BILOG-MG, referente aos cadernos de cada disciplina
#' @param carac Quantidade de caracteres antes da leitura do vetor de resposta no BILOG-MG
#'
#' @return Para cada disciplina, dois arquivos 'txt':
#'
#'     1. arquivo com os forms para o BILOG-MG
#'
#'     2. arquivo com os gabaritos dos forms para o BILOG-MG
#'
#'
#' @author Alexandre Jaloto
#'
#' @import stringr
#'
#'@examples
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
#'
#' gera.form (itens, bib, cad.1 = c (12, 14))
#' @export
gera.form = function (itens, bib, disc.cad = 2, cad.1 = c (1, 1),
                      carac = 34)
{
  if (class (itens) != 'data.frame')
  {stop('itens precisa ser um objeto do tipo data.frame')}

  if (class (bib) != 'data.frame')
  {stop('bib precisa ser um objeto do tipo data.frame')}

  # nomes das disciplinas
  disciplinas = as.character (unique (unlist (bib[,paste0('Disciplina', 1:disc.cad)])))

  # número total de cadernos
  tot.cad = max (bib$Caderno)

  # quantidade de blocos por caderno
  blocos.cad = sum (grepl("Bloco", names(bib)))

  # quantidade de blocos de cada disciplina em cada caderno
  blocos.disc = blocos.cad/disc.cad

  # quantidade de blocos de cada disciplina
  blocos = max (bib [, paste0 ('Bloco', 1:blocos.cad)])

  # nome dos arquivos a serem salvos com os form do BILOG-MG
  form = paste ('form_', disciplinas, '.txt', sep = "")

  # nome dos arquivos a serem salvos com os gabaritos
  arq.gab = paste ('gab_', disciplinas, '.txt', sep = "")

  # gerar estruturas dos cadernos de cada disciplina
  est.disc = gera.caderno (itens, bib, disc.cad)

  # quantidade de itens por bloco
  num.itens. = data.frame()
  for (i in 1:disc.cad)
  {
    num.itens. [i,1] = max (est.disc[[i]]$Posicao)
  }
  num.itens = max(num.itens.)

  ########################### GERAR OS ARQUIVOS DE FORM ###########################
  for (j in 1:length (disciplinas))
  {
    caderno = data.frame (est.disc[j])

    num.bilog = sprintf ("%03s", caderno[,6]) # num_bilog com 3 caracteres

    caderno[,6] = num.bilog

    num = matrix (ncol = blocos.cad*num.itens+2, nrow = tot.cad)
    num [,blocos.cad*num.itens+2] = ");"

    arquivo.form = file (form[j], "w+")		# criar arquivo para salvar os forms

    for (i in 0:(tot.cad-1))
    {

      a = i*blocos.cad*num.itens + 1
      b = i*blocos.cad*num.itens + blocos.cad*num.itens
      num [i+1,2] = caderno [a,6]
      num [i+1,3:(blocos.cad*num.itens+1)] = paste (", ", caderno [(a+1):b,6], sep = "")
      num [i+1, 1] = paste (">FORM", i+cad.1[j], " LENGTH= ", blocos.cad*num.itens, ", INUMBERS=(", sep = "")

      cat ( stringr::str_wrap (paste (num [i+1,], collapse = ""), width = 60), "\n", file = arquivo.form)		# escrever os forms no arquivo
    }

  }

  ########################### GERAR OS ARQUIVOS DE GABARITO ###########################

  for (j in 1:length (disciplinas))
  {
    caderno = data.frame (est.disc[j])
    gab = matrix (ncol = blocos.cad*num.itens+1, nrow = tot.cad)

    for (i in 0:(tot.cad-1))
    {

      a = i*blocos.cad*num.itens + 1
      b = i*blocos.cad*num.itens + blocos.cad*num.itens
      gab [i+1,2:(blocos.cad*num.itens+1)] = as.character (caderno [a:b,5])
      gab [i+1, 1] = paste (sprintf ("%03s", i+cad.1[j]), stringi::stri_dup (" ", carac-3), sep="")
    }
    gdata::write.fwf (gab, arq.gab[j], sep = "", colnames = FALSE, rownames = FALSE)
  }
}

