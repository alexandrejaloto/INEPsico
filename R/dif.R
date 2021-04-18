#' @title Análise de DIF
#' @name dif
#'
#' @description Verifica existência de DIF e a qualidade do ajuste de um item calibrado no BILOG-MG
#' @param arq.exp arquivo .EXP gerado pelo BILOG-MG
#' @param arq.sco arquivo .SCO gerado pelo BILOG-MG
#' @param perc os percentis que definirão o intervalo de análise para detecção de DIF
#' @param dif.dif a diferença máxima tolerada entre a proporção de acerto observada e a esperada segundo o modelo
#' @param grupo o grupo, no arquivo .BLM, que está em análise; ou seja, o grupo focal corresponde a qual grupo no arquivo .BLM?
#' @param int.comum valor lógico: se TRUE, o intervalo a ser analisado é o compreendido entre o maior percentil inferior e o
#' menor percentil superior.
#' @param salvar argumento lógico que indica se a saída será salva em dois arquivos (um para DIF e outro para ajuste do modelo)
#'
#' @details Para analisar DIF, a função diminui a proporção de acerto do grupo focal da proporção de acerto
#'  do grupo de referência. Para verificar a qualidade do ajuste do item, a função diminui a probabilidade de
#'  acerto segundo o modelo da proporção de acerto observada.
#'
#'  Sobre o argumento int.comum: supondo que o intervalo escolhido foi perc = c (.05, .95). A análise de DIF se dará para o intervalo
#'  compreendido entre o maior P5 e o menor P95 dos dois grupos.
#'
#' O que a função faz?
#' Ela lê o arquivo expect; seleciona os itens comuns; seleciona o
#' grupo de interesse e os outros grupos; compara as duas proporções esperadas de resposta correta; seleciona
#' o grupo de interesse; compara o teórico com a proporção esperada do grupo (para análise de ajuste)
#'
#' ATENÇÃO: é necessário que o início do arquivo SCO tenha duas linhas que não são referentes a dados
#'
#' @return Uma lista com três elementos:
#'
#' $DIF
#' Contém os itens que apresentaram DIF entre o grupo focal e algum outro grupo do BILOG-MG.
#'
#' $INTERVALOS
#' Os intervalos em que ocorreram as análises. P5 e P95 é o intervalo compreendido entre o maior
#' P5 e o menor P95 de cada análise entre dois grupos. LIMITE_INF e LIMITE_SUP compreende o
#' limite em que a análise de fato ocorreu, ou seja, os pontos de quadratura no interior do
#' intervalo do P% e P95.
#'
#' $AJUSTE
#' contém os itens que apresentaram problemas de ajuste.
#'
#' @author Alexandre Jaloto
#'
#' @import LaF
#'
#' @export
dif = function (arq.exp, arq.sco, perc = list (5, 95), dif.dif = 0.15,
                grupo = 7, int.comum = TRUE, salvar = FALSE)
{
  dados = ler.exp (arq.exp)

  expect = prop.exp (dados)
  expect.g = subset (expect, GRUPO == grupo)

  escore = ler.sco (arq.sco, prog = 'BLM')

  escore.g = subset (escore, Grupo == grupo)

  grupos = length (table (escore$Grupo))

  percentil.1a = quantile (escore.g$Proficiencia, as.numeric (perc[1])/100)
  percentil.2a = quantile (escore.g$Proficiencia, as.numeric (perc[2])/100)

  #### FALTA SELECIONAR SOMENTE OS COMUNS DO GRUPO DE INTERESSE,
  #### POIS SELECIONEI TODOS OS COMUNS
  info.item = as.data.frame (table (expect$ITEM))

  comuns. = subset (info.item, Freq > 2)
  comuns = comuns.[comuns.$Var1 %in% expect.g$ITEM,]

  # se for para analisar DIF no intervalo compreendido entre o maior P5 e o menor P95
  if (int.comum == TRUE)
  {
    dif = as.data.frame (matrix (ncol = (grupos+2)))
    names (dif) = c ("Item    ", 1:(grupos), "DIF")

  intervalo = data.frame (GRUPO = 1:(grupos))
tab.dif = list()
  for (k in 1:(grupos))
  {
#k = 1
  percentil.1b = quantile (subset (escore, Grupo == k)$Proficiencia, as.numeric (perc[1])/100)
  percentil.2b = quantile (subset (escore, Grupo == k)$Proficiencia, as.numeric (perc[2])/100)

  max.p5 = max (percentil.1a, percentil.1b)
  min.p95 = min (percentil.2a, percentil.2b)

  intervalo [k,2] = max.p5
  intervalo [k,3] = min.p95

  # selecionar as colunas do objeto expect que contêm os pontos de quadratura
  # dentro do intervalo
  int.ponto = expect [, c (T, T, T, (as.numeric (names(expect)[4:43]) >=
                                       max.p5 & as.numeric (names(expect)[4:43])
                                     <= min.p95))]

  intervalo[k,4] = names (int.ponto)[4]
  intervalo[k,5] = names (int.ponto)[length (names(int.ponto))]

  tab.dif[[k]] = data.frame(matrix(ncol = ncol(int.ponto)-2))
  names (tab.dif[[k]]) = c ('ITEM', names (int.ponto)[-(1:3)])

  for (i in 1:dim (comuns)[1])
  {

    item.comum = subset (int.ponto, ITEM == comuns[i,1] )

    dif[i,1] = as.character (comuns$Var1 [i])

      comum1 = subset (item.comum, GRUPO == k)
      comum2 = subset (item.comum, GRUPO == grupo)

      dif1 = max (abs (as.numeric (comum1 [1, c (-1, -2, -3)]) - as.numeric (comum2 [1, c (-1, -2, -3)])))

      # incluir as diferenças na tabela
      tab.dif[[k]][i,] = c (as.character (comuns[i,1]), abs (as.numeric (comum1 [1, c (-1, -2, -3)]) - as.numeric (comum2 [1, c (-1, -2, -3)])))

      # verificar agora em que ponto de quadratura ocorreu a maior diferença
      ponto.dif = which (dif1 == (abs (as.numeric (comum1 [1, c (-1, -2, -3)]) - as.numeric (comum2 [1, c (-1, -2, -3)]))))

      if (!is.na (dif1))
      {
        dif[i,k+1] = as.numeric (sprintf ("%05g", dif1 ))
      }

      if (length (dif [i, c (F, !is.na (dif [i, c (-1, -(grupos+2))]))]) != 0)
      {
        dif[i,(grupos+2)] = ifelse (max (dif [i, c (F, !is.na (dif [i, c (-1, -(grupos+2))]))]) > dif.dif, 1, 0)
      }

      }

  caso.dif = subset (dif, dif[,(grupos+2)] > 0)

  caso.dif [,1] = sprintf ("%08s", caso.dif [,1] )

  # excluir as linhas de tab.dif que são NA
  # primeiro, verificar quem é tudo NA
  NA. = apply(is.na(tab.dif[[k]]), 1, sum)

  # agora, selecionar as linhas em que NA. != do número de colunas de tab.dif[[k]]
  tab.dif[[k]] = tab.dif[[k]][which (NA. != ncol(tab.dif[[k]])),]

  }

# nomear as tabelas das diferenças de cada grupo
#names (tab.dif) = paste0 ('GRUPO_', 1:grupos)

# retirar a tabela do próprio grupo
#tab.dif. = tab.dif [- grupo]

# retirar as tabelas que estão em branco
#NA. = lapply(tab.dif., nrow)
#tab.dif. = tab.dif.[which (NA. != 0)]


names (intervalo) = c ('GRUPO', 'P5', 'P95', 'LIMITE_INF', 'LIMITE_SUP')

casos = list (DIF = caso.dif [,-dim(caso.dif)[2]], INTERVALOS = intervalo)

  # se for para analisar DIF no intervalo compreendido entre o P5 e o P95 do grupo
  } else {

  # selecionar as colunas do objeto expect que contêm os pontos de quadratura
  # dentro do intervalo
  int.ponto = expect [, c (T, T, T, (as.numeric (names(expect)[4:43]) >=
                                       percentil.1a & as.numeric (names(expect)[4:43])
                                     <= percentil.2a))]

  dif = as.data.frame (matrix (ncol = (grupos+2)))
  names (dif) = c ("Item    ", 1:(grupos), "DIF")

  tab.dif = list()

  for (k in 1:(grupos))
  {
#  k = 1
  tab.dif[[k]] = data.frame(matrix(ncol = ncol(int.ponto)-2))
    names (tab.dif[[k]]) = c ('ITEM', names (int.ponto)[-(1:3)])

    for (i in 1:dim (comuns)[1])
  {
#i = 1
    item.comum = subset (int.ponto, ITEM == comuns[i,1] )

    dif[i,1] = as.character (comuns$Var1 [i])

    comum1 = subset (item.comum, GRUPO == k)
    comum2 = subset (item.comum, GRUPO == grupo)

    dif1 = max (abs (as.numeric (comum1 [1, c (-1, -2, -3)]) - as.numeric (comum2 [1, c (-1, -2, -3)])))

      # incluir as diferenças na tabela
      tab.dif[[k]][i,] = c (as.character (comuns[i,1]), abs (as.numeric (comum1 [1, c (-1, -2, -3)]) - as.numeric (comum2 [1, c (-1, -2, -3)])))

      # verificar agora em que ponto de quadratura ocorreu a maior diferença
      ponto.dif = which (dif1 == (abs (as.numeric (comum1 [1, c (-1, -2, -3)]) - as.numeric (comum2 [1, c (-1, -2, -3)]))))

      if (!is.na (dif1))
      {
        dif[i,k+1] = as.numeric (sprintf ("%05g", dif1 ))
      }

      if (length (dif [i, c (F, !is.na (dif [i, c (-1, -(grupos+2))]))]) != 0)
      {
        dif[i,(grupos+2)] = ifelse (max (dif [i, c (F, !is.na (dif [i, c (-1, -(grupos+2))]))]) > dif.dif, 1, 0)
      }

    }
    # excluir as linhas de tab.dif que são NA
    # primeiro, verificar quem é tudo NA
    NA. = apply(is.na(tab.dif[[k]]), 1, sum)

    # agora, selecionar as linhas em que NA. é menor do número de colunas de tab.dif[[k]] - 1 (a primeira coluna é o nome do item)
    tab.dif[[k]] = tab.dif[[k]][which (NA. < (ncol(tab.dif[[k]]))-1),]

    }

  caso.dif = subset (dif, dif[,(grupos+2)] > 0)

  caso.dif [,1] = sprintf ("%08s", caso.dif [,1] )

  intervalo = data.frame (GRUPO = 1:(grupos))
  intervalo$P5 = percentil.1a
  intervalo$P95 = percentil.2a
  intervalo$LIMITE_INF = names (int.ponto)[4]
  intervalo$LIMITE_SUP = names (int.ponto)[length (names(int.ponto))]

  casos = list (DIF = caso.dif [,-dim(caso.dif)[2]], INTERVALOS = intervalo)

  }

  # nomear as tabelas das diferenças de cada grupo
  names (tab.dif) = paste0 ('GRUPO_', 1:grupos)

  # se houver casos de DIF, continua construindo a tabela das diferenças das proporções
  if (nrow (casos$DIF) != 0)
  {
  tab.dif. = tab.dif
  # deixar somente os itens que apresentaram DIF
  for (k in 1:length (tab.dif.))
  {
    #k = 2

#    tab.dif.[[k]] = tab.dif.[[k]][which(tab.dif.[[k]]$ITEM == casos$DIF$`Item    `),]

    tab.dif.[[k]] = tab.dif.[[k]][which(tab.dif.[[k]]$ITEM %in% casos$DIF$`Item    `),]
    }

  # retirar as tabelas que estão em branco, incluindo a do próprio grupo
  for (k in 1:length (tab.dif.))
  {
    #k = 4
  NA. = sum (as.numeric (tab.dif.[[paste0('GRUPO_', k)]][,2]), na.rm = TRUE)

  if (NA. == 0)
{
  retirar = which (names (tab.dif.) == paste0('GRUPO_', k))

  tab.dif. = tab.dif.[-retirar]
}
  }


  for (k in 1:length (tab.dif.))
  {
  # excluir as linhas de tab.dif que são NA
  # primeiro, verificar quem é tudo NA
  NA. = apply(is.na(tab.dif.[[k]]), 1, sum)

  # agora, selecionar as linhas em que NA. != do número de colunas de tab.dif[[k]] - 1 (porque uma coluna é o nome do item)
  tab.dif.[[k]] = tab.dif.[[k]][which (NA. != (ncol(tab.dif.[[k]])-1)),]
  }

  # se não houver casos de DIF, tab.dif. será uma lista vazia
  } else {tab.dif. = list()}


  # AJUSTE
  # AJUSTE
  # AJUSTE
  # AJUSTE

  int.ponto = expect [, c (T, T, T, (as.numeric (names(expect)[4:43]) >=
                                       percentil.1a & as.numeric (names(expect)[4:43])
                                     <= percentil.2a))]

  ajuste.g = subset (int.ponto, GRUPO == grupo)

  ajuste = as.data.frame (matrix (ncol = 3))
  names (ajuste) = c ("Item    ", "Máximo", "Ajuste")

  tab.ajuste = data.frame(matrix(ncol = ncol(int.ponto)-2))
  names (tab.ajuste) = c ('ITEM', names (int.ponto)[-(1:3)])


  for (i in 1:(dim (ajuste.g)[1]/2))
  {
    aj = as.numeric (ajuste.g [1 + ((i-1)*2), c(-(1:3))]) - as.numeric (ajuste.g [2 + ((i-1)*2), c(-(1:3))])

    ajuste [i,1] = ajuste.g [1 + ((i-1)*2),1]
    ajuste [i,2] = abs (as.numeric (sprintf ("%05g", max (abs(aj)) )))
    ajuste [i,3] = ifelse (max (abs(aj)) > dif.dif, 1, 0)

    tab.ajuste[i,] = c (ajuste.g [1 + ((i-1)*2), 1], abs (aj))

    }

  caso.ajuste = subset (ajuste, ajuste$Ajuste > 0)

  caso.ajuste [,1] = sprintf ("%08s", caso.ajuste [,1] )

  # deixar somente os itens que apresentaram DIF
  tab.ajuste = tab.ajuste[which(tab.ajuste$ITEM %in% caso.ajuste$`Item    `),]

  casos$AJUSTE = caso.ajuste[,-3]
  casos$proporcoes_DIF = tab.dif.
  casos$proporcoes_AJUSTE = tab.ajuste

  if (salvar == TRUE)
  {
  sink("DIF.txt")
  print (casos)
  sink()
  }

  return (casos)
}
