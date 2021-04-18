#' @title Análise via Teoria Clássica dos Testes (TCT)
#' @name tct
#'
#' @description Análise psicométrica de itens por meio da TCT
#'
#' @param banco.aberto Objeto do tipo data.frame ou matrix cuja
#' primeira variável é o número do caderno e as demais variáveis
#' são as respostas a cada item; é necessário que o banco esteja
#' aberto e os cadernos estejam ordenados a partir do 1; importante:
#' o banco já tem que ser somente da disciplina que será analisada
#' @param gab.aberto Objeto do tipo data.frame com duas variáveis:
#' código do item e gabarito; é necessário que a ordem dos itens seja a
#' mesma da ordem do objeto banco.aberto
#' @param alt As alternativas possíveis em cada item
#' @param usa.normit Valor lógico que indica se o escore utilizado para
#' a análise é o normit (TRUE) ou a soma de acertos (FALSE)
#' @param met.perc O método utilizado para o cálculo do percentil. Varia de 1 a 9. Para mais informações, verifique ajuda da função
#' quantile.
#'
#' @param pop TRUE se for população, FALSE (padrão) se for uma amostra. Essa escolha interfere nas contas que envolvem o cálculo da
#' variância ou do desvio padrão.
#'
#' @details A análise utiliza análise via normit. Para os cálculos que
#' envolvem o desvio padrão, considera-se a raiz da variância da
#' população; a função var considera n-1, assim como a função sd.
#'
#' @return A função retorna um objeto do tipo list com os seguintes elementos:
#'
#' $tct
#' Dados dos itens e da análise, quais sejam: Número sequencial do item;
#' Código do item; Gabarito do item; Índice de dificuldade; Índice de
#' discriminação; Porcentagem de acerto no grupo inferior; Porcentagem
#' de acerto no grupo superior; Correlação bisserial; Proporção de
#' escolha de cada alternativa; Correlação bisserial de cada alternativa.
#'
#' $normit
#' Dados dos indivíduos, quais sejam: Caderno apresentado; Resposta a
#' cada item (banco aberto); Soma de acertos; Normit.
#'
#' @author Alexandre Jaloto
#'
#' @examples
#' # criar um banco aberto
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
#'                   itens = rbind (itens.lc, itens.mt)
#' resp = matrix (sample (LETTERS[1:4], 180, replace = TRUE), ncol = 6)
#' banco = data.frame (CAD = seq(1:3), resp)
#' disc = 'LC'
#' aberto = abre.banco (banco = banco, itens = itens, bib = bib, disc = disc)
#'
#' tct = tct (banco.aberto = aberto$respostas, gab.aberto = aberto$gabarito)
#' @export

tct = function (banco.aberto, gab.aberto, alt = c ('A', 'B', 'C', 'D', '.', '*'), usa.normit = TRUE, met.perc = 6, pop = FALSE)
{

  ########################################## CALCULAR ESCORE TOTAL ##########################################
  ########################################## CALCULAR ESCORE TOTAL ##########################################
  ########################################## CALCULAR ESCORE TOTAL ##########################################

  # número total de alternativas (incluindo branco e dupla marcação)
  n.alt = length (alt)

  # corrigir o teste (1 = acerto; 0 = erro)
  correcao.1 = mirt::key2binary (banco.aberto[,c(-1)], gab.aberto[,2])
  # alguns são NA, e o mirt corrige como 0; ou seja, precisa colocar o NA
  correcao = ifelse (is.na(banco.aberto[,c(-1)]) == T, NA, correcao.1)

  # acresentar a variável da soma de acertos
  banco.aberto$ACERTOS = rowSums (correcao.1, na.rm = TRUE)

  # total de cadernos
  tot.cad = max(banco.aberto[,1])

  # número total de itens
  n.item = dim (banco.aberto)[2] - 2

  if (usa.normit == TRUE)
  {
    ########################################## NORMIT ##########################################
    ########################################## NORMIT ##########################################

    ########################### FALTA DEFINIR SE O PERCENTIL SERÁ DOS SUBMETIDOS OU DA POPULAÇÃO TOTAL ###########################
    ########################### FALTA DEFINIR SE O PERCENTIL SERÁ DOS SUBMETIDOS OU DA POPULAÇÃO TOTAL ###########################
    ########################### FALTA DEFINIR SE O PERCENTIL SERÁ DOS SUBMETIDOS OU DA POPULAÇÃO TOTAL ###########################
    ########################### FALTA DEFINIR SE O PERCENTIL SERÁ DOS SUBMETIDOS OU DA POPULAÇÃO TOTAL ###########################
    ########################### FALTA DEFINIR SE O PERCENTIL SERÁ DOS SUBMETIDOS OU DA POPULAÇÃO TOTAL ###########################
    ########################### FALTA DEFINIR SE O PERCENTIL SERÁ DOS SUBMETIDOS OU DA POPULAÇÃO TOTAL ###########################
    ########################### FALTA DEFINIR SE O PERCENTIL SERÁ DOS SUBMETIDOS OU DA POPULAÇÃO TOTAL ###########################
    ########################### FALTA DEFINIR SE O PERCENTIL SERÁ DOS SUBMETIDOS OU DA POPULAÇÃO TOTAL ###########################

    banco.aberto$NORMIT = NA

    # para escore = 0 e escore = 1
    for (j in 1:tot.cad)
    {
      # selecionar os participantes do caderno j
      cad = length(which(banco.aberto[,1] == j))

      # selecionar participantes com escore 0 do caderno 1
      tirou.0 = which(banco.aberto$ACERTOS == 0 & banco.aberto[,1] == j)
      # quantidade de participantes com escore 0 do caderno j
      escore.0 = length (tirou.0)

      # selecionar participantes com escore até 1 do caderno 1
      tirou.1 = which(banco.aberto$ACERTOS <= 1 & banco.aberto[,1] == j)

      # quantidade de participantes com escore até 1 do caderno j
      escore.1 = length (tirou.1)

      # calcular o normit de quem acertou 0
      normit0 = qnorm ((escore.0 + (escore.1/2))/(2*cad))

      # calcular o normit de quem acertou 1
      normit1 = qnorm ((escore.0 + escore.1)/(2*cad))

      # selecionar as pessoas que acertaram 0 e são do caderno j e colocar o normit
      # antes, tem que ver se existe alguém (tamanho != 0)
      if (dim(subset(banco.aberto, ACERTOS == 0 & banco.aberto[,1] == j))[1] != 0)
      {
        # atribuir o valor de normit às pessoas do caderno j que tiveram 0 acertos
        banco.aberto [which (banco.aberto$ACERTOS == 0
                             & banco.aberto$CADERNO == j),]$NORMIT = normit0
      }

      # selecionar as pessoas que acertaram 1 e são do caderno j e colocar o normit
      # antes, tem que ver se existe alguém (tamanho != 0)
      if (dim (subset (banco.aberto, ACERTOS == 1 & banco.aberto[,1] == j))[1] !=0 )
      {
        # atribuir o valor de normit às pessoas do caderno j que tiveram 1 acerto
        banco.aberto [which (banco.aberto$ACERTOS == 1
                             & banco.aberto[,1] == j),]$NORMIT = normit1
      }

      # máximo de acertos do caderno j
      if (nrow (subset(banco.aberto, banco.aberto[, 1] == j)) == 0)
        stop (paste0 ('Não existem respondentes para o caderno ', j))

      max.acertos = max (subset (banco.aberto, banco.aberto[,1] == j)$ACERTOS)
      for (i in 2:max.acertos)
      {
        # selecionar participantes com escore até i do caderno
        tirou.i = which (banco.aberto$ACERTOS <= i &
                           banco.aberto[,1] == j)
        # quantas pessoas acertaram até i?
        escore.i = length (tirou.i)

        # selecionar participantes com escore até i-1 do caderno
        tirou.i.menor = which (banco.aberto$ACERTOS <= i-1 &
                                 banco.aberto[,1] == j)
        # quantas pessoas acertaram até i?
        escore.i.menor = length (tirou.i.menor)

        # calcular normit de quem acertou i
        normit = qnorm ((escore.i.menor + escore.i)/(2*cad))

        # selecionar as pessoas que acertaram i e são do caderno j e colocar o normit
        # antes, tem que ver se existe alguém (tamanho != 0)
        if (dim (subset (banco.aberto, ACERTOS == i & banco.aberto[,1] == j))[1] != 0)
        {
          # atribuir o valor de normit às pessoas do caderno j que tiveram i acertos
          banco.aberto [which (banco.aberto$ACERTOS == i
                               & banco.aberto[,1] == j),]$NORMIT = normit
        }

      }
    }


    normit = banco.aberto
  }
  ########################################## CALCULAR BISSERIAL E PROPORÇÃO ##########################################
  ########################################## CALCULAR BISSERIAL E PROPORÇÃO ##########################################
  ########################################## CALCULAR BISSERIAL E PROPORÇÃO ##########################################
  ########################################## CALCULAR BISSERIAL E PROPORÇÃO ##########################################
  ########################################## CALCULAR BISSERIAL E PROPORÇÃO ##########################################
  ########################################## CALCULAR BISSERIAL E PROPORÇÃO ##########################################


  # calcular bisserial e proporção de acerto do item
  biserial.item = matrix (nrow = n.item)
  proporcao.item = matrix (nrow = n.item)

  # i vai de 1 até o número total de itens
  for (i in 1:n.item)
  {

    # selecionar quem acertou o item i (precisa do '+ 1' porque a primeira variávelo é o caderno)
    acertou = subset (banco.aberto, banco.aberto[,1 + i] ==
                        gab.aberto[i,2])

    # selecionar só quem tentou o item de fato (desconsiderar o não apresentado - considera-se que o '9' também é não apresentado)
    tentativas = subset (banco.aberto, banco.aberto[,1 + i] != "9"
                         & banco.aberto[,1 + i] != "NA")
    if (usa.normit == TRUE)
    {
      Mp2 = mean (acertou$NORMIT)
      M2 = mean (tentativas$NORMIT)

      # se for a população
      if (pop == TRUE)
      {
        # o desvio aqui está sendo a raiz da variância da população; a função var considera n-1, assim como a função sd
        S2 = sqrt (sum (((tentativas$NORMIT-mean (tentativas$NORMIT))^2)/(dim(tentativas)[1])))

        # se for uma amostra
      } else {
        S2 = sd (tentativas$NORMIT)
      }

      p2 = dim (acertou)[1] / dim (tentativas)[1]
      hp2 = exp((-qnorm (p2)^2)/2)/sqrt(2*pi)
      bis2 = ((Mp2 - M2)/S2) * (p2 / hp2)
    } else {
      Mp2 = mean (acertou$ACERTOS)
      M2 = mean (tentativas$ACERTOS)

      # se for a população
      if (pop == TRUE)
      {
        # o desvio aqui está sendo a raiz da variância da população; a função var considera n-1, assim como a função sd
        S2 = sqrt (sum (((tentativas$ACERTOS-mean (tentativas$ACERTOS))^2)/(dim(tentativas)[1])))

        # se for uma amostra
      } else {
        S2 = sd (tentativas$ACERTOS)
      }

      p2 = dim (acertou)[1] / dim (tentativas)[1]
      hp2 = exp((-qnorm (p2)^2)/2)/sqrt(2*pi)
      bis2 = ((Mp2 - M2)/S2) * (p2 / hp2)
    }
    biserial.item [i] = bis2
    proporcao.item [i] = p2
  }

  rownames (biserial.item) = gab.aberto[,1]
  rownames (proporcao.item) = gab.aberto[,1]

  # calculando a bisserial e a proporção de cada alternativa de cada item
  biserial = matrix (nrow = n.item, ncol = n.alt)
  proporcao = matrix (nrow = n.item, ncol = n.alt)

  for (i in 1:n.item)
  {
    for (j in 1:n.alt)
    {

      # selecionar quem marcou a alternativa alt[j]
      distrator = subset (banco.aberto, banco.aberto[,1 + i] == alt[j])

      # selecionar só quem tentou o item de fato (desconsiderar o não apresentado)
      tentativas = subset (banco.aberto, banco.aberto[,1 + i] != "9"
                           & banco.aberto[,1 + i] != "NA")

      if (usa.normit == TRUE)
      {
        Mp2 = mean (distrator$NORMIT)
        M2 = mean (tentativas$NORMIT)

        # se for a população
        if (pop == TRUE)
        {
          # o desvio aqui está sendo a raiz da variância da população; a função var considera n-1, assim como a função sd
          S2 = sqrt (sum (((tentativas$NORMIT-mean (tentativas$NORMIT))^2)/(dim(tentativas)[1])))

          # se for uma amostra
        } else {
          S2 = sd (tentativas$NORMIT)
        }

        p2 = dim (distrator)[1] / dim (tentativas)[1]
        hp2 = exp((-qnorm (p2)^2)/2)/sqrt(2*pi)
        bis2 = ((Mp2 - M2)/S2) * (p2 / hp2)
      } else {
        Mp2 = mean (distrator$ACERTOS)
        M2 = mean (tentativas$ACERTOS)

        # se for a população
        if (pop == TRUE)
        {
          # o desvio aqui está sendo a raiz da variância da população; a função var considera n-1, assim como a função sd
          S2 = sqrt (sum (((tentativas$ACERTOS-mean (tentativas$ACERTOS))^2)/(dim(tentativas)[1])))

          # se for uma amostra
        } else {
          S2 = sd (tentativas$ACERTOS)
        }

        p2 = dim (distrator)[1] / dim (tentativas)[1]
        hp2 = exp((-qnorm (p2)^2)/2)/sqrt(2*pi)
        bis2 = ((Mp2 - M2)/S2) * (p2 / hp2)
      }

      biserial [i, j] = bis2
      proporcao [i,j] = p2

    }
  }

  colnames (biserial) = c ( paste ("Bis_", alt, sep = ""))
  colnames (proporcao) = c ( paste ("Prop_", alt, sep = ""))

  rownames (biserial) = gab.aberto[,1]
  rownames (proporcao) = gab.aberto[,1]


  ########################################## ÍNDICE D ##########################################
  ########################################## ÍNDICE D ##########################################
  ########################################## ÍNDICE D ##########################################
  ########################################## ÍNDICE D ##########################################
  ########################################## ÍNDICE D ##########################################
  ########################################## ÍNDICE D ##########################################

  # criar matriz para adicionar os índices D de cada item
  D = matrix (nrow = n.item, ncol = 3)

  for (i in 1:n.item)
  {

    # selecionar só quem tentou o item de fato (desconsiderar o não apresentado)
    tentativas = subset (banco.aberto, banco.aberto[,1 + i] != "9"
                         & banco.aberto[,1 + i] != "NA")

    # se for usar NORMIT
    if (usa.normit == TRUE)
    {
      # os dois comandos abaixo são para o caso de indicar o P27 e o P73 do grupo que foi submetido ao item
        valor.perc.1 = quantile (tentativas$NORMIT, 0.27, type = met.perc)		# verificar o valor do percentil 73
        valor.perc.2 = quantile (tentativas$NORMIT, 0.73, type = met.perc)		# verificar o valor do percentil 27

      # selecionar o percentil 73 (os 27% com menor escore, considerando normit)
      perc.1 = subset (tentativas, NORMIT <= valor.perc.1)

      # selecionar o percentil 27 (os 27% com maior escore, considerando normit)
      perc.2 = subset (tentativas, NORMIT >= valor.perc.2)

      # se não usar NORMIT

    } else {


        # os dois comandos abaixo são para o caso de indicar o P27 e o P73 do grupo que foi submetido ao item
        valor.perc.1 = quantile (tentativas$ACERTOS, 0.27, type = met.perc)		# verificar o valor do percentil 73
        valor.perc.2 = quantile (tentativas$ACERTOS, 0.73, type = met.perc)		# verificar o valor do percentil 27

      # selecionar o percentil 73 (os 27% com menor escore, considerando o escore)
      perc.1 = subset (tentativas, ACERTOS <= valor.perc.1)

      # selecionar o percentil 27 (os 27% com maior escore, considerando o escore)
      perc.2 = subset (tentativas, ACERTOS >= valor.perc.2)
    }

    # selecionar os que acertaram do perc.1
    acerto.perc1 = subset (perc.1, perc.1[,1 + i] == gab.aberto[,2][i])

    # selecionar os que acertaram do perc.2
    acerto.perc2 = subset (perc.2, perc.2[,1 + i] == gab.aberto[,2][i])

    # valor de "acima"
    acima = dim(acerto.perc2)[1] / dim(perc.2)[1]
    # valor de "abaixo"
    abaixo = dim(acerto.perc1)[1] / dim(perc.1)[1]

    D[i,1] = acima - abaixo
    D[i,3] = acima
    D[i,2] = abaixo
  }


  ########################################## TABELA FINAL ##########################################
  ########################################## TABELA FINAL ##########################################
  ########################################## TABELA FINAL ##########################################

  numit = data.frame (1:n.item)

  tct = data.frame (numit, gab.aberto[,1], gab.aberto[,2], proporcao.item, D, biserial.item, proporcao, biserial)
  names (tct) = c ("Seq", "Item", "Gabarito", 'DIFI', 'DISC', 'ABAI', 'ACIM', 'BISE',colnames (proporcao), colnames(biserial))

  if (usa.normit == TRUE)
  {tct = list(tct, normit)
  names (tct) = c ('tct', 'normit')}

  return (tct)
}
