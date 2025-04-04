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
#' de acerto no grupo superior; Correlação bisserial (mesma fórmula do do
#' BILOG-MG, porém incluindo o item no escore);
#' Correlação bisserial
#' robusta (escore sem o item analisado; igual à do BILOG); Correlação de
#' Parson robusta (escore sem o item analisado);
#' Proporção de
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
#' itens = rbind (itens.lc, itens.mt)

#' resp = matrix (sample (LETTERS[1:4], 12*30, replace = TRUE), ncol = 12)
#' banco = data.frame (CAD = seq(1:3), resp)
#' disc = 'LC'
#' aberto = abre.banco (banco = banco, itens = itens, bib = bib, disc = disc)
#'
#' tct = tct (banco.aberto = aberto$respostas, gab.aberto = aberto$gabarito)
#' @export

tct = function (banco.aberto, gab.aberto, alt = c ('A', 'B', 'C', 'D', '.', '*'), usa.normit = TRUE, met.perc = 6, pop = FALSE)
{

  names(banco.aberto)[1] <- 'CADERNO'

  # CALCULAR ESCORE TOTAL

  # número total de alternativas (incluindo branco e dupla marcação)
  n.alt = length (alt)

  # corrigir o teste (1 = acerto; 0 = erro)
  correcao = mirt::key2binary (banco.aberto[,c(-1)], gab.aberto[,2])

  # acresentar a variável da soma de acertos
  banco.aberto$ACERTOS = rowSums (correcao, na.rm = TRUE)

  # total de cadernos
  tot.cad = max(banco.aberto[,1])

  # número total de itens
  n.item = dim (banco.aberto)[2] - 2

  if (usa.normit == TRUE)
  {
    # NORMIT ----

    ########################### FALTA DEFINIR SE O PERCENTIL SERÁ DOS SUBMETIDOS OU DA POPULAÇÃO TOTAL ###########################
    ########################### FALTA DEFINIR SE O PERCENTIL SERÁ DOS SUBMETIDOS OU DA POPULAÇÃO TOTAL ###########################
    ########################### FALTA DEFINIR SE O PERCENTIL SERÁ DOS SUBMETIDOS OU DA POPULAÇÃO TOTAL ###########################
    ########################### FALTA DEFINIR SE O PERCENTIL SERÁ DOS SUBMETIDOS OU DA POPULAÇÃO TOTAL ###########################
    ########################### FALTA DEFINIR SE O PERCENTIL SERÁ DOS SUBMETIDOS OU DA POPULAÇÃO TOTAL ###########################
    ########################### FALTA DEFINIR SE O PERCENTIL SERÁ DOS SUBMETIDOS OU DA POPULAÇÃO TOTAL ###########################
    ########################### FALTA DEFINIR SE O PERCENTIL SERÁ DOS SUBMETIDOS OU DA POPULAÇÃO TOTAL ###########################
    ########################### FALTA DEFINIR SE O PERCENTIL SERÁ DOS SUBMETIDOS OU DA POPULAÇÃO TOTAL ###########################

    banco.aberto$NORMIT = calcular_normit(banco.aberto = banco.aberto, tot.cad = tot.cad, escore = banco.aberto$ACERTOS)

    normit = banco.aberto
  }

  # CALCULAR BISSERIAL E PROPORÇÃO ----

  # Função auxiliar para calcular proporção
  calcular_proporcao <- function(item, gabarito) {
    acertou <- item[item == gabarito]
    tentativas <- item[!is.na(item) & item != "9"]
    sum(!is.na(acertou)) / length(tentativas)
  }

  # Calcular bisserial e proporção para cada item
  biserial.item <- sapply(1:n.item, function(i) {
    calcular_bisserial(banco.aberto[, i + 1], if (usa.normit) banco.aberto$NORMIT else banco.aberto$ACERTOS, gab.aberto[i, 2], pop)
  })

  biserial.item <- data.frame(biserial.item)

  proporcao.item <- sapply(1:n.item, function(i) {
    calcular_proporcao(banco.aberto[, i + 1], gab.aberto[i, 2])
  })

  proporcao.item <- data.frame(proporcao.item)

  rownames(biserial.item) <- gab.aberto[, 1]
  rownames(proporcao.item) <- gab.aberto[, 1]

  # Calcular bisserial e proporção para cada alternativa de cada item
  biserial <- sapply(1:n.item, function(i) {
    sapply(1:n.alt, function(j) {
      calcular_bisserial(banco.aberto[, i + 1], if (usa.normit) banco.aberto$NORMIT else banco.aberto$ACERTOS, alt[j], pop)
    })
  })

  biserial <- t(biserial)

  proporcao <- sapply(1:n.item, function(i) {
    sapply(1:n.alt, function(j) {
      calcular_proporcao(banco.aberto[, i + 1], alt[j])
    })
  })

  proporcao <- t(proporcao)

  colnames(biserial) <- paste("Bis_", alt, sep = "")
  colnames(proporcao) <- paste("Prop_", alt, sep = "")
  rownames(biserial) <- gab.aberto[, 1]
  rownames(proporcao) <- gab.aberto[, 1]

  # correlação robusta ----

  biserial_robusta <- sapply(1:n.item, function(item_index) {
    escore_sem_item <- rowSums(correcao[,-item_index], na.rm = TRUE)
    if (usa.normit)
      escore_sem_item <- calcular_normit(banco.aberto = banco.aberto[,-(item_index+1)], tot.cad = tot.cad, escore = escore_sem_item)
    calcular_bisserial(resp.item = banco.aberto[, item_index + 1], escore = escore_sem_item, gabarito = gab.aberto[item_index, 2], pop = pop)
  })

  pearson_robusta <- sapply(1:n.item, function(item_index) {
    escore_sem_item <- rowSums(correcao[,-item_index], na.rm = TRUE)
    if (usa.normit)
      escore_sem_item <- calcular_normit(banco.aberto = banco.aberto[,-(item_index+1)], tot.cad = tot.cad, escore = escore_sem_item)
    cor(correcao[,item_index], escore_sem_item, use = 'complete.obs')
  })

  # ÍNDICE D ----

  # Função auxiliar para calcular o índice D
  calcular_indice_d <- function(item_coluna, escore, gabarito, met.perc) {

    tentativas <- escore[!is.na(item_coluna) & item_coluna != "9"]
    valor.perc.1 <- quantile(tentativas, 0.27, type = met.perc)
    valor.perc.2 <- quantile(tentativas, 0.73, type = met.perc)
    perc.1 <- item_coluna[escore <= valor.perc.1]
    perc.2 <- item_coluna[escore >= valor.perc.2]
    acerto.perc1 <- sum(perc.1 == gabarito, na.rm = TRUE) / sum(!is.na(perc.1))
    acerto.perc2 <- sum(perc.2 == gabarito, na.rm = TRUE) / sum(!is.na(perc.2))
    c(acerto.perc2 - acerto.perc1, acerto.perc1, acerto.perc2)
  }

  # Calcular o índice D para cada item
  escore <- if (usa.normit) banco.aberto$NORMIT else banco.aberto$ACERTOS
  D <- sapply(1:n.item, function(i) {
    calcular_indice_d(banco.aberto[, i + 1], escore, gab.aberto[i, 2], met.perc)
  })

  # Transpor a matriz D para o formato desejado
  D <- t(D)

  # TABELA FINAL ----

  numit = data.frame (1:n.item)

  tabela = data.frame (numit, gab.aberto[,1], gab.aberto[,2], proporcao.item, D, biserial.item, biserial_robusta, pearson_robusta, proporcao, biserial)
  names (tabela) = c ("Seq", "Item", "Gabarito", 'DIFI', 'DISC', 'ABAI', 'ACIM', 'BISE', 'BISE_rob', 'PEARSON_rob', colnames (proporcao), colnames(biserial))

  if (usa.normit == TRUE)
  {
    tabela = list(tabela, normit)
    names (tabela) = c ('tct', 'normit')
  }

  return (tabela)
}
