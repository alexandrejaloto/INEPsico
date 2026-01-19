#' @title Grar gráficos AGI
#' @name grafico.agi
#'
#' @description Gerar gráficos para Análise Gráfica do Item (AGI)
#'
#' @param banco `data.frame` cuja primeira variável é o `ID` dos sujeitos
#' e as demais são as respostas aos itens.
#' @param gabarito `data.frame` cuja primeira variável é o nome dos
#' itens e a segunda, o gabarito do item
#' @param escore vetor com os escores dos sujeitos
#' @param cortes pontos de corte para classificar os sujeitos de acordo
#' com o escore
#'
#' @return Retorna uma lista nomeada de objetos da classe `ggplot2`.
#' Cada elemento da lista é um gráfico que representa a Análise Gráfica
#' do Item (AGI) para um item específico. Os nomes dos elementos da
#' lista correspondem aos números dos itens (ex: `lista_graficos[[1]]`
#' contém o gráfico do item 1).
#'
#' @author Alexandre Jaloto
#'
#' @examples
#'
#' data(banco.sim.3PL)
#' banco <- banco.sim.3PL$respostas
#' gabarito <- banco.sim.3PL$gabarito
#' correcao <- mirt::key2binary(banco[,-1], gabarito$Gabarito)
#' escore <- rowSums(correcao, na.rm = TRUE)
#' grafico <- grafico.agi(banco = banco, gabarito = gabarito, escore = escore, cortes = seq(0, 45, 9))
#' plot(grafico[[1]])
#'
#' @export

grafico.agi <- function(banco, gabarito, escore, cortes)
{

  classificacao <- cut(escore, cortes, labels = cortes[-1])

  levels_ordenados <- as.character(cortes[-1]) # ordem desejada
  classificacao <- factor(classificacao, levels = levels_ordenados, ordered = TRUE) # cria um fator ordenado

  # Adiciona a classificação ao banco de dados original
  banco$Classificacao <- (classificacao)

  # Iterando pelos itens e calculando as proporções
  lista_dados_grafico <- list() # lista para armazenar os dados de cada item
  for (item in 1:nrow(gabarito)) {

    # item <- 1

    dados_item <- data.frame() # data.frame para armazenar os dados do item atual

    for (nivel in levels(classificacao)) {

      filtro <- banco[banco$Classificacao == nivel, 1 + item] #acessa diretamente a coluna do item

      for (alt in LETTERS[1:5]) {
        prop <- prop.table(table(filtro))[alt]

        dados_item <- rbind(dados_item, data.frame(Item = item, Classificacao = nivel, Alternativa = alt, Proporcao = ifelse(is.na(prop[[1]]), 0, prop)))
      }
    }
    dados_item$Classificacao <- factor(dados_item$Classificacao, levels = levels_ordenados, ordered = TRUE) # cria um fator ordenado
    lista_dados_grafico[[item]] <- dados_item
  }

  # Criando os gráficos com ggplot2
  lista_graficos <- list()
  for(item in 1:nrow(gabarito)){
    grafico <- ggplot2::ggplot(lista_dados_grafico[[item]], ggplot2::aes(x = Classificacao, y = Proporcao, group = Alternativa, color = Alternativa)) +
      ggplot2::geom_line(size = 1.2) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_text(ggplot2::aes(label = Alternativa), nudge_y = 0.02, size = 5) +
      ggplot2::labs(title = paste("Item", item),
                    x = "Nível",
                    y = "Proporção",
                    color = "Alternativa") +
      ggplot2::theme_bw() +
      ggplot2::theme(text = ggplot2::element_text(size=16))
    lista_graficos[[item]] <- grafico
  }

  # Exibindo um gráfico (exemplo: item 1)
  lista_graficos[[1]]

  return(lista_graficos)

}
