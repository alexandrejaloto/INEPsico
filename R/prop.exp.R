#' @title Proporções esperadas
#' @name prop.exp
#'
#' @description "Abre" o objeto gerado pela função ler.exp. Ou seja, verifica as
#' proporções empíricas e do modelo de acerto do item em cada ponto de quadratura.
#' @param dados objeto gerado pela função ler.exp
#' @return data.frame com o nome do item no BILOG-MG, o grupo em que foi aplicado, a
#' variável analisada (PROPORÇÃO ou PROPORÇÃO DO MODELO), e a proporção em cada ponto de
#' quadratura.
#'
#' @author Alexandre Jaloto
#'
#' @export

prop.exp = function (dados)
{
expect = as.data.frame(matrix (ncol = 43))
tamanho.total = dim(dados)[1]

# o loop vai até tamanho.total/48 porque são 8 colunas e seis variáveis (POINT, WEIGHT, TRIED, RIGHT,
# PROPORTION, MODEL PROP)
for (k in 1:(tamanho.total/48))
{

  # ITEM
  expect [((k-1)*2+1),1] = as.character (dados [((k-1)*48+1),1])
  expect [((k-1)*2+2),1] = as.character (dados [((k-1)*48+1),1])

  # GRUPO
  expect [((k-1)*2+1),2] = dados [((k-1)*48+1),2]
  expect [((k-1)*2+2),2] = dados [((k-1)*48+1),2]

  # VARIÁVEL
  expect [((k-1)*2+1),3] = "PROPORCAO"
  expect [((k-1)*2+2),3] = "PROPORCAO_MODELO"

  for (i in 1:8)
  {
    # incluir os valores de proporção
    expect [((k-1)*2+1),((i-1)*5+4):((i-1)*5+8)] = dados [((k-1)*48+((i-1)*6+5)),4:8]

    # incluir os valores do modelo
    expect [((k-1)*2+2),((i-1)*5+4):((i-1)*5+8)] = dados [((k-1)*48+((i-1)*6+6)),4:8]
  }
}

names (expect) = c ("ITEM", "GRUPO", "VARIAVEL", dados [1, 4:8],
                    dados [7, 4:8], dados [13, 4:8], dados [19, 4:8],
                    dados [25, 4:8], dados [31, 4:8], dados [37, 4:8],
                    dados [43, 4:8])
return (expect)
}
