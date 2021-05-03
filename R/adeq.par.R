#' @title Adequação dos parâmetros estimados
#' @name adeq.par
#'
#' @description Verificar a adequação dos parâmetros dos itens estimados pelo BILOG-MG.
#'
#' @param arq.par Arquivo PAR geado pelo BILOG-MG
#' @param a Vetor com o menor valor aceitável e o maior valor aceitável do parâmetro 'a'
#' @param b Vetor com o menor valor aceitável e o maior valor aceitável do parâmetro 'b'
#' @param c Vetor com o menor valor aceitável e o maior valor aceitável do parâmetro 'c'
#' @param erro.a valor do maior valor aceitável para o erro do parâmetro 'a'
#' @param erro.b valor do maior valor aceitável para o erro do parâmetro 'b'
#' @param erro.c valor do maior valor aceitável para o erro do parâmetro 'c'
#' @param salvar Valor lógico. Indica se o resultado será salvo em um arquivo; caso seja TRUE, o arquivo será salvo como "ITENS_FORA_INTERVALO.txt"
#'
#' @details Utilize este campo para escrever detalhes mais tecnicos da
#'     sua funcao (se necessario), ou para detalhar melhor como
#'     utilizar determinados argumentos.
#'
#' @return A função retorna uma data.frame com o número do item no BILOG-MG e o motivo de sua exclusão
#'
#' @author Alexandre Jaloto
#'
#'
#' @examples
#' adeq.par (arq.par = 'PTLCV3.PAR', a = c (1, 3.5), b = c (-3.5, 3.8), c = c (0, .25))
#' @export

adeq.par = function (arq.par,
                     a = list(.45, 4),
                     b = list(-4, 4),
                     c = list(0, .45),
                     erro.a = NULL,
                     erro.b = NULL,
                     erro.c = NULL,
                     salvar = FALSE)
{
  par. = ler.par (arq.par)

  # verificar se há algum item que estourou na calibração e emitir uma mensagem de aviso
  if (length (which (par.$ERRO_INTERCEPTO == '**********')) > 0)
  {
    warning("Um ou mais itens nao calibraram adequadamente e nao foram incluidos nesta analise. Exclua esse(s) item(ns) e rode a calibracao novamente.")
  }

  # selecionar somente os itens novos, ou seja, que tiveram erro != 0; eliminar também os itens que estouraram, ou seja, tiveram erro = '**********'
  # há diferença entre as duas linhas abaixo porque em caso de um item estourar, o erro padrão é dado como ******, portanto o R lê como 'factor' e não como 'numeric'
  par = subset (par., ERRO_INTERCEPTO != "   0.00000" & ERRO_INTERCEPTO != "**********")
  par = subset (par, ERRO_INTERCEPTO != 0)

  # transformar em 'numeric' o erro do parâmetro b, caso não seja
  if (class (par$ERRO_B) == "factor")
  {
    par$ERRO_B = as.numeric (as.character (par$ERRO_B))
  }


  # selecionar itens com parâmetro a fora do intervalo adequado e inserir o motivo de abandono
  a.fora.menor = subset (par, PAR_A < a[1])
  a.fora.maior = subset (par, PAR_A > a[2])

  # caso haja algum item nessa condição, inserir o motivo
  if (dim (a.fora.menor)[1] != 0)
  {
    a.fora.menor$MOTIVO = paste ("a < ", a[1], sep = "")
  }

  if (dim (a.fora.maior)[1] != 0)
  {
    a.fora.maior$MOTIVO = paste ("a > ", a[2], sep = "")
  }

  # selecionar itens com parâmetro b fora do intervalo adequado e inserir o motivo de abandono
  b.fora.menor = subset (par, PAR_B < b[1])
  b.fora.maior = subset (par, PAR_B > b[2])

  if (dim (b.fora.menor)[1] != 0)
  {
    b.fora.menor$MOTIVO = paste ("b < ", b[1], sep = "")
  }

  if (dim (b.fora.maior)[1] != 0)
  {
    b.fora.maior$MOTIVO = paste ("b > ", b[2], sep = "")
  }

  # selecionar itens com parâmetro c fora do intervalo adequado e inserir o motivo de abandono
  c.fora.menor = subset (par, PAR_C < c[1])
  c.fora.maior = subset (par, PAR_C > c[2])

  if (dim (c.fora.menor)[1] != 0)
  {
    c.fora.menor$MOTIVO = paste ("c < ", c[1], sep = "")
  }

  if (dim (c.fora.maior)[1] != 0)
  {
    c.fora.maior$MOTIVO = paste ("c > ", c[2], sep = "")
  }

  # selecionar itens com erro do parâmetro a fora do limite e inserir o motivo de abandono
  if (length (erro.a) > 0)
  {
    a.erro = subset (par, ERRO_A > erro.a)

    if (dim (a.erro)[1] != 0)
    {
      a.erro$MOTIVO = paste ("erro a > ", erro.a, sep = "")
    }
  }

  # selecionar itens com erro do parâmetro b fora do limite e inserir o motivo de abandono
  if (length (erro.b) > 0)
  {
    b.erro = subset (par, ERRO_B > erro.b)

    if (dim (b.erro)[1] != 0)
    {
      b.erro$MOTIVO = paste ("erro b > ", erro.b, sep = "")
    }
  }

  # selecionar itens com erro do parâmetro c fora do limite e inserir o motivo de abandono
  if (length (erro.c) > 0)
  {
    c.erro = subset (par, ERRO_C > erro.c)

    if (dim (c.erro)[1] != 0)
    {
      c.erro$MOTIVO = paste ("erro c > ", erro.c, sep = "")
    }
  }

  # juntar todos os itens que estão fora do intervalo
  todos.fora = rbind (a.fora.menor,
                      a.fora.maior,
                      b.fora.menor,
                      b.fora.maior,
                      c.fora.menor,
                      c.fora.maior)

  if (length (erro.a) > 0)
  {
    todos.fora = rbind (todos.fora, a.erro)
  }

  if (length (erro.b) > 0)
  {
    todos.fora = rbind (todos.fora, b.erro)
  }

  if (length (erro.c) > 0)
  {
    todos.fora = rbind (todos.fora, c.erro)
  }

  # criar tabela com os num_itens e com o motivo do abandono
  fora = data.frame ('ITEM' = todos.fora$NUM_BILOG, 'MOTIVO' = todos.fora$MOTIVO)

  # salvar o arquivo
  if (salvar == TRUE)
  {
    write.table (fora, "ITENS_FORA_INTERVALO.txt", sep = "\t", row.names = F, quote = F)
  }
  return (fora)
}
