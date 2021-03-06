% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tct.R
\name{tct}
\alias{tct}
\title{Análise via Teoria Clássica dos Testes (TCT)}
\usage{
tct(
  banco.aberto,
  gab.aberto,
  alt = c("A", "B", "C", "D", ".", "*"),
  usa.normit = TRUE,
  met.perc = 6,
  pop = FALSE
)
}
\arguments{
\item{banco.aberto}{Objeto do tipo data.frame ou matrix cuja
primeira variável é o número do caderno e as demais variáveis
são as respostas a cada item; é necessário que o banco esteja
aberto e os cadernos estejam ordenados a partir do 1; importante:
o banco já tem que ser somente da disciplina que será analisada}

\item{gab.aberto}{Objeto do tipo data.frame com duas variáveis:
código do item e gabarito; é necessário que a ordem dos itens seja a
mesma da ordem do objeto banco.aberto}

\item{alt}{As alternativas possíveis em cada item}

\item{usa.normit}{Valor lógico que indica se o escore utilizado para
a análise é o normit (TRUE) ou a soma de acertos (FALSE)}

\item{met.perc}{O método utilizado para o cálculo do percentil. Varia de 1 a 9. Para mais informações, verifique ajuda da função
quantile.}

\item{pop}{TRUE se for população, FALSE (padrão) se for uma amostra. Essa escolha interfere nas contas que envolvem o cálculo da
variância ou do desvio padrão.}
}
\value{
A função retorna um objeto do tipo list com os seguintes elementos:

$tct
Dados dos itens e da análise, quais sejam: Número sequencial do item;
Código do item; Gabarito do item; Índice de dificuldade; Índice de
discriminação; Porcentagem de acerto no grupo inferior; Porcentagem
de acerto no grupo superior; Correlação bisserial; Proporção de
escolha de cada alternativa; Correlação bisserial de cada alternativa.

$normit
Dados dos indivíduos, quais sejam: Caderno apresentado; Resposta a
cada item (banco aberto); Soma de acertos; Normit.
}
\description{
Análise psicométrica de itens por meio da TCT
}
\details{
A análise utiliza análise via normit. Para os cálculos que
envolvem o desvio padrão, considera-se a raiz da variância da
população; a função var considera n-1, assim como a função sd.
}
\examples{
# criar um banco aberto
set.seed(1000)
gab.lc = sample (LETTERS[1:4], 9, replace = TRUE)
gab.mt = sample (LETTERS[1:4], 9, replace = TRUE)
itens.lc = data.frame (Bloco = rep (1:3, c (3,3,3)), Posicao = rep (1:3, 3),
                       Item = sample (12345:54321, 9), Origem = 'NOVO',
                       Gabarito = gab.lc, Num_bilog = 201:209,
                       Nome_bilog = paste ('P', 0001:0009, sep = ''), Disciplina = 'LC')
itens.mt = data.frame (Bloco = rep (1:3, c (3,3,3)), Posicao = rep (1:3, 3),
                       Gabarito = gab.mt, Item = sample (12345:54321, 9),
                       Origem = 'NOVO', Num_bilog = 201:209,
                       Nome_bilog = paste ('P', 0001:0009, sep = ''), Disciplina = 'MT')

bib = data.frame (Caderno = 1:3, Disciplina1 = rep ('LC', 3), Disciplina2 = rep ('MT', 3),
                  Bloco1 = 1:3, Bloco2 = c(2, 3, 1), Bloco3 = 1:3, Bloco4 = c(2, 3, 1))
                  itens = rbind (itens.lc, itens.mt)
resp = matrix (sample (LETTERS[1:4], 180, replace = TRUE), ncol = 6)
banco = data.frame (CAD = seq(1:3), resp)
disc = 'LC'
aberto = abre.banco (banco = banco, itens = itens, bib = bib, disc = disc)

tct = tct (banco.aberto = aberto$respostas, gab.aberto = aberto$gabarito)
}
\author{
Alexandre Jaloto
}
