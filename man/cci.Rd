% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cci.R
\name{cci}
\alias{cci}
\title{Curva característica do item}
\usage{
cci(
  a = 1.2,
  b = 0,
  c = 0.2,
  theta = seq(-4, 4, 0.01),
  info = FALSE,
  xlab = "Proficiência (habilidade)",
  ylab = "Probabiliade de acerto",
  ...
)
}
\arguments{
\item{a}{valor do parâmetro a}

\item{b}{valor do parâmetro b}

\item{c}{valor do parâmetro c}

\item{theta}{vetor com os valores de traço latente para a
construção do gráfico}

\item{info}{Valor lógico. Se TRUE, plota também a curva
de informação do item}

\item{...}{Outros argumentos das funções plot e lines}
}
\value{
A função retorna um gráfico com a curva característica do item
}
\description{
Produz a curva característica de um item (CCI)
}
\examples{
# em uma escala (0,1)
a = 1.5
b = 0.3
c = .15

cci (a, b, c)
cci (a, b, c, info = TRUE)

# agora em uma escala (500, 100)
a = .01
b = 505
c = .15
theta = seq (100, 900, 10)
cci (a, b, c, theta)

# alterando parâmtros do gráfico
cci (a, b, c, theta, col = 'red', lty = 2, main = 'CCI')

}
\author{
Alexandre Jaloto
}
