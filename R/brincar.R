#' @title Brincando com a TRI
#' @name brincar
#'
#' @description Gera um aplicativo shiny com o objetivo de conhecer melhor a TRI
#'
#' @author Alexandre Jaloto
#'
#' @examples
#' brincar()
#' @export
brincar = function()
{
  ui = shiny::fluidPage(
    shiny::fluidRow(
      # usar o pacote para esconder o botão da CAT
      shinyjs::useShinyjs(),

      # navegação com barra de menu em cima
      shiny::navbarPage(
        "Brincando com a TRI",
        shiny::tabPanel(

          # Questionário ------------------------------------------------------------


          "Questionário",
          shiny::column(
            5,
            shiny::wellPanel(
              shiny::div(
                id = 'form',
                shiny::textInput ('nome', label = 'Nome'),
                shiny::checkboxInput ('pe.frio', label = 'Na cama você frequentemente sente frio nos pés?', FALSE),
                shiny::checkboxInput ('escada', label = 'Você frequentemente desce as escadas de dois em dois degraus?', FALSE),
                shiny::checkboxInput ('basquete', label = 'Você acha que se daria bem em um time de basquete?', FALSE),
                shiny::checkboxInput ('policial', label = 'Como policial, você impressionaria muito?', FALSE),
                shiny::checkboxInput ('carro', label = 'Na maioria dos carros você se sente desconfortável?', FALSE),
                shiny::checkboxInput ('colegas', label = 'Você literalmente olha para seus colegas de cima para baixo?', FALSE),
                shiny::checkboxInput ('armario', label = 'Você é capaz de pegar um objeto no alto de um armário, sem usar escada?', FALSE),
                shiny::checkboxInput ('porta', label = 'Você abaixa quando vai passar por uma porta?', FALSE),
                shiny::checkboxInput ('aviao', label = 'Você consegue guardar a bagagem no porta-malas do avião?', FALSE),
                shiny::checkboxInput ('carro2', label = 'Você regula o banco do carro para trás?', FALSE),
                shiny::checkboxInput ('carona', label = 'Normalmente quando você está andando de carona lhe oferecem o banco da frente?', FALSE),
                shiny::checkboxInput ('foto', label = 'Quando você e várias pessoas vão tirar fotos, formando-se três fileiras, onde ninguém ficará agachado, você costuma ficar atrás?', FALSE),
                shiny::checkboxInput ('onibus', label = 'Você tem dificuldade para se acomodar no ônibus?', FALSE),
                shiny::checkboxInput ('fila', label = 'Em uma fila, por ordem de tamanho, você é sempre colocado atrás?', FALSE)
              )
            )
          ),
          shiny::column(
            6,
            shiny::numericInput ('altura.real', label = 'Qual a sua altura?', min = 0, max = 3, value = 0, step = 0.01),
            shiny::actionButton("submit", "Calcular altura", class = "btn-primary"),
            shiny::actionButton("salvar", "Salvar", class = "btn-primary"),
            shiny::h3(shiny::textOutput("altura")),
            shiny::h3(shiny::textOutput("erro"))
          )
        ),

        # CAT da altura -----------------------------------------------------------

        shiny::tabPanel(
          'CAT da altura',
          shiny::textOutput('enunc'),
          shiny::radioButtons(
            'it',
            '',
            c ('Sim' = 1, 'Não' = 0)
          ),
          shiny::actionButton("submit.cat", "Enviar", class = "btn-primary"),
          shiny::h5('Itens aplicados:', shiny::textOutput("aplicados")),
          shiny::h5('Respostas:', shiny::textOutput("padrao")),
          shiny::h5('Histórico do theta:', shiny::textOutput("theta.hist")),
          shiny::h5('Histórico do erro:', shiny::textOutput("se.hist")),
          shiny::h4('Histórico da altura:', shiny::textOutput("printar.alt"))
        ),

        # Parâmetros --------------------------------------------------------------


        shiny::tabPanel(
          "Parâmetros",
          # dividir o painel em colunas
          shiny::column(
            3,
            shiny::numericInput('n_item', 'Quantidade de itens', 1, min = 1, max = 3, width = '25%'),

            # caso haja pelo menos um item e seja escala (0,1)
            shiny::checkboxInput('mostra_info', 'Indicar informação'),
            shiny::checkboxInput('mostra_info_total', 'Indicar informação total'),
            shiny::conditionalPanel(
              condition = "input.n_item >=1 & input.escala == 'padrao'",
              shiny::h4('Item 1'),
              shiny::sliderInput('item1.a.p', value = 1, min = 0, max = 4, label = "a", step = .1, width = '30%'),
              shiny::sliderInput('item1.b.p', value = 0, min = -4, max = 4, label = "b", step = .1, width = '30%'),
              shiny::sliderInput('item1.c.p', value = .2, min = 0, max = 1, label = "c", step = .1, width = '30%')
            ),

            # caso haja pelo menos um item e seja escala (500,100)
            shiny::conditionalPanel(
              condition = "input.n_item >=1 & input.escala == 'Enem'",
              shiny::h4('Item 1'),
              shiny::sliderInput('item1.a.e', value = (1/100), min = 0, max = 4*1/100, label = "a", step = 1/(10*100), width = '30%'),
              shiny::sliderInput('item1.b.e', value = (0*100+500), min = 500-4*100, max = 500+4*100, label = "b", step = 100/10, width = '30%'),
              shiny::sliderInput('item1.c.e', value = .2, min = 0, max = 1, label = "c", step = .1, width = '30%')
            ),

            # caso haja pelo menos dois itens e seja escala (0,1)
            shiny::conditionalPanel(
              condition = "input.n_item >= 2 & input.escala == 'padrao'",
              shiny::h4('Item 2'),
              shiny::sliderInput('item2.a.p', value = 1, min = 0, max = 4, label = "a", step = .1, width = '30%'),
              shiny::sliderInput('item2.b.p', value = 1, min = -4, max = 4, label = "b", step = .1, width = '30%'),
              shiny::sliderInput('item2.c.p', value = .2, min = 0, max = 1, label = "c", step = .1, width = '30%')
            ),

            # caso haja pelo menos dois itens e seja escala (500,100)
            shiny::conditionalPanel(
              condition = "input.n_item >=2 & input.escala == 'Enem'",
              shiny::h4('Item 2'),
              shiny::sliderInput('item2.a.e', value = (1/100), min = 0, max = 4*1/100, label = "a", step = 1/(10*100), width = '30%'),
              shiny::sliderInput('item2.b.e', value = (1*100+500), min = 500-4*100, max = 500+4*100, label = "b", step = 100/10, width = '30%'),
              shiny::sliderInput('item2.c.e', value = .2, min = 0, max = 1, label = "c", step = .1, width = '30%')
            ),

            # caso haja pelo menos três itens e seja escala (0,1)
            shiny::conditionalPanel(
              condition = "input.n_item >= 3 & input.escala == 'padrao'",
              shiny::h4('Item 3'),
              shiny::sliderInput('item3.a.p', value = 1, min = 0, max = 4, label = "a", step = .1, width = '30%'),
              shiny::sliderInput('item3.b.p', value = -1, min = -4, max = 4, label = "b", step = .1, width = '30%'),
              shiny::sliderInput('item3.c.p', value = .2, min = 0, max = 1, label = "c", step = .1, width = '30%')
            ),

            # caso haja pelo menos três itens e seja escala (500,100)
            shiny::conditionalPanel(
              condition = "input.n_item >=3 & input.escala == 'Enem'",
              shiny::h4('Item 3'),
              shiny::sliderInput('item3.a.e', value = (1/100), min = 0, max = 4*1/100, label = "a", step = 1/(10*100), width = '30%'),
              shiny::sliderInput('item3.b.e', value = (-1*100+500), min = 500-4*100, max = 500+4*100, label = "b", step = 100/10, width = '30%'),
              shiny::sliderInput('item3.c.e', value = .2, min = 0, max = 1, label = "c", step = .1, width = '30%')
            )

          ),
          # Participantes
          shiny::column(
            3,
            shiny::numericInput('n_part', 'Quantidade de participantes', 0, min = 0, max = 2, width = '25%'),

            # caso haja pelo menos um item e seja escala (0,1)
            shiny::conditionalPanel(
              condition = "input.n_part >= 1 & input.escala == 'padrao'",
              shiny::sliderInput ('part1.p', value = -.5, min = -4, max = 4, label = "Proficiência do participante 1", step = .1, width = '30%')),
            shiny::conditionalPanel(
              condition = "input.n_part >= 2 & input.escala == 'padrao'",
              shiny::sliderInput ('part2.p', value = 1, min = -4, max = 4, label = "Proficiência do participante 2", step = .1, width = '30%')),


            # caso haja pelo menos um item e seja escala (500,100)
            shiny::conditionalPanel(
              condition = "input.n_part >= 1 & input.escala == 'Enem'",
              shiny::sliderInput ('part1.e', value = 500-100*.5, min = 100, max = 900, label = "Proficiência do participante 1", step = 10, width = '30%')),
            shiny::conditionalPanel(
              condition = "input.n_part >= 2 & input.escala == 'Enem'",
              shiny::sliderInput ('part2.e', value = 600, min = 100, max = 900, label = "Proficiência do participante 2", step = 10, width = '30%')),


            shiny::conditionalPanel(
              condition = "input.n_item >= 1
                                                        & input.n_part >= 1",
              shiny::checkboxInput('mostra_prob1', 'Indicar probabilidade')
            )
          ),
          shiny::column(
            6,
            shiny::selectInput('escala', 'Escala', choices = list ('Padrão' = 'padrao', 'Enem' = 'Enem')),
            shiny::plotOutput('plot_item1')
          )
        ),

        # estimar o theta ---------------------------------------------------------

        shiny::tabPanel(
          "Estimar o theta",
          shiny::column(12, align = "left",
                        'Trata-se de um teste fictício de 6 itens com parâmetros conhecidos
                 aplicado a uma pessoa.',
                        shiny::br(),
                        shiny::tags$b('Primeiro'),
                        ', selecione as respostas a cada item (1 = acerto; 0 = erro).',
                        shiny::br(),
                        shiny::tags$b('Em seguida'),
                        ', indique os valores dos parâmetros dos itens. Caso seja o modelo 2PL,
           o parâmetro "c" deve valer 0. Caso seja o modelo de Rasch, o parâmetro "a" deve
           valer 1.',
                        shiny::br(),
                        shiny::br(),
                        'A tabela possui 11 variáveis.',
                        shiny::br(),
                        shiny::tags$b('Theta'),
                        ': uma sequência de valores da variável latente que vai de -3 a 3.',
                        shiny::br(),
                        shiny::tags$b('Prob.1 a Prob.6'),
                        ': a probabilidade de se observar a resposta selecionada para
           aquele item, dados os parâmetros indicados e o theta. Se a pessoa acertou, é
           o equivalente ao P (resultado da função na TRI); se a pessoa errou, é o
           equivalente a 1-P.',
                        shiny::br(),
                        shiny::tags$b('Verossimilhança'),
                        ': equivale ao produto das probabilidades. A célula marcada é a
           maior probabilidade, ou seja a máxima verossimilhança. O theta correspondente
           a esse valor seria o estimado pelo método de Máxima Verossimilhança,
           Maximum Likelihood (ML).',
                        shiny::br(),
                        shiny::tags$b('Freq.Priori'),
                        ': em uma distribuição normal com média 0 e desvio padrão 1, qual a
           densidade desse valor de theta? Esta variável é como se fosse a frequência de
           cada theta nessa distribuição, que é a distribuição priori informada no nosso
           caso.',
                        shiny::br(),
                        shiny::tags$b('Posteriori'),
                        ': o produto da verossimilhança com a frequência do theta na distribuição priori.
           O maior valor corresponde à moda dessa distribuição posteriori. Se utilizarmos
           o método do Maximum a Posteriori (MAP). Preciso entender melhor o que representa
           esse produto, ou seja, o que cada valor significa. É a frequência de cada theta
           na distribuição posteriori?',
                        shiny::br(),
                        shiny::tags$b('Freq.Posteriori'),
                        ': o produto da posteriori com o theta. Equivale à frequência de cada theta na
           distribuição posteriori obtida.',
                        shiny::br(),
                        shiny::br(),
                        'O valor do EAP corresponde ao theta estimado pelo método Expected a Posteriori
           (EAP). Esta parte ainda preciso entender melhor. Mas entendo que
           equivale à média da frequência posteriori. Ou seja, é a razão entre a soma das
           frequências dos thetas na posteriori e a soma da posteriori. Ou seja, a razão
           das integrais dos gráficos.',
                        shiny::br(),
                        shiny::br(),
                        shiny::tags$table(
                          shiny::tags$tr(
                            shiny::tags$td(
                              align = "left",
                              'Resposta'),
                            shiny::tags$td(shiny::selectInput('i1', 'Item 1', as.numeric(c(0,1)), width = '50px')),
                            shiny::tags$td(shiny::selectInput('i2', 'Item 2', c(0,1), width = '50px')),
                            shiny::tags$td(shiny::selectInput('i3', 'Item 3', c(0,1), width = '50px')),
                            shiny::tags$td(shiny::selectInput('i4', 'Item 4', c(0,1), width = '50px')),
                            shiny::tags$td(shiny::selectInput('i5', 'Item 5', c(0,1), width = '50px')),
                            shiny::tags$td(shiny::selectInput('i6', 'Item 6', c(0,1), width = '50px')),
                          ),
                          shiny::tags$tr(
                            shiny::tags$td(
                              'Parâmetro a'
                            ),
                            shiny::tags$td(shiny::numericInput('a1', label = NULL, value = 1, min = 0, max = 3, step = .1, width = '65px')),
                            shiny::tags$td(shiny::numericInput('a2', label = NULL, value = 1, min = 0, max = 3, step = .1, width = '65px')),
                            shiny::tags$td(shiny::numericInput('a3', label = NULL, value = 1, min = 0, max = 3, step = .1, width = '65px')),
                            shiny::tags$td(shiny::numericInput('a4', label = NULL, value = 1, min = 0, max = 3, step = .1, width = '65px')),
                            shiny::tags$td(shiny::numericInput('a5', label = NULL, value = 1, min = 0, max = 3, step = .1, width = '65px')),
                            shiny::tags$td(shiny::numericInput('a6', label = NULL, value = 1, min = 0, max = 3, step = .1, width = '65px')),
                          ),
                          shiny::tags$tr(
                            shiny::tags$td(
                              'Parâmetro b'
                            ),
                            shiny::tags$td(shiny::numericInput('b1', label = NULL, value = -2, min = -4, max = 4, step = .2, width = '65px')),
                            shiny::tags$td(shiny::numericInput('b2', label = NULL, value = -1, min = -4, max = 4, step = .2, width = '65px')),
                            shiny::tags$td(shiny::numericInput('b3', label = NULL, value = 0, min = -4, max = 4, step = .2, width = '65px')),
                            shiny::tags$td(shiny::numericInput('b4', label = NULL, value = 1, min = -4, max = 4, step = .2, width = '65px')),
                            shiny::tags$td(shiny::numericInput('b5', label = NULL, value = 2, min = -4, max = 4, step = .2, width = '65px')),
                            shiny::tags$td(shiny::numericInput('b6', label = NULL, value = 2.5, min = -4, max = 4, step = .2, width = '65px')),
                          ),
                          shiny::tags$tr(
                            shiny::tags$td(
                              'Parâmetro c'
                            ),
                            shiny::tags$td(shiny::numericInput('c1', label = NULL, value = 0, min = 0, max = 1, step = .1, width = '65px')),
                            shiny::tags$td(shiny::numericInput('c2', label = NULL, value = 0, min = 0, max = 1, step = .1, width = '65px')),
                            shiny::tags$td(shiny::numericInput('c3', label = NULL, value = 0, min = 0, max = 1, step = .1, width = '65px')),
                            shiny::tags$td(shiny::numericInput('c4', label = NULL, value = 0, min = 0, max = 1, step = .1, width = '65px')),
                            shiny::tags$td(shiny::numericInput('c5', label = NULL, value = 0, min = 0, max = 1, step = .1, width = '65px')),
                            shiny::tags$td(shiny::numericInput('c6', label = NULL, value = 0, min = 0, max = 1, step = .1, width = '65px')),
                          ),
                        ),
                        shiny::tags$table(
                          shiny::tags$tr(
                            shiny::tags$td (tableHTML::tableHTML_output('tab.total')),
                            shiny::tags$td(
                              width = '250px',
                              style = "vertical-align: top; text-align: right",
                              shiny::htmlOutput('nota_eap2'))
                          )
                        )
          )
        )
      )
    )
  )



  # server ------------------------------------------------------------------


  server = function(input, output) {


    # parâmetros --------------------------------------------------------------

    theta.p = seq (-4, 4, by = (4 - (-4))/(1000-1))
    theta.e = seq (100, 900, by = (900 - 100)/(1000-1))


    output$plot_item1 = shiny::renderPlot({

      # se escala (0,1)
      if (input$escala == 'padrao')
      {

        # para o item 1
        P = input$item1.c.p + ((1 - input$item1.c.p)/(1 + exp(-input$item1.a.p * (theta.p - input$item1.b.p))))
        plot (xlim = c(-4, 4), ylim = c(0, 1), type = 'n', theta.p, P, xlab="Proficiência (habilidade)", ylab="Probabilidade de acerto", las = 1)
        lines (theta.p, P)
        # info
        info1 = input$item1.a.p^2 * (1 - P) * ((P - input$item1.c.p)/(1-input$item1.c.p))^2/P
        par(new=T)
        if (input$mostra_info == TRUE)
        {
          plot (theta.p, info1, type = 'n', xlab = '', ylab = '', axes = FALSE, ylim = c(0, .5))
          lines (theta.p, info1, lty = 3)
          # gerar novamente uma plotagem para a CCi do próximo item
          par(new=T)
          plot (xlim = c(-4, 4), ylim = c(0, 1), type = 'n', theta.p, P, xlab="Proficiência (habilidade)", ylab="Probabilidade de acerto", las = 1)
        }


        # para participante 1
        P = input$item1.c.p + ((1 - input$item1.c.p)/(1 + exp(-input$item1.a.p*(input$part1.p-input$item1.b.p))))
        if (input$n_part >= 1 & input$mostra_prob1 == TRUE)
        {segments (x0 = input$part1.p, y0 = 0, y1 = P, lty = 2)
          segments (x0 = -4, y0 = P, x1 = input$part1.p, lty = 2)}

        # para participante 2
        P = input$item1.c.p + ((1 - input$item1.c.p)/(1 + exp(-input$item1.a.p*(input$part2.p-input$item1.b.p))))
        if (input$n_part >= 2 & input$mostra_prob1 == TRUE)
        {segments (x0 = input$part2.p, y0 = 0, y1 = P, lty = 3)
          segments (x0 = -4, y0 = P, x1 = input$part2.p, lty = 3)}


        # para o item 2
        P = input$item2.c.p + ((1 - input$item2.c.p)/(1 + exp(-input$item2.a.p * (theta.p - input$item2.b.p))))
        if (input$n_item >= 2)
        {lines (theta.p, P, col = 'red')

          # info
          info2 = input$item2.a.p^2 * (1 - P) * ((P - input$item2.c.p)/(1-input$item2.c.p))^2/P
          if (input$mostra_info == TRUE)
          {
            par(new=T)
            plot (theta.p, info2, type = 'n', xlab = '', ylab = '', axes = FALSE, ylim = c(0, .5))
            lines (theta.p, info2, lty = 3, col = 'red')

            if (input$n_item == 2 & input$mostra_info_total == TRUE)
            {
              lines (theta.p, apply (data.frame (info1,info2),1,sum), lty = 3, col = 'darkgreen', lwd = 2)
            }
            # gerar novamente uma plotagem para a CCi do próximo item
            par(new=T)
            plot (xlim = c(-4, 4), ylim = c(0, 1), type = 'n', theta.p, P, xlab="Proficiência (habilidade)", ylab="Probabilidade de acerto", las = 1)
          }
        }


        # para participante 1
        P = input$item2.c.p + ((1 - input$item2.c.p)/(1 + exp(-input$item2.a.p*(input$part1.p-input$item2.b.p))))
        if (input$n_part >= 1 & input$n_item >=2 & input$mostra_prob1 == TRUE)
        {segments (x0 = input$part1.p, y0 = 0, y1 = P, lty = 2)
          segments (x0 = -4, y0 = P, x1 = input$part1.p, lty = 2, col = 'red')}

        # para participante 2
        P = input$item2.c.p + ((1 - input$item2.c.p)/(1 + exp(-input$item2.a.p*(input$part2.p-input$item2.b.p))))
        if (input$n_part >= 2 & input$n_item >=2 & input$mostra_prob1 == TRUE)
        {segments (x0 = input$part2.p, y0 = 0, y1 = P, lty = 3)
          segments (x0 = -4, y0 = P, x1 = input$part2.p, lty = 3, col = 'red')}


        # para o item 3
        P = input$item3.c.p + ((1 - input$item3.c.p)/(1 + exp(-input$item3.a.p*(theta.p-input$item3.b.p))))
        if (input$n_item >= 3)
        {(lines (theta.p, P, col = 'blue'))

          # info
          info3 = input$item3.a.p^2 * (1 - P) * ((P - input$item3.c.p)/(1-input$item3.c.p))^2/P
          if (input$mostra_info == TRUE)
          {
            par(new=T)
            plot (theta.p, info3, type = 'n', xlab = '', ylab = '', axes = FALSE, ylim = c(0, .5))
            lines (theta.p, info3, lty = 3, col = 'blue')

            if (input$n_item == 3 & input$mostra_info_total == TRUE)
            {
              lines (theta.p, apply (data.frame (info1,info2,info3),1,sum), lty = 3, col = 'darkgreen', lwd = 2)
            }
            # gerar novamente uma plotagem para a CCi do próximo item
            par(new=T)
            plot (xlim = c(-4, 4), ylim = c(0, 1), type = 'n', theta.p, P, xlab="Proficiência (habilidade)", ylab="Probabilidade de acerto", las = 1)
          }
        }



        # se escala (500,100)
      } else if (input$escala == 'Enem')
      {
        # para o item 1
        P = input$item1.c.e + ((1 - input$item1.c.e)/(1 + exp(-input$item1.a.e * (theta.e - input$item1.b.e))))
        plot (xlim = c(-4*100+500, 4*100+500), ylim = c(0, 1), type = 'n', theta.e, P, xlab="Proficiência (habilidade)", ylab="Probabilidade de acerto", las = 1)
        lines (theta.e, P)
        # info
        info1 = input$item1.a.e^2 * (1 - P) * ((P - input$item1.c.e)/(1-input$item1.c.e))^2/P
        par(new=T)
        if (input$mostra_info == TRUE)
        {
          plot (theta.e, info1, type = 'n', xlab = '', ylab = '', axes = FALSE, ylim = c(0, .00005))
          lines (theta.e, info1, lty = 3)
          # gerar novamente uma plotagem para a CCi do próximo item
          par(new=T)
          plot (xlim = c(-4*100+500, 4*100+500), ylim = c(0, 1), type = 'n', theta.e, P, xlab="Proficiência (habilidade)", ylab="Probabilidade de acerto", las = 1)
        }


        # para participante 1
        P = input$item1.c.e + ((1 - input$item1.c.e)/(1 + exp(-input$item1.a.e*(input$part1.e-input$item1.b.e))))
        if (input$n_part >= 1 & input$mostra_prob1 == TRUE)
        {segments (x0 = input$part1.e, y0 = 0, y1 = P, lty = 2)
          segments (x0 = -4*100+500, y0 = P, x1 = input$part1.e, lty = 2)}

        # para participante 2
        P = input$item1.c.e + ((1 - input$item1.c.e)/(1 + exp(-input$item1.a.e*(input$part2.e-input$item1.b.e))))
        if (input$n_part >= 2 & input$mostra_prob1 == TRUE)
        {segments (x0 = input$part2.e, y0 = 0, y1 = P, lty = 3)
          segments (x0 = -4*100+500, y0 = P, x1 = input$part2.e, lty = 3)}


        # para o item 2
        P = input$item2.c.e + ((1 - input$item2.c.e)/(1 + exp(-input$item2.a.e * (theta.e - input$item2.b.e))))
        if (input$n_item >= 2)
        {lines (theta.e, P, col = 'red')

          # info
          info2 = input$item2.a.e^2 * (1 - P) * ((P - input$item2.c.e)/(1-input$item2.c.e))^2/P
          if (input$mostra_info == TRUE)
          {
            par(new=T)
            plot (theta.e, info2, type = 'n', xlab = '', ylab = '', axes = FALSE, ylim = c(0, .00005))
            lines (theta.e, info2, lty = 3, col = 'red')

            if (input$n_item == 2 & input$mostra_info_total == TRUE)
            {
              lines (theta.e, apply (data.frame (info1,info2),1,sum), lty = 3, col = 'darkgreen', lwd = 2)
            }
            # gerar novamente uma plotagem para a CCi do próximo item
            par(new=T)
            plot (xlim = c(-4*100+500, 4*100+500), ylim = c(0, 1), type = 'n', theta.e, P, xlab="Proficiência (habilidade)", ylab="Probabilidade de acerto", las = 1)
          }
        }

        # para participante 1
        P = input$item2.c.e + ((1 - input$item2.c.e)/(1 + exp(-input$item2.a.e*(input$part1.e-input$item2.b.e))))
        if (input$n_part >= 1 & input$n_item >=2 & input$mostra_prob1 == TRUE)
        {segments (x0 = input$part1.e, y0 = 0, y1 = P, lty = 2)
          segments (x0 = -4*100+500, y0 = P, x1 = input$part1.e, lty = 2, col = 'red')}

        # para participante 2
        P = input$item2.c.e + ((1 - input$item2.c.e)/(1 + exp(-input$item2.a.e*(input$part2.e-input$item2.b.e))))
        if (input$n_part >= 2 & input$n_item >=2 & input$mostra_prob1 == TRUE)
        {segments (x0 = input$part2.e, y0 = 0, y1 = P, lty = 3)
          segments (x0 = -4*100+500, y0 = P, x1 = input$part2.e, lty = 3, col = 'red')}


        # para o item 3
        P = input$item3.c.e + ((1 - input$item3.c.e)/(1 + exp(-input$item3.a.e*(theta.e-input$item3.b.e))))
        if (input$n_item >= 3)
        {(lines (theta.e, P, col = 'blue'))

          # info
          info3 = input$item3.a.e^2 * (1 - P) * ((P - input$item3.c.e)/(1-input$item3.c.e))^2/P
          if (input$mostra_info == TRUE)
          {
            par(new=T)
            plot (theta.e, info3, type = 'n', xlab = '', ylab = '', axes = FALSE, ylim = c(0, .00005))
            lines (theta.e, info3, lty = 3, col = 'red')

            if (input$n_item == 3 & input$mostra_info_total == TRUE)
            {
              lines (theta.e, apply (data.frame (info1,info2,info3),1,sum), lty = 3, col = 'darkgreen', lwd = 2)
            }
            # gerar novamente uma plotagem para a CCi do próximo item
            par(new=T)
            plot (xlim = c(-4*100+500, 4*100+500), ylim = c(0, 1), type = 'n', theta.e, P, xlab="Proficiência (habilidade)", ylab="Probabilidade de acerto", las = 1)
          }
        }

      }


    })

    # questionário da altura --------------------------------------------------

    itens = data.frame(
      item = 1:14,
      enunciado = c(
        'Na cama você frequentemente sente frio nos pés?',
        'Você frequentemente desce as escadas de dois em dois degraus?',
        'Você acha que se daria bem em um time de basquete?',
        'Como policial, você impressionaria muito?',
        'Na maioria dos carros você se sente desconfortável?',
        'Você literalmente olha para seus colegas de cima para baixo?',
        'Você é capaz de pegar um objeto no alto de um armário, sem usar escada?',
        'Você abaixa quando vai passar por uma porta?',
        'Você consegue guardar a bagagem no porta-malas do avião?',
        'Você regula o banco do carro para trás?',
        'Normalmente quando você está andando de carona lhe oferecem o banco da frente?',
        'Quando você e várias pessoas vão tirar fotos, formando-se três fileiras, onde ninguém ficará agachado, você costuma ficar atrás?',
        'Você tem dificuldade para se acomodar no ônibus?',
        'Em uma fila, por ordem de tamanho, você é sempre colocado atrás?'
      ),
      a = c (0.4232783, 1.0295999, 1.2833717, 0.8096345, 1.8143789, 1.5435942, 2.9310132, 0.9539005, 1.0113734,
             2.4803165, 1.0453012, 1.9615977, 1.5082983, 2.3080323),
      b = c (1.83680682, 1.36330492, 1.38053410, 1.44631675, 1.23284018, 1.27474620, -0.12924484, 2.99072132, -0.88812405,
             0.04909726, 0.45853523, 0.27753228, 0.91737338, 0.59221396),
      c = 0
    )

    # média e desvio da calibração da altura
    m = 0.000113344
    s = 0.9711545

    # média e desvio reais da altura
    alt.media = 1.692369668
    alt.dp = 0.093317355

    # tabela dos itens para o mirt
    itens.mirt = data.frame(
      a1 = itens$a,
      d = -itens$a*itens$b,
      g = itens$c
    )

    # criar objeto mirt
    mod = mirtCAT::generate.mirt_object(
      itens.mirt,
      '3PL'
    )

    resp = shiny::reactive ({
      as.numeric ( c (input$pe.frio, input$escada, input$basquete, input$policial, input$carro, input$colegas, input$armario, input$porta, input$aviao, input$carro2, input$carona, input$foto, input$onibus, input$fila))
    })

    prof = reactive ({
      data.frame (mirt::fscores(mod, response.pattern = resp()))$F1
    })

    altura = shiny::reactive ({
      alt = round (as.numeric (((prof()-m)/s) * alt.dp + alt.media), 2)
      alt
    })



    #função para indicar a hora
    humanTime = function() format(Sys.time(), "%Y%m%d-%H%M%OS")

    shiny::observeEvent(input$submit, {

      output$respostas = shiny::renderText (c ('Padrão de respostas:', resp()))
      output$altura = shiny::renderText (c ('Altura estimada:', altura()))

      erro = shiny::reactive ({
        abs (round ((altura() - as.numeric (input$altura.real)), 2))
      })

      output$erro = shiny::renderText (paste0 ('Sua fita métrica errou por ', erro(), 'm.'))

    })

    # para salvar
    shiny::observeEvent(input$salvar, {

      fieldsAll = c ('pe.frio', 'escada', 'basquete', 'policial',
                     'carro', 'colegas', 'armario', 'porta', 'aviao',
                     'carro2', 'carona', 'foto', 'onibus', 'fila')


      formData = shiny::reactive({
        data = sapply(fieldsAll, function(x) input[[x]])
        data = data.frame (t(data), altura.estimada = altura(), altura.real = input$altura.real)
        data
      })

      saveData = function(data) {
        fileName = sprintf("%s_%s.csv",
                           humanTime(),
                           input$nome)


        write.table(data, file = fileName,
                    row.names = FALSE, quote = TRUE, sep = ';', dec = ',')
      }

      # action to take when submit button is pressed
      saveData(formData())
    })


    # CAT da altura -----------------------------------------------------------

    # informações iniciais
    padrao = reactiveVal(c())
    theta.cat = reactiveVal(0)
    aplicados  = reactiveVal(c())
    resp.itens = reactiveValues(resp = rep(NA, nrow(itens)))
    theta.hist = reactiveVal(0)
    se.hist = reactiveVal(1)
    fim = reactiveVal(FALSE)

    altura.cat  = reactiveVal(
      round (as.numeric (((0-m)/s) * alt.dp + alt.media), 2)
    )

    itens.disponiveis = reactive ({as.matrix(itens[-aplicados(),c(1,3:5)])})

    it_select = reactive({

      # selecionar primeiro item

      if(length (aplicados()) == 0)
        item_select = catIrt::itChoose(
          left_par = as.matrix(itens[,c(1,3:5)]),
          mod = "brm",
          numb = 1,
          n.select = 1,
          cat_theta = theta.cat(),
          # cat_theta = 0,
          select = "UW-FI",
          at = "theta")

      # selecionar segundo item em diante
      if(length (aplicados()) > 0)

        item_select = catIrt::itChoose(
          left_par = itens.disponiveis(),
          mod = "brm",
          numb = 1,
          n.select = 1,
          cat_theta = theta.cat(),
          select = "UW-FI",
          at = "theta"
        )

      # posição do item
      it_select = item_select$params[[1]]
      it_select
    })

    # enunciado
    tela = reactive ({
      if (fim()) {
        a = c('Acabô')
      } else {a = itens$enunciado[it_select()]}
      a
    })

    output$enunc = shiny::renderText(tela())

    # botão para submeter resposta
    shiny::observeEvent(input$submit.cat, {

      # resposta do item
      resposta. = shiny::isolate ({
        as.numeric (input$it)
      })

      resp.itens$resp[it_select()] = resposta.

      output$padrao = shiny::renderText (resp.itens$resp)

      aplicados( c(aplicados(), it_select()))

      output$aplicados = shiny::renderText (aplicados())

      theta_prov = mirt::fscores(mod, response.pattern = resp.itens$resp)

      theta.cat (data.frame (theta_prov)$F1)

      theta.hist(c (theta.hist(), theta.cat()))

      output$theta.hist = shiny::renderText (theta.hist())

      se.prov = data.frame (theta_prov)$SE_F1

      se.hist ( c(se.hist(), se.prov))

      output$se.hist = shiny::renderText (se.hist())

      delta.theta = abs (theta.hist()[length(theta.hist())] - theta.hist()[length(theta.hist())-1])

      altura.cat (c(altura.cat(), round (as.numeric (((theta.cat()-m)/s) * alt.dp + alt.media), 2)))

      output$printar.alt = shiny::renderText (altura.cat())

      fim (se.prov <= .3)

      fim (fim() | delta.theta <= .05)

      if (fim()) {
        shinyjs::toggle("submit.cat")
        shinyjs::toggle("it")
      }

    })
    # estimar o theta ---------------------------------------------------------

    theta = seq(-3, 3, .1)
    p1 = shiny::reactive(input$c1 + (1-input$c1)*exp({input$a1*(theta-input$b1)})/(1+exp({input$a1*(theta-input$b1)})))
    p2 = shiny::reactive(input$c2 + (1-input$c2)*exp({input$a2*(theta-input$b2)})/(1+exp({input$a2*(theta-input$b2)})))
    p3 = shiny::reactive(input$c3 + (1-input$c3)*exp({input$a3*(theta-input$b3)})/(1+exp({input$a3*(theta-input$b3)})))
    p4 = shiny::reactive(input$c4 + (1-input$c4)*exp({input$a4*(theta-input$b4)})/(1+exp({input$a4*(theta-input$b4)})))
    p5 = shiny::reactive(input$c5 + (1-input$c5)*exp({input$a5*(theta-input$b5)})/(1+exp({input$a5*(theta-input$b5)})))
    p6 = shiny::reactive(input$c6 + (1-input$c6)*exp({input$a6*(theta-input$b6)})/(1+exp({input$a6*(theta-input$b6)})))

    Prob.1 = shiny::reactive(p1()^as.numeric({input$i1})*(1-p1())^(1-as.numeric({input$i1})))
    Prob.2 = shiny::reactive(p2()^as.numeric({input$i2})*(1-p2())^(1-as.numeric({input$i2})))
    Prob.3 = shiny::reactive(p3()^as.numeric({input$i3})*(1-p3())^(1-as.numeric({input$i3})))
    Prob.4 = shiny::reactive(p4()^as.numeric({input$i4})*(1-p4())^(1-as.numeric({input$i4})))
    Prob.5 = shiny::reactive(p5()^as.numeric({input$i5})*(1-p5())^(1-as.numeric({input$i5})))
    Prob.6 = shiny::reactive(p6()^as.numeric({input$i6})*(1-p6())^(1-as.numeric({input$i6})))

    ml = shiny::reactive(Prob.1()*Prob.2()*Prob.3()*Prob.4()*Prob.5()*Prob.6())

    priori = dnorm(theta)

    posteriori = shiny::reactive(ml()*priori)

    freqposteriori = shiny::reactive(posteriori()*theta)

    eap = shiny::reactive(
      round(
        sum(freqposteriori())/sum(posteriori()),
        2
      )
    )

    output$nota_eap = shiny::renderText (
      paste0 ('EAP = ', eap()),
    )

    output$somafreqposteriori = shiny::renderText (
      paste0 ('Soma Freq.Posteriori = ', round (sum(freqposteriori()), 2)),
    )

    output$somaposteriori = shiny::renderText (
      paste0 ('Soma Posteriori = ', round (sum(posteriori()), 2))
    )

    output$nota_eap2 = shiny::renderText (
      paste(
        paste0 ('Soma Freq.Posteriori = ', round (sum(freqposteriori()), 2)),
        paste0 ('Soma Posteriori = ', round (sum(posteriori()), 2)),
        paste0 ('<b>EAP = ', eap(), '</b>'),
        sep = '<br>'
      )
    )

    output$tab.total = tableHTML::render_tableHTML({
      tab.total = data.frame(
        Theta = seq(-3,3,.1),
        Prob.1 = Prob.1(),
        Prob.2 = Prob.2(),
        Prob.3 = Prob.3(),
        Prob.4 = Prob.4(),
        Prob.5 = Prob.5(),
        Prob.6 = Prob.6(),
        Verossimilhança = ml(),
        Freq.Priori = priori,
        Posteriori = posteriori(),
        Freq.Posteriori = freqposteriori()
      ) %>%
        tableHTML::tableHTML(
          widths = c(rep (70, 7), rep (100, 4)),
          border = 0,
          round = 4,
          rownames = FALSE
        ) %>%
        tableHTML::add_css_table(
          css = list(c('text-align'), c('center'))
        ) %>%
        tableHTML::add_css_header(
          css = list(c('text-align'), c('center')), headers = 1:11
        ) %>%
        tableHTML::add_css_row(css = list('background-color', '#f2f2f2'),
                               rows = tableHTML::even(2:62)) %>%
        tableHTML::add_css_row(css = list('background-color', '#e6f0ff'),
                               rows = tableHTML::odd(2:62)) %>%
        tableHTML::add_css_conditional_column(columns = list ('Verossimilhança'), conditional = 'max', css = list('background-color', "green")) %>%
        tableHTML::add_css_conditional_column(columns = list ('Posteriori'), conditional = 'max', css = list('background-color', "green"))

    })

  }

  shiny::shinyApp(ui = ui, server = server)
}
