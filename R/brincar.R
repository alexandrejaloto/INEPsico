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
      # navegação com barra de menu em cima
      shiny::navbarPage("Brincando com a TRI",
                        # brincando com os parâmetros
                        shiny::tabPanel("Parâmetros",
                                        # dividir o painel em colunas
                                        shiny::column(3,
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
                                                      #),

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
                                        shiny::column(3,
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
                                        shiny::column(6,
                                                      shiny::selectInput('escala', 'Escala', choices = list ('Padrão' = 'padrao', 'Enem' = 'Enem')),
                                                      shiny::plotOutput('plot_item1')
                                        )
                                        ),
                        shiny::tabPanel("Questionário",
                                        shiny::column(5,
                                                      shiny::wellPanel(
                                                        shiny::div (
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
                                        shiny::column(6,
                                                      shiny::numericInput ('altura.real', label = 'Qual a sua altura?', min = 0, max = 3, value = 0, step = 0.01),
                                                      shiny::actionButton("submit", "Calcular altura", class = "btn-primary"),
                                                      shiny::actionButton("salvar", "Salvar", class = "btn-primary"),
                                                      #shiny::verbatimTextOutput('respostas'),
                                                      shiny::h3(shiny::textOutput("altura")),
                                                      shiny::h3(shiny::textOutput("erro"))
                                        )
                        )
                        )
  )
  )


  server = function(input, output) {
    theta.p = seq (-4, 4, by = (4 - (-4))/(1000-1))
    theta.e = seq (100, 900, by = (900 - 100)/(1000-1))

    output$plot_item1 = shiny::renderPlot({

      # se escala (0,1)
      if (input$escala == 'padrao')
      {

        # para o item 1
        P = input$item1.c.p + ((1 - input$item1.c.p)/(1 + exp(-input$item1.a.p * (theta.p - input$item1.b.p))))
        plot (xlim = c(-4, 4), ylim = c(0, 1), type = 'n', theta.p, P, xlab="Proficiência (habilidade)", ylab="Probabilidade de acerto", las = 1)
        #, xaxt = 'n')
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
        #, xaxt = 'n')
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





    # para o questionário da altura

    #função para indicar a hora
    humanTime = function() format(Sys.time(), "%Y%m%d-%H%M%OS")
    shiny::observeEvent(input$submit, {
      resp = shiny::isolate ({
        as.numeric ( c (input$pe.frio, input$escada, input$basquete, input$policial, input$carro, input$colegas, input$armario, input$porta, input$aviao, input$carro2, input$carona, input$foto, input$onibus, input$fila))
      })

      a1 = c (0.4232783, 1.0295999, 1.2833717, 0.8096345, 1.8143789, 1.5435942, 2.9310132, 0.9539005, 1.0113734,
              2.4803165, 1.0453012, 1.9615977, 1.5082983, 2.3080323)

      b = c (1.83680682, 1.36330492, 1.38053410, 1.44631675, 1.23284018, 1.27474620, -0.12924484, 2.99072132, -0.88812405,
             0.04909726, 0.45853523, 0.27753228, 0.91737338, 0.59221396)

      d = -a1 * b

      pars = data.frame (a1 = a1, d = d)

      mod = mirtCAT::generate.mirt_object (pars, itemtype = '2PL')

      # média e desvio da calibração da altur
      m = 0.000113344
      s = 0.9711545

      # média e desvio reais da altura
      alt.media = 1.692369668
      alt.dp = 0.093317355

      prof = reactive ({
        data.frame (mirt::fscores(mod, response.pattern = resp))$F1
      })

      altura = shiny::reactive ({
        alt = round (as.numeric (((prof()-m)/s) * alt.dp + alt.media), 2)
        alt
      })

      output$respostas = shiny::renderText (c ('Padrão de respostas:', resp))
      output$altura = shiny::renderText (c ('Altura estimada:', altura()))



      erro = shiny::reactive ({
        abs (round ((altura() - as.numeric (input$altura.real)), 2))
      })

      output$erro = shiny::renderText (paste0 ('Sua fita métrica errou por ', erro(), 'm.'))

    })





    # para salvar
    shiny::observeEvent(input$salvar, {
      resp = shiny::isolate ({
        as.numeric ( c (input$pe.frio, input$escada, input$basquete, input$policial, input$carro, input$colegas, input$armario, input$porta, input$aviao, input$carro2, input$carona, input$foto, input$onibus, input$fila))
      })

      a1 = c (0.4232783, 1.0295999, 1.2833717, 0.8096345, 1.8143789, 1.5435942, 2.9310132, 0.9539005, 1.0113734,
              2.4803165, 1.0453012, 1.9615977, 1.5082983, 2.3080323)

      b = c (1.83680682, 1.36330492, 1.38053410, 1.44631675, 1.23284018, 1.27474620, -0.12924484, 2.99072132, -0.88812405,
             0.04909726, 0.45853523, 0.27753228, 0.91737338, 0.59221396)

      d = -a1 * b

      pars = data.frame (a1 = a1, d = d)

      mod = mirtCAT::generate.mirt_object (pars, itemtype = '2PL')

      # média e desvio da calibração da altur
      m = 0.000113344
      s = 0.9711545

      # média e desvio reais da altura
      alt.media = 1.692369668
      alt.dp = 0.093317355

      prof = reactive ({
        data.frame (mirt::fscores(mod, response.pattern = resp))$F1
      })

      altura = reactive ({
        alt = round (as.numeric (((prof()-m)/s) * alt.dp + alt.media), 2)
        alt
      })

      output$respostas = shiny::renderText (c ('Padrão de respostas:', resp))
      output$altura = shiny::renderText (c ('Altura estimada:', altura()))

      erro = shiny::reactive ({
        abs (round ((altura() - as.numeric (input$altura.real)), 2))
      })

      output$erro = shiny::renderText (paste0 ('Sua fita métrica errou por ', erro(), 'm.'))


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
  }

  shiny::shinyApp(ui = ui, server = server)
}
