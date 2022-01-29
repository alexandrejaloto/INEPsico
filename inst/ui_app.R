library (catIrt)
library (dplyr)
library (mirtCAT)
library (mirt)
library (shiny)
library (tableHTML)
library (ggplot2)

# df_textos_brincar = INEPsico::df_textos_brincar
# df_textos_brincar = readxl::read_xlsx (
#   'df_textos_brincar.xlsx'
# ) %>% data.frame()

# criar objetos para radio e select input. eles vão aparecer lá embaixo e
# precisam ser com objetos porque dá erro se não for assim
radio = c(1, 0)
names (radio) = c (
  df_textos_brincar[df_textos_brincar$id == 'sim', objetos$lingua()],
  df_textos_brincar[df_textos_brincar$id == 'nao', objetos$lingua()]
)

padrao = list (
  'padrao',
  'Enem'
)

names(padrao) = c (
  df_textos_brincar[df_textos_brincar$id == 'padrao', objetos$lingua()],
  'Enem'
)


# ui ----------------------------------------------------------------------


# ui = shiny::fluidPage(
shiny::shinyUI(

  shiny::fluidRow(
    # usar o pacote para esconder o botão da CAT
    shinyjs::useShinyjs(),

    # navegação com barra de menu em cima
    shiny::navbarPage(
      df_textos_brincar[df_textos_brincar$id == 'brincando', objetos$lingua()],
      shiny::tabPanel(

        # Questionário ------------------------------------------------------------


        df_textos_brincar[df_textos_brincar$id == 'questionario', objetos$lingua()],
        shiny::column(
          5,
          shiny::wellPanel(
            shiny::div(
              id = 'form',
              shiny::textInput ('nome', label = df_textos_brincar[df_textos_brincar$id == 'nome', objetos$lingua()]),
              shiny::checkboxInput ('pe.frio', label = df_textos_brincar[df_textos_brincar$id == 'item_pefrio', objetos$lingua()], FALSE),
              shiny::checkboxInput ('escada', label = df_textos_brincar[df_textos_brincar$id == 'item_escada', objetos$lingua()], FALSE),
              shiny::checkboxInput ('basquete', label = df_textos_brincar[df_textos_brincar$id == 'item_basquete', objetos$lingua()], FALSE),
              shiny::checkboxInput ('policial', label = df_textos_brincar[df_textos_brincar$id == 'item_policial', objetos$lingua()], FALSE),
              shiny::checkboxInput ('carro', label = df_textos_brincar[df_textos_brincar$id == 'item_carro', objetos$lingua()], FALSE),
              shiny::checkboxInput ('colegas', label = df_textos_brincar[df_textos_brincar$id == 'item_colegas', objetos$lingua()], FALSE),
              shiny::checkboxInput ('armario', label = df_textos_brincar[df_textos_brincar$id == 'item_armario', objetos$lingua()], FALSE),
              shiny::checkboxInput ('porta', label = df_textos_brincar[df_textos_brincar$id == 'item_porta', objetos$lingua()], FALSE),
              shiny::checkboxInput ('aviao', label = df_textos_brincar[df_textos_brincar$id == 'item_aviao', objetos$lingua()], FALSE),
              shiny::checkboxInput ('carro2', label = df_textos_brincar[df_textos_brincar$id == 'item_carro2', objetos$lingua()], FALSE),
              shiny::checkboxInput ('carona', label = df_textos_brincar[df_textos_brincar$id == 'item_carona', objetos$lingua()], FALSE),
              shiny::checkboxInput ('foto', label = df_textos_brincar[df_textos_brincar$id == 'item_foto', objetos$lingua()], FALSE),
              shiny::checkboxInput ('onibus', label = df_textos_brincar[df_textos_brincar$id == 'item_onibus', objetos$lingua()], FALSE),
              shiny::checkboxInput ('fila', label = df_textos_brincar[df_textos_brincar$id == 'item_fila', objetos$lingua()], FALSE)
            )
          )
        ),
        shiny::column(
          6,
          shiny::numericInput ('altura.real', label = df_textos_brincar[df_textos_brincar$id == 'altura.real', objetos$lingua()], min = 0, max = 3, value = 0, step = 0.01),
          shiny::actionButton("submit", df_textos_brincar[df_textos_brincar$id == 'submit', objetos$lingua()], class = "btn-primary"),
          shiny::actionButton("salvar", df_textos_brincar[df_textos_brincar$id == 'salvar', objetos$lingua()], class = "btn-primary"),
          shiny::h3(shiny::textOutput("altura")),
          shiny::h3(shiny::textOutput("erro"))
        )
      ),

      # CAT da altura -----------------------------------------------------------

      shiny::tabPanel(
        df_textos_brincar[df_textos_brincar$id == 'cat_altura', objetos$lingua()],
        shiny::textOutput('enunc'),
        shiny::radioButtons(
          'it',
          '',
          radio
        ),
        shiny::actionButton("submit.cat", "OK", class = "btn-primary"),
        shiny::h5(df_textos_brincar[df_textos_brincar$id == 'itens_aplicados', objetos$lingua()], shiny::textOutput("aplicados")),
        shiny::h4(df_textos_brincar[df_textos_brincar$id == 'historico_altura', objetos$lingua()], shiny::textOutput("printar.alt")),
        shiny::plotOutput('plot_theta_hist', width = '40%'),
        shiny::h5(df_textos_brincar[df_textos_brincar$id == 'respostas', objetos$lingua()], shiny::textOutput("padrao")),
        shiny::h5(df_textos_brincar[df_textos_brincar$id == 'historico_theta', objetos$lingua()], shiny::textOutput("theta.hist")),
        shiny::h5(df_textos_brincar[df_textos_brincar$id == 'historico_erro', objetos$lingua()], shiny::textOutput("se.hist"))
      ),

      # Parâmetros --------------------------------------------------------------


      shiny::tabPanel(
        df_textos_brincar[df_textos_brincar$id == 'parametros', objetos$lingua()],
        # dividir o painel em colunas
        shiny::column(
          3,
          shiny::numericInput('n_item', df_textos_brincar[df_textos_brincar$id == 'n_item', objetos$lingua()], 1, min = 1, max = 3, width = '25%'),

          # caso haja pelo menos um item e seja escala (0,1)
          shiny::checkboxInput('mostra_info', df_textos_brincar[df_textos_brincar$id == 'mostra_info', objetos$lingua()]),
          shiny::checkboxInput('mostra_info_total', df_textos_brincar[df_textos_brincar$id == 'mostra_info_total', objetos$lingua()]),
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
          shiny::numericInput('n_part', df_textos_brincar[df_textos_brincar$id == 'n_part', objetos$lingua()], 0, min = 0, max = 2, width = '25%'),

          # caso haja pelo menos um item e seja escala (0,1)
          shiny::conditionalPanel(
            condition = "input.n_part >= 1 & input.escala == 'padrao'",
            shiny::sliderInput ('part1.p', value = -.5, min = -4, max = 4, label = df_textos_brincar[df_textos_brincar$id == 'part1.p', objetos$lingua()], step = .1, width = '30%')),
          shiny::conditionalPanel(
            condition = "input.n_part >= 2 & input.escala == 'padrao'",
            shiny::sliderInput ('part2.p', value = 1, min = -4, max = 4, label = df_textos_brincar[df_textos_brincar$id == 'part2.p', objetos$lingua()], step = .1, width = '30%')),


          # caso haja pelo menos um item e seja escala (500,100)
          shiny::conditionalPanel(
            condition = "input.n_part >= 1 & input.escala == 'Enem'",
            shiny::sliderInput ('part1.e', value = 500-100*.5, min = 100, max = 900, label = df_textos_brincar[df_textos_brincar$id == 'part1.e', objetos$lingua()], step = 10, width = '30%')),
          shiny::conditionalPanel(
            condition = "input.n_part >= 2 & input.escala == 'Enem'",
            shiny::sliderInput ('part2.e', value = 600, min = 100, max = 900, label = df_textos_brincar[df_textos_brincar$id == 'part2.e', objetos$lingua()], step = 10, width = '30%')),


          shiny::conditionalPanel(
            condition = "input.n_item >= 1
                                                        & input.n_part >= 1",
            shiny::checkboxInput('mostra_prob1', df_textos_brincar[df_textos_brincar$id == 'mostra_prob1', objetos$lingua()])
          )
        ),
        shiny::column(
          6,
          shiny::selectInput(
            'escala',
            df_textos_brincar[df_textos_brincar$id == 'escala', objetos$lingua()],
            choices = padrao
          ),
          shiny::plotOutput('plot_item1')
        )
      ),

      # estimar o theta ---------------------------------------------------------

      shiny::tabPanel(
        df_textos_brincar[df_textos_brincar$id == 'estimar_theta', objetos$lingua()],
        shiny::column(
          12, align = "left",
          shiny::HTML(df_textos_brincar[df_textos_brincar$id == 'descricao_estimar_theta', objetos$lingua()]),
          shiny::tags$table(
            shiny::tags$tr(
              shiny::tags$td(
                align = "left",
                df_textos_brincar[df_textos_brincar$id == 'resposta', objetos$lingua()]
              ),
              shiny::tags$td(shiny::selectInput('i1', 'Item 1', as.numeric(c(0,1)), width = '50px')),
              shiny::tags$td(shiny::selectInput('i2', 'Item 2', c(0,1), width = '50px')),
              shiny::tags$td(shiny::selectInput('i3', 'Item 3', c(0,1), width = '50px')),
              shiny::tags$td(shiny::selectInput('i4', 'Item 4', c(0,1), width = '50px')),
              shiny::tags$td(shiny::selectInput('i5', 'Item 5', c(0,1), width = '50px')),
              shiny::tags$td(shiny::selectInput('i6', 'Item 6', c(0,1), width = '50px')),
            ),
            shiny::tags$tr(
              shiny::tags$td(
                df_textos_brincar[df_textos_brincar$id == 'parametro_a', objetos$lingua()]
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
                df_textos_brincar[df_textos_brincar$id == 'parametro_b', objetos$lingua()]
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
                df_textos_brincar[df_textos_brincar$id == 'parametro_c', objetos$lingua()]
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
      ),


      # Sobre -------------------------------------------------------------------

      shiny::tabPanel(
        df_textos_brincar[df_textos_brincar$id == 'sobre', objetos$lingua()],
        shiny::HTML(df_textos_brincar[df_textos_brincar$id == 'txt_sobre', objetos$lingua()],)
      )
    )
  )
)
