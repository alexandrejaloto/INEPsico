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

  # ui ----------------------------------------------------------------------

  ui <- shiny::fluidPage(
    shiny::uiOutput("ui")
  )

  server <- function(input, output) {

    objetos <- reactiveValues(
      login = reactiveVal(FALSE),
      lingua = reactiveVal(c())
    )

# carrego o ui ------------------------------------------------------------

    output$ui <- shiny::renderUI({


      if (!objetos$login())
      {
        shiny::tagList(
          shiny::radioButtons(
            'lingua',
            '',
            choiceNames = c('Português', 'English'),
            choiceValues = c('port', 'ing')
          ),
          shiny::actionButton('logar', 'OK')
        )
      } else {

        source(
          file.path(.libPaths()[1], 'ui_app.R'),
          local = TRUE,
          encoding = 'utf-8'
        )$value

      }

    })

    observeEvent(input$logar, {

      objetos$lingua(input$lingua)
      objetos$login(TRUE)

      source(
        file.path(.libPaths()[1], 'server_app.R'),
        local = TRUE,
        encoding = 'utf-8'
      )$value

    })

  }

  shinyApp(ui = ui, server = server)


}
brincar()

