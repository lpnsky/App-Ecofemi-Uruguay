library(shiny)
library(shinythemes)
library(shinycssloaders)
library(glue)
library(DT)
library(plotly)
library(gridExtra)
library(shinyjs)
library(shinydashboard)
library(shinyalert)
library(shinyWidgets)


ui <- fluidPage( 
    theme = shinytheme("lumen"),
    
    #define fakeClick for buttons
    (tags$head(tags$script(HTML('var fakeClick = function(tabName) {
                                                         var dropdownList = document.getElementsByTagName("a");
                                                         for (var i = 0; i < dropdownList.length; i++) {
                                                         var link = dropdownList[i];
                                                         if(link.getAttribute("data-value") == tabName) {
                                                         link.click();
                                                         };
                                                         }
                                                         };
                                                         '))) ),
    

    chooseSliderSkin("Modern", color = "#158cba"),
    
    tags$style(type = 'text/css', 
               HTML(".container-fluid > .nav > li > 
                        a[data-value='Metodología'] {background-color: #D3D3D3; color:black}
                      
                    ")),
    
    #y agregar esto?
    # .container-fluid > .nav > li > 
    #   a[data-value='Mercado de Trabajo'] {background-color: #e2616e; color:black}
    #       .container-fluid > .nav > li > 
    #       a[data-value='Ingresos'] {background-color: #687aad; color:black}
    #           .container-fluid > .nav > li > 
    #           a[data-value='Uso del tiempo'] {background-color: #e7bfce; color:black}
    #               .container-fluid > .nav > li > 
    #               a[data-value='Trabajadoras de Casas Particulares'] {background-color: #8adbd1; color:black}
    

    uiOutput(outputId = "main_ui") # defino un uiOutput llamado "main_ui"
)


##### server #####



server <- function (input, output,session) {
    
    
    
     #render UI 
    output$main_ui <- renderUI({
        main_ui}) # usa la función renderUI que se debe utilizar con los outputs "uiOutput" o "htmlOutput"
    
    ########## 
    
    # Output modules ----------------------------------------------------------
    #sample_plot_server('ejemplo')
    tasas_sexo_server('tasas_sexo')
    tasas_edad_server('tasas_edad')
    # tipo_insercion_server('jerarquias')
    # ramas_server('ramas')

    brechas_server('brechas_general')
    # brechas_desag_server('brechas_desag')
    # deciles_server('deciles')

    # horas_remunerado_server('horas_remuneradas')
    # horas_no_remunerado_server('horas_no_rem')


}


##### RUN #####

shinyApp(ui, server)

