
#tabla_resultados <- readRDS("www/tabla_resultados.RDS")

#tabla_resultados[["ramas_sexo_df"]]


#colores <- c("#e5616e","#e9c1d0","#fbd17e","#8594c6","#8cddd3")


library(plotly)
library(shinyWidgets)
library(shinydashboard)


ramas_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
   
    
    armar_tabla <- function(dataframe,
                            valores_filter,
                            periodo_i,
                            periodo_f,
                            valuacion
    ){
      datagraf1 <- dataframe %>% 
        filter(`Rama de la ocupación` %in% valores_filter) %>%                          
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        mutate(ANO4 = as.character(round(as.numeric(ANO4),0)),
               TRIMESTRE = as.character(round(as.numeric(TRIMESTRE),0)))
      
      datagraf <- datagraf1%>% 
        
        filter(as.integer(periodo) %in% c(unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i])):unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))))%>% 
        
        select(-periodo,-trabajadoras_totales,"Año" = "ANO4",
               "Trimestre" = "TRIMESTRE", 
               "Rama de la ocupación",
               "Proporción del empleo femenino total", 
               "Tasa de feminización", 
               "Ingreso mensual promedio (precios corrientes)"="Ingreso mensual promedio",
               "Ingreso horario (precios corrientes)"="Ingreso horario",
               "Ingreso mensual promedio (precios constantes)" = "Ingreso mensual promedio (constante)",
               "Ingreso horario (precios constantes)" = "Ingreso horario (constante)"  )
      
      if(valuacion =="Precios corrientes"){
        
        datagraff <- datagraf %>% 
          select(-c( "Ingreso mensual promedio (precios constantes)","Ingreso horario (precios constantes)"))
        
        return(datagraff)
        
      } else if(valuacion ==paste0("Precios constantes (",nombre_trimestre_base,")")){
        
        datagraff <- datagraf %>% 
          select(-c("Ingreso mensual promedio (precios corrientes)","Ingreso horario (precios corrientes)"))
      }
        
        return(datagraff)
    }
    
    
    
    plot_i <- function(base,
                     vary,
                     eje_x,
                     valores_filter,
                     periodo_i,
                     periodo_f,
                     valuacion){
      
      if(valuacion =="Precios corrientes"){
        
       vary <- vary
        
      } else if(valuacion ==paste0("Precios constantes (",nombre_trimestre_base,")")){
        
        vary <- paste0(vary, " (constante)")
      }
      
      datagraf1 <- base %>%         
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        rename("ingreso" = vary)
      
      datagraf2 <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i])):unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))) %>% 
        filter(`Rama de la ocupación` %in% valores_filter)
       
      
      grafico <- ggplot(datagraf2, aes(x=periodo, y=ingreso, color=`Rama de la ocupación`, size =`Proporción del empleo femenino total`
                                       ,text=paste0('</br><b>',`Rama de la ocupación`,'</b></br>Período: ',periodo, '</br>', vary,': $',round(ingreso,2), '</br>Proporción del empleo femenino total: ', round(`Proporción del empleo femenino total`,1),'%')
                                       
      )) + 
        geom_point(alpha = .5)+
        scale_size(range = c(1, 15))+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.7),
              legend.position = "bottom",
              panel.background = element_rect(fill = "gray99", color = "gray90"),
              #plot.background = element_rect(fill="gray99", color = NA),
              strip.text.y = element_text(angle = 0),
              panel.grid.minor.y = element_blank()) +
        scale_color_manual(values = colores14) +
        labs(x = eje_x,
             y = vary,
             fill = "",
             caption = "Fuente: Elaboración propia en base a EPH-INDEC")+
        scale_y_continuous(labels = function(x) (paste0("$",x)))
      
      #grafico
      ggplotly(grafico, tooltip = c("text")) %>%
        layout(showlegend = F,
               font = list(family = "Times New Roman"))
    }
    
    
    
    plot_f <- function(base,
                       valores_filter, 
                       periodo_i, periodo_f){
      
      datagraf1 <- base %>% 
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        filter(`Rama de la ocupación` %in% valores_filter)
      
      tabla <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i])):unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))) 
      
      
      grafico <- ggplot(tabla, aes(periodo, `Tasa de feminización`, color = `Rama de la ocupación`, group = `Rama de la ocupación`
                                   ,text=paste0('</br><b>',`Rama de la ocupación`,'</b></br>Período: ',periodo,
                                                 '</br>Tasa de feminización: ',round(`Tasa de feminización`,2),'%')
      )) +
        geom_line(linewidth = 1, alpha = 0.75) +
        geom_point(size = 1) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.7),
              legend.position = "none",
              panel.background = element_rect(fill = "gray99", color = "gray90"),
          
              strip.text.y = element_text(angle = 0),
              panel.grid.minor.y = element_blank()) +
        scale_color_manual(values = colores14) +
        labs( x = "Período",
          y = paste0("Tasa de feminización"),
          color = "",
         
        )+ 
        scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) 
      
      
      
      grafico <- ggplotly(grafico, tooltip = c("text")) %>% layout(font = list(family = "Times New Roman"))
      
      return(grafico)
      
      
      
    }
    
    
    
    generar_titulo <- function(periodo_i, periodo_f,valuacion){
      titulo <- paste0("</br><b>","<font size='+2'>","Ingresos y tasa de feminización por rama de actividad. </font>","<font size='+1'>","</br> Desde ", periodo_i, " hasta ", periodo_f,". ","</font>","</br>", valuacion, ". Población con ingresos y horas trabajadas mayores a 0.","</b>")
      titulo
    }
    
    
    output$plot_ingreso <- renderPlotly({
      
      plot_i(tabla_resultados[["ramas_sexo_df"]],
           
           eje_x = "Período",
           vary = input$ingreso_id,
           valores_filter = input$ramas_id,
           input$id_periodo[1],
           input$id_periodo[2],
           input$precios_id) 
    })
    
    output$plot_feminizacion <- renderPlotly({
      
      plot_f(tabla_resultados[["ramas_sexo_df"]],
             valores_filter=input$ramas_id, 
             input$id_periodo[1],
             input$id_periodo[2]) 
    })
    
    
    
    
    
    output$tabla <- renderTable({
      armar_tabla(tabla_resultados[["ramas_sexo_df"]],
                  valores_filter = input$ramas_id,
                  input$id_periodo[1],
                  input$id_periodo[2],
                  input$precios_id
      )
    }#,
    #width="600px"
    )
    
    output$metadata <- renderText({tabla_metadata$metadata[tabla_metadata$indicador == paste0("Ramas de la ocupación")]})
    output$metadata_femi <- renderText({tabla_metadata$metadata[tabla_metadata$indicador == paste0("Tasa de feminización")]})
    output$metadata_ingresos <- renderText({tabla_metadata$metadata[tabla_metadata$indicador == paste0("Valuación")]})
    output$metadata_tamanios <- renderText({paste0("Nota: el tamaño de las esferas indica la proporción del empleo femenino total que representa la rama de actividad.")})
    
    output$titulo1 <- renderText({generar_titulo(input$id_periodo[1],input$id_periodo[2],
                                                 input$precios_id)})
    output$titulo2 <- renderText({generar_titulo(input$id_periodo[1],input$id_periodo[2],
                                                 input$precios_id)})
    
    output$downloadTable <- downloadHandler(
      
      filename = function(){paste('Ramas_actividad.xlsx',sep='')},
      content = function(file){
        
        write.xlsx(armar_tabla(tabla_resultados[["ramas_sexo_df"]],
                               valores_filter = input$ramas_id,
                               input$id_periodo[1],
                               input$id_periodo[2],
                               input$precios_id
        ), 
                   file)  
        
        shinyalert(
          title = "",
          text = texto_cita,
          size = "xs", 
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "ok!",
          confirmButtonCol = colores2[1],
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
        
        }
    )
    
  })
}



ramas_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Ramas de la actividad',
           
           
           tags$div(p("También puede interesarte: ", style= "text-align: right") ),
           
           tags$div( style = "display: flex;padding: 1px 0;justify-content: flex-end;",
             tags$a("Tasas básicas",style=paste0(btn_style,"background:#e2616e;border-color: #e2616e;"),
                    onclick="fakeClick('Tasas básicas')",
                    class="btn btn-primary btn-s"),
             tags$a("Tasas básicas por grupos de edad",style=paste0(btn_style,"background:#e2616e;border-color: #e2616e;"),
                    onclick="fakeClick('Por grupos de edad')",
                    class="btn btn-primary btn-s"),
             tags$a("Tipo de inserción laboral",style=paste0(btn_style,"background:#e2616e;border-color: #e2616e;"),
                    onclick="fakeClick('Tipo de inserción laboral')",
                    class="btn btn-primary btn-s")
             
           ),
           
           
           titlePanel('Ramas de la actividad'),
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('ramas_id'),label = 'Elegir ramas de actividad',
                           choices = ramas,
                           selected = ramas[c(2,4,11)],
                           multiple = TRUE),
               selectInput(ns('ingreso_id'),label = 'Elegir variable de ingreso',
                           choices = c("Ingreso mensual promedio", "Ingreso horario"),
                           selected = "Ingreso mensual promedio",
                           multiple = FALSE),
               selectInput(ns('precios_id'),label = 'Valuación:',
                           choices = c("Precios corrientes", paste0("Precios constantes (",nombre_trimestre_base,")")),
                           selected = paste0("Precios constantes (",nombre_trimestre_base,")"),
                           multiple = FALSE),
               sliderTextInput(ns('id_periodo'), "Trimestre:", choices = trimestres, selected = trimestres[c(1,length(trimestres))]),
               
               br(), 
               hr(), 
               h4("Sobre el indicador"), 
               h5(textOutput(ns('metadata'))), 
               br(), 
               tags$div( style="display: inline-flex;",  id = "div_1",
                         tags$p('Ver ejemplo: ', style="color: black;font-size:15px; font-family: 'News Cycle';
  font-style: normal"), 
                         HTML('&nbsp;'),
                         tags$a(" Trabajadoras de casas particulares", id = "sd_der",
                                
                                onclick="fakeClick('Derechos laborales')",
                                style="font-size:15px; font-family: 'News Cycle';
  font-style: normal"
                                
                         ),
               ),
               
               hr(),
               h5(textOutput(ns('metadata_femi'))), 
               hr(),
               h5(textOutput(ns('metadata_ingresos'))),
               hr(),
               h5(textOutput(ns('metadata_tamanios')))
               
             ),
             mainPanel( tabsetPanel(
               
               tabPanel("Gráfico",
                        value = "g_ramas",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo1'))), 
                        br(),
                        plotlyOutput(ns('plot_ingreso'), height = 500)%>% withSpinner(type = 5, color ="#e5616e"),
                        br(),
                        plotlyOutput(ns('plot_feminizacion'), height = 500)%>% withSpinner(type = 5, color ="#e5616e"),
                        br(),
                        
                        
                        tags$div( style="display: inline-flex;",  id = ns("fuentes"),
                                  tags$p(texto_fuentes), 
                                  HTML('&nbsp;'),
                                  tags$a("Metodología", id = ns("f_metod"),
                                         
                                         
                                         onclick="fakeClick('Metodología')"#,
                                         
                                  ),
                        )
                        
                        
               ),
               
               tabPanel("Tabla",
                        value = "t_ramas",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo2'))), 
                        br(),
                        fluidRow(
                          column(12,
                                 column(10, 
                                        box(tableOutput(ns('tabla')), width = 12)),
                                 column(2,   
                                        box(width = NULL,
                                            downloadButton(ns('downloadTable'),'Descargar tabla'))
                                        
                                        
                                 ))
                        ),
                        br(),
                        
                        
                        tags$div( style="display: inline-flex;",  id = ns("fuentes2"),
                                  tags$p(texto_fuentes), 
                                  HTML('&nbsp;'),
                                  tags$a("Metodología", id = ns("f_metod"),
                                         
                                         
                                         onclick="fakeClick('Metodología')"#,
                                         
                                  ),
                        )
                        
               )
               
               
             )
             
             
             )
             
             
           )
  )
}