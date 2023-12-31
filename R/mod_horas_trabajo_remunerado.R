library(plotly)
library(shinyWidgets)
library(shinydashboard)


horas_remunerado_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    

    armar_tabla <- function(dataframe,
                            horas_var,
                            facet_var,
                            nombre_facet,
                            valores,
                            periodo_i,
                            periodo_f
    ){
      
      datagraf1 <- dataframe %>% 
        
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        rename("horas" = horas_var,
               "var_filtro" = facet_var) %>% 
        mutate(ANO4 = as.character(round(as.numeric(ANO4),0)),
               TRIMESTRE = as.character(round(as.numeric(TRIMESTRE),0)))
      
      
      datagraf <- datagraf1%>% 
        
        filter(as.integer(periodo) %in% c(unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i])):unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))) %>% 
        select(-periodo) %>% 
        
        filter(var_filtro %in% c(valores)) %>% 
        
        select("Año" = "ANO4", 
               "Trimestre" = "TRIMESTRE", 
               var_filtro,
               "Sexo",
               "Horas semanales" = "horas")
      
      str_nombre <- paste0(nombre_facet)
      
      colnames(datagraf)[3] <- str_nombre
      
    return(datagraf)
      
    
      
    }
    
    
   
    
    generar_titulo <- function(horas,facet_var, valores,periodo_i, periodo_f){
      titulo <- paste0('</br>',"<b>","<font size='+2'>","Horas semanales de trabajo remunerado (", horas , ") por ", facet_var,".","</font>", '</br>',"Desde ", periodo_i, " hasta ", periodo_f,"</b>")
      titulo
    }
    
    
    grafico <- function(base,var,
                                      facet_var,
                                      valores, 
                                      nombre,
                                      periodo_i, periodo_f
                                      
    ){
      
      
      t_union1 <- tabla_resultados[["horas_semanales_df"]]%>% 
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        mutate(var_facet = "General")%>% 
        rename("valor" = var) %>% 
        mutate(Grupo = paste0(var_facet,"-",Sexo)) %>% 
        mutate(resaltar = "Si")
      
      t_union2 <- tabla_resultados[[base]]%>% 
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        rename("var_facet" = facet_var)%>% 
        rename("valor" = var) %>% 
        filter(var_facet %in% c(valores)) %>% 
        mutate(Grupo = paste0(var_facet,"-",Sexo))%>% 
        mutate(resaltar = "No")
      
      datagraf1 <- bind_rows(t_union2,t_union1)
      
      
      t_unida <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i])):unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))) 
      
      grafico <- t_unida %>% 
        ggplot(.,aes(x = periodo, y = valor, color = Sexo, group = Grupo, alpha =resaltar, size = resaltar
                     ,text=paste0('</br><b>',var_facet, '</b></br>', Sexo, '</br>Horas: ',valor, '</br>Período: ',periodo
                                  )))+
        geom_line()+ 
        geom_point() +
        scale_alpha_discrete(range=c(.5,1))+
        scale_size_discrete(range=c(.5,1))+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.7),
              legend.position = "none",
              panel.background = element_rect(fill = "gray99", color = "gray90"),
             
              strip.text.y = element_text(angle = 0),
              panel.grid.minor.y = element_blank()) +
        scale_color_manual(values = colores2) +
        labs(title = "",
             x = "Periodo",
             y = paste0("Horas semanales de trabajo remunerado (", nombre, ")"),
             color = "")+
        guides(color = "none",
               alpha="none")
      
    
      grafico <- ggplotly(grafico, tooltip = c("text")) %>% layout(font = list(family = "Times New Roman"))
      
      return(grafico)
    }
    
    
    
    
    
    
    observe({
      x <- input$var_desag_id
      
      options = opciones_actualizacion %>%
        filter(variable %in% x) %>%
        pull(valores)
      
      
      updateSelectInput(session, 'valores_id',
                        choices = options,
                        selected = options#[1]
      )
    })
    
    output$plot <- renderPlotly({grafico(base = nombres_horas_remuneradas$tabla[nombres_horas_remuneradas$variable_desag_nombre == input$var_desag_id],
                                         var = unique(nombres_horas_remuneradas$cod[nombres_horas_remuneradas$nombre == input$horas_id]),
                                         facet_var=unique(nombres_horas_remuneradas$variable_desag[nombres_horas_remuneradas$variable_desag_nombre == input$var_desag_id]),
                                         valores = input$valores_id, 
                                         nombre = input$horas_id,
                                                periodo_i = input$id_periodo[1],
                                                 periodo_f = input$id_periodo[2]
    )})
    
    output$tabla <- renderTable({
      armar_tabla(tabla_resultados[[(nombres_horas_remuneradas$tabla[nombres_horas_remuneradas$variable_desag_nombre == input$var_desag_id])]],
                  
                  horas_var =unique(nombres_horas_remuneradas$cod[nombres_horas_remuneradas$nombre == input$horas_id]),
                  
                  facet_var=unique(nombres_horas_remuneradas$variable_desag[nombres_horas_remuneradas$variable_desag_nombre == input$var_desag_id]),
                  
                  nombre_facet = input$var_desag_id,
                  valores = input$valores_id,
                  input$id_periodo[1],
                  input$id_periodo[2]
      )
    }#,
    #width="600px"
    )
    
    output$metadata <- renderText({tabla_metadata$metadata[tabla_metadata$indicador == paste0("Horas de trabajo remunerado semanales")]})
    
    output$titulo1 <- renderText({generar_titulo(input$horas_id,input$var_desag_id, valores = input$valores_id,input$id_periodo[1],input$id_periodo[2])})
    output$titulo2 <- renderText({generar_titulo(input$horas_id,input$var_desag_id, valores = input$valores_id,input$id_periodo[1],input$id_periodo[2])})
    
    output$downloadTable <- downloadHandler(
      
      filename = function(){paste('Horas_',input$horas_id,'.xlsx',sep='')},
      content = function(file){
        
        write.xlsx(armar_tabla(tabla_resultados[[(nombres_horas_remuneradas$tabla[nombres_horas_remuneradas$variable_desag_nombre == input$var_desag_id])]],
                               
                               horas_var =unique(nombres_horas_remuneradas$cod[nombres_horas_remuneradas$nombre == input$horas_id]),
                               
                               facet_var=unique(nombres_horas_remuneradas$variable_desag[nombres_horas_remuneradas$variable_desag_nombre == input$var_desag_id]),
                               
                               nombre_facet = input$var_desag_id,
                               valores = input$valores_id,
                               input$id_periodo[1],
                               input$id_periodo[2]
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


horas_remunerado_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Trabajo remunerado',
           
           tags$div(p("También puede interesarte: ", style= "text-align: right") ),
           
           tags$div( style = "display: flex;padding: 1px 0;justify-content: flex-end;",
             tags$a("Trabajo no remunerado",style=paste0(btn_style,"background:#e7bfce;border-color: #e7bfce;color: black"),
                    onclick="fakeClick('Trabajo no remunerado')",
                    class="btn btn-primary btn-s")
             
           ),
           
           titlePanel('Horas de trabajo remunerado'),
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('horas_id'),label = 'Horas semanales de trabajo remunerado:',
                           choices = vector_horas,
                           selected = vector_horas[1],
                           multiple = FALSE),
               selectInput(ns('var_desag_id'),label = 'Elegir desagregación:',
                           choices = unique(nombres_horas_remuneradas$variable_desag_nombre),
                           selected = NULL,
                           multiple = FALSE),
               selectInput(ns('valores_id'),label = 'Elegir valores:',
                           choices = NULL,
                           selected = NULL,
                           multiple = T)
               
               ,
               sliderTextInput(ns('id_periodo'), "Trimestre:", choices = trimestres, selected = trimestres[c(1,length(trimestres))]),
               
               br(), 
               hr(), 
               h4("Sobre el indicador"), 
               h5(textOutput(ns('metadata')))
               
             ),
             mainPanel( tabsetPanel(
               
               tabPanel("Gráfico",
                        value = "g_horas_rem",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo1'))), 
                        br(),
                        plotlyOutput(ns('plot'), height = 600)%>% withSpinner(type = 5, color ="#e5616e"),
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
                        value = "t_horas_rem",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo2'))), 
                        br(),
                        fluidRow(
                          column(12,
                                 column(9, 
                                        box(tableOutput(ns('tabla')), width = 12)),
                                 column(3, 
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
