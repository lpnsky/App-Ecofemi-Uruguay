library(shiny)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(gt)
library(markdown)



about_ui <- tabPanel(title = 'Inicio',
                     #includeMarkdown('README.md')
                     
                     titlePanel(fluidRow(
                       column(9,'Visualizador de género'),
                       column(1, img(height = 100, width = 200, src = "img/ech_logo.png")))),
                     # ),
                     #            title=div('Visualizador de género',
                     #                      img(src="img/ech_logo.png",height = 100, width = 200,style="right:30px;"))),
                      

                     fluidRow(
                       column(12,
                              tags$div(style = "display:flex",

                                     tags$div( class="panel panel-primary",
                                               style = "border-color: #4f6c8d;width:50%",
                                               
                                               a(tags$div(class= "panel-heading",
                                                        style="background:#4f6c8d;border-color: #4f6c8d;",
                                                        h3('Mercado de Trabajo',style="color:#ffffff;")),
                                                 onclick="fakeClick('Tasas básicas')",
                                                 style ="text-decoration: none !important;"
                                               )
                                               ,
                                               
                                               tags$div(class="panel-body", style="display: flex;",
                                                        
                                                        a(img(height = 100, width = 100,src = "img/trabajo.png", style="flex: 0 0 15%;")
                                                          ,  onclick="fakeClick('Tasas básicas')")
                                                          ,
                                                        
                                                        a(p('En esta sección presentamos las tasas básicas del mercado de trabajo desagregadas por sexo, así como también por grupo etario. Por otro lado, estudiamos las condiciones de inserción laboral de las mujeres y los varones a partir de su jerarquía y rama de actividad.', style="text-align: left;font-size:18px;")
                                                          ,  onclick="fakeClick('Tasas básicas')",
                                                          style ="text-decoration: none !important;color: black !important;")
                                                        
                                                   
                                               ),tags$div(class="panel-body", style = "display: flex;padding: 1px 0;justify-content: flex-end",
                                                 tags$a("Tasas básicas",style=paste0(btn_style,"background:#4f6c8d;border-color: #4f6c8d;font-size:14px"),
                                                        onclick="fakeClick('Tasas básicas')",
                                                        class="btn btn-primary btn-s")),
                                               
                                               tags$div(class="panel-body",  style = "display: flex;padding: 1px 0;justify-content: flex-end",
                                                 tags$a("Tasas básicas por grupos de edad",style=paste0(btn_style,"background:#4f6c8d;border-color: #4f6c8d;font-size:14px"),
                                                        onclick="fakeClick('Por grupos de edad')",
                                                        class="btn btn-primary btn-s"))
                                                 
                                               ,tags$div(class="panel-body", style = "display: flex;padding: 1px 0;justify-content: flex-end",
                                                 
                                                 
                                                 
                                                 
                                               )
                                               
                                   
                                    
                              ),  
                              
                              #espacio entre cajas
                              HTML('&nbsp;'),
                              HTML('&nbsp;'),
                              HTML('&nbsp;'),
                              
                       
                                     
                              tags$div( class="panel panel-primary",
                                        style = "border-color: #f2dd63;width:50%",
                                        
                                        
                                        a(tags$div( class= "panel-heading",
                                                    style="background:#f2dd63;border-color: #f2dd63",
                                                    h3('Ingresos')),
                                          onclick="fakeClick('Brechas de ingresos - general')",
                                          style ="text-decoration: none !important;"
                                        )
                                        ,
                                        
                                        tags$div(class="panel-body",style="display: flex;",
                                                 a(img(height = 100, width = 100,src = "img/ingresos.png", style="flex: 0 0 15%;"),
                                                   onclick="fakeClick('Brechas de ingresos - general')"
                                                 ),
                                                 
                                                 a(p('En esta sección estudiamos las brechas de ingresos entre mujeres y varones, así como también la distribución de la población de cada sexo entre los distintos deciles de ingreso.', style="text-align: left;font-size:18px")
                                                   ,  onclick="fakeClick('Brechas de ingresos - general')",
                                                   style ="text-decoration: none !important;color: black !important;")
                                                 
                                        ),tags$div(class="panel-body",  style = "display: flex;padding: 1px 0;justify-content: flex-end",
                                                   tags$a("Brechas de ingresos - general", style=paste0(btn_style,"background:#f2dd63;border-color: #f2dd63;font-size:14px;color:#000000"),
                                                          onclick="fakeClick('Brechas de ingresos - general')",
                                                          class="btn btn-primary btn-s")),
                                        
                                        
                                       
                                               
                                              
                                     )
                            #  )
                             
                              
                       )
                       )
                     ),
                     
                     

                              #espacio entre cajas
                             HTML('&nbsp;'),
                             HTML('&nbsp;'),
                             HTML('&nbsp;')
                            
                           
                    
                     ,
                     br(),
                     br()

                                          )




main_ui <- {
  
 
  
  navbarPage('Visualizador de género',
             
            
             about_ui,
            
          
             
             navbarMenu(title = 'Mercado de Trabajo',
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"
                        ),
             tasas_sexo_ui('tasas_sexo'),
             tasas_edad_ui('tasas_edad'),
             # tipo_insercion_ui('jerarquias'),
             # ramas_ui('ramas')
             ),
             
             
           
             
             
             navbarMenu(title = 'Ingresos',
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"
                        ),
             brechas_ui('brechas_general'),
             # brechas_desag_ui('brechas_desag'),
             # deciles_ui('deciles')
             
             tags$footer(HTML("El código para realizar esta app se basa en el proporcionado por <a href='https://ecofeminita.com/?v=9f72f02c2586' target='_blank' style='color: #158cba;'>Ecofeminita</a>, disponible en <a href='https://github.com/Ecofeminita/shinyapp' target='_blank' style='color: #158cba;'>Github</a>."))
           
             )
             
            
             

                        #remunerado
                        # horas_remunerado_ui('horas_remuneradas'),
                        # #no remunerado
                        # horas_no_remunerado_ui('horas_no_rem')


             
       

                        #principales indicadores del informe
                        # serv_dom_ocupadas_ui('s_d_ocup'),
                        # serv_dom_ing_ui('s_d_ing'),
                        # serv_dom_derechos_ui('s_d_derechos')





             
         #    ,metodologia_ui,
             
           # ,  footer=tags$a("Volver", style=btn_style, id = "a_ecofem",
           #          href="https://ecofeminita.com/",
           #          class="btn btn-primary btn-s"
           # 
           #   )
             
  )
}
