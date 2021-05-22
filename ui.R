library(shiny)
library(shinythemes)
library(shinydashboard)



header = dashboardHeader(title = tags$img(src = "logof.png", height = "50", width = "150"), 
                         dropdownMenu(type = "messages", 
                                      messageItem(
                                          from = "Felipe Cortés",
                                          message = "Click here to go to LinkedIn!",
                                          href = "https://www.linkedin.com/in/felipe-cortes-bello/",
                                          icon = icon("linkedin", "fa-2x")),
                                      
                                      messageItem(
                                        from = "Emilio Gnecco",
                                        message = "Click here to go to LinkedIn!",
                                        href = "https://www.linkedin.com/in/emiliognecco/",
                                        icon = icon("linkedin", "fa-2x")),
                                      
                                      messageItem(
                                        from = "Catalina Rojas",
                                        message = "Click here to go to LinkedIn!",
                                        href = "https://www.linkedin.com/in/mcrv98/",
                                        icon = icon("linkedin", "fa-2x")),
                                      
                                      icon = icon("linkedin", "fa-2x")))



sidebar  = dashboardSidebar(sidebarMenu(
    menuItem("iBanking",
             tabName = "IB",
             icon = icon("sketch")),
    
    
    menuItem("WACC",
             tabName = "wacc",
             icon = icon("chart-pie"),
             menuSubItem("Ke", tabName = "ke"),
             menuSubItem("Betas", tabName = "betas"),
             menuSubItem("Retorno de mercado", tabName = "ret_mer"),
             menuSubItem("Wacc", tabName = "cn_wacc")),
    

    menuItem("Options",
             tabName = "options",
             icon = icon("clock"))
    
    
))

body = dashboardBody(tags$script(HTML("$('body').addClass('fixed');")),
                     tabItems(
                         tabItem(tabName = "IB", h2(tags$b("iBanking: Accesible, fácil de usar y poderosa"), align = "center"), br(),
                                 
                                 h4(style="text-align: justify;", "Bienvenido a iBanking, una aplicación para profesionales de la 
                                    indutria financiera. Esta app está pensada para banqueros y quants que quieran hacer cálculos 
                                    desde la web o desde su dispositivo móvil, todo muy rápido y con gran alcance. Por ahora nos 
                                    encontramos en fase beta con perspectivas de incorporar más herramientas. Por tal motivo
                                    ahora solo verás dos grandes rubros:
                                     tasas de descuento y opciones. Te invitamos a explorar cada uno de ellos."), br(),
                                 
                                 h4(strong("¡Bienvenido al futuro de las finanzas!")),
                                 
                                 HTML('<center><img src="ib.png" width="1000"></center>'),
                            
                                 
                                 h4(style="text-align: justify;", "Para sugerencias por favor escribir a: felipecortesbusiness@gmail.com")
                                 
                                 
                                 
                                 ),
                         
                         
                         
                         tabItem(tabName = "ke", h3(tags$b("Calculo costo de patrimonio"), align = "center"),
                                 tabBox(width = 12,
                                        
                                        tabPanel(title = tags$b("CAPM"), h3(tags$b("Método 1, costo de patrimonio")), 
                                                 sidebarLayout(
                                                     sidebarPanel(
                                                         
                                                         uiOutput("risk_free_CAPM"),
                                                         h6("*Por defecto bono soberano a 10 años de EEUU"),
                                                         numericInput(inputId = "RM", label = "Digite el retorno del mercado (%)*", value = 0),
                                                         h6("*Si no cuenta con un RM ingrese a la pestaña de retorno de mercado"),
                                                         numericInput(inputId = "beta_m1", label = "Digite un beta*", value = 0),
                                                         h6("*Si no cuenta con un beta ingrese a la pestaña de betas"),
                                                         actionButton(inputId = "calcular_CAPM", label = "Calcular Ke")
                                                         
                                                     ),
                                                     
                                                     mainPanel(
                                                         
                                                         plotlyOutput("FED"),
                                                         infoBoxOutput("Ke1", width = "100%")
                                                         
                                                         
                                                     )
                                                     
                                                 )
                                                
                                                 
                                                 ),
                                        tabPanel(title = tags$b("CAPM internacional"), h3(tags$b("Método 2, costo de patrimonio")),
                                                 
                                                 sidebarLayout(
                                                     sidebarPanel(
                                                         
                                                         
                                                         numericInput(inputId = "beta_m2", label = "Digite un beta desapalancado*", value = 0),
                                                         h6("*Si no cuenta con un beta ingrese a la pestaña de betas"),
                                                         numericInput(inputId = "tx", label = "Digite tasa de impuestos (%)", value = 0),
                                                         numericInput(inputId = "CS", label = "Digite la relación deuda/ capital", value = 0),
                                                         uiOutput("risk_free_CAPM2"),
                                                         h6("*Por defecto bono soberano a 10 años de EEUU"),
                                                         numericInput(inputId = "RM2", label = "Digite el retorno del mercado estadounidense (%)*", value = 0),
                                                         h6("*Si no cuenta con un RM ingrese a la pestaña de retorno de mercado"),
                                                         numericInput(inputId = "Country_risk", label = "Digite prima por riesgo país (%)*", value = 0),
                                                         h6("*Si no cuenta con una, diríjase a la pestaña de Contry Risk Premium"),
                                                         numericInput(inputId = "dev", label = "Digite tasa de devaluación de moneda local (%)", value = 0),
                                                         actionButton(inputId = "calcular_CAPM2", label = "Calcular Ke")
                                                         
                                                     ),
                                                     
                                                     mainPanel(
                                                         
                                                         box(title = "Valor Ke por CAPM", solidHeader = T, background = "navy", collapsible = T, collapsed = F, width = "100%",
                                                             
                                                             infoBoxOutput("ke2", width = "100%")
                                                             
                                                             )
                                                         
                                                         
                                                         
                                                     )
                                                     
                                                 )                                                 
                                                 
                                                 
                                                 
                                                 
                                                 
                                                 ),
                                        tabPanel(title = tags$b("Country Risk Premium"), h3(tags$b("Prima por riesgo país. Datos de Damodaran")),
                                                 
                                                 dataTableOutput("CRP")
                                                 
                                                 
                                                 )
                                        
                                        
                                        )
                                 
                                 
                                 
                                 ),
                         
                         
                         tabItem(tabName = "ret_mer", h3(tags$b("Retorno de mercado"), align = "center"),
                                 
                                 sidebarLayout(
                                     sidebarPanel(
                                         
                                         dateRangeInput(inputId = "date_rm", label = "Escoja el periodo de análisis", start = "2016-01-01", end = Sys.Date()),
                                         br(),
                                         radioButtons(inputId = "market", label = "Escoja el benchmark de referencia de mercado", choices = c("S&P" = "^GSPC", "NASDAQ 100" = "^IXIC", "Dow Jones" = "^DJI", "Russell 2000" = "^RUT", "IBEX 35" = "^IBEX", "DAX 30" = "^GDAXI", "FTSE 100" = "^FTSE", "Nikkei 225" = "^N225")),
                                         infoBoxOutput("return", width = "100%")
                                         
                                     ),
                                     
                                     mainPanel(
                                         
                                         plotlyOutput("prices_market")
                                         

                                     )
                                     
                                 )
                                 
                                 
                                 ),
                         
                         
                         
                         tabItem(tabName = "betas", h3(tags$b("Cálculo de betas"), align = "center"),
                                 
                                 tabBox(width = 12,
                                     
                                     tabPanel(title = tags$b("Betas de regresión"),
                                             
                                              sidebarLayout(
                                                  sidebarPanel(
                                                      
                                                      dateRangeInput(inputId = "date_beta", label = "Escoja el periodo de análisis", start = "2016-01-01", end = Sys.Date()),
                                                      br(),
                                                      textInput(inputId = "ticker", label = "Digite el ticker de la acción", value = "AAPL"),
                                                      br(),
                                                      radioButtons(inputId = "benchi", label = "Escoja el benchmark de referencia de mercado", choices = c("S&P" = "^GSPC", "NASDAQ 100" = "^IXIC", "Dow Jones" = "^DJI", "Russell 2000" = "^RUT")),
                                                  
                                                      
                                                  ),
                                                  
                                                  mainPanel(
                                                      
                                                      plotlyOutput("reg_f"),
                                                      infoBoxOutput("beta_value"),
                                                      infoBoxOutput("r_squared"),
                                                      infoBoxOutput("corre")
                                                  )
                                                  
                                              )
                                              
                                               

                                              
                                              
                                              ),
                                     tabPanel(title = tags$b("Bottom- up betas"), h3(tags$b("Bottom up betas. Datos de Damodaran")),
                                              
                                              dataTableOutput("damobet")
                                              
                                              )
                                     
                                 )
                                 
                                 
                                 ),
                         
                         
                         
                         tabItem(tabName = "cn_wacc", h3(tags$b("Consolidado"), align = "center"),
                                 
                                 h4("Si llegó hasta esta pestaña se asume que ya tiene un costo de patrimonio establecido. De no ser así, visite primero la pestaña 'Ke' y según sea necesario determine el costo del patrimonio bajo CAPM tradicional o CAPM internacional. 
                                    Si ya cuenta con ese costo, llené los datos restantes para hallar el costo promedio ponderado del capital (WACC)."),
                                 
                                 sidebarLayout(
                                     
                                     sidebarPanel(
                                         
                                         selectInput(inputId = "method", label = "¿Qué método eligió para calcular Ke?", choices = c("CAPM tradicional" = "tradicional", "CAPM internacional" = "internacional")),
                                         numericInput(inputId = "kd", label = "Digite el costo de la deuda antes de impuestos (%)", value = 0),
                                         numericInput(inputId = "txx", label = "Digite la tasa impositiva (%)", value = 0),
                                         numericInput(inputId = "NET", label = "Digite la relación deuda/ activos (%)", value = 0)
                                         
                                         
                                         
                                     ), 
                                     
                                     mainPanel(
                                         
                                        fluidRow(
                                            
                                            infoBoxOutput("ke_final", width = 6),
                                            infoBoxOutput("kd_final", width = 6)

                                            
                                        ), 
                                        
                                        infoBoxOutput("wacc_final", width = "100%"),
                                        
                                        plotlyOutput("pie_wacc")
                                         
                                         
                                     ))
                                 
                                 
                                 
                                 
                                 ),
                         
                         
                         
                         tabItem(tabName = "options", h2(tags$b("Calculadora de opciones"), align = "center"),
                                 tabBox(width = 12, 
                                        
                                        tabPanel(title = tags$b("Valoración"), h3(tags$b("Valoración de opciones")),
                                                 
                                                 fluidRow(
                                                     column(width = 5, box(width = NULL, background = "navy", numericInput(inputId = "spot", label = "Digite el precio de mercado del subyacente", value = 0),
                                                                           numericInput(inputId = "strike", label = "Digite el precio de ejercicio de la opción", value = 0),
                                                                           numericInput(inputId = "sigma", label = "Digite la desviación estándar anual (%) del subyacente", value = 0),
                                                                           numericInput(inputId = "t", label = "Digite el tiempo al vencimiento en años", value = 0),
                                                                           numericInput(inputId = "y", label = "Digite yield anual (%) en caso de existir", value = 0),
                                                                           uiOutput("rf"),
                                                                           h6("*Por defecto bono soberano a 10 años de EEUU"),
                                                                           actionButton(inputId = "correr_options", label = "Calcular")
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           )),
                                                     column(width = 7, box(width = NULL, 
                                                                           infoBoxOutput("call", width = "100%"),
                                                                           infoBoxOutput("put", width = "100%")
                                                                           
                                                                           ))
                                                     
                                                 )
                                                 
                                                 ),
                                        
                                        
                                        tabPanel(title = tags$b("Volatilidad Implícita"), h3(tags$b("Cálculo de volatilidad implícita")),
                                                 
                                                 fluidRow(column(width = 6, box(width = NULL, background = "navy", h3("Volatilidad implícita put", align = "center"), 
                                                                 
                                                                 numericInput(inputId = "spot2", label = "Digite el precio de mercado del subyacente", value = 0, width = "100%"),
                                                                 numericInput(inputId = "strike2", label = "Digite el precio de ejercicio", value = 0, width = "100%"),
                                                                 numericInput(inputId = "tt", label = "Digite el tiempo al vencimiento en años", value = 0, width = "100%"),
                                                                 numericInput(inputId = "put2", label = "Digite el precio de mercado del put", value = 0, width = "100%"),
                                                                 uiOutput("rf2"),
                                                                 h6("*Por defecto bono soberano a 10 años de EEUU"),
                                                                 actionButton(inputId = "correr_options2", label = "Calcular"),
                                                                 br(),
                                                                 br(),
                                                                 infoBoxOutput(outputId = "putvolat", width = "100%"),
                                                                 br()
                                                                 
                                                                 
                                                                 )),
                                                          column(width = 6, box(width = NULL, background = "navy", h3("Volatilidad implícita call", align = "center"),
                                                                 
                                                                 
                                                                 numericInput(inputId = "spot3", label = "Digite el precio de mercado del subyacente", value = 0, width = "100%"),
                                                                 numericInput(inputId = "strike3", label = "Digite el precio de ejercicio", value = 0, width = "100%"),
                                                                 numericInput(inputId = "ttt", label = "Digite el tiempo al vencimiento en años", value = 0, width = "100%"),
                                                                 numericInput(inputId = "call2", label = "Digite el precio de mercado del call", value = 0, width = "100%"),
                                                                 uiOutput("rf3"),
                                                                 h6("*Por defecto bono soberano a 10 años de EEUU"),
                                                                 actionButton(inputId = "correr_options3", label = "Calcular"),
                                                                 br(),
                                                                 br(),
                                                                 infoBoxOutput(outputId = "callvolat", width = "100%"),
                                                                 br()
                                                                 
                                                                 
                                                                 ))), box(width = 12, title = h3(tags$b("Sobre el VIX"), align = "center"),
                                                                          
                                                                          sidebarLayout(sidebarPanel(
                                                                              
                                                                          dateRangeInput(inputId = "date", label = "Seleccione fechas para graficar el VIX", start = "2015-01-01", end = Sys.Date()),
                                                                          infoBoxOutput("last_vix", width = "100%")
                                                                              
                                                                              
                                                                          ),
                                                                          
                                                                          mainPanel(
                                                                              
                                                                          plotlyOutput("VIX")      
                                                                              
                                                                              
                                                                          ))

                                                                          
                                                                          )), 
                                        
                                        
                                        tabPanel(title = tags$b("Griegas"), h3(tags$b("Consulte aquí las griegas de la opción")),
                                                                               
                                                                               sidebarLayout(
                                                                                   sidebarPanel(
                                                                                       
                                                                                       radioButtons(inputId = "select", label = "Seleccione el tipo de opción", choices = c("Put" = "put_greeks", "Call" = "call_greeks"))
                                                                                   ),
                                                                                   
                                                                                   mainPanel(
                                                                                       infoBoxOutput("delta", width = "100%"),
                                                                                       infoBoxOutput("gamma", width = "100%"),
                                                                                       infoBoxOutput("vega", width = "100%"),
                                                                                       infoBoxOutput("theta", width = "100%"),
                                                                                       infoBoxOutput("rho", width = "100%")
                                                                                       
                                                                                   )
                                                                                   
                                                                                   
                                                                               )    
                                                 
                                                                        )))
                         

                         ))                                                                             
          

ui <- dashboardPage(header = header, sidebar = sidebar, body = body, skin = "blue")





