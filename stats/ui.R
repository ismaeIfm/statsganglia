
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  titlePanel(h1("Accounting")),
  fluidRow(
    
    column(3,
           dateRangeInput("dates", label = h3("Rango de Fechas"), 
                          start= "2014-02-10", min = "2014-02-10")
           )
  ),  
  
  
  mainPanel(
    tabsetPanel(
      tabPanel("Resumen",
               sidebarLayout(
                 sidebarPanel(
                   p(strong("A:"),	"abort"),
                   p(strong("C:"),	"checkpoint"),
                   p(strong("D:"),	"delete"),
                   p(strong("E:"),	"exit"),
                   p(strong("Q:"),	"queue"),
                   p(strong("R:"),	"rerun"),
                   p(strong("S:"),	"start"),
                   p(strong("T:"),	"restart")
                 ), 
                 mainPanel(plotOutput("summary"))
               ),
               strong("Trabajos Activos:"), textOutput("activeJobs"),
               h3("Usuarios Activos: "),
               plotOutput("usersActivity")
      ), 
      tabPanel("Usuarios",
               selectInput("select_users", label = h3("Usuario"), choices=c(" " = 1)), 
               h3(textOutput("actualUser")),
               h4("Actividad del usuario: "),
               plotOutput("activityByUser"), 
               h4(strong("Número de trabajos:"), textOutput("activeJobsByUser")),
               h4("Información de los trabajos del usuario: "),
               tableOutput("jobInformationByUser")
      ),
      tabPanel("Trabajos",
               selectInput("select_jobs", label = h3("Trabajo"), choices=c(" " = 1)),
               h4("Nombre: "),
               h2(textOutput("jobnameById")),
               sidebarLayout(
                 sidebarPanel(h5("Información Usuario: "),
                              h6("Usuario: "),
                              h3(strong(textOutput("usernameById"))),
                              h6("Grupo: "),
                              h3(textOutput("groupnameById"))
                              ), 
                 mainPanel(
                   fluidRow(
                     column(4,
                            h4(strong("Estado de Salida: ")),
                            textOutput("exitStatusById")
                     ),
                     column(4,
                            h4(strong("Número de CPUs listados: ")),
                            textOutput("listNCPUsById")
                     ), 
                     column(4,
                            h4(strong("ExecHost: ")),
                            textOutput("execHostById")
                     )
                   ),
                   
                   h3(strong("Bitácora: ")),
                   fluidRow(
                     column(4,
                            strong("Creación: "), 
                            textOutput("ctimeById"),
                            strong("Encolado: "),
                            textOutput("qtimeById")
                     ), 
                     column(4,
                            strong("Inicio: "), 
                           textOutput("startById"),
                           strong("Elegible: "),
                           textOutput("etimeById")
                     ),column(4,
                              strong("Fin: "), 
                              textOutput("endById"),
                              strong("Eliminado: "),
                              textOutput("dtimeById")
                     ) 
                   ),
                   h4(strong("Eliminado por: ")),
                   
                   h3(strong("Memoria: ")),
                   fluidRow(
                     
                     column(6,
                            strong("Memoria listada: "), 
                            textOutput("listMemById"),
                            strong("Memoria usada: "),
                            textOutput("usedMemById")
                     ),column(6,
                              strong("Memoria Virtual usada: "),
                              textOutput("usedVMemById")
                     ) 
                   ), 
                   h3(strong("Tiempos: ")),
                   fluidRow(
                     column(6,
                            strong("Tiempo de CPU listado: "), 
                            textOutput("listCPUTById"),
                            strong("Tiempo de CPU usado: "), 
                            textOutput("usedCPUTById")
                            
                     ),column(6,
                              strong("Walltime listado: "),
                              textOutput("listWalltimeById"),  
                              strong("Walltime usado: "),
                              textOutput("usedWalltimeById")

                     ) 
                   )
                 )
               )
      )
    ) 
  )
  
  
))