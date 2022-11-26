
suppressPackageStartupMessages(library(Surrogate))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(soiltexture))
suppressPackageStartupMessages(library(psych) )
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(munsell))
suppressPackageStartupMessages(library(tmap))
#N<-200 # nombre de ligne 
#input<-RandVec(
#    a=0, b=1, # Min/max valeur
#   s=1, # somme des variables
#  n=3, # Nombre de variables
# m=N #Nombre de ligne
#)

### Transformons le format vecteur en dataframe

#mor<-t(input$RandVecOutput)%>%as.data.frame()



## Renommons les variables
#names(mor)<-c("CLAY","SILT","SAND")
#mor <- mor%>%mutate(CLAY=CLAY*100,SILT=SILT*100,SAND=SAND*100)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(h1("SOIL DATA")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("telechar","Telecharger un fichier"),
      #checkboxInput("snake", "Rename columns to snake case?"),
      #checkboxInput("constant", "Remove constant columns?"),
      #checkboxInput("empty", "Remove empty cols?"),
      br(),
      
      #numericInput("nombre","Choix du profil pedologique",value= 1),
      sliderInput("nombre1","Choix du profil pedologique",min = 1,max=1,value = 5,step = 1),
      downloadButton("file1",label = " fichier avec classe TT "),
      br(),
      br(),
      
      downloadButton("file2",label = "Plot Triangle Textural",)
      
      
      
      #selectInput("variable","Choix Variables",NULL,multiple = TRUE)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Summary","",
                 dataTableOutput("tete"),
                 h3("Summary"),
                 dataTableOutput("resum"),
                 br(),
                 h3("Caracterisation Profil"),
                 dataTableOutput("tab")),
        tabPanel("Analysis","",
                 fluidRow(
                   column(4,selectInput("type_triangle",label = "Choix Triangle",choices = c("USDA.TT","HYPRES.TT","FR.GEPPA.TT"))),
                   column(4,selectInput("variable1","Choix Var. categorielles",NULL)), 
                   column(4,selectInput("variable","Choix Var. quantitives",NULL,multiple = TRUE)))
                 ,
                 plotOutput("tt"),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 fluidRow(
                   textOutput(h3("titre")),
                   br(),
                   
                   column(12,plotlyOutput("regr"))
                 ),plotlyOutput("box")
                 
                 
        ),
        tabPanel("Spatial","",
                 tmapOutput("spat"),
                 plotOutput("soil_plot")
                 
                 
        )
      )
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  donnees <- reactive({
    req(input$telechar)
    vroom::vroom(input$telechar$datapath,delim = ";",locale = locale(decimal_mark=","))})
  output$tete <- renderDataTable(head(donnees()))
  
  observeEvent(donnees(),{
    updateSelectInput(session,inputId = "variable",choices = names(donnees()[,unlist(lapply(donnees(), is.numeric))])
    )})
  observeEvent(donnees(),{
    updateSliderInput(session,inputId = "nombre1",max =length(splita()) 
    )})
  observeEvent(donnees(),{
    updateSelectInput(session,inputId = "variable1",choices = names(donnees()[,unlist(lapply(donnees(), is.character))])
    )})
  #output$resum <- renderDataTable(head(as.data.frame(describe(donnees()[,-c(1,2)]))))
  output$ex <- DT::renderDT(describe(donnees()[,-c(1,2)]),
                            options = list(pageLength = 5)
  )
  output$resum <- renderDataTable(
    
    describe(donnees()[,-c(1,2)]),
    options = list(pageLength = 5)
  )
  
  #options = list(pageLength = 5)
  donnees_tt <- reactive(as.data.frame(na.omit(donnees()%>%select(c("CLAY","SILT","SAND")))))
  
  output$tt<- renderPlot({TT.plot(
    class.sys = input$type_triangle,
    tri.data = donnees_tt(),
    col = "red",
    main = "Triangle texturale"
  )},res = 92,width = 500,
  height = 650)
  output$hist <- renderTable( donnees()[,c(unlist(input$variable))])
  donnees1<-reactive(as.data.frame(donnees()[,c(unlist(input$variable1))]))
  donnees2<-reactive(as.data.frame(donnees()[,c(unlist(input$variable))]))
  
  output$titre<-renderText(glue::glue("Regression entre {names(donnees2())[1]} et {names(donnees1())[2]}"))
  output$regr <- renderPlotly(ggplotly(ggplot( data = donnees2(),aes(donnees2()[,1],donnees2()[,2]))+geom_smooth()+theme_minimal()+xlab(names(donnees2())[1])+ylab(names(donnees2())[2])+ ggtitle(glue::glue("Regression entre {names(donnees2())[1]} et {names(donnees2())[2]}" ))))
  output$box <- renderPlotly(ggplotly(ggplot( data = donnees1(),aes(donnees1()[,1],donnees2()[,1]))+geom_boxplot()+theme_minimal()+xlab(names(donnees1())[1])+ylab(names(donnees2())[1])+ ggtitle(glue::glue("Boxplot  {names(donnees1())[1]} et {names(donnees2())[1]}" ))))
  donnees3 <- reactive(sf::st_as_sf(as.data.frame(na.omit(donnees()[,c(1,2)])),coords=c("LNGI","LATI")))
  output$spat <- renderTmap(tmap::tm_shape(donnees3())+tm_dots())
  
  splita <- reactive({ sp <- donnees()%>%group_by(PRID)
  group_split(sp)})
  don <- reactive(as.data.frame(splita()[[input$nombre1]]))
  #%>%dplyr::mutate(hex=mnsl(fix_mnsl(don()$Munshell_col_hud,show_col_types = FALSE))
  #new_data <- reactive({don()%>%dplyr::bind_cols(mnsl(fix_mnsl(don()$Munshell_col_hud)))
  #})
  #fix_mnsl(mor$Munshell_col_hud)
  output$tab <- renderDataTable(don())
  #new_data1 <- reactive(splita$hex<-mnsl(splita$fix_mnsl(donnees()$Munshell_col_hud)))
  #output$tab <- renderDataTable(new_data1())
  #output$soil_plot<-renderPlot(ggplot(
  # data = new_data(),
  #aes(
  #x=class_TT,y=-epais_hor,
  #fill=fct_reorder(epais_hor,.desc=TRUE))
  #)+
  # geom_col(
  #  width=0.4
  #)+
  #scale_fill_manual(
  # breaks=new_data()$epais_hor,
  #values=new_data()$fix_mnsl(don()$Munshell_col_hud)
  #)+
  #guides(fill=FALSE)+
  #scale_x_discrete(position = "top")+
  #labs(
  #title = "Soil chronosequence",
  #y = "Depth (meters)",
  #x=""
  #)+
  #theme_minimal())
  data_sol <- reactive({donnees_compl <- cbind(donnees_tt(), "TEXCLASS" = TT.points.in.classes(tri.data  = donnees_tt()[,c("CLAY","SAND","SILT")],
                                                                                               css.names = c("CLAY",'SILT','SAND'),
                                                                                               class.sys = input$type_triangle, 
                                                                                               PiC.type  = "t",
                                                                                               collapse  = ', '))
  donnees_compl
  })
  
  
  output$file1 <- downloadHandler(
    
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file$name), ".csv")
    },
    content = function(file) {
      write.csv(data_sol(), file)
    }
  )
  #output$file2 <- downloadHandler(
  
  # filename = function() {
  #  paste0(input$file, ".png")
  #},
  #content = function(file) {
  # (data_sol(), file)
  #}
  #)
  output$file2 <- downloadHandler(
    filename = function() { paste("triangeTT", '.png', sep='') },
    content = function(file) {
      png(file)
      TT.plot(
        class.sys = input$type_triangle,
        tri.data = donnees_tt(),
        col = "red",
        main = "Triangle texturale"
      )
      dev.off()
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
