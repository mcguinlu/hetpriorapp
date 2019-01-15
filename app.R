#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(invgamma)
hetdata <- read.csv("Bayesian Priors WebTech.csv")

##########
#   UI   #
##########

ui <- fluidPage( #####
    titlePanel(h2("Heterogeneity Priors for Bayesian Meta-Analysis", align = "center"),windowTitle = "hetprior"),
    navbarPage(em("hetprior"),
               theme = shinythemes::shinytheme("flatly"),
                tabPanel("Home",
                       sidebarLayout(
                          sidebarPanel(
                            br(),
                            h4("Quick Guide"),
                            p("Use the ", em("Look-up"), "tab to quickly find the details of your required distribution."),
                            p("Use the ", em("Explore"), "tab to browse the complete database of prior distribtuions."),
                            br(),
                            h4("Development"),
                            p("This app was developed by the ",
                              a("Bristol Appraisal & Review of Reserach (BARR)",
                                     href = "https://bristol.ac.uk/population-health-sciences/centres/cresyda/barr/"),
                              " group. Team member details can be found in the ",
                              em("About"), "
                              section.")
                          ),
                       
                            mainPanel(
                            h2("Introducing", em("hetprior")),
                            h3("Background"),
                            p(em("hetprior")," is a new suite of tools from the BARR group at the University of Bristol",
                            "which makes it easy to look-up an informative heterogeneity prior for use in your own Bayesian meta-analysis.")
                            # br(),
                            # p("For an introduction more examples, visit the ",
                            #   a("Github account",
                            #     href = "https://github.com/mcguinlu")),
                            # br(),
                            # h3("Features"),
                            # p("- ", em("To be completed. . . "))
                                  )
                                   )
                       ),
             
             tabPanel("Look-up",
                      sidebarLayout(
                        fluid = TRUE,
                        
                                    sidebarPanel(
                                          width = 3,
                                          
                                          p("Please complete the boxes below."),
                                          p("Note: subsequent options are updated based on input."),
                                          
                                          selectizeInput("hetstat", label = "Heterogeneity statistic:",
                                                         choices = c(unique(as.character(hetdata$Heterogeneity.statistic))),
                                                         options = list(placeholder = 'Choose',
                                                                        onInitialize = I('function() { this.setValue(""); }'))),
                                          
                                          uiOutput("datatypeui"),
                                          uiOutput("effectmeasureui"),
                                          uiOutput("distformui"),
                                          uiOutput("interventiontypeui"),
                                          uiOutput("natureoutcomeui"),
                                          uiOutput("actiondo1"),
                                          uiOutput("medicalareaui"),
                                          uiOutput("samplesizeui"),
                                          uiOutput("actiondo2"), 
                                          br(),
                                          actionButton("clear","Clear")
                                          

                                    
                                          ),
                                    
                                    mainPanel(
                                    conditionalPanel(
                                      "output.buttonclick == 1",
                                         width = 9, offset =3,
                                      fluidRow(
                                          column(3, uiOutput("meanui")), 
                                          column(3, uiOutput("sdui"))
                                              ),
                                      fluidRow(
                                        column(3, uiOutput("medianui")),
                                        column(3, uiOutput("lowquantui")),
                                        column(3, uiOutput("highquantui"))
                                              ),
                                          uiOutput("notes1ui"),
                                          uiOutput("plotui"),
                                          br(p(em("Reference:"), textOutput("reference")))
                                          # will need to move this to the server and replace with a uiOuput
                                          # because we need to seperate Mean/SD vs Shape/Rate based on input$distributionform

                                          )
                                    )
                                    )
                      ),
                      
             tabPanel("Explore",
                      
                      fluidRow(
                        column(2,
                               selectInput("hetstat_e",
                                           "Heterogeneity statistic:",
                                           c("All",
                                             unique(as.character(hetdata$Heterogeneity.statistic))))),
                        column(2,
                                selectInput("datatype_e",
                                            "Data type:",
                                            c("All",
                                              unique(as.character(hetdata$Data.Type))))),
                        column(2,
                               selectInput("effectmeasure_e",
                                           "Effect measure:",
                                           c("All",
                                             unique(as.character(hetdata$Effect.Measure))))),
                        column(2,
                               selectInput("distributionform_e",
                                           "Distribution form:",
                                           c("All",
                                             unique(as.character(hetdata$Distribution.form))))),
                        column(2,
                               selectInput("interventiontype_e",
                                           "Intervention type:",
                                           c("All",
                                             unique(as.character(hetdata$Type.of.Intervention))))),
                        column(2,
                               selectInput("natureoutcome_e",
                                           "Outcome nature:",
                                           c("All",
                                             unique(as.character(hetdata$Nature.of.Outcome)))))

                        ),
                      
                      fluidRow(column(10, offset = 1,DT::dataTableOutput("table_explore"))
                               ) ),
             
             
             tabPanel("References",
                      h3("References"),
                      p("The data for this project came from a series of 5 papers. These are detailed below:"),
                      br(),
                      tags$ul(
                        tags$li(p("Turner, Rebecca M., Jonathan Davey, Mike J. Clarke, Simon G. Thompson, and Julian PT Higgins.",
                                  a(em("Predicting the extent of heterogeneity in meta-analysis, using empirical data from the Cochrane Database of Systematic Reviews. "),
                                    href = "https://doi.org/10.1093/ije/dys041"),
                                  "International Journal of Epidemiology 41, No. 3 (2012): 818-827. ")),
                        br(),
                        tags$li(p("Turner, Rebecca M., Dan Jackson, Yinghui Wei, Simon G. Thompson, and Julian PT Higgins.",
                                  a(em("Predictive distributions for between-study heterogeneity and simple methods for their application in Bayesian meta-analysis. "),
                                    href = "https://doi.org/10.1002/sim.6381"),
                                    "Statistics in Medicine 34, No. 6 (2015): 984-998. ")),
                        br(),
                        tags$li(p("Rhodes, Kirsty M., Rebecca M. Turner, Ian R. White, Dan Jackson, David J. Spiegelhalter, and Julian PT Higgins.",
                                  a(em("Implementing informative priors for heterogeneity in meta?analysis using meta-regression and pseudo data. "),
                                    href = "https://doi.org/10.1002/sim.7090"),
                                  "Statistics in Medicine 35, No. 29 (2016): 5495-5511. ")),
                        br(),
                        tags$li(p("Rhodes, Kirsty M., Rebecca M. Turner, and Julian PT Higgins. ",
                                  a(em("Predictive distributions were developed for the extent of heterogeneity in meta-analyses of continuous outcome data. "),
                                    href = "https://doi.org/10.1016/j.jclinepi.2014.08.012"),
                                  "Journal of Clinical Epidemiology 68, No. 1 (2015): 52-60. ")),
                        br(),
                        tags$li(p("Rhodes, Kirsty M., Rebecca M. Turner, and Julian PT Higgins. ",
                                  a(em("Empirical evidence about inconsistency among studies in a pair-wise meta-analysis. "),
                                    href = "https://doi.org/10.1002/jrsm.1193"),
                                  "Research Synthesis Methods 7, No. 4 (2016): 346-370. "))

                        )),
             
             tabPanel("About",
                      h3("Acknowledgements"),
                      p("Prof. Julian Higgins conceived the idea for this web app, and it was developed and tested by Luke McGuinness.",
                      "Dr. Rebecca Turner & Dr. Kirsty Rhodes,",
                      "the primary authors of the publications which provided the data used in this app,",
                      "were involved from an early stage and provided useful clarification and guidance.",
                      "Dr. José Lopez-Lopez provided guidance on constructing the distribution plots, while",
                      "Ciara Gardiner and Gary Smith consulted on the design and implementation of the app itself."),
                      p("For all enquires related to this project, please contact ", 
                        a("Luke McGuinness", href = "mailto:luke.mcguinness@bristol.ac.uk"),"."),
                    
                      h3("Resources"),
                      p("This app was built using",
                        a("Shiny", 
                          href = "https://shiny.rstudio.com"),
                        ", an R package that makes it easy to build interactive web apps straight from R. ",
                        "Development was managed using RStudio and Github, and the code for this app can be found ",
                        a("here", 
                          href = "https://github.com/mcguinlu/hetpriorapp"),
                        "."
                        )
                      )

)
)



##############
#   SERVER   #
##############

server <- function(input, output, session) {#####

# Observe input and update options for subsequent boxes
 
  
   
  observeEvent(input$hetstat,{
    v$buttonclick <- 0
    updateSelectInput(session,'datatype',
                      choices = c("", unique(as.character(hetdata$Data.Type[hetdata$Heterogeneity.statistic==input$hetstat]))))
  })
  
  observeEvent(input$datatype,{
    v$buttonclick <- 0
    updateSelectInput(session,'effectmeasure',
                      choices = c("", unique(as.character(hetdata$Effect.Measure[hetdata$Heterogeneity.statistic==input$hetstat & 
                                                                                 hetdata$Data.Type==input$datatype]))))
  })
  
  observeEvent(input$effectmeasure,{
    v$buttonclick <- 0
    updateSelectInput(session,'distributionform',
                      choices = c("", unique(as.character(hetdata$Distribution.form[hetdata$Heterogeneity.statistic==input$hetstat & 
                                                                                   hetdata$Data.Type==input$datatype &
                                                                                   hetdata$Effect.Measure==input$effectmeasure]))))
  })
  
  observeEvent(input$distributionform,{
    v$buttonclick <- 0
    updateSelectInput(session,'interventiontype',
                      choices = c("", unique(as.character(hetdata$Type.of.Intervention[hetdata$Heterogeneity.statistic==input$hetstat & 
                                                                                       hetdata$Data.Type==input$datatype &
                                                                                       hetdata$Effect.Measure==input$effectmeasure &
                                                                                       hetdata$Distribution.form==input$distributionform]))))
  })
  
  observeEvent(input$interventiontype,{
    v$buttonclick <- 0
    updateSelectInput(session,'natureoutcome',
                      choices = c("", unique(as.character(hetdata$Nature.of.Outcome[hetdata$Heterogeneity.statistic==input$hetstat & 
                                                                                      hetdata$Data.Type==input$datatype &
                                                                                      hetdata$Effect.Measure==input$effectmeasure &
                                                                                      hetdata$Distribution.form==input$distributionform &
                                                                                      hetdata$Type.of.Intervention==input$interventiontype]))))
  })
  
  observeEvent(input$natureoutcome,{
    v$buttonclick <- 0
    updateSelectInput(session,'medicalarea',
                      choices = c("", unique(as.character(hetdata$Medical.area[hetdata$Heterogeneity.statistic==input$hetstat & 
                                                                                      hetdata$Data.Type==input$datatype &
                                                                                      hetdata$Effect.Measure==input$effectmeasure &
                                                                                      hetdata$Distribution.form==input$distributionform &
                                                                                      hetdata$Type.of.Intervention==input$interventiontype &
                                                                                      hetdata$Nature.of.Outcome==input$natureoutcome
                                                                                      ]))))
  })  
  
  observeEvent(input$medicalarea,{
    v$buttonclick <- 0
    updateSelectInput(session,'samplesize',
                      choices = c("", unique(as.character(hetdata$Average.Sample.Size[hetdata$Heterogeneity.statistic==input$hetstat & 
                                                                                 hetdata$Data.Type==input$datatype &
                                                                                 hetdata$Effect.Measure==input$effectmeasure &
                                                                                 hetdata$Distribution.form==input$distributionform &
                                                                                 hetdata$Type.of.Intervention==input$interventiontype &
                                                                                 hetdata$Nature.of.Outcome==input$natureoutcome &
                                                                                 hetdata$Medical.area==input$medicalarea
                                                                                ]))))
  })


  
###  RENDER NEW SELECTION BOXES BASED ON INPUT   ### 
  output$datatypeui <-
    renderUI({
      if (is.null(input$hetstat)){}else{
      if (input$hetstat!=""){
        selectizeInput("datatype", label = "Data type:",
                       choices = NULL,
                       options = list(placeholder = 'Choose',
                                      onInitialize = I('function() { this.setValue(""); }')))        
    }}})

  output$effectmeasureui <-
    renderUI({
      if (is.null(input$datatype)){}else{
      if (input$hetstat!="" & input$datatype!=""){
        selectizeInput("effectmeasure", label = "Effect measure:",
                       choices = NULL,
                       options = list(placeholder = 'Choose',
                                      onInitialize = I('function() { this.setValue(""); }')))
    }}})
  
  output$distformui <-
    renderUI({
      if (is.null(input$effectmeasure)){}else{
      if (input$hetstat!="" & input$datatype!="" & input$effectmeasure!=""){
          selectizeInput("distributionform", label = "Distribution form:",
                         choices = NULL,
                         options = list(placeholder = 'Choose',
                                        onInitialize = I('function() { this.setValue(""); }')))
        }}})
  
  output$interventiontypeui <-
    renderUI({
      if (is.null(input$distributionform)){}else{
        if (input$hetstat!="" & input$datatype!="" & input$effectmeasure!="" & input$distributionform!=""){
          selectizeInput("interventiontype", label = "Type of intervention:",
                         choices = NULL,
                         options = list(placeholder = 'Choose',
                                        onInitialize = I('function() { this.setValue(""); }')))
        }}})
  
  output$natureoutcomeui <-
    renderUI({
      if (is.null(input$interventiontype)){}else{
        if (input$hetstat!="" & input$datatype!="" & input$effectmeasure!="" & input$distributionform!="" & input$interventiontype!=""){
          selectizeInput("natureoutcome", label = "Nature of Outcome:",
                         choices = NULL,
                         options = list(placeholder = 'Choose',
                                        onInitialize = I('function() { this.setValue(""); }')))
          }}})
  
  output$actiondo1 <- 
    renderUI({
    if (is.null(input$natureoutcome)){}else{
      if (input$hetstat!="" & 
          input$datatype!="" & 
          input$effectmeasure!="" & 
          input$distributionform!="" & 
          input$interventiontype!="" & 
          input$natureoutcome!="" &
          (length(unique(hetdata$Medical.area[hetdata$Heterogeneity.statistic==input$hetstat & 
                                             hetdata$Data.Type==input$datatype &
                                             hetdata$Effect.Measure==input$effectmeasure &
                                             hetdata$Distribution.form==input$distributionform &
                                             hetdata$Type.of.Intervention==input$interventiontype &
                                             hetdata$Nature.of.Outcome==input$natureoutcome]))==1) &
          (length(unique(hetdata$Average.Sample.Size[hetdata$Heterogeneity.statistic==input$hetstat & 
                                             hetdata$Data.Type==input$datatype &
                                             hetdata$Effect.Measure==input$effectmeasure &
                                             hetdata$Distribution.form==input$distributionform &
                                             hetdata$Type.of.Intervention==input$interventiontype &
                                             hetdata$Nature.of.Outcome==input$natureoutcome]))==1)){
        actionButton("do1", "Submit")
      }}})
    
  output$medicalareaui <-
    renderUI({
      if (is.null(input$natureoutcome)){}else{
        if (input$hetstat!="" &
            input$datatype!="" & 
            input$effectmeasure!="" & 
            input$distributionform!="" & 
            input$interventiontype!="" & 
            input$natureoutcome!="" &
            ((length(unique(hetdata$Medical.area[hetdata$Heterogeneity.statistic==input$hetstat & 
                                                hetdata$Data.Type==input$datatype &
                                                hetdata$Effect.Measure==input$effectmeasure &
                                                hetdata$Distribution.form==input$distributionform &
                                                hetdata$Type.of.Intervention==input$interventiontype &
                                                hetdata$Nature.of.Outcome==input$natureoutcome]))!=1) |
            (length(unique(hetdata$Average.Sample.Size[hetdata$Heterogeneity.statistic==input$hetstat & 
                                                       hetdata$Data.Type==input$datatype &
                                                       hetdata$Effect.Measure==input$effectmeasure &
                                                       hetdata$Distribution.form==input$distributionform &
                                                       hetdata$Type.of.Intervention==input$interventiontype &
                                                       hetdata$Nature.of.Outcome==input$natureoutcome]))!=1))){
          selectizeInput("medicalarea", label = "Medical area:",
                         choices = NULL,
                         options = list(placeholder = 'Choose',
                                        onInitialize = I('function() { this.setValue(""); }')))
          }}})
  
  output$samplesizeui <-
    renderUI({
      if (is.null(input$natureoutcome)){}else{
        if (input$hetstat!="" &
            input$datatype!="" &
            input$effectmeasure!="" &
            input$distributionform!="" &
            input$interventiontype!="" &
            input$natureoutcome!="" &
            ((length(unique(hetdata$Medical.area[hetdata$Heterogeneity.statistic==input$hetstat & 
                                                hetdata$Data.Type==input$datatype &
                                                hetdata$Effect.Measure==input$effectmeasure &
                                                hetdata$Distribution.form==input$distributionform &
                                                hetdata$Type.of.Intervention==input$interventiontype &
                                                hetdata$Nature.of.Outcome==input$natureoutcome]))!=1) |
            (length(unique(hetdata$Average.Sample.Size[hetdata$Heterogeneity.statistic==input$hetstat & 
                                                       hetdata$Data.Type==input$datatype &
                                                       hetdata$Effect.Measure==input$effectmeasure &
                                                       hetdata$Distribution.form==input$distributionform &
                                                       hetdata$Type.of.Intervention==input$interventiontype &
                                                       hetdata$Nature.of.Outcome==input$natureoutcome]))!=1))){
          selectizeInput("samplesize", label = "Average sample size:",
                         choices = NULL,
                         options = list(placeholder = 'Choose',
                                        onInitialize = I('function() { this.setValue(""); }')))
        }}})
  
  output$actiondo2 <-
    renderUI({
      if (is.null(input$medicalarea) |is.null(input$samplesize)){}else{
        if (input$hetstat!="" &
            input$datatype!="" & 
            input$effectmeasure!="" & 
            input$distributionform!="" & 
            input$interventiontype!="" & 
            input$natureoutcome!="" & 
            input$medicalarea!="" 
            & input$samplesize!=""
            ){
          actionButton("do2", "Submit")
          }}})

###  TABLE GENERATION FOR LOOK-UP   ###
# Generate table based on input
v <- reactiveValues(data = NULL, buttonclick = 0)
  
observeEvent(input$do1, {
        v$data <- subset(hetdata,
                       Heterogeneity.statistic == input$hetstat &
                       Data.Type == input$datatype &
                       Effect.Measure==input$effectmeasure &
                       Distribution.form==input$distributionform &
                       Type.of.Intervention==input$interventiontype &
                       Nature.of.Outcome==input$natureoutcome
                       
        )
        v$buttonclick <- 1
        
        v$mean <- as.numeric(v$data[1,9])
        v$sd <- as.numeric(v$data[1,10])
        v$median <- v$data[1,11]
        v$lowquant <- v$data[1,12]
        v$highquant <- v$data[1,13]
        v$notes1 <- v$data[1,14]
        v$reference <- v$data [1,15]
        v$datlognormal <- data.frame(hetprior1 = rlnorm(10000, mean = v$mean, sd = v$sd))
        v$datinvgamma <- data.frame(hetprior2 = rinvgamma(10000, v$mean, v$sd))
})

observeEvent(input$do2, {
        v$data <- subset(hetdata,
                   Heterogeneity.statistic == input$hetstat &
                   Data.Type == input$datatype &
                   Effect.Measure==input$effectmeasure &
                   Distribution.form==input$distributionform &
                   Type.of.Intervention==input$interventiontype &
                   Nature.of.Outcome==input$natureoutcome &
                   Medical.area==input$medicalarea &
                   Average.Sample.Size==input$samplesize
                   )
        v$buttonclick <- 1
        v$mean <- as.numeric(v$data[1,9])
        v$sd <- as.numeric(v$data[1,10])
        v$median <- v$data[1,11]
        v$lowquant <- v$data[1,12]
        v$highquant <- v$data[1,13]
        v$notes1 <- v$data[1,14]
        v$reference <- v$data [1,15]
        v$datlognormal <- data.frame(hetprior1 = rlnorm(10000, mean = v$mean, sd = v$sd))
        v$datinvgamma <- data.frame(hetprior2 = rinvgamma(10000, v$mean, v$sd))
        
        })

observeEvent(input$clear, {
  v$buttonclick <- 0
  v$data <- NULL
  updateTextInput(session, "hetstat", value = "")
  v$mean <- NULL
  v$sd <- NULL
  v$median <- NULL
  v$lowquant <- NULL
  v$highquant <- NULL
  v$notes1 <- NULL
  v$reference <- NULL
})



#Render table
output$table_lookup <- DT::renderDataTable(DT::datatable({
  if (is.null(v$data)) return(hetdata)
  v$data
  }))

#For conditional panel
output$buttonclick <- renderText(v$buttonclick)
outputOptions(output, "buttonclick", suspendWhenHidden = FALSE)

#Render Outputs
output$mean <- renderText(v$mean)
output$sd <- renderText(v$sd)
output$median <- renderText(as.character(v$median))
output$reference <- renderText(as.character(v$reference))
output$lowquant <- renderText(as.character(v$lowquant))
output$highquant <- renderText(as.character(v$highquant))
output$notes1 <- renderText(as.character(v$notes1))


output$meanui <- 
  renderUI({
    if (input$distributionform == "Inverse gamma"){
      br(p(em("Shape:"), textOutput("mean")))
    }else{
      br(p(em("Mean:"), textOutput("mean")))
    }})

output$sdui <- 
  renderUI({
    if (input$distributionform == "Inverse gamma"){
      br(p(em("Scale:"), textOutput("sd")))
    }else{
      br(p(em("Standard deviation:"), textOutput("sd")))
    }})

 output$medianui <- 
   renderUI({
     if (is.null("median")){ }else{
       if (v$median != "None"){
          br(p(em("Median:"), textOutput("median")))
          
      }}})
 
 output$lowquantui <- 
   renderUI({
     if (is.null("lowquant")){ }else{
       if (v$lowquant != "None"){
         br(p(em("2.5% Quantile:"), textOutput("lowquant")))
         
       }}})
 
 output$highquantui <- 
   renderUI({
     if (is.null("highquant")){ }else{
       if (v$highquant != "None"){
         br(p(em("97.5% Quantile:"), textOutput("highquant")))
         
       }}}) 
 
 output$notes1ui <- 
   renderUI({
     if (is.null("notes1")){ }else{
       if (v$notes1 != "None "){
         br(p(em("Notes:"), textOutput("notes1")))
         
       }}})
 
 
output$plot <- renderPlot({
if(input$distributionform == "Log normal"){ 
    ggplot(v$datlognormal, aes(x=hetprior1)) +
    geom_density(color = "black", fill = "lightsteelblue3") +
    xlim(0, 1) +
    labs(x = input$hetstat, y = "Density")
}else{
    if(input$distributionform == "Inverse gamma"){ 
        ggplot(v$datinvgamma, aes(x=hetprior2)) +
        geom_density(color = "black", fill = "lightsteelblue3") +
        xlim(0, 1)+
        labs(x = input$hetstat, y = "Density")}}
}) 

 
output$plotui <- renderUI({
   br(p(em("Plot:", plotOutput("plot"))))
   })



 
 
###   TABLE GENERATION FOR EXPLORE   ###
output$table_explore <- DT::renderDataTable(DT::datatable({
  
  data_e <- hetdata
  if (input$hetstat_e != "All") {
    data_e <- hetdata[hetdata[,1] == input$hetstat_e,]
  }
  
  if (input$datatype_e != "All") {
    data_e <- hetdata[hetdata[,2] == input$datatype_e,]
  }
  
  if (input$effectmeasure_e != "All") {
    data_e <- hetdata[hetdata[,3] == input$effectmeasure_e,]
  }
  
  if (input$distributionform_e != "All") {
    data_e <- hetdata[hetdata[,4] == input$distributionform_e,]
  }
  
  if (input$interventiontype_e != "All") {
    data_e <- hetdata[hetdata[,5] == input$interventiontype_e,]
  }
  
  if (input$natureoutcome_e != "All") {
    data_e <- hetdata[hetdata[,6] == input$natureoutcome_e,]
  }
  
  data_e
  
  },extensions = 'Responsive', 
  options = list(
    columnDefs = list(list(className = 'dt-center', targets = '_all')),
    pageLength = 20,
    lengthMenu = c(5, 10, 20, 50, 100),
    columns = list(
      list(title = 'ID'),
      list(title = 'Heterogeneity statistic'),
      list(title = 'Data type'),
      list(title = 'Effect measure'),
      list(title = 'Distribution form'),
      list(title = 'Intervention type'),
      list(title = 'Nature of outcome'),
      list(title = 'Medical area'),
      list(title = 'Average sample size'),
      list(title = 'Mean/Shape'),
      list(title = 'Standard deviation/Scale'),
      list(title = 'Median'),
      list(title = '2.5% Quantile'),
      list(title = '97.5% Quantile'),
      list(title = 'Notes'),
      list(title = 'Reference'))
    )))



}


#######################
#   RUN APPLICATION   #
#######################
shinyApp(ui = ui, server = server)