#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
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
                              em("Acknowledgements"), "
                              section.")
                          ),
                       
                            mainPanel(
                            h2("Introducing", em("hetprior")),
                            h3("Background"),
                            p(em("hetprior")," is a new suite of tools from the BARR group at the University of Bristol",
                            "which makes it easy to look-up an informative heterogeneity prior to inform your own Bayesian meta-analysis."),
                            # br(),
                            # p("For an introduction more examples, visit the ",
                            #   a("Github account",
                            #     href = "https://github.com/mcguinlu")),
                            br(),
                            h3("Features"),
                            p("- ", em("Coming soon"))
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
                                      "output.buttonclick == '1'",
                                      width = 9, offset =3,
                                          br(p(em("Mean:"), textOutput("mean"))), 
                                          br(p(em("Standard deviation:"), textOutput("sd"))),
                                          uiOutput("medianui"),
                                          uiOutput("lowquantui"),
                                          uiOutput("highquantui"),
                                          uiOutput("notes1ui"),
                                          uiOutput("notes2ui"),
                                          br(p(em("Reference:"), textOutput("reference"))),
                                          # will need to move this to the server and replace with a uiOuput
                                          # because we need to seperate Mean/SD vs Shape/Rate based on input$distributionform
                                          br(),
                                          p(strong(em("Coming January 2019: Graphing of prior distributions . . . ")))
                                          
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
                        # column(2,
                        #        selectInput("medicalarea_e",
                        #                    "Medical area:",
                        #                    c("All",
                        #                      unique(as.character(hetdata$Medical.area))))),
                        # column(2,
                        #        selectInput("samplesize_e",
                        #                    "Average sample size:",
                        #                    c("All",
                        #                      unique(as.character(hetdata$Average.Sample.Size)))))
                        
                        ),
                      
                      fluidRow(column(10, offset = 1,DT::dataTableOutput("table_explore"))
                               ) ),
             
             
             tabPanel("References",
                      h3("References"),
                      p("The data for this project came from a series of 5 papers. These are detailed below:"),
                      br(),
                      tags$ul(
                        tags$li(p("Turner, Rebecca M., Jonathan Davey, Mike J. Clarke, Simon G. Thompson, and Julian PT Higgins.",
                                  em("Predicting the extent of heterogeneity in meta-analysis, using empirical data from the Cochrane Database of Systematic Reviews. "),
                                  "International Journal of Epidemiology 41, No. 3 (2012): 818-827. ",
                                  a("Link",
                                       href = "https://doi.org/10.1093/ije/dys041"))),
                        br(),
                        tags$li(p("Turner, Rebecca M., Dan Jackson, Yinghui Wei, Simon G. Thompson, and Julian PT Higgins.",
                                  em("Predictive distributions for between-study heterogeneity and simple methods for their application in Bayesian meta-analysis. "),
                                  "Statistics in Medicine 34, No. 6 (2015): 984-998. ",
                                  a("Link",
                                    href = "https://doi.org/10.1002/sim.6381"))),
                        br(),
                        tags$li(p("Rhodes, Kirsty M., Rebecca M. Turner, Ian R. White, Dan Jackson, David J. Spiegelhalter, and Julian PT Higgins.",
                                  em("Implementing informative priors for heterogeneity in meta?analysis using meta-regression and pseudo data. "),
                                  "Statistics in Medicine 35, No. 29 (2016): 5495-5511. ",
                                  a("Link",
                                    href = "https://doi.org/10.1002/sim.7090"))),
                        br(),
                        tags$li(p("Rhodes, Kirsty M., Rebecca M. Turner, and Julian PT Higgins. ",
                                  em("Predictive distributions were developed for the extent of heterogeneity in meta-analyses of continuous outcome data. "),
                                  "Journal of Clinical Epidemiology 68, no. 1 (2015): 52-60. ",
                                  a("Link",
                                    href = "https://doi.org/10.1016/j.jclinepi.2014.08.012"))),
                        br(),
                        tags$li(p("Rhodes, Kirsty M., Rebecca M. Turner, and Julian PT Higgins. ",
                                  em("Empirical evidence about inconsistency among studies in a pair-wise meta-analysis. "),
                                  "Research Synthesis Methods 7, No. 4 (2016): 346-370. ",
                                  a("Link",
                                    href = "https://doi.org/10.1002/jrsm.1193")))

                        )),
             
             tabPanel("About",
                      h3("Acknowledgements"),
                      p(strong(em("Coming soon. . ."))),
                      
                      h3("Development"),
                      p(strong(em("Coming soon. . ."))),
                      
                      h3("Team"),
                      p(strong(em("Coming soon. . ."))),
                      
                      h3("Resources"),
                      p("This app was built using",
                        a("Shiny", 
                          href = "https://shiny.rstudio.com"),
                        ", an R package that makes it easy to build interactive web apps straight from R. ",
                        "Development was managed using RStudio and Github, and the code for this app can be found ",
                        a("here", 
                          href = ""),
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
    updateSelectInput(session,'datatype',
                      choices = c("", unique(as.character(hetdata$Data.Type[hetdata$Heterogeneity.statistic==input$hetstat]))))
  })
  
  observeEvent(input$datatype,{
    updateSelectInput(session,'effectmeasure',
                      choices = c("", unique(as.character(hetdata$Effect.Measure[hetdata$Heterogeneity.statistic==input$hetstat & 
                                                                                 hetdata$Data.Type==input$datatype]))))
  })
  
  observeEvent(input$effectmeasure,{
    updateSelectInput(session,'distributionform',
                      choices = c("", unique(as.character(hetdata$Distribution.form[hetdata$Heterogeneity.statistic==input$hetstat & 
                                                                                   hetdata$Data.Type==input$datatype &
                                                                                   hetdata$Effect.Measure==input$effectmeasure]))))
  })
  
  observeEvent(input$distributionform,{
    updateSelectInput(session,'interventiontype',
                      choices = c("", unique(as.character(hetdata$Type.of.Intervention[hetdata$Heterogeneity.statistic==input$hetstat & 
                                                                                       hetdata$Data.Type==input$datatype &
                                                                                       hetdata$Effect.Measure==input$effectmeasure &
                                                                                       hetdata$Distribution.form==input$distributionform]))))
  })
  
  observeEvent(input$interventiontype,{
    updateSelectInput(session,'natureoutcome',
                      choices = c("", unique(as.character(hetdata$Nature.of.Outcome[hetdata$Heterogeneity.statistic==input$hetstat & 
                                                                                      hetdata$Data.Type==input$datatype &
                                                                                      hetdata$Effect.Measure==input$effectmeasure &
                                                                                      hetdata$Distribution.form==input$distributionform &
                                                                                      hetdata$Type.of.Intervention==input$interventiontype]))))
  })
  
  observeEvent(input$natureoutcome,{
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
        
        v$mean <- v$data[1,9]
        v$sd <- v$data[1,10]
        v$median <- v$data[1,11]
        v$lowquant <- v$data[1,12]
        v$highquant <- v$data[1,13]
        v$notes1 <- v$data[1,14]
        v$notes2 <- v$data[1,15]
        
        v$reference <- v$data [1,16]
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
        v$mean <- v$data[1,9]
        v$sd <- v$data[1,10]
        v$median <- v$data[1,11]
        v$lowquant <- v$data[1,12]
        v$highquant <- v$data[1,13]
        v$notes1 <- v$data[1,14]
        v$notes2 <- v$data[1,15]
        v$reference <- v$data [1,16]
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
  v$notes2 <- NULL
  
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
output$notes2 <- renderText(as.character(v$notes2))

 output$medianui <- 
   renderUI({
     if (is.null("median")){}else{
       if (v$median != "None"){
          br(p(em("Median:"), textOutput("median")))
          
      }}})
 
 output$lowquantui <- 
   renderUI({
     if (is.null("lowquant")){}else{
       if (v$lowquant != "None"){
         br(p(em("2.5% Quantile:"), textOutput("lowquant")))
         
       }}})
 
 output$highquantui <- 
   renderUI({
     if (is.null("highquant")){}else{
       if (v$highquant != "None"){
         br(p(em("97.5% Quantile:"), textOutput("highquant")))
         
       }}}) 
 
 output$notes1ui <- 
   renderUI({
     if (is.null("notes1")){}else{
       if (v$notes1 != "None"){
         br(p(em("Notes:"), textOutput("notes1")))
         
       }}})
 
 output$notes2ui <- 
   renderUI({
     if (is.null("notes2")){}else{
       if (v$notes2 != "None"){
         br(p(em("Further notes:"), textOutput("notes2")))
         
       }}})

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
  
  # if (input$medicalarea_e != "All") {
  #   data_e <- hetdata[hetdata$Medical.area == input$medicalarea_e,]
  # }
  # 
  # if (input$samplesize_e != "All") {
  #   data_e <- hetdata[hetdata$Distribution.form == input$samplesize_e,]
  # }
  data_e
  
  #data_output <- data_e[,1:8], 
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
      list(title = 'Further notes'),
      list(title = 'Reference'))
    )))
}


#######################
#   RUN APPLICATION   #
#######################
shinyApp(ui = ui, server = server)