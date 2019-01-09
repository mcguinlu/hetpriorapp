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
    titlePanel(h1("Heterogeneity Prior for Bayesian Meta-Analysis", align = "center"),windowTitle = "hetprior"),
    navbarPage("",
               theme = shinythemes::shinytheme("flatly"),
               # tabPanel("Home",
               #        sidebarLayout(
               #            sidebarPanel(
               #              br(),
               #              h2("Installation"),
               #              p(em("hetprior")," is available on github, so you can install it using the commands below from your R console:"),
               #              code('install.packages("hetprior")'),
                            # br(),
                            # br(),
                            # br(),
                            # br(),
                            # br(),
               #              em("hetprior"), 
               #              " is a product of MESI Labs"
               #            ),
               #        
               #            mainPanel(
               #              h1("Introducing", em("hetprior")),
               #              p(em("hetprior")," is a new package from MESI Labs which makes it easy to look-up informative priors for your Bayesian meta-analysis."),
               #              br(),
               #              p("For an introduction more examples, visit the ",
               #                a("Github account", 
               #                  href = "https://github.com/mcguinlu")),
               #              br(),
               #              h2("Features"),
               #              p("- "),
               #              p("- ")
               #                    )
               #                    )
               #        ),
             
             tabPanel("Look-up",
                      sidebarLayout(
                        fluid = TRUE,
                        
                                    sidebarPanel(
                                          width = 3,
                                          
                                          p("Please complete the boxes below, or explore the complete dataset on the right."),
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
                                    
                                    conditionalPanel(
                                      "output.buttonclick == '1'",

                                          # DT::dataTableOutput("table_lookup"),
                                          br(),
                                          br(),
                                          
                                          p(em("Mean:"), textOutput("mean")), 
                                          br(),
                                          p(em("Standard deviation:"), textOutput("sd")),
                                          br(),
                                          p(em("Reference:"), textOutput("reference"))
                                          # will need to move this to the server and replace with a uiOuput
                                          # because we need to seperate Mean/SD vs Shape/Rate based on input$distributionform
                                          
                                    
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
                                           "Effect Measure:",
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
                                           "Outcome Nature:",
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
                      
                      
                      fluidRow(column(9,offset = 2,DT::dataTableOutput("table_explore"))
                               ) ),
             
             tabPanel("References"
             
                      ),
             
             tabPanel("Acknowledgements"
             
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
        v$sd <- v$data[1,10]
        v$mean <- v$data[1,9]
        v$reference <- v$data [1,17]
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
        v$reference <- v$data [1,17]
        })

observeEvent(input$clear, {
  v$buttonclick <- 0
  v$data <- NULL
  updateTextInput(session, "hetstat", value = "")
  v$mean <- NULL
  v$sd <- NULL
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
output$reference <- renderText(as.character(v$reference))


###   TABLE GENERATION FOR EXPLORE   ###
output$table_explore <- DT::renderDataTable(DT::datatable({
  
  data_e <- hetdata
  if (input$hetstat_e != "All") {
    data_e <- hetdata[hetdata$Heterogeneity.statistic == input$hetstat_e,]
  }
  
  if (input$datatype_e != "All") {
    data_e <- hetdata[hetdata$Data.Type == input$datatype_e,]
  }
  
  if (input$effectmeasure_e != "All") {
    data_e <- hetdata[hetdata$Effect.Measure == input$effectmeasure_e,]
  }
  
  if (input$distributionform_e != "All") {
    data_e <- hetdata[hetdata$Distribution.form == input$distributionform_e,]
  }
  
  if (input$interventiontype_e != "All") {
    data_e <- hetdata[hetdata$Type.of.Intervention == input$interventiontype_e,]
  }
  
  if (input$natureoutcome_e != "All") {
    data_e <- hetdata[hetdata$Nature.of.Outcome == input$natureoutcome_e,]
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
  },extensions = 'Responsive'))
}


#######################
#   RUN APPLICATION   #
#######################
shinyApp(ui = ui, server = server)