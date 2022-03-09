# Feature Visualization Script
# Will Paces, March 2022

# Specify required packages
used.libs <- c('RSQLite', 'shiny', 'htmlwidgets', 'shinythemes', 'ggplot2', 'utils')

# Define package-loader function
load.used <- function(x){
  req<-unlist(lapply(x,require,character.only=TRUE))
  need<-x[req==FALSE]

  n<-length(need)
  if(n>0){
    libsmsg<-if(n>2) paste(paste(need[1:(n-1)],collapse=", "),",",sep="") else need[1]
    print(libsmsg)
    if(n>1){
      libsmsg<-paste(libsmsg," and ", need[n],sep="")
    }
    libsmsg<-paste("The following packages could not be found: ",libsmsg,"\n\r\n\rInstall missing packages?",collapse="")
    if(winDialog(type = c("yesno"), libsmsg)=="YES"){
      install.packages(need)
      lapply(need,require,character.only=TRUE)
    }
  }

  lapply(x,require,character.only=TRUE)
}

# Call load function on used packages vector
load.used(used.libs)



# Server process ----------
# clear environment
rm(list = ls())

server <- function(input, output, session){
  
  fyl <- reactiveValues()
  list.tab <- reactiveValues()
  data.tabs <- reactiveValues()
  data.tabs$tables <- ''
  feat.tabs <- reactiveValues()
  taboptions <- reactiveValues()
  sepops <- reactiveValues()
  sepops$options <- ''
  taboptions$shortname <- ''
  to.save <- reactiveValues()
  
  # Select folder and detect/display constituent datasets
  observeEvent(input$loadfold, {
    fyl$expfol <- choose.dir()
    setwd(fyl$expfol)
    all.files <- list.files(fyl$expfol)
    data.files <- subset(all.files, grepl('.sqlite$', all.files))
    list.tab$data.files <- data.files
  })
  
  output$tablechk3 <- renderUI({
    checkboxGroupInput('chkbox3', label = h3('Feature data files within folder:'),
                       choices = list.tab$data.files)
  })
  
  # "Select All" option
  observe({
    updateCheckboxGroupInput(session, 'chkbox3', choices = list.tab$data.files,
                             selected = if(input$alltog) list.tab$data.files)
  })
  
  # Identify features in selected tables and print to UI
  observeEvent(input$tabbut, {
    data.filename <- input$chkbox3
    feat.term <- "QuantifyCellMorphology"
    
    heads <- NULL
    msng <- NULL
    
    for(i in 1:length(data.filename)){
      db.con <- dbConnect(SQLite(), dbname = data.filename[i])
      tables <- dbListTables(db.con)
      t.name <- subset(tables, grepl(feat.term, tables))
      
      dbDisconnect(db.con)
      
      if(length(t.name) > 0){
        db.con2 <- dbConnect(SQLite(), paste0(t.name, ".sqlite"))
        sq.cmd <- paste('select * from ', t.name, ' limit 1', sep = '')
        res <-try(dbGetQuery(db.con2, sq.cmd), silent = T)
        dbDisconnect(db.con2)
        
        if(!is.character(res)){
          add.header <- names(res)
          heads <- c(heads, add.header)
        }
      }else{
        msng <- c(msng, data.filename[i])
      }
    }
    
    heads <- unique(heads)
    
    data.tabs$tables <- heads
    
    if(is.null(heads)){
      prgtxt <- paste('Missing Entries from: ', paste(msng, collapse = ', ', sep = ''), sep = '')
      updateTextInput(session, 'exstat', value = prgtxt)
    }

  })
  
  output$pltFeat <- renderUI({
    checkboxGroupInput('plotfeats', label = h3('Measured Feature'), choices = data.tabs$tables)
  })

  # Uncomment the following section to kill session when window is closed - otherwise, keep running
  
    # session$onSessionEnded(function(){
    #  stopApp()
    #  q('no')
    # })
  
  # Plot features
  observeEvent(input$pltrf, {
    # Count number of times button is pressed
    runcount2 <- input$pltrf
    
    # Generate status message
    prgtxt <- paste("Generating plot ", runcount2, '...', sep = '')
    updateTextInput(session, 'exstat2', value = prgtxt)
    
    # Prepare inputs for loading data
    ft.head <- input$plotfeats
    data.filename <- input$chkbox3
    plot.mode <- input$radbut2
    search.name <- "QuantifyCellMorphology"
    qry.names <- paste(ft.head, collapse = ', ')

    obj.data <- data.frame('source' = NULL, 'value' = NULL)
    
    for(i in 1:length(data.filename)){
      # Connect to SQLite database and read table names
      db.con <- dbConnect(SQLite(), dbname = data.filename[i])
      tables <- dbListTables(db.con)
      
      # Select specified table
      morph.tab <- subset(tables, grepl(search.name, tables))
      
      # Prepare command to extract selected 
      sq.cmd <- paste('select ', qry.names, ' from ', morph.tab, sep = '')
      res <-try(dbGetQuery(db.con, sq.cmd), silent = T)
      dbDisconnect(db.con)
      
      if(!is.character(res)){
        for(j in 1:length(ft.head)){
          data.tag <- rep(ft.head[j], times = nrow(res))
          add.data <- data.frame('source' = data.tag, 'value' = res[, j])
          obj.data <- rbind.data.frame(obj.data, add.data)
        }
      }
    }
    
    obj.data$source <- as.factor(obj.data$source)
    obj.data <- na.omit(obj.data)
    
    pltxt <- gsub('_', ' ', qry.names)
    
    if(plot.mode == 'Histogram'){
      # Determine bin width
      bw <- (2 * IQR(obj.data$value))/(length(obj.data$value) ^(1/3))
      
      # Generate histogram
      g <- ggplot(obj.data, aes(value, fill = source)) +
        geom_histogram(binwidth = bw, alpha = 0.65) +
        labs(x = paste(pltxt, ' Value', sep = ''), y = 'Count', fill = 'Feature') +
        ggtitle(paste(pltxt, ' Distribution(s)', sep = ''))
      
    }else if(plot.mode == 'CDF'){
      # Generate CDF plot
      g <- ggplot(obj.data, aes(x = value, colour = source)) +
        stat_ecdf(geom = 'step') +
        scale_y_continuous(labels = function(p) paste0(p*100, '%')) +
        ggtitle(paste(pltxt, ' Distribution(s)', sep = '')) +
        labs(x = paste(pltxt, ' Values', sep = ''), y = 'Cumulative Percentage', colour = 'Feature')
      
    }else if(plot.mode == 'Box Plot'){
      # Generate box plot
      g <- ggplot(obj.data, aes(x = source, y = value)) +
        geom_boxplot(outlier.shape = NA) +
        ggtitle(paste(pltxt, ' Distribution(s)', sep = '')) +
        labs(x = "Feature", y = 'Value')
    }
    
    output$plt <- renderPlot({
      plot(g)
    })
    
    to.save$plot <- g
  })
  
  observeEvent(input$pltxpt, {
    expt.name <- as.character(input$pltname)
    fin.plot <- to.save$plot
    
    if(is.null(fin.plot)){
      prgtxt <- 'No plot available. Create plots using "Generate Plot" button.'
      updateTextInput(session, 'exstat2', value = prgtxt)
    }else{
      illegal.chars <- c('\\?',  '/', '\\\\', ':',  '\"', '<',  '>',  '\\|')
      ill.name <- NULL
      for(y in 1:length(illegal.chars)){
        ill.name[y] <- grepl(illegal.chars[y], expt.name)
      }
      if(any(ill.name)){
        prgtxt <- paste('Illegal output name. Remove symbols', paste(illegal.chars[ill.name], collapse = ' '), sep = ' ')
        updateTextInput(session, 'exstat2', value = prgtxt)

      }else{
        tiff(paste(expt.name, '.tiff', sep = ''), width = 6 ,height = 5, units = 'in', res = 300)
        plot(fin.plot)
        dev.off()
        
        prgtxt <- 'Plot saved, see folder for result'
        updateTextInput(session, 'exstat2', value = prgtxt)
      }
    }
  })
  
  
}

ui <- navbarPage('Feature Visualization Tool 1.0', theme = shinytheme('flatly'),
                 # Set up first tab for data option selections
                 tabPanel('Select Feature Data from Slide Analysis',
                          fluidRow(column(4,
                                          actionButton('loadfold', label = 'Select folder to examine'),
                                          uiOutput('tablechk3'),
                                          checkboxInput('alltog', 'Select All')),
                                   column(4,
                                          actionButton('tabbut', label = 'List Selected Table Headers'),
                                          uiOutput('pltFeat')),
                                   column(4,
                                          fluidRow(radioButtons('radbut2', label = h3('Plot Type'),
                                                                choices = c('CDF', 'Histogram', "Box Plot"))),
                                          fluidRow(actionButton('pltrf', label = 'Generate Plot')),
                                          fluidRow(textInput('pltname', label = 'Filename for Plot Export')),
                                          fluidRow(textInput('exstat2', label = 'Status:', value = 'Waiting for input...'))
                                          ))),
                 # Set up second tab for plot viewing
                 tabPanel('Generated Plots',
                          fluidRow(column(9,
                                          plotOutput('plt')),
                                   column(3,
                                          actionButton('pltxpt', label = 'Export Plot')))
                          ))


shinyApp(ui = ui, server = server)

