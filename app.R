 #### nf-core/bacass app ####
 
 # a shiny frontend for the nf-core/bacass pipeline
 # https://github.com/nf-core/bacass.git
 libs <- c("shiny", "shinyFiles", "shinyjs", "shinyalert", "shinypop",
           "processx", "stringr", "digest", "dplyr", "pingr", "yaml")
 lapply(libs, library, character.only = TRUE)
 
 # define reactive to track user counts
 users <- reactiveValues(count = 0)
 
 source("ncct_modal.R", local = FALSE)$value # don't share across sessions, who knows what could happen!
 source("ncct_make_yaml.R")
 
 
 #### ui ####
 ui <- function(x) {
   # I like to mix up R, JS and CSS
   navbarPage(title = tags$button("nf-core/bacass",
                               id = "bacassButton", # events can be listened to as input$cazytableButton in server
                               class = "action-button", #shiny needs this class def to work
                               title = "If you want to start over, just reload the page.",
                               onMouseOver = "this.style.color='orange'", # old school js
                               onMouseOut = "this.style.color='green'",
                               style = "color: green; font-weight: bold; border: none; background-color: inherit;"),
             
              windowTitle = "nf-core/bacass", 
              collapsible = TRUE,
    
    tabPanel("nf-core/bacass output",
            # attempts to use external progress bar
            includeCSS("css/custom.css"),
            #useShinyFeedback(),
            useShinyjs(),
            useShinyalert(), 
            use_notiflix_notify(position = "left-bottom", width = "380px"),
            
            # snackbars begin
            # snackbarWarning(id = "tower_snackbar", 
            #                 message = "Is TOWER_ACCESS_TOKEN available in Sys.getenv() ?"),
            # snackbarSuccess("fastp_trimmed", 
            #                 message = "Default fastp parameters will be used"),
            # snackbars end
            
            #shiny::uiOutput("mqc_report_button", inline = TRUE),
            
            shiny::div(id = "commands_pannel",
              shinyFilesButton(id = "csv_file", 
                             label = "Select bacass .csv/.tsv file", 
                             multiple = FALSE,
                             buttonType = "default", 
                              class = NULL,
                             style = "color: green; font-weight: bold;", 
                               title = "Please select a .csv/.tsv file.", 
                             icon = icon("folder-open")),
              actionButton("run", "Run nf-core/bacass pipeline", 
                         style = "color: green; font-weight: bold;", 
                         onMouseOver = "this.style.color = 'orange' ", 
                         onMouseOut = "this.style.color = 'green' ", 
                         icon = icon("play")),
              actionButton("reset", "Reset", 
                         style = "color: green; font-weight: bold;",
                         onMouseOver = "this.style.color = 'orange' ",
                         onMouseOut = "this.style.color = 'green' ", 
                         icon = icon("redo")),
              # actionButton("stopButton", "STOP run", 
              #              style = "color: green; font-weight: bold;", 
              #              onMouseOver = "this.style.color = 'red' ", 
              #              onMouseOut = "this.style.color = 'green' ", 
              #              icon = icon("stop-circle-o")),
            
            actionButton("more", "More options", 
                         icon = icon("cog"),
                         class = "rightAlign"),
            actionButton("landing_page", "Go to home page", 
                         icon = icon("home"),
                         class = "rightAlign", 
                         onclick ="window.open('http://google.com', '_blank')"),
            
            tags$div(id = "optional_inputs",
              absolutePanel(top = 140, right = 20,
                          selectizeInput("nxf_profile",
                                         label = "Select nextflow profile",
                                         choices = c("docker", "conda", "test"),
                                         selected = "docker",
                                         multiple = FALSE),
                          tags$hr(),
                          selectizeInput("annotation_tool", 
                                         label = "Annotation tool", 
                                         choices = c("prokka", "dfast"), 
                                         selected = "dfast", 
                                         multiple = FALSE),
                          tags$hr(),
                          actionButton("ncct", "Enter NCCT project info"),
                          tags$hr(),
                          checkboxInput("skip_kraken", "Skip kraken2", value = FALSE),
                          tags$hr(),
                          checkboxInput("tower", "Use Nextflow Tower to monitor run", value = FALSE)
                          #tags$hr(),

              )
            )
  
          ),
            
            verbatimTextOutput("stdout")
            
    ),
    tabPanel("Help", 
             includeMarkdown("help.md"))
    
  )
 }
 #### server ####
  server <- function(input, output, session) {
    options(shiny.launch.browser = TRUE, shiny.error=recover)
    
    #----
    # Initialization of reactive for optional params for nxf, 
    # they are set later in renderPrint to params for nxf; others may be implemented here
    # set TOWER_ACCESS_TOKEN in ~/.Renviron
    optional_params <- reactiveValues(tower = "", mqc = "", skip_kraken = "", profile = "")
    
    # update user counts at each server call
    isolate({
      users$count <- users$count + 1
    })
    
    # observe changes in users$count and write to log, observers use eager eval
    observe({
      writeLines(as.character(users$count), con = "userlog")
    })
    
    # observer for optional inputs
    hide("optional_inputs")
    observeEvent(input$more, {
      shinyjs::toggle("optional_inputs")
    })
    
    #----
    # strategy for ncct modal and multiqc config file handling:
    # if input$ncct_ok is clicked, the modal inputs are fed into the ncct_make_yaml() function, which generates
    # a multiqc_config.yml file and saves it using tempfile()
    # initially, the reactive value mqc_config$rv is set to "", if input$ncct_ok then it is set to
    # c("--multiqc_config", mqc_config_temp) and this reactive is given as param to the nxf pipeline
    
    #observer to generate ncct modal
    observeEvent(input$ncct, {
      if(pingr::is_online()) {
        ncct_modal_entries <- yaml::yaml.load_file("https://gist.githubusercontent.com/angelovangel/d079296b184eba5b124c1d434276fa28/raw/ncct_modal_entries")
        showModal( ncct_modal(ncct_modal_entries) )
      } else {
        shinyalert("No internet!",
                   text = "This feature requires internet connection",
                   type = "warning")
      }

    })
    
    # generate yml file in case OK of modal was pressed
    # the yml file is generated in the app exec env, using temp()
    observeEvent(input$ncct_ok, {
      mqc_config_temp <- tempfile()
      optional_params$mqc <- c("--multiqc_config", mqc_config_temp)
      ncct_make_yaml(customer = input$customer,
                     project_id = input$project_id,
                     ncct_contact = input$ncct_contact,
                     project_type = input$project_type,
                     lib_prep = input$lib_prep,
                     indexing = input$indexing,
                     seq_setup = input$seq_setup,
                     ymlfile = mqc_config_temp)
      nx_notify_success("Project info saved!")
      removeModal()
    })
    
    
    # generate random hash for multiqc report temp file name
    mqc_hash <- sprintf("%s_%s.html", as.integer(Sys.time()), digest::digest(runif(1)) )
    
    # dir choose management --------------------------------------
    volumes <- c(Home = fs::path_home(), getVolumes()() )
    
    shinyFileChoose(input, 
                    'csv_file', 
                    session=session,
                    roots=volumes, 
                    filetypes=c('', 'csv', 'tsv'))
    
    #-----------------------------------
    # show currently selected csv/tsv file (and count fastq files there?)
    
    output$stdout <- renderPrint({
      # set optional parameters, valid for all CASEs
      # tower
      optional_params$tower <- if(input$tower) {
        "-with-tower"
      } else {
        ""
      }
      
      # skip kraken
      optional_params$skip_kraken <- if(input$skip_kraken) {
        "--skip_kraken2"
      } else {
        ""
      }
      # handle case -profile test
      optional_params$profile <- if(input$nxf_profile == "test") {
        "test,docker"
      } else {
        input$nxf_profile
      }
      # CASE1 - test profile
      if(input$nxf_profile == "test") {
        cat("In case a test profile is selected there is no need to select a .csv/.tsv file")
        nx_notify_success("test profile selected - you can start the pipeline!")
        wd <<- getwd()
        resultsdir <<- file.path(wd, 'results')
        
        nxf_args <<- c("run" ,"nf-core/bacass", 
                       "-profile", optional_params$profile)
        cat(" Nextflow command to be executed:\n\n",
            "nextflow", nxf_args)
                       
      } else if (is.integer(input$csv_file) ) {
        cat("Please select a .csv/.tsv file to start the pipeline\n")
      
      # CASE 2 normal run
      } else {
        wd <<- fs::path_dir( parseFilePaths(volumes, input$csv_file)$datapath )
        resultsdir <<- file.path(wd, 'results')
        
        nxf_args <<- c("run" ,"nf-core/bacass",
                       "--input", parseFilePaths(volumes, input$csv_file)$datapath,
                       "--annotation_tool", input$annotation_tool,
                       "-profile", optional_params$profile, 
                       "--max_cpus", 8, 
                       "--max_memory", '6.GB', 
                       optional_params$skip_kraken,
                       optional_params$tower,
                       "--with-report", paste(resultsdir, "/nxf_workflow_report.html", sep = ""),
                       optional_params$mqc)
        
        cat(" Nextflow command to be executed:\n\n",
            "nextflow", nxf_args)
       }
    })

    #---
    # real call to nextflow-fastp-------
    #----      
    # setup progress bar and callback function to update it
    progress <- shiny::Progress$new(min = 0, max = 1, style = "old")
    
    
    # callback function, to be called from run() on each chunk of output
    # cb_count <- function(chunk, process) {
    #   counts <- str_count(chunk, pattern = "process > fastp")
    #   #print(counts)
    #   val <- progress$getValue() * nfastq
    #   progress$inc(amount = counts/nfastq,
    #                detail = paste0(floor(val), " of ", nfastq, " files"))
    # 
    # 
    # }
    
    # kill function, used to kill the proc when the stopButton is pressed
    # kill_func <- function(chunk, proc) {
    #   if (input$stopButton >= 1) proc$kill() 
    # }
    
    # using processx to better control stdout
    observeEvent(input$run, {
      
      if(is.integer(input$csv_file) & input$nxf_profile != "test") {
        shinyjs::html(id = "stdout", "\nPlease select a .csv/.tsv file first...", add = TRUE)
        nx_notify_warning("No .csv/.tsv file selected!")
      } else {
        # set run button color to red?
        shinyjs::disable(id = "commands_pannel")
        # shinyjs::enable(id = "stopButton")
        nx_notify_success("Looks good, starting...")
        
        # change label during run
        shinyjs::html(id = "run", html = "Running... please wait")
        progress$set(message = "Pipeline running... ", value = 0)
        on.exit(progress$close() )
        
      # Dean Attali's solution
      # https://stackoverflow.com/a/30490698/8040734
        withCallingHandlers({
          shinyjs::html(id = "stdout", "")
          p <- processx::run("nextflow", 
                      args = nxf_args,
                      wd = wd,
                      #echo_cmd = TRUE, echo = TRUE,
                      stdout_line_callback = function(line, proc) {message(line)}, # here you can kill the proc!!!
                      #stdout_callback = kill_func,
                      stderr_to_stdout = TRUE, 
                      error_on_status = FALSE
                      )
          }, 
            message = function(m) {
              shinyjs::html(id = "stdout", html = m$message, add = TRUE); 
              runjs("document.getElementById('stdout').scrollTo(0,1e9);") # scroll the page to bottom with each message, 1e9 is just a big number
            }
        )
        
        
        if(p$status == 0) {
          # hide command pannel 
          shinyjs::hide("commands_pannel")
          
          # clean work dir in case run finished ok
          #work_dir <- paste(parseDirPath(volumes, input$csv_file), "/work", sep = "")
          work_dir <- paste(dirname(as.character(parseFilePaths(volumes, input$csv_file)$datapath)), "/work", sep = "")
          system2("rm", args = c("-rf", work_dir))
          cat("deleted", work_dir, "\n")
            
          # copy mqc to www/ to be able to open it, also use hash to enable multiple concurrent users
          # mqc_report <- paste(parseDirPath(volumes, input$fastq_folder), 
          #                  "/results/multiqc_report.html", # make sure the nextflow-fastp pipeline writes to "results-fastp"
          #                  sep = "")
          #  
          # system2("cp", args = c(mqc_report, paste("www/", mqc_hash, sep = "")) )
          
          
          # render the new action buttons to show report
          # output$mqc_report_button <- renderUI({
          #   actionButton("mqc", label = "MultiQC report",
          #                icon = icon("th"),
          #                onclick = sprintf("window.open('%s', '_blank')", mqc_hash)
          #   )
          # })
          # 
          #
          # build js callback string for shinyalert
          # js_cb_string <- sprintf("function(x) { if (x == true) {window.open('%s') ;} } ", mqc_hash)
          
          shinyalert("Run finished!", type = "success", 
                   animation = "slide-from-bottom",
                   text = "Pipeline finished, check results folder", 
                   showCancelButton = FALSE, 
                   confirmButtonText = "OK",
                   #callbackJS = js_cb_string 
                   #callbackR = function(x) { js$openmqc(mqc_url) }
                   )
        } else {
          shinyjs::html(id = "run", html = "Finished with errors")
          shinyjs::enable(id = "commands_pannel")
          shinyjs::disable(id = "run")
          shinyalert("Error!", type = "error", 
                     animation = "slide-from-bottom", 
                     text = "Pipeline finished with errors, press OK to reload the app and try again.", 
                     showCancelButton = TRUE, 
                     callbackJS = "function(x) { if (x == true) {history.go(0);} }"
                     )
        }
      }
      
    })
    
    
    #------------------------------------------------------------
    session$onSessionEnded(function() {
      # delete own mqc from www, it is meant to be temp only 
      #system2("rm", args = c("-rf", paste("www/", mqc_hash, sep = "")) )
      
      #user management
      isolate({
        users$count <- users$count - 1
        writeLines(as.character(users$count), con = "userlog")
      })
      
    })
  
    #---
  # ask to start over if title or reset clicked
  #----                     
  observeEvent(input$bacassButton, {
    shinyalert(title = "",
               type = "warning",
               text = "Start again or stay on page?", 
               html = TRUE, 
               confirmButtonText = "Start again", 
               showCancelButton = TRUE, 
               callbackJS = "function(x) { if (x == true) {history.go(0);} }" # restart app by reloading page
               )
  })
  observeEvent(input$reset, {
    shinyalert(title = "",
               type = "warning",
               text = "Start again or stay on page?", 
               html = TRUE, 
               confirmButtonText = "Start again", 
               showCancelButton = TRUE, 
               # actually, session$reload() as an R callback should also work
               callbackJS = "function(x) { if (x == true) {history.go(0);} }" # restart app by reloading page
      )
    })
   
    
  }
 
 
 
 shinyApp(ui, server)
 