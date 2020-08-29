#' CTSgetR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_CTSgetR_ui <- function(id) {
  ns <- NS(id)
  
  tagList(uiOutput(ns('sidebar_ui')))
  
}

#' tCTSgetR Server Function
#'
#' @noRd 
mod_CTSgetR_server <- function(id,data=NULL) {

    moduleServer(
      id,
      function(input, output, session) {
        
        
        #if data is NULL
        #use comma xseparated text input
        #else supply a data frame to choose inpout column from
        output$data_input<-renderUI({
          
          ns <- session$ns
          
          if(is.null(data)){
            tagList(
              textInput(ns('from_obj'), 'identifier(s)',value='alanine', placeholder = 'alanine, Pyruvic acid'),
              tippy_this(ns('from_obj'), 'Supply comma separated identifiers')
              )
            
          } else {
            
            tagList(
              selectizeInput(ns('from_obj'), 'identifiers',choices=colnames(data)),
              tippy_this(ns('from_obj'), 'Select column containing values to translate from')
            )
          }
          
        })
        
        
        output$sidebar_ui <- renderUI({
          
          ns <- session$ns
          
          
          from_opts <- valid_from()
          to_opts <- valid_to()
          
          fluidRow(column(
            12,
            uiOutput(ns('data_input')),
            selectizeInput(ns('from_id'), 'from', choices = from_opts, selected = 'Chemical Name'),
            selectizeInput(ns('to_id'), 'to', choices = to_opts, selected='KEGG',multiple=TRUE),
            tags$div(actionButton(ns('cts_translate'), 'translate', icon = icon('life-ring')),align='center')
          ))
          
        })
        
        
        #process input
        get_input<-reactive({
          
          shiny::validate(need(!is.null(input$from_obj),'Choose identifier options and then translate.'))
          
          .db_name<-Sys.getenv('ctsgetr_DB')
          if(.db_name ==''){
            .db_name<-'ctsgetr.sqlite'
          }
          
          
          if(is.null(data)){
             id <- input$from_obj %>%
            strsplit(.,',') %>%
            unlist() %>%
            trimws()
          } else {
            id<-data[,input$from_obj] %>%
              as.character()
          }
          
         
          
          from <- input$from_id
          to <- input$to_id
          
          list(id=id,from=from,to=to,db_name=.db_name)
          
        })
        
      
        
        get_rv<-eventReactive(input$cts_translate,{
          
         
          args<-get_input()
          
          #switch between local and
          #API based lookup
          #could not simplify with fun<-CTSgetR or other; do.call(fun,args)
          if(!Sys.getenv('CTSgetR_API') == ''){
            
            args<-list(url= Sys.getenv('CTSgetR_API'), body=args)
            
            future({
              do.call(post_ocpu, args)
            })   %...>%
              (function(e) {
                e$results
              }) %...!%
              (function(e) {
                warning(e)
                return(NULL)
              })
            
          } else {  
            
            future({
              do.call(CTSgetR, args)
            })   %...>%
              (function(e) {
                e
              }) %...!%
              (function(e) {
                warning(e)
                return(NULL)
              })
            
          }
          
        })
       
        
        return(get_rv)
        
      }
    )
}




test<-function(){
  
  
  #modules test
  module_test<-function(){
    
    
    # Module definition, new method
    myModuleUI <- function(id, label = "Input text: ") {
      ns <- NS(id)
      tagList(
        textInput(ns("txt"), label),
        textOutput(ns("result"))
      )
    }
    
    myModuleServer <- function(id, prefix = "") {
      moduleServer(
        id,
        function(input, output, session) {
          
          
          
          
          output$result <- renderText({
            paste0(prefix, toupper(input$txt))
          })
          
          
        }
      )
    }
    
    
    
    # Use the module in an application
    ui <- fluidPage(myModuleUI("myModule1"))
    server <- function(input, output, session) {
      myModuleServer("myModule1", prefix = "Converted to uppercase: ")
    }
    shinyApp(ui, server)
    
    
    
  }
  
  
  #async example
  # https://rstudio.github.io/promises/articles/casestudy.html
  #shiny with promises https://rstudio.github.io/promises/articles/shiny.html
  #https://engineering-shiny.org/optimizing-shiny-code.html#asynchronous-in-shiny
  
  #deps for local version
  library(shiny)
  library(tippy)
  library(CTSgetR)
  library(ocpuclient) # for CTSgetR API
  
  # Sys.setenv('ctsgetr_DB'='inst/ctsgetr.sqlite') #local
  Sys.setenv('ctsgetr_DB'='/ctsgetr/inst/ctsgetr.sqlite') # at API
  Sys.setenv('CTSgetR_API'='http://localhost/ocpu/library/CTSgetR/R/CTSgetR') # url of API endpoint See notes for how to start API
  
  
  CTSgetR_module<-function(){
    library(promises)
    library(future)
    plan(multisession)
    
    
    example<-data.frame('chemical_name' = c('alanine','DMT'))
    
    #module
    ui <- fluidPage(
      
      sidebarLayout(position = "left",
                    sidebarPanel(tagList(mod_CTSgetR_ui("translate"))),
                    mainPanel(verbatimTextOutput("main_out")))
      
    )
    
    server <- function(input, output, session) {
      
      translation <- mod_CTSgetR_server('translate',data=example)
      
      output$main_out <- renderPrint({
        translation() %...>% print(.)
        
      })
    }
    
    shinyApp(ui, server)
  }
  
  #deps for async api version
  library(ocpuclient)
  library(promises)
  library(future)
  plan(multisession)
  # plan(multiprocess)
  # plan(sequential)
  
  #module
  ui <- fluidPage(
    # fluidRow(
    #   mod_CTSgetR_ui("translate"),
    #   verbatimTextOutput('main_out'),
    #   actionButton('cts_debug', 'explode', icon = icon('life-ring'))
    # )
    sidebarLayout(position = "left",
                  sidebarPanel(tagList(mod_CTSgetR_ui("translate"))),
                  mainPanel(verbatimTextOutput("main_out")))

  )
  
  server <- function(input, output, session) {
    
    rv<-reactiveValues(translation=NULL)
    
    translation<-mod_CTSgetR_server('translate')
    
    # # observe({
    #   future({
    #     mod_CTSgetR_server('translate')
    #   })   %...>%
    #     (function(e) {
    #       rv$translation<-f(e)
    #     }) %...!%
    #     (function(e) {
    #       warning(e)
    #       return(NULL)
    #     })
    # })
    
    # observe({
    #   input$cts_debug
    #   # browser()
    #   rv<-translations()
    # })
    
    output$main_out <- renderPrint({
      
      # res<-translation()
      
      # rv$translation<-translation()
      
      translation() %...>% print(.)
      # translation() %...>%
      #   (function(e) {
      #     rv$translation<-e
      #   }) %...!%
      #   (function(e) {
      #     warning(e)
      #     rv$translation<-NULL
      #   })
      
      # future({
      #   translation()
      # })   %...>%
      #   (function(e) {
      #     rv$translation<-e
      #   }) %...!%
      #   (function(e) {
      #     warning(e)
      #     return(NULL)
      #   })

      # browser()
      
      # promise_resolve()
     # req(reactiveValuesToList(rv)$translation)


    })
  }
  
  shinyApp(ui, server)

  #example of shiny modules and futures
  #ref: https://blog.hmrtn.xyz/2020/02/28/a-few-tips-for-writing-robust-shiny-applications/
  test<-function(){
    
    # --- Putting it all together ---
    library(shiny)
    library(future)
    # - script.R -
    # A slow function
    slow_fun <- function(x) {
      Sys.sleep(x)
    }
    # This will take ~1s
    async <- function(x, y) {
      x_ <- future({slow_fun(x)}) %plan% multiprocess
      y_ <- future({slow_fun(y)}) %plan% multiprocess
      
      list(value(x_), value(y_))
    }
    # - Module script .R -
    module_ui <- function(id) {
      ns <- NS(id)
      tagList(
        plotOutput(outputId = ns('random_plot'))
      )
    }
    module <- function(input, output, session, X, Y) {
      xy <- async(as.numeric(X()), as.numeric(Y()))
      x <- xy[[1]]
      y <- xy[[2]]
      output$random_plot <- renderPlot({
        tryCatch( # Handle errors with tryCatch
          expr = {
            plot(rnorm(x), rnorm(y))
          },
          error = function(e) {
            showNotification(as.character(e))
          }
        )
      })
    }
    # - ui.R -
    ui <- function(request) {
      fluidPage(
        textInput(inputId = 'X', label = 'X: ', value = 100),
        textInput(inputId = 'Y', label = 'Y: ', value = 100),
        actionButton(inputId = 'refresh_btn', label = 'Refresh'),
        module_ui('some_id')
      )
    }
    # - server.R -
    server <- function(input, output, session) {
      
      observeEvent( # watch the refresh button 
        eventExpr = { input$refresh_btn },
        handlerExpr = {
          callModule( module, 
                      'some_id', 
                      reactive(input$X), 
                      reactive(input$Y) )
        }
      )
    }
    # run
    shinyApp(ui, server)
    
  }
  
# no modules async app ----------------------------------------------------
  
  ui <- fluidPage({


    
    sidebarLayout(position = "left",
                  sidebarPanel(uiOutput('sidebar_ui')),
                  mainPanel(verbatimTextOutput("main_out")))
  
  })
  
  
  
  server <- function(input, output) {
  
    #state store
    #sesssion_db #rbind of all user 
    #   querries for the session
    rv <- reactiveValues()
    
      
    output$sidebar_ui <- renderUI({
      from_opts <- valid_from()
      to_opts <- valid_to()
      
      fluidRow(column(
        12,
        textInput('from_obj', 'identifier(s)',value='alanine', placeholder = 'alanine, Pyruvic acid'),
        tippy_this('from_obj', 'Supply comma separated identifiers'),
        selectizeInput('from_id', 'from', choices = from_opts, selected = 'Chemical Name'),
        selectizeInput('to_id', 'to', choices = to_opts, selected='KEGG',multiple=TRUE),
        tags$div(actionButton('cts_translate', 'translate', icon = icon('life-ring')),align='center')
      ))
      
    })
    
    
    #process input
    get_input<-reactive({
      
      shiny::validate(need(!is.null(input$from_obj),'Choose identifier options and then translate.'))
      
      .db_name<-Sys.getenv('ctsgetr_DB')
      if(.db_name ==''){
        .db_name<-'ctsgetr.sqlite'
      }
      
      #local calculate
      id <- input$from_obj %>%
        strsplit(.,',') %>%
        unlist() %>%
        trimws()
      
      from <- input$from_id
      to <- input$to_id
      
      list(id=id,from=from,to=to,db_name=.db_name)
      
    })
    
    #get translations
    # .get_translations<-eventReactive(input$cts_translate,{
    
      observeEvent(input$cts_translate,{
      # https://www.r-bloggers.com/asynchronous-background-execution-in-shiny-using-callr/
     
      args<-get_input()
      
      #switch between local and
      #API based lookup
      if(!Sys.getenv('CTSgetR_API') == ''){
        
        args<-list(url = Sys.getenv('CTSgetR_API'), body=args)
        main_fun<-post_ocpu
        f<-function(x){
          if(is.null(x$error)) x$results else x
        }
      
      } else {
        main_fun<-CTSgetR #
        f<-function(x){x}
      }
    
      
      
      future({
        do.call(main_fun, args)
        # CTSgetR(id=args$id,from=args$from,to=args$to,db_name=args$db_name)
      })   %...>%
        (function(e) {
          rv$session_db<-f(e)
        }) %...!%
        (function(e) {
          warning(e)
          return(NULL)
        })
    
      
      
    })
    
    
    output$main_out <- renderPrint({
      
      req(rv$session_db)
      #want to trigger calculation
      #then show the updated rv$session_db
      #.get_translations() #%...>% {.}
      # browser()

       # print(reactiveValuesToList(rv))
      
    })

  }
  
  shinyApp(ui = ui, server = server)
      
      

#proof of async app -----------------------------------------------------
#inter leaved spleep/awake
  
  
  
  sleep_awake<-function(id=round(rnorm(1,10,1)*10000,0),sleep_time=1,awake=NULL){
    
    
    if(is.null(awake)) awake <- paste0('[',Sys.time(),'] ','I am awake!')
    
    Sys.sleep(sleep_time)
    print(paste0('[',id,']',awake))
    
  }
  
  x <- function(sleep=2, name = 'A') {
    future({
      sleep_awake(name, sleep = sleep)
    }) %...>%
      (function(e) {
        return(e)
      }) %...!%
      (function(e) {
        warning(e)
        return(NULL)
      })
  }  
  
  start<-Sys.time()
  for( i in 1:10){x(a)}
  # sleep_awake()
  (end<-Sys.time()-start)
  #create 2 UIs which can freeze and update
  #counter
  ui <- fluidPage({
    
    sidebarLayout(position = "left",
                  sidebarPanel(uiOutput('sidebar_ui')),
                  mainPanel(
                    tagList(
                    column(12, verbatimTextOutput('main_a')),
                    column(12, verbatimTextOutput('main_b'))
                    )
                  ))
    
  })
  
  server <- function(input, output) {
    
    output$sidebar_ui <- renderUI({
      fluidRow(column(
        12,
        numericInput('sleep_a', 'sleep A',1,0,5,1),
        tags$div(actionButton('go_a', 'go', icon = icon('life-ring')),align='center'),
        numericInput('sleep_b', 'sleep B',1,0,5,1),
        tags$div(actionButton('go_b', 'go', icon = icon('life-ring')),align='center')
      ))
      
    })
    
    
    rv <- reactiveValues(res = NULL)
    
    observeEvent(input$go_a, {
      
      sleep <- input$sleep_a
      start<-Sys.time()
      future({
        sleep_awake('A', sleep = sleep)
      }) %...>%
        (function(e) {
          rv$a <- e
        }) %...!%
          (function(e) {
            rv$a <- NULL
            warning(e)
          })
      print('A--',Sys.time()-start)
      print(rv$b)
    })
    
    observeEvent(input$go_b, {
      
      sleep <- input$sleep_b
      start<-Sys.time()
      future({
        
        sleep_awake('B', sleep = sleep)
      }) %...>%
        (function(e) {
          rv$b <- e
        }) %...!%
        (function(e) {
          rv$b <- NULL
          warning(e)
        })
      print('B--',Sys.time()-start)
      print(rv$b)
    })
    
    # trigger_b <- eventReactive(input$go_b, {
    #   sleep<-input$sleep_b
    #   future({
    #     sleep_awake('B',sleep=sleep)
    #   }) %...>% return()
    # })
    
    output$main_a <- renderPrint({
      # req(rv$a)
      shiny::validate(need(!is.null(rv$a),''))
      rv$a
      
    })
    
    output$main_b <- renderPrint({
      req(rv$b) # trigger_b() #%...>% print()
    })
    
    # output$main_out_A<- renderUI({
    #   
    #   fluidRow(column(12,uiOutput('main_a')),
    #            column(12,uiOutput('main_b')))
    # })
      
  }
  

  shinyApp(ui = ui, server = server)

  
  

# working example in ----------------------------------------------------------

  library(shiny)
  library(promises)
  library(future)
  plan(multisession)
  
  ui <- function(request) {
    tagList(verbatimTextOutput("pr"),
            plotOutput("plot"))
  }
  
  server <- function(input,
                     output,
                     session) {
    
    rv <- reactiveValues(res = NULL)
    
    future({
      Sys.sleep(5)
      rnorm(5)
    }) %...>%
      (function(e) {
        rv$res <- e
      }) %...!%
      (function(e) {
        rv$res <- NULL
        warning(e)
      })
    
    output$pr <- renderPrint({
      req(rv$res)
    })
    
    output$plot <- renderPlot({
      plot(iris)
    })
  }
  
  shinyApp(ui, server)
}
