#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



#necessary packages
#update.packages("htmltools")
update.packages("shiny")
#update.packages("shinyBS")
library(bnlearn)
library(gRain)
library(gRbase)
#install.packages("htmltools")
#install.packages("shiny")
install.packages("shiny")
library(shiny)
load(file = "data_for_shiny_catchment1.Rdata",verbose = F)
attach(variables_for_shiny)
print(labs)
install.packages("shinyBS")
library(shinyBS)
library(fmsb)
library(scales)
#update.packages("htmltools")
#update.packages("shiny")
#update.packages("shinyBS")

#one panel
ui <- fluidPage(
  tabsetPanel(
    tabPanel(h4(title_panel,style="color:rgb(0,61,144);"),
             
             # Application title
             #  titlePanel(title_panel,windowTitle = window_title),
             
             fluidRow( 
               #Input
               column(width = 3, 
                      style = "background-color:#D5D6CE;",
                      h5("Please indicate the appropriate status of the following biological metrics/inidces:"),
                      lapply(1:length(evidence), function(i) {
                        selectInput(inputId = names(evidence[i]), 
                                    #label =  input_question[i],           
                                     label =  h5(input_question[i],style = "font-size:13px;font-weight:bold;",
                                                 bsButton(inputId = paste('q',names(evidence)[i],sep = "_"),
                                             label = "",
                                             icon = icon("question"),
                                             style = "info",
                                             size = "extra-small")
                                    ),
                                    choices = input_choices[[i]],
                                    selected = NA)
                        }),
                      lapply(1:length(evidence), function(i) {
                           bsPopover(id = paste('q',names(evidence)[i],sep = "_"),
                                  title = "",
                                  content = input_help[i],
                                  placement = "right",
                                  trigger = "focus",
                                  options = list(container = "body")
                                  )
                        
                      }),
                                    
                        
                      
                      br(),
                      sliderInput(inputId = "threshold",
                                  label = h5("Change the %-scale of the radar plot here", style = "font-size:13px;font-weight:bold;",
                                             bsButton(inputId = "q_slider",label = "",icon = icon("question"),style = "info", size = "extra-small")
                                  ),
                                  
                                  min = min_threshold_scale, max = max_threshold_scale, value = default_threshold_scale ,step = 1,animate = T,post = "%"),
                      bsPopover(id = "q_slider", title = "",
                                content = paste0(p("You can change the probability scaling of the plot by sliding to the left or right.") #,"More info at: ", 
#                                                 a("link to somewhere", 
#                                                   href = "http://www.google.com",
#                                                   target="_blank")
                                ),
                                placement = "right", 
                                trigger = "focus", 
                                options = list(container = "body")
                      )
               ),
               column(width = 9,
                      tabsetPanel(
                        tabPanel("Diagnostic plot",
                                 #usage information
                                 column(width = 3,#style = "background-color:#D3D4FF;",
                                        includeHTML(path = "html_files/model_description_dia.html"),style = "text-align:justify;"
                                       
                                 ),
                                 # Show a plot of the generated distribution
                                 column(width = 9, #style = "background-color:#D5D6CE;",
                                        h3("You are in the diagnostic analysis",style = "text-align:center;color:rgb(0,61,144);font-size:21px;"),
                                        h3(output_window_title,style = "text-align:center;font-size:27px;"),
                                        #plotOutput("distPlot",height = "1800px")
                                        plotOutput("radar",height = "600px", width = "110%",
                                                   click = "radarclick"),
                                        bsModal(id = "more_info", title = "" , "radar", size = "large",
                                                htmlOutput("explanation"),
                                                plotOutput("plot"),
                                                downloadButton('downloadPlot', 'Download'))
                                        ##chartJSRadarOutput("radar", width = "450", height = "300")
                                 )
                        ),
                        
                        tabPanel("Causal hierarchy",
                                 h3("Summary:"),
                                 #dataTableOutput('table'),
                 #                fluidRow(
                 #                  splitLayout(cellWidths = c("50%", "50%"), tableOutput('table'), includeHTML(path = 'html_files/hierarchy_dia.html'))
                 #                )
                                 tableOutput('table'),
                                 includeHTML(path = 'html_files/hierarchy_dia.html')
                        ),
                        
                        tabPanel("Read more",
                                 h3("How to get information for diagnosis"),
                                 includeHTML(path = "html_files/readmore_dia.html")
                        )
                      )
               )
             )
    ),
    
    
    tabPanel(h4(rev_title_panel,style="color:rgb(37,126,80);"),
             fluidRow( 
               #Input
               column(width = 3, 
                      style = "background-color:#D5D6CE;",
                      h5("Please select the appropriate classes for the following causes of degradation:"),
                      lapply(1:length(rev_evidence), function(i) {
                        selectInput(inputId = names(rev_evidence[i]), 
                                    #rev_input_question[i],
                                    label = h5(rev_input_question[i],style = "font-size:13px;font-weight:bold;",
                                               bsButton(inputId = paste('rev_q',names(rev_evidence)[i],sep = "_"),
                                                        label = "",
                                                        icon = icon("question"),
                                                        style = "info",
                                                        size = "extra-small")
                                    ),
                                    choices = rev_input_choices[[i]],
                                    selected = NA)
                      }),
                      lapply(1:length(rev_evidence), function(i) {
                        bsPopover(id = paste('rev_q',names(rev_evidence)[i],sep = "_"),
                                  title = "",
                                  content = rev_input_help[i],
                                  placement = "right",
                                  trigger = "focus",
                                  options = list(container = "body")
                        )
                        
                      }),
                      
                      br(),
                      sliderInput("rev_threshold", 
                                  label = h5("Change the %-scale of the radar plot here", style = "font-size:13px;font-weight:bold;",
                                             bsButton(inputId = "rev_q_slider",label = "",icon = icon("question"),style = "info", size = "extra-small")
                                  ),
                                  
                                  min = rev_min_threshold_scale, max = rev_max_threshold_scale, value = rev_default_threshold_scale ,step = 1,animate = T,post = "%"),
                      bsPopover(id = "rev_q_slider", title = "",
                                content = paste0(p("You can change the probability scaling of the plot by sliding to the left or right.") #,"More info at: ", 
                                                 #                                                 a("link to somewhere", 
                                                 #                                                   href = "http://www.google.com",
                                                 #                                                   target="_blank")
                                ),
                                placement = "right", 
                                trigger = "focus", 
                                options = list(container = "body")
                      )
               ),               
               column(width = 9,
                      tabsetPanel(
                        tabPanel("Prognostic plot",
                                 #usage information
                                 column(width = 3,#style = "background-color:#D3D4FF;",
                                        includeHTML(path = "html_files/model_description_pro.html"),style = "text-align:justify;"
                                 ),
                                 # Show a plot of the generated distribution
                                 column(width = 9, #style = "background-color:#D5D6CE;",
                                        h3("You are in the prognostic analysis",style = "text-align:center;color:rgb(37,126,80);font-size:21px;"),
                                        h3(rev_output_window_title,style='text-align:center;font-size:27px;'),
                                        #plotOutput("distPlot",height = "1800px")
                                        plotOutput("rev_radar",height = "600px", width = "110%",
                                                   click = "rev_radarclick"),
                                        bsModal(id = "rev_more_info", title = "", trigger = "rev_radar", size = "large",
                                                htmlOutput("rev_explanation"),
                                                plotOutput("rev_plot"),
                                                downloadButton('rev_downloadPlot', 'Download'))
                                        ##chartJSRadarOutput("radar", width = "450", height = "300")
                                 )
                        )
                        ,
                        tabPanel("Biological impact hierarchy",
                                 h3("Summary:"),
                                 tableOutput('rev_table'),
                                 includeHTML(path = 'html_files/hierarchy_pro.html')
                        ),
                        
                        tabPanel("Read more",
                                 h3("How does prognosis works"),
                                 includeHTML(path = "html_files/readmore_pro.html")
                        )
                      )
                      
               )
             )
    )
  ),style = "min-width: 80em;"
)
    
    
    
    # Define server logic required to draw a histogram
    server <- function(input, output) {
      
      posteriors <- reactive({
        #get evidence
        for(ei in 1:length(evidence)){
          evidence[[ei]] <- input[[names(evidence)[ei]]]
        }
 #         print("++++++++++++++++++++++++++++")
 #         print(priors)
 # #        print(evidence)
 #         print("==========================")
 #         print(gRain::querygrain(object = grain_net, nodes = nodes_for_observation, type = "marginal", evidence = evidence))
        #calculate posterior
        post <- gRain::querygrain(object = grain_net, nodes = nodes_for_observation, type = "marginal", evidence = evidence)
#        print(names(post))
#        print(nodes_for_observation)
        post <- post[nodes_for_observation]
#        print(names(post))
#        print(names(priors))
        
        return(post)  
      })
      
      rev_posteriors <- reactive({
        #get evidence
        for(ei in 1:length(rev_evidence)){
          rev_evidence[[ei]] <- input[[names(rev_evidence)[ei]]]
        }
        #calculate posterior
        rev_post <- gRain::querygrain(object = grain_net, nodes = rev_nodes_for_observation, type = "marginal", evidence = rev_evidence)
        rev_post <- rev_post[rev_nodes_for_observation]
        return(rev_post)
      })
      
      
      output$radar <- renderPlot({
        plot_radar_chart(prior = priors, post = posteriors(), pscale = input$threshold/100,for_plot = for_plot, rchartlabs = rchartlabs, col = rgb(0/256,61/256,144/256))
#        print(names(priors))
#        print(for_plot)
#        print(rchartlabs)
      })
      output$rev_radar <- renderPlot({
        plot_radar_chart(prior = rev_priors, post = rev_posteriors(), pscale = input$rev_threshold/100,for_plot = rev_for_plot, rchartlabs = rev_rchartlabs, col = rgb(37/256,126/256,80/256))
      })
      
      #   output$table <- renderDataTable({
      output$table <- renderTable({
        table_radar_chart(prior = priors, post = posteriors(), pscale = input$threshold/100,for_plot = for_plot, rchartlabs = rchartlabs, columnames = c("Potential causes of deterioration","Probability (%)"))
      },digits = 1)
      output$rev_table <- renderTable({
        table_radar_chart(prior = rev_priors, post = rev_posteriors(), pscale = input$rev_threshold/100,for_plot = rev_for_plot, rchartlabs = rev_rchartlabs,columnames = c("Metric name","Probability (%)"))
      },digits = 1)
      
      
      lab_for_modal <- eventReactive(input$radarclick, {
        rad_click <- (input$radarclick)
        labs_click <- clicked_lab(rad_click = rad_click,labs = labs)
      })
      rev_lab_for_modal <- eventReactive(input$rev_radarclick, {
        rev_rad_click <- (input$rev_radarclick)
        rev_labs_click <- clicked_lab(rad_click = rev_rad_click,labs = rev_labs)
      })
      
      
      output$explanation <- renderUI({
        rad_click <- (input$radarclick)
        labs_click <- clicked_lab(rad_click = rad_click,labs = labs)
        tagList(
          # h1(labs[which(labs_click)]),
          h2(rchartlabs[which(labs_click)]),
          br(),
          tags$table( class = "image",
                      align = "right", 
                      style = "max-width:350px;max-height:350px;padding: 20px 10px 10px 10px;",
                      tags$caption(popup_plot_textandimage[which(labs_click),3],
                                   align="bottom",
                                   style = "text-align: right; font-size:xx-small;"
                      ),
                      tags$tr(
                        tags$td(
                          img(
                            src=popup_plot_textandimage[which(labs_click),2], 
                            align = "right", 
                            style = "max-width:350px;max-height:350px;padding: 20px 10px 10px 10px;"
                          )
                        )
                      )
          ), 
          
          #img(src=popup_plot_textandimage[which(labs_click),2], align = "right", style = "max-width:350px;max-height:350px;padding: 20px 10px 10px 10px;"),
          ### the rest of your code
          
          includeHTML(paste("html_files/",popup_plot_textandimage[which(labs_click),1],sep = "")),
          hr(style = "border-top: 2px solid rgb(0,61,144);"),
          h3("How to interpret the probabilities"),
          p(barplot_exp[which(labs_click)],style = "text-align:justify;")

        )})
      output$rev_explanation <- renderUI({
        rev_rad_click <- (input$rev_radarclick)
        rev_labs_click <- clicked_lab(rad_click = rev_rad_click,labs = rev_labs)
        tagList(
          # h1(labs[which(labs_click)]),
          h2(rev_rchartlabs[which(rev_labs_click)]),
          br(),
          tags$table( class = "image",
                      align = "right", 
                      style = "max-width:350px;max-height:350px;padding: 20px 10px 10px 10px;",
                      tags$caption(rev_popup_plot_textandimage[which(rev_labs_click),3],
                                   align="bottom",
                                   style = "text-align: right; font-size:xx-small;"
                      ),
                      tags$tr(
                        tags$td(
                          img(
                            src=rev_popup_plot_textandimage[which(rev_labs_click),2], 
                            align = "right", 
                            style = "max-width:350px;max-height:350px;padding: 20px 10px 10px 10px;"
                          )
                        )
                      )
          ), 
          
          #img(src=rev_popup_plot_textandimage[which(rev_labs_click),2], align = "right", style = "max-width:350px;max-height:350px;"),
          ### the rest of your code
          
          includeHTML(paste("html_files/",rev_popup_plot_textandimage[which(rev_labs_click),1],sep = "")),
          hr(style = "border-top: 2px solid rgb(37,126,80);"),
          h3("How to interpret the probabilities"),
          p(rev_barplot_exp[which(rev_labs_click)], style = "text-align:justify;")
        )})
      
      output$plot <- renderPlot({
        rad_click <- (input$radarclick)
        labs_click <- clicked_lab(rad_click = rad_click,labs = labs)
        
        plot_post_bar(prior = priors[[which(labs_click)]], post = posteriors()[[which(labs_click)]],scale = input$threshold/100)
      },width = 400, height = 200)
      output$rev_plot <- renderPlot({
        rev_rad_click <- (input$rev_radarclick)
        rev_labs_click <- clicked_lab(rad_click = rev_rad_click,labs = rev_labs)
        
        plot_post_bar(prior = rev_priors[[which(rev_labs_click)]], post = rev_posteriors()[[which(rev_labs_click)]],scale = input$rev_threshold/100)
      },width = 400, height = 200)
      
      
      plotInput <- function(){
        rad_click <- (input$radarclick)
        labs_click <- clicked_lab(rad_click = rad_click,labs = labs)
        
        plot_post_bar(prior = priors[[which(labs_click)]], post = posteriors()[[which(labs_click)]],scale = input$threshold/100)
      }
      rev_plotInput <- function(){
        rev_rad_click <- (input$rev_radarclick)
        rev_labs_click <- clicked_lab(rad_click = rev_rad_click,labs = rev_labs)
        
        plot_post_bar(prior = rev_priors[[which(rev_labs_click)]], post = rev_posteriors()[[which(rev_labs_click)]],scale = input$rev_threshold/100)
      }
      
      
      output$downloadPlot <- downloadHandler(
        filename = "Shinyplot.png",
        content = function(file) {
          png(file)
          plotInput()
          dev.off()
        })
      output$rev_downloadPlot <- downloadHandler(
        filename = "Shinyplot.png",
        content = function(file) {
          png(file)
          rev_plotInput()
          dev.off()
        })
      
    }
    
    
    # Run the application 
    shinyApp(ui = ui, server = server)
    
    