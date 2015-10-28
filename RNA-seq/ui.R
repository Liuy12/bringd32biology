shinyUI(fluidPage(
  # Add custom CSS & Javascript;
  tagList(
    tags$head(
      tags$script(type="text/javascript", src = "JavaScript/md5.js"),
      tags$script(type="text/javascript", src = "JavaScript/passwdInputBinding.js"),
      tags$script(type="text/javascript", src = "JavaScript/GoogleAnalytics.js"),
      tags$link(type="text/css", rel="stylesheet", href="CSS/style.css"),
      tags$link(type="text/css", rel="stylesheet", href="https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css"),
      HTML("<style>
         .tooltip {
           opacity:1;
           }
           </style>"),
      tags$style(HTML("
    .progress-striped .bar {
      background-color: #149bdf;
      background-image: -webkit-gradient(linear, 0 100%, 100% 0, color-stop(0.25, rgba(255, 255, 255, 0.6)), color-stop(0.25, transparent), color-stop(0.5, transparent), color-stop(0.5, rgba(255, 255, 255, 0.6)), color-stop(0.75, rgba(255, 255, 255, 0.6)), color-stop(0.75, transparent), to(transparent));
      background-image: -webkit-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
      background-image: -moz-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
      background-image: -o-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
      background-image: linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
      -webkit-background-size: 40px 40px;
         -moz-background-size: 40px 40px;
           -o-background-size: 40px 40px;
              background-size: 40px 40px;
    }
  "))
    )
  ),
  theme = shinytheme("flatly"),
  fluidRow(dashboardPage(skin = 'blue',
                         dashboardHeader(uiOutput('test'), title = img(src = 'img/logo.png', style = "max-width:50%")
                         ),
                         dashboardSidebar(
                           sidebarMenu(id = 'sidebar', 
                                       #                                        sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                       #                                                          label = "Search..."),
                                       menuItem("Introduction", tabName = "introduction", icon = icon("circle-o")),
                                       menuItem("Start analysis", tabName = "Analysis", icon = icon("bolt"),
                                                menuSubItem("Set input options", tabName = 'SetInput', icon = icon("flag")),
                                                menuSubItem('Charts', tabName = 'Charts', icon = icon("bar-chart"))
                                       ),
                                       menuItem('Tutorial', tabName = "Tuto", icon = icon("dashboard")),
                                       menuItem('Docomentation', tabName = "doc", icon = icon("file-text")),
                                       menuItem("About us", tabName = "aboutus", icon = icon("users"))
                           )
                         ),
                         dashboardBody(
                           tabItems(
                             # First tab content
                             tabItem(tabName = "introduction", 
                                     includeHTML('www/HTML/carousel.html'),
                                     includeHTML('www/HTML/Introduction.html')),
                             tabItem(tabName = "SetInput", 
                                     #uiOutput('progressbar'),
                                     fluidRow(
                                       column(width = 6, 
                                              box(
                                                title = "Experiment information", status = "warning", solidHeader = TRUE,width = NULL,
                                                collapsible = TRUE,
                                                radioButtons('countMatrix', 'Are input values count/integer values?', choices = c('Yes', 'No'), selected = 'Yes', inline = TRUE),
                                                verbatimTextOutput("value_countMatrix"),                                               
                                                radioButtons('spikein', 'Are spike-ins included?', choices = c('Yes', 'No'), selected = 'No', inline = TRUE),
                                                verbatimTextOutput("value_spikein"),
                                                radioButtons('ExpDesign', 'What is the design of the experiment?', choices = c('Not available', 'Two-level', 'Multi-level'), selected = 'Two-level', inline = TRUE),
                                                verbatimTextOutput("value_ExpDesign"),
                                                conditionalPanel("input.ExpDesign =='Multi-level'",
                                                                 radioButtons('MultiLevel', 'For experiments with multi-level design', choices = c('Specify two conditions to compare', 'Identify most variable genes'), selected = 'Identify most variable genes', inline = FALSE),
                                                                 verbatimTextOutput("value_MultiLevel"),
                                                                 conditionalPanel("input.MultiLevel == 'Specify two conditions to compare'", 
                                                                                  fluidRow(
                                                                                    column(4, offset = 1, textInput("Con_S1", label = "Enter first condition name (For example, C1)", 
                                                                                                                    value = ""
                                                                                    )),
                                                                                    column(4,offset = 2, textInput("Con_S2", label = "Enter second condition name (For example, C2)", 
                                                                                                                   value = ""
                                                                                    ))
                                                                                  ),
                                                                                  fluidRow(
                                                                                    column(4, offset = 1, verbatimTextOutput("value_Con_S1")),
                                                                                    column(4, offset = 2, verbatimTextOutput("value_Con_S2"))
                                                                                  )
                                                                 ))
                                              ),
                                              box(
                                                title = "File input", status = "primary", solidHeader = TRUE, width = NULL,
                                                collapsible = TRUE,
                                                #                                           HTML('<div class="form-group shiny-input-container">
                                                #   <label class="control-label" for="DEmethod">Please select a method for DE analysis, <a target="_blank" href="http://liuy12.github.io/2015/08/02/Comparison%20of%20differential%20expression%20methods.html">not sure?</a></label>
                                                #   <div>
                                                #     <select id="DEmethod" class="form-control"><option value="XBSeq">XBSeq</option>
                                                # <option value="DESeq">DESeq</option>
                                                # <option value="DESeq2">DESeq2</option>
                                                # <option value="edgeR">edgeR</option>
                                                # <option value="edgeR-robust">edgeR-robust</option>
                                                # <option value="limma-voom">limma-voom</option>
                                                # <option value="scde">scde</option></select>
                                                # <script type="application/json" data-for="SCVmethod">{}</script>
                                                # </div>
                                                # </div>'),
                                                uiOutput('DEinput'),
                                                #                                                        conditionalPanel(condition = "input.countMatrix == 'Yes' & (input.ExpDesign ==  'Two-level' | input.MultiLevel == 'Specify two conditions to compare')", 
                                                #                                                                         selectizeInput(
                                                #                                                                           "DEmethod", 
                                                #                                                                           label = 'Please select a method for DE analysis',
                                                #                                                                           choices = c('XBSeq', 'DESeq', 'DESeq2', 'edgeR', 'edgeR-robust', 'limma-voom', 'scde', 'SAMSeq'),
                                                #                                                                           options = list(placeholder = 'select a method below',
                                                #                                                                                          onInitialize = I('function() { this.setValue(""); }'))
                                                #                                                                         )
                                                #                                                                         ),
                                                #                                                        conditionalPanel(condition = "input.ExpDesign == 'Not available' | input.MultiLevel == 'Identify most variable genes'", 
                                                #                                                                         selectizeInput(
                                                #                                                                           "DEmethod", 
                                                #                                                                           label = 'Please select a method for highly variable genes analysis',
                                                #                                                                           choices = c('Brennecke_2013'),
                                                #                                                                           options = list(placeholder = 'select a method below',
                                                #                                                                                          onInitialize = I('function() { this.setValue(""); }'))
                                                #                                                                         )
                                                #                                                        ),   
                                                #                                                        conditionalPanel(condition = "input.countMatrix == 'No' & (input.ExpDesign ==  'Two-level' | input.MultiLevel == 'Specify two conditions to compare')", 
                                                #                                                                         selectizeInput(
                                                #                                                                           "DEmethod", 
                                                #                                                                           label = 'Please select a method for DE analysis',
                                                #                                                                           choices = c('monocle', 'limma'),
                                                #                                                                           options = list(placeholder = 'select a method below',
                                                #                                                                                          onInitialize = I('function() { this.setValue(""); }'))
                                                #                                                                         )
                                                #                                                        ),
                                                verbatimTextOutput("value_DE"),
                                                fileInput(
                                                  'file_obs', 'Choose CSV/TXT File for RNA-seq', accept=c('text/csv', 
                                                                                                          'text/comma-separated-values,text/plain', 
                                                                                                          '.csv')
                                                ),
                                                conditionalPanel(
                                                  condition = "input.DEmethod == 'XBSeq'",
                                                  fileInput(
                                                    'file_bg', 'Choose CSV/TXT File for RNA-seq (bg), required if you choose XBSeq', 
                                                    accept=c('text/csv',
                                                             'text/comma-separated-values,text/plain', 
                                                             '.csv')
                                                  )
                                                ),
                                                conditionalPanel(
                                                  condition = "input.ExpDesign != 'Not available'",
                                                  fileInput('file_design', 
                                                            'Choose CSV/TXT File for experiment design',
                                                            accept=c('text/csv', 
                                                                     'text/comma-separated-values,text/plain', 
                                                                     '.csv')
                                                  )
                                                ),
                                                conditionalPanel(
                                                  condition = "input.spikein == 'Yes'",
                                                  fileInput(
                                                    'file_spikein', 'Choose CSV/TXT File for spike-ins', 
                                                    accept=c('text/csv',
                                                             'text/comma-separated-values,text/plain', 
                                                             '.csv')
                                                  )
                                                )
                                              )
                                       ),
                                       column(width = 6, 
                                              box(
                                                title = "Options for DE/HVG method", status = "info", solidHeader = TRUE,width = NULL,
                                                collapsible = TRUE,
                                                conditionalPanel(
                                                  condition = "input.DEmethod == 'DESeq' | input.DEmethod == 'XBSeq'",
                                                  selectizeInput("SCVmethod", 
                                                                 label = "Please select a method to estimate dispersion", 
                                                                 choices =c('pooled', 'per-condition', 'blind'),
                                                                 selected = 'pooled'
                                                  ),
                                                  verbatimTextOutput("SCVmethod"),
                                                  selectizeInput("SharingMode",
                                                                 label = "Please select a method for sharing mode",
                                                                 choices = c('maximum', 'fit-only', 'gene-est-only'),
                                                                 selected = 'maximum'
                                                  ),
                                                  verbatimTextOutput("SharingMode"),
                                                  selectizeInput("fitType",
                                                                 label = "Please select a method for fitType",
                                                                 choices = c('local', 'parametric'),
                                                                 selected = 'local'
                                                  ),
                                                  verbatimTextOutput("fitType"),
                                                  conditionalPanel(
                                                    condition = "input.DEmethod == 'XBSeq'",
                                                    selectizeInput("ParamEst", 
                                                                   label = "Please select a method to estimate distribution parameters", 
                                                                   choices =c('Non-parametric' = 'NP', 
                                                                              'Maximum liklihood estimation' = 'MLE'),
                                                                   selected = 'NP'
                                                    ),
                                                    verbatimTextOutput("ParamEst")
                                                  )
                                                ),
                                                conditionalPanel(
                                                  condition = "input.DEmethod == 'DESeq2'",
                                                  selectizeInput("fitType_DESeq2", 
                                                                 label = "Please select a method for fit type", 
                                                                 choices =c('local', 'parametric', 'mean'),
                                                                 selected = 'local'
                                                  ),
                                                  verbatimTextOutput("fitType_DESeq2"),
                                                  selectizeInput("Test",
                                                                 label = "Please select a method for statistical test",
                                                                 choices = c('Wald test' = 'Wald',
                                                                             'Log ratio test' = 'LRT'),
                                                                 selected = 'Wald'
                                                  ),
                                                  verbatimTextOutput("Test"),
                                                  selectizeInput("cooksCutoff",
                                                                 label = "Please choose either to turn on or off cooks distance cutoff",
                                                                 choices = c('on',
                                                                             'off'),
                                                                 selected = 'off'
                                                  ),
                                                  verbatimTextOutput("cooksCutoff")
                                                )
                                                #                conditionalPanel(
                                                #                  condition = "input.DEmethod == 'edgeR-robust'",
                                                #                  selectizeInput("residualType", 
                                                #                                 label = "Please select a method for calculating residuals", 
                                                #                                 choices =c("pearson", "deviance", "anscombe"),
                                                #                                 selected = 'pearson'
                                                #                  ),
                                                #                  verbatimTextOutput("residualType")
                                                #                )
                                              ),
                                              box(
                                                title = "Criteria for DE/HVG genes", status = "success", solidHeader = TRUE,width = NULL,
                                                collapsible = TRUE,
                                                numericInput("log2bmcutoff", label = "Please set a cutoff for log2 expression intensity (Usually can be determined from density plot)", 
                                                             value = 5, min = 1
                                                ),
                                                verbatimTextOutput("log2bmcutoff_value"),
                                                uiOutput('DECriteria'),
                                                actionButton('DEstart', label = 'Start analysis!'),
                                                textOutput("DEstart")
                                              )
                                       ))),
                             tabItem(tabName = "Charts",
                                     uiOutput("Chartpage")
                             ),
                             tabItem(tabName = "Tuto",
                                     includeHTML('www/HTML/Tutorial.html')),
                             tabItem(tabName = 'doc',
                                     includeHTML('www/HTML/Documentation.html')),
                             tabItem(tabName = "aboutus", 
                                     fluidRow(
                                       valueBoxOutput('totalvisits'),
                                       valueBoxOutput('thismonthvisits'),
                                       valueBoxOutput('totalusers')),
                                     includeHTML('www/HTML/timeline.html')
                             )
                           )
                         )
  ))))
