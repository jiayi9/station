
ui <- fluidPage(title = "Station Dashboard   ",
 # theme = shinytheme("cosmo"),
 tags$body(tags$script(src="iframeResizer.contentWindow.min.js")),
 
 tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                              }
                              "))),
  
#  HTML("<script> var draggableDiv = $('.draggable');
#                                                                           draggableDiv.draggable({
#                                                                             handle: $('.box-header', draggableDiv)
#                                                                           });</script> "),
 
 
 navbarPage(tags$b(tags$i("Station Dashboard")),inverse = TRUE, position = "fixed-top",
   tabPanel(" Data & Overview", br(),br(),br(),
            
            
            
            h4("Disclaimer: All data here used are simulated fake data."),
#             checkboxInput("showPreChecks","Show data pre-checks",value = FALSE),
#             div(style="font-size:x-small",
#                 
#             conditionalPanel("input.showPreChecks == 1",
#                              h4("Data pre-Checks"),
#                              fluidRow(
#                                column(8, dataTableOutput("extreme_summary")),
#                                column(4, 
#                                       br(),
#                                       numericInput("z","Number of Sigmas off the Means",min=3,max=30,value = 15,step=1),
#                                       radioButtons("outlierTreatment",label = NULL,
#                                                    choices = c("Exclude from analysis", "Retain in analysis"),
#                                                    selected = "Retain in analysis", inline=TRUE))
#                               )
#                              
#                              
#                              )
#             ),

            HTML('<button data-toggle="collapse" data-target="#dataPreCheck">Show data pre-checks</button>'),
            tags$div(id="dataPreCheck",  class="collapse",style="font-size:x-small", 
                     h4("Data pre-Checks"),
                     fluidRow(
                       column(8, dataTableOutput("extreme_summary")),
                       column(4, 
                              br(),
                              numericInput("z","Number of Sigmas off the Means",min=3,max=30,value = 15,step=1),
                              radioButtons("outlierTreatment",label = NULL,
                                           choices = c("Exclude from analysis", "Retain in analysis"),
                                           selected = "Retain in analysis", inline=TRUE))
                     )
                     
            ),
            hr(),
            

            h4("Data Selection"),
            div(style="font-size:x-small",
    
            splitLayout(
              uiOutput("PRODUCT"),
              uiOutput("SITE"),
              uiOutput("CAPACITY"),
              uiOutput("RUNTYPE"),
              br()
              

              
              
              

              
              ),
          splitLayout(
            selectInput("by","Analyze by",choices = c("Day","Week"),selected = "Day"),
            uiOutput("FROM"),
            uiOutput("TO"),
            numericInput("rollN","MA rolling basis",min=2,max=30,value = 4),
            
            verticalLayout(br(),br(),textOutput("dates_num"))
            
            ,
            br(),br(),br(),br()
            ),
    
            splitLayout(
              uiOutput("HDA_CODE"),
              
              uiOutput("PREFIX"),
              uiOutput("LINE"),
              uiOutput("RPI"),
              uiOutput("HSI"),
              uiOutput("CSI"),
              uiOutput("SBR"),
              
              br(),br()
              
              ),
            splitLayout(
              uiOutput("MBA_PART_NUM"),
              uiOutput("MOTOR_BASE"),
              uiOutput("MOTOR_RAMP_VEN"),
              uiOutput("HSA_FW"),
              uiOutput("HSA_REV"),
              uiOutput("HSA_VENDOR"),
              uiOutput("MEDIA_TYPE"),
              br(),br()
              
              ),
            splitLayout(
              uiOutput("RAMP_CAVITY"),
              uiOutput("RAMP_MOLD"),
#              uiOutput("MOTOR_SERIAL_NUM"),
#              uiOutput("MOTPF_SERIAL_EXTD_NUM"),
              uiOutput("MOTPF_SHIP_DATE"),
              uiOutput("RAMP_DATE_CODE"),
              br(),br(),br(),br(),br()
              
            ),
              br(),
              splitLayout(
                br(),br(),br(),br(),br(),br(),br(),
                actionButton("ok","OK",class = "btn btn-primary btn-s",style="font-size:95%;height:30px;width:60px"),
                br(),br()
                
                )
            
          ),

          div(style="font-size:x-small",textOutput("seg_text")),

          hr(),
          h4("Mean Shift Detector"),
          div(style="font-size:x-small",
              
              dataTableOutput("shift_table"),
              helpText("Means below 2.5 std's are marked blue. Means above 2.5 std's are marked red.")
              
          ),
          hr(),
          h4("Trend of the Selected Parameter"),
          plotOutput("shift_click",brush = brushOpts("brush_01",resetOnNew = TRUE),hover = hoverOpts(id ="hover")),
          hr(),
          
#             checkboxInput("showContri","Contribution Analysis",value = FALSE),
#             
#             conditionalPanel("input.showContri",
#                              
#               #numericInput("shift_rollN","MA rolling basis",min=2,max=30,value = 7),
#               
#               h4("Top Contributing Attributes to Outliers"),
#               
#               textOutput("shift_contri_info",container = tags$b),
#               actionButton("runOutlierContriRank","Show Rank",class="btn btn-primary btn-sm"),
#               radioButtons("shift_breakdown", "Break down by",choices = c("Attribute to Day","Day to Attribute","Just Attribute"),inline = TRUE),
#               #checkboxInput("shift_breakdownbyweek","Break down by day",value = TRUE),
#               helpText("Note: A selection of at least 10 points recommended for valid P-values and rankings."),
#               textOutput("shift_pvalue_insig",container = tags$b),
#               
#               #verbatimTextOutput("LL"),
#               uiOutput("outlier_contri")
# 
# 
#             ),
          HTML('<button data-toggle="collapse" data-target="#showContri">Outliers contribution analysis</button>'),
          tags$div(id="showContri",  class="collapse", 
                   h4("Top Contributing Attributes to Outliers"),
                   
                   textOutput("shift_contri_info",container = tags$b),
                   actionButton("runOutlierContriRank","Show Rank",class="btn btn-primary btn-sm"),
                   radioButtons("shift_breakdown", "Break down by",choices = c("Attribute to Day","Day to Attribute","Just Attribute"),selected = "Just Attribute",inline = TRUE),
                   #checkboxInput("shift_breakdownbyweek","Break down by day",value = TRUE),
                   div(style="font-size:x-small",helpText("Note: A selection of at least 10 points recommended for valid P-values and rankings.")),
                   textOutput("shift_pvalue_insig",container = tags$b),
                   
                   #verbatimTextOutput("LL"),
                   uiOutput("outlier_contri")
                     
          )

            ),
tabPanel("Trace",br(),br(),br(),
         tabsetPanel(
           tabPanel("Selected Drives & PRE2 Error Code",
                    h4("PRE2 Error Code Check"),
                    div( style = "text-align:right",
                         fluidRow(
                           column(4,
                                  tableOutput("compare")
                                  
                                  
                           ),
                           column(2,
                                  uiOutput("FAIL_CODE_DEFINE")
                                  
                           )
                         )
                         
                    ),
                    div( style = "font-size:x-small",
                         dataTableOutput("ref_table")
                    )
                    
           ),
           tabPanel("Upstream & Downstream Trends",
                    fluidRow(
                      column(6,
                             plotOutput("motor_4", brush = brushOpts("brush_21",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
                             div(style="font-size:x-small",helpText("Red horizontal lines are Specs for 1D. Orange horizontal lines are Specs for 2D")),
                             plotOutput("motor_1", brush = brushOpts("brush_24",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
                             
                             plotOutput("motor_5", brush = brushOpts("brush_23",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
                             
                             #H2_ramp_cyl
                             plotOutput("motor_8", brush = brushOpts("brush_28",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
                             
                             #H1-H0
                             plotOutput("motor_7", brush = brushOpts("brush_27",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
                             
                             
                             plotOutput("PROJECTION_DYNAMIC_FLANGE_HT_chart", brush = brushOpts("brush_31",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
                             plotOutput("RAMP_SEATING_HT_chart", brush = brushOpts("brush_33",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover"))
                             
                             
                      ),
                      column(6,
                             plotOutput("motor_3", brush = brushOpts("brush_22",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
                             div(style="font-size:x-small",helpText(" ")),
                             
                             plotOutput("motor_2", brush = brushOpts("brush_25",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
                             plotOutput("motor_6", brush = brushOpts("brush_26",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
                             
                             #H3_ramp_cyl
                             plotOutput("motor_9", brush = brushOpts("brush_29",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
                             
                             plotOutput("motor_10", brush = brushOpts("brush_30",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
                             
                             plotOutput("PIVOT_SEAT_HT_chart", brush = brushOpts("brush_32",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover"))
                             
                             
                             
                             
                             
                      )
                    )
           )
           #              ,
           #               tabPanel("CERT",
           #                        fluidRow(
           #                          column(6,
           #                                 plotOutput("cert_1", brush = brushOpts("brush_13",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
           #                                 plotOutput("cert_2", brush = brushOpts("brush_14",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
           #                                 plotOutput("cert_3", brush = brushOpts("brush_15",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
           #                                 plotOutput("cert_4", brush = brushOpts("brush_16",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover"))
           #                                 
           #                          ),
           #                          column(6,
           #                                 plotOutput("cert_5", brush = brushOpts("brush_17",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
           #                                 plotOutput("cert_6", brush = brushOpts("brush_18",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
           #                                 plotOutput("cert_7", brush = brushOpts("brush_19",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
           #                                 plotOutput("cert_8", brush = brushOpts("brush_20",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover"))
           #                                 
           #                          )
           #                        ),
           #                        hr(),
           #                        h4("Selected Drives CERT parameters"),
           #                        div( style = "font-size:x-small",
           #                             tableOutput("cert_sn_table")
           #                        )
           #                        
           #                        )
         )
         
),
   tabPanel("Explore",br(),br(),br(),

            h4("Boxplot"),
            div(style="font-size:x-small",
                splitLayout(
                  uiOutput("MULTI_PARA"),
                  uiOutput("MULTI_ATTR"),
                  uiOutput("MULTI_FACET_1"),
                  uiOutput("MULTI_FACET_2"),
                  uiOutput("MULTI_FACET_3"),
                  uiOutput("MULTI_FACET_4"),
                  uiOutput("MULTI_FACET_5"),
                  selectInput("view_mode","View Mode",c("Dynamic & Fixed Scales","Static & Free Scales"))
                  
                  
                )
            ),
            plotOutput("multi",brush = brushOpts("brush_0",resetOnNew = TRUE),hover = hoverOpts(id ="hover")),            
            div(style="font-size:x-small",helpText("Note: Points selection with this boxplot is precise only to facet 2 currently.")),
            hr(),
            h4("Scatterplot"),
            div(style="font-size:x-small",
                splitLayout(
                  uiOutput("SCATTER_PARA_1"),
                  uiOutput("SCATTER_PARA_2"),
                  uiOutput("SCATTER_FACET_1"),
                  uiOutput("SCATTER_FACET_2"),
                  uiOutput("SCATTER_FACET_3"),
                  selectInput("ellipse","Ellipse",choices = c("NO","YES"))
                  ,br(),br()
                  
                  
                )
            ),
            plotOutput("multi_scatter",brush = brushOpts("brush_00",resetOnNew = TRUE),hover = hoverOpts(id ="hover")),

            splitLayout(
              div(style="font-size:x-small", htmlOutput("multi_hints_1")),
              div(style="font-size:x-small", htmlOutput("multi_hints_2"))
            ),
            hr(),
            h4("Regression"),
            
            fluidRow(
              column(4,uiOutput("regX")),
              column(4,uiOutput("regY"))
            ),
            fluidRow(
              column(6,            verbatimTextOutput("regSummary")),
              column(6,plotOutput("resPlot"))

            )
            
            ),
   tabPanel("Station",br(),br(),br(),
            div(style="font-size:x-small",
                
            selectInput("station","Station:",choices = c("RPI","HSI","CSI"),selected = "RPI"),
                
            dataTableOutput("station_table"),
            helpText("Numbers < 0.05 are highlighted and mean there is a significant difference of means.")
            
            ),

            
            #splitLayout(
              plotOutput("disp_chart_1",brush = brushOpts("brush_d1",resetOnNew = TRUE),hover = hoverOpts(id ="hover"),height="300px"),
              plotOutput("disp_chart_2",brush = brushOpts("brush_d2",resetOnNew = TRUE),hover = hoverOpts(id ="hover"),height="300px"),
              plotOutput("disp_chart_3",brush = brushOpts("brush_d3",resetOnNew = TRUE),hover = hoverOpts(id ="hover"),height="300px"),
             #           ),
#             h4("Conrtibution Analysis"),
#             hr(),
#             #verbatimTextOutput("yyy"),
#             textOutput("station_pvalue_insig",container = tags$b),
#             actionButton("runStationContriRank","Show Rank",class="btn btn-primary btn-sm"),
#             radioButtons("station_breakdown", "Break down by",choices = c("Attribute to Day","Day to Attribute","Just Attribute"),inline = TRUE),
#             helpText("Note: This Rank is not related to outlier selection."),
#             #checkboxInput("station_breakdownbyweek","Break down by day",value = TRUE),
#             uiOutput("station_contri"),
            p()
            
            
            ),

   tabPanel("",br(),br(),br(),
            uiOutput("MULTI_FACET_6"),
            h4("Relative measurements"),
            hr(),
            fluidRow(
              column(6,
                     plotOutput("REL_chart_1",brush = brushOpts("brush_7",resetOnNew = TRUE),hover = hoverOpts(id ="hover"),height="300px"),
                     plotOutput("REL_chart_2",brush = brushOpts("brush_8",resetOnNew = TRUE),hover = hoverOpts(id ="hover"),height="300px"),
                     plotOutput("REL_scatter",brush = brushOpts("brush_9",resetOnNew = TRUE),hover = hoverOpts(id ="hover"),height="300px")
                     
              ),
              column(6,
                     plotOutput("REL_PC_chart_1",brush = brushOpts("brush_10",resetOnNew = TRUE),hover = hoverOpts(id ="hover"),height="300px"),
                     plotOutput("REL_PC_chart_2",brush = brushOpts("brush_11",resetOnNew = TRUE),hover = hoverOpts(id ="hover"),height="300px"),
                     plotOutput("REL_PC_scatter",brush = brushOpts("brush_12",resetOnNew = TRUE),hover = hoverOpts(id ="hover"),height="300px")
                     
              )
              
            ),
            h4("Absolute measurements"),
            hr(),
            
            fluidRow(
              column(6,
                     plotOutput("ABS_chart_1",brush = brushOpts("brush_1",resetOnNew = TRUE),hover = hoverOpts(id ="hover"),height="300px"),
                     plotOutput("ABS_chart_2",brush = brushOpts("brush_2",resetOnNew = TRUE),hover = hoverOpts(id ="hover"),height="300px"),
                     plotOutput("ABS_chart_3",brush = brushOpts("brush_3",resetOnNew = TRUE),hover = hoverOpts(id ="hover"),height="300px")
                     ),
              column(6,
                     plotOutput("ABS_PC_chart_1",brush = brushOpts("brush_4",resetOnNew = TRUE),hover = hoverOpts(id ="hover"),height="300px"),
                     plotOutput("ABS_PC_chart_2",brush = brushOpts("brush_5",resetOnNew = TRUE),hover = hoverOpts(id ="hover"),height="300px"),
                     plotOutput("ABS_PC_chart_3",brush = brushOpts("brush_6",resetOnNew = TRUE),hover = hoverOpts(id ="hover"),height="300px")
                     )
              
              )
            
            
            ),

   tabPanel("",""
            

            # fluidRow(
            #   column(6,
            #          plotOutput("motor_4", brush = brushOpts("brush_21",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
            #          plotOutput("motor_1", brush = brushOpts("brush_24",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
            #          
            #          plotOutput("motor_5", brush = brushOpts("brush_23",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
            #          
            #          plotOutput("motor_7", brush = brushOpts("brush_27",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover"))
            #          
            #   ),
            #   column(6,
            #          plotOutput("motor_3", brush = brushOpts("brush_22",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
            #          
            #          plotOutput("motor_2", brush = brushOpts("brush_25",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover")),
            #          plotOutput("motor_6", brush = brushOpts("brush_26",resetOnNew = TRUE),height="300px",hover = hoverOpts(id ="hover"))
            #          
            #          
            #   )
            # )
            # 

            
            )
   ),

absolutePanel(
  top = 80, right = 50, width = 300,draggable = TRUE,fixed = TRUE,
  splitLayout(
    checkboxInput("hint","Info",value = FALSE),
    checkboxInput("show","List",value = FALSE),

    
#     conditionalPanel("input.hide==0",
#                      actionButton("add1","Add",class="btn btn-primary btn-sm")),
    conditionalPanel("input.show==1",
                     actionButton("reset","Reset",class="btn btn-primary btn-sm")
                     ),
    conditionalPanel("input.show==1",
                     downloadButton("save","Save",class="btn btn-primary btn-sm")
                     )
              ),
    conditionalPanel("input.hint==1",
                     wellPanel(
                       div( style="font-size:x-small",
                       htmlOutput("multi_hints")
                       )
                     )
      ),
    conditionalPanel("input.show==1",
                     wellPanel(
                       div(class="small_text",
                           #textOutput("hover_info"),
                           div( style="font-size:x-small",
                                
                           tags$b("Drive selected:"),
                           textOutput("outlierList")
                           )
                       ),
                       style = "opacity: 0.92"
                     )
    )
  ),
  
  p("")
)
