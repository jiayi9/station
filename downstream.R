####### pre2  fail code

output$FAIL_CODE_DEFINE = renderUI({
  choices = sort(setdiff(unique(raw()$FAIL_CODE),"N/A"))
  default_values = c("48400","14873","11164")
  defaults = default_values[ default_values %in% choices]
  validate(need(length(choices)>0,"No Fail Codes in the data selected."))
  selectInput("fail_code_define","Define Fails",choices = choices,selected = defaults,multiple = TRUE)
})

ERR_CODE = reactive({
  R = raw()$FAIL_CODE
  
  validate(need(  length(input$fail_code_define)>0 , "Define at least one fail code."   ))
  
 # print(R)
  R = ifelse(R %in% input$fail_code_define,"FAIL","PASS")
  print("FAIL CODE")
  print(table(R))
  cat("-----------------------------\n")
  
  R
  
})

DSN_ERR_CODE = reactive({
  req(vals$DSN)
  
  Drives = ifelse(dsn() %in% vals$DSN,"Outliers","Non-outliers")
  
  data.frame( Drives, PRE2_FAIL = ERR_CODE())
})

output$compare = renderTable({
  
  validate(need(  length(vals$DSN)>0 , "Select some drives to analyze."   ))
  
  X = table(DSN_ERR_CODE())
  
  R = matrix("",2,3)
  colnames(R) = c("FAIL","PASS","FAIL RATE")
  rownames(R) = c("Other drives","Selected drives")
  R[1:2,1:2] = as.character(round(X))
  R[1,3] = as.character(round(X[1,1]/X[1,2],4))
  R[2,3] = as.character(round(X[2,1]/X[2,2],4))
  
  R
  
},include.rownames=TRUE)

output$ref_table = renderDataTable({
  
  R = RAW()[RAW()$DRIVE_SERIAL_NUM %in% vals$DSN ,]
  
  R = R[order(R[,1]),]
  
  datatable(R, 
            
            caption = htmltools::tags$caption(
              style = 'caption-side: bottom; text-align: center;',
              'Table 2: ', htmltools::em('Data of selected drives')
            ),
            #selection ='single',
            options = list(                paging = FALSE,
                                           ordering = TRUE,
                                           filtering = FALSE,
                                           searching =FALSE,
                                           info=FALSE)
  )
  
  
})


#######  cert
# 
# output$cert_1 = renderPlot({
#   set.seed(123);D = raw();D$selected = vals$keeprows
#   boxplot_MA(D ,xname = "CLRM_EXIT_DATE", yname = "H1_RAMP_CYL",TT = "H1_RAMP_CYL",rollN = input$rollN)
# })
# 
# output$cert_2 = renderPlot({
#   set.seed(123);D = raw();D$selected = vals$keeprows
#   boxplot_MA(D,xname = "CLRM_EXIT_DATE", yname = "H1_CRASH_STOP_CYL",TT = "H1_CRASH_STOP_CYL",rollN = input$rollN)
# })
# 
# output$cert_3 = renderPlot({
#   set.seed(123);D = raw();D$selected = vals$keeprows
#   boxplot_MA(D ,xname = "CLRM_EXIT_DATE", yname = "H1_TRK_0_CYL",TT = "H1_TRK_0_CYL",rollN = input$rollN)
# })
# 
# output$cert_4 = renderPlot({
#   set.seed(123);D = raw();D$selected = vals$keeprows
#   boxplot_MA(D ,xname = "CLRM_EXIT_DATE", yname = "H1_PHYS_MAX_CYL",TT = "H1_PHYS_MAX_CYL",rollN = input$rollN)
# })
# 
# output$cert_5 = renderPlot({
#   set.seed(123);D = raw();D$selected = vals$keeprows
#   boxplot_MA(D ,xname = "CLRM_EXIT_DATE", yname = "H0_RAMP_CYL",TT = "H0_RAMP_CYL",rollN = input$rollN)
# })
# 
# output$cert_6 = renderPlot({
#   set.seed(123);D = raw();D$selected = vals$keeprows
#   boxplot_MA(D ,xname = "CLRM_EXIT_DATE", yname = "H0_CRASH_STOP_CYL",TT = "H0_CRASH_STOP_CYL",rollN = input$rollN)
# })
# 
# output$cert_7 = renderPlot({
#   set.seed(123);D = raw();D$selected = vals$keeprows
#   boxplot_MA(D ,xname = "CLRM_EXIT_DATE", yname = "H0_TRK_0_CYL",TT = "H0_TRK_0_CYL",rollN = input$rollN)
# })
# 
# output$cert_8 = renderPlot({
#   set.seed(123);D = raw();D$selected = vals$keeprows
#   boxplot_MA(D ,xname = "CLRM_EXIT_DATE", yname = "H0_PHYS_MAX_CYL",TT = "H0_PHYS_MAX_CYL",rollN = input$rollN)
# })

output$cert_sn_table = renderTable({
  D = data.frame(SERIAL_NUM=dsn(),raw())
  D[dsn() %in% vals$DSN, c("SERIAL_NUM",CERT_list)]
})
