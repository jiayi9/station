




server <- function(input, session,output) {
  source("global.R",local = TRUE)
  
  source("FUNCTIONS.R",local = TRUE)
  
  source("data.R", local = TRUE)
  
  source("control.R", local = TRUE)  
  
  source("values.R", local = TRUE)

  
  
#   primary_para = c("ARM_Z", "RAMP_Z", "DISC_Z", "RAMP_Z8", "ARM_Z1", 
#                    "ARM_Z2", "ARM_Z3", "ARM_Z4", "ARM_Z5", "DISC_Z6", "DISC_Z7", 
#                    "SUSPENSION_Z9", "SUSPENSION_Z10")
  
#   output$disp_table = DT::renderDataTable({
#     req(input$motor_ramp_ven)
#     
#     FMS = fms()
#     GROUPS = data.frame(RPI = rpi(),  CSI = csi(), HSI =hsi())
#     m = ncol(FMS)
#     n = ncol(GROUPS)
#     
#     R = matrix(1,m,n)
#     R2 = matrix(1,m,n)
#     
#     
#     
# #     R = outer(1:m, 1:n, FUN = function(r,c) {
# #       x =  as.character(GROUPS[,c])
# #       y = FMS[,r]
# #       robust_anova_p(x,y)
# #     })
#     
#     for(i in 1:m){
#       for(j in 1:n){
#         R[i,j] = robust_anova_p(GROUPS[,j],FMS[,i])
#       }
#     }
#     rownames(R) = names(FMS)
#     colnames(R) = names(GROUPS)
#     
#     for(i in 1:m){
#       for(j in 1:n){
#         R2[i,j] = robust_bartlett_p(GROUPS[,j],FMS[,i])
#       }
#     }
# 
#     rownames(R2) = names(FMS)
#     colnames(R2) = names(GROUPS)
# 
#     R = cbind(R,R2)    
#     
#     colnames(R) = c("RPI","CSI","HSI","RPI_2","CSI_2","HSI_2")
# 
#     colfunc <- colorRampPalette(c("red", "white"))
#     colors = colfunc(100)
# 
#     datatable(R, 
#                
#               container = sketch,
#               caption = htmltools::tags$caption(
#                 style = 'caption-side: bottom; text-align: center;',
#                 'Table 1: ', htmltools::em('Summary of statistical tests p values.')
#               ),
#               #selection ='single',
#               options = list(                paging = FALSE,
#                                              ordering = FALSE,
#                                              filtering = FALSE,
#                                              searching =FALSE)
#               )%>% 
#   formatStyle(
#     #paste0("RPI"),
#     c("RPI","HSI","CSI","RPI","RPI_2","HSI_2","CSI_2"),
#     color = styleInterval(seq(0,0.1,length.out = 99), colors)
#   )
# 
#   },server=TRUE)
#   
#   output$selected = renderPrint({
#     input$disp_table_rows_selected
#   })


#   DATA = reactive({
# 
#     data.frame(fms(),RPI = rpi(), CSI = csi(), HSI = hsi(), CLRM_EXIT_DATE = date(),abs(),rel(),ABS_PC(),REL_PC(),cert(),DSN=dsn())
#     
#   })

#   D = reactive({
#     D = DATA() 
#     validate(need(nrow(D)>0,"There are 0 rows in the subsetted data"))
#     validate(need(length(unique(D[,"CLRM_EXIT_DATE"]))>=input$rollN,"Too less weeks, decrease MA rolling basis in 'Trend' tab first."))
# 
#     D
#   })  


# 

# 
#   output$disp_chart_2 = renderPlot({
#     set.seed(123)
#     
#     req(input$disp_table_rows_selected)
#     yname = input$disp_table_rows_selected[length(input$disp_table_rows_selected)]
#         
#     X = D()
#     #print(names(X))
#     if(is.numeric(input$disp_table_rows_selected)) yname = names(X)[input$disp_table_rows_selected]
#     validate(need(!all(is.na(X[,yname])),"All Missing values"))
#     
#     p1 = ggplot(X,aes_string(x = "HSI", y =yname )) + 
#       geom_boxplot(outlier.shape = NA) + 
#       geom_jitter(aes(color=vals$keeprows),width = 0.4,show.legend = FALSE,size = ifelse(vals$keeprows,4,1.5))+
#     scale_color_manual(values = c("black","red"),guide=FALSE)+ theme_bw()
#     
#     
#     p1
#   })
# 
#   output$disp_chart_3 = renderPlot({
#     set.seed(123)
#     
#     req(input$disp_table_rows_selected)
#     yname = input$disp_table_rows_selected[length(input$disp_table_rows_selected)]
# 
#     X = D()
#     
#     if(is.numeric(input$disp_table_rows_selected)) yname = names(X)[input$disp_table_rows_selected]
#     validate(need(!all(is.na(X[,yname])),"All Missing values"))
#     
#     #print(names(X))
#     p1 = ggplot(X,aes_string(x = "CSI", y =yname )) + 
#       geom_boxplot(outlier.shape = NA) + 
#       geom_jitter(aes(color=vals$keeprows),width = 0.4,show.legend = FALSE,size = ifelse(vals$keeprows,4,1.5))+
#     scale_color_manual(values = c("black","red"),guide=FALSE)
#     
#     
#     p1
#   })


  

  source("trend.R",local = TRUE)




#### Outlier vs Non-outlier   




  source("multi.R",local = TRUE)









# 
#   minus = reactive({
#     X = data.frame(fms(),round(comboMinus(fms()),4))    
#     
#     AllNa = sapply(X, function(x) all(is.na(x)))
#     
#     R = X[,!AllNa]
#     
#     R
#     
#   })
# 
#   pca_d = reactive({
#     withProgress(message = "Running PCA",{
# 
#       D = minus()
#       print(dim(D))
#       tsN = 6
#       
#       validate(need(
#         ncol(D) <= nrow(D),
#         "Too less rows for PCA"
#         
#         ))
#       
#       fit = pca_general(D,tsN = tsN)
#       print(dim(fit$TSX))
#       TS = apply(fit$TSX[,1:tsN], 1, sum)
#       
#       CONTRI = fit$contri
#       
#       TS = round(TS,4)
#       
#       R = data.frame(DSN=dsn(),CLRM_EXIT_DATE = date(), TS,D,  CONTRI ,stringsAsFactors = FALSE)
#     })
#     R
#     
#   })
#   
#   output$TS_chart = renderPlot({
#     X = pca_d()
#     ggplot(X, aes(x=CLRM_EXIT_DATE,y = TS)) + geom_boxplot(outlier.shape = NA)+
#       geom_jitter(aes(color=vals$keeprows_TS),width=0.1,size=3) +
#       scale_color_manual(values = c("black","red"),guide=FALSE)+
#       ggtitle("T Square Chart")+
#       ylab("T square value")+
#       geom_hline(yintercept = qchisq(0.9999,6),linetype="dashed",color="red")
#   })
# 
# observeEvent(input$brush_TS,{
#   output$TS_drive_info = DT::renderDataTable({
#     R = pca_d()[vals$keeprows_TS,]
#     datatable(R, 
#               
# #               caption = htmltools::tags$caption(
# #                 style = 'caption-side: bottom; text-align: center;',
# #                 'Table 2: ', htmltools::em('Selected Drive with Big T squares')
# #               ),
#               #selection ='single',
#               options = list(                paging = FALSE,
#                                              ordering = FALSE,
#                                              filtering = FALSE,
#                                              searching =FALSE)
#     )
#   })
#   
#   
#   output$TS_hint = renderText({
#     if(is.na(TS_selected_drive())) {
#       R = "Please select one drive for contribution analysis"
#     } else {
#       R = paste("Contribution analysis for drive",TS_selected_drive())
#     }
#     R
#   })
# 
# })
# 
#   TS_selected_drive =  reactive({pca_d()[vals$keeprows_TS,1][input$TS_drive_info_rows_selected][1]})
# 
#   
#   output$contri_1 = renderPlot({
#     
#     req(pca_d(),TS_selected_drive())
#     
#     X = pca_d()
#     
#     x = X[X$DSN==TS_selected_drive(), 
#           
#           paste0(primary_para,"_CONTRI")
#           ]
#     names(x) = sapply(names(x), function(i) substr(i,1,nchar(i)-7),USE.NAMES = FALSE)
#     contriVBar(x)
#   })
# 
#   output$contri_2 = renderPlot({
#     req(pca_d(),TS_selected_drive())
#     
#     
#     X = pca_d()
#     
#     n = which(names(X)=="SUSPENSION_Z10_CONTRI"  )
#     
#     x = X[X$DSN==TS_selected_drive(), (n+1):ncol(X)]
#     names(x) = sapply(names(x), function(i) substr(i,1,nchar(i)-7),USE.NAMES = FALSE)
#     
#     contriVBar(x,SORT=TRUE)
#   })
# 
#   output$TS_PARA = renderUI({
#     X = pca_d()
#     
#     n1 = which(names(X)=="ARM_Z")
#     n2 = which(names(X)=="SUSPENSION_Z9_SUSPENSION_Z10")
#     Names = names(X)
#     selectInput("ts_para","Parametric", choices = Names[n1:n2])
#   })
# 
#   output$TS_custom_boxplot = renderPlot({
#     req(pca_d(),input$ts_para)
#     yname = input$ts_para
#     X = pca_d()
#     ggplot(X, aes_string(x="CLRM_EXIT_DATE",y = yname)) + geom_boxplot(outlier.shape = NA)+
#       geom_jitter(aes(color=vals$keeprows_TS),width=0.2,size=2) +
#       scale_color_manual(values = c("black","red"),guide=FALSE)+
#       ggtitle(yname)+
#       ylab("value")+
#       theme(axis.text.x = element_text(angle = 90, hjust = 1))
#   })
# 
#   output$minus_table = DT::renderDataTable({
#     pca_d()
#   })
# 
#   output$screeplot = renderPlot({
#     D = minus()
#     validate(need(
#       ncol(D) <= nrow(D),
#       "Too less rows for PCA"
#     ))
#     screeplot(minus())
#   })
# 
#   output$pca_boxplots = renderPlot({
#     pca_boxplots(minus(),date())
#   })
# 





  output$save <- downloadHandler(
    filename = function() { paste('selected_drives_info', '.csv', sep='') },
    content = function(file) {
      write.csv(raw()[vals$keeprows,], file)
    }
  )



  source("downstream.R", local = TRUE)

  source("upstream.R", local = TRUE)


  source("shift.R",local = TRUE)

  source("station.R", local = TRUE)


########### the following is the request from Brian Quinn for a tentative regression analysis

  output$regX = renderUI({
    
    choices = c(multi_Para_list,ATTR_list  )
    
    selectInput("regX","Inputs",choices = choices, multiple = TRUE)
    
  })

  output$regY = renderUI({
    
    choices = c(multi_Para_list)
    
    selectInput("regY","Output",choices = choices)
  })
  
  output$regSummary = renderPrint({
    if(!is.null(input$regX)){
      FORMULA =   paste(input$regY,"~",paste0(input$regX,collapse = "+"))
      D = raw()
      fit = lm(data=D, formula(FORMULA))
      summary(fit)
      
    } else {
      "Select some inputs."
    }
  
  })

  output$resPlot = renderPlot({
    if(!is.null(input$regX)){
      FORMULA =   paste(input$regY,"~",paste0(input$regX,collapse = "+"))
      D = raw()
      fit = lm(data=D, formula(FORMULA))
      plot(resid(fit), main = "Residual Plot", xlab="Index",ylab="Residuals")
      abline(h = 0,lty=2,col="red")
      
    } else {
      NULL
    }
  })


}