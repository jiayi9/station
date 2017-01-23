

shift_table = reactive({
  
  # the following line prevents 'Error: arguments imply differing number of rows: xxxx, xxxx'
  validate(need(  nrow(fms()) == length(date())   ,""))
  
  D = fms()
  X = data.frame(D,DATE = as.character(date()))
  MEANS = sapply(D, function(x) mean(x,na.rm = TRUE))  
  # print(MEANS)
  
  library(dplyr)
  
  
  X2 = X %>% group_by(DATE) %>% dplyr::summarise(
    RAMP_TO_DISC = mean(RAMP_TO_DISC,na.rm = TRUE),
    RAMP_Z = mean(RAMP_Z,na.rm = TRUE),
    DISC_Z = mean(DISC_Z,na.rm = TRUE),
    ARM_TO_DISC = mean(ARM_TO_DISC, na.rm = TRUE),
    ARM_Z = mean(ARM_Z, na.rm = TRUE)
    )
  
  X3 = round(data.frame(sapply(X2[,-1], function(x)  (x-mean(x,na.rm = TRUE))/sd(x,na.rm = TRUE) )),2)

  if(input$by=="Day"){
    rownames(X3) = paste(substr(X2$DATE,3,4),'',substr(X2$DATE,5,6),'',substr(X2$DATE,7,8))
  } else {
    rownames(X3) = paste(substr(X2$DATE,3,7))#,'',substr(X2$DATE,5,6),'',substr(X2$DATE,7,8))
  }
 X3
 
})


output$shift_table = renderDataTable({
  

  
  
  
  R =  t(shift_table())
  
  colfunc <- colorRampPalette(c("red", "white"))
  colors = colfunc(100)
  
  datatable(R, 
            
            caption = htmltools::tags$caption(
              style = 'caption-side: bottom; text-align: center;',
              'Table 1: ', htmltools::em('Daily/Weekly Z-scores of Means')
            ),
            selection ='single',
            options = list(                paging = FALSE,
                                           ordering = FALSE,
                                           filtering = FALSE,
                                           searching =FALSE,
                                           info=FALSE)
  )%>% 
    formatStyle(
      #paste0("RPI"),
      #c("RPI","HSI","CSI","RPI","RPI_2","HSI_2","CSI_2"),
      colnames(R),
      color = styleInterval(c(-3,-2.5,2.5,3),c("red","red","lightgrey","red","red"))
                            
    )
})



output$shift_click = renderPlot({
  set.seed(123)
  
  req(input$shift_table_rows_selected)
  yname = input$shift_table_rows_selected[length(input$shift_table_rows_selected)]
  
  print(input$shift_table_rows_selected)
  print(yname)
  
  
  
  X = raw()
  
  
  
  X$selected = vals$keeprows
  
  validate(need(length(unique(date()))>input$rollN,"Too less weeks for MA chart. Try decreasing MA rolling basis."))
  
  if(is.numeric(input$shift_table_rows_selected)) yname = names(fms())[yname]
  
  print(yname)
  
  validate(need(!all(is.na(fms()[,yname])),"All Missing values"))
  
  xname = BY()
  
  
  p1 = ggplot(X,aes_string(x = xname, y =yname )) + 
    theme_bw() +
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(aes(color=selected,size=selected),width = 0.2,show.legend = FALSE)+
    scale_color_manual(values = c("black","red"),guide=FALSE)+
    theme(legend.position="none",
          axis.text.x = element_text(angle = 90, hjust = 0),
          plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
    )
  
  if(yname=="RAMP_TO_DISC"){
    if('1000' %in% input$capacity & !('2000' %in% input$capacity)){
      boxplot_MA(X, xname = xname, yname = yname, TT = yname, rollN = input$rollN, ref = c(0.62,0.85),theme_BW = TRUE)
      
    } else if( !('1000' %in% input$capacity) & ('2000' %in% input$capacity)){
      boxplot_MA(X, xname = xname, yname = yname, TT = yname, rollN = input$rollN, ref2=c(0.49,0.72),theme_BW = TRUE)
      
    } else{
      boxplot_MA(X, xname = xname, yname = yname, TT = yname, rollN = input$rollN, ref = c(0.62,0.85),ref2=c(0.49,0.72),theme_BW = TRUE)
      
    }
    
    
    
  } else {
    boxplot_MA(X, xname = xname, yname = yname, TT = yname ,rollN = input$rollN,theme_BW = TRUE)
  }

})



output$shift_contri_info = renderText({
  if(  length(vals$DSN) == 0){
    R = "Please FIRST select outliers AND THEN click Show Rank >"
  } else {
    R = paste("You have selected",length(vals$DSN),"Drives.")
  }
  R
})


outlier_p_table = eventReactive(input$runOutlierContriRank,{
  withProgress(message = "Ranking attributes for outlier analysis",{
  X = raw()[,contri_ATTR_list]
  X[] = lapply(X,as.character)
  
  y = as.character(dsn() %in% vals$DSN)
#  print(y)
  
  p_table = sort(sapply(X, function(x) robust_chisq_p(x,y)))
  })
  p_table  
})

outlier_ranked_attr = reactive({
  names(outlier_p_table())[outlier_p_table()<0.05]
})


output$LL = renderPrint({
  outlier_ranked_attr()
})


observeEvent(input$runOutlierContriRank,{
  withProgress(message = "Plotting Top 10 attributes contirbuting to outliers",{
    
  NAMES = outlier_ranked_attr()
  ROWS = length(NAMES)
  
  ROWS = min(20, ROWS)
  
  if(is.numeric(input$shift_table_rows_selected)) {
    yname = names(fms())[input$shift_table_rows_selected]
  } else {
    yname = input$shift_table_rows_selected
  }
  
  output$outlier_contri = renderUI({
    if (ROWS>0) {
      table_output_list = lapply(1:ROWS,function(i){
        
        name = paste("row",i,sep="")
        tags$div(class = "group-output",
                 plotOutput(name,height="300px"),
                 br()  
        )
      })
      do.call(tagList,table_output_list)
    } else{
      NULL
    }
  })
  validate(need(nrow(raw())>0,""))
  D = raw()
  D$selected=vals$keeprows
  
  if(ROWS>0){
    for(j in 1:ROWS){
      local({
        my_i = j
        
        name = paste("row",my_i,sep="")
        
        output[[name]] = renderPlot({
          
          xname = NAMES[my_i]
          x = raw()[,xname]
          set.seed(123)
          #radioButtons("shift_breakdown", "Break down by",choices = c("Attribute to Day","Day to attribute","Just Attribute"))
          
          if(input$shift_breakdown =="Day to Attribute"){
            
            p = ggplot(D,aes_string(x = xname, y = yname )) + 
              theme_bw() +
              geom_boxplot(outlier.shape = NA) + 
              geom_jitter(aes(color=selected,size=selected),width = 0.2,show.legend = FALSE)+
              scale_color_manual(values = c("black","red"),guide=FALSE)+
              theme(legend.position="none",
                    axis.text.x = element_text(angle = 90, hjust = 0),
                    plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
#              ) + facet_grid(.~CLRM_EXIT_DATE,scales = "free",space = "free")+
              ) + facet_grid( reformulate(BY(),response = "."),scales = "free",space = "free")+

              ggtitle( paste(yname,"--------",xname, "         P value = ", round(robust_chisq_p(as.character(vals$keeprows),x),4)))
             
          
          } else if(input$shift_breakdown == "Attribute to Day") {
            
            p = ggplot(D,aes_string(x = BY(), y = yname )) + 
              theme_bw() +
              geom_boxplot(outlier.shape = NA) + 
              geom_jitter(aes(color=selected,size=selected),width = 0.2,show.legend = FALSE)+
              scale_color_manual(values = c("black","red"),guide=FALSE)+
              theme(legend.position="none",
                    axis.text.x = element_text(angle = 90, hjust = 0),
                    plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
              ) + facet_grid(reformulate(xname,response="."),scales = "free",space = "free")+
            ggtitle( paste(yname,"--------",xname, "         P value = ", round(robust_chisq_p(as.character(vals$keeprows),x),4)))
            
            
          } else {
            p = ggplot(D,aes_string(x = xname, y = yname )) + 
              theme_bw() +
              geom_boxplot(outlier.shape = NA) + 
              geom_jitter(aes(color=selected,size=selected),width = 0.2,show.legend = FALSE)+
              scale_color_manual(values = c("black","red"),guide=FALSE)+
              theme(legend.position="none",
                    axis.text.x = element_text(angle = 90, hjust = 0),
                    plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
              ) +
              ggtitle( paste(yname,"--------",xname, "         P value = ", round(robust_chisq_p(as.character(vals$keeprows),x),4)))
            
          }
          
          

          p
        })
      })
    }
  }
  
  })
})


output$shift_pvalue_insig = renderText({
  p_table = sort(outlier_p_table())
  if (all(p_table>=0.05)){
    "All attributes have no significant contribution: all p values of ANOVA tests >= 0.05"
  } else {
    paste("There are", sum(p_table<0.05), "signifcant contributing attributes:", paste(names(p_table)[p_table<0.05],collapse = ", ") )
  }
})


















# 
# p_table = reactive({
#   X = raw()[,contri_ATTR_list]
#   X[] = lapply(X,as.character)
#   if(is.numeric(input$shift_table_rows_selected)) {
#     yname = names(fms())[input$shift_table_rows_selected]
#   } else {
#     yname = input$shift_table_rows_selected
#   }
#   
#   y = raw()[,yname]
#   #   print(head(X))
#   #   print(y)
#   
#   sort(sapply(X, function(x) robust_anova_p(x,y)))
#   
# })











# contri_X = reactive({
#   R = raw()[,   names(p_table())[p_table()<0.05]     ]
#   print(names(R))
#   R
# })

# 
# observe({
#   X = contri_X()
#   ROWS = ncol(X)
#   if(is.numeric(input$shift_table_rows_selected)) {
#     yname = names(fms())[input$shift_table_rows_selected]
#   } else {
#     yname = input$shift_table_rows_selected
#   }
#   y = raw()[,yname]
#   
#   output$contri = renderUI({
#     if (ROWS>0) {
#       table_output_list = lapply(1:ROWS,function(i){
#         
#         name = paste("row",i,sep="")
#         tags$div(class = "group-output",
#                  plotOutput(name,height="300px"),
#                  br()  
#         )
#       })
#       do.call(tagList,table_output_list)
#     } else{
#       NULL
#     }
#   })
#   validate(need(nrow(raw())>0,""))
#   D = raw()
#   D$selected=vals$keeprows
#   
#   if(ROWS>0){
#     for(j in 1:ROWS){
#       local({
#         my_i = j
#         
#         name = paste("row",my_i,sep="")
#         
#         output[[name]] = renderPlot({
#           
#           x = X[,my_i]
#           xname = names(X)[my_i]
#           
#           set.seed(123)
#           
#           
#           ggplot(D,aes_string(x = xname, y = yname )) + 
#             geom_boxplot(outlier.shape = NA) + 
#             geom_jitter(aes(color=selected),width = 0.4,show.legend = FALSE,size = ifelse(vals$keeprows,4,1.5))+
#             scale_color_manual(values = c("black","red"),guide=FALSE)+
#             theme(legend.position="none",
#                   axis.text.x = element_text(angle = 90, hjust = 0),
#                   plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
#             )+facet_grid(.~CLRM_EXIT_DATE,scales = "free",space = "free")+
#             ggtitle( paste(yname,"-----",xname, "----- P value = ", robust_anova_p(x,y)))
# 
#           
#         })
#       })
#     }
#   }
#   
#   
# })
# 
# 
# output$L= renderText({
#   p_table()
# })