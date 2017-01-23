#


library(dplyr)

tb_RPI = reactive({
  D = data.frame(raw()[,FIVE] , RPI = rpi() , LINE = line())
  tb = D %>% group_by(LINE) %>% dplyr::summarise(
    Ramp_To_Disc = robust_anova_p(RPI,RAMP_TO_DISC),
    Ramp_Z = robust_anova_p(RPI,RAMP_Z),
    Disc_Z = robust_anova_p(RPI,DISC_Z),
    Arm_To_Disc = robust_anova_p(RPI,ARM_TO_DISC),
    Arm_Z = robust_anova_p(RPI,ARM_Z)
    )
  R = t(tb)
  colnames(R) = R[1,]
  R = R[-1,,drop=FALSE]
  R
})

tb_CSI = reactive({
  D = data.frame(raw()[,FIVE] , CSI = csi() , LINE = line())
  tb = D %>% group_by(LINE) %>% dplyr::summarise(
    Ramp_To_Disc = robust_anova_p(CSI,RAMP_TO_DISC),
    Ramp_Z = robust_anova_p(CSI,RAMP_Z),
    Disc_Z = robust_anova_p(CSI,DISC_Z),
    Arm_To_Disc = robust_anova_p(CSI,ARM_TO_DISC),
    Arm_Z = robust_anova_p(CSI,ARM_Z)
  )
  R = t(tb)
  colnames(R) = R[1,]
  R = R[-1,,drop=FALSE]
})

tb_HSI = reactive({
  D = data.frame(raw()[,FIVE] , HSI = hsi() , LINE = line())
  tb = D %>% group_by(LINE) %>% dplyr::summarise(
    Ramp_To_Disc = robust_anova_p(HSI,RAMP_TO_DISC),
    Ramp_Z = robust_anova_p(HSI,RAMP_Z),
    Disc_Z = robust_anova_p(HSI,DISC_Z),
    Arm_To_Disc = robust_anova_p(HSI,ARM_TO_DISC),
    Arm_Z = robust_anova_p(HSI,ARM_Z)
  )
  R = t(tb)
  colnames(R) = R[1,]
  R = R[-1,,drop=FALSE]
})


output$station_table = renderDataTable({
  
  R = if(input$station=="RPI"){tb_RPI()} else if(input$station=="CSI") {tb_CSI()} else {tb_HSI()}
 # R = as.numeric(R)
 # R = round(R,2)
 class(R) = "numeric"
 R = round(R,2)
  datatable(R, 
            
            caption = htmltools::tags$caption(
              style = 'caption-side: bottom; text-align: center;',
              'Table 1: ', htmltools::em('Summary of ANOVA p values')
            ),
#            selection =list(target ='cell',mode="single"),
            

            
            options = list(                paging = FALSE,
                                           ordering = FALSE,
                                           filtering = FALSE,
                                           searching =FALSE,
                                           info = FALSE)
  )%>% 
    formatStyle(
      colnames(R),
      color = styleInterval(cuts = c(0.05),values = c("red","black"))
    )
})

selected_para = reactive({ 
#   req(input$station_table_cells_selected)
#   toupper(rownames(tb_RPI())[input$station_table_cells_selected[1,1]])  
#  print(input$station_table_rows_selected)
  req(input$station_table_rows_selected)
  if( is.numeric(input$station_table_rows_selected)){
    R = toupper(rownames(tb_RPI())[input$station_table_rows_selected[1]])  
  } else {
    R = toupper(input$station_table_rows_selected[1])
  }
  
  print(R)
  R
})

selected_line = reactive({ 
  req(input$station_table_cells_selected)
  colnames(tb_RPI())[input$station_table_cells_selected[1,2]]  
  })

selected_data = reactive({
  raw()[line() == selected_line(),]
})


station_p_table = reactive({
  withProgress(message = "Ranking attributes for station analysis",{
    xname = if(input$station=="RPI"){"RPI_ID"} else if(input$station=="C") {"CLAMP_SCREW_ID"} else {"HD_STACK_ID"}
    
    X = selected_data()[,setdiff(contri_ATTR_list,xname)]
    X[] = lapply(X,as.character)
    
    y = if(input$station=="RPI"){rpi()[line() == selected_line()]} else if(input$station=="CSI") {csi()[line() == selected_line()]} else {hsi()[line() == selected_line()]}
    #  print(y)
    
    p_table = sort(sapply(X, function(x) robust_chisq_p(x,y)))
  })
  print(dim(X))
  print(table(y))
  p_table  
})

station_ranked_attr = reactive({
  names(station_p_table())[station_p_table()<0.05]
})

output$station_pvalue_insig = renderText({
  p_table = sort(station_p_table())
  if (all(p_table>=0.05)){
    "All attributes have no significant contribution: all p values of chisq tests >= 0.05"
  } else {
    paste("There are", sum(p_table<0.05), 
          "signifcant contributing attributes:", 
          paste(names(p_table)[p_table<0.05],collapse = ", "),
          "to the selected station:", input$station,".   ")
  }
})

output$yyy = renderPrint({
  list(input$station_table_cells_selected,   
       colnames(tb_RPI())[input$station_table_cells_selected[1,2]],
       rownames(tb_RPI())[input$station_table_cells_selected[1,1]],
       station_p_table()
  )
})



observeEvent(input$runStationContriRank,{
  withProgress(message = "Plotting Top attributes contirbuting to outliers",{
    
    NAMES = station_ranked_attr()
    ROWS = length(NAMES)
    
    yname = selected_para()
    #xname = if(input$station=="RPI"){"RPI_ID"} else if(input$station=="C") {"CLAMP_SCREW_ID"} else {"HD_STACK_ID"}
    
    output$station_contri = renderUI({
      if (ROWS>0) {
        table_output_list = lapply(1:ROWS,function(i){
          
          name = paste("row2",i,sep="")
          tags$div(class = "group-output",
                   plotOutput(name),
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
    station = if(input$station=="RPI"){rpi()[line() == selected_line()]} else if(input$station=="CSI") {csi()[line() == selected_line()]} else {hsi()[line() == selected_line()]}
    
    if(ROWS>0){
      for(j in 1:ROWS){
        local({
          my_i = j
          
          name = paste("row2",my_i,sep="")
          
          output[[name]] = renderPlot({
            
            xname = NAMES[my_i]
            x = raw()[line()==selected_line(),xname]
            set.seed(123)
            
            
#             p = ggplot(D,aes_string(x = xname, y = yname )) + 
#               geom_boxplot(outlier.shape = NA) + 
#               geom_jitter(aes(color=selected,size=selected),width = 0.2,show.legend = FALSE)+
#               scale_color_manual(values = c("black","red"),guide=FALSE)+
#               theme(legend.position="none",
#                     axis.text.x = element_text(angle = 90, hjust = 0),
#                     plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
#                     
#               )+
#               ggtitle( paste(yname,"  >  ",input$station, "--------",xname,"     >   chisq_P_value = ", round(robust_chisq_p(station,x),4)))
#             
#             if(input$station_breakdownbyweek){
#               p = p + facet_grid(.~CLRM_EXIT_DATE,scales = "free",space = "free")
#             }
            
            
            
            
            
            
            
            if(input$station_breakdown =="Day to Attribute"){
              
              p = ggplot(D,aes_string(x = xname, y = yname )) + 
                geom_boxplot(outlier.shape = NA) + 
                geom_jitter(aes(color=selected,size=selected),width = 0.2,show.legend = FALSE)+
                scale_color_manual(values = c("black","red"),guide=FALSE)+
                theme(legend.position="none",
                      axis.text.x = element_text(angle = 90, hjust = 0),
                      plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
                ) + facet_grid(reformulate(BY(),response = "."),scales = "free",space = "free")+
                ggtitle( paste(yname,"  >  ",input$station, "--------",xname,"     >   chisq_P_value = ", round(robust_chisq_p(station,x),4)))
              
              
            } else if(input$station_breakdown == "Attribute to Day") {
              
              p = ggplot(D,aes_string(x = BY(), y = yname )) + 
                geom_boxplot(outlier.shape = NA) + 
                geom_jitter(aes(color=selected,size=selected),width = 0.2,show.legend = FALSE)+
                scale_color_manual(values = c("black","red"),guide=FALSE)+
                theme(legend.position="none",
                      axis.text.x = element_text(angle = 90, hjust = 0),
                      plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
                ) + facet_grid(reformulate(xname,response="."),scales = "free",space = "free")+
                ggtitle( paste(yname,"  >  ",input$station, "--------",xname,"     >   chisq_P_value = ", round(robust_chisq_p(station,x),4)))
              
              
            } else {
              p = ggplot(D,aes_string(x = xname, y = yname )) + 
                geom_boxplot(outlier.shape = NA) + 
                geom_jitter(aes(color=selected,size=selected),width = 0.2,show.legend = FALSE)+
                scale_color_manual(values = c("black","red"),guide=FALSE)+
                theme(legend.position="none",
                      axis.text.x = element_text(angle = 90, hjust = 0),
                      plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
                ) +
                ggtitle( paste(yname,"  >  ",input$station, "--------",xname,"     >   chisq_P_value = ", round(robust_chisq_p(station,x),4)))
              
            }
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            p
          })
        })
      }
    }
    
  })
})


output$disp_chart_1 = renderPlot({
  withProgress(message = "Drawing station charts",{
  set.seed(123)
  yname = selected_para()
  X = raw()
  X$selected = vals$keeprows
  
  print(names(X))
  print(yname)
  
  validate(need(!all(is.na(X[,yname])),"All Missing values"))
  p1 = ggplot(X,aes_string(x = "RPI_ID", y =yname )) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(aes(color=selected,size=selected),width = 0.2,show.legend = FALSE)+
    scale_color_manual(values = c("black","red"),guide=FALSE)+
#    facet_grid(.~LINE_NUM)+
    facet_grid(.~LINE_NUM,space = "free",scales = "free")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.y=element_blank())
  p1
  })
})
output$disp_chart_2 = renderPlot({
  withProgress(message = "Drawing station charts",{
    
  set.seed(123)
  yname = selected_para()
  X = raw()
  X$selected = vals$keeprows
  validate(need(!all(is.na(X[,yname])),"All Missing values"))
  p1 = ggplot(X,aes_string(x = "CLAMP_SCREW_ID", y =yname )) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(aes(color=selected,size=selected),width = 0.2,show.legend = FALSE)+
    scale_color_manual(values = c("black","red"),guide=FALSE)+
#    facet_grid(.~LINE_NUM)+
    facet_grid(.~LINE_NUM,space = "free",scales = "free")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.y=element_blank())
  p1
  })
})
output$disp_chart_3 = renderPlot({
  withProgress(message = "Drawing station charts",{
    
  set.seed(123)
  yname = selected_para()
  X = raw()
  X$selected = vals$keeprows
  validate(need(!all(is.na(X[,yname])),"All Missing values"))
  p1 = ggplot(X,aes_string(x = "HD_STACK_ID", y =yname )) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(aes(color=selected,size=selected),width = 0.2,show.legend = FALSE)+
    scale_color_manual(values = c("black","red"),guide=FALSE)+
#    facet_grid(.~LINE_NUM)+
    facet_grid(.~LINE_NUM,space = "free",scales = "free")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.y=element_blank())
  p1
  })
})