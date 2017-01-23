
#################  ABS Trend Shift  ##############



output$ABS_PC_chart_1 = renderPlot({
  withProgress(message = "Drawing Trend charts 1",{
  set.seed(123);D = raw();D$selected = vals$keeprows
  yname = "ABS_PC_1"
  TT = "1st Principal Component"
  validate(need(length(unique(D[,BY()]))>=input$rollN,"Too less dates, decrease MA rolling basis"))  
  boxplot_MA(D = D,xname = BY(), yname = yname, TT = TT,rollN = input$rollN,ref = NA)
})})

output$ABS_PC_chart_2 = renderPlot({
  withProgress(message = "Drawing Trend charts 2",{
    
  set.seed(123);D = raw();D$selected = vals$keeprows
  yname = "ABS_PC_2"
  TT = "2nd Principal Component"  
  boxplot_MA(D = D,xname = BY(), yname = yname, TT = TT,rollN = input$rollN,ref = NA)
})})

output$ABS_PC_chart_3 = renderPlot({    
  withProgress(message = "Drawing Trend charts 3",{
    
  set.seed(123);D = raw();D$selected = vals$keeprows
  yname = "ABS_PC_3"
  TT = "3rd Principal Component"
  boxplot_MA(D = D,xname = BY(), yname = yname, TT = TT,rollN = input$rollN,ref = NA)
})})


output$ABS_chart_1 = renderPlot({
  withProgress(message = "Drawing Trend charts 4",{
    
  set.seed(123);D = raw();D$selected = vals$keeprows
  yname = "ARM_Z"
  boxplot_MA(D = D,xname = BY(), yname = yname, TT = yname,rollN = input$rollN,ref = NA, theme_BW = TRUE)
})})

output$ABS_chart_2 = renderPlot({
  withProgress(message = "Drawing Trend charts 5",{
    
  set.seed(123);D = raw();D$selected = vals$keeprows
  yname = "RAMP_Z"
  boxplot_MA(D = D,xname = BY(), yname = yname, TT = yname,rollN = input$rollN,ref = NA, theme_BW = TRUE)
})})

output$ABS_chart_3 = renderPlot({
  withProgress(message = "Drawing Trend charts 6",{
    
  set.seed(123);D = raw();D$selected = vals$keeprows
  yname = "DISC_Z"
  boxplot_MA(D = D,xname = BY(), yname = yname, TT = yname,rollN = input$rollN,ref = NA, theme_BW = TRUE)
})})





#################  REL Trend Shift  ##############


output$REL_PC_chart_1 = renderPlot({
  withProgress(message = "Drawing Trend charts 7",{
    
  set.seed(123);D = raw();D$selected = vals$keeprows
  yname = "REL_PC_1"
  TT = "1st Principal Component"
  boxplot_MA(D = D,xname = BY(), yname = yname,TT = TT,rollN = input$rollN,ref = NA , theme_BW = TRUE)
})})

output$REL_PC_chart_2 = renderPlot({
  withProgress(message = "Drawing Trend charts 8",{
    
  set.seed(123);D = raw();D$selected = vals$keeprows
  yname = "REL_PC_2"
  TT = "2nd Principal Component"
  boxplot_MA(D = D,xname = BY(), yname = yname, TT = TT,rollN = input$rollN,ref = NA, theme_BW = TRUE)
})})


output$REL_chart_1 = renderPlot({
  withProgress(message = "Drawing Trend charts 9",{
    
  set.seed(123);D = raw();D$selected = vals$keeprows
  yname = "ARM_TO_DISC"
  TT =yname  
  boxplot_MA(D = D,xname = BY(), yname = yname, TT = TT,rollN = input$rollN,ref = NA)
})})

output$REL_chart_2 = renderPlot({
  withProgress(message = "Drawing Trend charts 10 ",{
    
  set.seed(123);D = raw();D$selected = vals$keeprows
  yname = "RAMP_TO_DISC"
  TT =yname  
  boxplot_MA(D = D,xname = BY(), yname = yname, TT = TT,rollN = input$rollN,ref = NA)
})})

#### REL scatter
output$REL_PC_scatter = renderPlot({
  withProgress(message = "Drawing Trend charts 11",{
    
  set.seed(123);D = raw();D$selected = vals$keeprows
  ggplot(D,aes_string(y="REL_PC_1",x="REL_PC_2"))+ theme_bw()+
    geom_point(aes(color=vals$keeprows),show.legend = FALSE,size = ifelse(vals$keeprows,4,1.5))+
    scale_color_manual(values = c("black","red"),guide=FALSE)+
    ggtitle("REL_PC_1 vs REL_PC_2")+
    stat_ellipse(level = 0.999,col="red",linetype="dashed")+
    theme(
      plot.title = element_text( colour = "blue",size=ggplot2::rel(1.5))
    )
})})

output$REL_scatter = renderPlot({
  withProgress(message = "Drawing Trend charts 12",{
    
  set.seed(123);D = raw();D$selected = vals$keeprows
  ggplot(D,aes_string(y="ARM_TO_DISC",x="RAMP_TO_DISC"))+
    geom_point(aes(color=vals$keeprows),show.legend = FALSE,size = ifelse(vals$keeprows,4,1.5))+
    scale_color_manual(values = c("black","red"),guide=FALSE)+
    ggtitle("ARM_TO_DISC vs RAMP_TO_DISC")+
    stat_ellipse(level = 0.999,col="red",linetype="dashed")+
    theme(
      plot.title = element_text( colour = "blue",size=ggplot2::rel(1.5))
    )
})})


