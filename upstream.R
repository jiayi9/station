
output$motor_1 = renderPlot({
  withProgress(message = "Drawing Upstream charts 1",{
    
  set.seed(123);D=raw();D$selected = vals$keeprows
  tryCatch({
  boxplot_MA(D ,xname = BY(), yname = "FLANGE_HEIGHT_AVG",TT = "FLANGE_HEIGHT_AVG",rollN = input$rollN,theme_BW=1)
  },error=function(e){NULL},warning=function(w){NULL})
  })
})

output$motor_2 = renderPlot({
  withProgress(message = "Drawing Upstream charts 2",{
    
  set.seed(123);D=raw();D$selected = vals$keeprows
  tryCatch({
  boxplot_MA(D ,xname = BY(), yname = "FLANGE_HEIGHT_PARALLEL",TT = "FLANGE_HEIGHT_PARALLEL",rollN = input$rollN,theme_BW=1)
  },error=function(e){NULL},warning=function(w){NULL})

})})

output$motor_3 = renderPlot({
  withProgress(message = "Drawing FMS charts 3",{
    
  yname = "ARM_TO_DISC"
  set.seed(123);D=raw();D$selected = vals$keeprows
  tryCatch({
  TT =yname  
  boxplot_MA(D ,xname = BY(), yname = yname, TT = TT,rollN = input$rollN,ref = NA)
  },error=function(e){NULL},warning=function(w){NULL})
  
})})

output$motor_4 = renderPlot({
  withProgress(message = "Drawing FMS charts 4",{
    
  set.seed(123);D=raw();D$selected = vals$keeprows

  yname = "RAMP_TO_DISC"
  TT =yname  
  xname = BY()
  
  tryCatch({
#  boxplot_MA(D ,xname = BY(), yname = yname, TT = TT,rollN = input$rollN,ref = c(0.62,0.85),ref2=c(0.49,0.72))
    
    if(yname=="RAMP_TO_DISC"){
      if('1000' %in% input$capacity & !('2000' %in% input$capacity)){
        boxplot_MA(D, xname = xname, yname = yname, TT = yname, rollN = input$rollN, ref = c(0.62,0.85))
        
      } else if( !('1000' %in% input$capacity) & ('2000' %in% input$capacity)){
        boxplot_MA(D, xname = xname, yname = yname, TT = yname, rollN = input$rollN, ref2=c(0.49,0.72))
        
      } else{
        boxplot_MA(D, xname = xname, yname = yname, TT = yname, rollN = input$rollN, ref = c(0.62,0.85),ref2=c(0.49,0.72))
        
      }
      
      
      
    } else {
      boxplot_MA(D, xname = xname, yname = yname, TT = yname ,rollN = input$rollN)
    }
    
    
  },error=function(e){NULL},warning=function(w){NULL})

})})


output$motor_5 = renderPlot({
  withProgress(message = "Drawing Downstream charts 5",{
    
  set.seed(123);D=raw();D$selected = vals$keeprows
  tryCatch({
  boxplot_MA(D ,xname = BY(), yname = "H0_RAMP_CYL",TT = "H0_RAMP_CYL",rollN = input$rollN,bgcolor=1)
  },error=function(e){NULL},warning=function(w){NULL})

})})

output$motor_6 = renderPlot({
  withProgress(message = "Drawing Downstream charts 6",{
    
  set.seed(123);D=raw();D$selected = vals$keeprows
  tryCatch({
  boxplot_MA(D ,xname = BY(), yname = "H1_RAMP_CYL",TT = "H1_RAMP_CYL",rollN = input$rollN,bgcolor=1)
  },error=function(e){NULL},warning=function(w){NULL})

})})
output$motor_7 = renderPlot({
  withProgress(message = "Drawing Downstream charts 7",{
    
  set.seed(123);D=raw();D$selected = vals$keeprows
  tryCatch({
  boxplot_MA(D ,xname = BY(), yname = "H1_H0_DELTA_RAMP_CYL",TT = "H1_H0_DELTA_RAMP_CYL",rollN = input$rollN,bgcolor=1)
  },error=function(e){NULL},warning=function(w){NULL})

})})


output$motor_8 = renderPlot({
  withProgress(message = "Drawing Downstream charts 8",{

    set.seed(123);D=raw();D$selected = vals$keeprows
    validate(need(
      !all(is.na(D[,"H2_RAMP_CYL"]))
      ,
      "No proper data"
    ))
    p=tryCatch({
    boxplot_MA(D ,xname = BY(), yname = "H2_RAMP_CYL",TT = "H2_RAMP_CYL",rollN = input$rollN,bgcolor=1)
    },error=function(e){NULL},warning=function(w){NULL})
    p
  })})

output$motor_9 = renderPlot({
  withProgress(message = "Drawing Downstream charts 9",{
    
    set.seed(123);D=raw();D$selected = vals$keeprows
    validate(need(
      !all(is.na(D[,"H3_RAMP_CYL"]))
      ,
      "No proper data"
    ))
    tryCatch({
    boxplot_MA(D ,xname = BY(), yname = "H3_RAMP_CYL",TT = "H3_RAMP_CYL",rollN = input$rollN,bgcolor=1)
    },error=function(e){NULL},warning=function(w){NULL})

  })})
output$motor_10 = renderPlot({
  withProgress(message = "Drawing Downstream charts 10",{
    
    set.seed(123);D=raw();D$selected = vals$keeprows
    validate(need(
      !all(is.na(D[,"H3_H2_DELTA_RAMP_CYL"]))
      ,
      "No proper data"
    ))
    tryCatch({
    boxplot_MA(D ,xname = BY(), yname = "H3_H2_DELTA_RAMP_CYL",TT = "H3_H2_DELTA_RAMP_CYL",rollN = input$rollN,bgcolor=1)
    },error=function(e){NULL},warning=function(w){NULL})

  })})



### guilong start
output$PROJECTION_DYNAMIC_FLANGE_HT_chart = renderPlot({
  withProgress(message = "Drawing Ramp data",{
    set.seed(123);D=raw();D$selected = vals$keeprows
    validate(need(
      !all(is.na(D[,"PROJECTION_DYNAMIC_FLANGE_HT"]))
      ,
      "No proper data"
    ))
    tryCatch({
      boxplot_MA(D ,xname = BY(), yname = "PROJECTION_DYNAMIC_FLANGE_HT",TT = "PROJECTION_DYNAMIC_FLANGE_HT",rollN = input$rollN,bgcolor=1)
    },error=function(e){NULL},warning=function(w){NULL})
  })
})

output$RAMP_SEATING_HT_chart = renderPlot({
  withProgress(message = "Drawing Ramp data",{
    set.seed(123);D=raw();D$selected = vals$keeprows
    validate(need(
      !all(is.na(D[,"RAMP_SEATING_HT"]))
      ,
      "No proper data"
    ))
    tryCatch({
      boxplot_MA(D ,xname = BY(), yname = "RAMP_SEATING_HT",TT = "RAMP_SEATING_HT",rollN = input$rollN,bgcolor=1)
    },error=function(e){NULL},warning=function(w){NULL})
  })
})

output$PIVOT_SEAT_HT_chart = renderPlot({
  withProgress(message = "Drawing Ramp data",{
    set.seed(123);D=raw();D$selected = vals$keeprows
    validate(need(
      !all(is.na(D[,"PIVOT_SEAT_HT"]))
      ,
      "No proper data"
    ))
    tryCatch({
      boxplot_MA(D ,xname = BY(), yname = "PIVOT_SEAT_HT",TT = "PIVOT_SEAT_HT",rollN = input$rollN,bgcolor=1)
    },error=function(e){NULL},warning=function(w){NULL})
  })
})


### guilong end
