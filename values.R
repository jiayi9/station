

#vals <- reactiveValues(DSN = character(), keeprows = FALSE, keeprows_TS = FALSE, keeprows_multi = FALSE)

vals <- reactiveValues(DSN = character(), keeprows = FALSE)


BY = reactive({
  if(input$by == "Day"){
    R = "CLRM_EXIT_DATE"
  } else {
    R = "FMS_FISCAL_YEAR_WEEK"
  }
  R
})


#multi boxplot
# observeEvent(input$brush_0,{
#   Res=brushedPoints(raw(),input$brush_0,allRows = TRUE)
#   vals$keeprows = vals$keeprows |Res$selected_
#   
# })

#multi scatterplot
# observeEvent(input$brush_00,{
#   Res=brushedPoints(raw(),input$brush_00,allRows = TRUE)
#   vals$keeprows = vals$keeprows |Res$selected_
#   
# })

#shift
# observeEvent(input$brush_01,{
#   Res=brushedPoints(raw(),input$brush_01,allRows = TRUE)
#   vals$keeprows = vals$keeprows |Res$selected_
# })
# 
# observeEvent(input$brush_d1,{
#   Res=brushedPoints(raw(),input$brush_d1,allRows = TRUE)
#   vals$keeprows = vals$keeprows |Res$selected_
# })
# 
# observeEvent(input$brush_d2,{
#   Res=brushedPoints(raw(),input$brush_d2,allRows = TRUE)
#   vals$keeprows = vals$keeprows |Res$selected_
# })
# observeEvent(input$brush_d3,{
#   Res=brushedPoints(raw(),input$brush_d3,allRows = TRUE)
#   vals$keeprows = vals$keeprows |Res$selected_
# })

observeEvent(input$ok,{
  vals$keeprows = rep(FALSE,nrow(raw()))
},priority = -1)

# lapply(
#   X = c(1:27,"0","00","01","d1","d2","d3"),
#   FUN = function(i){
#     observeEvent(input[[paste0("brush_", i)]], {
#       Res = brushedPoints(raw(),input[[paste0("brush_", i)]], allRows = TRUE)
#       Res$selected_[ is.na(Res$selected_)] = FALSE 
#       vals$keeprows = vals$keeprows |Res$selected_
# #       print(sum(is.na(vals$keeprows)))
# #       print(sum(is.na(Res$selected)))
# 
#     })
#   }
# )

for (i in c(1:33,"0","00","01","d1","d2","d3")){
  local({
  j = i
  observeEvent(input[[paste0("brush_", j)]], {
    Res = brushedPoints(raw(),input[[paste0("brush_", j)]], allRows = TRUE)
    Res$selected_[ is.na(Res$selected_)] = FALSE 
    vals$keeprows = vals$keeprows |Res$selected_
  })
  })
}

# TS

# observeEvent(input$brush_TS,{
#   Res=brushedPoints(pca_d(),input$brush_TS,allRows = TRUE)
#   vals$keeprows_TS = Res$selected_
# })

#multi

# observeEvent(input$brush_multi,{
#   Res=brushedPoints(RAW(),input$brush_multi,allRows = TRUE)
#   vals$keeprows_multi = Res$selected_
# })



# selected = reactive({
#   rep(TRUE,length(INDEX()))
# })
# 









# observeEvent(input$add1,{
#   vals$DSN = union(vals$DSN,  dsn()[vals$keeprows] )
#   vals$DSN = na.omit(vals$DSN)  
# })

observe({
  vals$DSN = union(vals$DSN,  dsn()[vals$keeprows] )
  vals$DSN = na.omit(vals$DSN)  
})

# observeEvent(input$add_multi,{
#   vals$DSN = union(vals$DSN,  DSN()[D_multi()$selected_] )
# })

observeEvent(input$reset,{
  vals$DSN = character()
  vals$keeprows = FALSE
})


output$outlierList = renderText({
  paste(vals$DSN, collapse = " ")
})  


output$hover_info <- renderText({
  hover = input$hover
  D = raw()

  dsn = as.character(nearPoints(D,input$hover)[1,"DSN"])
  paste("Drive hovered :", ifelse(is.na(dsn),"",dsn))
  
})
