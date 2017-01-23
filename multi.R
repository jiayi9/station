WIDTH = 0.2 



D_multi = reactive({
  raw()
})

output$multi = renderPlot({
  req(D_multi(), 
      input$multi_para,
      input$multi_attr,
      input$multi_facet_1,
      input$multi_facet_2,
      input$multi_facet_3,
      input$multi_facet_4,
      input$multi_facet_5  
      )
  
  D = D_multi()
  validate(need(nrow(D)>0,""))
  set.seed(123)
  #print(vals$keeprows)
  
  if(input$view_mode == "Dynamic & Fixed Scales") {view_mode = "fixed"} else { view_mode = "free"}
  
  
  D$selected = vals$keeprows

  p =  if(input$multi_facet_1 == "None" & input$multi_facet_2 == "None" & input$multi_facet_3 == "None" &
            input$multi_facet_4 == "None" & input$multi_facet_5 == "None"){#} & input$multi_facet_6 == "None"){
    
    ggplot(D,aes_string(y=input$multi_para, x = input$multi_attr)) + geom_boxplot(outlier.shape = NA) + 
      #stat_summary(fun.y=median, geom="line",aes(group=1),color="red")+
      #geom_jitter(aes(color=vals$keeprows),width=0.2)+
      #              geom_jitter(aes(color=vals$keeprows),width = WIDTH,show.legend = FALSE,size = ifelse(vals$keeprows,4,1.5))+
      geom_jitter(aes(color=selected,size = selected),width = WIDTH,show.legend = FALSE)+
      
      scale_color_manual(values = c("black","red"),guide=FALSE)+
      theme(legend.position="none",
            axis.text.x = element_text(angle = 90, hjust = 0),
            plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
      )
    
    
  } else if(input$multi_facet_1 != "None" & input$multi_facet_2 == "None" & input$multi_facet_3 == "None" &
              input$multi_facet_4 == "None" & input$multi_facet_5 == "None"){# & input$multi_facet_6 == "None"){
    
    TT = paste(input$multi_facet_1)
    
    ggplot(D,aes_string(y=input$multi_para, x = input$multi_attr)) + geom_boxplot(outlier.shape = NA) + 
      #stat_summary(fun.y=median, geom="line",aes(group=1),color="red")+
      geom_jitter(aes(color=selected,size=selected),width = WIDTH,show.legend = FALSE)+
      #facet_grid(reformulate(input$multi_facet_1),scales="free",space="free")+
      
      facet_grid(reformulate(input$multi_facet_1),scales = view_mode, space = view_mode )+
      scale_color_manual(values = c("black","red"),guide=FALSE)+
      
      ggtitle(TT)+
      theme(legend.position="none",
            axis.text.x = element_text(angle = 90, hjust = 0),
            plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
      )
    
    
  } else if(input$multi_facet_1 != "None" & input$multi_facet_2 != "None" & input$multi_facet_3 == "None" &
              input$multi_facet_4 == "None" & input$multi_facet_5 == "None"){# & input$multi_facet_6 == "None"){
    TT = paste(input$multi_facet_1,"  > ", input$multi_facet_2)

    ggplot(D,aes_string(y=input$multi_para, x = input$multi_attr)) + geom_boxplot(outlier.shape = NA) + 
      #stat_summary(fun.y=median, geom="line",aes(group=1),color="red")+
      geom_jitter(aes(color=selected,size=selected),width = WIDTH,show.legend = FALSE)+
      scale_color_manual(values = c("black","red"),guide=FALSE)+
      facet_grid(reformulate(c(input$multi_facet_1,input$multi_facet_2),response="."),scales = view_mode, space = view_mode)+
      ggtitle(TT)+
      theme(legend.position="none",
            axis.text.x = element_text(angle = 90, hjust = 0),
            plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
      )
    
  } else if(input$multi_facet_1 != "None" & input$multi_facet_2 != "None" & input$multi_facet_3 != "None" &
              input$multi_facet_4 == "None" & input$multi_facet_5 == "None"){# & input$multi_facet_6 == "None"){
    
    TT = paste(input$multi_facet_1,"  > ", input$multi_facet_2, "  > ", input$multi_facet_3)
    
    ggplot(D,aes_string(y=input$multi_para, x = input$multi_attr)) + geom_boxplot(outlier.shape = NA) + 
      #stat_summary(fun.y=median, geom="line",aes(group=1),color="red")+
      geom_jitter(aes(color=selected,size=selected),width = WIDTH,show.legend = FALSE)+
      scale_color_manual(values = c("black","red"),guide=FALSE)+
      facet_grid(reformulate(c(input$multi_facet_1,input$multi_facet_2,input$multi_facet_3),response="."),
                 scales = view_mode, space = view_mode 
                 )+
      ggtitle(TT)+
      theme(legend.position="none",
            axis.text.x = element_text(angle = 90, hjust = 0),
            plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
      )
    
  } else if(input$multi_facet_1 != "None" & input$multi_facet_2 != "None" & input$multi_facet_3 != "None" &
              input$multi_facet_4 != "None" & input$multi_facet_5 == "None"){# & input$multi_facet_6 == "None"){
    
    TT = paste(input$multi_facet_1,"  > ", input$multi_facet_2, "  > ", input$multi_facet_3, "  > ", input$multi_facet_4)
    
    
    ggplot(D,aes_string(y=input$multi_para, x = input$multi_attr)) + geom_boxplot(outlier.shape = NA) + 
      #stat_summary(fun.y=median, geom="line",aes(group=1),color="red")+
      geom_jitter(aes(color=selected,size=selected),width = WIDTH,show.legend = FALSE)+
      scale_color_manual(values = c("black","red"),guide=FALSE)+
      facet_grid(reformulate(c(input$multi_facet_1,input$multi_facet_2,input$multi_facet_3,input$multi_facet_4),response="."),scales = view_mode, space = view_mode )+
      ggtitle(TT)+
      theme(legend.position="none",
            axis.text.x = element_text(angle = 90, hjust = 0),
            plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
      )
  } else if(input$multi_facet_1 != "None" & input$multi_facet_2 != "None" & input$multi_facet_3 != "None" &
              input$multi_facet_4 != "None" & input$multi_facet_5 != "None"){# & input$multi_facet_6 == "None"){
    
    TT = paste(input$multi_facet_1,"  > ", input$multi_facet_2, "  > ", input$multi_facet_3, "  > ", input$multi_facet_4, "  > ", input$multi_facet_5)
    
    ggplot(D,aes_string(y=input$multi_para, x = input$multi_attr)) + geom_boxplot(outlier.shape = NA) + 
      #stat_summary(fun.y=median, geom="line",aes(group=1),color="red")+
      geom_jitter(aes(color=selected,size=selected),width = WIDTH,show.legend = FALSE)+
      scale_color_manual(values = c("black","red"),guide=FALSE)+
      facet_grid(reformulate(c(input$multi_facet_1,input$multi_facet_2,input$multi_facet_3,input$multi_facet_4
                               ,input$multi_facet_5),response="."),scales = view_mode, space = view_mode )+
      ggtitle(TT)+
      theme(legend.position="none",
            axis.text.x = element_text(angle = 90, hjust = 0),
            plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
      )
    
  } else if(input$multi_facet_1 != "None" & input$multi_facet_2 != "None" & input$multi_facet_3 != "None" &
              input$multi_facet_4 != "None" & input$multi_facet_5 != "None"){# & input$multi_facet_6 != "None"){
    
    TT = paste(input$multi_facet_1,"  > ", input$multi_facet_2, "  > ", input$multi_facet_3, "  > ", input$multi_facet_4, "  > ", input$multi_facet_5, "  > ", input$multi_facet_6)
    
    ggplot(D,aes_string(y=input$multi_para, x = input$multi_attr)) + geom_boxplot(outlier.shape = NA) + 
      #stat_summary(fun.y=median, geom="line",aes(group=1),color="red")+
      geom_jitter(aes(color=selected,size=selected),width = WIDTH,show.legend = FALSE)+
      scale_color_manual(values = c("black","red"),guide=FALSE)+
      facet_grid(reformulate(c(input$multi_facet_1,input$multi_facet_2,input$multi_facet_3,input$multi_facet_4
                               ,input$multi_facet_5,input$multi_facet_6),response="."),scales = view_mode, space = view_mode )+
      ggtitle(TT)+
      theme(legend.position="none",
            axis.text.x = element_text(angle = 90, hjust = 0),
            plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
            
      )
  } else {
    NULL
  } 
  p
})














####### scatterplot  

multi_Para_list = c(PARA_list,
                    #CERT_list,
                    "FLANGE_HEIGHT_AVG",
                    "FLANGE_HEIGHT_PARALLEL",
                    "H0_RAMP_CYL",
                    "H1_RAMP_CYL",
                    "H1_H0_DELTA_RAMP_CYL",
                    "H3_H2_DELTA_RAMP_CYL",
                    "PROJECTION_DYNAMIC_FLANGE_HT",
                    "RAMP_SEATING_HT",
                    "PIVOT_SEAT_HT"
                    )

output$SCATTER_PARA_1 = renderUI({
  
  selectInput("scatter_para_1","Parametric X",choices = multi_Para_list)
  
})

output$SCATTER_PARA_2 = renderUI({
  
  selectInput("scatter_para_2","Parametric Y",choices = multi_Para_list,selected = multi_Para_list[2])
  
})


output$SCATTER_FACET_1 = renderUI({
  
  #  choices = ifelse(input$multi_attr_1=="None", "None", ATTR_list)
  
  choices = c("None",ATTR_list)
  
  selectInput("scatter_facet_1","Facet 1",choices = choices, selected = "None"  )
  
  
})

output$SCATTER_FACET_2 = renderUI({
  #  choices = ifelse(input$scatter_attr_2=="None", "None", ATTR_list)
  choices = c("None",ATTR_list)
  
  selectInput("scatter_facet_2","Facet 2",choices = choices, selected = "None"  )
  
  
})

output$SCATTER_FACET_3 = renderUI({
  #  choices = ifelse(input$scatter_attr_2=="None", "None", ATTR_list)
  choices = c("None",ATTR_list)
  
  selectInput("scatter_facet_3","Facet 3",choices = choices, selected = "None"  )
  
  
})

output$SCATTER_FACET_4 = renderUI({
  #  choices = ifelse(input$scatter_attr_2=="None", "None", ATTR_list)
  choices = c("None",ATTR_list)
  
  selectInput("scatter_facet_4","Facet 4",choices = choices, selected = "None"  )
  
  
})

output$SCATTER_FACET_5 = renderUI({
  #  choices = ifelse(input$scatter_attr_2=="None", "None", ATTR_list)
  choices = c("None",ATTR_list)
  
  selectInput("scatter_facet_5","Facet 5",choices = choices, selected = "None"  )
  
  
})

output$SCATTER_FACET_6 = renderUI({
  #  choices = ifelse(input$scatter_attr_2=="None", "None", ATTR_list)
  choices = c("None",ATTR_list)
  
  selectInput("scatter_facet_6","Facet 6",choices = choices, selected = "None"  )
  
  
})



output$multi_scatter = renderPlot({
  req(D_multi(), 
      input$scatter_para_1,
      input$scatter_para_2,
      input$scatter_facet_1,
      input$scatter_facet_2,
      input$scatter_facet_3
#       input$scatter_facet_4,
#       input$scatter_facet_5,
#       input$scatter_facet_6
  )
  


  D = D_multi()
  validate(need(nrow(D)>0,""))
  set.seed(123)
  #print(vals$keeprows)
  
  D$selected = vals$keeprows
  
  p =  if(input$scatter_facet_1 == "None" & input$scatter_facet_2 == "None" & input$scatter_facet_3 == "None" ){
#           &
#             input$scatter_facet_4 == "None" & input$scatter_facet_5 == "None" & input$scatter_facet_6 == "None"){
    
    ggplot(D,aes_string(y=input$scatter_para_1, x = input$scatter_para_2)) + 
      geom_point(aes(color=selected,size = selected),show.legend = FALSE) + 
      scale_color_manual(values = c("black","red"),guide=FALSE)+
      theme(legend.position="none",
            axis.text.x = element_text(angle = 90, hjust = 0),
            plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
      )
    
    
  } else if(input$scatter_facet_1 != "None" & input$scatter_facet_2 == "None" & input$scatter_facet_3 == "None"){ 
#             &
#               input$scatter_facet_4 == "None" & input$scatter_facet_5 == "None" & input$scatter_facet_6 == "None"){
    
    TT = paste(input$scatter_facet_1)
    
    ggplot(D,aes_string(y=input$scatter_para_1, x = input$scatter_para_2)) + 
      geom_point(aes(color=selected,size = selected),show.legend = FALSE) +
      facet_grid(reformulate(input$scatter_facet_1))+
      
      scale_color_manual(values = c("black","red"),guide=FALSE)+
      theme(legend.position="none",
            axis.text.x = element_text(angle = 90, hjust = 0),
            plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
      )+
      ggtitle(TT)
    
 
    
  } else if(input$scatter_facet_1 != "None" & input$scatter_facet_2 != "None" & input$scatter_facet_3 == "None" ){
#             &
#               input$scatter_facet_4 == "None" & input$scatter_facet_5 == "None" & input$scatter_facet_6 == "None"){
    TT = paste(input$scatter_facet_1,"  > ", input$scatter_facet_2)
    
    
    
    
    ggplot(D,aes_string(y=input$scatter_para_1, x = input$scatter_para_2)) + 
      geom_point(aes(color=selected,size = selected),show.legend = FALSE) +
      facet_grid(reformulate(c(input$scatter_facet_1,input$scatter_facet_2),response="."))+
      
      scale_color_manual(values = c("black","red"),guide=FALSE)+
      theme(legend.position="none",
            axis.text.x = element_text(angle = 90, hjust = 0),
            plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
      )+
      ggtitle(TT)
    
    

    
  } else if(input$scatter_facet_1 != "None" & input$scatter_facet_2 != "None" & input$scatter_facet_3 != "None" ){
#            & input$scatter_facet_4 == "None" & input$scatter_facet_5 == "None" & input$scatter_facet_6 == "None"){
    
    TT = paste(input$scatter_facet_1,"  > ", input$scatter_facet_2, "  > ", input$scatter_facet_3)
    ggplot(D,aes_string(y=input$scatter_para_1, x = input$scatter_para_2)) + 
      geom_point(aes(color=selected,size = selected),show.legend = FALSE) +
      facet_grid(reformulate(c(input$scatter_facet_1,input$scatter_facet_2,input$scatter_facet_3),response="."))+
      
      scale_color_manual(values = c("black","red"),guide=FALSE)+
      theme(legend.position="none",
            axis.text.x = element_text(angle = 90, hjust = 0),
            plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
      )+
      ggtitle(TT)

#     
#   } else if(input$scatter_facet_1 != "None" & input$scatter_facet_2 != "None" & input$scatter_facet_3 != "None" &
#               input$scatter_facet_4 != "None" & input$scatter_facet_5 == "None" & input$scatter_facet_6 == "None"){
#     
#     TT = paste(input$scatter_facet_1,"  > ", input$scatter_facet_2, "  > ", input$scatter_facet_3, "  > ", input$scatter_facet_4)
#     
#     
#     ggplot(D,aes_string(y=input$scatter_para_1, x = input$scatter_para_2)) + 
#       geom_point(aes(color=selected,size = selected),show.legend = FALSE) +
#       facet_grid(reformulate(c(input$scatter_facet_1,input$scatter_facet_2,input$scatter_facet_3,input$scatter_facet_4),response="."))+
#       
#       scale_color_manual(values = c("black","red"),guide=FALSE)+
#       theme(legend.position="none",
#             axis.text.x = element_text(angle = 90, hjust = 0),
#             plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
#       )+
#       ggtitle(TT)
# 
#   } else if(input$scatter_facet_1 != "None" & input$scatter_facet_2 != "None" & input$scatter_facet_3 != "None" &
#               input$scatter_facet_4 != "None" & input$scatter_facet_5 != "None" & input$scatter_facet_6 == "None"){
#     
#     TT = paste(input$scatter_facet_1,"  > ", input$scatter_facet_2, "  > ", input$scatter_facet_3, "  > ", input$scatter_facet_4, "  > ", input$scatter_facet_5)
#     
#     
#     ggplot(D,aes_string(y=input$scatter_para_1, x = input$scatter_para_2)) + 
#       geom_point(aes(color=selected,size = selected),show.legend = FALSE) +
#       facet_grid(reformulate(c(input$scatter_facet_1,input$scatter_facet_2,input$scatter_facet_3,input$scatter_facet_4
#                                ,input$scatter_facet_5),response="."))+
#       scale_color_manual(values = c("black","red"),guide=FALSE)+
#       theme(legend.position="none",
#             axis.text.x = element_text(angle = 90, hjust = 0),
#             plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
#       )+
#       ggtitle(TT)
#     
# 
#     
#   } else if(input$scatter_facet_1 != "None" & input$scatter_facet_2 != "None" & input$scatter_facet_3 != "None" &
#               input$scatter_facet_4 != "None" & input$scatter_facet_5 != "None" & input$scatter_facet_6 != "None"){
#     
#     TT = paste(input$scatter_facet_1,"  > ", input$scatter_facet_2, "  > ", input$scatter_facet_3, "  > ", input$scatter_facet_4, "  > ", input$scatter_facet_5, "  > ", input$scatter_facet_6)
#     
#     ggplot(D,aes_string(y=input$scatter_para_1, x = input$scatter_para_2)) + 
#       geom_point(aes(color=selected,size = selected),show.legend = FALSE) +
#       facet_grid(reformulate(c(input$scatter_facet_1,input$scatter_facet_2,input$scatter_facet_3,input$scatter_facet_4
#                                ,input$scatter_facet_5,input$scatter_facet_6),response="."))+
#       scale_color_manual(values = c("black","red"),guide=FALSE)+
#       theme(legend.position="none",
#             axis.text.x = element_text(angle = 90, hjust = 0),
#             plot.title = element_text( colour = "blue",size=ggplot2::rel(1.4))
#       )+
#       ggtitle(TT)
    
    

            
      
  } else {
    NULL
  } 
  if(input$ellipse =="YES"){
    p = p + stat_ellipse(type="norm", linetype=2,level = 0.99,color="red")
  }
p
})






output$multi_hints_1 = renderUI({
  t1 = paste("<b>Product:</b>",paste(input$product,collapse = ","))
  t2 = paste("<b>Site:</b>",paste(input$site,collapse = ","))
  t3 = paste("<b>Capacity:</b>",paste(input$capacity,collapse = ","))
  t4 = paste("<b>Run Type:</b>",paste(input$runtype,collapse = ","))
  t5 = paste("<b>Range from</b>", paste(input$from,"to", input$to,collapse = ","))
  t6 = paste("<b>HDA_code:</b>",paste(input$hda_code,collapse = ","))
  t7 = paste("<b>Prefix:</b>",paste(input$prefix,collapse = ","))
  t8 = paste("<b>Line:</b>",paste(input$line,collapse = ","))
  t9 = paste("<b>RPI:</b>",paste(input$rpi,collapse = ","))

  t10 = paste("<b>SBR:</b>", paste(input$sbr,collapse = ",")) 

  t11 = paste("<b>MBA Part Num:</b>",paste(input$mba_part_num,collapse = ","))
  t12 = paste("<b>Motor Base:</b>",paste(input$motor_base,collapse = ","))
  t13 = paste("<b>Motor Ramp Ven:</b>",paste(input$motor_ramp_ven,collapse = ","))
  t14 = paste("<b>HSA FW:</b>",paste(input$hsa_fw,collapse = ","))
  t15 = paste("<b>HSA Rev:</b>",paste(input$hsa_rev,collapse = ","))
  t16 = paste("<b>HSA Vendor:</b>",paste(input$hsa_vendor,collapse = ","))
  t17 = paste("<b>Media Type:</b>",paste(input$media_type,collapse = ","))
  
#  HTML(markdownToHTML(fragment.only = TRUE, text = Text))
  HTML(paste(
    t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,
             #t11,t12,t13,t14,t15,t16,t17,
             sep = '<br/>')) 
})


output$multi_hints_2 = renderUI({
  t1 = paste("<b>Product:</b>",paste(input$product,collapse = ","))
  t2 = paste("<b>Site:</b>",paste(input$site,collapse = ","))
  t3 = paste("<b>Capacity:</b>",paste(input$capacity,collapse = ","))
  t4 = paste("<b>Run Type:</b>",paste(input$runtype,collapse = ","))
  t5 = paste("<b>Range from</b>", paste(input$from,"to", input$to,collapse = ","))
  t6 = paste("<b>HDA_code:</b>",paste(input$hda_code,collapse = ","))
  t7 = paste("<b>Prefix:</b>",paste(input$prefix,collapse = ","))
  t8 = paste("<b>Line:</b>",paste(input$line,collapse = ","))
  t9 = paste("<b>RPI:</b>",paste(input$rpi,collapse = ","))
  
  t10 = paste("<b>SBR:</b>", paste(input$sbr,collapse = ",")) 
  
  t11 = paste("<b>MBA Part Num:</b>",paste(input$mba_part_num,collapse = ","))
  t12 = paste("<b>Motor Base:</b>",paste(input$motor_base,collapse = ","))
  t13 = paste("<b>Motor Ramp Ven:</b>",paste(input$motor_ramp_ven,collapse = ","))
  t14 = paste("<b>HSA FW:</b>",paste(input$hsa_fw,collapse = ","))
  t15 = paste("<b>HSA Rev:</b>",paste(input$hsa_rev,collapse = ","))
  t16 = paste("<b>HSA Vendor:</b>",paste(input$hsa_vendor,collapse = ","))
  t17 = paste("<b>Media Type:</b>",paste(input$media_type,collapse = ","))
  
  #  HTML(markdownToHTML(fragment.only = TRUE, text = Text))
  HTML(paste(#t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,
             t11,t12,t13,t14,t15,t16,t17,
             sep = '<br/>')) 
})



output$multi_hints = renderUI({
  t1 = paste("<b>Product:</b>",paste(input$product,collapse = ","))
  t2 = paste("<b>Site:</b>",paste(input$site,collapse = ","))
  t3 = paste("<b>Capacity:</b>",paste(input$capacity,collapse = ","))
  t4 = paste("<b>Run Type:</b>",paste(input$runtype,collapse = ","))
  t5 = paste("<b>Range from</b>", paste(input$from,"to", input$to,collapse = ","))
  t6 = paste("<b>HDA_code:</b>",paste(input$hda_code,collapse = ","))
  t7 = paste("<b>Prefix:</b>",paste(input$prefix,collapse = ","))
  t8 = paste("<b>Line:</b>",paste(input$line,collapse = ","))
  t9 = paste("<b>RPI:</b>",paste(input$rpi,collapse = ","))
  
  t10 = paste("<b>SBR:</b>", paste(input$sbr,collapse = ",")) 
  
  t11 = paste("<b>MBA Part Num:</b>",paste(input$mba_part_num,collapse = ","))
  t12 = paste("<b>Motor Base:</b>",paste(input$motor_base,collapse = ","))
  t13 = paste("<b>Motor Ramp Ven:</b>",paste(input$motor_ramp_ven,collapse = ","))
  t14 = paste("<b>HSA FW:</b>",paste(input$hsa_fw,collapse = ","))
  t15 = paste("<b>HSA Rev:</b>",paste(input$hsa_rev,collapse = ","))
  t16 = paste("<b>HSA Vendor:</b>",paste(input$hsa_vendor,collapse = ","))
  t17 = paste("<b>Media Type:</b>",paste(input$media_type,collapse = ","))
  
  #  HTML(markdownToHTML(fragment.only = TRUE, text = Text))
  HTML(paste(
    t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,
    t11,t12,t13,t14,t15,t16,t17,
    sep = '<br/>')) 
})