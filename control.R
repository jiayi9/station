

# major

output$PRODUCT = renderUI({
  choices = sort(unique(PRODUCT()),na.last = FALSE)
  selectInput("product","Product",choices = choices,selected = choices[1], multiple = TRUE)
})

INDEX_PRODUCT =  reactive(PRODUCT() %in% input$product)

output$SITE = renderUI({
  req(input$product)
  choices = sort(unique(SITE()[INDEX_PRODUCT()]),na.last = FALSE)
  selectInput("site","Site",choices = choices,multiple = TRUE,selected = choices[1])
  
})

INDEX_PRODUCT_SITE = reactive(INDEX_PRODUCT() & (SITE() %in% input$site))


output$CAPACITY = renderUI({
  req(input$site)
  choices = sort(unique( CAPACITY()[INDEX_PRODUCT_SITE()]),na.last = FALSE)
  
#  choices = c("ALL",choices)
  

  selectInput("capacity","Capacity",choices = choices, multiple = TRUE, selected = choices[1])
})

INDEX_PRODUCT_SITE_CAPACITY = reactive({
  req(input$capacity)
#   if( "ALL" %in% input$capacity ){
#     R = INDEX_PRODUCT_SITE()
#   } else {
#     R = INDEX_PRODUCT_SITE() & (CAPACITY() %in% input$capacity)
#     
#   }
#     R
  INDEX_PRODUCT_SITE() & (CAPACITY() %in% input$capacity)
})

output$RUNTYPE = renderUI({
  req(input$capacity)
  
  choices = sort(unique( RUNTYPE()[INDEX_PRODUCT_SITE_CAPACITY()]),na.last = FALSE)
  selectInput("runtype","Run type", choices = choices, selected = choices,multiple = TRUE)
  
})

INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE_tmp = reactive(INDEX_PRODUCT_SITE_CAPACITY() & (RUNTYPE() %in% input$runtype))

# major -> dates

output$FROM = renderUI({
  req(input$runtype)
  choices = sort(unique( DATE()[   INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE_tmp()  ])   ,na.last = FALSE)
  if(length(choices)>=15){
    selectInput("from","FROM", choices = choices, selected = choices[length(choices)-14])    
  } else {
    selectInput("from","FROM", choices = choices, selected = choices[1])    
  }
  
})

output$TO = renderUI({
  req(input$runtype)
    
  choices = sort(unique( DATE()[   INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE_tmp()  ])   ,na.last = FALSE)
  
  if(input$by == "Week"){
    choices = choices[week_to_numeric(choices) >= week_to_numeric(input$from)]
  } else{
    choices = choices[ choices >= input$from]
  }
  selectInput("to","TO", choices = choices, selected = choices[length(choices)])
  
})

output$dates_num = renderText({
  paste("You have selected", length(unique(date())), ifelse(input$by=="Day","Days","Weeks"))
})

# output$DATE_RANGE = renderUI({
#   choices = sort(unique( DATE()[   INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE_tmp()  ]) , decreasing = TRUE)
#   dropdownButton(
#     label = "Date Range", status ="default",width =50,
#     actionButton(inputId = "date_range_all", label = "(Un)select all",
#                  class="btn btn-primary btn-sm"),
#     #br(),
#     checkboxGroupInput("date_range","",choices = choices,selected = choices[1:7])
#   )
# })

# observeEvent(input$date_range_all, {
#   print(input$date_range)
#   choices = sort(unique( DATE()[   INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE_tmp()  ]) , decreasing = TRUE)
#     
#   if (is.null(input$date_range)) {
#     print(choices)
#     updateCheckboxGroupInput(
#       session = session, inputId = "date_range", selected = as.character(choices)
#     )
#   } else {
#     updateCheckboxGroupInput(
#       session = session, inputId = "date_range", selected = ""
#     )
#   }
# })

INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE = reactive({
  if(input$by == "Day"){
  R = INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE_tmp() & 
    (as.numeric(DATE()) >= as.numeric(input$from)) &
    (as.numeric(DATE()) <= as.numeric(input$to))
  } else {
    From = week_to_numeric(input$from)
    To =week_to_numeric(input$to)
    Weeks = week_to_numeric(DATE())
    
    R= INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE_tmp() &
      Weeks >= From &
      Weeks <=To
    
  }
  R
})

# major -> SBR

output$SBR = renderUI({
  choices = sort( unique(  SBR()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()]),na.last = FALSE)
  dropdownButton(
    label = "SBR", status ="default",width =50,
    actionButton(inputId = "sbr_all", label = "(Un)select all",
                 class="btn btn-primary btn-sm"),
    #br(),
    checkboxGroupInput("sbr","",choices = choices,selected = choices)
  )
  
})


# major - > motors
output$MBA_PART_NUM = renderUI({
  choices = sort(unique(  MBA_PART_NUM()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()]),na.last = FALSE)
#   selectInput("mba_part_num","MBA Part Num",choices = choices, multiple = TRUE, selected = choices)
  
  dropdownButton(
    label = "MBA Part Num", status ="default",width =50,
    actionButton(inputId = "mba_part_num_all", label = "(Un)select all",
                 class="btn btn-primary btn-sm"),
    #br(),
    checkboxGroupInput("mba_part_num","",choices = choices,selected = choices)
  )
  
})

output$MOTOR_BASE = renderUI({
  choices = sort(unique(  MOTOR_BASE()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()]),na.last = FALSE)
#  selectInput("motor_base","Motor Base",choices = choices, multiple = TRUE, selected = choices)
  
  dropdownButton(
    label = "Motor Base", status ="default",width =50,
    actionButton(inputId = "motor_base_all", label = "(Un)select all",
                 class="btn btn-primary btn-sm"),
    #br(),
    checkboxGroupInput("motor_base","",choices = choices,selected = choices)
  )
  
})

output$MOTOR_RAMP_VEN = renderUI({
  choices = sort(unique(  MOTOR_RAMP_VEN()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()]),na.last = FALSE)
  selectInput("motor_ramp_ven","Motor Ramp Ven",choices = choices, multiple = TRUE, selected = choices)

  dropdownButton(
    label = "Motor Ramp Ven", status ="default",width =50,
    actionButton(inputId = "motor_ramp_ven_all", label = "(Un)select all",
                 class="btn btn-primary btn-sm"),
    #br(),
    checkboxGroupInput("motor_ramp_ven","",choices = choices,selected = choices)
  )
  
})


# major -> media

output$MEDIA_TYPE = renderUI({
  choices = sort(unique(  MEDIA_TYPE()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()]),na.last = FALSE)
#   selectInput("media_type","Media Type",choices = choices, multiple = TRUE, selected = choices)
  
  dropdownButton(
    label = "Media Type", status ="default",width =50,
    actionButton(inputId = "media_type_all", label = "(Un)select all",
                 class="btn btn-primary btn-sm"),
    #br(),
    checkboxGroupInput("media_type","",choices = choices,selected = choices)
  )
  
})


# major -> hsa

output$HSA_FW = renderUI({
  choices = sort( unique(  HSA_FW()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()]  ) ,na.last = FALSE)
#  selectInput("hsa_fw","HSA FW",choices = choices, multiple = TRUE, selected = choices)
  
  dropdownButton(
    label = "HSA FW", status ="default",width =50,
    actionButton(inputId = "hsa_fw_all", label = "(Un)select all",
                 class="btn btn-primary btn-sm"),
    #br(),
    checkboxGroupInput("hsa_fw","",choices = choices,selected = choices)
  )
  
})

output$HSA_REV = renderUI({
  choices = sort(unique(  HSA_REV()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()]),na.last = FALSE)
#  selectInput("hsa_rev","HSA REV",choices = choices, multiple = TRUE, selected = choices)
  
  dropdownButton(
    label = "HSA REV", status ="default",width =50,
    actionButton(inputId = "hsa_rev_all", label = "(Un)select all",
                 class="btn btn-primary btn-sm"),
    #br(),
    checkboxGroupInput("hsa_rev","",choices = choices,selected = choices)
  )
  
})

output$HSA_VENDOR = renderUI({
  choices = sort(unique(  HSA_VENDOR()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()]),na.last = FALSE)
#  selectInput("hsa_vendor","HSA Vendor",choices = choices, multiple = TRUE, selected = choices)
  
  dropdownButton(
    label = "HSA Vendor", status ="default",width =50,
    actionButton(inputId = "hsa_vendor_all", label = "(Un)select all",
                 class="btn btn-primary btn-sm"),
    #br(),
    checkboxGroupInput("hsa_vendor","",choices = choices,selected = choices)
  )
  
})


# major -> prefix HDA_code

output$PREFIX = renderUI({
  choices = sort(unique(  PREFIX()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()]),na.last = FALSE)
  #selectInput("prefix","Prefix",choices = choices, multiple = TRUE, selected = choices)
  
  dropdownButton(
    label = "Prefix", status ="default",width =50,
    actionButton(inputId = "prefix_all", label = "(Un)select all",
                 class="btn btn-primary btn-sm"),
    #br(),
    checkboxGroupInput("prefix","",choices = choices,selected = choices)
  )
  
})

output$HDA_CODE = renderUI({
  choices = sort(unique(  HDA_CODE()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()]),na.last = FALSE)
 # selectInput("hda_code","HDA Code",choices = choices, multiple = TRUE, selected = choices)

    dropdownButton(
      label = "HDA Code", status ="default",width =50,
      actionButton(inputId = "hda_code_all", label = "(Un)select all",
                   class="btn btn-primary btn-sm"),
      #br(),
      checkboxGroupInput("hda_code","",choices = choices,selected = choices)
    )

  
})


#HERE  added by GUI LONG start

# RAMP_CAVITY = reactive({
#   as.character(RAW()[,"RAMP_CAVITY"])
# })
output$RAMP_CAVITY = renderUI({
  choices = sort(unique(  RAMP_CAVITY()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()]),na.last = FALSE)
  # selectInput("hda_code","HDA Code",choices = choices, multiple = TRUE, selected = choices)
  
  dropdownButton(
    label = "RAMP CAVITY", status ="default",width =50,
    actionButton(inputId = "ramp_cavity_all", label = "(Un)select all",
                 class="btn btn-primary btn-sm"),
    #br(),
    checkboxGroupInput("ramp_cavity","",choices = choices,selected = choices)
  )
})
# 
# RAMP_MOLD = reactive({
#   as.character(RAW()[,"RAMP_MOLD"])
# })
output$RAMP_MOLD = renderUI({
  choices = sort(unique(  RAMP_MOLD()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()]),na.last = FALSE)
  # selectInput("hda_code","HDA Code",choices = choices, multiple = TRUE, selected = choices)
  
  dropdownButton(
    label = "RAMP MOLD", status ="default",width =50,
    actionButton(inputId = "ramp_mold_all", label = "(Un)select all",
                 class="btn btn-primary btn-sm"),
    #br(),
    checkboxGroupInput("ramp_mold","",choices = choices,selected = choices)
  )
})
# 
# MOTOR_SERIAL_NUM = reactive({
#   as.character(RAW()[,"MOTOR_SERIAL_NUM"])
# })
# output$MOTOR_SERIAL_NUM = renderUI({
#   choices = sort(unique(  MOTOR_SERIAL_NUM()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()]),na.last = FALSE)
#   # selectInput("hda_code","HDA Code",choices = choices, multiple = TRUE, selected = choices)
#   
#   dropdownButton(
#     label = "MOTOR SERIAL NUM", status ="default",width =50,
#     actionButton(inputId = "motor_serial_num_all", label = "(Un)select all",
#                  class="btn btn-primary btn-sm"),
#     #br(),
#     checkboxGroupInput("motor_serial_num","",choices = choices,selected = choices)
#   )
# })
# 
# MOTPF_SERIAL_EXTD_NUM = reactive({
#   as.character(RAW()[,"MOTPF_SERIAL_EXTD_NUM"])
# })
# output$MOTPF_SERIAL_EXTD_NUM = renderUI({
#   choices = sort(unique(  MOTPF_SERIAL_EXTD_NUM()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()]),na.last = FALSE)
#   # selectInput("hda_code","HDA Code",choices = choices, multiple = TRUE, selected = choices)
#   
#   dropdownButton(
#     label = "MOTPF SERIAL EXTD NUM", status ="default",width =50,
#     actionButton(inputId = "motor_serial_extd_all", label = "(Un)select all",
#                  class="btn btn-primary btn-sm"),
#     #br(),
#     checkboxGroupInput("motpf_serial_extd_num","",choices = choices,selected = choices)
#   )
# })
# 
# MOTPF_SHIP_DATE  = reactive({
#   as.character(RAW()[,"MOTPF_SHIP_DATE "])
# })
output$MOTPF_SHIP_DATE = renderUI({
  choices = sort(unique(  MOTPF_SHIP_DATE()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()]),na.last = FALSE)
  # selectInput("hda_code","HDA Code",choices = choices, multiple = TRUE, selected = choices)
  
  dropdownButton(
    label = "MOTPF SHIP DATE", status ="default",width =50,
    actionButton(inputId = "motpf_ship_date_all", label = "(Un)select all",
                 class="btn btn-primary btn-sm"),
    #br(),
    checkboxGroupInput("motpf_ship_date","",choices = choices,selected = choices)
  )
})
# 
# RAMP_DATE_CODE   = reactive({
#   as.character(RAW()[,"RAMP_DATE_CODE  "])
# })
output$RAMP_DATE_CODE = renderUI({
  choices = sort(unique(  RAMP_DATE_CODE()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()]),na.last = FALSE)
  # selectInput("hda_code","HDA Code",choices = choices, multiple = TRUE, selected = choices)
  
  dropdownButton(
    label = "RAMP DATE CODE", status ="default",width =50,
    actionButton(inputId = "ramp_date_code_all", label = "(Un)select all",
                 class="btn btn-primary btn-sm"),
    #br(),
    checkboxGroupInput("ramp_date_code","",choices = choices,selected = choices)
  )
})
#HERE  added by GUI LONG end


# output$SBR = renderUI({
#   choices = unique(  SBR()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()])
#   # selectInput("hda_code","HDA Code",choices = choices, multiple = TRUE, selected = choices)
#   
#   dropdownButton(
#     label = "SBR", status ="default",width =50,
#     actionButton(inputId = "sbr_all", label = "(Un)select all",
#                  class="btn btn-primary btn-sm"),
#     #br(),
#     checkboxGroupInput("sbr","",choices = choices,selected = choices)
#   )
#   
#   
# })


# observeEvent(input$hda_code_all, {
#   print(input$hda_code)
#   choices = sort(unique( HDA_CODE()[   INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()  ]) , decreasing = TRUE)
#     
#   if (is.null(input$hda_code)) {
#     print(choices)
#     updateCheckboxGroupInput(
#       session = session, inputId = "hda_code", selected = as.character(choices)
#     )
#   } else {
#     updateCheckboxGroupInput(
#       session = session, inputId = "hda_code", selected = ""
#     )
#   }
# })


bottons = c("mba_part_num","motor_base","hda_code","motor_ramp_ven","media_type","hsa_fw","hsa_rev","hsa_vendor","prefix","sbr")

#guilong = c("ramp_cavity", "ramp_mold", "motor_serial_num", "motpf_serial_extd", "motpf_ship_date", "ramp_date_code")
#guilong = c("ramp_cavity", "ramp_mold", "motpf_serial_extd", "motpf_ship_date", "ramp_date_code")
guilong = c("ramp_cavity", "ramp_mold",  "motpf_ship_date", "ramp_date_code")

bottons = c(bottons,guilong)

lapply(
  bottons,
  FUN = function(i){
    observeEvent(input[[paste0(i,"_all")]], {
      botton = paste0(i,"_all")
      
      #Y = HDA_CODE()#do.call(toupper(i))
      eval(parse(text=paste0("Y=",toupper(i),"()")))
      
      INDEX = INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE()
      choices = sort(unique( Y[INDEX]) , decreasing = TRUE)
      
      if (is.null(input[[i]])) {
        print(choices)
        updateCheckboxGroupInput(
          session = session, inputId = i, selected = as.character(choices)
        )
      } else {
        updateCheckboxGroupInput(
          session = session, inputId = i, selected = ""
        )
      }
    })
  }
)

INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE_HDA = reactive(INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE() & HDA_CODE() %in% input$hda_code)

# major -> hda_code  -> line

output$LINE = renderUI({
  choices = sort(unique(  LINE()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE_HDA()]),na.last = FALSE)
#  selectInput("line","Line",choices = choices, multiple = TRUE, selected = choices)

  dropdownButton(
    label = "Line", status ="default",width =50,
    actionButton(inputId = "line_all", label = "(Un)select all",
                 class="btn btn-primary btn-sm"),
    #br(),
    checkboxGroupInput("line","",choices = choices,selected = choices)
  )
})

bottons_2 = c("line")

lapply(
  bottons_2,
  FUN = function(i){
    observeEvent(input[[paste0(i,"_all")]], {
      botton = paste0(i,"_all")
      
      #Y = HDA_CODE()#do.call(toupper(i))
      eval(parse(text=paste0("Y=",toupper(i),"()")))
      
      INDEX = INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE_HDA()
      choices = sort(unique( Y[INDEX]) , decreasing = TRUE)
      
      if (is.null(input[[i]])) {
        print(choices)
        updateCheckboxGroupInput(
          session = session, inputId = i, selected = as.character(choices)
        )
      } else {
        updateCheckboxGroupInput(
          session = session, inputId = i, selected = ""
        )
      }
    })
  }
)


INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE_HDA_LINE = reactive(INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE_HDA() & LINE() %in% input$line)


# major -> hda_code -> line -> rpi hsi csi

output$RPI = renderUI({
  choices = sort(unique(  RPI()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE_HDA_LINE()]),na.last = FALSE)
#  selectInput("rpi","RPI",choices = choices, multiple = TRUE, selected = choices)  
  
  dropdownButton(
    label = "RPI", status ="default",width =50,
    actionButton(inputId = "rpi_all", label = "(Un)select all",
                 class="btn btn-primary btn-sm"),
    #br(),
    checkboxGroupInput("rpi","",choices = choices,selected = choices)
  )
  
})

output$HSI = renderUI({
  choices = sort(unique(  HSI()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE_HDA_LINE()]),na.last = FALSE)
#  selectInput("hsi","HSI",choices = choices, multiple = TRUE, selected = choices)  
  
  dropdownButton(
    label = "HSI", status ="default",width =50,
    actionButton(inputId = "hsi_all", label = "(Un)select all",
                 class="btn btn-primary btn-sm"),
    #br(),
    checkboxGroupInput("hsi","",choices = choices,selected = choices)
  )
})

output$CSI = renderUI({
  choices =sort( unique(  CSI()[INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE_HDA_LINE()]),na.last = FALSE)
#  selectInput("csi","CSI",choices = choices, multiple = TRUE, selected = choices)  
  
  dropdownButton(
    label = "CSI", status ="default",width =50,
    actionButton(inputId = "csi_all", label = "(Un)select all",
                 class="btn btn-primary btn-sm"),
    #br(),
    checkboxGroupInput("csi","",choices = choices,selected = choices)
  )
})

bottons_3 = c("rpi","hsi","csi")

lapply(
  bottons_3,
  FUN = function(i){
    observeEvent(input[[paste0(i,"_all")]], {
      botton = paste0(i,"_all")
      
      #Y = HDA_CODE()#do.call(toupper(i))
      eval(parse(text=paste0("Y=",toupper(i),"()")))
      
      INDEX = INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE_HDA_LINE()
      choices = sort(unique( Y[INDEX]) , decreasing = TRUE)
      
      if (is.null(input[[i]])) {
        print(choices)
        updateCheckboxGroupInput(
          session = session, inputId = i, selected = as.character(choices)
        )
      } else {
        updateCheckboxGroupInput(
          session = session, inputId = i, selected = ""
        )
      }
    })
  }
)


INDEX = reactive({
  
#   cat("-=-=-=-=-=-=-=-=-=-=-=-=-=-")
#   print(input$motpf_serial_extd_num)
#   print(sum(MOTPF_SERIAL_EXTD_NUM() %in% input$motpf_serial_extd_num))
#   cat("-=-=-=-=-=-=-=-=-=-=-=-=-=-")
  
  INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE_HDA_LINE() &
  INDEX_PRODUCT_SITE_CAPACITY_RUNTYPE() &  # data range applied
    MBA_PART_NUM() %in% input$mba_part_num &
    MOTOR_BASE() %in% input$motor_base &
    MOTOR_RAMP_VEN() %in% input$motor_ramp_ven &
    MEDIA_TYPE() %in% input$media_type &
    HSA_FW() %in% input$hsa_fw &
     HSA_REV() %in% input$hsa_rev &
     HSA_VENDOR() %in% input$hsa_vendor &
    RPI() %in% input$rpi &
    HSI() %in% input$hsi &
    CSI() %in% input$csi &
    
    PREFIX() %in% input$prefix &
    
    SBR() %in% input$sbr &
    
    RAMP_CAVITY() %in% input$ramp_cavity &
    RAMP_MOLD() %in% input$ramp_mold &
     #MOTPF_SERIAL_EXTD_NUM() %in% input$motpf_serial_extd_num &
     RAMP_DATE_CODE() %in% input$ramp_date_code &
     MOTPF_SHIP_DATE() %in% input$motpf_ship_date
  
  
})

output$seg_text = renderText({
  LEVELS = sapply(RAW(), function(x) length(unique(x)))
  n = sum(LEVELS==1)
  m = sum(sapply(RAW(),function(x) all(is.na(x))))    
  
  
  LEVELS2 = sapply(raw(), function(x) length(unique(x)))
  n2 = sum(LEVELS2==1)
  m2 = sum(sapply(raw(),function(x) all(is.na(x))))    
  
  
  
  paste("The raw file contains",nrow(RAW_file()),"rows",ncol(RAW_file()),"cols(",
        n,"all-identical-value cols,",
        m,"all-missing-value cols). ",
        "The segmentated file contains",nrow(raw()),"rows",ncol(raw())-5,"cols(",
        n2,"all-identical-value cols,",
        m2,"all-missing-value cols). "
        
        
  )
})

# 
# 
# 
# 
# 
# 
# output$LINE = renderUI({
#   choices = unique( LINE()[INDEX_PRODUCT_CAPACITY_RUNTYPE()])
#   selectInput("line","Line",choices = choices)
# })
# 
# INDEX_PRODUCT_CAPACITY_RUNTYPE_LINE = reactive({
#   INDEX_PRODUCT_CAPACITY_RUNTYPE() & (LINE() %in% input$line)
# })
# 
# output$MBA_PART_NUM = renderUI({
#   choices = unique(  MBA_PART_NUM()[INDEX_PRODUCT_CAPACITY_RUNTYPE_LINE()])
#   selectInput("mba_part_num","MBA Part Num",choices = choices, multiple = TRUE, selected = choices[1])
#   
# })
# 
# output$MOTOR_BASE = renderUI({
#   choices = unique(  MOTOR_BASE()[INDEX_PRODUCT_CAPACITY_RUNTYPE_LINE()])
#   selectInput("motor_base","Motor Base",choices = choices, multiple = TRUE, selected = choices[1])
#   
# })
# 
# output$MOTOR_RAMP_VEN = renderUI({
#   choices = unique(  MOTOR_RAMP_VEN()[INDEX_PRODUCT_CAPACITY_RUNTYPE_LINE()])
#   selectInput("motor_ramp_ven","Motor Ramp Ven",choices = choices, multiple = TRUE, selected = choices[1])
#   
# })
# 
# INDEX_MBA_PART_NUM = reactive({ MBA_PART_NUM() %in% input$mba_part_num })
# 
# INDEX_MOTOR_BASE = reactive({ MOTOR_BASE() %in% input$motor_base })
# 
# INDEX_MOTOR_RAMP_VEN = reactive({ MOTOR_RAMP_VEN() %in% input$motor_ramp_ven})
# 
# INDEX = reactive({
#   INDEX_PRODUCT_CAPACITY_RUNTYPE_LINE() &
#     INDEX_MBA_PART_NUM() &
#     INDEX_MOTOR_BASE() &
#     INDEX_MOTOR_RAMP_VEN()
# })
# 
# 
# 
# ###################   multi
# 
# ATTR_list = sort(c("FAIL_CODE", "FAIL_CODE_GROUP", "LINE_NUM", "RPI_ID", "HDA_CODE", 
#               "HD_STACK_ID", "MBA_PART_NUM", "MOTOR_BASE", "MEDIA_TYPE", "MOTOR_RAMP_VEN", 
#               "MOTOR_VEND_ID", "HSA_FW", "HSA_REV", "HSA_VENDOR", "CLRM_EXIT_DATE", 
#               "MOTOR_SERIAL_NUM", "PRDCT_PRODUCT_FORMAT_CAPACITY", "CLAMP_SCREW_ID", 
#               "RUN_TYPE_PRIME_REWORK", "PRODUCT_NAME","FISCAL_YEAR_WEEK"))




output$MULTI_PARA = renderUI({
  
  selectInput("multi_para","Parametric",choices = multi_Para_list)
  
})

output$MULTI_ATTR = renderUI({
  
  choices = ATTR_list
  
#   X = RAW()[,ATTR_list]
#   y = ifelse(is.na(FC()),0,1)
#   D = names(data_ranked_ANOVA(X, y)$P)
  
  selectInput("multi_attr","Attribute",choices = ATTR_list, selected = "CLRM_EXIT_DATE"  )
  
  
})
output$MULTI_FACET_1 = renderUI({
  
#  choices = ifelse(input$multi_attr_1=="None", "None", ATTR_list)
  
  choices = c("None",ATTR_list)
  
  selectInput("multi_facet_1","Facet 1",choices = choices, selected = "None"  )
  
  
})

output$MULTI_FACET_2 = renderUI({
#  choices = ifelse(input$multi_attr_2=="None", "None", ATTR_list)
  choices = c("None",ATTR_list)
  
  selectInput("multi_facet_2","Facet 2",choices = choices, selected = "None"  )
  
  
})

output$MULTI_FACET_3 = renderUI({
  #  choices = ifelse(input$multi_attr_2=="None", "None", ATTR_list)
  choices = c("None",ATTR_list)
  
  selectInput("multi_facet_3","Facet 3",choices = choices, selected = "None"  )
  
  
})

output$MULTI_FACET_4 = renderUI({
  #  choices = ifelse(input$multi_attr_2=="None", "None", ATTR_list)
  choices = c("None",ATTR_list)
  
  selectInput("multi_facet_4","Facet 4",choices = choices, selected = "None"  )
  
  
})

output$MULTI_FACET_5 = renderUI({
  #  choices = ifelse(input$multi_attr_2=="None", "None", ATTR_list)
  choices = c("None",ATTR_list)
  
  selectInput("multi_facet_5","Facet 5",choices = choices, selected = "None"  )
  
  
})

output$MULTI_FACET_6 = renderUI({
  #  choices = ifelse(input$multi_attr_2=="None", "None", ATTR_list)
  choices = c("None",ATTR_list)
  
  selectInput("multi_facet_6","Facet 6",choices = choices, selected = "None"  )
  
  
})
