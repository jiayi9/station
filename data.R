
#R = read.csv("D:/FMS/FMS_3/new3.csv")
# FMS = R[,c(10,11,12,23,24)]
# GROUPS = R[,c(27,28,30)]
# ABS = R[,c("ARM_Z","RAMP_Z","DISC_Z")]
# REL = R[,c("ARM_TO_DISC","RAMP_TO_DISC")]

# if(exists("ResultsPath")){
# 
#   env <- unlist(strsplit(ResultsPath ,"/"))[3]
#   if (env == "sasdata"){
#     udv_job = "E1051220"
#     
#   } else if (env == "sasdata-stage") {
#   #  udv_job = "E7011636"
#     udv_job = "E7011802"
#   } else {
#     udv_job = "E7011802"
#     
#   }
# }




RAW_file = reactive({
  withProgress(message = "loading data",{
#   filename = "file.csv"
#   R = read.csv(filename,stringsAsFactors=FALSE)
  
#    udv_job = "E7011461"
#     if(exists("ResultsPath")){
#       Results<-paste(ResultsPath, udv_job, sep = "/")
#       rdaFile <- file.path(Results, "all.rda")
#       R = readRDS(rdaFile)
#     } else {
#       R = readRDS("all.rda")
#     }
    R = readRDS("sample_data.rda")
  })
    library(reshape2)
  withProgress(message = "Splitting columns by Head Num",{
    print(dim(R))
    
    R = dcast(R, formula(
        paste(
          paste( setdiff(names(R),c("RAMP_CYL","P185C_HD_PHYS_PSN")),collapse = "+"),
          "~",
          "P185C_HD_PHYS_PSN"
          )
        ),
        value.var = "RAMP_CYL"
    )
  
    if("0" %in% names(R)) { names(R)[names(R)=="0"] = "H0_RAMP_CYL"}
    if("1" %in% names(R)) { names(R)[names(R)=="1"] = "H1_RAMP_CYL"}
    if("2" %in% names(R)) { names(R)[names(R)=="2"] = "H2_RAMP_CYL"}
    if("3" %in% names(R)) { names(R)[names(R)=="3"] = "H3_RAMP_CYL"}

    
  NAMES = c("DRIVE_SERIAL_NUM","DRIVE_SN","FAIL_CODE",
            "PRODUCT_INTERNAL_NAME","PRODUCT_FORMAT_CAPACITY","RUN_TYPE_PRIME_REWORK","LINE_NUM",
            "CLRM_EXIT_DATE","FMS_FISCAL_YEAR_WEEK",
            "RPI_ID","CLAMP_SCREW_ID","HD_STACK_ID",
            "RAMP_TO_DISC",
            "RAMP_Z",
            "DISC_Z",
            "ARM_TO_DISC",
            "ARM_Z",
#            "ARM_Z1", "ARM_Z2", "ARM_Z3", "ARM_Z4", "ARM_Z5", 
#            "DISC_Z6", "DISC_Z7", "RAMP_Z8","SUSPENSION_Z9", "SUSPENSION_Z10",
            "MBA_PART_NUM","MOTOR_VEND_ID","MOTOR_BASE","MOTOR_SERIAL_NUM"
            
            )
  
  R = forwardCols(R,NAMES,Order = "asInForward")
  
  R = R[!is.na(R$CLRM_EXIT_DATE) & !is.na(R$FMS_FISCAL_YEAR_WEEK),]
  
#   toCharNames = c("LINE_NUM","CLRM_EXIT_DATE","MBA_PART_NUM","HSA_FW","HDA_CODE",
#                   "RAMP_CAVITY","RAMP_MOLD","MOTPF_SERIAL_EXTD_NUM","MOTPF_SHIP_DATE","RAMP_DATE_CODE")
  R = toChar(R, ATTR_list)
  
  #print(names(R))
  
  R[,ATTR_list][is.na(R[,ATTR_list])] ="N/A"

#   R$LINE_NUM = as.character(R$LINE_NUM)
#   R$CLRM_EXIT_DATE = as.character(R$CLRM_EXIT_DATE)
#   R$MBA_PART_NUM = as.character(R$MBA_PART_NUM)
#   R$HSA_FW = as.character(R$HSA_FW)
#   R$HDA_CODE = as.character(R$HDA_CODE)
  
  if( all(c("H0_RAMP_CYL","H1_RAMP_CYL") %in% names(R))  ){
    R$H1_H0_DELTA_RAMP_CYL = R[,"H1_RAMP_CYL"] - R[,"H0_RAMP_CYL"]  
  }
  
  if( all(c("H2_RAMP_CYL","H3_RAMP_CYL") %in% names(R))  ){
    R$H3_H2_DELTA_RAMP_CYL = R[,"H3_RAMP_CYL"] - R[,"H2_RAMP_CYL"]  
  }
  

  print_summary(R,"RAW_file")
  })
  R$DRIVE_SERIAL_NUM = R$DRIVE_SN
  R$FLANGE_HEIGHT_AVG = R$MOTPF_FLANGE_HEIGHT_AVG
  R$FLANGE_HEIGHT_PARALLEL = R$MOTPF_FLANGE_HEIGHT_PARALLEL
  

  
  R
})

extremeINDEX = reactive({
  withProgress(message ="Checking extreme values" ,{
  D = RAW_file()[,FIVE]
  INDEX = extremeDetectIndex(D,input$z)
#   print(table(INDEX))
#   print(nrow(D))
#   print(sum(is.na(INDEX)))
  })
  INDEX
  
})

output$extreme_summary = renderDataTable({
  D = RAW_file()[,FIVE]
  
  X = data.frame(matrix(0,3,5))
  colnames(X) = FIVE
  rownames(X) = c("Mean","UCL","LCL")
  X[1,] = sapply(D,function(x) mean(x,na.rm = TRUE))
  X[2,] = X[1,] + input$z*sapply(D,function(x) sd(x,na.rm = TRUE))
  X[3,] = X[1,] - input$z*sapply(D,function(x) sd(x,na.rm = TRUE))
  
  INDEX = extremeDetectIndex(D,input$z)
  INDEX[is.na(INDEX)] = FALSE
  X2 = D[INDEX,,drop=FALSE]
  
  Rownames =   DSN_RAW_file()[INDEX]   
  
  print(X2)
  print(Rownames)
  
  INDEX_2 = !duplicated(Rownames)
  
  Rownames_2 = Rownames[INDEX_2]
  X2 = X2[INDEX_2,]
  
  print(X2)
  print(Rownames_2)
  
  rownames(X2) = Rownames_2
  
#  rownames(X2) = DSN()[INDEX]
  
  R = rbind(X,X2)
  R =round(R,2)
  
  print(R)
  print(R[1,1])
  
  datatable(R, 
            
            caption = htmltools::tags$caption(
              style = 'caption-side: bottom; text-align: center;',
              'Table 1: ', htmltools::em('Summary')
            ),
            #selection =list(target ='row',mode="single"),
            #selection ="row",
            options = list(                paging = FALSE,
                                           ordering = FALSE,
                                           filtering = FALSE,
                                           searching =FALSE,
                                           info = FALSE)
  ) %>%
    formatStyle(
      "RAMP_TO_DISC",
      target = "row",
      color =  styleEqual(c(R[1,1],R[2,1],R[3,1]), c('red',"blue","blue"))
    )
})

# output$extreme_case_table = renderDataTable({
#   D = RAW_file()[,FIVE]
#   INDEX = extremeDetectIndex(D,input$z)
#   INDEX[is.na(INDEX)] = FALSE
#   X = D[INDEX,,drop=FALSE]
#   rownames(X) = DSN()[INDEX]
#   datatable(X, 
#             
#             caption = htmltools::tags$caption(
#               style = 'caption-side: bottom; text-align: center;',
#               'Table 2: ', htmltools::em('Extreme value cases')
#             ),
#             selection =list(target ='cell',mode="single"),
#             options = list(                paging = FALSE,
#                                            ordering = FALSE,
#                                            filtering = FALSE,
#                                            searching =FALSE)
#   ) 
# })

RAW = reactive({
  R = RAW_file()
  if(input$outlierTreatment =="Exclude from analysis"){
    #print(sum(extremeINDEX()))
    R = R[!extremeINDEX(),]
  } 
  print_summary(R, "RAW")
  R #= data.frame(R,selected = vals$keeprows)
})



PRODUCT = reactive({
  RAW()[,"PRODUCT_INTERNAL_NAME"]
})

SITE = reactive({
  RAW()[,"SITE"]
})

CAPACITY = reactive({
  as.character(RAW()[,"PRODUCT_FORMAT_CAPACITY"])
})

RUNTYPE = reactive({
  RAW()[,"RUN_TYPE_PRIME_REWORK"]
})


MEDIA_TYPE = reactive({
  RAW()[,"MEDIA_TYPE"]
})

HSA_FW = reactive({
  RAW()[,"HSA_FW"]
})

HSA_REV = reactive({
  RAW()[,"HSA_REV"]
})

HSA_VENDOR = reactive({
  RAW()[,"HSA_VENDOR"]
})



LINE = reactive({
  RAW()[,"LINE_NUM"]
})

MBA_PART_NUM = reactive({
  RAW()[,"MBA_PART_NUM"]
})

MOTOR_BASE = reactive({
  RAW()[,"MOTOR_BASE"]
})

MOTOR_RAMP_VEN = reactive({
  RAW()[,"MOTOR_RAMP_VEN"]
})


# guilong request start



RAMP_CAVITY = reactive({
  as.character(RAW()[,"RAMP_CAVITY"])
})

RAMP_MOLD = reactive({
  as.character(RAW()[,"RAMP_MOLD"])
})

MOTOR_SERIAL_NUM = reactive({
  as.character(RAW()[,"MOTOR_SERIAL_NUM"])
})

MOTPF_SERIAL_EXTD_NUM = reactive({
  as.character(RAW()[,"MOTPF_SERIAL_EXTD_NUM"])
})

MOTPF_SHIP_DATE  = reactive({
  as.character(RAW()[,"MOTPF_SHIP_DATE"])
})

RAMP_DATE_CODE   = reactive({
  as.character(RAW()[,"RAMP_DATE_CODE"])
})
# guilong request end



HSI = reactive({
  RAW()[,"HD_STACK_ID"]
})

RPI = reactive({
  RAW()[,"RPI_ID"]
})

CSI = reactive({
  RAW()[,"CLAMP_SCREW_ID"]
})

DAY = reactive({
  RAW()[,"CLRM_EXIT_DATE"]
})

WEEK = reactive({
  RAW()[,"FMS_FISCAL_YEAR_WEEK"]  
})

SBR = reactive({
  RAW()[,"DRIVE_SBR_NUM"]
})

DATE = reactive({
  if(input$by == "Day"){
    R = DAY()
  } else {
    R = WEEK()
  }
  R
})

DSN = reactive({
  RAW()[,"DRIVE_SERIAL_NUM"]
})
DSN_RAW_file = reactive({
  RAW_file()[,"DRIVE_SERIAL_NUM"]
})

FAIL_CODE = reactive({
  RAW()[,"FAIL_CODE"]
})

HDA_CODE = reactive({
  RAW()[,"HDA_CODE"]
})

PREFIX = reactive({
  RAW()[,"PREFIX"]
})

ABS = reactive({
  R = RAW()[, names(RAW()) %in% c("ARM_Z","RAMP_Z","DISC_Z")]
  print_summary(R,"ABS")
  cat("-----------------------------\n")
  
  R
})
 
REL = reactive({
  R = RAW()[, names(RAW()) %in% c("ARM_TO_DISC","RAMP_TO_DISC")]
  print_summary(R,"REL")
  cat("-----------------------------\n")
  
  R
})

#####################  Index subsetted #######################


raw = eventReactive(input$ok,{
#raw = reactive({  

  validate(need(sum(INDEX())>0 ,"There are 0 rows in the subsetted data - No rows selected."))
  R = data.frame(RAW()[ INDEX() , ], ABS_PC(),REL_PC())
  validate(need(
    length(unique(date())) >= input$rollN    ,
    paste("Select more days/weeks or Decrease the MA rolling basis.\nYou have selected data of only",length(unique(R[,BY()])), 
          paste0(input$by,"s"))
    ))
  R
})

ABS_PC = reactive({
  
  ABS_train = na.omit(abs())
  
  
  validate(need(
    all(sapply(ABS_train,function(x) length(unique(x))>1))
    ,
    "There is constant columns in the data"
  ))
  
  fit = prcomp(ABS_train,center=TRUE,scale.=TRUE)
  
  PC = data.frame(as.matrix(scale(abs(),center=TRUE,scale=TRUE))%*%fit$rotation)
  
  print_summary(data.frame(PC),"ABS_PC")
  
  tryCatch({
    names(PC) = paste0("ABS_PC_",1:3)
    },error=function(e){})
  
  PC
  
})

REL_PC = reactive({
  
  REL_train = na.omit(rel())
  
  
  validate(need(
    all(sapply(REL_train,function(x) length(unique(x))>1))
    ,
    "There is constant columns in the data"
  ))
  
  fit = prcomp(REL_train,center=TRUE,scale.=TRUE)
  
  PC = data.frame(as.matrix(scale(rel(),center=TRUE,scale=TRUE))%*%fit$rotation)
  
  print_summary(data.frame(PC),"REL_PC")
  
  tryCatch({
    names(PC) = paste0("REL_PC_",1:2)
  },error=function(e){})
  
  PC
  
})

fms = reactive({
  Cols = c(
          "RAMP_TO_DISC",
          "RAMP_Z",
          "DISC_Z",
          "ARM_TO_DISC",
          "ARM_Z"
#           ,
#           "ARM_Z1", "ARM_Z2", "ARM_Z3", "ARM_Z4", "ARM_Z5", 
#           "DISC_Z6", "DISC_Z7", "RAMP_Z8","SUSPENSION_Z9", "SUSPENSION_Z10"
           )
  R = raw()[,Cols]
  R
})

# cert = reactive({
#   Cols = c(
#     "H0_RAMP_CYL", "H0_CRASH_STOP_CYL", "H0_TRK_0_CYL", 
#     "H0_PHYS_MAX_CYL", "H1_RAMP_CYL", "H1_CRASH_STOP_CYL", "H1_TRK_0_CYL", 
#     "H1_PHYS_MAX_CYL"
#     
#   )
#   R = raw()[,Cols]
#   R
# })


upstream = reactive({
  Cols = c(
    "FLANGE_HEIGHT_AVG",
    "FLANGE_HEIGHT_PARALLEL"
    )
})

hsi = reactive({
  HSI()[INDEX()]
})

csi = reactive({
  CSI()[INDEX()]
})

rpi = reactive({
  RPI()[INDEX()]
})


date = reactive({
  DATE()[INDEX()]
})


abs = reactive({
  ABS()[INDEX(),]
})

rel = reactive({
  REL()[INDEX(),]
})


dsn = reactive({
  DSN()[INDEX()]
})

line = reactive({
  LINE()[INDEX()]
})





####################################################################
# 

# 
# 
# 
# 
# FMS = reactive({
#   X = RAW()
#   R = X[,c(10,11,12,23,24)]
#   print_summary(R,"FMS")
#   cat("-----------------------------\n")
#   
#   R
# })
# 
# 
# HSI = reactive({
#   R = RAW()$HD_STACK_ID
#   print("HSI")
#   print(table(R))
#   cat("-----------------------------\n")
#   
#   R
# })
# 
# RPI = reactive({
#   R = RAW()$RPI_ID
#   print("RPI")
#   print(table(R))
#   cat("-----------------------------\n")
#   
#   R
# })
# 
# LINE = reactive({
#   R = RAW()$LINE_NUM
#   print("LINE")
#   print(table(R))
#   cat("-----------------------------\n")
#   
#   R
# })
# 
# DSN = reactive({
#   R = as.character(RAW()$SERIAL_NUM)
#   print("Drive Serial Num")
#   #print(table(R))
#   cat("-----------------------------\n")
#   
#   R
#   
# })
# 

# 
# 
# DATE = reactive({
#   R = as.character( RAW()$CLRM_EXIT_DATE )
#   print("CLRM_EXIT_DATE")
#   #print(table(R))
#   R
# })
# 
# 
# GROUPS = reactive({
#   R = data.frame(
#     LINE=LINE(),
#     HSI=HSI(),
#     RPI=RPI()
#     )
#   print_summary(R,"GROUPS")
#   cat("-----------------------------\n")
#   
#   R[] = lapply(R,as.character)
#   R
# })
# 
# 
