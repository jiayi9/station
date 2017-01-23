
x = readRDS("D:/FMS_Dashboard_test/all.rda")

y = x[1:10000,]

PARA_list = c(
  "RAMP_TO_DISC", "ARM_TO_DISC",
  "ARM_Z", "RAMP_Z", "DISC_Z")

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




for(i in multi_Para_list){
  y[,i] = rnorm(nrow(y),rpois(1,20), rpois(1,20)*2)
}





ATTR_list = sort(c("FAIL_CODE", "LINE_NUM", "RPI_ID", "HDA_CODE", 
                   "HD_STACK_ID", "MBA_PART_NUM", "MOTOR_BASE", "MEDIA_TYPE", "MOTOR_RAMP_VEN", 
                   "MOTOR_VEND_ID", "HSA_FW", "HSA_REV", "HSA_VENDOR", 
                   "CLAMP_SCREW_ID", 
                   "RUN_TYPE_PRIME_REWORK",
                   "RAMP_CAVITY","RAMP_MOLD","MOTPF_SERIAL_EXTD_NUM","MOTPF_SHIP_DATE","RAMP_DATE_CODE",
                   "DRIVE_SBR_NUM"
                   
))

y$PRODUCT_INTERNAL_NAME = "Product001"

y$PRODUCT_FORMAT_CAPACITY = "12306"

y$DRIVE_SN = paste0("part_",1:nrow(y))

for(j in ATTR_list){
  y[,j] = sample(LETTERS[1:10],size = nrow(y),replace = TRUE)
}

saveRDS(y,file = "D:/FMS_Dashboard_github/sample_data.rda")