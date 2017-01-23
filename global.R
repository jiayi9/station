# InsPack <- function(pack) { 
#   if (!pack %in% rownames(installed.packages())) { 
#     print(paste("installing",pack)) 
#     install.packages(pack) 
#   } else {
#     print(paste(pack," already installed")) 
#   }
# }
# InsPack("ggplot2")
# InsPack("DT")

#InsPack("mvoutlier")
library(ggplot2)
library(DT)
library(markdown)


options(shiny.maxRequestSize=100*1024^2)
options(warn=-1)
#options(scipen=99)

FIVE = c("RAMP_TO_DISC","RAMP_Z","DISC_Z","ARM_TO_DISC","ARM_Z")

# CERT_list = c("H1_RAMP_CYL", "H1_CRASH_STOP_CYL", "H1_TRK_0_CYL", "H1_PHYS_MAX_CYL",
#               "H0_RAMP_CYL", "H0_CRASH_STOP_CYL", "H0_TRK_0_CYL", "H0_PHYS_MAX_CYL")

ATTR_list = sort(c("FAIL_CODE", "LINE_NUM", "RPI_ID", "HDA_CODE", 
              "HD_STACK_ID", "MBA_PART_NUM", "MOTOR_BASE", "MEDIA_TYPE", "MOTOR_RAMP_VEN", 
              "MOTOR_VEND_ID", "HSA_FW", "HSA_REV", "HSA_VENDOR", 
               "PRODUCT_FORMAT_CAPACITY", "CLAMP_SCREW_ID", 
              "RUN_TYPE_PRIME_REWORK", "PRODUCT_INTERNAL_NAME", "CLRM_EXIT_DATE","FMS_FISCAL_YEAR_WEEK",
              "RAMP_CAVITY","RAMP_MOLD","MOTPF_SERIAL_EXTD_NUM","MOTPF_SHIP_DATE","RAMP_DATE_CODE",
              "DRIVE_SBR_NUM"
              
              ))

contri_ATTR_list = sort(c("FAIL_CODE", "LINE_NUM", "RPI_ID", "HDA_CODE", 
                   "HD_STACK_ID", "MBA_PART_NUM", "MOTOR_BASE", "MEDIA_TYPE", "MOTOR_RAMP_VEN", 
                   "MOTOR_VEND_ID", "HSA_FW", "HSA_REV", "HSA_VENDOR", 
                    "PRODUCT_FORMAT_CAPACITY", "CLAMP_SCREW_ID", 
                   "RUN_TYPE_PRIME_REWORK", "PRODUCT_INTERNAL_NAME",
                   "RAMP_CAVITY","RAMP_MOLD","MOTPF_SHIP_DATE","RAMP_DATE_CODE"
                   
                   ))

PARA_list = c(
  "RAMP_TO_DISC", "ARM_TO_DISC",
  "ARM_Z", "RAMP_Z", "DISC_Z"
  # "RAMP_Z8", "ARM_Z1", 
  # "ARM_Z2", "ARM_Z3", "ARM_Z4", "ARM_Z5", "DISC_Z6", "DISC_Z7", 
  # "SUSPENSION_Z9", "SUSPENSION_Z10"
  )