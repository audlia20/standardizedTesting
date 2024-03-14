
advancedMath <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Advanced Mathematics.csv")
advancedPlacement <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Advanced Placement.csv")
algebraI <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Algebra I.csv")
algebraII <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Algebra II.csv")
biology <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Biology.csv")
calculus <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Calculus.csv")
chemistry <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Chemistry.csv")
computerScience <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Computer Science.csv")
corporalPunishment <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Corporal Punishment.csv")
covidDirectionalIndicators <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/COVID Directional Indicators.csv")
dualEnrollment <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Dual Enrollment.csv")
enrollment <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Enrollment.csv")
expulsions <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Expulsions.csv")
geometry <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Geometry.csv")
giftedAndTalented <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Gifted and Talented.csv")
harassmentAndBullying <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Harassment and Bullying.csv")
internationalBaccalaureate <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/International Baccalaureate.csv")
internetAccessAndDevices <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Internet Access and Devices.csv")
justiceFacilities <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Justice Facilities.csv")
offenses <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Offenses.csv")
physics <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Physics.csv")
referralsAndArrests <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Referrals and Arrests.csv")
restraintAndSeclusion <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Restraint and Seclusion.csv")
retention <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Retention.csv")
satAndAct <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/SAT and ACT.csv")
schoolCharacteristics <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/School Characteristics.csv")
schoolSupport <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/School Support.csv")
singleSexAthletics <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Single sex Athletics.csv")
singleSexClasses <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Single sex Classes.csv")
suspensions <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Suspensions.csv")
transfers <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/CRDC/School/Transfers.csv")
absenteeism <- read.csv("/Users/audreyliang/Downloads/2020-21-crdc-data/EDFacts/FS195 DG814/ID 814 SCH - Chronic Absenteeism.csv")
names(absenteeism)[names(absenteeism) == "SCHOOL_NAME"] <- "SCH_NAME"
# Assuming you have your data frames loaded as mentioned earlier
df_list <- list(advancedMath, advancedPlacement, algebraI, algebraII, biology, calculus, chemistry,
                computerScience, corporalPunishment, covidDirectionalIndicators, dualEnrollment,
                enrollment, expulsions, geometry, giftedAndTalented, harassmentAndBullying,
                internationalBaccalaureate, internetAccessAndDevices, justiceFacilities, offenses,
                physics, referralsAndArrests, restraintAndSeclusion, retention, satAndAct,
                schoolCharacteristics, schoolSupport, singleSexAthletics, singleSexClasses,
                suspensions, transfers, absenteeism)
key_columns <- c("LEA_STATE", "LEA_STATE_NAME", "LEAID", "LEA_NAME", "SCHID", "SCH_NAME")


all_data_merged <- Reduce(function(x, y) merge(x, y, by = key_columns, all = TRUE), df_list)


columns_to_encode <- setdiff(names(all_data_merged), key_columns)


all_data_merged[columns_to_encode] <- lapply(all_data_merged[columns_to_encode], function(x) {
  x[x < 0] <- NA
  return(x)
})


standardized <- c("TOT_SATACT_M", "TOT_SATACT_F", "TOT_ENR_F", "TOT_ENR_M") # replace with actual column names


rows_with_complete_cases <- complete.cases(all_data_merged[, standardized])


cleaned_data <- all_data_merged[rows_with_complete_cases, ]
cleaned_data <- cleaned_data[cleaned_data$TOT_SATACT_F>0 & cleaned_data$TOT_SATACT_M>0 & cleaned_data$TOT_ENR_F>0 & cleaned_data$TOT_ENR_M>0,]
nrow(cleaned_data)
summary(cleaned_data$TOT_SATACT_M)



filtered_data <- cleaned_data[cleaned_data$SCH_GRADE_PS =="No" & cleaned_data$SCH_GRADE_KG == "No" & cleaned_data$SCH_GRADE_G01 == "No" &
                      cleaned_data$SCH_GRADE_G02 == "No" & cleaned_data$SCH_GRADE_G03 == "No" & cleaned_data$SCH_GRADE_G04 == "No" & 
                      cleaned_data$SCH_GRADE_G05 == "No" & cleaned_data$SCH_GRADE_G06 == "No" & cleaned_data$SCH_GRADE_G07 == "No" &
                      cleaned_data$SCH_GRADE_G08 == "No" & cleaned_data$SCH_GRADE_G09 == "Yes" & cleaned_data$SCH_GRADE_G10 == "Yes" 
                    & cleaned_data$SCH_GRADE_G11 == "Yes" & cleaned_data$SCH_GRADE_G12 == "Yes",]



nrow(filtered_data) #12141
summary(filtered_data$TOT_SATACT_M)

filtered_data$TOT_ENR_TOTAL = filtered_data$TOT_ENR_F + filtered_data$TOT_ENR_M

filtered_data$tested <- (filtered_data$TOT_SATACT_M + filtered_data$TOT_SATACT_F)/filtered_data$TOT_ENR_TOTAL
summary(filtered_data$tested)

filtered_data <- filtered_data[filtered_data$tested<=1,]
nrow(filtered_data) #12135
summary(filtered_data$tested)

#total number to include in analysis, encode 0 and then add up m/f, divide these all by total enrollment. remove rows that contain all zero
summary(filtered_data$TOT_MATHENR_ADVM_M) #1688/13997  adanced math encode 0
summary(filtered_data$TOT_MATHENR_ADVM_F) #1694/13997 
summary(filtered_data$TOT_APENR_M) #3740/13997 ap course participation, encode 0
summary(filtered_data$TOT_APENR_F) #3767/13997 
summary(filtered_data$TOT_MATHENR_ALG2_M) #399/13997 algebra 2 participation, encode 0
summary(filtered_data$TOT_MATHENR_ALG2_F) #409/13997
summary(filtered_data$TOT_SCIENR_BIOL_M) #279/13997 biology participation, encode 0
summary(filtered_data$TOT_SCIENR_BIOL_F) #294/13997
summary(filtered_data$TOT_MATHENR_CALC_M) #4341/13997 calculus participation, encode 0
summary(filtered_data$TOT_MATHENR_CALC_F)#4333/13997
summary(filtered_data$TOT_SCIENR_CHEM_M) #944/13997 chemistry participation, encode 0
summary(filtered_data$TOT_SCIENR_CHEM_F) #950/13997
summary(filtered_data$TOT_COMPENR_CSCI_M) #4770/13997 computer science participation, encode 0
summary(filtered_data$TOT_COMPENR_CSCI_F)#4770/13997
summary(filtered_data$TOT_DUAL_M) #2178/13997 dual enrollment, encode 0
summary(filtered_data$TOT_DUAL_F) #2191/13997
summary(filtered_data$TOT_LEPPROGENR_M) #219/13997 LEP Programs, encode 0
summary(filtered_data$TOT_LEPPROGENR_F) #148/13997
summary(filtered_data$TOT_IDEAENR_M) #77/13997 Students with disabilities served under IDEA, encode 0
summary(filtered_data$TOT_IDEAENR_F) #67/13997
summary(filtered_data$TOT_MATHENR_GEOM_M) #423/13997 geometry
summary(filtered_data$TOT_MATHENR_GEOM_F)#426/13997
summary(filtered_data$TOT_MATHENR_GEOM_M) #423/13997 geometry
summary(filtered_data$TOT_MATHENR_GEOM_F)#426/13997
summary(filtered_data$TOT_HBREPORTED_SEX_M) #1468/13997 Students reported as harassed or bullied on the basis of sex
summary(filtered_data$TOT_HBREPORTED_SEX_F)#1468/13997
summary(filtered_data$TOT_HBREPORTED_RAC_M) #1469/13997 Students reported as harassed or bullied on the basis of sex
summary(filtered_data$TOT_HBREPORTED_RAC_F)#1469/13997
summary(filtered_data$TOT_HBREPORTED_DIS_M) #1466/13997 Students reported as harassed or bullied on the basis of disability
summary(filtered_data$TOT_HBREPORTED_DIS_F)#1466/13997
summary(filtered_data$TOT_HBDISCIPLINED_SEX_M) #1467/13997 Students disciplined for bullying or harassment on the basis of sex
summary(filtered_data$TOT_HBDISCIPLINED_SEX_F)#1467/13997
summary(filtered_data$TOT_HBDISCIPLINED_RAC_M) #1467/13997 Students disciplined for bullying or harassment on the basis of sex
summary(filtered_data$TOT_HBDISCIPLINED_RAC_F)#1467/13997
summary(filtered_data$TOT_HBDISCIPLINED_DIS_M) #1466/13997 Students disciplined for bullying or harassment on the basis of sex
summary(filtered_data$TOT_HBDISCIPLINED_DIS_F)#1466/13997
summary(filtered_data$TOT_SCIENR_PHYS_M) #3085/13997 Students disciplined for bullying or harassment on the basis of sex
summary(filtered_data$TOT_SCIENR_PHYS_F)#3085/13997
variables <- c(
  "TOT_MATHENR_ADVM_M", "TOT_MATHENR_ADVM_F", "TOT_APENR_M", "TOT_APENR_F",
  "TOT_MATHENR_ALG2_M", "TOT_MATHENR_ALG2_F", "TOT_SCIENR_BIOL_M", "TOT_SCIENR_BIOL_F",
  "TOT_MATHENR_CALC_M", "TOT_MATHENR_CALC_F", "TOT_SCIENR_CHEM_M", "TOT_SCIENR_CHEM_F",
  "TOT_COMPENR_CSCI_M", "TOT_COMPENR_CSCI_F", "TOT_DUAL_M", "TOT_DUAL_F",
  "TOT_LEPPROGENR_M", "TOT_LEPPROGENR_F", "TOT_IDEAENR_M", "TOT_IDEAENR_F",
  "TOT_DISCWODIS_EXPWE_M", "TOT_DISCWODIS_EXPWE_F", "TOT_DISCWDIS_EXPWE_IDEA_M",
  "TOT_DISCWDIS_EXPWE_IDEA_F", "TOT_DISCWODIS_EXPWOE_M", "TOT_DISCWODIS_EXPWOE_F",
  "TOT_DISCWDIS_EXPWOE_IDEA_M", "TOT_DISCWDIS_EXPWOE_IDEA_F", "TOT_DISCWODIS_EXPZT_M",
  "TOT_DISCWODIS_EXPZT_F", "TOT_DISCWDIS_EXPZT_IDEA_M", "TOT_DISCWDIS_EXPZT_IDEA_F",
  "TOT_MATHENR_GEOM_M", "TOT_MATHENR_GEOM_F",
  "TOT_HBREPORTED_SEX_M", "TOT_HBREPORTED_SEX_F", "TOT_HBREPORTED_RAC_M", "TOT_HBREPORTED_RAC_F",
  "TOT_HBREPORTED_DIS_M", "TOT_HBREPORTED_DIS_F", "TOT_HBDISCIPLINED_SEX_M", "TOT_HBDISCIPLINED_SEX_F",
  "TOT_HBDISCIPLINED_RAC_M", "TOT_HBDISCIPLINED_RAC_F", "TOT_HBDISCIPLINED_DIS_M", "TOT_HBDISCIPLINED_DIS_F",
  "TOT_SCIENR_PHYS_M", "TOT_SCIENR_PHYS_F", "TOT_DISCWODIS_REF_M", "TOT_DISCWODIS_REF_F",
  "TOT_DISCWDIS_REF_IDEA_M", "TOT_DISCWDIS_REF_IDEA_F", "TOT_DISCWODIS_ARR_M", "TOT_DISCWODIS_ARR_F",
  "TOT_DISCWDIS_ARR_IDEA_M", "TOT_DISCWDIS_ARR_IDEA_F", "TOT_RS_NONIDEA_MECH_M", "TOT_RS_NONIDEA_MECH_F",
  "TOT_RS_IDEA_MECH_M", "TOT_RS_IDEA_MECH_F", "TOT_RS_NONIDEA_PHYS_M", "TOT_RS_NONIDEA_PHYS_F",
  "TOT_RS_IDEA_PHYS_M", "TOT_RS_IDEA_PHYS_F", "TOT_RS_NONIDEA_SECL_M", "TOT_RS_NONIDEA_SECL_F",
  "TOT_DISCWODIS_ISS_M", "TOT_DISCWODIS_ISS_F", "TOT_DISCWDIS_ISS_IDEA_M", "TOT_DISCWDIS_ISS_IDEA_F",
  "TOT_DISCWODIS_TFRALT_M", "TOT_DISCWODIS_TFRALT_F", "TOT_SSSPORTS", "TOT_SSPART",
  "SCH_FTECOUNSELORS", "SCH_FTESERVICES_NUR", "SCH_FTESERVICES_PSY", "SCH_FTESERVICES_SOC",
  "SCH_OFFENSE_RAPE", "SCH_OFFENSE_BATT", "SCH_OFFENSE_ROBWW", "SCH_OFFENSE_ROBWOW",
  "SCH_OFFENSE_ATTWW", "SCH_OFFENSE_ATTWOW", "SCH_OFFENSE_THRWW", "SCH_OFFENSE_THRWOW",
  "SCH_OFFENSE_POSSWX", "SCH_INTERNET_WIFIENDEV"
)


filtered_data[variables] <- lapply(filtered_data[variables], function(x) ifelse(is.na(x), 0, x))


filtered_data <- filtered_data[rowSums(filtered_data[variables] == 0) != length(variables), ]


column_pairs = list(
  "advancedMath" = c("TOT_MATHENR_ADVM_M", "TOT_MATHENR_ADVM_F"),
  "APenroll" = c("TOT_APENR_M", "TOT_APENR_F"),
  "algebraII" = c("TOT_MATHENR_ALG2_M", "TOT_MATHENR_ALG2_F"),
  "biology" = c("TOT_SCIENR_BIOL_M", "TOT_SCIENR_BIOL_F"),
  "calculus" = c("TOT_MATHENR_CALC_M", "TOT_MATHENR_CALC_F"),
  "chemistry" = c("TOT_SCIENR_CHEM_M", "TOT_SCIENR_CHEM_F"),
  "computerscience" = c("TOT_COMPENR_CSCI_M", "TOT_COMPENR_CSCI_F"),
  "dual" = c("TOT_DUAL_M", "TOT_DUAL_F"),
  "ESL program" = c("TOT_LEPPROGENR_M", "TOT_LEPPROGENR_F"),
  "disabilities" = c("TOT_IDEAENR_M", "TOT_IDEAENR_F"),
  "geometry" = c("TOT_MATHENR_GEOM_M", "TOT_MATHENR_GEOM_F"),
  "sexbullied" = c("TOT_HBREPORTED_SEX_M", "TOT_HBREPORTED_SEX_F"),
  "racebullied" = c("TOT_HBREPORTED_RAC_M", "TOT_HBREPORTED_RAC_F"),
  "disbullied" = c("TOT_HBREPORTED_DIS_M", "TOT_HBREPORTED_DIS_F"),
  "sexbully" = c("TOT_HBDISCIPLINED_SEX_M", "TOT_HBDISCIPLINED_SEX_F"),
  "racebully" = c("TOT_HBDISCIPLINED_RAC_M", "TOT_HBDISCIPLINED_RAC_F"),
  "disbully" = c("TOT_HBDISCIPLINED_DIS_M", "TOT_HBDISCIPLINED_DIS_F"),
  "physics" = c("TOT_SCIENR_PHYS_M", "TOT_SCIENR_PHYS_F")
)




for (new_col in names(column_pairs)) {
  male_col = column_pairs[[new_col]][1]
  female_col = column_pairs[[new_col]][2]
  
  
  summed_values = rowSums(filtered_data[c(male_col, female_col)], na.rm = TRUE)
  

  filtered_data[[paste0(new_col, "_PER_ENR")]] = summed_values / filtered_data$TOT_ENR_TOTAL
}




gender_specific_cols = unlist(column_pairs)
filtered_data <- filtered_data[!names(filtered_data) %in% c(gender_specific_cols, "TOT_ENR_F", "TOT_ENR_M")]


summary(filtered_data$advancedMath_PER_ENR)
for (new_col in names(column_pairs)) {
  print(summary(filtered_data[[paste0(new_col, "_PER_ENR")]]))
}

#include advancedMath_PER_ENR, APenroll_PER_ENR, algebraII_PER_ENR, biology_PER_ENR, calculus_PER_ENR, chemistry_PER_ENR, computerscience_PER_ENR, 
#dual_PER_ENR, ESL program_PER_ENR, disabilities_PER_ENR, geometry_PER_ENR, Summary of physics_PER_ENR


summary(filtered_data$TOT_DISCWODIS_EXPWE_M) #1401/13997 without Disabilities who received an expulsion with educational services
summary(filtered_data$TOT_DISCWODIS_EXPWE_F) #1401/13997
summary(filtered_data$TOT_DISCWDIS_EXPWE_IDEA_M) #1325/13997 with Disabilities who received an expulsion with educational services
summary(filtered_data$TOT_DISCWDIS_EXPWE_IDEA_F)#1325/13997
summary(filtered_data$TOT_DISCWODIS_EXPWOE_M) #1087/13997 without Disabilities who received an expulsion without educational services
summary(filtered_data$TOT_DISCWODIS_EXPWOE_F)#1086/13997
summary(filtered_data$TOT_DISCWDIS_EXPWOE_IDEA_M) #1084/13997 with Disabilities who received an expulsion without educational services
summary(filtered_data$TOT_DISCWDIS_EXPWOE_IDEA_F)#1084/13997
summary(filtered_data$TOT_DISCWODIS_EXPZT_M) #1086/13997 without Disabilities who received an expulsion under zero tolerance
summary(filtered_data$TOT_DISCWODIS_EXPZT_F)#1086/13997
summary(filtered_data$TOT_DISCWDIS_EXPZT_IDEA_M) #1084/13997 with Disabilities who received an expulsion under zero tolerance
summary(filtered_data$TOT_DISCWDIS_EXPZT_IDEA_F)#1084/13997

summary(filtered_data$TOT_DISCWODIS_REF_M) #1441/13997 Students referred to law enforcement without disability
summary(filtered_data$TOT_DISCWODIS_REF_F)#1439/13997
summary(filtered_data$TOT_DISCWDIS_REF_IDEA_M) #1441/13997 Students referred to law enforcement w disability
summary(filtered_data$TOT_DISCWDIS_REF_IDEA_F)#1439/13997
summary(filtered_data$TOT_DISCWODIS_ARR_M) #1531/13997 Students arresed without disability
summary(filtered_data$TOT_DISCWODIS_ARR_F)#1495/13997
summary(filtered_data$TOT_DISCWDIS_ARR_IDEA_M) #1516/13997 Students arresed without disability
summary(filtered_data$TOT_DISCWDIS_ARR_IDEA_M)#1516/13997
summary(filtered_data$TOT_DISCWODIS_ISS_M) #1120/13997 without disabilities who received one or more in-school suspensions
summary(filtered_data$TOT_DISCWODIS_ISS_F) #1113/13997 
summary(filtered_data$TOT_DISCWDIS_ISS_IDEA_M) #1098/13997 with disabilities who received one or more in-school suspensions
summary(filtered_data$TOT_DISCWDIS_ISS_IDEA_M) #1098/13997
summary(filtered_data$TOT_DISCWODIS_TFRALT_M) #1089/13997 without disability transfrered for discplinary reasons
summary(filtered_data$TOT_DISCWODIS_TFRALT_F) #1090/13997
summary(filtered_data$TOT_DISCWDIS_TFRALT_IDEA_M) #1089/13997 disability transfrered for discplinary reasons
summary(filtered_data$TOT_DISCWDIS_TFRALT_IDEA_F) #1090/13997


old_columns <- c()


categories = c("EXPWE", "EXPWOE", "EXPZT", "REF", "ARR", "ISS", "TFRALT")


for (category in categories) {

  columns_m = c(paste0("TOT_DISCWODIS_", category, "_M"), paste0("TOT_DISCWDIS_", category, "_IDEA_M"))
  columns_f = c(paste0("TOT_DISCWODIS_", category, "_F"), paste0("TOT_DISCWDIS_", category, "_IDEA_F"))
  
 
  columns_m_exist = columns_m[columns_m %in% names(filtered_data)]
  columns_f_exist = columns_f[columns_f %in% names(filtered_data)]
  

  if (length(columns_m_exist) > 0 && length(columns_f_exist) > 0) {
    filtered_data[[paste0("TOT_", category)]] = rowSums(filtered_data[columns_m_exist], na.rm = TRUE) +
      rowSums(filtered_data[columns_f_exist], na.rm = TRUE)
    
    
    old_columns <- c(old_columns, columns_m_exist, columns_f_exist)
  }
}

filtered_data <- filtered_data[!names(filtered_data) %in% old_columns]
#ignore for now
# summary(filtered_data$TOT_EXPWE) 
# summary(filtered_data$TOT_EXPWOE)
# summary(filtered_data$TOT_EXPZT)
filtered_data$TOT_REF_PER_ENR <- (filtered_data$TOT_REF)/(filtered_data$TOT_ENR_TOTAL)
filtered_data$TOT_ARR_PER_ENR <- (filtered_data$TOT_ARR)/(filtered_data$TOT_ENR_TOTAL)
filtered_data$TOT_ISS_PER_ENR <- (filtered_data$TOT_ISS)/(filtered_data$TOT_ENR_TOTAL)
filtered_data$TOT_TFRALT_PER_ENR <- (filtered_data$TOT_TFRALT)/(filtered_data$TOT_ENR_TOTAL)
filtered_data <- filtered_data[!names(filtered_data) %in% c("TOT_REF", "TOT_ARR", "TOT_ISS",  "TOT_TFRALT")]
summary(filtered_data$TOT_REF_PER_ENR) # include
summary(filtered_data$TOT_ARR_PER_ENR)
summary(filtered_data$TOT_ISS_PER_ENR) #include
summary(filtered_data$TOT_TFRALT_PER_ENR)



summary(filtered_data$TOT_RS_NONIDEA_MECH_M) #2029/13997 Non-IDEA students subjected to mechanical restraint
summary(filtered_data$TOT_RS_NONIDEA_MECH_F)#2028/13997
summary(filtered_data$TOT_RS_IDEA_MECH_M) #2028/13997 IDEA students subjected to mechanical restraint
summary(filtered_data$TOT_RS_IDEA_MECH_F)#2028/13997
summary(filtered_data$TOT_RS_NONIDEA_PHYS_M) #2030/13997 Non-IDEA students subjected to physical restraint add to mechanical
summary(filtered_data$TOT_RS_NONIDEA_PHYS_F)#2031/13997
summary(filtered_data$TOT_RS_IDEA_PHYS_M) #2030/13997 IDEA students subjected to physical restraint
summary(filtered_data$TOT_RS_IDEA_PHYS_F)#2029/13997
summary(filtered_data$TOT_RS_NONIDEA_SECL_M) #2029/13997 Non-IDEA students subjected to seclusion
summary(filtered_data$TOT_RS_NONIDEA_SECL_F)#2029/13997


# old_columns <- c()
# 

# restraint_types = c("MECH", "PHYS", "SECL")  # Mechanical, Physical, Seclusion
# categories = c("NONIDEA", "IDEA")


# for (restraint in c("MECH", "PHYS")) {
#   for (category in categories) {
#     columns = c(
#       paste0("TOT_RS_", category, "_", restraint, "_M"),
#       paste0("TOT_RS_", category, "_", restraint, "_F")
#     )
     

#     filtered_data[[paste0("TOT_RS_", category, "_", restraint)]] = rowSums(filtered_data[columns], na.rm = TRUE)
   

#     old_columns <- c(old_columns, columns)
#   }
#   

#   nonidea_col = paste0("TOT_RS_NONIDEA_", restraint)
#   idea_col = paste0("TOT_RS_IDEA_", restraint)
#   filtered_data[[paste0("TOT_RS_", restraint)]] = filtered_data[[nonidea_col]] + filtered_data[[idea_col]]
   

#   old_columns <- c(old_columns, nonidea_col, idea_col)
# }
 

# secl_columns = c(
#   "TOT_RS_NONIDEA_SECL_M", "TOT_RS_NONIDEA_SECL_F",
#   "TOT_RS_IDEA_SECL_M", "TOT_RS_IDEA_SECL_F"
# )

# filtered_data$TOT_RS_SECL = rowSums(filtered_data[secl_columns], na.rm = TRUE)
 

# old_columns <- c(old_columns, secl_columns)
 

# filtered_data <- filtered_data[!names(filtered_data) %in% old_columns]
# 
# 
# 
# 
# 
# 
# filtered_data$TOT_REF_PER_ENR <- (filtered_data$TOT_)/(filtered_data$TOT_ENR_TOTAL)
# filtered_data$TOT_ARR_PER_ENR <- (filtered_data$TOT_ARR)/(filtered_data$TOT_ENR_TOTAL)
# filtered_data$TOT_ISS_PER_ENR <- (filtered_data$TOT_ISS)/(filtered_data$TOT_ENR_TOTAL)
# filtered_data$TOT_TFRALT_PER_ENR <- (filtered_data$TOT_TFRALT)/(filtered_data$TOT_ENR_TOTAL)
# filtered_data <- filtered_data[!names(filtered_data) %in% c("TOT_REF", "TOT_ARR", "TOT_ISS",  "TOT_TFRALT")]
# summary(filtered_data$TOT_RS_MECH)
# summary(filtered_data$TOT_RS_PHYS)
# summary(filtered_data$TOT_RS_SECL) 




summary(filtered_data$TOT_SSSPORTS) #2401/13997 single-sex interscholastic sports, include


summary(filtered_data$TOT_SSPART) #2402/13997 single-sex interscholastic sports participants
summary(filtered_data$SCH_FTETEACH_TOT)
summary(filtered_data$SCH_FTECOUNSELORS)
summary(filtered_data$SCH_FTESERVICES_NUR)
summary(filtered_data$SCH_FTESERVICES_PSY)
summary(filtered_data$SCH_FTESERVICES_SOC)
summary(filtered_data$SCH_INTERNET_WIFIENDEV) #number of wi-Fi enabled devices provided by the school to students
filtered_data$sportsrate <- filtered_data$TOT_SSPART/(filtered_data$TOT_ENR_TOTAL)
filtered_data$teacherRatio <- filtered_data$TOT_ENR_TOTAL / (filtered_data$SCH_FTETEACH_TOT+0.0001) #students per teacher
filtered_data$counselorRatio <- filtered_data$TOT_ENR_TOTAL / (filtered_data$SCH_FTECOUNSELORS+0.0001) #students per counselor
filtered_data$healthProf <- filtered_data$TOT_ENR_TOTAL / (filtered_data$SCH_FTESERVICES_PSY + filtered_data$SCH_FTESERVICES_NUR + filtered_data$SCH_FTESERVICES_SOC+0.0001)
filtered_data$deviceRatio <- filtered_data$SCH_INTERNET_WIFIENDEV/(filtered_data$TOT_ENR_TOTAL) #device per student

ratios <- c("sportsrate", "teacherRatio", "counselorRatio", "healthProf", "deviceRatio")
rows_with_NAs <- apply(filtered_data[ratios], 1, function(x) any(is.na(x)))


filtered_data <- filtered_data[!rows_with_NAs,]
for (column in ratios) {
  print(paste0("Summary of ", column, ":"))
  print(summary(filtered_data[[paste0(column)]]))
}
nrow(filtered_data)
filtered_data <- filtered_data[filtered_data$sportsrate <= 1,]
nrow(filtered_data)







summary(filtered_data$SCH_OFFENSE_RAPE) #rape incidents
summary(filtered_data$SCH_OFFENSE_BATT) #sexual assault other than rape
summary(filtered_data$SCH_OFFENSE_ROBWW) #robbery w weapon
summary(filtered_data$SCH_OFFENSE_ROBWOW) #robbery without weapon
summary(filtered_data$SCH_OFFENSE_ATTWW) #physical attack w weapon
summary(filtered_data$SCH_OFFENSE_ATTWOW)#physical attack without weapon
summary(filtered_data$SCH_OFFENSE_THRWW) #threat of physical attack w weapon
summary(filtered_data$SCH_OFFENSE_THRWOW)#threat of physical attack without weapon
summary(filtered_data$SCH_OFFENSE_POSSWX) #possession of firearm or explosive device

filtered_data$sexualAssault <- filtered_data$SCH_OFFENSE_RAPE + filtered_data$SCH_OFFENSE_BATT

filtered_data$attacks <- filtered_data$SCH_OFFENSE_ROBWW + filtered_data$SCH_OFFENSE_ROBWOW +
  filtered_data$SCH_OFFENSE_ATTWW + filtered_data$SCH_OFFENSE_ATTWOW +
  filtered_data$SCH_OFFENSE_THRWW + filtered_data$SCH_OFFENSE_THRWOW

summary(filtered_data$sexualAssault) #include
summary(filtered_data$attacks) #include

filtered_data$LEA_STATE<-as.factor(filtered_data$LEA_STATE)

summary(filtered_data$SCH_DIND_INSTRUCTIONTYPE) #categorical

filtered_data$SCH_DIND_INSTRUCTIONTYPE <- as.factor(filtered_data$SCH_DIND_INSTRUCTIONTYPE)
filtered_data$SCH_DIND_INSTRUCTIONTYPE <- relevel(filtered_data$SCH_DIND_INSTRUCTIONTYPE, ref = "D") #use this



summary(filtered_data$SCH_INTERNET_FIBER) #binary Is this school connected to Internet through a fiber-optic connection
filtered_data$SCH_INTERNET_FIBER<-as.factor(filtered_data$SCH_INTERNET_FIBER)
summary(filtered_data$SCH_INTERNET_WIFI) #binary Does this school have Wi-Fi access in every classroom?
filtered_data$SCH_INTERNET_WIFI<-as.factor(filtered_data$SCH_INTERNET_WIFI)
summary(filtered_data$SCH_INTERNET_SCHDEV) #binary Does this school allow students to take home school-issued devices that can be used to access the Internet for student learning?
filtered_data$SCH_INTERNET_SCHDEV<-as.factor(filtered_data$SCH_INTERNET_SCHDEV)
summary(filtered_data$SCH_INTERNET_STUDDEV) #binary Does this school allow students to bring to school student-owned devices that can be used to access the Internet for classroom or student learning?	
filtered_data$SCH_INTERNET_STUDDEV<-as.factor(filtered_data$SCH_INTERNET_STUDDEV)

#the above are all missing 432, 3% of all data, still want to keep variables, just omit the missing values
nrow(filtered_data)
summary(filtered_data$SCH_INTERNET_FIBER)
filtered_data <- filtered_data[!is.na(filtered_data$SCH_INTERNET_FIBER), ]
nrow(filtered_data)
summary(filtered_data$SCH_INTERNET_WIFI)
summary(filtered_data$SCH_INTERNET_SCHDEV)
summary(filtered_data$SCH_INTERNET_STUDDEV)

# summary(filtered_data$SCH_FIREARM_IND) #binary Was there at least one incident at the school that involved a shooting (regardless of whether anyone was hurt
# filtered_data$SCH_FIREARM_IND<-as.factor(filtered_data$SCH_FIREARM_IND)
# summary(filtered_data$SCH_FIREARM_IND)
# summary(filtered_data$SCH_HOMICIDE_IND) #binary Did any of the school’s students, faculty, or staff die as a result of a homicide committed at your school
# filtered_data$SCH_HOMICIDE_IND<-as.factor(filtered_data$SCH_HOMICIDE_IND)
# summary(filtered_data$SCH_HOMICIDE_IND)

# #total numbers don't include
# summary(filtered_data$TOT_APMATHENR_M) #6297/13997 
# summary(filtered_data$TOT_APMATHENR_F) #6294/13997 
# summary(filtered_data$TOT_APSCIENR_M) #6617/13997 
# summary(filtered_data$TOT_APSCIENR_F) #6621/13997
# summary(filtered_data$TOT_APCOMPENR_M) #9935/13997 
# summary(filtered_data$TOT_APCOMPENR_F) #9930/13997
# #TOT_ALGENR_G08_M not applicable, for Middle school
# #TOT_PSDISC_CORP_M not applicable for preschool
# #TOT_PSENR_M preschool
# summary(filtered_data$TOT_LEPENR_M) #146/13997 english proficiency, encode 0
# summary(filtered_data$TOT_LEPENR_F) #110/13997
# summary(filtered_data$TOT_504ENR_M) #504/13997 Students with disabilities served under IDEA, encode 0
# summary(filtered_data$TOT_504ENR_F) #504/13997
# #gifted and talented program not included
# summary(filtered_data$TOT_DISCWODIS_CORP_M) #13414/13997 students without disability corporal punishment, encode 0
# summary(filtered_data$TOT_DISCWODIS_CORP_F)#13414/13997
# summary(filtered_data$TOT_DISCWDIS_CORP_IDEA_M) #13414/13997 students with disability corporal punishment, encode 0, add to without
# summary(filtered_data$TOT_DISCWDIS_CORP_IDEA_F) #13414/13997
# summary(filtered_data$TOT_IBENR_M) #13246/13997 Students enrolled in IB
# summary(filtered_data$TOT_IBENR_F)#13245/13997
# #TOT_RET_KG_M kindergarten
# #TOT_SSTEAMS teams sports
# #TOT_SSCLASSES_ALGG single sex math classes
# #TOT_DISCWODIS_SINGOOS_M one suspension only
# #TOT_DAYSMISSED_M days missed to suspension
# 
# 
# #remember TOT_ABSENT_M
# List of column names you want to include
columns_to_include <- c("advancedMath_PER_ENR", "APenroll_PER_ENR", "algebraII_PER_ENR", "biology_PER_ENR", 
                        "calculus_PER_ENR", "chemistry_PER_ENR", "computerscience_PER_ENR", "dual_PER_ENR", 
                        "ESL program_PER_ENR", "disabilities_PER_ENR", "geometry_PER_ENR", "physics_PER_ENR", 
                        "TOT_REF_PER_ENR", "TOT_ISS_PER_ENR", "sportsrate", "teacherRatio", "counselorRatio", 
                        "healthProf", "deviceRatio", "LEA_STATE", "attacks", "sexualAssault", 
                        "SCH_DIND_INSTRUCTIONTYPE", "SCH_INTERNET_FIBER", "SCH_INTERNET_WIFI", "SCH_INTERNET_SCHDEV", 
                        "SCH_INTERNET_STUDDEV", "tested")

final_data <- filtered_data[, columns_to_include]
final_data <- na.omit(final_data)
nrow(final_data)
model_full <- lm(tested ~ advancedMath_PER_ENR + APenroll_PER_ENR + algebraII_PER_ENR + biology_PER_ENR + calculus_PER_ENR + chemistry_PER_ENR
                 + computerscience_PER_ENR + dual_PER_ENR + `ESL program_PER_ENR` + disabilities_PER_ENR + geometry_PER_ENR + physics_PER_ENR + 
                   TOT_REF_PER_ENR + TOT_ISS_PER_ENR + sportsrate + teacherRatio + counselorRatio + healthProf + deviceRatio + LEA_STATE +
                   attacks + sexualAssault +SCH_DIND_INSTRUCTIONTYPE + SCH_INTERNET_FIBER + SCH_INTERNET_WIFI + SCH_INTERNET_SCHDEV + 
                   SCH_INTERNET_STUDDEV, data = final_data)
summary(model_full)       #0.4496             


library(car)


# Calculate VIF
vif_results <- vif(model_full)  
print(vif_results)
#expected because it's a categorical variable representing over 50 states, not removing anything

## All subset selection 
#Mallow's CP and R^2 don't work, too many variables

# automatic selection process, i.e., stepwise regression with AIC criterion and stepwise
#regression with BIC criterion (try both)
step(model_full, direction = "both", k = 2)
# lm(formula = tested ~ advancedMath_PER_ENR + APenroll_PER_ENR + 
#      algebraII_PER_ENR + biology_PER_ENR + calculus_PER_ENR + 
#      chemistry_PER_ENR + dual_PER_ENR + `ESL program_PER_ENR` + 
#      disabilities_PER_ENR + TOT_REF_PER_ENR + sportsrate + healthProf + 
#      deviceRatio + LEA_STATE + sexualAssault + SCH_DIND_INSTRUCTIONTYPE + 
#      SCH_INTERNET_FIBER + SCH_INTERNET_WIFI, data = final_data)

step(model_full, direction = "both", k = log(nrow(final_data)))
# lm(formula = tested ~ advancedMath_PER_ENR + APenroll_PER_ENR +
# algebraII_PER_ENR + biology_PER_ENR + calculus_PER_ENR +
#   chemistry_PER_ENR + dual_PER_ENR + `ESL program_PER_ENR` +
#   disabilities_PER_ENR + TOT_REF_PER_ENR + sportsrate + LEA_STATE,
# data = final_data)

nrow(final_data)
CV1=CV2<- 0
n <- dim(final_data)[1]
n.shuffle = sample(1:n,n,replace=FALSE)
id.cv <- list()
fold_size <- floor(n / 10)
for (i in 1:10) {
  if (i < 10) {
    id.cv[[i]] <- n.shuffle[((i - 1) * fold_size + 1):(i * fold_size)]
  } else {
    id.cv[[i]] <- n.shuffle[((i - 1) * fold_size + 1):n]
  }
}
for(i in 1:10)
{
  fit1 <- lm(formula = tested ~ advancedMath_PER_ENR + APenroll_PER_ENR + algebraII_PER_ENR + biology_PER_ENR + calculus_PER_ENR + 
               chemistry_PER_ENR + dual_PER_ENR + `ESL program_PER_ENR` + disabilities_PER_ENR + TOT_REF_PER_ENR + sportsrate + healthProf + 
               deviceRatio + LEA_STATE + sexualAssault + SCH_DIND_INSTRUCTIONTYPE + SCH_INTERNET_FIBER + SCH_INTERNET_WIFI, data=final_data[-id.cv[[i]],])
  
  fit2 <- lm(formula = tested ~ advancedMath_PER_ENR + APenroll_PER_ENR +
               algebraII_PER_ENR + biology_PER_ENR + calculus_PER_ENR +
               chemistry_PER_ENR + dual_PER_ENR + `ESL program_PER_ENR` +
               disabilities_PER_ENR + TOT_REF_PER_ENR + sportsrate + LEA_STATE,
             data=final_data[-id.cv[[i]],])
  CV1 <- CV1 + (1/n) * sum((final_data$tested[id.cv[[i]]] - predict(fit1,final_data[id.cv[[i]], ]))^2)
  CV2 <- CV2 + (1/n) * sum((final_data$tested[id.cv[[i]]] - predict(fit2,final_data[id.cv[[i]], ]))^2)
}
print(c(CV1,CV2)) #0.01019347 0.01020930
print(sqrt(c(CV1,CV2))) #0.1009740 0.1010279
# choose fit2, parsimonious as not much diff in CV

hist(final_data$tested, 
     main = "Histogram of SAT/ACT Test Participation Rate", 
     xlab = "Ratio of Students who took SAT/ACT", 
     col = "darkblue")
#skewed right


other = lm(formula = tested ~ advancedMath_PER_ENR + APenroll_PER_ENR + algebraII_PER_ENR + biology_PER_ENR + calculus_PER_ENR + 
             chemistry_PER_ENR + dual_PER_ENR + `ESL program_PER_ENR` + disabilities_PER_ENR + TOT_REF_PER_ENR + sportsrate + healthProf + 
             deviceRatio + LEA_STATE + sexualAssault + SCH_DIND_INSTRUCTIONTYPE + SCH_INTERNET_FIBER + SCH_INTERNET_WIFI, data=final_data)
summary(other) #0.4498 R squared
##Best 1rst order model
first=lm(formula = tested ~ advancedMath_PER_ENR + APenroll_PER_ENR +
                algebraII_PER_ENR + biology_PER_ENR + calculus_PER_ENR +
                chemistry_PER_ENR + dual_PER_ENR + `ESL program_PER_ENR` +
                disabilities_PER_ENR + TOT_REF_PER_ENR + sportsrate + LEA_STATE,
              data=final_data)
summary(first) #0.4481 R squared

#interaction terms
step(first,.~.^2,direction="both",k=2) #use AIC
# lm(formula = tested ~ advancedMath_PER_ENR + APenroll_PER_ENR +
#      algebraII_PER_ENR + biology_PER_ENR + calculus_PER_ENR +
#      chemistry_PER_ENR + dual_PER_ENR + `ESL program_PER_ENR` +
#      disabilities_PER_ENR + TOT_REF_PER_ENR + sportsrate + LEA_STATE +
#      APenroll_PER_ENR:LEA_STATE + algebraII_PER_ENR:LEA_STATE +
#      `ESL program_PER_ENR`:LEA_STATE + dual_PER_ENR:LEA_STATE +
#      sportsrate:LEA_STATE + calculus_PER_ENR:LEA_STATE + calculus_PER_ENR:`ESL program_PER_ENR` +
#      calculus_PER_ENR:chemistry_PER_ENR + APenroll_PER_ENR:sportsrate +
#      advancedMath_PER_ENR:disabilities_PER_ENR + TOT_REF_PER_ENR:sportsrate +
#      algebraII_PER_ENR:biology_PER_ENR + advancedMath_PER_ENR:calculus_PER_ENR +
#      APenroll_PER_ENR:calculus_PER_ENR + APenroll_PER_ENR:dual_PER_ENR +
#      advancedMath_PER_ENR:dual_PER_ENR + APenroll_PER_ENR:`ESL program_PER_ENR` +
#      calculus_PER_ENR:disabilities_PER_ENR + advancedMath_PER_ENR:biology_PER_ENR +
#      biology_PER_ENR:sportsrate + chemistry_PER_ENR:sportsrate +
#      chemistry_PER_ENR:dual_PER_ENR, data = final_data)
step(first,.~.^2,direction="both",k=log(nrow(final_data))) #use BIC
# lm(formula = tested ~ advancedMath_PER_ENR + APenroll_PER_ENR +
#      algebraII_PER_ENR + biology_PER_ENR + calculus_PER_ENR +
#      chemistry_PER_ENR + dual_PER_ENR + `ESL program_PER_ENR` +
#      disabilities_PER_ENR + TOT_REF_PER_ENR + sportsrate + LEA_STATE +
#      calculus_PER_ENR:chemistry_PER_ENR + APenroll_PER_ENR:sportsrate +
#      advancedMath_PER_ENR:disabilities_PER_ENR + calculus_PER_ENR:`ESL program_PER_ENR` +
#      calculus_PER_ENR:disabilities_PER_ENR + APenroll_PER_ENR:calculus_PER_ENR +
#      TOT_REF_PER_ENR:sportsrate + calculus_PER_ENR:dual_PER_ENR,
#    data = final_data)

CV1=CV2<- 0
for(i in 1:10)
{
  fit1 <- lm(formula = tested ~ advancedMath_PER_ENR + APenroll_PER_ENR +
               algebraII_PER_ENR + biology_PER_ENR + calculus_PER_ENR +
               chemistry_PER_ENR + dual_PER_ENR + `ESL program_PER_ENR` +
               disabilities_PER_ENR + TOT_REF_PER_ENR + sportsrate + LEA_STATE +
               APenroll_PER_ENR:LEA_STATE + algebraII_PER_ENR:LEA_STATE +
               `ESL program_PER_ENR`:LEA_STATE + dual_PER_ENR:LEA_STATE +
               sportsrate:LEA_STATE + calculus_PER_ENR:LEA_STATE + calculus_PER_ENR:`ESL program_PER_ENR` +
               calculus_PER_ENR:chemistry_PER_ENR + APenroll_PER_ENR:sportsrate +
               advancedMath_PER_ENR:disabilities_PER_ENR + TOT_REF_PER_ENR:sportsrate +
               algebraII_PER_ENR:biology_PER_ENR + advancedMath_PER_ENR:calculus_PER_ENR +
               APenroll_PER_ENR:calculus_PER_ENR + APenroll_PER_ENR:dual_PER_ENR +
               advancedMath_PER_ENR:dual_PER_ENR + APenroll_PER_ENR:`ESL program_PER_ENR` +
               calculus_PER_ENR:disabilities_PER_ENR + advancedMath_PER_ENR:biology_PER_ENR +
               biology_PER_ENR:sportsrate + chemistry_PER_ENR:sportsrate +
               chemistry_PER_ENR:dual_PER_ENR, data = final_data[-id.cv[[i]],])
  
  fit2 <- lm(formula = tested ~ advancedMath_PER_ENR + APenroll_PER_ENR +
               algebraII_PER_ENR + biology_PER_ENR + calculus_PER_ENR +
               chemistry_PER_ENR + dual_PER_ENR + `ESL program_PER_ENR` +
               disabilities_PER_ENR + TOT_REF_PER_ENR + sportsrate + LEA_STATE +
               calculus_PER_ENR:chemistry_PER_ENR + APenroll_PER_ENR:sportsrate +
               advancedMath_PER_ENR:disabilities_PER_ENR + calculus_PER_ENR:`ESL program_PER_ENR` +
               calculus_PER_ENR:disabilities_PER_ENR + APenroll_PER_ENR:calculus_PER_ENR +
               TOT_REF_PER_ENR:sportsrate + calculus_PER_ENR:dual_PER_ENR,
             data = final_data[-id.cv[[i]],])
  CV1 <- CV1 + (1/n) * sum((final_data$tested[id.cv[[i]]] - predict(fit1,final_data[id.cv[[i]], ]))^2)
  CV2 <- CV2 + (1/n) * sum((final_data$tested[id.cv[[i]]] - predict(fit2,final_data[id.cv[[i]], ]))^2)
}
print(c(CV1,CV2)) #0.01006552 0.01010850, close enogh
sqrt(c(0.01006552, 0.01010850))


other <- lm(formula = tested ~ advancedMath_PER_ENR + APenroll_PER_ENR +
              algebraII_PER_ENR + biology_PER_ENR + calculus_PER_ENR +
              chemistry_PER_ENR + dual_PER_ENR + `ESL program_PER_ENR` +
              disabilities_PER_ENR + TOT_REF_PER_ENR + sportsrate + LEA_STATE +
              APenroll_PER_ENR:LEA_STATE + algebraII_PER_ENR:LEA_STATE +
              `ESL program_PER_ENR`:LEA_STATE + dual_PER_ENR:LEA_STATE +
              sportsrate:LEA_STATE + calculus_PER_ENR:LEA_STATE + calculus_PER_ENR:`ESL program_PER_ENR` +
              calculus_PER_ENR:chemistry_PER_ENR + APenroll_PER_ENR:sportsrate +
              advancedMath_PER_ENR:disabilities_PER_ENR + TOT_REF_PER_ENR:sportsrate +
              algebraII_PER_ENR:biology_PER_ENR + advancedMath_PER_ENR:calculus_PER_ENR +
              APenroll_PER_ENR:calculus_PER_ENR + APenroll_PER_ENR:dual_PER_ENR +
              advancedMath_PER_ENR:dual_PER_ENR + APenroll_PER_ENR:`ESL program_PER_ENR` +
              calculus_PER_ENR:disabilities_PER_ENR + advancedMath_PER_ENR:biology_PER_ENR +
              biology_PER_ENR:sportsrate + chemistry_PER_ENR:sportsrate +
              chemistry_PER_ENR:dual_PER_ENR, data = final_data)
summary(other) #0.4938
length(coef(other)) - 1 #377 variables
high_order <- lm(formula = tested ~ advancedMath_PER_ENR + APenroll_PER_ENR +
                   algebraII_PER_ENR + biology_PER_ENR + calculus_PER_ENR +
                   chemistry_PER_ENR + dual_PER_ENR + `ESL program_PER_ENR` +
                   disabilities_PER_ENR + TOT_REF_PER_ENR + sportsrate + LEA_STATE +
                   calculus_PER_ENR:chemistry_PER_ENR + APenroll_PER_ENR:sportsrate +
                   advancedMath_PER_ENR:disabilities_PER_ENR + calculus_PER_ENR:`ESL program_PER_ENR` +
                   calculus_PER_ENR:disabilities_PER_ENR + APenroll_PER_ENR:calculus_PER_ENR +
                   TOT_REF_PER_ENR:sportsrate + calculus_PER_ENR:dual_PER_ENR,
                 data = final_data)
summary(high_order) #0.4549
length(coef(high_order)) - 1 #69 variables

##Partial F test
anova_result <- anova(first, high_order)
anova_result #go with the interaction model since p-value less than 0.05

plot(resid(high_order) ~ fitted(high_order), xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = 2)
#The residual plot doesn't satisfy the equal variance assumption and linearity assumption; 
#Thus, we do box-cox transformation.
library("MASS")
boxcox(high_order)#optimal around 0.5
final <- lm(formula = sqrt(tested) ~ advancedMath_PER_ENR + APenroll_PER_ENR +
              algebraII_PER_ENR + biology_PER_ENR + calculus_PER_ENR +
              chemistry_PER_ENR + dual_PER_ENR + `ESL program_PER_ENR` +
              disabilities_PER_ENR + TOT_REF_PER_ENR + sportsrate + LEA_STATE +
              calculus_PER_ENR:chemistry_PER_ENR + APenroll_PER_ENR:sportsrate +
              advancedMath_PER_ENR:disabilities_PER_ENR + calculus_PER_ENR:`ESL program_PER_ENR` +
              calculus_PER_ENR:disabilities_PER_ENR + APenroll_PER_ENR:calculus_PER_ENR +
              TOT_REF_PER_ENR:sportsrate + calculus_PER_ENR:dual_PER_ENR,
            data = final_data)
summary(final) #R squared 0.5053
plot(resid(final) ~ fitted(final), xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = 2) #much better

#indepdence satisfied, no trends
plot(resid(final))
abline(h = 0, col = 2)

#normality
qqnorm(resid(final))
qqline(resid(final), col = 2)
#The QQplot symmetric, but heavy tails, especially right. we are not too concerned because we have 10,000+ data points and CLT

par(mfrow=c(1,1))
plot(final)

# For the Residuals vs Fitted plot
plot(final, which = 1)

# For the Normal Q-Q plot, 180988
plot(final, which = 2)

# For the Scale-Location plot
plot(final, which = 3)

# For the Residuals vs Leverage plot
plot(final, which = 5)

nrow(final_data) #11437
cooks_distances <- cooks.distance(final)
influential_points <- which(cooks_distances > (4/(nrow(final_data)-length(coef(final)))))
last_data <- final_data[-influential_points, ]
nrow(last_data) #10801

#rerun w updated dataset
last <- lm(formula = sqrt(tested) ~ advancedMath_PER_ENR + APenroll_PER_ENR +
              algebraII_PER_ENR + biology_PER_ENR + calculus_PER_ENR +
              chemistry_PER_ENR + dual_PER_ENR + `ESL program_PER_ENR` +
              disabilities_PER_ENR + TOT_REF_PER_ENR + sportsrate + LEA_STATE +
              calculus_PER_ENR:chemistry_PER_ENR + APenroll_PER_ENR:sportsrate +
              advancedMath_PER_ENR:disabilities_PER_ENR + calculus_PER_ENR:`ESL program_PER_ENR` +
              calculus_PER_ENR:disabilities_PER_ENR + APenroll_PER_ENR:calculus_PER_ENR +
              TOT_REF_PER_ENR:sportsrate + calculus_PER_ENR:dual_PER_ENR,
            data = last_data)
summary(last) #0.6096 adjusted R squared

# Convert LEA_STATE to character to avoid factor level issues
last_data$LEA_STATE <- as.character(last_data$LEA_STATE)

# Set up cross-validation
CV1<-0
n <- nrow(last_data)
n.shuffle = sample(1:n,n,replace=FALSE)
id.cv <- list()
fold_size <- floor(n / 10)
for (i in 1:10) {
  if (i < 10) {
    id.cv[[i]] <- n.shuffle[((i - 1) * fold_size + 1):(i * fold_size)]
  } else {
    id.cv[[i]] <- n.shuffle[((i - 1) * fold_size + 1):n]
  }
}
for(i in 1:10)
{
  fit1 <- lm(formula = sqrt(tested) ~ advancedMath_PER_ENR + APenroll_PER_ENR +
               algebraII_PER_ENR + biology_PER_ENR + calculus_PER_ENR +
               chemistry_PER_ENR + dual_PER_ENR + `ESL program_PER_ENR` +
               disabilities_PER_ENR + TOT_REF_PER_ENR + sportsrate + LEA_STATE +
               calculus_PER_ENR:chemistry_PER_ENR + APenroll_PER_ENR:sportsrate +
               advancedMath_PER_ENR:disabilities_PER_ENR + calculus_PER_ENR:`ESL program_PER_ENR` +
               calculus_PER_ENR:disabilities_PER_ENR + APenroll_PER_ENR:calculus_PER_ENR +
               TOT_REF_PER_ENR:sportsrate + calculus_PER_ENR:dual_PER_ENR,
             data = last_data[-id.cv[[i]],])
  
  CV1 <- CV1 + (1/n) * sum((last_data$tested[id.cv[[i]]] - predict(fit1,last_data[id.cv[[i]], ]))^2)
  
}

print(CV1) #0.05783713
sqrt(CV1) # roughly speaking, the average difference, 0.2404935
(CV1^2) #brings it back to the prediction error on the original scale, 0.003345134

#assumptions
par(mfrow=c(1,1))


# For the Residuals vs Fitted plot
plot(last, which = 1) #looks good
# For the Normal Q-Q plot
plot(last, which = 2) #normalty looks a lot better
# For the Scale-Location plot
plot(last, which = 3)
# For the Residuals vs Leverage plot
plot(last, which = 5)

#additional visalizations
average_tested_per_state <- aggregate(tested ~ LEA_STATE, last_data, mean)
average_tested_per_state <- average_tested_per_state[order(average_tested_per_state$tested), ]
library(ggplot2)
ggplot(average_tested_per_state, aes(x=LEA_STATE, y=tested)) +
  geom_bar(stat="identity", fill="darkblue") +
  labs(title="Average SAT/ACT Rarticipation Rate by State",
       x="State",
       y="SAT/ACT Rarticipation Rate") +
  theme(axis.text.x=element_text(angle=90, hjust=1))


