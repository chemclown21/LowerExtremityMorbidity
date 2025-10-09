#=======================================
#LE Trauma Vulnerability Project
#Step 1: Stitch together two datasets
#Code written by Vitto Resnick, 10/08/25
#=======================================

#========= UTILITIES =========
rm(list = ls(all.names = TRUE))

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(stringr)
library(cobalt)
library(WeightIt)
library(survey)

negatives = c("no","0","false")

#========= LOADING DATA =========
# Read in your file (assume you named it classified_data.csv)
files_directory = '/Users/vresnick/Documents/GitHub/LowerExtremityMorbidity'
setwd(files_directory)
outdir = "outputs"

dt1 <- fread("original_datasets/Pre_0_geriatric_processed_data_v3.csv")
setnames(dt1, make.names(colnames(dt1)))
dt2 <- fread("original_datasets/Post_0_geriatric_ages_processed_17_23.csv")
setnames(dt2, make.names(colnames(dt2)))

mech_dict <- fread("dicts/mech_dict.csv", colClasses = c(key = "double", value = "character"))



#========= PATIENT DEMOGRAPHICS =========
#inc_key
dt1[, inc_key := as.character(inc_key)]
dt2[, inc_key := as.character(inc_key)]


dt1[, Guidelines := "Pre"]
dt2[, Guidelines := "Post"]

#Age
dt1[, Age := fifelse(age == -99, 90L, as.integer(age))]
dt2[, Age := fifelse(ageyears == -99, 90L, as.integer(ageyears))]

#Sex
dt1[, Sex := fcase(SEX %in% c("Not Known BIU 2", "Not Known/Not Recorded BIU 2"),"", 
                   default = as.character(SEX))]
dt2[, Sex := fcase(SEX %in% c("Male",1,"1.0"),"Male", 
                   SEX %in% c("Female",2,"2.0"),"Female", 
                   SEX %in% c(3,"3.0"),"Nonbinary", 
                   default = as.character(SEX))]

#Race
dt2[, Race := fcase(Race.Complete %in% c("White, Hispanic","White, non-Hispanic"),"White", 
                    Race.Complete %in% c("Black or African American"),"Black", 
                    Race.Complete %in% c("Pacific Islander","American Indian","Unknown","Asian","Other"),"Other", 
                    default = "Missing!")]

#Hispanic_Ethnicity
dt1[, Hispanic_Ethnicity := fcase(Ethnicity %in% c("No"),FALSE,
                                  Ethnicity %in% c("Hispanic Ethnicity"),TRUE)]
dt2[, Hispanic_Ethnicity := fcase(Race.Complete %in% c("White, Hispanic"),TRUE, 
                                  default = FALSE)]

#========= INJURY =========
#ISS relabel for dt1
dt1[, ISS := iss]

#Mechanism
dt1[, Mechanism := fcase(
  grepl("^$", mechanism), "Unspecified",
  default = as.character(mechanism)   # otherwise copy original
)]

setkey(mech_dict, key)
dt2[mech_dict, Mechanism := i.value, on = .(mechanism = key)]

#ECodes
dt1[, Primary_ECode := ecode]
dt2[, Primary_ECode := primaryecodeicd10]
dt1[, ECode_Description := ecodedes]
dt2[, ECode_Description := ecode_desc]

#========= CLINICAL EXPERIENCE =========
#Hospital_Discharge_Disposition
dt1[, Hospital_Discharge_Disposition := fcase(
  grepl("Expired", hospdisp, fixed = TRUE, ignore.case = TRUE),"Deceased/Expired",
  grepl("Discharged/Transferred to inpatient rehab or designated unit", hospdisp, fixed = TRUE, ignore.case = TRUE),           "Discharged/Transferred to inpatient rehab or designated unit",
  grepl("Discharged/Transferred to a short-term general hospital for inpatient", hospdisp, fixed = TRUE, ignore.case = TRUE),  "Discharged/Transferred to a short-term general hospital for inpatient care",
  grepl("Discharged/Transferred to Long Term Care Hospital", hospdisp, fixed = TRUE, ignore.case = TRUE),                      "Discharged/Transferred to Long Term Care Hospital (LTCH)",
  grepl("Discharged/transferred to a psychiatric hospital or psychiatric distin", hospdisp, fixed = TRUE, ignore.case = TRUE), "Discharged/Transferred to a psychiatric hospital or psychiatric distinct part unit of a hospital",
  grepl("Discharged/Transferred to Skilled Nursing Facility", hospdisp, fixed = TRUE, ignore.case = TRUE),                     "Discharged/Transferred to Skilled Nursing Facility (SNF)",
  grepl("Discharged/Transferred to an Intermediate Care Facility", hospdisp, fixed = TRUE, ignore.case = TRUE),                "Discharged/Transferred to an Intermediate Care Facility (ICF)",
  grepl("Discharged/Transferred to an Intermediate Care Facility (ICF)", hospdisp, fixed = TRUE, ignore.case = TRUE),          "Discharged/Transferred to an Intermediate Care Facility (ICF)",
  grepl("Discharge/Transferred to home under care of organized home health serv", hospdisp, fixed = TRUE, ignore.case = TRUE), "Discharged/Transferred to home under care of organized home health service",
  grepl("Discharged to home or self-care (routine discharge)", hospdisp, fixed = TRUE, ignore.case = TRUE),                    "Discharged to home or self-care (routine discharge)",
  grepl("Discharged/Transferred to hospice care", hospdisp, fixed = TRUE, ignore.case = TRUE),                                 "Discharged/Transferred to hospice care",
  grepl("Left against medical advice", hospdisp, fixed = TRUE, ignore.case = TRUE),                                            "Left against medical advice or discontinued care",
  grepl("Left against medical advice or discontinued care", hospdisp, fixed = TRUE, ignore.case = TRUE),                       "Left against medical advice or discontinued care",
  grepl("Discharged/Transferred to another type of institution not defined else", hospdisp, fixed = TRUE, ignore.case = TRUE), "Discharged/Transferred to another type of institution not defined elsewhere",
  grepl("Discharged/Transferred to court/law enforcement", hospdisp, fixed = TRUE, ignore.case = TRUE),                        "Discharged/Transferred to court/law enforcement.",
  grepl("Not Known BIU 2", hospdisp, fixed = TRUE, ignore.case = TRUE),                                                        "Not Known/Not Recorded BIU 2",
  grepl("Not Known/Not Recorded BIU 2", hospdisp, fixed = TRUE, ignore.case = TRUE),                                           "Not Known/Not Recorded BIU 2",
  default = as.character(hospdisp)   # otherwise copy original
)]

dt2[, Hospital_Discharge_Disposition := fcase(
  grepl("^$", hospdischargedisposition), "Not Known/Not Recorded BIU 2",
  grepl("\\<1\\.0\\>", hospdischargedisposition), "Discharged/Transferred to a short-term general hospital for inpatient care",
  grepl("\\<2\\.0\\>", hospdischargedisposition), "Discharged/Transferred to an Intermediate Care Facility (ICF)",
  grepl("\\<3\\.0\\>", hospdischargedisposition), "Discharged/Transferred to home under care of organized home health service",
  grepl("\\<4\\.0\\>", hospdischargedisposition), "Left against medical advice or discontinued care",
  grepl("\\<5\\.0\\>", hospdischargedisposition), "Deceased/Expired",
  grepl("Deceased/expired", hospdischargedisposition, fixed = TRUE, ignore.case = TRUE), "Deceased/Expired",
  grepl("\\<6\\.0\\>", hospdischargedisposition), "Discharged to home or self-care (routine discharge)",
  grepl("\\<7\\.0\\>", hospdischargedisposition), "Discharged/Transferred to Skilled Nursing Facility (SNF)",
  grepl("\\<8\\.0\\>", hospdischargedisposition), "Discharged/Transferred to hospice care",
  grepl("\\<10\\.0\\>", hospdischargedisposition), "Discharged/Transferred to court/law enforcement.",
  grepl("\\<11\\.0\\>", hospdischargedisposition), "Discharged/Transferred to inpatient rehab or designated unit",
  grepl("\\<12\\.0\\>", hospdischargedisposition), "Discharged/Transferred to Long Term Care Hospital (LTCH)",
  grepl("\\<13\\.0\\>", hospdischargedisposition), "Discharged/Transferred to a psychiatric hospital or psychiatric distinct part unit of a hospital",
  grepl("\\<14\\.0\\>", hospdischargedisposition), "Discharged/Transferred to another type of institution not defined elsewhere",
  default = as.character(hospdischargedisposition)   # otherwise copy original
)]


#clean up rehab discharge data  (making sure that anything unknown is NA)
dt1[, Rehab_Discharge := fcase(Hospital_Discharge_Disposition %in% c("Discharged/Transferred to another type of rehabilitation or long term","Discharged/Transferred to inpatient rehab or designated unit"), TRUE,
  default = FALSE)]
dt2[, Rehab_Discharge := fcase(Hospital_Discharge_Disposition %in% c("Discharged/Transferred to another type of rehabilitation or long term","Discharged/Transferred to inpatient rehab or designated unit"), TRUE,
                               default = FALSE)]

#Death
dt2[, Death := fcase(
  grepl("Deceased/Expired", Hospital_Discharge_Disposition), TRUE,
  default = FALSE)]

#Payment Method
dt1[, Primary_Payment_Method := fcase( #let's just add the word insurance and then it matches the new DB!
  grepl("Private/Commercial", payment,fixed = TRUE), "Private/Commercial Insurance",
  default = as.character(payment)   # otherwise copy original
)]

dt2[, Primary_Payment_Method := fcase(
  grepl("Medicare", primarymethodpayment,fixed = TRUE), "Medicare, Medicaid, or Government",
  grepl("Medicaid", primarymethodpayment,fixed = TRUE), "Medicare, Medicaid, or Government",
  grepl("Other Government", primarymethodpayment,fixed = TRUE), "Medicare, Medicaid, or Government",
  
  grepl("Private/Commercial Insurance", primarymethodpayment,fixed = TRUE), "Private/Commercial Insurance",
  #typo!!!
  grepl("Private/Commerical Insurance", primarymethodpayment,fixed = TRUE), "Private/Commercial Insurance",
  
  grepl("Self-Pay", primarymethodpayment,fixed = TRUE), "No insurance/self-pay",
  grepl("Not Billed (for any reason)", primarymethodpayment,fixed = TRUE), "No insurance/self-pay",
  
  grepl("Other", primarymethodpayment,fixed = TRUE), "Unknown Payer",
  grepl("^$", primarymethodpayment), "Unknown Payer",
  default = "uh oh!"   # otherwise copy original
)]

#Long_ICU_LOS_3d
dt1[, Long_ICU_LOS_3d := fcase(icudays>3,TRUE,
                               is.na(icudays),NA,
                               icudays %in% c(-2, -29, "-2", "-29"), NA,
                               default = FALSE)]
dt2[, Long_ICU_LOS_3d := fcase(totaliculos %in% c(">3d"),TRUE,default = FALSE)]

#Long_LOS_to_Final_Discharge_5d
dt1[, Long_LOS_to_Final_Discharge_5d := fcase(losdays>5,TRUE,
                                         is.na(losdays),NA,
                                         losdays %in% c(-2, -29, "-2", "-29"), NA,
                                         default = FALSE)]
dt2[, Long_LOS_to_Final_Discharge_5d := fcase(finaldischargedays %in% c(">5d"),TRUE,default = FALSE)]





#========= COMORBIDITIES =========

#Comorbidity 1: Other_Comorbidity
dt1[, Other_Comorbidity := fcase(is.na(Other),FALSE,
                                 Other %in% c(1, "1"),TRUE,default = FALSE)]
dt2[, Other_Comorbidity := !tolower(as.character(Other_pc)) %in% c("","0","false","no")]


#Comorbidity 2: Alcohol_Use_Disorder_Alcoholism
dt1[, Alcohol_Use_Disorder_Alcoholism := fcase(is.na(Alcoholism),FALSE,
                                               Alcoholism %in% c(2, "2"),TRUE,default = FALSE)]
dt2[, Alcohol_Use_Disorder_Alcoholism := !tolower(as.character(Alcohol.Use.Disorder)) %in% c("","0","false","no")]

#Comorbidity 4: Bleeding_Disorder
dt1[, Bleeding_Disorder := fcase(is.na(Bleeding.Disorder),FALSE,
                                 Bleeding.Disorder %in% c(4, "4"),TRUE, default = FALSE)]
dt2[, Bleeding_Disorder := !tolower(as.character(Bleeding.Disorder)) %in% c("","0","false","no")]

#Comorbidity 5: Currently_Receiving_Chemotherapy_for_Cancer
dt1[, Currently_Receiving_Chemotherapy_for_Cancer := fcase(is.na(Chemotherapy),FALSE,
                                                           Chemotherapy %in% c(5, "5"),TRUE, default = FALSE)]
dt2[, Currently_Receiving_Chemotherapy_for_Cancer := !tolower(as.character(Currently.Receiving.Chemotherapy.for.Cancer)) %in% c("","0","false")]

#Comorbidity 6: Congenital_Anomalies 
dt1[, Congenital_Anomalies := fcase(is.na(Congenital.Anomalies),FALSE,
                                    Congenital.Anomalies %in% c(6, "6"),TRUE, default = FALSE)]
dt2[, Congenital_Anomalies := fcase(Congenital.Anomalies %in% c(1, "1"),TRUE, default = FALSE)]

#Comorbidity 7: Congestive_Heart_Failure_CHF
dt1[, Congestive_Heart_Failure_CHF := fcase(is.na(CHF),FALSE,
                                            CHF %in% c(7, "7"),TRUE, default = FALSE)]
dt2[, Congestive_Heart_Failure_CHF := !tolower(as.character(Congestive.Heart.Failure)) %in% c("","0","false","no")]

#Comorbidity 8: Current_Smoker
dt1[, Current_Smoker := fcase(is.na(Smoker),FALSE,
                              Smoker %in% c(8, "8"),TRUE, default = FALSE)]
dt2[, Current_Smoker := !tolower(as.character(Current.Smoker)) %in% c("","0","false","no")]

#Comorbidity 9: Chronic_Renal_Failure_CKD
dt1[, Chronic_Renal_Failure_CKD := fcase(is.na(CKD),FALSE,
                                         CKD %in% c(9, "9"),TRUE, default = FALSE)]
dt2[, Chronic_Renal_Failure_CKD := !tolower(as.character(Chronic.Renal.Failure)) %in% c("","0","false","no")]

#Comorbidity 10: Cerebrovascular Accident (CVA) with residual neurological deficit
dt1[, CVA := fcase(is.na(CVA.residual.neuro.defect),FALSE,
                   CVA.residual.neuro.defect %in% c(10, "10"),TRUE, default = FALSE)]
dt2[, CVA := !tolower(as.character(Cerebrovascular.Accident)) %in% c("","0","false","no")]

#Comorbidity 11: Diabetes_Mellitus
dt1[, Diabetes_Mellitus := fcase(is.na(Diabetes.mellitus),FALSE,
                                 Diabetes.mellitus %in% c(11, "11"),TRUE, default = FALSE)]
dt2[, Diabetes_Mellitus := !tolower(as.character(Diabetes.Mellitus)) %in% c("","0","false","no")]

#Comorbidity 12: Disseminated_Cancer
dt1[, Disseminated_Cancer := fcase(is.na(Disseminated.cancer),FALSE,
                                   Disseminated.cancer %in% c(12, "12"),TRUE, default = FALSE)]
dt2[, Disseminated_Cancer := !tolower(as.character(Disseminated.Cancer)) %in% c("","0","false","no")]

#Comorbidity 15: Functionally_Dependent_Health_Status
dt1[, Functionally_Dependent_Health_Status := fcase(is.na(Functionaly.Dependent.health.status),FALSE,
                                                    Functionaly.Dependent.health.status %in% c(15, "15"),TRUE,default = FALSE)]
dt2[, Functionally_Dependent_Health_Status := !tolower(as.character(Functionaly.Dependent.Health.Status)) %in% c("no","0","false","")]

#Comorbidity 17: History of Myocardial Infarction Comorbidity (History_MI_pc)
dt1[, History_MI_pc := fcase(is.na(History.of.myocardial.infarction),FALSE,
                             History.of.myocardial.infarction %in% c(17, "17"),TRUE,default = FALSE)]
dt2[, History_MI_pc := !tolower(as.character(Myocardial.Infarction_pc)) %in% c("no","0","false","")]

#Comorbidity 19: Hypertension_Requiring_Medication
dt1[, Hypertension_Requiring_Medication := fcase(is.na(Hypertension.requiring.medication),FALSE,
                                                 Hypertension.requiring.medication %in% c(19, "19"),TRUE,default = FALSE)]
dt2[, Hypertension_Requiring_Medication := !tolower(as.character(Hypertension)) %in% c("no","0","false","")]

#Comorbidity 21: Prematurity_37_Weeks
dt1[, Prematurity_37_Weeks := fcase(is.na(Prematurity),FALSE,
                                    Prematurity %in% c(21, "21"),TRUE, default = FALSE)]
dt2[, Prematurity_37_Weeks := !tolower(as.character(Prematurity)) %in% c("no","0","false","")]

#Comorbidity 23: Respiratory Disease & Chronic Obstructive Pulmonary Disease
dt1[, Respiratory_COPD := fcase(is.na(Respiratory.Disease),FALSE,
                                Respiratory.Disease %in% c(23, "23"),TRUE, default = FALSE)]
dt2[, Respiratory_COPD := !tolower(as.character(Chronic.Obstructive.Pulmonary.Disease)) %in% c("no","0","false","")]

#Comorbidity 24: Steroid_Use
dt1[, Steroid_Use := fcase(is.na(Steroid.Use),FALSE,
                           Steroid.Use %in% c(24, "24"),TRUE, default = FALSE)]
dt2[, Steroid_Use := !tolower(as.character(Steroid.Use)) %in% c("no","0","false","")]

#Comorbidity 25: Cirrhosis
dt1[, Cirrhosis_ := fcase(is.na(Cirrhosis),FALSE,
                          Cirrhosis %in% c(25, "25"),TRUE, default = FALSE)]
dt2[, Cirrhosis_ := !tolower(as.character(Cirrhosis)) %in% c("","0","false","no")]

#Comorbidity 26: Dementia
dt1[, Dementia_ := fcase(is.na(Dementia),FALSE,
                         Dementia %in% c(26, "26"),TRUE, default = FALSE)]
dt2[, Dementia_ := !tolower(as.character(Dementia)) %in% c("","0","false","no")]

#Comorbidity 28: Substance_Abuse_Disorder 
dt1[, Dementia_ := fcase(is.na(Drug.use.disorder),FALSE,
                         Drug.use.disorder %in% c(28, "28"),TRUE, default = FALSE)]
dt2[, Dementia_ := !tolower(as.character(Substance.Abuse.Disorder)) %in% c("","0","false","no")]

#========= COMPLICATIONS =========

#Complication 1: Other_Hospital_Complication
dt1[, Other_Hospital_Complication := fcase(Other.Complication %in% c(1, "1"),TRUE, default = FALSE)]
dt2[, Other_Hospital_Complication := !tolower(as.character(Other_hc)) %in% c("no","0","false")]

#Complication 4: Acute_Kidney_Injury
dt1[, Acute_Kidney_Injury := fcase(Acute.Kidney.Injury %in% c(4, "4"),TRUE, default = FALSE)]
dt2[, Acute_Kidney_Injury := !tolower(as.character(Acute.Kidney.Injury)) %in% c("no","0","false")]

#Complication 5: Acute_Lung_Injury_ARDS
dt1[, Acute_Lung_Injury_ARDS := fcase(Acute.lung.injury.ARDS %in% c(5, "5"),TRUE, default = FALSE)]
dt2[, Acute_Lung_Injury_ARDS := !tolower(as.character(ARDS)) %in% c("no","0","false")]

#Complication 8: Cardiac_Arrest_with_Resuscitative_Efforts
dt1[, Cardiac_Arrest_with_Resuscitative_Efforts := fcase(Cardiac.arrest.with.resuscitative.efforts %in% c(8, "8"),TRUE, default = FALSE)]
dt2[, Cardiac_Arrest_with_Resuscitative_Efforts := !tolower(as.character(Cardiac.Arrest)) %in% c("no","0","false")]

#Complication 12: Deep Surgical Site Infection (Deep_SSI)
dt1[, Deep_SSI := fcase(Deep.surgical.site.infection %in% c(12, "12"),TRUE, default = FALSE)]
dt2[, Deep_SSI := !tolower(as.character(Deep.Surgical.Site.Infection)) %in% c("no","0","false")]

#Complication 13: Drug_or_Alcohol_Withdrawal_Syndrome
dt1[, Drug_or_Alcohol_Withdrawal_Syndrome := fcase(Drug.or.alcohol.withdrawal.syndrome %in% c(13, "13"),TRUE, default = FALSE)]
dt2[, Drug_or_Alcohol_Withdrawal_Syndrome := !tolower(as.character(Alcohol.Withdrawal.Syndrome)) %in% c("no","0","false")]

#Complication 14: Deep_Vein_Thrombosis_DVT_Thrombophlebitis
dt1[, Deep_Vein_Thrombosis_DVT_Thrombophlebitis := fcase(DVT.thrombophlebitis %in% c(14, "14"),TRUE, default = FALSE)]
dt2[, Deep_Vein_Thrombosis_DVT_Thrombophlebitis := !tolower(as.character(Deep.Surgical.Site.Infection)) %in% c("no","0","false")]

#Complication 15: Extremity_Compartment_Syndrome
dt1[, Extremity_Compartment_Syndrome := fcase(Extremity.compartment.syndrome %in% c(15, "15"),TRUE, default = FALSE)]
dt2[, Extremity_Compartment_Syndrome := !tolower(as.character(Extremity.Compartment.Syndrome)) %in% c("no","0","false","")]

#Complication 18: Myocardial Infarction Hospital Complication (MI_hc)
dt1[, MI_hc := fcase(Myocardial.infarction %in% c(18, "18"),TRUE, default = FALSE)]
dt2[, MI_hc := !tolower(as.character(Myocardial.Infarction_hc)) %in% c("no","0","false","")]

#Complication 19: Organ/Space Surgical site infection (Organ_Space_SSI)
dt1[, Organ_Space_SSI := fcase(Organ.space.surgical.site.infection %in% c(19, "19"),TRUE, default = FALSE)]
dt2[, Organ_Space_SSI := !tolower(as.character(Organ.Space.SSI)) %in% c("no","0","false","")]

#Complication 21: Pulmonary_Embolism
dt1[, Pulmonary_Embolism := fcase(Pulmonary.embolism %in% c(21, "21"),TRUE, default = FALSE)]
dt2[, Pulmonary_Embolism := !tolower(as.character(Pulmonary.Embolism)) %in% c("no","0","false","")]

#Complication 22: Stroke_CVA
dt1[, Stroke_CVA := fcase(Stroke.CVA %in% c(22, "22"),TRUE, default = FALSE)]
dt2[, Stroke_CVA := !tolower(as.character(Stroke.CVA)) %in% c("no","0","false","")]

#Complication 23: Superficial Incisional Surgical Site Infection (Superficial_Incisional_SSI)
dt1[, Superficial_Incisional_SSI := fcase(Superficial.surgical.site.infection %in% c(23, "23"),TRUE, default = FALSE)]
dt2[, Superficial_Incisional_SSI := !tolower(as.character(Superficial.Incisional.SSI)) %in% c("no","0","false","")]


#Complication 25: Unplanned_Intubation
dt1[, Unplanned_Intubation := fcase(Unplanned.intubation %in% c(25, "25"),TRUE,default = FALSE)]
dt2[, Unplanned_Intubation := !tolower(as.character(Unplanned.Intubation)) %in% c("no","0","false","")]


#Complication 28: Central Line-associated Bloodstream Infection (CLABSI)
dt1[, CLABSI := fcase(Catheter.related.blood.stream.infection %in% c(28, "28"),TRUE, default = FALSE)]
dt2[, CLABSI := !tolower(as.character(Central.Line.associated.Bloodstream.Infection)) %in% c("no","0","false")]

#Complication 29: Osteomyelitis
dt1[, Osteomyelitis_ := fcase(Osteomyelitis %in% c(29, "29"),TRUE, default = FALSE)]
dt2[, Osteomyelitis_ := !tolower(as.character(Osteomyelitis)) %in% c("no","0","false","")]

#Complication 30: Unplanned return/visit to the OR (Unplanned_OR)
dt1[, Unplanned_OR := fcase(Unplanned.return.to.the.OR %in% c(30, "30"),TRUE,default = FALSE)]
dt2[, Unplanned_OR := !tolower(as.character(Unplanned.Visit.to.OR)) %in% c("no","0","false","")]

#Complication 31: Unplanned Return/Admission to the ICU (Unplanned_ICU)
dt1[, Unplanned_ICU := fcase(Unplanned.return.to.the.ICU %in% c(31, "31"),TRUE, default = FALSE)]
dt2[, Unplanned_ICU := !tolower(as.character(Unplanned.admission.to.ICU)) %in% c("no","0","false","")]

#Complication 32: Severe_Sepsis
dt1[, Severe_Sepsis := fcase(Severe.Sepsis %in% c(31, "31"),TRUE, default = FALSE)]
dt2[, Severe_Sepsis := !tolower(as.character(Severe.Sepsis)) %in% c("no","0","false","")]


#========= STITCH DATASETS =========

# example: make these columns logical in both
logi_cols <- c(
  "Acute_Kidney_Injury","Acute_Lung_Injury_ARDS","Alcohol_Use_Disorder_Alcoholism",
  "Drug_or_Alcohol_Withdrawal_Syndrome","Bleeding_Disorder","Cardiac_Arrest_with_Resuscitative_Efforts",
  "CLABSI","Chronic_Renal_Failure_CKD","Cirrhosis_","Congenital_Anomalies","Congestive_Heart_Failure_CHF",
  "Current_Smoker","Currently_Receiving_Chemotherapy_for_Cancer","Deep_SSI",
  "Deep_Vein_Thrombosis_DVT_Thrombophlebitis","Dementia_","Diabetes_Mellitus","Disseminated_Cancer",
  "Extremity_Compartment_Syndrome","Organ_Space_SSI","Osteomyelitis_","Prematurity_37_Weeks",
  "Pulmonary_Embolism","Severe_Sepsis","Steroid_Use","Stroke_CVA",
  "Superficial_Incisional_SSI","Unplanned_ICU","Unplanned_Intubation","Unplanned_OR",
  "Functionally_Dependent_Health_Status","Hypertension_Requiring_Medication",
  "Long_ICU_LOS_3d","LOS_to_Final_Discharge_5d"
)

coerce_logi <- function(D, cols) {
  exist <- intersect(cols, names(D))
  if (length(exist)) D[, (exist) := lapply(.SD, as.logical), .SDcols = exist]
  D
}
dt1 <- coerce_logi(dt1, logi_cols)
dt2 <- coerce_logi(dt2, logi_cols)

# bind
dt_all <- rbindlist(list(dt1, dt2), use.names = TRUE, fill = TRUE)


#========= EXPORT =========
fname_csv <- sprintf("1_stitched_Pre_Post_%s.csv", format(Sys.Date(), "%y%m%d"))
fname_rds <- sprintf("1_stitched_Pre_Post_%s.rds", format(Sys.Date(), "%y%m%d"))

fwrite(dt_all, file.path(outdir, fname_csv))
saveRDS(dt_all, file.path(outdir, fname_rds))
