### Functions for the shiny app

# Database initialization -------------------------------------------------

# This will create the initial database for a new user
# Only runs the first time they log in
# Includes the monthly update dashboard data to save
firstDB <- function(loc, filename, username){
  
  lst <- list()
  
  lst$demographics <- data.frame(
    Username = username,
    demographicsDate = as.Date(NA),
    HealthSystem = as.character(""),
    Q1_DBA = as.character(""),
    Q2 = as.character(""),
    Q2_other = as.character(""),
    Q3 = as.numeric(NA),
    Q4a = as.character(""),
    Q4b = as.character(""),
    Q4c = as.character(""),
    Q5 = as.character(""),
    Q5_Email = as.character(""),
    Q6 = as.character(""),
    Q7 = as.character(""),
    Q8 = as.character(""),
    Q8_Email = as.character(""),
    Q9 = as.character(""),
    Q10 = as.character(""),
    Q10_other = as.numeric(0),
    Q11 = as.character(""),
    Q11_1 = as.character(""),
    Q11_2 = as.character(""),
    Q11_3 = as.character(""),
    Q11_4 = as.character(""),
    Q11_5 = as.character(""),
    Q11_other = as.character(""),
    Q12 = as.character(""),
    Q12_1 = as.character(""),
    Q12_2 = as.character(""),
    Q12_3 = as.character(""),
    Q12_4 = as.character(""),
    Q12_5 = as.character(""),
    Q12_6 = as.character(""),
    Q12_other = as.character(""),
    Q13 = as.character(""),
    Q13_amount = as.character(""),
    Q13_source = as.character(""),
    Q13_date = as.Date(NA),
    Q13_length = as.numeric(NA),
    Q14 = as.character(""),
    Q14_1 = as.character(""),
    Q14_2 = as.character(""),
    Q14_3 = as.character(""),
    Q14_other = as.character(""),
    DemoComments = as.character("")
  )
  
  
  lst$systems <- data.frame(
    Username = username,
    systemsDate = as.Date(NA),
    Q15 = as.character(""),
    Q15_other = as.character(""),
    Q15_EHRversion = as.character(""),
    Q16 = as.character(""),
    Q17 = as.character(""),
    Q17_1 = as.character(""),
    Q17_2 = as.character(""),
    Q17_3 = as.character(""),
    Q17_4 = as.character(""),
    Q17_5 = as.character(""),
    Q18 = as.character(""),
    Q19 = as.character(""),
    Q19_1 = as.character(""),
    Q19_2 = as.character(""),
    Q19_3 = as.character(""),
    Q19_4 = as.character(""),
    Q19_5 = as.character(""),
    Q20 = as.character(""),
    Q20_notes = as.character(""),
    Q20_orders = as.character(""),
    Q20_other = as.character(""),
    Q21 = as.character(""),
    Q21_text = as.character(""),
    Q22 = as.character(""),
    Q22_notes = as.character(""),
    Q23 = as.character(""),
    Q23_notes = as.character(""),
    Q24 = as.character(""),
    Q24_years = as.numeric(NA),
    Q25 = as.character(""),
    Q25_1 = as.character(""),
    Q25_2 = as.character(""),
    Q25_3 = as.character(""),
    Q25_4 = as.character(""),
    Q25_5 = as.character(""),
    Q25_other = as.character(""),
    Q26 = as.character(""),
    Q26_1 = as.character(""),
    Q26_2 = as.character(""),
    Q26_3 = as.character(""),
    Q26_4 = as.character(""),
    Q26_5 = as.character(""),
    Q26_6 = as.character(""),
    Q26_7 = as.character(""),
    Q26_8 = as.character(""),
    Q26_9 = as.character(""),
    Q26_other = as.character(""),
    SystemsComments = as.character("")
  )
  
  lst$savedRates <- data.frame(
    Username = username,
    baseratesDate = as.Date(NA),
    Q27 = as.character(""),
    Q27_other = as.character(""),
    Q28 = as.character(""),
    Q28_1 = as.character(""),
    Q28_2 = as.character(""),
    Q28_3 = as.character(""),
    Q28_4 = as.character(""),
    Q28_other = as.character(""),
    Q29 = as.character(""),
    Q30 = as.character(""),
    FemAge1_total = as.numeric(NA),
    FemAge1_dose1 = as.numeric(NA),
    FemAge1_dose2 = as.numeric(NA),
    MenAge1_total = as.numeric(NA),
    MenAge1_dose1 = as.numeric(NA),
    MenAge1_dose2 = as.numeric(NA),
    BothAge1_total = as.numeric(NA),
    BothAge1_dose1 = as.numeric(NA),
    BothAge1_dose2 = as.numeric(NA),
    FemAge2_total = as.numeric(NA),
    FemAge2_dose1 = as.numeric(NA),
    FemAge2_dose2 = as.numeric(NA),
    FemAge2_mening = as.numeric(NA),
    FemAge2_tdap = as.numeric(NA),
    MenAge2_total = as.numeric(NA),
    MenAge2_dose1 = as.numeric(NA),
    MenAge2_dose2 = as.numeric(NA),
    MenAge2_mening = as.numeric(NA),
    MenAge2_tdap = as.numeric(NA),
    BothAge2_total = as.numeric(NA),
    BothAge2_dose1 = as.numeric(NA),
    BothAge2_dose2 = as.numeric(NA),
    BothAge2_mening = as.numeric(NA),
    BothAge2_tdap = as.numeric(NA),
    FemAge3_total = as.numeric(NA),
    FemAge3_dose1 = as.numeric(NA),
    FemAge3_dose2 = as.numeric(NA),
    FemAge3_mening = as.numeric(NA),
    FemAge3_tdap = as.numeric(NA),
    MenAge3_total = as.numeric(NA),
    MenAge3_dose1 = as.numeric(NA),
    MenAge3_dose2 = as.numeric(NA),
    MenAge3_mening = as.numeric(NA),
    MenAge3_tdap = as.numeric(NA),
    BothAge3_total = as.numeric(NA),
    BothAge3_dose1 = as.numeric(NA),
    BothAge3_dose2 = as.numeric(NA),
    BothAge3_mening = as.numeric(NA),
    BothAge3_tdap = as.numeric(NA),
    Q31 = as.character(""),
    Q31_other = as.character(""),
    Q32 = as.character(""),
    Q32_details = as.character(""),
    Q32_details1 = as.character(""),
    Q32_details2 = as.character(""),
    Q32_details3 = as.character(""),
    Q32_details4 = as.character(""),
    Q32_other = as.character(""),
    Q33 = as.character(""),
    Q33_other = as.character(""),
    Q34 = as.character(""),
    RatesComments = as.character("")
  )
  
  lst$savedActivities <- data.frame(
    Username = username,
    activitiesDate = as.Date(NA),
    Q35 = as.character(""),
    Q35_1 = as.character(""),
    Q35_2 = as.character(""),
    Q35_3 = as.character(""),
    Q35_4 = as.character(""),
    Q35_other = as.character(""),
    Q36a = as.numeric(NA),
    Q36b = as.character(""),
    Q36c = as.character(""),
    Q36d = as.character(""),
    Q36e = as.character(""),
    Q36f = as.character(""),
    Q37 = as.character(""),
    Q37_1 = as.character(""),
    Q37_2 = as.character(""),
    Q37_3 = as.character(""),
    Q37_4 = as.character(""),
    Q38 = as.character(""),
    Q38_1 = as.character(""),
    Q38_2 = as.character(""),
    Q38_3 = as.character(""),
    Q38_4 = as.character(""),
    Q38_5 = as.character(""),
    Q38_6 = as.character(""),
    Q38_7 = as.character(""),
    Q38_8 = as.character(""),
    Q38_9 = as.character(""),
    Q38_other = as.character(""),
    Q39 = as.character(""),
    Q39_1 = as.character(""),
    Q39_2 = as.character(""),
    Q39_3 = as.character(""),
    Q39_4 = as.character(""),
    Q39_5 = as.character(""),
    Q39_other = as.character(""),
    Q40 = as.character(""),
    Q41 = as.character(""),
    Q42 = as.character(""),
    act1 = as.character(""),
    act2 = as.character(""),
    act3 = as.character(""),
    act4 = as.character(""),
    act5 = as.character(""),
    act6 = as.character(""),
    act7 = as.character(""),
    act8 = as.character(""),
    act9 = as.character(""),
    act10 = as.character(""),
    time1 = as.character(""),
    time2 = as.character(""),
    time3 = as.character(""),
    time4 = as.character(""),
    time5 = as.character(""),
    time6 = as.character(""),
    time7 = as.character(""),
    time8 = as.character(""),
    time9 = as.character(""),
    time10 = as.character(""),
    ppl1 = as.character(""),
    ppl2 = as.character(""),
    ppl3 = as.character(""),
    ppl4 = as.character(""),
    ppl5 = as.character(""),
    ppl6 = as.character(""),
    ppl7 = as.character(""),
    ppl8 = as.character(""),
    ppl9 = as.character(""),
    ppl10 = as.character(""),
    ActivitiesComments = as.character("")
  )
  
  lst$followupRates <- data.frame(
    Username = username,
    fwupratesDate = as.Date(NA),
    Q1FU = as.character(""),
    Q1FU_other = as.character(""),
    Q2FU = as.character(""),
    Q2FU_1 = as.character(""),
    Q2FU_2 = as.character(""),
    Q2FU_3 = as.character(""),
    Q2FU_4 = as.character(""),
    Q2FU_other = as.character(""),
    Q3FU = as.character(""),
    Q4FU = as.character(""),
    FemFUAge1_total = as.numeric(NA),
    FemFUAge1_dose1 = as.numeric(NA),
    FemFUAge1_dose2 = as.numeric(NA),
    MenFUAge1_total = as.numeric(NA),
    MenFUAge1_dose1 = as.numeric(NA),
    MenFUAge1_dose2 = as.numeric(NA),
    BothFUAge1_total = as.numeric(NA),
    BothFUAge1_dose1 = as.numeric(NA),
    BothFUAge1_dose2 = as.numeric(NA),
    FemFUAge2_total = as.numeric(NA),
    FemFUAge2_dose1 = as.numeric(NA),
    FemFUAge2_dose2 = as.numeric(NA),
    FemFUAge2_mening = as.numeric(NA),
    FemFUAge2_tdap = as.numeric(NA),
    MenFUAge2_total = as.numeric(NA),
    MenFUAge2_dose1 = as.numeric(NA),
    MenFUAge2_dose2 = as.numeric(NA),
    MenFUAge2_mening = as.numeric(NA),
    MenFUAge2_tdap = as.numeric(NA),
    BothFUAge2_total = as.numeric(NA),
    BothFUAge2_dose1 = as.numeric(NA),
    BothFUAge2_dose2 = as.numeric(NA),
    BothFUAge2_mening = as.numeric(NA),
    BothFUAge2_tdap = as.numeric(NA),
    FemFUAge3_total = as.numeric(NA),
    FemFUAge3_dose1 = as.numeric(NA),
    FemFUAge3_dose2 = as.numeric(NA),
    FemFUAge3_mening = as.numeric(NA),
    FemFUAge3_tdap = as.numeric(NA),
    MenFUAge3_total = as.numeric(NA),
    MenFUAge3_dose1 = as.numeric(NA),
    MenFUAge3_dose2 = as.numeric(NA),
    MenFUAge3_mening = as.numeric(NA),
    MenFUAge3_tdap = as.numeric(NA),
    BothFUAge3_total = as.numeric(NA),
    BothFUAge3_dose1 = as.numeric(NA),
    BothFUAge3_dose2 = as.numeric(NA),
    BothFUAge3_mening = as.numeric(NA),
    BothFUAge3_tdap = as.numeric(NA),
    FollowupRatesComments = as.character("")
  )
  
  lst$additionalInfo <- data.frame(
    Username = username,
    additionalInfoDate = as.Date(NA),
    Q5FU = as.character(""),
    Q5FU_text = as.character(""),
    Q6FU = as.character(""),
    Q6FU_text = as.character(""),
    Q7FU = as.character(""),
    Q7FU_1 = as.character(""),
    Q7FU_2 = as.character(""),
    Q7FU_3 = as.character(""),
    Q7FU_4 = as.character(""),
    Q7FU_5 = as.character(""),
    Q7FU_other = as.character(""),
    Q8FU = as.character(""),
    Q8FU_1 = as.character(""),
    Q8FU_2 = as.character(""),
    Q8FU_3 = as.character(""),
    Q8FU_other = as.character(""),
    Q9FU = as.character(""),
    Q9FU_1 = as.character(""),
    Q9FU_2 = as.character(""),
    Q9FU_3 = as.character(""),
    Q9FU_4 = as.character(""),
    Q9FU_5 = as.character(""),
    Q9FU_6 = as.character(""),
    Q9FU_7 = as.character(""),
    Q9FU_8 = as.character(""),
    Q9FU_9 = as.character(""),
    Q9FU_other = as.character(""),
    Q10FU = as.character(NA),
    Q11FU = as.character(NA),
    Q12FU = as.character(NA),
    Q13FU = as.character(""),
    Q13FU_1 = as.character(""),
    Q13FU_2 = as.character(""),
    Q13FU_3 = as.character(""),
    Q13FU_4 = as.character(""),
    Q13FU_5 = as.character(""),
    Q13FU_6 = as.character(""),
    Q13FU_7 = as.character(""),
    Q13FU_8 = as.character(""),
    Q13FU_other = as.character(""),
    Q14FU = as.character(""),
    Q14FU_1 = as.character(""),
    Q14FU_2 = as.character(""),
    Q14FU_3 = as.character(""),
    Q14FU_4 = as.character(""),
    Q14FU_5 = as.character(""),
    Q14FU_other = as.character(""),
    Q15FU = as.character(""),
    Q16FU = as.character(""),
    Q17FU = as.character(""),
    Q18FU = as.character(""),
    Q18FU_more = as.character(""),
    AdditionalInfoComments = as.character("")
  )
  
  
  
  # And the monthly updates -------------------------------------------------
  
  myMonths <- c("January",
                "February",
                "March",
                "April",
                "May",
                "June",
                "July",
                "August",
                "September",
                "October",
                "November",
                "December")
  # Three ages
  
  # 9-10
  f <- data.frame(Month = myMonths,
                  Total = NA,
                  Dose1 = NA,
                  Dose2 = NA)
  age1 <- Reduce(function(x,y) full_join(x,y,"Month"), list(f,f,f))
  names(age1) <- c("Month", "TotalF", "Dose1F", "Dose2F", "TotalM", "Dose1M", "Dose2M", "Total", "Dose1", "Dose2")
  rm(f)
  
  # 11-12
  f <- data.frame(Month = myMonths,
                  Total = NA,
                  Dose1 = NA,
                  Dose2 = NA,
                  Meningococcal = NA,
                  TDap = NA)
  age2 <- Reduce(function(x,y) full_join(x,y,"Month"), list(f,f,f))
  names(age2) <- c("Month", "TotalF","Dose1F", "Dose2F", "MeningococcalF", "TDapF",
                   "TotalM","Dose1M", "Dose2M", "MeningococcalM", "TDapM",
                   "Total","Dose1", "Dose2", "Meningococcal", "TDap")
  rm(f)
  
  # 13
  age3 <- age2
  
  
  lst$age1 <- age1
  lst$age2 <- age2
  lst$age3 <- age3
  
  
  
  myDB <- dbConnect(RSQLite::SQLite(), filename, overwrite = T)
  lapply(names(lst), function(x) {
    dbWriteTable(myDB, x, lst[[x]], overwrite = T)
  })
  
  
  base <-  c("demographics", "systems", "savedRates", "savedActivities")
  fu <- c("followupRates", "additionalInfo")
  
  baselineTable <- lapply(base, function(x) {
    df <- dbReadTable(myDB, x)
  }) %>% 
    Reduce(function(x,y) full_join(x,y,"Username"),.)
  
  followupTable <- lapply(fu, function(x) {
    df <- dbReadTable(myDB, x)
  }) %>% 
    Reduce(function(x,y) full_join(x,y,"Username"),.)
  
  n <- names(baselineTable)[-1]
  m <- names(followupTable)[-1]
  
  baselineTable$BaselineSubmitDate <- as.Date(NA)
  followupTable$FollowupSubmitDate <- as.Date(NA)
  
  baselineTable <- baselineTable[,c("Username", "BaselineSubmitDate",n)]
  followupTable <- followupTable[,c("Username", "FollowupSubmitDate",m)]
  
  
  dbWriteTable(myDB, "baselineTable", baselineTable, overwrite = T)
  dbWriteTable(myDB, "followupTable", followupTable, overwrite = T)
  dbDisconnect(myDB)
  
  saveAzure(loc, filename)
  
  storage_download(loc, filename)
  myDB <- dbConnect(SQLite(), filename)

  
}

# This is to generate a hard stop
# If the user does not input any rate information, we don't want them to submit the data
# This nullDB just creates an empty dataset
# Whatever they submit will be compared to the empty version and if they match, an error will come up
# It isn't fancy, but I was feeling lazy
nullDB <-  function(){
  lst <- list()

lst$demographics <- data.frame(
  Q1_DBA = as.character(""),
  Q2 = as.character(""),
  Q2_other = as.character(""),
  Q3 = as.numeric(NA),
  Q4a = as.character(""),
  Q4b = as.character(""),
  Q4c = as.character(""),
  Q5 = as.character(""),
  Q5_Email = as.character(""),
  Q6 = as.character(""),
  Q7 = as.character(""),
  Q8 = as.character(""),
  Q8_Email = as.character(""),
  Q9 = as.character(""),
  Q10 = as.character(""),
  Q10_other = as.numeric(0),
  Q11 = as.character(""),
  Q11_1 = as.character(""),
  Q11_2 = as.character(""),
  Q11_3 = as.character(""),
  Q11_4 = as.character(""),
  Q11_5 = as.character(""),
  Q11_other = as.character(""),
  Q12 = as.character(""),
  Q12_1 = as.character(""),
  Q12_2 = as.character(""),
  Q12_3 = as.character(""),
  Q12_4 = as.character(""),
  Q12_5 = as.character(""),
  Q12_6 = as.character(""),
  Q12_other = as.character(""),
  Q13 = as.character(""),
  Q13_amount = as.character(""),
  Q13_source = as.character(""),
  Q13_date = as.Date(NA),
  Q13_length = as.numeric(NA),
  Q14 = as.character(""),
  Q14_1 = as.character(""),
  Q14_2 = as.character(""),
  Q14_3 = as.character(""),
  Q14_other = as.character("")
)


lst$systems <- data.frame(
  Q15 = as.character(""),
  Q15_other = as.character(""),
  Q15_EHRversion = as.character(""),
  Q16 = as.character(""),
  Q17 = as.character(""),
  Q17_1 = as.character(""),
  Q17_2 = as.character(""),
  Q17_3 = as.character(""),
  Q17_4 = as.character(""),
  Q17_5 = as.character(""),
  Q18 = as.character(""),
  Q19 = as.character(""),
  Q19_1 = as.character(""),
  Q19_2 = as.character(""),
  Q19_3 = as.character(""),
  Q19_4 = as.character(""),
  Q19_5 = as.character(""),
  Q20 = as.character(""),
  Q20_notes = as.character(""),
  Q20_orders = as.character(""),
  Q20_other = as.character(""),
  Q21 = as.character(""),
  Q21_text = as.character(""),
  Q22 = as.character(""),
  Q22_notes = as.character(""),
  Q23 = as.character(""),
  Q23_notes = as.character(""),
  Q24 = as.character(""),
  Q24_years = as.numeric(NA),
  Q25 = as.character(""),
  Q25_1 = as.character(""),
  Q25_2 = as.character(""),
  Q25_3 = as.character(""),
  Q25_4 = as.character(""),
  Q25_5 = as.character(""),
  Q25_other = as.character(""),
  Q26 = as.character(""),
  Q26_1 = as.character(""),
  Q26_2 = as.character(""),
  Q26_3 = as.character(""),
  Q26_4 = as.character(""),
  Q26_5 = as.character(""),
  Q26_6 = as.character(""),
  Q26_7 = as.character(""),
  Q26_8 = as.character(""),
  Q26_9 = as.character(""),
  Q26_other = as.character("")
)

lst$savedRates <- data.frame(
  Q27 = as.character(""),
  Q27_other = as.character(""),
  Q28 = as.character(""),
  Q28_1 = as.character(""),
  Q28_2 = as.character(""),
  Q28_3 = as.character(""),
  Q28_4 = as.character(""),
  Q28_other = as.character(""),
  Q29 = as.character(""),
  Q30 = as.character(""),
  Q31 = as.character(""),
  Q31_other = as.character(""),
  Q32 = as.character(""),
  Q32_details = as.character(""),
  Q32_details1 = as.character(""),
  Q32_details2 = as.character(""),
  Q32_details3 = as.character(""),
  Q32_details4 = as.character(""),
  Q32_other = as.character(""),
  Q33 = as.character(""),
  Q33_other = as.character(""),
  Q34 = as.character("")
)

lst$savedActivities <- data.frame(
  Q35 = as.character(""),
  Q35_1 = as.character(""),
  Q35_2 = as.character(""),
  Q35_3 = as.character(""),
  Q35_4 = as.character(""),
  Q35_other = as.character(""),
  Q36a = as.numeric(NA),
  Q36b = as.character(""),
  Q36c = as.character(""),
  Q36d = as.character(""),
  Q36e = as.character(""),
  Q36f = as.character(""),
  Q37 = as.character(""),
  Q37_1 = as.character(""),
  Q37_2 = as.character(""),
  Q37_3 = as.character(""),
  Q37_4 = as.character(""),
  Q38 = as.character(""),
  Q38_1 = as.character(""),
  Q38_2 = as.character(""),
  Q38_3 = as.character(""),
  Q38_4 = as.character(""),
  Q38_5 = as.character(""),
  Q38_6 = as.character(""),
  Q38_7 = as.character(""),
  Q38_8 = as.character(""),
  Q38_9 = as.character(""),
  Q38_other = as.character(""),
  Q39 = as.character(""),
  Q39_1 = as.character(""),
  Q39_2 = as.character(""),
  Q39_3 = as.character(""),
  Q39_4 = as.character(""),
  Q39_5 = as.character(""),
  Q39_other = as.character(""),
  Q40 = as.character(""),
  Q41 = as.character(""),
  Q42 = as.character(""),
  act1 = as.character(""),
  act2 = as.character(""),
  act3 = as.character(""),
  act4 = as.character(""),
  act5 = as.character(""),
  act6 = as.character(""),
  act7 = as.character(""),
  act8 = as.character(""),
  act9 = as.character(""),
  act10 = as.character(""),
  time1 = as.character(""),
  time2 = as.character(""),
  time3 = as.character(""),
  time4 = as.character(""),
  time5 = as.character(""),
  time6 = as.character(""),
  time7 = as.character(""),
  time8 = as.character(""),
  time9 = as.character(""),
  time10 = as.character(""),
  ppl1 = as.character(""),
  ppl2 = as.character(""),
  ppl3 = as.character(""),
  ppl4 = as.character(""),
  ppl5 = as.character(""),
  ppl6 = as.character(""),
  ppl7 = as.character(""),
  ppl8 = as.character(""),
  ppl9 = as.character(""),
  ppl10 = as.character("")
)

lst$followupRates <- data.frame(
  Q1FU = as.character(""),
  Q1FU_other = as.character(""),
  Q2FU = as.character(""),
  Q2FU_1 = as.character(""),
  Q2FU_2 = as.character(""),
  Q2FU_3 = as.character(""),
  Q2FU_4 = as.character(""),
  Q2FU_other = as.character(""),
  Q3FU = as.character(""),
  Q4FU = as.character("")
)

lst$additionalInfo <- data.frame(
  Q5FU = as.character(""),
  Q5FU_text = as.character(""),
  Q6FU = as.character(""),
  Q6FU_text = as.character(""),
  Q7FU = as.character(""),
  Q7FU_1 = as.character(""),
  Q7FU_2 = as.character(""),
  Q7FU_3 = as.character(""),
  Q7FU_4 = as.character(""),
  Q7FU_5 = as.character(""),
  Q7FU_other = as.character(""),
  Q8FU = as.character(""),
  Q8FU_1 = as.character(""),
  Q8FU_2 = as.character(""),
  Q8FU_3 = as.character(""),
  Q8FU_other = as.character(""),
  Q9FU = as.character(""),
  Q9FU_1 = as.character(""),
  Q9FU_2 = as.character(""),
  Q9FU_3 = as.character(""),
  Q9FU_4 = as.character(""),
  Q9FU_5 = as.character(""),
  Q9FU_6 = as.character(""),
  Q9FU_7 = as.character(""),
  Q9FU_8 = as.character(""),
  Q9FU_9 = as.character(""),
  Q9FU_other = as.character(""),
  Q10FU = as.character(NA),
  Q11FU = as.character(NA),
  Q12FU = as.character(NA),
  Q13FU = as.character(""),
  Q13FU_1 = as.character(""),
  Q13FU_2 = as.character(""),
  Q13FU_3 = as.character(""),
  Q13FU_4 = as.character(""),
  Q13FU_5 = as.character(""),
  Q13FU_6 = as.character(""),
  Q13FU_7 = as.character(""),
  Q13FU_8 = as.character(""),
  Q13FU_other = as.character(""),
  Q14FU = as.character(""),
  Q14FU_1 = as.character(""),
  Q14FU_2 = as.character(""),
  Q14FU_3 = as.character(""),
  Q14FU_4 = as.character(""),
  Q14FU_5 = as.character(""),
  Q14FU_other = as.character(""),
  Q15FU = as.character(""),
  Q16FU = as.character(""),
  Q17FU = as.character(""),
  Q18FU = as.character(""),
  Q18FU_more = as.character("")
)



# And the monthly updates -------------------------------------------------

myMonths <- c("January",
              "February",
              "March",
              "April",
              "May",
              "June",
              "July",
              "August",
              "September",
              "October",
              "November",
              "December")
# Three ages

# 9-10
f <- data.frame(Month = myMonths,
                Total = NA,
                Dose1 = NA,
                Dose2 = NA)
age1 <- Reduce(function(x,y) full_join(x,y,"Month"), list(f,f,f))
names(age1) <- c("Month", "TotalF", "Dose1F", "Dose2F", "TotalM", "Dose1M", "Dose2M", "Total", "Dose1", "Dose2")
rm(f)

# 11-12
f <- data.frame(Month = myMonths,
                Total = NA,
                Dose1 = NA,
                Dose2 = NA,
                Meningococcal = NA,
                TDap = NA)
age2 <- Reduce(function(x,y) full_join(x,y,"Month"), list(f,f,f))
names(age2) <- c("Month", "TotalF","Dose1F", "Dose2F", "MeningococcalF", "TDapF",
                 "TotalM","Dose1M", "Dose2M", "MeningococcalM", "TDapM",
                 "Total","Dose1", "Dose2", "Meningococcal", "TDap")
rm(f)

# 13
age3 <- age2


lst$age1 <- age1
lst$age2 <- age2
lst$age3 <- age3



myDB <- dbConnect(RSQLite::SQLite(), "nullDB.DB", overwrite = T)
lapply(names(lst), function(x) {
  dbWriteTable(myDB, x, lst[[x]], overwrite = T)
})

}



# Storage functions -------------------------------------------------------

# Storing data using shinyapps.io is a pain in the ass.
# shinyapps.io will not hold any permanent data files, so I have to store it on Azure
# Ideally we could use a single database file to store everything
# Ideally we would set up a MySQL server on Azure to remote connect between shinyapps.io
# But that is overkill for such a tiny project
#  Thus, every user (n ~ 100) will get a separate SQLite file stored on Azure
#  This file will be downloaded from Azure, updated and saved, then sent BACK to Azure
#  This is stupid inefficient
#  But, it is also idiot-proof safe.  Even if data gets lost, the most recently saved version
#  Is safe on Azure.
# We can't remote connect to SQLite files (must be read locally, just like RDS/Rdata, etc)
# So this was the best way to do it
# But it really sucks, nonetheless - it will be better when we have a local R server


# Save a data frame to azure

# dat = data name in the environment
# filename = name the file.RDS
# AzureDir = container/fileshare directory in Azure

# Save Azure -  It will save the local version of the file to Azure, then delete the local version so there is
# never more than one version of the database floating around
saveAzure <- function(loc, filename) {
  storage_upload(loc, filename)
  file.remove(filename)
}

#  To update a section of the database, I need to download the file, write the new table, and then save it back to azure
updateDB <- function(loc, filename, table, df){
  storage_download(loc, filename, overwrite = T)
  conn <- dbConnect(RSQLite::SQLite(), filename)
  dbWriteTable(conn, table, df, append = T)
  saveAzure(loc, filename)
}

# I have continuous updates in each of the databases in case our PIs want to go through earlier responses to check for
# Errors.
# But when I want to pull the data, I only want the most recent version
# Each version has a timestamp, this will just pull the latest time stamp
getRecentData <- function(loc, filename, table) {
  storage_download(loc, filename, overwrite = T)
  conn <- dbConnect(RSQLite::SQLite(), filename)
  df <- dbReadTable(conn, table)
  dateVar <- names(df)[2]
  df <- df[order(df[[dateVar]], decreasing = T),]
  return(df[1,])
  dbDisconnect(conn)
}

# This downloads the most recent data for each of the subsections and compiles them into a single dataset
getAllData <- function(loc, filename){
  storage_download(loc, filename, overwrite=T)
  conn <- dbConnect(RSQLite::SQLite(), filename)
  tables <- c("demographics", "systems", "savedRates", "savedActivities", "followupRates","additionalInfo")
  # Each of these tables has multiple versions, I just need the most recent update
  foo <- lapply(tables, function(x) {
    df <- dbReadTable(conn, x)
    dateVar <- names(df)[2]
    df <- df[order(df[[dateVar]], decreasing = T),]
    return(df[1,])
  }) %>%
    Reduce(function(x,y) full_join(x,y, "Username"), .)
  return(foo)
}

# This pulls together the most recent versions of the baseline data
# This function is redundant, I should have just used getAllData()
# But they were written at different times and I'm too lazy to change the whole app
getBaselineData <- function(loc, filename){
  storage_download(loc, filename, overwrite=T)
conn <- dbConnect(RSQLite::SQLite(), filename)
tables <- c("demographics", "systems", "savedRates", "savedActivities")
# Each of these tables has multiple versions, I just need the most recent update
foo <- lapply(tables, function(x) {
  df <- dbReadTable(conn, x)
  dateVar <- names(df)[2]
  df <- df[order(df[[dateVar]], decreasing = T),]
  return(df[1,])
}) %>%
  Reduce(function(x,y) full_join(x,y, "Username"), .)
return(foo)
}

# This pulls together the most recent versions of the followup data
# This function is redundant, I should have just used getAllData()
# But they were written at different times and I'm too lazy to change the whole app
getFollowupData <- function(loc, filename){
  storage_download(loc, filename, overwrite=T)
  conn <- dbConnect(RSQLite::SQLite(), filename)
  tables <- c("followupRates","additionalInfo")
  # Each of these tables has multiple versions, I just need the most recent update
  foo <- lapply(tables, function(x) {
    df <- dbReadTable(conn, x)
    dateVar <- names(df)[2]
    df <- df[order(df[[dateVar]], decreasing = T),]
    return(df[1,])
  }) %>%
    Reduce(function(x,y) full_join(x,y, "Username"), .)
  return(foo)
}

# This will compile all the study data for all health systems
# I'm a little ambivalent about including this, but they want people to be able to compare themselves to others
# I'll use this function in the monitoring apps... but I suppose I can do it here too.
# Like the other functions, it goes through each SQLite database and pulls the most recent
# entry
allStudyData <- function(loc) {
  
  files <- list_azure_files(loc) %>%
    filter(isdir == "FALSE") %>%
    filter(name != ".DB")
  
  mclapply(files$name, function(x) {
    storage_download(loc, x, overwrite = T)
  }, mc.cores = getOption("mc.cores", detectCores()))
  
  # Merge the data
  tbls <- c("demographics", "systems", "savedRates", "savedActivities", "followupRates", "additionalInfo")
  myData <- mclapply(files$name, function(x) {
    conn <- dbConnect(SQLite(), x) # connect to the database
    foo <- lapply(tbls, function(y) {
      df <- dbReadTable(conn, y) # read the table
      dateVar <- names(df)[2]
      # Get the most recent version
      df <- df[order(df[[dateVar]], decreasing = T),]
      df[[dateVar]] <- as.POSIXct(df[[dateVar]], origin = "1970-01-01")
      return(df[1,])
    }) %>%
      Reduce(function(x,y) full_join(x,y,"Username"), .)
  }, mc.cores = getOption("mc.cores", detectCores())) %>% 
    do.call("rbind",.)
  
  # Clean up shinyapps.io
  lapply(files$name, file.remove)
  rm(tbls)
  
  return(myData)
}



# Figures -----------------------------------------------------------------


# Notes on Figures - this is inefficient
# FUDisplay1 and FUDisplay2 are identical - except one uses the dose1 variables, the other dose2
# FUDisplay3 is the same as Figure1 - only followup vs baseline
# FUDisplay4 is the same as figure2 - only followup vs baseline




# Sum boys and girls rates to get total:
# Writing one for baseline and another for followup, because I'm lazy
# Otherwise they are identical
sumBaselineRates <- function(baselineRates){
  baselineRates$flagAge1 <- with(baselineRates, ifelse(
    is.na(BothAge1_total) & is.na(BothAge1_dose1) & is.na(BothAge1_dose2),1,0))

baselineRates$flagAge2 <- with(baselineRates, ifelse(
  is.na(BothAge2_total) &
    is.na(BothAge2_dose1) & is.na(BothAge1_dose2) &
    is.na(BothAge2_mening) &
    is.na(BothAge2_tdap), 1, 0))

baselineRates$flagAge3 <- with(baselineRates, ifelse(
  is.na(BothAge3_total) &
    is.na(BothAge3_dose1) & is.na(BothAge1_dose2) &
    is.na(BothAge3_mening) &
    is.na(BothAge3_tdap), 1, 0))
# If flagged, sum them
# These will remain NA if either Fem or Men are missing

# Age 1
baselineRates$BothAge1_total <-  with(baselineRates,
                                      ifelse(flagAge1 == 1, FemAge1_total + MenAge1_total, BothAge1_total))
baselineRates$BothAge1_dose1 <- with(baselineRates,
                                     ifelse(flagAge1 == 1, FemAge1_dose1 + MenAge1_dose1, BothAge1_dose1))
baselineRates$BothAge1_dose2 <- with(baselineRates,
                                     ifelse(flagAge1 == 1, FemAge1_dose2 + MenAge1_dose2, BothAge1_dose2))

# Age 2
baselineRates$BothAge2_total <-  with(baselineRates,
                                      ifelse(flagAge2 == 1, FemAge2_total + MenAge2_total, BothAge2_total))

baselineRates$BothAge2_dose1 <- with( baselineRates,
                                      ifelse(flagAge2 == 1, FemAge2_dose1 + MenAge2_dose1, BothAge2_dose1))

baselineRates$BothAge2_dose2 <-  with(baselineRates,
                                      ifelse(flagAge2 == 1, FemAge2_dose2 + MenAge2_dose2, BothAge2_dose2))

baselineRates$BothAge2_mening <- with(baselineRates,
                                      ifelse(flagAge2 == 1, FemAge2_mening + MenAge2_mening,  BothAge2_mening))

baselineRates$BothAge2_tdap <- with(baselineRates,
                                    ifelse(flagAge2 == 1, FemAge2_tdap + MenAge2_tdap, BothAge2_tdap))

# Age 3
baselineRates$BothAge3_total <-  with(baselineRates,
                                      ifelse(flagAge3 == 1, FemAge3_total + MenAge3_total, BothAge3_total))

baselineRates$BothAge3_dose1 <- with( baselineRates,
                                      ifelse(flagAge3 == 1, FemAge3_dose1 + MenAge3_dose1, BothAge3_dose1))

baselineRates$BothAge3_dose2 <-  with(baselineRates,
                                      ifelse(flagAge3 == 1, FemAge3_dose2 + MenAge3_dose2, BothAge3_dose2))

baselineRates$BothAge3_mening <- with(baselineRates,
                                      ifelse(flagAge3 == 1, FemAge3_mening + MenAge3_mening,  BothAge3_mening))

baselineRates$BothAge3_tdap <- with(baselineRates,
                                    ifelse(flagAge3 == 1, FemAge3_tdap + MenAge3_tdap, BothAge3_tdap))

return(baselineRates)
}



sumFURates <- function(rates) {
  
  rates$flagAge1 <- with(rates, ifelse(
    is.na(BothFUAge1_total) & is.na(BothFUAge1_dose1) & is.na(BothFUAge1_dose2),1,0))
  
  rates$flagAge2 <- with(rates, ifelse(
    is.na(BothFUAge2_total) &
      is.na(BothFUAge2_dose1) & is.na(BothFUAge1_dose2) &
      is.na(BothFUAge2_mening) &
      is.na(BothFUAge2_tdap), 1, 0))
  
  rates$flagAge3 <- with(rates, ifelse(
    is.na(BothFUAge3_total) &
      is.na(BothFUAge3_dose1) & is.na(BothFUAge1_dose2) &
      is.na(BothFUAge3_mening) &
      is.na(BothFUAge3_tdap), 1, 0))
  # If flagged, sum them
  # These will remain NA if either Fem or Men are missing
  
  # Age 1
  rates$BothFUAge1_total <-  with(rates,
                                  ifelse(flagAge1 == 1, FemFUAge1_total + MenFUAge1_total, BothFUAge1_total))
  rates$BothFUAge1_dose1 <- with(rates,
                                 ifelse(flagAge1 == 1, FemFUAge1_dose1 + MenFUAge1_dose1, BothFUAge1_dose1))
  rates$BothFUAge1_dose2 <- with(rates,
                                 ifelse(flagAge1 == 1, FemFUAge1_dose2 + MenFUAge1_dose2, BothFUAge1_dose2))
  
  # Age 2
  rates$BothFUAge2_total <-  with(rates,
                                  ifelse(flagAge2 == 1, FemFUAge2_total + MenFUAge2_total, BothFUAge2_total))
  
  rates$BothFUAge2_dose1 <- with( rates,
                                  ifelse(flagAge2 == 1, FemFUAge2_dose1 + MenFUAge2_dose1, BothFUAge2_dose1))
  
  rates$BothFUAge2_dose2 <-  with(rates,
                                  ifelse(flagAge2 == 1, FemFUAge2_dose2 + MenFUAge2_dose2, BothFUAge2_dose2))
  
  rates$BothFUAge2_mening <- with(rates,
                                  ifelse(flagAge2 == 1, FemFUAge2_mening + MenFUAge2_mening,  BothFUAge2_mening))
  
  rates$BothFUAge2_tdap <- with(rates,
                                ifelse(flagAge2 == 1, FemFUAge2_tdap + MenFUAge2_tdap, BothFUAge2_tdap))
  
  # Age 3
  rates$BothFUAge3_total <-  with(rates,
                                  ifelse(flagAge3 == 1, FemFUAge3_total + MenFUAge3_total, BothFUAge3_total))
  
  rates$BothFUAge3_dose1 <- with( rates,
                                  ifelse(flagAge3 == 1, FemFUAge3_dose1 + MenFUAge3_dose1, BothFUAge3_dose1))
  
  rates$BothFUAge3_dose2 <-  with(rates,
                                  ifelse(flagAge3 == 1, FemFUAge3_dose2 + MenFUAge3_dose2, BothFUAge3_dose2))
  
  rates$BothFUAge3_mening <- with(rates,
                                  ifelse(flagAge3 == 1, FemFUAge3_mening + MenFUAge3_mening,  BothFUAge3_mening))
  
  rates$BothFUAge3_tdap <- with(rates,
                                ifelse(flagAge3 == 1, FemFUAge3_tdap + MenFUAge3_tdap, BothFUAge3_tdap))
  
  return(rates)
}



# Figures -----------------------------------------------------------------

# This will pull the legend for grid.arrange() figures
getLegend <- function(myggplot) {
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}




# Baseline dashboard figures ----------------------------------------------

# Figure 1 - calculate rates for dose 1 and dose 2 - all ages 
#  Bar chart for girls, boys, combined
figure1 <- function(dat) {
  
  dat <- sumBaselineRates(dat)
  
  
  # Define rates - girls, boys, combined
  # Rates = doses / total
  totalFem <- with(dat,
                   sum(FemAge1_total, FemAge2_total, FemAge3_total, na.rm=T))
  dose1Fem <- with(dat,
                   sum(FemAge1_dose1, FemAge2_dose1, FemAge3_dose1, na.rm=T))
  dose2Fem <- with(dat,
                   sum(FemAge1_dose2, FemAge2_dose2, FemAge3_dose2, na.rm=T))
  
  
  totalMen <- with(dat,
                   sum(MenAge1_total, MenAge2_total, MenAge3_total, na.rm=T))
  dose1Men <- with(dat,
                   sum(MenAge1_dose1, MenAge2_dose1, MenAge3_dose1, na.rm=T))
  dose2Men <- with(dat,
                   sum(MenAge1_dose2, MenAge2_dose2, MenAge3_dose2, na.rm=T))
  
  totalBoth <- with(dat,
                    sum(BothAge1_total, BothAge2_total, BothAge3_total, na.rm=T))
  dose1Both <- with(dat,
                    sum(BothAge1_dose1, BothAge2_dose1, BothAge3_dose1, na.rm=T))
  dose2Both <- with(dat,
                    sum(BothAge1_dose2, BothAge2_dose2, BothAge3_dose2, na.rm=T))
  
  
  
  
  rates1Fem <- dose1Fem / totalFem
  rates2Fem <- dose2Fem / totalFem
  rates1Men <- dose1Men / totalMen
  rates2Men <- dose2Men / totalMen
  rates1Both <- dose1Both / totalBoth
  rates2Both <- dose2Both / totalBoth

  
  # Rates of HPV vaccination - all ages
  rates <- data.frame(rates = c(
    rates1Fem, rates1Men, rates1Both, 
    rates2Fem, rates2Men, rates2Both))
  
  rm(totalFem, dose1Fem, dose2Fem, totalMen, dose1Men, dose2Men,
     totalBoth, dose1Both, dose2Both,
     rates1Fem, rates1Men, rates1Both, 
     rates2Fem, rates2Men, rates2Both)
  
  rates$Gender <- c("Females", "Males", "Combined", "Females", "Males", "Combined")
  rates$Gender <- factor(rates$Gender, levels = c("Females", "Males", "Combined"))
  rates$Dose <- c(rep("1+ Dose HPV", 3), rep("HPV Complete", 3))
  rates$Dose <- factor(rates$Dose, levels = c("1+ Dose HPV", "HPV Complete"))
  
  # Dec 1st 2020 - they want the rates included as text on the figure
  # Need to format the rates first
  rates$rates_string <- (rates$rates*100) %>%
    round(1) %>%
    format(nsmall = 1) %>%
    paste0(., "%")


  g <- ggplot(rates, aes(x = Dose,
                         y = rates,
                         fill = Gender)) +
    geom_col(width = 0.5, position = position_dodge(0.7))  +
    geom_text(aes(label=rates_string), vjust=-0.5, size=3,
              position = position_dodge(0.7)) +
    scale_y_continuous(limits = seq(0,1),
                       breaks = seq(0,1,0.2),
                       labels = c("0", "20%", "40%", "60%", "80%", "100%")) +
    labs(title = "Baseline HPV Vaccination Rates by Sex (Age 9-13)",
         subtitle = dat[["HealthSystem"]]) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  rm(rates)
  return(g)
}


# Figure 2 - Age 13 ONLY male/female/combined rates for:
#   1+ doses
#   2 doses
#   Meningicoccal
#   Tdap

# December 1st 2020 - They also want age 11-12
# Let's see if I can roll the whole thing in one function

# age= age2 (11-12) or age3 (age 13)
figure2 <- function(dat, age){

  dat <- sumBaselineRates(dat)
  
  newNumbers <- c("Fem_total", "Fem_dose1", "Fem_dose2", "Fem_mening", "Fem_tdap",
                  "Men_total", "Men_dose1", "Men_dose2", "Men_mening", "Men_tdap",
                  "Both_total", "Both_dose1", "Both_dose2", "Both_mening", "Both_tdap")
  
  age2 <- c("FemAge2_total", "FemAge2_dose1", "FemAge2_dose2", "FemAge2_mening", "FemAge2_tdap",
            "MenAge2_total", "MenAge2_dose1", "MenAge2_dose2", "MenAge2_mening", "MenAge2_tdap",
            "BothAge2_total", "BothAge2_dose1", "BothAge2_dose2", "BothAge2_mening", "BothAge2_tdap")
  
  age3 <- c("FemAge3_total", "FemAge3_dose1", "FemAge3_dose2", "FemAge3_mening", "FemAge3_tdap",
            "MenAge3_total", "MenAge3_dose1", "MenAge3_dose2", "MenAge3_mening", "MenAge3_tdap",
            "BothAge3_total", "BothAge3_dose1", "BothAge3_dose2", "BothAge3_mening", "BothAge3_tdap")
  
  if (age == "age2") dat[,newNumbers] <- dat[,age2]
  if (age == "age3") dat[,newNumbers] <- dat[,age3]
  
  
  # girls 
  totalFem <- with(dat, sum(Fem_total,na.rm=T))
  dose1Fem <- with(dat, sum(Fem_dose1,na.rm=T))
  dose2Fem <- with(dat, sum(Fem_dose2,na.rm=T))
  meningFem <- with(dat, sum(Fem_mening,na.rm=T))
  tdapFem <-   with(dat, sum(Fem_tdap,na.rm=T))
  
  # boys
  totalMen <- with(dat, sum(Men_total,na.rm=T))
  dose1Men <- with(dat, sum(Men_dose1,na.rm=T))
  dose2Men <- with(dat, sum(Men_dose2,na.rm=T))
  meningMen <- with(dat,sum(Men_mening,na.rm=T))
  tdapMen <- with(dat, sum(Men_tdap,na.rm=T))
  
  
  totalBoth <- with(dat, sum(Both_total,na.rm=T))
  dose1Both <- with(dat, sum(Both_dose1,na.rm=T))
  dose2Both <- with(dat, sum(Both_dose2,na.rm=T))
  meningBoth <- with(dat, sum(Both_mening,na.rm=T))
  tdapBoth <- with(dat, sum(Both_tdap,na.rm=T))
  
  rates1Fem <- dose1Fem / totalFem
  rates2Fem <- dose2Fem / totalFem
  rates1Men <- dose1Men / totalMen
  rates2Men <- dose2Men / totalMen
  rates1Both <- dose1Both / totalBoth
  rates2Both <- dose2Both / totalBoth
  
  
  ratesFemMenin <- meningFem / totalFem
  ratesMenMenin <- meningMen / totalMen
  ratesBothMenin <- meningBoth / totalBoth
  
  ratesFemtdap <- tdapFem / totalFem
  ratesMentdap <- tdapMen / totalMen
  ratesBothtdap <- tdapBoth / totalBoth
  
  rates <- data.frame(rates = c(
    rates1Fem, rates1Men, rates1Both, 
    rates2Fem, rates2Men, rates2Both,
    ratesFemMenin, ratesMenMenin, ratesBothMenin,
    ratesFemtdap, ratesMentdap, ratesBothtdap))
  
  
  
  rates$Gender <- rep(c("Females", "Males", "Combined"), 4)
  rates$Gender <- factor(rates$Gender, levels = c("Females", "Males", "Combined"))
  rates$Dose <- c(rep("1+ Dose HPV", 3), rep("HPV Complete", 3),
                  rep("Mening", 3), rep("Tdap",3))
  rates$Dose <- factor(rates$Dose,
                       levels = c("1+ Dose HPV",
                                  "HPV Complete",
                                  "Mening",
                                  "Tdap") )       
  
  rates$rates_string <- (rates$rates*100) %>%
    round(1) %>%
    format(nsmall = 1) %>%
    paste0(., "%")

  
  title <- ifelse(age == "age2", "Baseline HPV Vaccination Rates by Sex (Ages 11-12)",
                                "Baseline HPV Vaccination Rates by Sex (Age 13)")
  
  g <- ggplot(rates, aes(x = Gender,
                         y = rates,
                         fill = Dose)) +
    geom_col(width = 0.5, position = position_dodge(0.7))  +
    geom_text(aes(label=rates_string), vjust=-0.5, size=3,
              position = position_dodge(0.7)) +
    scale_y_continuous(limits = seq(0,1),
                       breaks = seq(0,1,0.2),
                       labels = c("0", "20%", "40%", "60%", "80%", "100%")) +
    labs(title = title,
         subtitle = dat[["healthSystem"]]) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  rm(rates)
  return(g)
}


# Figure 3 - Age specific rates of Dose1 and Dose2
figure3 <- function(dat){

  dat <- sumBaselineRates(dat)
  
  
  rates1Fem <- with(dat,
                    sum(FemAge1_dose1,na.rm=T) / sum(FemAge1_total,na.rm=T))
  rates2Fem <- with(dat,
                    sum(FemAge2_dose1,na.rm=T) / sum(FemAge2_total,na.rm=T))
  rates3Fem <- with(dat,
                    sum(FemAge3_dose1,na.rm=T) / sum(FemAge3_total,na.rm=T))
  rates1Men <- with(dat,
                    sum(MenAge1_dose1,na.rm=T) / sum(MenAge1_total,na.rm=T))
  rates2Men <- with(dat,
                    sum(MenAge2_dose1,na.rm=T) / sum(MenAge2_total,na.rm=T))
  rates3Men <- with(dat,
                    sum(MenAge3_dose1,na.rm=T) / sum(MenAge3_total,na.rm=T))
  rates1Both <- with(dat,
                     sum(BothAge1_dose1,na.rm=T) / sum(BothAge1_total,na.rm=T))
  rates2Both <- with(dat,
                     sum(BothAge2_dose1,na.rm=T) / sum(BothAge2_total,na.rm=T))
  rates3Both <- with(dat,
                     sum(BothAge3_dose1,na.rm=T) / sum(BothAge3_total,na.rm=T))
  
  rates1 <- data.frame(rates = 
                         c(rates1Fem, rates2Fem, rates3Fem,
                           rates1Men, rates2Men, rates3Men,
                           rates1Both, rates2Both, rates3Both))
  rates1$Gender <- c(rep("Females",3),
                     rep("Males",3),
                     rep("Combined",3))
  rates1$Dose <- "1+ Dose HPV"
  rates1$Age <- rep(c("Age 9-10", "Age 11-12", "Age 13"),3)
  
  
  rm(rates1Fem, rates2Fem, rates3Fem,
     rates1Men, rates2Men, rates3Men,
     rates1Both, rates2Both, rates3Both) 
  
  
  rates1Fem <- with(dat,
                    sum(FemAge1_dose2,na.rm=T) / sum(FemAge1_total,na.rm=T))
  rates2Fem <- with(dat,
                    sum(FemAge2_dose2,na.rm=T) / sum(FemAge2_total,na.rm=T))
  rates3Fem <- with(dat,
                    sum(FemAge3_dose2,na.rm=T) / sum(FemAge3_total,na.rm=T))
  rates1Men <- with(dat,
                    sum(MenAge1_dose2,na.rm=T) / sum(MenAge1_total,na.rm=T))
  rates2Men <- with(dat,
                    sum(MenAge2_dose2,na.rm=T) / sum(MenAge2_total,na.rm=T))
  rates3Men <- with(dat,
                    sum(MenAge3_dose2,na.rm=T) / sum(MenAge3_total,na.rm=T))
  rates1Both <- with(dat,
                     sum(BothAge1_dose2,na.rm=T) / sum(BothAge1_total,na.rm=T))
  rates2Both <- with(dat,
                     sum(BothAge2_dose2,na.rm=T) / sum(BothAge2_total,na.rm=T))
  rates3Both <- with(dat,
                     sum(BothAge3_dose2,na.rm=T) / sum(BothAge3_total,na.rm=T))
  
  rates2 <- data.frame(rates = 
                         c(rates1Fem, rates2Fem, rates3Fem,
                           rates1Men, rates2Men, rates3Men,
                           rates1Both, rates2Both, rates3Both))
  rm(rates1Fem, rates2Fem, rates3Fem,
     rates1Men, rates2Men, rates3Men,
     rates1Both, rates2Both, rates3Both) 
  
  rates2$Gender <- c(rep("Females",3),
                     rep("Males",3),
                     rep("Combined",3))
  rates2$Dose <- "HPV Complete"
  rates2$Age <- rep(c("Age 9-10", "Age 11-12", "Age 13"),3)
  
  rates <- rbind(rates1,rates2)
  rm(rates1,rates2)
  
  rates$Gender <- factor(rates$Gender,
                         levels = c("Females", "Males", "Combined"))
  rates$Dose <- factor(rates$Dose,
                       levels = c("1+ Dose HPV",
                                  "HPV Complete"))
  rates$Age <- factor(rates$Age,
                      levels = c("Age 9-10", "Age 11-12", "Age 13"))
  
  rates$rates_string <- (rates$rates*100) %>%
    round(1) %>%
    format(nsmall = 1) %>%
    paste0(., "%")
 
    
    
  figures <- lapply(c("Females", "Males", "Combined"), function(x) {
    ggplot(filter(rates, Gender == x), aes(x = Dose,
                                           y = rates,
                                           fill = Age)) +
      geom_col(width = 0.5, position = position_dodge(0.7))  +
      geom_text(aes(label=rates_string), vjust=-0.5, size=2,
                position = position_dodge(0.7)) +
      scale_y_continuous(limits = seq(0,1),
                         breaks = seq(0,1,0.2),
                         labels = c("0", "20%", "40%", "60%", "80%", "100%")) +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  getLegend <- function(myggplot) {
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  
  
  # December 1st 2020 - They want to drop the males and females seperately
  #  I don't want to change the code, so I'm just going to grid.arrage only one graph
  #  It'll be easy to change back if they want it.
  
  plot(figures[[3]]) + labs (title = paste("Baseline HPV Vaccination Rates by Sex and Age Group",
                                           dat[["HealthSystem"]],
                                           sep = "\n"),
                            subtitle = "Males and Females Combined")
  
  
  #  Males, Females, Combined - old graph
  # grid.arrange(figures[[1]] + theme(legend.position = "none") + labs(title = "Females"),
  #              figures[[2]] + theme(legend.position = "none") + labs(title = "Males"),
  #              figures[[3]] + labs(title = "Males and Females Combined"),
  #              nrow = 2,
  #              layout_matrix= rbind(c(1,2),
  #                                   c(3,3)),
  #              top = paste("Baseline HPV Vaccination Rates by Sex and Age Group",
  #                          dat[["HealthSystem"]],
  #                          sep = "\n"))
}


# Figure 4 - compare the users HPV results to the rest of the study
figure4 <- function(dat, 
                    myData,
                    region = "All regions combined",
                    system = "All system types") {
  

  # Set my titles based on region and system
  title1 <- dat[["HealthSystem"]]
  title2 <- ifelse(region == "All regions combined" & system == "All system types",
                   "All regions and health system types combined", ifelse(
                     region == "All regions combined" & system != "All system types", 
                     paste(system, "systems across all regions combined"), ifelse(
                     region != "All regions combined" & system == "All system types",
                     paste("All system types combined for the",region, "region"), 
                     paste(system, "systems within the", region, "region"))))  

  
  west <- c("AK","OR","WA","NV","CA","HI","GU")
  north <- c("ID","MT","WY","UT","CO","ND","SD","NE","KS","MN","IA","MO","WI")
  south <- c("AZ","NM","TX","OK","AR","LA","MS","AL")
  northCentral <- c("MI","IL","IN","OH","KY","WV","TN")
  southEast <- c("PR","FL","GA","SC","NC","VA")
  northEast <- c("DC","MD","DE","NJ","PA","NY","RI","CT","MA","NH","VT","ME")
  

  
  myData$Region <- with(myData, ifelse(
        Q4b %in% west, "West", ifelse(
          Q4b %in% north, "North", ifelse(
            Q4b %in% south, "South", ifelse(
              Q4b %in% northCentral, "North Central", ifelse(
                Q4b %in% southEast, "Southeast", ifelse(
                  Q4b %in% northEast, "Northeast", NA)))))))

  lst <- list(all = myData, mysystem = dat)
  
  # Filter by region
  if (region != "All regions combined") {
    lst$all <- filter(lst$all, !is.na(Region) & Region == region)
  }
  
  # Filter by health system type
  if (system != "All system types") {
    lst$all <- filter(lst$all, !is.na(Q2) & Q2 == system)
  }
  
  lst <- lapply(lst, sumBaselineRates)
  
  # Prepare the data
  rates <- lapply(lst, function(dat) {
    
    rates1Fem <- with(dat,
                      sum(FemAge1_dose1,na.rm=T) / sum(FemAge1_total,na.rm=T))
    rates2Fem <- with(dat,
                      sum(FemAge2_dose1,na.rm=T) / sum(FemAge2_total,na.rm=T))
    rates3Fem <- with(dat,
                      sum(FemAge3_dose1,na.rm=T) / sum(FemAge3_total,na.rm=T))
    rates1Men <- with(dat,
                      sum(MenAge1_dose1,na.rm=T) / sum(MenAge1_total,na.rm=T))
    rates2Men <- with(dat,
                      sum(MenAge2_dose1,na.rm=T) / sum(MenAge2_total,na.rm=T))
    rates3Men <- with(dat,
                      sum(MenAge3_dose1,na.rm=T) / sum(MenAge3_total,na.rm=T))
    rates1Both <- with(dat,
                       sum(BothAge1_dose1,na.rm=T) / sum(BothAge1_total,na.rm=T))
    rates2Both <- with(dat,
                       sum(BothAge2_dose1,na.rm=T) / sum(BothAge2_total,na.rm=T))
    rates3Both <- with(dat,
                       sum(BothAge3_dose1,na.rm=T) / sum(BothAge3_total,na.rm=T))
    
    rates1 <- data.frame(rates = 
                           c(rates1Fem, rates2Fem, rates3Fem,
                             rates1Men, rates2Men, rates3Men,
                             rates1Both, rates2Both, rates3Both))
    rates1$Gender <- c(rep("Females",3),
                       rep("Males",3),
                       rep("Combined",3))
    rates1$Dose <- "1+ Dose HPV"
    rates1$Age <- rep(c("Age 9-10", "Age 11-12", "Age 13"),3)
    
    
    rm(rates1Fem, rates2Fem, rates3Fem,
       rates1Men, rates2Men, rates3Men,
       rates1Both, rates2Both, rates3Both) 
    
    
    rates1Fem <- with(dat,
                      sum(FemAge1_dose2,na.rm=T) / sum(FemAge1_total,na.rm=T))
    rates2Fem <- with(dat,
                      sum(FemAge2_dose2,na.rm=T) / sum(FemAge2_total,na.rm=T))
    rates3Fem <- with(dat,
                      sum(FemAge3_dose2,na.rm=T) / sum(FemAge3_total,na.rm=T))
    rates1Men <- with(dat,
                      sum(MenAge1_dose2,na.rm=T) / sum(MenAge1_total,na.rm=T))
    rates2Men <- with(dat,
                      sum(MenAge2_dose2,na.rm=T) / sum(MenAge2_total,na.rm=T))
    rates3Men <- with(dat,
                      sum(MenAge3_dose2,na.rm=T) / sum(MenAge3_total,na.rm=T))
    rates1Both <- with(dat,
                       sum(BothAge1_dose2,na.rm=T) / sum(BothAge1_total,na.rm=T))
    rates2Both <- with(dat,
                       sum(BothAge2_dose2,na.rm=T) / sum(BothAge2_total,na.rm=T))
    rates3Both <- with(dat,
                       sum(BothAge3_dose2,na.rm=T) / sum(BothAge3_total,na.rm=T))
    
    rates2 <- data.frame(rates = 
                           c(rates1Fem, rates2Fem, rates3Fem,
                             rates1Men, rates2Men, rates3Men,
                             rates1Both, rates2Both, rates3Both))
    rm(rates1Fem, rates2Fem, rates3Fem,
       rates1Men, rates2Men, rates3Men,
       rates1Both, rates2Both, rates3Both) 
    
    rates2$Gender <- c(rep("Females",3),
                       rep("Males",3),
                       rep("Combined",3))
    rates2$Dose <- "HPV Complete"
    rates2$Age <- rep(c("Age 9-10", "Age 11-12", "Age 13"),3)
    
    rates <- rbind(rates1,rates2)
    rm(rates1,rates2)
    
    rates$Gender <- factor(rates$Gender,
                           levels = c("Females", "Males", "Combined"))
    rates$Dose <- factor(rates$Dose,
                         levels = c("1+ Dose HPV",
                                    "HPV Complete"))
    rates$Age <- factor(rates$Age,
                        levels = c("Age 9-10", "Age 11-12", "Age 13"))
    
    rates$rates_string <- (rates$rates*100) %>%
      round(1) %>%
      format(nsmall = 1) %>%
      paste0(., "%")
    
    return(rates)
  })
  
  figures <- lapply(rates, function(x) {
     ggplot(filter(x, Gender == "Combined"), aes(x = Dose,
                                                y = rates,
                                                fill = Age)) +
      geom_col(width = 0.5, position = position_dodge(0.7))  +
      geom_text(aes(label=rates_string), vjust=-0.5, size=2,
                position = position_dodge(0.7)) +
      scale_y_continuous(limits = seq(0,1),
                         breaks = seq(0,1,0.2),
                         labels = c("0", "20%", "40%", "60%", "80%", "100%")) +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  grid.arrange(arrangeGrob(
    figures[[2]] + theme(legend.position = "none") + labs(title = title1),
    figures[[1]] + theme(legend.position = "none") + labs(title = title2),
    nrow = 1),
    getLegend(figures[[1]]), 
    nrow=2, heights = c(10,1))
  
}




# Followup dashboard ------------------------------------------------------


#  Data one - 1 HPV dose - change in time   -------------------------------
fuDisplay1 <- function(dat, plots = 0) {

  dat <- sumBaselineRates(dat) %>%
         sumFURates()
  
  
  
  # Get baseline and updated rates - Dose1
  
  # Girls first
  totalGirls1 <- with(dat, sum(FemAge1_total, na.rm=T))
  totalGirls2 <- with(dat, sum(FemAge2_total, na.rm=T))
  totalGirls3 <- with(dat, sum(FemAge3_total, na.rm=T))
  totalGirls <-  sum(totalGirls1, totalGirls2, totalGirls3)
  
  totalGirls1FU <- with(dat, sum(FemFUAge1_total, na.rm=T))
  totalGirls2FU <- with(dat, sum(FemFUAge2_total, na.rm=T))
  totalGirls3FU <- with(dat, sum(FemFUAge3_total, na.rm=T))
  totalGirlsFU <- sum(totalGirls1FU, totalGirls2FU, totalGirls3FU)
  
  girlsDose1 <- with(dat, sum(FemAge1_dose1, na.rm=T))
  girlsDose2 <- with(dat, sum(FemAge2_dose1, na.rm=T))
  girlsDose3 <- with(dat, sum(FemAge3_dose1, na.rm=T))
  totalDose <- sum(girlsDose1, girlsDose2, girlsDose3)
  
  girlsDose1FU <- with(dat, sum(FemFUAge1_dose1, na.rm=T))
  girlsDose2FU <- with(dat, sum(FemFUAge2_dose1, na.rm=T))
  girlsDose3FU <- with(dat, sum(FemFUAge3_dose1, na.rm=T))
  totalDoseFU <- sum(girlsDose1FU, girlsDose2FU, girlsDose3FU)
  
  rate1 <- girlsDose1 / totalGirls1
  rate2 <- girlsDose2 / totalGirls2
  rate3 <- girlsDose3 / totalGirls3
  rateTot <- totalDose / totalGirls
  
  rate1FU <- girlsDose1FU / totalGirls1FU
  rate2FU <- girlsDose2FU / totalGirls2FU
  rate3FU <- girlsDose3FU / totalGirls3FU
  rateTotFU <- totalDoseFU / totalGirlsFU
  
  # Make the data table
  Girls <- data.frame(AgeGroup = 
                        c("Age 9-10",
                          "Age 11-12",
                          "Age 13",
                          "All ages"),
                      Baseline = 
                        c(rate1, rate2, rate3, rateTot),
                      Updates = 
                        c(rate1FU, rate2FU, rate3FU, rateTotFU))
  
  
  rm(rate1, rate2, rate3, rateTot,
     rate1FU, rate2FU, rate3FU, rateTotFU,
     girlsDose1, totalGirls1,
     girlsDose2, totalGirls2,
     girlsDose3, totalGirls3,
     totalDose, totalGirls,
     girlsDose1FU, totalGirls1FU,
     girlsDose2FU, totalGirls2FU,
     girlsDose3FU, totalGirls3FU,
     totalDoseFU, totalGirlsFU)
  
  
  # Boys first
  totalBoys1 <- with(dat,  sum(MenAge1_total, na.rm=T))
  totalBoys2 <- with(dat, sum(MenAge2_total, na.rm=T))
  totalBoys3 <- with(dat,  sum(MenAge3_total, na.rm=T))
  totalBoys <- sum(totalBoys1, totalBoys2, totalBoys3)
  
  totalBoys1FU <- with(dat, sum(MenFUAge1_total, na.rm=T))
  totalBoys2FU <- with(dat, sum(MenFUAge2_total, na.rm=T))
  totalBoys3FU <- with(dat, sum(MenFUAge3_total, na.rm=T))
  totalBoysFU <- sum(totalBoys1FU, totalBoys2FU, totalBoys3FU)
  
  BoysDose1 <- with(dat, sum(MenAge1_dose1, na.rm=T))
  BoysDose2 <- with(dat, sum(MenAge2_dose1, na.rm=T))
  BoysDose3 <- with(dat, sum(MenAge3_dose1, na.rm=T))
  totalDose <- sum(BoysDose1, BoysDose2, BoysDose3)
  
  BoysDose1FU <- with(dat, sum(MenFUAge1_dose1, na.rm=T))
  BoysDose2FU <- with(dat, sum(MenFUAge2_dose1, na.rm=T))
  BoysDose3FU <- with(dat, sum(MenFUAge3_dose1, na.rm=T))
  totalDoseFU <- sum(BoysDose1FU, BoysDose2FU, BoysDose3FU)
  
  rate1 <- BoysDose1 / totalBoys1
  rate2 <- BoysDose2 / totalBoys2
  rate3 <- BoysDose3 / totalBoys3
  rateTot <- totalDose / totalBoys
  
  rate1FU <- BoysDose1FU / totalBoys1FU
  rate2FU <- BoysDose2FU / totalBoys2FU
  rate3FU <- BoysDose3FU / totalBoys3FU
  rateTotFU <- totalDoseFU / totalBoysFU
  
  # Make the data table
  Boys <- data.frame(AgeGroup = 
                       c("Age 9-10",
                         "Age 11-12",
                         "Age 13",
                         "All ages"),
                     Baseline = 
                       c(rate1, rate2, rate3, rateTot),
                     Updates = 
                       c(rate1FU, rate2FU, rate3FU, rateTotFU))
  rm(rate1, rate2, rate3, rateTot,
     rate1FU, rate2FU, rate3FU, rateTotFU,
     BoysDose1, totalBoys1,
     BoysDose2, totalBoys2,
     BoysDose3, totalBoys3,
     totalDose, totalBoys,
     BoysDose1FU, totalBoys1FU,
     BoysDose2FU, totalBoys2FU,
     BoysDose3FU, totalBoys3FU,
     totalDoseFU, totalBoysFU)
  
  
  # Both first
  totalBoth1 <- with(dat, sum(BothAge1_total, na.rm=T))
  totalBoth2 <- with(dat, sum(BothAge2_total, na.rm=T))
  totalBoth3 <- with(dat, sum(BothAge3_total, na.rm=T))
  totalBoth <- sum(totalBoth1, totalBoth2, totalBoth3)
  
  totalBoth1FU <- with(dat, sum(BothFUAge1_total, na.rm=T))
  totalBoth2FU <- with(dat, sum(BothFUAge2_total, na.rm=T))
  totalBoth3FU <- with(dat, sum(BothFUAge3_total, na.rm=T))
  totalBothFU <- sum(totalBoth1FU, totalBoth2FU, totalBoth3FU)
  
  BothDose1 <- with(dat, sum(BothAge1_dose1, na.rm=T))
  BothDose2 <- with(dat, sum(BothAge2_dose1, na.rm=T))
  BothDose3 <- with(dat, sum(BothAge3_dose1, na.rm=T))
  totalDose <- sum(BothDose1, BothDose2, BothDose3)
  
  BothDose1FU <- with(dat, sum(BothFUAge1_dose1, na.rm=T))
  BothDose2FU <- with(dat, sum(BothFUAge2_dose1, na.rm=T))
  BothDose3FU <- with(dat, sum(BothFUAge3_dose1, na.rm=T))
  totalDoseFU <- sum(BothDose1FU, BothDose2FU, BothDose3FU)
  
  rate1 <- BothDose1 / totalBoth1
  rate2 <- BothDose2 / totalBoth2
  rate3 <- BothDose3 / totalBoth3
  rateTot <- totalDose / totalBoth
  
  rate1FU <- BothDose1FU / totalBoth1FU
  rate2FU <- BothDose2FU / totalBoth2FU
  rate3FU <- BothDose3FU / totalBoth3FU
  rateTotFU <- totalDoseFU / totalBothFU
  
  # Make the data table
  Both <- data.frame(AgeGroup = 
                       c("Age 9-10",
                         "Age 11-12",
                         "Age 13",
                         "All ages"),
                     Baseline = 
                       c(rate1, rate2, rate3, rateTot),
                     Updates = 
                       c(rate1FU, rate2FU, rate3FU, rateTotFU))
  rm(rate1, rate2, rate3, rateTot,
     rate1FU, rate2FU, rate3FU, rateTotFU,
     BothDose1, totalBoth1,
     BothDose2, totalBoth2,
     BothDose3, totalBoth3,
     totalDose, totalBoth,
     BothDose1FU, totalBoth1FU,
     BothDose2FU, totalBoth2FU,
     BothDose3FU, totalBoth3FU,
     totalDoseFU, totalBothFU)
  
  
  rates <- data.frame(rbind(
    data.frame(Gender = c("Females", rep(NA, nrow(Girls))),
               rbind(NA, Girls)),
    data.frame(Gender = c("Males", rep(NA, nrow(Boys))),
               rbind(NA, Boys)),
    data.frame(Gender = c("Combined", rep(NA, nrow(Both))),
               rbind(NA, Both))
  ))  
  rates$RateChange <- rates$Updates - rates$Baseline
  rates$RateChange <- with(rates, ifelse(
    is.na(RateChange), NA, format(round((RateChange*100),2), nsmall = 2)))
  rates$RateChange <- with(rates, ifelse(
    is.na(RateChange), NA, paste0(RateChange,"%")))
  rates$RateChange <- sub(" ","",rates$RateChange)
  
  rates[,c("Baseline2", "Updates2")] <- lapply(rates[,c("Baseline","Updates")], function(x) {
    f <- ifelse(is.na(x), NA, format(round((x*100),2), nsmall = 2))
    f <- ifelse(is.na(x), NA, paste0(f,"%"))
    f <- sub(" ","",f)
    return(f)
  })
  
  # Return the table
  if (plots == 0) {
  return(dplyr::select(rates, Gender, AgeGroup, Baseline=Baseline2, Updates=Updates2, RateChange))
  }
  if (plots == 1) { 
  # Get the graphs
  reshaped <- lapply(list(Girls,Boys,Both), function(x) {
    AgeGroup <- x$AgeGroup
    Baseline <- x$Baseline
    Updates <- x$Updates
    df <- data.frame(AgeGroup = c(AgeGroup,AgeGroup),
                     Rates = c(Baseline,Updates),
                     Time = c(rep("Baseline", 4), rep("Updates", 4)))
    df$AgeGroup <- factor(df$AgeGroup,
                          levels = AgeGroup)
    df$Time <- factor(df$Time, c("Baseline", "Updates"))
    df$AgeGroup <- factor(df$AgeGroup, levels = c(
      "Age 9-10",
      "Age 11-12",
      "Age 13",
      "All ages"
    ))
    df$Rates[is.na(df$Rates)] <- 0
    return(df)
  })
  names(reshaped) <- c("Girls", "Boys", "Both")
  
  quickFun <- function(sex, title){
    g <- ggplot(sex, aes(x = Time, y=Rates, group = AgeGroup, color = AgeGroup)) +
      geom_line(size = 1.25) +
      geom_point(size = 2) +
      scale_y_continuous(limits = seq(0,1),
                         breaks = seq(0,1,0.2),
                         labels = c("0", "20%", "40%", "60%", "80%", "100%")) +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            axis.title = element_blank(),
            plot.subtitle = element_text(hjust = 0.5))
    return(g)
  }
  
  girlsGG <- quickFun(reshaped$Girls, "Females Aged 9-13")
  boysGG <- quickFun(reshaped$Boys, "Males Aged 9-13")
  bothGG <- quickFun(reshaped$Both, "Males and Females Aged 9-13")
  
  leg <- getLegend(girlsGG)
  
  

    return(
  grid.arrange(girlsGG + theme(legend.position = "none") + labs(subtitle = "Females"),
                    boysGG + theme(legend.position = "none") + labs(subtitle = "Males"),
                    bothGG + labs(subtitle = "Males and Females Combined"),
                    nrow = 2,
                    layout_matrix= rbind(c(1,2),
                                         c(3,3)),
                    top = dat[["HeatlthSystem"]],
                     heights = c(2,3))
    )} 
  
}

#  Data one - Complete HPV dose - change in time   -------------------------------
fuDisplay2 <- function(dat, plots = 0) {
  dat <- sumBaselineRates(dat) %>%
    sumFURates()
  
  
  # Get baseline and updated rates - Dose1
  
  # Girls first
  totalGirls1 <- with(dat, sum(FemAge1_total, na.rm=T))
  totalGirls2 <- with(dat, sum(FemAge2_total, na.rm=T))
  totalGirls3 <- with(dat,  sum(FemAge3_total, na.rm=T))
  totalGirls <- sum(totalGirls1, totalGirls2, totalGirls3)
  
  totalGirls1FU <- with(dat, sum(FemFUAge1_total, na.rm=T))
  totalGirls2FU <- with(dat, sum(FemFUAge2_total, na.rm=T))
  totalGirls3FU <- with(dat, sum(FemFUAge3_total, na.rm=T))
  totalGirlsFU <- sum(totalGirls1FU, totalGirls2FU, totalGirls3FU)
  
  girlsDose1 <- with(dat,   sum(FemAge1_dose2, na.rm=T))
  girlsDose2 <- with(dat, sum(FemAge2_dose2, na.rm=T))
  girlsDose3 <- with(dat,  sum(FemAge3_dose2, na.rm=T))
  totalDose <- sum(girlsDose1, girlsDose2, girlsDose3)
  
  girlsDose1FU <- with(dat, sum(FemFUAge1_dose2, na.rm=T))
  girlsDose2FU <- with(dat, sum(FemFUAge2_dose2, na.rm=T))
  girlsDose3FU <- with(dat,  sum(FemFUAge3_dose2, na.rm=T))
  totalDoseFU <- sum(girlsDose1FU, girlsDose2FU, girlsDose3FU)
  
  rate1 <- girlsDose1 / totalGirls1
  rate2 <- girlsDose2 / totalGirls2
  rate3 <- girlsDose3 / totalGirls3
  rateTot <- totalDose / totalGirls
  
  rate1FU <- girlsDose1FU / totalGirls1FU
  rate2FU <- girlsDose2FU / totalGirls2FU
  rate3FU <- girlsDose3FU / totalGirls3FU
  rateTotFU <- totalDoseFU / totalGirlsFU
  
  # Make the data table
  Girls <- data.frame(AgeGroup = 
                        c("Age 9-10",
                          "Age 11-12",
                          "Age 13",
                          "All ages"),
                      Baseline = 
                        c(rate1, rate2, rate3, rateTot),
                      Updates = 
                        c(rate1FU, rate2FU, rate3FU, rateTotFU))
  
  rm(rate1, rate2, rate3, rateTot,
     rate1FU, rate2FU, rate3FU, rateTotFU,
     girlsDose1, totalGirls1,
     girlsDose2, totalGirls2,
     girlsDose3, totalGirls3,
     totalDose, totalGirls,
     girlsDose1FU, totalGirls1FU,
     girlsDose2FU, totalGirls2FU,
     girlsDose3FU, totalGirls3FU,
     totalDoseFU, totalGirlsFU)
  
  
  # Boys first
  totalBoys1 <- with(dat, sum(MenAge1_total, na.rm=T))
  totalBoys2 <- with(dat,  sum(MenAge2_total, na.rm=T))
  totalBoys3 <- with(dat, sum(MenAge3_total, na.rm=T))
  totalBoys <- sum(totalBoys1, totalBoys2, totalBoys3)
  
  totalBoys1FU <- with(dat, sum(MenFUAge1_total, na.rm=T))
  totalBoys2FU <- with(dat,  sum(MenFUAge2_total, na.rm=T))
  totalBoys3FU <- with(dat,  sum(MenFUAge3_total, na.rm=T))
  totalBoysFU <- sum(totalBoys1FU, totalBoys2FU, totalBoys3FU)
  
  BoysDose1 <- with(dat,  sum(MenAge1_dose2, na.rm=T))
  BoysDose2 <- with(dat, sum(MenAge2_dose2, na.rm=T))
  BoysDose3 <- with(dat, sum(MenAge3_dose2, na.rm=T))
  totalDose <- sum(BoysDose1, BoysDose2, BoysDose3)
  
  BoysDose1FU <- with(dat, sum(MenFUAge1_dose2, na.rm=T))
  BoysDose2FU <- with(dat, sum(MenFUAge2_dose2, na.rm=T))
  BoysDose3FU <- with(dat,  sum(MenFUAge3_dose2, na.rm=T))
  totalDoseFU <- sum(BoysDose1FU, BoysDose2FU, BoysDose3FU)
  
  rate1 <- BoysDose1 / totalBoys1
  rate2 <- BoysDose2 / totalBoys2
  rate3 <- BoysDose3 / totalBoys3
  rateTot <- totalDose / totalBoys
  
  rate1FU <- BoysDose1FU / totalBoys1FU
  rate2FU <- BoysDose2FU / totalBoys2FU
  rate3FU <- BoysDose3FU / totalBoys3FU
  rateTotFU <- totalDoseFU / totalBoysFU
  
  # Make the data table
  Boys <- data.frame(AgeGroup = 
                       c("Age 9-10",
                         "Age 11-12",
                         "Age 13",
                         "All ages"),
                     Baseline = 
                       c(rate1, rate2, rate3, rateTot),
                     Updates = 
                       c(rate1FU, rate2FU, rate3FU, rateTotFU))
  rm(rate1, rate2, rate3, rateTot,
     rate1FU, rate2FU, rate3FU, rateTotFU,
     BoysDose1, totalBoys1,
     BoysDose2, totalBoys2,
     BoysDose3, totalBoys3,
     totalDose, totalBoys,
     BoysDose1FU, totalBoys1FU,
     BoysDose2FU, totalBoys2FU,
     BoysDose3FU, totalBoys3FU,
     totalDoseFU, totalBoysFU)
  
  
  # Both first
  totalBoth1 <- with(dat,
                     sum(BothAge1_total, na.rm=T))
  totalBoth2 <- with(dat,
                     sum(BothAge2_total, na.rm=T))
  totalBoth3 <- with(dat,
                     sum(BothAge3_total, na.rm=T))
  totalBoth <- sum(totalBoth1, totalBoth2, totalBoth3)
  
  totalBoth1FU <- with(dat,
                       sum(BothFUAge1_total, na.rm=T))
  totalBoth2FU <- with(dat,
                       sum(BothFUAge2_total, na.rm=T))
  totalBoth3FU <- with(dat,
                       sum(BothFUAge3_total, na.rm=T))
  totalBothFU <- sum(totalBoth1FU, totalBoth2FU, totalBoth3FU)
  
  BothDose1 <- with(dat,
                    sum(BothAge1_dose2, na.rm=T))
  BothDose2 <- with(dat,
                    sum(BothAge2_dose2, na.rm=T))
  BothDose3 <- with(dat,
                    sum(BothAge3_dose2, na.rm=T))
  totalDose <- sum(BothDose1, BothDose2, BothDose3)
  
  BothDose1FU <- with(dat,
                      sum(BothFUAge1_dose2, na.rm=T))
  BothDose2FU <- with(dat,
                      sum(BothFUAge2_dose2, na.rm=T))
  BothDose3FU <- with(dat,
                      sum(BothFUAge3_dose2, na.rm=T))
  totalDoseFU <- sum(BothDose1FU, BothDose2FU, BothDose3FU)
  
  rate1 <- BothDose1 / totalBoth1
  rate2 <- BothDose2 / totalBoth2
  rate3 <- BothDose3 / totalBoth3
  rateTot <- totalDose / totalBoth
  
  rate1FU <- BothDose1FU / totalBoth1FU
  rate2FU <- BothDose2FU / totalBoth2FU
  rate3FU <- BothDose3FU / totalBoth3FU
  rateTotFU <- totalDoseFU / totalBothFU
  
  # Make the data table
  Both <- data.frame(AgeGroup = 
                       c("Age 9-10",
                         "Age 11-12",
                         "Age 13",
                         "All ages"),
                     Baseline = 
                       c(rate1, rate2, rate3, rateTot),
                     Updates = 
                       c(rate1FU, rate2FU, rate3FU, rateTotFU))
  rm(rate1, rate2, rate3, rateTot,
     rate1FU, rate2FU, rate3FU, rateTotFU,
     BothDose1, totalBoth1,
     BothDose2, totalBoth2,
     BothDose3, totalBoth3,
     totalDose, totalBoth,
     BothDose1FU, totalBoth1FU,
     BothDose2FU, totalBoth2FU,
     BothDose3FU, totalBoth3FU,
     totalDoseFU, totalBothFU)
  
  
  rates <- data.frame(rbind(
    data.frame(Gender = c("Females", rep(NA, nrow(Girls))),
               rbind(NA, Girls)),
    data.frame(Gender = c("Males", rep(NA, nrow(Boys))),
               rbind(NA, Boys)),
    data.frame(Gender = c("Combined", rep(NA, nrow(Both))),
               rbind(NA, Both))
  ))  
  rates$RateChange <- rates$Updates - rates$Baseline
  rates$RateChange <- with(rates, ifelse(
    is.na(RateChange), NA, format(round((RateChange*100),2), nsmall = 2)))
  rates$RateChange <- with(rates, ifelse(
    is.na(RateChange), NA, paste0(RateChange,"%")))
  rates$RateChange <- sub(" ","",rates$RateChange)
  
  rates[,c("Baseline2", "Updates2")] <- lapply(rates[,c("Baseline","Updates")], function(x) {
    f <- ifelse(is.na(x), NA, format(round((x*100),2), nsmall = 2))
    f <- ifelse(is.na(x), NA, paste0(f,"%"))
    f <- sub(" ","",f)
    return(f)
  })
  
  # Return the table
  if (plots == 0) {
    return(dplyr::select(rates, Gender, AgeGroup, Baseline=Baseline2, Updates=Updates2, RateChange))
  }
  if (plots == 1) { 
    # Get the graphs
    reshaped <- lapply(list(Girls,Boys,Both), function(x) {
      AgeGroup <- x$AgeGroup
      Baseline <- x$Baseline
      Updates <- x$Updates
      dat <- data.frame(AgeGroup = c(AgeGroup,AgeGroup),
                       Rates = c(Baseline,Updates),
                       Time = c(rep("Baseline", 4), rep("Updates", 4)))
      dat$AgeGroup <- factor(dat$AgeGroup,
                            levels = AgeGroup)
      dat$Time <- factor(dat$Time, c("Baseline", "Updates"))
      return(dat)
    })
    names(reshaped) <- c("Girls", "Boys", "Both")
    
    quickFun <- function(dat, title){
      g <- ggplot(dat, aes(x = Time, y=Rates, group = AgeGroup, color = AgeGroup)) +
        geom_line(size = 1.25) +
        geom_point(size = 2) +
        scale_y_continuous(limits = seq(0,1),
                           breaks = seq(0,1,0.2),
                           labels = c("0", "20%", "40%", "60%", "80%", "100%")) +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              axis.title = element_blank(),
              plot.subtitle = element_text(hjust = 0.5))
      return(g)
    }
    
    girlsGG <- quickFun(reshaped$Girls, "Females Aged 9-13")
    boysGG <- quickFun(reshaped$Boys, "Males Aged 9-13")
    bothGG <- quickFun(reshaped$Both, "Males and Females Aged 9-13")
    
    leg <- getLegend(girlsGG)
    
    
    
    return(
      grid.arrange(girlsGG + theme(legend.position = "none") + labs(subtitle = "Females"),
                   boysGG + theme(legend.position = "none") + labs(subtitle = "Males"),
                   bothGG + labs(subtitle = "Males and Females Combined"),
                   nrow = 2,
                   layout_matrix= rbind(c(1,2),
                                        c(3,3)),
                   top = dat[["HealthSystem"]],
                   heights = c(2,3))
    )} 
  
}


#  Final rates by sex - age 9-13  -------------------------------
fuDisplay3 <- function(dat, healthSystem){

  dat <- sumFURates(dat)

  
  
  
  # Define rates - girls, boys, combined
  # Rates = doses / total
  totalFem <- with(dat,
                   sum(FemFUAge1_total, FemFUAge2_total, FemFUAge3_total, na.rm=T))
  dose1Fem <- with(dat,
                   sum(FemFUAge1_dose1, FemFUAge2_dose1, FemFUAge3_dose1, na.rm=T))
  dose2Fem <- with(dat,
                   sum(FemFUAge1_dose2, FemFUAge2_dose2, FemFUAge3_dose2, na.rm=T))
  
  
  totalMen <- with(dat,
                   sum(MenFUAge1_total, MenFUAge2_total, MenFUAge3_total, na.rm=T))
  dose1Men <- with(dat,
                   sum(MenFUAge1_dose1, MenFUAge2_dose1, MenFUAge3_dose1, na.rm=T))
  dose2Men <- with(dat,
                   sum(MenFUAge1_dose2, MenFUAge2_dose2, MenFUAge3_dose2, na.rm=T))
  
  totalBoth <- with(dat,
                    sum(BothFUAge1_total, BothFUAge2_total, BothFUAge3_total, na.rm=T))
  dose1Both <- with(dat,
                    sum(BothFUAge1_dose1, BothFUAge2_dose1, BothFUAge3_dose1, na.rm=T))
  dose2Both <- with(dat,
                    sum(BothFUAge1_dose2, BothFUAge2_dose2, BothFUAge3_dose2, na.rm=T))
  
  rates1Fem <- dose1Fem / totalFem
  rates2Fem <- dose2Fem / totalFem
  rates1Men <- dose1Men / totalMen
  rates2Men <- dose2Men / totalMen
  rates1Both <- dose1Both / totalBoth
  rates2Both <- dose2Both / totalBoth

  # Rates of HPV vaccination - all ages
  rates <- data.frame(rates = c(
    rates1Fem, rates1Men, rates1Both, 
    rates2Fem, rates2Men, rates2Both))
  
  rm(totalFem, dose1Fem, dose2Fem, totalMen, dose1Men, dose2Men,
     totalBoth, dose1Both, dose2Both,
     rates1Fem, rates1Men, rates1Both, 
     rates2Fem, rates2Men, rates2Both)
  
  rates$Gender <- c("Females", "Males", "Combined", "Females", "Males", "Combined")
  rates$Gender <- factor(rates$Gender, levels = c("Females", "Males", "Combined"))
  rates$Dose <- c(rep("1+ Dose HPV", 3), rep("HPV Complete", 3))
  
  rates$rates_string <- (rates$rates*100) %>%
    round(1) %>%
    format(nsmall = 1) %>%
    paste0(., "%")
  
  rates$rates_string <- (rates$rates*100) %>%
    round(1) %>%
    format(nsmall = 1) %>%
    paste0(., "%")
  
  g <- ggplot(rates, aes(x = Dose,
                         y = rates,
                         fill = Gender)) +
    geom_col(width = 0.5, position = position_dodge(0.7))  +
    geom_text(aes(label=rates_string), vjust=-0.5, size=3,
              position = position_dodge(0.7)) +
    scale_y_continuous(limits = seq(0,1),
                       breaks = seq(0,1,0.2),
                       labels = c("0", "20%", "40%", "60%", "80%", "100%")) +
    labs(title = "Final HPV Vaccination Rates by Sex (Age 9-13)",
         subtitle = dat[["HealthSystem"]]) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  rm(rates)
  return(g)
}

#  Final rates by sex - age 13 -------------------------------
fuDisplay4 <- function(dat) {

   dat <- sumFURates(dat)
  
  
  # girls
  totalFem <- with(dat,
                   sum(FemFUAge3_total,na.rm=T))
  dose1Fem <- with(dat,
                   sum(FemFUAge3_dose1,na.rm=T))
  dose2Fem <- with(dat,
                   sum(FemFUAge3_dose2,na.rm=T))
  meningFem <- with(dat,
                    sum(FemFUAge3_mening,na.rm=T))
  tdapFem <- with(dat,
                  sum(FemFUAge3_tdap,na.rm=T))
  
  # boys
  totalMen <- with(dat,
                   sum(MenFUAge3_total,na.rm=T))
  dose1Men <- with(dat,
                   sum(MenFUAge3_dose1,na.rm=T))
  dose2Men <- with(dat,
                   sum(MenFUAge3_dose2,na.rm=T))
  meningMen <- with(dat,
                    sum(MenFUAge3_mening,na.rm=T))
  tdapMen <- with(dat,
                  sum(MenFUAge3_tdap,na.rm=T))
  
  
  totalBoth <- with(dat,
                    sum(BothFUAge3_total,na.rm=T))
  dose1Both <- with(dat,
                    sum(BothFUAge3_dose1,na.rm=T))
  dose2Both <- with(dat,
                    sum(BothFUAge3_dose2,na.rm=T))
  meningBoth <- with(dat,
                     sum(BothFUAge3_mening,na.rm=T))
  tdapBoth <- with(dat,
                   sum(BothFUAge3_tdap,na.rm=T))
  
  rates1Fem <- dose1Fem / totalFem
  rates2Fem <- dose2Fem / totalFem
  rates1Men <- dose1Men / totalMen
  rates2Men <- dose2Men / totalMen
  rates1Both <- dose1Both / totalBoth
  rates2Both <- dose2Both / totalBoth
  
  
  ratesFemMenin <- meningFem / totalFem
  ratesMenMenin <- meningMen / totalMen
  ratesBothMenin <- meningBoth / totalBoth
  
  ratesFemtdap <- tdapFem / totalFem
  ratesMentdap <- tdapMen / totalMen
  ratesBothtdap <- tdapBoth / totalBoth
  
  rates <- data.frame(rates = c(
    rates1Fem, rates1Men, rates1Both, 
    rates2Fem, rates2Men, rates2Both,
    ratesFemMenin, ratesMenMenin, ratesBothMenin,
    ratesFemtdap, ratesMentdap, ratesBothtdap))
  
  
  
  rates$Gender <- rep(c("Females", "Males", "Combined"), 4)
  rates$Gender <- factor(rates$Gender, levels = c("Females", "Males", "Combined"))
  rates$Dose <- c(rep("1+ Dose HPV", 3), rep("HPV Complete", 3),
                  rep("Mening", 3), rep("Tdap",3))
  rates$Dose <- factor(rates$Dose,
                       levels = c("1+ Dose HPV",
                                  "HPV Complete",
                                  "Mening",
                                  "Tdap") )         
  
  rates$rates_string <- (rates$rates*100) %>%
    round(1) %>%
    format(nsmall = 1) %>%
    paste0(., "%")
  
  
  
  g <- ggplot(rates, aes(x = Gender,
                         y = rates,
                         fill = Dose)) +
    geom_col(width = 0.5, position = position_dodge(0.7))  +
    geom_text(aes(label=rates_string), vjust=-0.5, size=3,
              position = position_dodge(0.7)) +
    scale_y_continuous(limits = seq(0,1),
                       breaks = seq(0,1,0.2),
                       labels = c("0", "20%", "40%", "60%", "80%", "100%")) +
    labs(title = "Final HPV Vaccination Rates by Sex (Age 13)",
         subtitle = dat[["HealthSystem"]]) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  rm(rates)
  return(g)
}




# Monthly update functions ------------------------------------------------

monthlyFigure <- function(df) { 
  
  
  foo <- names(df)
  
  if (!"TDap" %in% foo) {
    dose1 <- data.frame(Months = df$Month, Total = df$Total, Measure = "1+ Dose HPV", N = df$Dose1)
    dose2 <- data.frame(Months = df$Month, Total = df$Total, Measure = "HPV Complete", N = df$Dose2)
    newDf <- rbind(dose1, dose2)
    newDf$Rates <- newDf$N / newDf$Total
    newDf$Months <- factor(newDf$Month,
                           levels = df$Month,
                           labels = substr(df$Month, 1, 3))
    rm(dose1, dose2)
  } else {
    dose1 <- data.frame(Months = df$Month, Total = df$Total, Measure = "1+ Dose HPV", N = df$Dose1)
    dose2 <- data.frame(Months = df$Month, Total = df$Total, Measure = "HPV Complete", N = df$Dose2)
    mening <- data.frame(Months = df$Month, Total = df$Total, Measure = "Meningococcal", N = df$Meningococcal)
    tdap <- data.frame(Months = df$Month, Total = df$Total, Measure = "Tdap", N = df$TDap)
    newDf <- rbind(dose1, dose2, mening, tdap)
    newDf$Rates <- newDf$N / newDf$Total
    newDf$Months <- factor(newDf$Month,
                           levels = df$Month,
                           labels = substr(df$Month, 1, 3))
    rm(dose1,dose2,mening, tdap)
  }
  
  
  
  ggplot(newDf, aes(x = Months, y = Rates, group = Measure, color = Measure, na.rm=T)) +
    geom_line(size = 1.25, na.rm=T) +
    geom_point(size = 2, na.rm=T) +
    scale_y_continuous(limits = seq(0,1),
                       breaks = seq(0,1,0.2),
                       labels = c("0", "20%", "40%", "60%", "80%", "100%")) +
    labs(title = "Monthly vaccination rates") + 
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 14))
  
}

threeFigures <- function(ageDat) {
  
  dat <- ageDat
  
  getLegend <- function(myggplot) {
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  
  
  # Sum boys and girls if 100% combined data are missing
  if (table(is.na(dat$age1$Total)) == 12) {
    dat$age1$Total = dat$age1$TotalF + dat$age1$TotalM
    dat$age1$Dose1 = dat$age1$Dose1F + dat$age1$Dose1M
    dat$age1$Dose2 = dat$age1$Dose2F + dat$age1$Dose2M
  }
  if (table(is.na(dat$age2$Total)) == 12) {
    dat$age2$Total = dat$age2$TotalF + dat$age2$TotalM
    dat$age2$Dose1 = dat$age2$Dose1F + dat$age2$Dose1M
    dat$age2$Dose2 = dat$age2$Dose2F + dat$age2$Dose2M
    dat$age2$Meningococcal = dat$age2$MeningococcalF + dat$age2$MeningococcalM
    dat$age2$TDap = dat$age2$TDapF + dat$age2$TDapM
  }
  
  if (table(is.na(dat$age3$Total)) == 12) {
    dat$age3$Total = dat$age3$TotalF + dat$age3$TotalM
    dat$age3$Dose1 = dat$age3$Dose1F + dat$age3$Dose1M
    dat$age3$Dose2 = dat$age3$Dose2F + dat$age3$Dose2M
    dat$age3$Meningococcal = dat$age3$MeningococcalF + dat$age3$MeningococcalM
    dat$age3$TDap = dat$age3$TDapF + dat$age3$TDapM
  }
  
  
  # Just sum all the ages for the final graph
  girls <- list(
    age1 = dplyr::select(dat$age1, Month, Total = TotalF, Dose1 = Dose1F, Dose2 = Dose2F),
    age2 = dplyr::select(dat$age2, Month, Total = TotalF, Dose1 = Dose1F, Dose2 = Dose2F, Meningococcal = MeningococcalF, Tdap = TDapF),
    age3 = dplyr::select(dat$age3, Month, Total = TotalF, Dose1 = Dose1F, Dose2 = Dose2F, Meningococcal = MeningococcalF, Tdap = TDapF)
  )
  
  boys <- list(
    age1 = dplyr::select(dat$age1, Month, Total = TotalM, Dose1 = Dose1M, Dose2 = Dose2M),
    age2 = dplyr::select(dat$age2, Month, Total = TotalM, Dose1 = Dose1M, Dose2 = Dose2M, Meningococcal = MeningococcalM, Tdap = TDapM),
    age3 = dplyr::select(dat$age3, Month, Total = TotalM, Dose1 = Dose1M, Dose2 = Dose2M, Meningococcal = MeningococcalM, Tdap = TDapM)
  )
  
  both <- list(
    age1 = dplyr::select(dat$age1, Month, Total, Dose1, Dose2),
    age2 = dplyr::select(dat$age2, Month, Total, Dose1, Dose2, Meningococcal, Tdap = TDap),
    age3 = dplyr::select(dat$age3, Month, Total, Dose1, Dose2, Meningococcal, Tdap = TDap)
  )
  
  littleFun <- function(lst){
    
    monthOrder <- lst$age1$Month
    
    foo <- list()
    foo$Total <- data.frame(Month = lst$age1$Month, age1 = lst$age1$Total, age2 = lst$age2$Total, age3 = lst$age3$Total)
    foo$Dose1 <- data.frame(Month = lst$age1$Month, age1 = lst$age1$Dose1, age2 = lst$age2$Dose1, age3 = lst$age3$Dose1)
    foo$Dose2 <- data.frame(Month = lst$age1$Month, age1 = lst$age1$Dose2, age2 = lst$age2$Dose2, age3 = lst$age3$Dose2)
    foo$Meningococcal <- data.frame(Month = lst$age2$Month, age2 = lst$age2$Meningococcal, age3 = lst$age3$Meningococcal)
    foo$Tdap <- data.frame(Month = lst$age2$Month, age2 = lst$age2$Tdap, age3 = lst$age3$Tdap)
    
    # Sum the age groups for each vaccination 
    foo <- lapply(names(foo), function(x) {
      df <- foo[[x]]
      cols <- names(df)[-1]
      df$Sum <- apply(df[,cols], 1, sum, na.rm=T)
      df$Sum <- ifelse(df$Sum == 0, NA, df$Sum)
      df <- dplyr::select(df, Month, Sum)
      names(df) <- c("Month", x)
      return(df)
    }) %>% 
      Reduce(function(x,y) full_join(x,y,"Month"), .)
    
    # Calculate the rates
    foo$Dose1 <- foo$Dose1 / foo$Total
    foo$Dose2 <- foo$Dose2 / foo$Total
    foo$Meningococcal <- foo$Meningococcal / foo$Total
    foo$Tdap <- foo$Tdap / foo$Total
    
    # Restructure the data for processing
    cols <- c("Total", "Dose1", "Dose2", "Meningococcal", "Tdap")
    final <- lapply(cols, function(x) {
      data.frame(Month = foo$Month, Group = x, Rate = foo[[x]])
    }) %>% do.call("rbind",.)
    final$Month <- factor(final$Month, levels = monthOrder, labels = substr(monthOrder,1,3))
    final$Group <- factor(final$Group, 
                          levels = cols,
                          labels = c("Total",
                                     "1+ Dose HPV",
                                     "HPV Complete",
                                     "Meningococcal",
                                     "Tdap"))
    
    return(final)
  }
  
  
  allData <- lapply(list(girls,boys,both), littleFun)
  names(allData) <- c("Females", "Males", "Males and females combined")
  
  g <- lapply(names(allData), function(x) {
    dat <- dplyr::filter(allData[[x]], Group != "Total")
    ggplot(dat, aes(x = Month, y = Rate, group = Group, color = Group, na.rm=TRUE)) +
      geom_line(size = 1.25, na.rm=T) +
      geom_point(size = 2, na.rm=T) + 
      scale_y_continuous(limits = seq(0,1),
                         breaks = seq(0,1,0.2),
                         labels = c("0", "20%", "40%", "60%", "80%", "100%")) +
      labs(title = x) +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(hjust = 0.5, size =14),
            axis.text.x = element_text(size = 8))
  })
  
  
  grid.arrange(g[[1]] + theme(legend.position = "none") + labs(title = "Females"),
               g[[2]] + theme(legend.position = "none") + labs(title = "Males"),
               g[[3]] + labs(title = "Males and Females Combined"),
               nrow = 2,
               layout_matrix= rbind(c(1,2),
                                    c(3,3)),
               top=textGrob("Monthly vaccination rates for ages 9-13", 
                            gp=gpar(fontsize=15,font=8)))
  
  
}




