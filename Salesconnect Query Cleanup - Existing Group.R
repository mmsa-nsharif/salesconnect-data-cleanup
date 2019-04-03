##### Program: Sales Connect Dataset. Includes RP/CP by Account Number
##### This program is used to create salesconnect data as an input to the preditive models
##### The dataset is refreshed monthly
##### Date Last Modified: 3/26/2019
##### Author: Naveed Sharif. MMSA | Predictive Modeling and Analytics

##### Packages required for program
library(tidyverse)
library(forcats)
library(lubridate)
library(stringr)
library(colorspace)
library(grid)
library(data.table)
library(tcltk)
library(dummies)
library(naniar)

### Functions required for program
## Function: omit NA values in a data frame, but only in some columns I am interested in
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

## Function: allows you to move columns by different order
moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", 
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}

## Function: removes white spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

### Import Salesconnect_Rate_2013to2018 dataset. Need to pull from Sales connect. Report Name: Firmographics_Existing Group
### prior to importing in R:
## (1) remove the time stamp on the bottom of the spreadsheet
## (2) update header names to make them more readable in R
Salesconnect_RateData <- read.csv("V:\\People\\Ryan Malabed\\File Drop\\Naveed\\Firmographics - Existing Group_v2.csv", stringsAsFactors = TRUE)
glimpse(Salesconnect_RateData) # lets evaluate the df at current state
mean(is.na(Salesconnect_RateData)) # this is false. 

### This SalesConnect report will be based on Existing KP accounts. Opportunities will be excluded.
### Therefore exclude Accounts where Account Number is NA
sum(is.na(Salesconnect_RateData$AccountNumber))
Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(AccountNumber = as.character(AccountNumber)) %>%
  mutate(AccountNumber = na_if(AccountNumber, "")) %>%
  filter(!is.na(AccountNumber))

glimpse(Salesconnect_RateData)

### Modify the region level names 
table(Salesconnect_RateData$Region)

Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(Region = na_if(Region, "")) %>%
  mutate(Region = na_if(Region, "Group Health")) %>%
  mutate(Region = na_if(Region, "Multi-State")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Region = fct_recode(Region,
                             "NC"    = "California - Northern",
                             "SC"    = "California - Southern",
                             "CO"    = "Colorado",
                             "GA"    = "Georgia",
                             "HI"    = "Hawaii",
                             "MA"    = "Mid-Atlantic States",
                             "NW"    = "Northwest",
                             "WA"    = "Washington"))

### Create a unique Group ID 
Salesconnect_RateData$RegionAccountNumber <- paste(Salesconnect_RateData$Region,Salesconnect_RateData$AccountNumber, sep = '_')
Salesconnect_RateData <- Salesconnect_RateData[moveme(names(Salesconnect_RateData), "RegionAccountNumber first")]

### Modify Business Line and LOB 
table(Salesconnect_RateData$LOB)
Salesconnect_RateData <- Salesconnect_RateData %>%
  select(-LOB) %>%
  mutate(BusinessSegment = na_if(BusinessSegment, "")) %>%
  mutate(BusinessSegment = as.character(BusinessSegment)) %>%
  mutate(BusinessSegment = fct_recode(BusinessSegment,
                                     "KP Group"          = "Kaiser Account",
                                     "Labor and Trust"   = "Labor & Trust",
                                     "MidSize"           = "Mid-size",
                                     "MultiState"        = "Multi-State",
                                     "MidSize"           = "Mid-size")) %>%
  rename(LOB = BusinessSegment)

### Modify status level names 
table(Salesconnect_RateData$Status)
Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(Status = na_if(Status, "")) %>%
  mutate(Status = as.character(Status)) %>%
  mutate(Status = fct_recode(Status,
                             "Prospect_Active"    = "Prospect - Active",
                             "Prospect_Inactive"  = "Prospect - Inactive",
                             "Terminated"         = "KP Terminated",
                             "NoLongerExists"     = "No Longer Exists"
                             ))

### Modify date format and add a Year and Month field
Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(EffectiveDate = as.character(EffectiveDate),
         EffectiveDate = mdy(EffectiveDate),
         EffectiveDateYear = year(EffectiveDate),
         EffectiveDateMonth = month(EffectiveDate))

### Modify product level names 
table(Salesconnect_RateData$Product)
Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(Product = na_if(Product, "")) %>%
  mutate(Product = na_if(Product, " ")) %>%
  mutate(Product = na_if(Product, "Catastrophic")) %>%
  mutate(Product = as.character(Product)) %>%
  mutate(Product = fct_recode(Product,
                              "POS"                = "Deductible POS",
                              "POS"                = "Deductible Triple Option",
                              "POS"                = "Fee for Service",
                              "POS"                = "Indemnity",
                              "POS"                = "SIG Deductible POS",
                              "POS"                = "OOA",
                              "POS"                = "Triple Option",
                              "POS"                = "OOA",
                              "POS"                = "SIG POS",
                              "POS"                = "KAISER PERMANENTE POS PLAN FOR LARGE GROUP",
                              "POS"                = "POS DB",
                              "POS"                = "POS DN",
                              "PPO"                = "KAISER PERMANENTE INSURANCE COMPANY PPO PLAN",
                              "HMO"                = "SIG HMO",
                              "HMO"                = "Signature HMO",
                              "HMO"                = "EPO",
                              "HMO"                = "KAISER PERMANENTE TRADITIONAL PLAN",
                              "HMO"                = "HMO Plus",
                              "DHMO"               = "SIG DHMO",
                              "DHMO"               = "DHMO plus",
                              "DHMO"               = "Signature DHMO",
                              "DHMO"               = "KAISER PERMANENTE DEDUCTIBLE PLAN",
                              "DHMO"               = "$150 DHMO",
                              "HDHP"               = "SIG HDHP",
                              "HDHP"               = "Signature HDHP",
                              "HDHP"               = "HSA-Qualified Flex Choice",
                              "Chiro"              = "Chiropractic",
                              "Chiro"              = "AMERICAN SPECIALTY HEALTH PLANS CHIRO & ACU PLAN", 
                              "Chiro"              = "AMERICAN SPECIALTY HEALTH PLANS CHIROPRACTIC PLAN", 
                              "DENTAL"             = "Dental (Other)",
                              "DENTAL"             = "Dental HMO/PVMX",
                              "DENTAL"             = "Dental HMO",
                              "DENTAL"             = "Dental PPO/PVMX",
                              "DENTAL"             = "Dental PPO",
                              "DENTAL"             = "Dental",
                              "Medicare"           = "Medicare Part D",
                              "Medicare"           = "Medicare Cost",
                              "Medicare"           = "Medicare Supplement",
                              "Medicare"           = "Medicare Advantage",
                              "Other"              = "Ded/coins",
                              "Other"              = "Community care",
                              "Other"              = "Deductible Flex Choice",
                              "Other"              = "Providence"
                              ))

### Impute products based on logic provided by the Rates team
## Correct product: HDHP
summary(Salesconnect_RateData$Deductible)

Salesconnect_RateData %>%
  filter(Deductible >= 1200 & Product == "HMO") # should be HDHP product. lets fix this.

Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(Product = as.character(Product)) %>%
  mutate(Deductible = ifelse(Deductible < 0,0,Deductible)) %>%
  mutate(Deductible = ifelse(Deductible > 5000,5000,Deductible)) %>%
  mutate(Product = ifelse(Deductible >= 1200 & (Product == "HMO" | Product == "DHMO") & EffectiveDateYear == 2012,"HDHP",Product)) %>%
  mutate(Product = ifelse(Deductible >= 1250 & (Product == "HMO" | Product == "DHMO") & (EffectiveDateYear == 2013 | EffectiveDateYear == 2014),"HDHP",Product)) %>%
  mutate(Product = ifelse(Deductible >= 1300 & (Product == "HMO" | Product == "DHMO") & (EffectiveDateYear == 2015 | EffectiveDateYear == 2016 | EffectiveDateYear == 2017),"HDHP",Product)) %>%
  mutate(Product = ifelse(Deductible >= 1350 & (Product == "HMO" | Product == "DHMO") & EffectiveDateYear >= 2018,"HDHP",Product))

## Correct product: DHMO
Salesconnect_RateData %>%
  filter((Deductible >= 250 & Deductible < 1200) & Product == "HMO") # should be DHMO product. lets fix this.

Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(Product = as.character(Product)) %>%
  mutate(Product = ifelse((Deductible >= 250 & Deductible < 1200) & (Product == "HMO" | Product == "HDHP") & EffectiveDateYear == 2012,"DHMO",Product)) %>%
  mutate(Product = ifelse((Deductible >= 250 & Deductible < 1250) & (Product == "HMO" | Product == "HDHP") & (EffectiveDateYear == 2013 | EffectiveDateYear == 2014),"DHMO",Product)) %>%
  mutate(Product = ifelse((Deductible >= 250 & Deductible < 1300) & (Product == "HMO" | Product == "HDHP") & (EffectiveDateYear == 2015 | EffectiveDateYear == 2016 | EffectiveDateYear == 2017),"DHMO",Product)) %>%
  mutate(Product = ifelse((Deductible >= 250 & Deductible < 1350) & (Product == "HMO" | Product == "DHMO") & EffectiveDateYear >= 2018,"DHMO",Product))

## Correct product: HMO
Salesconnect_RateData %>%
  filter((Deductible >= 0 & Deductible < 250) & (Product == "DHMO" | Product == "HDHP")) # should be HMO product. lets fix this.

Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(Product = as.character(Product)) %>%
  mutate(Product = ifelse((Deductible >= 0 & Deductible < 250) & (Product == "DHMO" | Product == "HDHP"),"HMO",Product)) %>%
  mutate(Deductible = ifelse((Deductible >= 0 & Deductible < 250) & Product == "HMO",0,Deductible))

## Create product dummy variables
Salesconnect_RateData <- cbind.data.frame(Salesconnect_RateData,dummies::dummy(Salesconnect_RateData$Product, sep = "_"))
for( i in colnames(Salesconnect_RateData)){
  colnames(Salesconnect_RateData)[which(colnames(Salesconnect_RateData)==i)] = gsub("Salesconnect_RateData_","Product_",(i))
}
remove(i)

### Modify carrier level names 
table(Salesconnect_RateData$Carrier)
Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(Carrier = na_if(Carrier, "")) %>%
  mutate(Carrier = na_if(Carrier, "SIMNSA")) %>%
  mutate(Carrier = na_if(Carrier, "Unknown")) %>%
  mutate(Carrier = na_if(Carrier, "None")) %>%  
  mutate(Carrier = na_if(Carrier, "No Current Carrier")) %>%    
  mutate(Carrier = as.character(Carrier)) %>%
  mutate(Carrier = fct_recode(Carrier,
                              "Aetna"           = "Aetna - S/F",
                              "Aetna"           = "Coventry",
                              "Aetna"           = "Coventry Health",
                              "BCBS"            = "Anthem Blue Cross",
                              "BCBS"            = "Anthem",
                              "BCBS"            = "Blue Cross & Blue Shield of Illinois",
                              "BCBS"            = "CareFirst",
                              "BCBS"            = "UniCare",
                              "BCBS"            = "Regence BCBS",
                              "BCBS"            = "Anthem - S/F",
                              "BCBS"            = "Blue Cross of California",
                              "BCBS"            = "Lifewise",
                              "BCBS"            = "Other Blue Cross / Blue Shield",
                              "BCBS"            = "AmeriGroup",
                              "BCBS"            = "Anthem (Wellpoint)",
                              "BCBS"            = "Blue Cross",
                              "BCBS"            = "Blue Shield",
                              "BCBS"            = "Care More",
                              "BCBS"            = "Highmark",
                              "BCBS"            = "Premera",
                              "BCBS"            = "Regence Blue Shield of Washington",
                              "BCBS"            = "Regence BCBS of Oregon",
                              "BCBS"            = "WellPoint",
                              "BCBS"            = "WellChoice",
                              "CalChoice"       = "Cal-Choice 51+",
                              "CalChoice"       = "Cal Choice Small Group",
                              "Cigna"           = "Cigna - S/F",
                              "Dignity"         = "Western Health Advantage",
                              "KP"              = "Group Health",
                              "KP"              = "Kaiser Permanente Inside Exchange",
                              "KP"              = "Kaiser Permanente",
                              "HealthNet"       = "Health Net of California",
                              "HealthNet"       = "Health Net Insurance",
                              "Other"           = "HMA/Summerlin",
                              "Other"           = "Other HMO",
                              "Other"           = "MetLife",
                              "Other"           = "Family Health Hawaii",
                              "Other"           = "Nippon Life Insurance",
                              "Other"           = "Other Dental",
                              "Other"           = "Principal Financial Group",
                              "Other"           = "Western Growers",
                              "Other"           = "Guardian",
                              "Other"           = "GEMCare",
                              "Other"           = "Individual Exchange",
                              "Other"           = "Washington Dental Services",
                              "Other"           = "Denver Health Medical Plan",
                              "Other"           = "Great West Life",
                              "Other"           = "MAMSI",
                              "Other"           = "Other Carrier",
                              "Other"           = "The Standard",
                              "Other"           = "Willamette Dental",
                              "Other"           = "Pacific Source Health Insurance",
                              "Other"           = "Providence Health Plans",
                              "Other"           = "PacifiCare",
                              "Other"           = "Ameritas",
                              "Other"           = "Other, See Comments",
                              "Other"           = "TPA",
                              "Other"           = "Other TPA",
                              "Other"           = "National Self Funding Plan",
                              "Other"           = "Self Funded Plan",
                              "Other"           = "TPA - Third Party Self-Funded",
                              "Other"           = "Principal Life Insurance",
                              "Other"           = "Private Exchange",
                              "Other"           = "SCAN",
                              "Other"           = "Asuris",
                              "Other"           = "First Choice",
                              "Other"           = "United Ag",
                              "United"          = "Rocky Mtn HMO",
                              "United"          = "United Healthcare",
                              "United"          = "United Healthcare - S/F"
                              ))

## Create competitor Carrier Dummies
Salesconnect_RateData <- cbind.data.frame(Salesconnect_RateData,dummies::dummy(Salesconnect_RateData$Carrier, sep = "_"))
for( i in colnames(Salesconnect_RateData)){
  colnames(Salesconnect_RateData)[which(colnames(Salesconnect_RateData)==i)] = gsub("Salesconnect_RateData_","",(i))
}
remove(i)

Salesconnect_RateData <- Salesconnect_RateData %>%
  rename(Carrier_NA = `NA`)

glimpse(Salesconnect_RateData)

### Modify Plan Status and Stage
Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(PlanStatus = na_if(PlanStatus, "")) %>%
  mutate(PlanStatus = as.character(PlanStatus)) %>%
  mutate(PlanStatus = fct_recode(PlanStatus,
                                 "ClonedPendingRenewal"    = "Cloned Pending Renewal",
                                 "ProposedNew"             = "Proposed New",
                                 "ProposedRenewal"         = "Proposed Renewal",
                                 "Sold"                    = "Won",
                                 "Lost"                    = "Closed lost")) %>%
  mutate(Stage = na_if(Stage, "")) %>%
  mutate(Stage = as.character(Stage)) %>%
  mutate(Stage = fct_recode(Stage,
                            "ClonedPendingRenewal"    = "Cloned - Pending Review",
                            "ClosedLost"              = "Closed Lost",
                            "ClosedWithdrawn"         = "Closed Withdrawn",
                            "ClosedWon"               = "Closed Won",
                            "OpportunityDevelopment"  = "Opportunity Development",
                            "Proposal"                = "Proposal/UW",
                            "RenewalInitiated"        = "Renewal Initiated"
                            ))

### Modify T1 Medical Rate
summary(Salesconnect_RateData$T1MedicalRate)

Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(T1MedicalRate = ifelse((Product == "HMO" | Product == "DHMO" | Product == "HDHP" | Product == "PPO" | Product == "POS") 
                                & (T1MedicalRate < 100 | T1MedicalRate > 2000),NA,T1MedicalRate))

### Modify T1 Monthly Contribution Rate
summary(Salesconnect_RateData$T1MonthlyContribution)

Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(T1MonthlyContribution = ifelse((Product == "HMO" | Product == "DHMO" | Product == "HDHP" | Product == "PPO" | Product == "POS") 
                                & (T1MonthlyContribution > 2000),NA,T1MonthlyContribution)) %>%
  mutate(T1MonthlyContribution = ifelse((T1MonthlyContribution < 0),0,T1MonthlyContribution)) %>%
  mutate(T1MonthlyContribution = ifelse((ContributionMethod == "100% Employer Paid"),0,T1MonthlyContribution))

### Modify ContributionMethod
table(Salesconnect_RateData$ContributionMethod)
Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(ContributionMethod = na_if(ContributionMethod, "")) %>%
  mutate(ContributionMethod = na_if(ContributionMethod, "Don?t Know")) %>%
  mutate(ContributionMethod = na_if(ContributionMethod, "Contribution Not Available")) %>%
  mutate(ContributionMethod = as.character(ContributionMethod)) %>%
  mutate(ContributionMethod = fct_recode(ContributionMethod,
                                         "FullEmployerPaid"     = "100% Employer Paid",
                                         "FixedEmployee"        = "Fixed Employee %",
                                         "FixedEmployee"        = "Fixed Employee $",
                                         "FixedEmployer"        = "Fixed Employer %",
                                         "FixedEmployer"        = "Fixed Employer $",
                                         "ValueBased"           = "Value-Based",
                                         "Custom"               = "Custom Formula",
                                         "Custom"               = "Risk Adjusted",
                                         "Custom"               = "Custom/Other",
                                         "PercentageofAll"      = "Percentage of All",
                                         "PercentageofSingle"   = "Percentage of Single",
                                         "EqualERDollar"        = "Equal ER Dollar",
                                         "EqualEEDollar"        = "Equal EE Dollar",
                                         "RenewalInitiated"     = "Renewal Initiated"
                                         ))

### Modify Funding Mechanism
table(Salesconnect_RateData$FundingMechanism)
Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(FundingMechanism = na_if(FundingMechanism, "")) %>%
  mutate(FundingMechanism = as.character(FundingMechanism)) %>%
  mutate(FundingMechanism = fct_recode(FundingMechanism,
                                       "Other"           = "Alternative Funding",
                                       "Other"           = "Deferred Premium",
                                       "Other"           = "Indemnity",
                                       "None"            = "None",
                                       "FullyInsured"    = "Fully Insured",
                                       "SelfFunded"      = "Partially Self-Funded",
                                       "SelfFunded"      = "Self-Funded",
                                       "SelfFunded"      = "Self Funded"
                                       ))

## Create Funding Mechanism dummy variables
Salesconnect_RateData <- cbind.data.frame(Salesconnect_RateData,dummies::dummy(Salesconnect_RateData$FundingMechanism, sep = "_"))
for( i in colnames(Salesconnect_RateData)){
  colnames(Salesconnect_RateData)[which(colnames(Salesconnect_RateData)==i)] = gsub("Salesconnect_RateData_","FundingMechanism_",(i))
}
remove(i)

glimpse(Salesconnect_RateData)

### Modify Payment Frequency
table(Salesconnect_RateData$PaymentFrequency)
Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(PaymentFrequency = na_if(PaymentFrequency, "")) %>%
  mutate(PaymentFrequency = na_if(PaymentFrequency, "Unknown")) %>%
  mutate(PaymentFrequency = as.character(PaymentFrequency)) %>%
  mutate(PaymentFrequency = fct_recode(PaymentFrequency,
                                       "Other"      = "8-Monthly",
                                       "Other"      = "9-Monthly",
                                       "Other"      = "Annually",
                                       "Other"      = "Tenthly",
                                       "Other"      = "Weekly",
                                       "Other"      = "Quarterly",
                                       "Other"      = "Semi-Monthly",
                                       "BiWeekly"   = "Bi-Weekly"
                                       ))

### Modify Rate Type and Network type
table(Salesconnect_RateData$RateType)
Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(RateType = na_if(RateType, "")) %>%
  mutate(NetworkType = na_if(NetworkType, ""))

### Modify Offering Scenario. use this one. less NAs
table(Salesconnect_RateData$OfferingScenario)
Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(OfferingScenario = na_if(OfferingScenario, "")) %>%
  mutate(OfferingScenario = as.character(OfferingScenario)) %>%
  mutate(OfferingScenario = fct_recode(OfferingScenario,
                                       "Sole"    = "Sole Carrier",
                                       "TR"      = "Total Replacement (KPIC - OOA)",
                                       "TR"      = "Total Replacement (KPIC)"
  ))

## Create Offering Scenario dummy variables
Salesconnect_RateData <- cbind.data.frame(Salesconnect_RateData,dummies::dummy(Salesconnect_RateData$OfferingScenario, sep = "_"))
for( i in colnames(Salesconnect_RateData)){
  colnames(Salesconnect_RateData)[which(colnames(Salesconnect_RateData)==i)] = gsub("Salesconnect_RateData_","OfferingScenario_",(i))
}
remove(i)

glimpse(Salesconnect_RateData)

### Modify TR Slice
table(Salesconnect_RateData$TR_Slice)
Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(TR_Slice = na_if(TR_Slice, "")) %>%
  mutate(TR_Slice = as.character(TR_Slice)) %>%
  mutate(TR_Slice = fct_recode(TR_Slice,
                               "SlicetoTR"     = "Slice-to-TR",
                               "SlicetoTR"     = "Slice to TR-OOA",
                               "SlicetoSole"   = "Slice-to-Sole Carrier",
                               "Slice"         = "Slice-to-Slice",
                               "SoletoTR"      = "Sole Carrier-to-TR",
                               "SoletoTR"      = "Sole Carrier to TR-OOA",
                               "SoletoSlice"   = "Sole Carrier-to-Slice",
                               "SoletoTR"      = "Sole Carrier to TR-OOA",
                               "Sole"          = "Sole Carrier-to-Sole Carrier",
                               "TRtoSole"      = "TR-to-Sole Carrier",
                               "TRtoSlice"     = "TR-to-Slice",
                               "TRtoSlice"     = "TR-OOA to Slice",
                               "TR"            = "TR-to-TR",
                               "TR"            = "TR-OOA to TR",
                               "TR"            = "TR to TR-OOA",                               
                               "TR"            = "Total Replacement"
  ))

### Fix the industry code by removing invalid industry codes
Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(IndustryCode_NAICS = na_if(IndustryCode_NAICS, "")) %>%
  mutate(IndustryCode_NAICS_abbrv = substring(IndustryCode_NAICS, 1, 2)) %>%
  replace_with_na(replace = list(IndustryCode_NAICS_abbrv = c("Ed","Hi","nu","Nu","NU","ot","Tr","un","0","1"
                                                              ,"12","13","15","17","18","19","20","28","35"
                                                              ,"36","37","38","39","41","47","50","58","60"
                                                              ,"65","69","73","75","76","78","80","82","86"
                                                              ,"87","91","95","97","98","99","00","01","07"
                                                              ,"09","10","14","16","24","25","26","27","29"
                                                              ,"30","34","40","43","46","47","57","59","63"
                                                              ,"64","67","70","74","79","8/","83","84","85"
                                                              ,"89","93","Ag","Ar","Ba","Be","Ch","Co","Ex"
                                                              ,"Ho","In","Ja","La","Ma","Me","no","Oa","Pe"
                                                              ,"Re","Wh","94","05","08","7",", ","")))

## Merge industry descriptions to SalesConnectDatamart dataset 
IndustryCode <- read.csv("V:\\People\\Naveed Sharif\\Data\\IndustryCode.csv", stringsAsFactors = FALSE)
IndustryCode$IndustryCode_NAICS_abbrv <- as.character(IndustryCode$IndustryCode_NAICS)

Salesconnect_RateData <- left_join(Salesconnect_RateData, IndustryCode, by = c("IndustryCode_NAICS_abbrv"))

table(Salesconnect_RateData$IndustryDescription)
remove(IndustryCode)

### Modify Brokerage Firm City and Zipcode
table(Salesconnect_RateData$BrokerageFirmCity)
Salesconnect_RateData <- Salesconnect_RateData %>%
  replace_with_na(replace = list(BrokerageFirmCity = c("","12100 n.e. 195th street s","n/a"))) %>%
  mutate(BrokerageFirmCity = as.character(BrokerageFirmCity)) %>%
  mutate(BrokerageFirmCity = tolower(BrokerageFirmCity)) %>%
  mutate(BrokerageFirmCity = fct_recode(BrokerageFirmCity,
                               "agoura hills"    = "agoura",
                               "alexandria"      = "alexandria,",
                               "aliso viejo"     = "aliso vieijo",
                               "bridgewater"     = "bridge water",
                               "carrollton"      = "carrolton",
                               "ellicott city"   = "ellicot city",
                               "ft. lauderdale"  = "ft lauderdale",
                               "johns creek"     = "john's creek",
                               "portland"        = "portaland",
                               "salt lake city"  = "salt lake",
                               "san francisco"   = "san fransisco",
                               "st. louis"       = "st louis",
                               "st. louis park"  = "st louis park",
                               "torrance"        = "torrence",
                               "warrington"      = "warrrington")) %>%
  mutate(BrokerageFirmZip = na_if(BrokerageFirmZip, ""))

## Modify Brokerage info null values
Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(BrokerageFirmID18 = na_if(BrokerageFirmID18, "")) %>%
  mutate(BrokerageFirmID = na_if(BrokerageFirmID, "")) %>%
  mutate(BrokerageFirmStreet = na_if(BrokerageFirmStreet, "")) %>%
  mutate(BrokerageFirmState = na_if(BrokerageFirmState, "")) %>%
  mutate(BrokerageFirmZip = na_if(BrokerageFirmZip, "")) %>%
  mutate(BrokerCommission = na_if(BrokerCommission, "")) %>%
  mutate(BrokerPhone = na_if(BrokerPhone, "")) 

## Modify Brokerage values with links. need to extract names and exclude links. 
Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(BrokerageFirmName = as.character(BrokerageFirmName)) %>%
  mutate(BrokerName = as.character(BrokerName)) %>%
  mutate(BrokerEmail = as.character(BrokerEmail))

BrokerageFirmName <- str_replace_all(word(Salesconnect_RateData$BrokerageFirmName, 5, sep = fixed('"')), c(">" = "", "<" =  "", "/a" =  ""))
BrokerName <- str_replace_all(word(Salesconnect_RateData$BrokerName, 5, sep = fixed('"')), c(">" = "", "<" =  "", "/a" =  ""))
BrokerEmail <- str_replace_all(word(Salesconnect_RateData$BrokerEmail, 5, sep = fixed('"')), c(">" = "", "<" =  "", "/a" =  ""))

Salesconnect_RateData <- Salesconnect_RateData %>%
  select(-BrokerageFirmName,-BrokerName,-BrokerEmail)

Salesconnect_RateData <- cbind(Salesconnect_RateData,BrokerageFirmName, BrokerName, BrokerEmail)

Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(BrokerageFirmName = trim(BrokerageFirmName)) %>%
  mutate(BrokerName = trim(BrokerName)) %>%
  mutate(BrokerEmail = trim(BrokerEmail)) %>%
  mutate(BrokerageFirmName = na_if(BrokerageFirmName, "")) %>%
  mutate(BrokerName = na_if(BrokerName, "")) %>%
  mutate(BrokerEmail = na_if(BrokerEmail, ""))

remove(BrokerageFirmName,BrokerName,BrokerEmail)

Salesconnect_RateData <- Salesconnect_RateData %>% 
  mutate(BrokerageFirmStreet = tolower(BrokerageFirmStreet)) %>%
  mutate(BrokerageFirmZip = tolower(BrokerageFirmZip)) %>%
  mutate(BrokerageFirmName = tolower(BrokerageFirmName)) %>%
  mutate(BrokerName = tolower(BrokerName)) %>%
  mutate(BrokerEmail = tolower(BrokerEmail)) %>%
  mutate(BrokerageFirmState = tolower(BrokerageFirmState))  

Salesconnect_RateData <- Salesconnect_RateData[moveme(names(Salesconnect_RateData), "BrokerageFirmName before BrokerPhone")]
Salesconnect_RateData <- Salesconnect_RateData[moveme(names(Salesconnect_RateData), "BrokerName before BrokerPhone")]
Salesconnect_RateData <- Salesconnect_RateData[moveme(names(Salesconnect_RateData), "BrokerEmail before BrokerPhone")]

## Modify broker firm state labels
table(Salesconnect_RateData$BrokerageFirmState)
Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(BrokerageFirmState = as.character(BrokerageFirmState)) %>%
  mutate(BrokerageFirmState = fct_recode(BrokerageFirmState,
                                        "az"    = "arizona",
                                        "co"    = "colorado",
                                        "md"    = "maryland",
                                        "va"    = "virgiina",
                                        "va"    = "virginia",
                                        "wa"    = "washington",
                                        "wi"    = "wisconsin",
                                        "ia"    = "waukee"))

### Group is offered an HRA / HSA plan
Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(PlanName = as.character(PlanName)) %>%
  mutate(HSAorHRA = str_detect(PlanName, paste(c("HSA","HRA"), collapse = '|')))

### Remove duplicate records
Salesconnect_RateData <- Salesconnect_RateData %>%
  mutate(IDduplicaterecords = paste(RegionAccountNumber, Carrier, Product, Deductible
                                    , OOPM, EffectiveDate, HSAorHRA, T1MedicalRate, sep = '_')) %>%
  arrange(IDduplicaterecords) %>%
  distinct(.,IDduplicaterecords, .keep_all= TRUE) %>%
  select(-IDduplicaterecords)

## deductibles. creating ordinal bins.
## Salesconnect_RateData <- Salesconnect_RateData %>%
##  mutate(DeductibleBin = case_when(Deductible <= 250 ~ 0,
##                              Deductible > 250 & Deductible <= 500 ~ 1,
##                              Deductible > 500 & Deductible <= 1000 ~ 2,
##                              Deductible > 1000 & Deductible <= 1500 ~ 3,
##                              Deductible > 1500 & Deductible <= 2500 ~ 4,
##                              Deductible > 2500 ~ 5))

## out of poket max (oopm). creating ordinal bins
## Salesconnect_RateData <- Salesconnect_RateData %>%
##  mutate(OOPM_Bin = case_when(OOPM <= 100 ~ 0,
##                              OOPM > 100 & OOPM <= 500 ~ 1,
##                              OOPM > 500 & OOPM <= 1000 ~ 2,
##                              OOPM > 1000 & OOPM <= 1500 ~ 3,
##                              OOPM > 1500 & OOPM <= 2000 ~ 4,
##                              OOPM > 2000 & OOPM <= 2500 ~ 5,
##                              OOPM > 2500 & OOPM <= 3000 ~ 6,
##                              OOPM > 3000 & OOPM <= 3500 ~ 7,
##                              OOPM > 3500 & OOPM <= 4000 ~ 8,
##                              OOPM > 4000 & OOPM <= 5000 ~ 9,
##                              OOPM > 5000 & OOPM <= 6000 ~ 10,
##                              OOPM > 6000 ~ 11))

### Create new group variables. 
## Group is offered an HRA / HSA plan
KP <- filter(Salesconnect_RateData, Carrier == "KP")
Comp <- filter(Salesconnect_RateData, Carrier != "KP")

KP <- KP %>%
  group_by(RegionAccountNumber, EffectiveDate) %>%
  mutate(KPoffersHSAorHRA = ifelse(HSAorHRA == "TRUE",1,0)) %>%
  summarize(GL_KPoffersHSAorHRA = max(KPoffersHSAorHRA))
  
Comp <- Comp %>%
  group_by(RegionAccountNumber, EffectiveDate) %>%
  mutate(CompoffersHSAorHRA = ifelse(HSAorHRA == "TRUE",1,0)) %>%
  summarize(GL_CompoffersHSAorHRA = max(CompoffersHSAorHRA))

Salesconnect_RateData <- Salesconnect_RateData %>%
  left_join(.,KP, by = c("RegionAccountNumber","EffectiveDate")) %>%
  left_join(.,Comp, by = c("RegionAccountNumber","EffectiveDate")) %>%
  select(-HSAorHRA) %>%
  mutate(GL_KPoffersHSAorHRA = ifelse(is.na(GL_KPoffersHSAorHRA),0,GL_KPoffersHSAorHRA)) %>%
  mutate(GL_CompoffersHSAorHRA = ifelse(is.na(GL_CompoffersHSAorHRA),0,GL_CompoffersHSAorHRA))

remove(KP,Comp) 

## Offering Scenario
KP <- filter(Salesconnect_RateData, Carrier == "KP")
Comp <- filter(Salesconnect_RateData, Carrier != "KP")

KP <- KP %>%
  group_by(RegionAccountNumber, EffectiveDate) %>%
  mutate(OfferingScenario_SoleTR = ifelse(OfferingScenario_Sole == 1 | OfferingScenario_TR == 1,1,0)) %>%
  summarize(GL_KPOfferingScenario_SoleTR = max(OfferingScenario_SoleTR)
            , GL_KPOfferingScenario_Slice = max(OfferingScenario_Slice))

Salesconnect_RateData <- Salesconnect_RateData %>%
  left_join(.,KP, by = c("RegionAccountNumber","EffectiveDate")) %>%
  mutate(GL_KPOfferingScenario_SoleTR = ifelse(is.na(GL_KPOfferingScenario_SoleTR),0,GL_KPOfferingScenario_SoleTR)) %>%
  mutate(GL_KPOfferingScenario_Slice = ifelse(is.na(GL_KPOfferingScenario_Slice),0,GL_KPOfferingScenario_Slice))

remove(KP,Comp) 

## Funding Mechanism
Comp <- filter(Salesconnect_RateData, Carrier != "KP")

Comp <- Comp %>%
  group_by(RegionAccountNumber, EffectiveDate) %>%
  summarize(GL_FundingMechanism_SelfFunded = max(FundingMechanism_SelfFunded),
            GL_FundingMechanism_FullyInsured = max(FundingMechanism_FullyInsured))

Salesconnect_RateData <- Salesconnect_RateData %>%
  left_join(.,Comp, by = c("RegionAccountNumber","EffectiveDate")) %>%
  mutate(GL_FundingMechanism_SelfFunded = ifelse(is.na(GL_FundingMechanism_SelfFunded),0,GL_FundingMechanism_SelfFunded)) %>%
  mutate(GL_FundingMechanism_FullyInsured = ifelse(is.na(GL_FundingMechanism_FullyInsured),0,GL_FundingMechanism_FullyInsured))

remove(Comp)

## Count of unique products offered in a group
KP <- Salesconnect_RateData %>%
  filter(Carrier == "KP") %>%
  mutate(UniqueProductID = paste(Product, OOPM, Deductible, sep = '_'))

Comp <- Salesconnect_RateData %>%
  filter(Carrier != "KP") %>%
  mutate(UniqueProductID = paste(Product, OOPM, Deductible, sep = '_'))

KP <- KP %>%
  group_by(RegionAccountNumber, EffectiveDate) %>%
  summarize(GL_KPNumOfUniqueProducts = n_distinct(UniqueProductID),
            GL_KPNumProducts = n_distinct(Product))

Comp <- Comp %>%
  group_by(RegionAccountNumber, EffectiveDate) %>%
  summarize(GL_CompNumOfUniqueProducts = n_distinct(UniqueProductID),
            GL_CompNumProducts = n_distinct(Product))

Salesconnect_RateData <- Salesconnect_RateData %>%
  left_join(.,KP, by = c("RegionAccountNumber","EffectiveDate")) %>%
  left_join(.,Comp, by = c("RegionAccountNumber","EffectiveDate")) %>%
  mutate(GL_KPNumOfUniqueProducts = ifelse(is.na(GL_KPNumOfUniqueProducts),0,GL_KPNumOfUniqueProducts)) %>%
  mutate(GL_KPNumProducts = ifelse(is.na(GL_KPNumProducts),0,GL_KPNumProducts)) %>%
  mutate(GL_CompNumOfUniqueProducts = ifelse(is.na(GL_CompNumOfUniqueProducts),0,GL_CompNumOfUniqueProducts)) %>%
  mutate(GL_CompNumProducts = ifelse(is.na(GL_CompNumProducts),0,GL_CompNumProducts)) %>%
  arrange(RegionAccountNumber, EffectiveDateYear)

remove(KP, Comp)

## Flagging groups that offer an HMO, HDHP, HMO, POS/PPO
KP <- Salesconnect_RateData %>%
  filter(Carrier == "KP") %>%
  group_by(RegionAccountNumber, EffectiveDate) %>%
  summarize(GL_KPProductHMO = max(Product_HMO)
            , GL_KPProductDHMO = max(Product_DHMO)
            , GL_KPProductHDHP = max(Product_HDHP))
  
Comp <- Salesconnect_RateData %>%
  filter(Carrier != "KP") %>%
  group_by(RegionAccountNumber, EffectiveDate) %>%
  mutate(Product_PPOPOS = ifelse(Product_POS == 1 | Product_PPO == 1,1,0)) %>%
  summarize(GL_CompProductHMO = max(Product_HMO)
            , GL_CompProductDHMO = max(Product_DHMO)
            , GL_CompProductHDHP = max(Product_HDHP)
            , GL_CompProductPPOPOS = max(Product_PPOPOS))

Salesconnect_RateData <- Salesconnect_RateData %>%
  left_join(.,KP, by = c("RegionAccountNumber","EffectiveDate")) %>%
  left_join(.,Comp, by = c("RegionAccountNumber","EffectiveDate")) %>%
  mutate(GL_KPProductHMO = ifelse(is.na(GL_KPProductHMO),0,GL_KPProductHMO)) %>%
  mutate(GL_KPProductDHMO = ifelse(is.na(GL_KPProductDHMO),0,GL_KPProductDHMO)) %>%
  mutate(GL_KPProductHDHP = ifelse(is.na(GL_KPProductHDHP),0,GL_KPProductHDHP)) %>%
  mutate(GL_CompProductHMO = ifelse(is.na(GL_CompProductHMO),0,GL_CompProductHMO)) %>%
  mutate(GL_CompProductDHMO = ifelse(is.na(GL_CompProductDHMO),0,GL_CompProductDHMO)) %>%
  mutate(GL_CompProductHDHP = ifelse(is.na(GL_CompProductHDHP),0,GL_CompProductHDHP)) %>%
  mutate(GL_CompProductPPOPOS = ifelse(is.na(GL_CompProductPPOPOS),0,GL_CompProductPPOPOS))

remove(KP, Comp)

## Count of unique carries in a group (including KP)
NumofCarriers <- Salesconnect_RateData %>%
  group_by(RegionAccountNumber,EffectiveDate) %>%
  summarize(GL_NumOfCarriers = n_distinct(Carrier))

Salesconnect_RateData <- Salesconnect_RateData %>%
  left_join(.,NumofCarriers, by = c("RegionAccountNumber","EffectiveDate"))

remove(NumofCarriers)

## Flag Carriers 
Carriers <- Salesconnect_RateData %>%
  group_by(RegionAccountNumber, EffectiveDate) %>%
  summarize(GL_Competitor_Aetna = max(Aetna)
            , GL_Competitor_BCBS = max(BCBS)
            , GL_Competitor_Cigna = max(Cigna)
            , GL_Competitor_United = max(United)
            , GL_Competitor_HealthNet = max(HealthNet)
            , GL_Competitor_CalChoice = max(CalChoice)
            , GL_Competitor_HMAA = max(HMAA)
            , GL_Competitor_HMSA = max(HMSA)
            , GL_Competitor_UHA = max(UHA)
            , GL_Competitor_Humana = max(Humana)
            , GL_Competitor_Moda = max(Moda)
            , GL_Competitor_Sharp = max(Sharp)
            , GL_Competitor_Sutter = max(Sutter)
            , GL_Competitor_Dignity = max(Dignity))

Salesconnect_RateData <- Salesconnect_RateData %>%
  left_join(.,Carriers, by = c("RegionAccountNumber","EffectiveDate")) %>%
  mutate(GL_Competitor_Aetna = ifelse(is.na(GL_Competitor_Aetna),0,GL_Competitor_Aetna)) %>%
  mutate(GL_Competitor_BCBS = ifelse(is.na(GL_Competitor_BCBS),0,GL_Competitor_BCBS)) %>%
  mutate(GL_Competitor_Cigna = ifelse(is.na(GL_Competitor_Cigna),0,GL_Competitor_Cigna)) %>%
  mutate(GL_Competitor_United = ifelse(is.na(GL_Competitor_United),0,GL_Competitor_United)) %>%
  mutate(GL_Competitor_HealthNet = ifelse(is.na(GL_Competitor_HealthNet),0,GL_Competitor_HealthNet)) %>%
  mutate(GL_Competitor_CalChoice = ifelse(is.na(GL_Competitor_CalChoice),0,GL_Competitor_CalChoice)) %>%
  mutate(GL_Competitor_HMAA = ifelse(is.na(GL_Competitor_HMAA),0,GL_Competitor_HMAA)) %>%
  mutate(GL_Competitor_HMSA = ifelse(is.na(GL_Competitor_HMSA),0,GL_Competitor_HMSA)) %>%
  mutate(GL_Competitor_UHA = ifelse(is.na(GL_Competitor_UHA),0,GL_Competitor_UHA)) %>%
  mutate(GL_Competitor_Humana = ifelse(is.na(GL_Competitor_Humana),0,GL_Competitor_Humana)) %>%
  mutate(GL_Competitor_Moda = ifelse(is.na(GL_Competitor_Moda),0,GL_Competitor_Moda)) %>%
  mutate(GL_Competitor_Sharp = ifelse(is.na(GL_Competitor_Sharp),0,GL_Competitor_Sharp)) %>%
  mutate(GL_Competitor_Sutter = ifelse(is.na(GL_Competitor_Sutter),0,GL_Competitor_Sutter)) %>%
  mutate(GL_Competitor_Dignity = ifelse(is.na(GL_Competitor_Dignity),0,GL_Competitor_Dignity))

remove(Carriers)

### Creating a collection of datasets that have RP, CP, Deductible, & OOPM calculations
### Seperate out by effective date year. We will want to construct the dataset as panel dataset
### Note. These are capturing time variant feautres.
Salesconnect_RateData <- Salesconnect_RateData %>%
  arrange(RegionAccountNumber, EffectiveDate)

## 2012 Rate, Deductible, OOPM by min, max, and mean. 
Salesconnect_RateData_2012 <- Salesconnect_RateData %>%
  filter(EffectiveDateYear == 2012)

Salesconnect_RateData_2012_KP <- Salesconnect_RateData_2012 %>%
  group_by(RegionAccountNumber) %>%
  filter(Carrier == "KP") %>%
  summarize(KPRateMean = round(mean(T1MedicalRate, na.rm = TRUE),0)
            , KPRateLowest = round(min(T1MedicalRate, na.rm = TRUE),0)
            , KPCRateMean = round(mean(T1MonthlyContribution, na.rm = TRUE),0)
            , KPCRateLowest = round(min(T1MonthlyContribution, na.rm = TRUE),0)
            , KPDeductibleHighest = round(max(Deductible, na.rm = TRUE),0)
            , KPDeductibleLowest = round(min(Deductible, na.rm = TRUE),0)
            , KPDeductibleMean = round(mean(Deductible, na.rm = TRUE),0)
            , KPOopmHighest = round(max(OOPM, na.rm = TRUE),0)
            , KPOopmLowest = round(min(OOPM, na.rm = TRUE),0)
            , KPOopmMean = round(mean(OOPM, na.rm = TRUE),0)) %>%
  replace_with_na(replace = list(KPRateMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPRateLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPCRateMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPCRateLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleHighest = c("Inf", "-Inf","NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmHighest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmMean = c("Inf", "-Inf", "NaN")))

Salesconnect_RateData_2012_Comp <- Salesconnect_RateData_2012 %>%
  group_by(RegionAccountNumber) %>%
  filter(Carrier != "KP") %>%
  summarize(CompRateMean = round(mean(T1MedicalRate, na.rm = TRUE),0)
            , CompRateLowest = round(min(T1MedicalRate, na.rm = TRUE),0)
            , CompCRateMean = round(mean(T1MonthlyContribution, na.rm = TRUE),0)
            , CompCRateLowest = round(min(T1MonthlyContribution, na.rm = TRUE),0)
            , CompDeductibleHighest = round(max(Deductible, na.rm = TRUE),0)
            , CompDeductibleLowest = round(min(Deductible, na.rm = TRUE),0)
            , CompDeductibleMean = round(mean(Deductible, na.rm = TRUE),0)
            , CompOopmHighest = round(max(OOPM, na.rm = TRUE),0)
            , CompOopmLowest = round(min(OOPM, na.rm = TRUE),0)
            , CompOopmMean = round(mean(OOPM, na.rm = TRUE),0)) %>%
  replace_with_na(replace = list(CompRateMean = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompRateLowest = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompCRateMean = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompCRateLowest = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleHighest = c("Inf", "-Inf","NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmHighest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmMean = c("Inf", "-Inf", "NaN")))

Salesconnect_RateData_2012_All <- Salesconnect_RateData_2012_KP %>%
  left_join(.,Salesconnect_RateData_2012_Comp, by = "RegionAccountNumber") %>%
  mutate(RP_Mean = round((CompRateMean/KPRateMean) - 1,2)) %>%
  mutate(RP_Lowest = round((CompRateLowest/KPRateLowest) - 1,2)) %>%
  mutate(CP_Mean = round(CompCRateMean - KPCRateMean,0)) %>%
  mutate(CP_Lowest = round(CompCRateLowest - KPCRateLowest,0))

Salesconnect_RateData_2012_final <- Salesconnect_RateData_2012 %>%
  left_join(.,Salesconnect_RateData_2012_All, by = c("RegionAccountNumber"))
  
remove(Salesconnect_RateData_2012_KP,Salesconnect_RateData_2012_Comp,Salesconnect_RateData_2012_All,Salesconnect_RateData_2012)

## Dropping variables no longer needed that was used to create the group level (GL) variables
## also dropping rows where the number of unique KP products == 0
## from the Salesfore data perspective, this means Kaiser was not offering
Salesconnect_RateData_2012_final <- Salesconnect_RateData_2012_final %>% 
  filter(GL_KPNumOfUniqueProducts != 0) %>%
  select(-PlanID, -OwnerName, -RenewalMonthNumber, -Type, -PlanName, -Exchange, -AccountRecordType, -ContractAccountNumber
         , -OpportunityName, -ThisPlanCovers, -Carrier, -Product, -OOPM, -Deductible, -PlanStatus, -Stage, -ContributionMethod
         , -T1MonthlyEmployerContribution, -FundingMechanism, -PaymentFrequency, -Primary, -RateType, -NetworkType, -ActualOfferingScenario
         , -OfferingScenario, -KPSoleCarrier, -TR_Slice, -AccountType, -T1MedicalRate, -T2MedicalRate, -T3MedicalRate, -T4MedicalRate
         , -T1MonthlyContribution, -Product_Chiro, -Product_DENTAL, -Product_DHMO, -Product_HDHP, -Product_HMO, -Product_Medicare
         , -Product_Other, -Product_POS, -Product_PPO, -Product_Vision, -Product_NA, -Aetna, -BCBS, -Other, -CalChoice, -Cigna
         , -KP, -HealthNet, -HMAA, -HMSA, -Humana, -Moda, -United, -Sharp, -Sutter, -UHA, -Dignity, -Carrier_NA, -FundingMechanism_Other
         , -FundingMechanism_FullyInsured, -FundingMechanism_None, -FundingMechanism_SelfFunded, -FundingMechanism_NA, -OfferingScenario_Slice
         , -OfferingScenario_Sole, -OfferingScenario_TR, -OfferingScenario_NA) %>%
  group_by(RegionAccountNumber) %>%
  filter(row_number(RegionAccountNumber) == 1)

## 2013 Rate, Deductible, OOPM by min, max, and mean. 
Salesconnect_RateData_2013 <- Salesconnect_RateData %>%
  filter(EffectiveDateYear == 2013)

Salesconnect_RateData_2013_KP <- Salesconnect_RateData_2013 %>%
  group_by(RegionAccountNumber) %>%
  filter(Carrier == "KP") %>%
  summarize(KPRateMean = round(mean(T1MedicalRate, na.rm = TRUE),0)
            , KPRateLowest = round(min(T1MedicalRate, na.rm = TRUE),0)
            , KPCRateMean = round(mean(T1MonthlyContribution, na.rm = TRUE),0)
            , KPCRateLowest = round(min(T1MonthlyContribution, na.rm = TRUE),0)
            , KPDeductibleHighest = round(max(Deductible, na.rm = TRUE),0)
            , KPDeductibleLowest = round(min(Deductible, na.rm = TRUE),0)
            , KPDeductibleMean = round(mean(Deductible, na.rm = TRUE),0)
            , KPOopmHighest = round(max(OOPM, na.rm = TRUE),0)
            , KPOopmLowest = round(min(OOPM, na.rm = TRUE),0)
            , KPOopmMean = round(mean(OOPM, na.rm = TRUE),0)) %>%
  replace_with_na(replace = list(KPRateMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPRateLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPCRateMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPCRateLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleHighest = c("Inf", "-Inf","NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmHighest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmMean = c("Inf", "-Inf", "NaN")))

Salesconnect_RateData_2013_Comp <- Salesconnect_RateData_2013 %>%
  group_by(RegionAccountNumber) %>%
  filter(Carrier != "KP") %>%
  summarize(CompRateMean = round(mean(T1MedicalRate, na.rm = TRUE),0)
            , CompRateLowest = round(min(T1MedicalRate, na.rm = TRUE),0)
            , CompCRateMean = round(mean(T1MonthlyContribution, na.rm = TRUE),0)
            , CompCRateLowest = round(min(T1MonthlyContribution, na.rm = TRUE),0)
            , CompDeductibleHighest = round(max(Deductible, na.rm = TRUE),0)
            , CompDeductibleLowest = round(min(Deductible, na.rm = TRUE),0)
            , CompDeductibleMean = round(mean(Deductible, na.rm = TRUE),0)
            , CompOopmHighest = round(max(OOPM, na.rm = TRUE),0)
            , CompOopmLowest = round(min(OOPM, na.rm = TRUE),0)
            , CompOopmMean = round(mean(OOPM, na.rm = TRUE),0)) %>%
  replace_with_na(replace = list(CompRateMean = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompRateLowest = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompCRateMean = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompCRateLowest = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleHighest = c("Inf", "-Inf","NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmHighest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmMean = c("Inf", "-Inf", "NaN")))

Salesconnect_RateData_2013_All <- Salesconnect_RateData_2013_KP %>%
  left_join(.,Salesconnect_RateData_2013_Comp, by = "RegionAccountNumber") %>%
  mutate(RP_Mean = round((CompRateMean/KPRateMean) - 1,2)) %>%
  mutate(RP_Lowest = round((CompRateLowest/KPRateLowest) - 1,2)) %>%
  mutate(CP_Mean = round(CompCRateMean - KPCRateMean,0)) %>%
  mutate(CP_Lowest = round(CompCRateLowest - KPCRateLowest,0))

Salesconnect_RateData_2013_final <- Salesconnect_RateData_2013 %>%
  left_join(.,Salesconnect_RateData_2013_All, by = c("RegionAccountNumber"))

remove(Salesconnect_RateData_2013_KP,Salesconnect_RateData_2013_Comp,Salesconnect_RateData_2013_All,Salesconnect_RateData_2013)

## Dropping variables no longer needed that was used to create the group level (GL) variables
## also dropping rows where the number of unique KP products == 0
## from the Salesfore data perspective, this means Kaiser was not offering
Salesconnect_RateData_2013_final <- Salesconnect_RateData_2013_final %>% 
  filter(GL_KPNumOfUniqueProducts != 0) %>%
  select(-PlanID, -OwnerName, -RenewalMonthNumber, -Type, -PlanName, -Exchange, -AccountRecordType, -ContractAccountNumber
         , -OpportunityName, -ThisPlanCovers, -Carrier, -Product, -OOPM, -Deductible, -PlanStatus, -Stage, -ContributionMethod
         , -T1MonthlyEmployerContribution, -FundingMechanism, -PaymentFrequency, -Primary, -RateType, -NetworkType, -ActualOfferingScenario
         , -OfferingScenario, -KPSoleCarrier, -TR_Slice, -AccountType, -T1MedicalRate, -T2MedicalRate, -T3MedicalRate, -T4MedicalRate
         , -T1MonthlyContribution, -Product_Chiro, -Product_DENTAL, -Product_DHMO, -Product_HDHP, -Product_HMO, -Product_Medicare
         , -Product_Other, -Product_POS, -Product_PPO, -Product_Vision, -Product_NA, -Aetna, -BCBS, -Other, -CalChoice, -Cigna
         , -KP, -HealthNet, -HMAA, -HMSA, -Humana, -Moda, -United, -Sharp, -Sutter, -UHA, -Dignity, -Carrier_NA, -FundingMechanism_Other
         , -FundingMechanism_FullyInsured, -FundingMechanism_None, -FundingMechanism_SelfFunded, -FundingMechanism_NA, -OfferingScenario_Slice
         , -OfferingScenario_Sole, -OfferingScenario_TR, -OfferingScenario_NA) %>%
  group_by(RegionAccountNumber) %>%
  filter(row_number(RegionAccountNumber) == 1)

## 2014 Rate, Deductible, OOPM by min, max, and mean. 
Salesconnect_RateData_2014 <- Salesconnect_RateData %>%
  filter(EffectiveDateYear == 2014)

Salesconnect_RateData_2014_KP <- Salesconnect_RateData_2014 %>%
  group_by(RegionAccountNumber) %>%
  filter(Carrier == "KP") %>%
  summarize(KPRateMean = round(mean(T1MedicalRate, na.rm = TRUE),0)
            , KPRateLowest = round(min(T1MedicalRate, na.rm = TRUE),0)
            , KPCRateMean = round(mean(T1MonthlyContribution, na.rm = TRUE),0)
            , KPCRateLowest = round(min(T1MonthlyContribution, na.rm = TRUE),0)
            , KPDeductibleHighest = round(max(Deductible, na.rm = TRUE),0)
            , KPDeductibleLowest = round(min(Deductible, na.rm = TRUE),0)
            , KPDeductibleMean = round(mean(Deductible, na.rm = TRUE),0)
            , KPOopmHighest = round(max(OOPM, na.rm = TRUE),0)
            , KPOopmLowest = round(min(OOPM, na.rm = TRUE),0)
            , KPOopmMean = round(mean(OOPM, na.rm = TRUE),0)) %>%
  replace_with_na(replace = list(KPRateMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPRateLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPCRateMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPCRateLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleHighest = c("Inf", "-Inf","NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmHighest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmMean = c("Inf", "-Inf", "NaN")))

Salesconnect_RateData_2014_Comp <- Salesconnect_RateData_2014 %>%
  group_by(RegionAccountNumber) %>%
  filter(Carrier != "KP") %>%
  summarize(CompRateMean = round(mean(T1MedicalRate, na.rm = TRUE),0)
            , CompRateLowest = round(min(T1MedicalRate, na.rm = TRUE),0)
            , CompCRateMean = round(mean(T1MonthlyContribution, na.rm = TRUE),0)
            , CompCRateLowest = round(min(T1MonthlyContribution, na.rm = TRUE),0)
            , CompDeductibleHighest = round(max(Deductible, na.rm = TRUE),0)
            , CompDeductibleLowest = round(min(Deductible, na.rm = TRUE),0)
            , CompDeductibleMean = round(mean(Deductible, na.rm = TRUE),0)
            , CompOopmHighest = round(max(OOPM, na.rm = TRUE),0)
            , CompOopmLowest = round(min(OOPM, na.rm = TRUE),0)
            , CompOopmMean = round(mean(OOPM, na.rm = TRUE),0)) %>%
  replace_with_na(replace = list(CompRateMean = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompRateLowest = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompCRateMean = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompCRateLowest = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleHighest = c("Inf", "-Inf","NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmHighest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmMean = c("Inf", "-Inf", "NaN")))

Salesconnect_RateData_2014_All <- Salesconnect_RateData_2014_KP %>%
  left_join(.,Salesconnect_RateData_2014_Comp, by = "RegionAccountNumber") %>%
  mutate(RP_Mean = round((CompRateMean/KPRateMean) - 1,2)) %>%
  mutate(RP_Lowest = round((CompRateLowest/KPRateLowest) - 1,2)) %>%
  mutate(CP_Mean = round(CompCRateMean - KPCRateMean,0)) %>%
  mutate(CP_Lowest = round(CompCRateLowest - KPCRateLowest,0))

Salesconnect_RateData_2014_final <- Salesconnect_RateData_2014 %>%
  left_join(.,Salesconnect_RateData_2014_All, by = c("RegionAccountNumber"))

remove(Salesconnect_RateData_2014_KP,Salesconnect_RateData_2014_Comp,Salesconnect_RateData_2014_All,Salesconnect_RateData_2014)

## Dropping variables no longer needed that was used to create the group level (GL) variables
## also dropping rows where the number of unique KP products == 0
## from the Salesfore data perspective, this means Kaiser was not offering
Salesconnect_RateData_2014_final <- Salesconnect_RateData_2014_final %>% 
  filter(GL_KPNumOfUniqueProducts != 0) %>%
  select(-PlanID, -OwnerName, -RenewalMonthNumber, -Type, -PlanName, -Exchange, -AccountRecordType, -ContractAccountNumber
         , -OpportunityName, -ThisPlanCovers, -Carrier, -Product, -OOPM, -Deductible, -PlanStatus, -Stage, -ContributionMethod
         , -T1MonthlyEmployerContribution, -FundingMechanism, -PaymentFrequency, -Primary, -RateType, -NetworkType, -ActualOfferingScenario
         , -OfferingScenario, -KPSoleCarrier, -TR_Slice, -AccountType, -T1MedicalRate, -T2MedicalRate, -T3MedicalRate, -T4MedicalRate
         , -T1MonthlyContribution, -Product_Chiro, -Product_DENTAL, -Product_DHMO, -Product_HDHP, -Product_HMO, -Product_Medicare
         , -Product_Other, -Product_POS, -Product_PPO, -Product_Vision, -Product_NA, -Aetna, -BCBS, -Other, -CalChoice, -Cigna
         , -KP, -HealthNet, -HMAA, -HMSA, -Humana, -Moda, -United, -Sharp, -Sutter, -UHA, -Dignity, -Carrier_NA, -FundingMechanism_Other
         , -FundingMechanism_FullyInsured, -FundingMechanism_None, -FundingMechanism_SelfFunded, -FundingMechanism_NA, -OfferingScenario_Slice
         , -OfferingScenario_Sole, -OfferingScenario_TR, -OfferingScenario_NA) %>%
  group_by(RegionAccountNumber) %>%
  filter(row_number(RegionAccountNumber) == 1)

## 2015 Rate, Deductible, OOPM by min, max, and mean. 
Salesconnect_RateData_2015 <- Salesconnect_RateData %>%
  filter(EffectiveDateYear == 2015)

Salesconnect_RateData_2015_KP <- Salesconnect_RateData_2015 %>%
  group_by(RegionAccountNumber) %>%
  filter(Carrier == "KP") %>%
  summarize(KPRateMean = round(mean(T1MedicalRate, na.rm = TRUE),0)
            , KPRateLowest = round(min(T1MedicalRate, na.rm = TRUE),0)
            , KPCRateMean = round(mean(T1MonthlyContribution, na.rm = TRUE),0)
            , KPCRateLowest = round(min(T1MonthlyContribution, na.rm = TRUE),0)
            , KPDeductibleHighest = round(max(Deductible, na.rm = TRUE),0)
            , KPDeductibleLowest = round(min(Deductible, na.rm = TRUE),0)
            , KPDeductibleMean = round(mean(Deductible, na.rm = TRUE),0)
            , KPOopmHighest = round(max(OOPM, na.rm = TRUE),0)
            , KPOopmLowest = round(min(OOPM, na.rm = TRUE),0)
            , KPOopmMean = round(mean(OOPM, na.rm = TRUE),0)) %>%
  replace_with_na(replace = list(KPRateMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPRateLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPCRateMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPCRateLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleHighest = c("Inf", "-Inf","NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmHighest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmMean = c("Inf", "-Inf", "NaN")))

Salesconnect_RateData_2015_Comp <- Salesconnect_RateData_2015 %>%
  group_by(RegionAccountNumber) %>%
  filter(Carrier != "KP") %>%
  summarize(CompRateMean = round(mean(T1MedicalRate, na.rm = TRUE),0)
            , CompRateLowest = round(min(T1MedicalRate, na.rm = TRUE),0)
            , CompCRateMean = round(mean(T1MonthlyContribution, na.rm = TRUE),0)
            , CompCRateLowest = round(min(T1MonthlyContribution, na.rm = TRUE),0)
            , CompDeductibleHighest = round(max(Deductible, na.rm = TRUE),0)
            , CompDeductibleLowest = round(min(Deductible, na.rm = TRUE),0)
            , CompDeductibleMean = round(mean(Deductible, na.rm = TRUE),0)
            , CompOopmHighest = round(max(OOPM, na.rm = TRUE),0)
            , CompOopmLowest = round(min(OOPM, na.rm = TRUE),0)
            , CompOopmMean = round(mean(OOPM, na.rm = TRUE),0)) %>%
  replace_with_na(replace = list(CompRateMean = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompRateLowest = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompCRateMean = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompCRateLowest = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleHighest = c("Inf", "-Inf","NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmHighest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmMean = c("Inf", "-Inf", "NaN")))

Salesconnect_RateData_2015_All <- Salesconnect_RateData_2015_KP %>%
  left_join(.,Salesconnect_RateData_2015_Comp, by = "RegionAccountNumber") %>%
  mutate(RP_Mean = round((CompRateMean/KPRateMean) - 1,2)) %>%
  mutate(RP_Lowest = round((CompRateLowest/KPRateLowest) - 1,2)) %>%
  mutate(CP_Mean = round(CompCRateMean - KPCRateMean,0)) %>%
  mutate(CP_Lowest = round(CompCRateLowest - KPCRateLowest,0))

Salesconnect_RateData_2015_final <- Salesconnect_RateData_2015 %>%
  left_join(.,Salesconnect_RateData_2015_All, by = c("RegionAccountNumber"))

remove(Salesconnect_RateData_2015_KP,Salesconnect_RateData_2015_Comp,Salesconnect_RateData_2015_All,Salesconnect_RateData_2015)

## Dropping variables no longer needed that was used to create the group level (GL) variables
## also dropping rows where the number of unique KP products == 0
## from the Salesfore data perspective, this means Kaiser was not offering
Salesconnect_RateData_2015_final <- Salesconnect_RateData_2015_final %>% 
  filter(GL_KPNumOfUniqueProducts != 0) %>%
  select(-PlanID, -OwnerName, -RenewalMonthNumber, -Type, -PlanName, -Exchange, -AccountRecordType, -ContractAccountNumber
         , -OpportunityName, -ThisPlanCovers, -Carrier, -Product, -OOPM, -Deductible, -PlanStatus, -Stage, -ContributionMethod
         , -T1MonthlyEmployerContribution, -FundingMechanism, -PaymentFrequency, -Primary, -RateType, -NetworkType, -ActualOfferingScenario
         , -OfferingScenario, -KPSoleCarrier, -TR_Slice, -AccountType, -T1MedicalRate, -T2MedicalRate, -T3MedicalRate, -T4MedicalRate
         , -T1MonthlyContribution, -Product_Chiro, -Product_DENTAL, -Product_DHMO, -Product_HDHP, -Product_HMO, -Product_Medicare
         , -Product_Other, -Product_POS, -Product_PPO, -Product_Vision, -Product_NA, -Aetna, -BCBS, -Other, -CalChoice, -Cigna
         , -KP, -HealthNet, -HMAA, -HMSA, -Humana, -Moda, -United, -Sharp, -Sutter, -UHA, -Dignity, -Carrier_NA, -FundingMechanism_Other
         , -FundingMechanism_FullyInsured, -FundingMechanism_None, -FundingMechanism_SelfFunded, -FundingMechanism_NA, -OfferingScenario_Slice
         , -OfferingScenario_Sole, -OfferingScenario_TR, -OfferingScenario_NA) %>%
  group_by(RegionAccountNumber) %>%
  filter(row_number(RegionAccountNumber) == 1)

## 2016 Rate, Deductible, OOPM by min, max, and mean. 
Salesconnect_RateData_2016 <- Salesconnect_RateData %>%
  filter(EffectiveDateYear == 2016)

Salesconnect_RateData_2016_KP <- Salesconnect_RateData_2016 %>%
  group_by(RegionAccountNumber) %>%
  filter(Carrier == "KP") %>%
  summarize(KPRateMean = round(mean(T1MedicalRate, na.rm = TRUE),0)
            , KPRateLowest = round(min(T1MedicalRate, na.rm = TRUE),0)
            , KPCRateMean = round(mean(T1MonthlyContribution, na.rm = TRUE),0)
            , KPCRateLowest = round(min(T1MonthlyContribution, na.rm = TRUE),0)
            , KPDeductibleHighest = round(max(Deductible, na.rm = TRUE),0)
            , KPDeductibleLowest = round(min(Deductible, na.rm = TRUE),0)
            , KPDeductibleMean = round(mean(Deductible, na.rm = TRUE),0)
            , KPOopmHighest = round(max(OOPM, na.rm = TRUE),0)
            , KPOopmLowest = round(min(OOPM, na.rm = TRUE),0)
            , KPOopmMean = round(mean(OOPM, na.rm = TRUE),0)) %>%
  replace_with_na(replace = list(KPRateMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPRateLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPCRateMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPCRateLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleHighest = c("Inf", "-Inf","NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmHighest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmMean = c("Inf", "-Inf", "NaN")))

Salesconnect_RateData_2016_Comp <- Salesconnect_RateData_2016 %>%
  group_by(RegionAccountNumber) %>%
  filter(Carrier != "KP") %>%
  summarize(CompRateMean = round(mean(T1MedicalRate, na.rm = TRUE),0)
            , CompRateLowest = round(min(T1MedicalRate, na.rm = TRUE),0)
            , CompCRateMean = round(mean(T1MonthlyContribution, na.rm = TRUE),0)
            , CompCRateLowest = round(min(T1MonthlyContribution, na.rm = TRUE),0)
            , CompDeductibleHighest = round(max(Deductible, na.rm = TRUE),0)
            , CompDeductibleLowest = round(min(Deductible, na.rm = TRUE),0)
            , CompDeductibleMean = round(mean(Deductible, na.rm = TRUE),0)
            , CompOopmHighest = round(max(OOPM, na.rm = TRUE),0)
            , CompOopmLowest = round(min(OOPM, na.rm = TRUE),0)
            , CompOopmMean = round(mean(OOPM, na.rm = TRUE),0)) %>%
  replace_with_na(replace = list(CompRateMean = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompRateLowest = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompCRateMean = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompCRateLowest = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleHighest = c("Inf", "-Inf","NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmHighest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmMean = c("Inf", "-Inf", "NaN")))

Salesconnect_RateData_2016_All <- Salesconnect_RateData_2016_KP %>%
  left_join(.,Salesconnect_RateData_2016_Comp, by = "RegionAccountNumber") %>%
  mutate(RP_Mean = round((CompRateMean/KPRateMean) - 1,2)) %>%
  mutate(RP_Lowest = round((CompRateLowest/KPRateLowest) - 1,2)) %>%
  mutate(CP_Mean = round(CompCRateMean - KPCRateMean,0)) %>%
  mutate(CP_Lowest = round(CompCRateLowest - KPCRateLowest,0))

Salesconnect_RateData_2016_final <- Salesconnect_RateData_2016 %>%
  left_join(.,Salesconnect_RateData_2016_All, by = c("RegionAccountNumber"))

remove(Salesconnect_RateData_2016_KP,Salesconnect_RateData_2016_Comp,Salesconnect_RateData_2016_All,Salesconnect_RateData_2016)

## Dropping variables no longer needed that was used to create the group level (GL) variables
## also dropping rows where the number of unique KP products == 0
## from the Salesfore data perspective, this means Kaiser was not offering
Salesconnect_RateData_2016_final <- Salesconnect_RateData_2016_final %>% 
  filter(GL_KPNumOfUniqueProducts != 0) %>%
  select(-PlanID, -OwnerName, -RenewalMonthNumber, -Type, -PlanName, -Exchange, -AccountRecordType, -ContractAccountNumber
         , -OpportunityName, -ThisPlanCovers, -Carrier, -Product, -OOPM, -Deductible, -PlanStatus, -Stage, -ContributionMethod
         , -T1MonthlyEmployerContribution, -FundingMechanism, -PaymentFrequency, -Primary, -RateType, -NetworkType, -ActualOfferingScenario
         , -OfferingScenario, -KPSoleCarrier, -TR_Slice, -AccountType, -T1MedicalRate, -T2MedicalRate, -T3MedicalRate, -T4MedicalRate
         , -T1MonthlyContribution, -Product_Chiro, -Product_DENTAL, -Product_DHMO, -Product_HDHP, -Product_HMO, -Product_Medicare
         , -Product_Other, -Product_POS, -Product_PPO, -Product_Vision, -Product_NA, -Aetna, -BCBS, -Other, -CalChoice, -Cigna
         , -KP, -HealthNet, -HMAA, -HMSA, -Humana, -Moda, -United, -Sharp, -Sutter, -UHA, -Dignity, -Carrier_NA, -FundingMechanism_Other
         , -FundingMechanism_FullyInsured, -FundingMechanism_None, -FundingMechanism_SelfFunded, -FundingMechanism_NA, -OfferingScenario_Slice
         , -OfferingScenario_Sole, -OfferingScenario_TR, -OfferingScenario_NA) %>%
  group_by(RegionAccountNumber) %>%
  filter(row_number(RegionAccountNumber) == 1)

## 2017 Rate, Deductible, OOPM by min, max, and mean. 
Salesconnect_RateData_2017 <- Salesconnect_RateData %>%
  filter(EffectiveDateYear == 2017)

Salesconnect_RateData_2017_KP <- Salesconnect_RateData_2017 %>%
  group_by(RegionAccountNumber) %>%
  filter(Carrier == "KP") %>%
  summarize(KPRateMean = round(mean(T1MedicalRate, na.rm = TRUE),0)
            , KPRateLowest = round(min(T1MedicalRate, na.rm = TRUE),0)
            , KPCRateMean = round(mean(T1MonthlyContribution, na.rm = TRUE),0)
            , KPCRateLowest = round(min(T1MonthlyContribution, na.rm = TRUE),0)
            , KPDeductibleHighest = round(max(Deductible, na.rm = TRUE),0)
            , KPDeductibleLowest = round(min(Deductible, na.rm = TRUE),0)
            , KPDeductibleMean = round(mean(Deductible, na.rm = TRUE),0)
            , KPOopmHighest = round(max(OOPM, na.rm = TRUE),0)
            , KPOopmLowest = round(min(OOPM, na.rm = TRUE),0)
            , KPOopmMean = round(mean(OOPM, na.rm = TRUE),0)) %>%
  replace_with_na(replace = list(KPRateMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPRateLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPCRateMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPCRateLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleHighest = c("Inf", "-Inf","NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmHighest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmMean = c("Inf", "-Inf", "NaN")))

Salesconnect_RateData_2017_Comp <- Salesconnect_RateData_2017 %>%
  group_by(RegionAccountNumber) %>%
  filter(Carrier != "KP") %>%
  summarize(CompRateMean = round(mean(T1MedicalRate, na.rm = TRUE),0)
            , CompRateLowest = round(min(T1MedicalRate, na.rm = TRUE),0)
            , CompCRateMean = round(mean(T1MonthlyContribution, na.rm = TRUE),0)
            , CompCRateLowest = round(min(T1MonthlyContribution, na.rm = TRUE),0)
            , CompDeductibleHighest = round(max(Deductible, na.rm = TRUE),0)
            , CompDeductibleLowest = round(min(Deductible, na.rm = TRUE),0)
            , CompDeductibleMean = round(mean(Deductible, na.rm = TRUE),0)
            , CompOopmHighest = round(max(OOPM, na.rm = TRUE),0)
            , CompOopmLowest = round(min(OOPM, na.rm = TRUE),0)
            , CompOopmMean = round(mean(OOPM, na.rm = TRUE),0)) %>%
  replace_with_na(replace = list(CompRateMean = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompRateLowest = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompCRateMean = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompCRateLowest = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleHighest = c("Inf", "-Inf","NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmHighest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmMean = c("Inf", "-Inf", "NaN")))

Salesconnect_RateData_2017_All <- Salesconnect_RateData_2017_KP %>%
  left_join(.,Salesconnect_RateData_2017_Comp, by = "RegionAccountNumber") %>%
  mutate(RP_Mean = round((CompRateMean/KPRateMean) - 1,2)) %>%
  mutate(RP_Lowest = round((CompRateLowest/KPRateLowest) - 1,2)) %>%
  mutate(CP_Mean = round(CompCRateMean - KPCRateMean,0)) %>%
  mutate(CP_Lowest = round(CompCRateLowest - KPCRateLowest,0))

Salesconnect_RateData_2017_final <- Salesconnect_RateData_2017 %>%
  left_join(.,Salesconnect_RateData_2017_All, by = c("RegionAccountNumber"))

remove(Salesconnect_RateData_2017_KP,Salesconnect_RateData_2017_Comp,Salesconnect_RateData_2017_All,Salesconnect_RateData_2017)

## Dropping variables no longer needed that was used to create the group level (GL) variables
## also dropping rows where the number of unique KP products == 0
## from the Salesfore data perspective, this means Kaiser was not offering
Salesconnect_RateData_2017_final <- Salesconnect_RateData_2017_final %>% 
  filter(GL_KPNumOfUniqueProducts != 0) %>%
  select(-PlanID, -OwnerName, -RenewalMonthNumber, -Type, -PlanName, -Exchange, -AccountRecordType, -ContractAccountNumber
         , -OpportunityName, -ThisPlanCovers, -Carrier, -Product, -OOPM, -Deductible, -PlanStatus, -Stage, -ContributionMethod
         , -T1MonthlyEmployerContribution, -FundingMechanism, -PaymentFrequency, -Primary, -RateType, -NetworkType, -ActualOfferingScenario
         , -OfferingScenario, -KPSoleCarrier, -TR_Slice, -AccountType, -T1MedicalRate, -T2MedicalRate, -T3MedicalRate, -T4MedicalRate
         , -T1MonthlyContribution, -Product_Chiro, -Product_DENTAL, -Product_DHMO, -Product_HDHP, -Product_HMO, -Product_Medicare
         , -Product_Other, -Product_POS, -Product_PPO, -Product_Vision, -Product_NA, -Aetna, -BCBS, -Other, -CalChoice, -Cigna
         , -KP, -HealthNet, -HMAA, -HMSA, -Humana, -Moda, -United, -Sharp, -Sutter, -UHA, -Dignity, -Carrier_NA, -FundingMechanism_Other
         , -FundingMechanism_FullyInsured, -FundingMechanism_None, -FundingMechanism_SelfFunded, -FundingMechanism_NA, -OfferingScenario_Slice
         , -OfferingScenario_Sole, -OfferingScenario_TR, -OfferingScenario_NA) %>%
  group_by(RegionAccountNumber) %>%
  filter(row_number(RegionAccountNumber) == 1)

## 2018 Rate, Deductible, OOPM by min, max, and mean. 
Salesconnect_RateData_2018 <- Salesconnect_RateData %>%
  filter(EffectiveDateYear == 2018)

Salesconnect_RateData_2018_KP <- Salesconnect_RateData_2018 %>%
  group_by(RegionAccountNumber) %>%
  filter(Carrier == "KP") %>%
  summarize(KPRateMean = round(mean(T1MedicalRate, na.rm = TRUE),0)
            , KPRateLowest = round(min(T1MedicalRate, na.rm = TRUE),0)
            , KPCRateMean = round(mean(T1MonthlyContribution, na.rm = TRUE),0)
            , KPCRateLowest = round(min(T1MonthlyContribution, na.rm = TRUE),0)
            , KPDeductibleHighest = round(max(Deductible, na.rm = TRUE),0)
            , KPDeductibleLowest = round(min(Deductible, na.rm = TRUE),0)
            , KPDeductibleMean = round(mean(Deductible, na.rm = TRUE),0)
            , KPOopmHighest = round(max(OOPM, na.rm = TRUE),0)
            , KPOopmLowest = round(min(OOPM, na.rm = TRUE),0)
            , KPOopmMean = round(mean(OOPM, na.rm = TRUE),0)) %>%
  replace_with_na(replace = list(KPRateMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPRateLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPCRateMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPCRateLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleHighest = c("Inf", "-Inf","NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmHighest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmMean = c("Inf", "-Inf", "NaN")))

Salesconnect_RateData_2018_Comp <- Salesconnect_RateData_2018 %>%
  group_by(RegionAccountNumber) %>%
  filter(Carrier != "KP") %>%
  summarize(CompRateMean = round(mean(T1MedicalRate, na.rm = TRUE),0)
            , CompRateLowest = round(min(T1MedicalRate, na.rm = TRUE),0)
            , CompCRateMean = round(mean(T1MonthlyContribution, na.rm = TRUE),0)
            , CompCRateLowest = round(min(T1MonthlyContribution, na.rm = TRUE),0)
            , CompDeductibleHighest = round(max(Deductible, na.rm = TRUE),0)
            , CompDeductibleLowest = round(min(Deductible, na.rm = TRUE),0)
            , CompDeductibleMean = round(mean(Deductible, na.rm = TRUE),0)
            , CompOopmHighest = round(max(OOPM, na.rm = TRUE),0)
            , CompOopmLowest = round(min(OOPM, na.rm = TRUE),0)
            , CompOopmMean = round(mean(OOPM, na.rm = TRUE),0)) %>%
  replace_with_na(replace = list(CompRateMean = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompRateLowest = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompCRateMean = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompCRateLowest = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleHighest = c("Inf", "-Inf","NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmHighest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmMean = c("Inf", "-Inf", "NaN")))

Salesconnect_RateData_2018_All <- Salesconnect_RateData_2018_KP %>%
  left_join(.,Salesconnect_RateData_2018_Comp, by = "RegionAccountNumber") %>%
  mutate(RP_Mean = round((CompRateMean/KPRateMean) - 1,2)) %>%
  mutate(RP_Lowest = round((CompRateLowest/KPRateLowest) - 1,2)) %>%
  mutate(CP_Mean = round(CompCRateMean - KPCRateMean,0)) %>%
  mutate(CP_Lowest = round(CompCRateLowest - KPCRateLowest,0))

Salesconnect_RateData_2018_final <- Salesconnect_RateData_2018 %>%
  left_join(.,Salesconnect_RateData_2018_All, by = c("RegionAccountNumber"))

remove(Salesconnect_RateData_2018_KP,Salesconnect_RateData_2018_Comp,Salesconnect_RateData_2018_All,Salesconnect_RateData_2018)

## Dropping variables no longer needed that was used to create the group level (GL) variables
## also dropping rows where the number of unique KP products == 0
## from the Salesfore data perspective, this means Kaiser was not offering
Salesconnect_RateData_2018_final <- Salesconnect_RateData_2018_final %>% 
  filter(GL_KPNumOfUniqueProducts != 0) %>%
  select(-PlanID, -OwnerName, -RenewalMonthNumber, -Type, -PlanName, -Exchange, -AccountRecordType, -ContractAccountNumber
         , -OpportunityName, -ThisPlanCovers, -Carrier, -Product, -OOPM, -Deductible, -PlanStatus, -Stage, -ContributionMethod
         , -T1MonthlyEmployerContribution, -FundingMechanism, -PaymentFrequency, -Primary, -RateType, -NetworkType, -ActualOfferingScenario
         , -OfferingScenario, -KPSoleCarrier, -TR_Slice, -AccountType, -T1MedicalRate, -T2MedicalRate, -T3MedicalRate, -T4MedicalRate
         , -T1MonthlyContribution, -Product_Chiro, -Product_DENTAL, -Product_DHMO, -Product_HDHP, -Product_HMO, -Product_Medicare
         , -Product_Other, -Product_POS, -Product_PPO, -Product_Vision, -Product_NA, -Aetna, -BCBS, -Other, -CalChoice, -Cigna
         , -KP, -HealthNet, -HMAA, -HMSA, -Humana, -Moda, -United, -Sharp, -Sutter, -UHA, -Dignity, -Carrier_NA, -FundingMechanism_Other
         , -FundingMechanism_FullyInsured, -FundingMechanism_None, -FundingMechanism_SelfFunded, -FundingMechanism_NA, -OfferingScenario_Slice
         , -OfferingScenario_Sole, -OfferingScenario_TR, -OfferingScenario_NA) %>%
  group_by(RegionAccountNumber) %>%
  filter(row_number(RegionAccountNumber) == 1)

## 2019 Rate, Deductible, OOPM by min, max, and mean. 
Salesconnect_RateData_2019 <- Salesconnect_RateData %>%
  filter(EffectiveDateYear == 2019)

Salesconnect_RateData_2019_KP <- Salesconnect_RateData_2019 %>%
  group_by(RegionAccountNumber) %>%
  filter(Carrier == "KP") %>%
  summarize(KPRateMean = round(mean(T1MedicalRate, na.rm = TRUE),0)
            , KPRateLowest = round(min(T1MedicalRate, na.rm = TRUE),0)
            , KPCRateMean = round(mean(T1MonthlyContribution, na.rm = TRUE),0)
            , KPCRateLowest = round(min(T1MonthlyContribution, na.rm = TRUE),0)
            , KPDeductibleHighest = round(max(Deductible, na.rm = TRUE),0)
            , KPDeductibleLowest = round(min(Deductible, na.rm = TRUE),0)
            , KPDeductibleMean = round(mean(Deductible, na.rm = TRUE),0)
            , KPOopmHighest = round(max(OOPM, na.rm = TRUE),0)
            , KPOopmLowest = round(min(OOPM, na.rm = TRUE),0)
            , KPOopmMean = round(mean(OOPM, na.rm = TRUE),0)) %>%
  replace_with_na(replace = list(KPRateMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPRateLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPCRateMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPCRateLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleHighest = c("Inf", "-Inf","NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPDeductibleMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmHighest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(KPOopmMean = c("Inf", "-Inf", "NaN")))

Salesconnect_RateData_2019_Comp <- Salesconnect_RateData_2019 %>%
  group_by(RegionAccountNumber) %>%
  filter(Carrier != "KP") %>%
  summarize(CompRateMean = round(mean(T1MedicalRate, na.rm = TRUE),0)
            , CompRateLowest = round(min(T1MedicalRate, na.rm = TRUE),0)
            , CompCRateMean = round(mean(T1MonthlyContribution, na.rm = TRUE),0)
            , CompCRateLowest = round(min(T1MonthlyContribution, na.rm = TRUE),0)
            , CompDeductibleHighest = round(max(Deductible, na.rm = TRUE),0)
            , CompDeductibleLowest = round(min(Deductible, na.rm = TRUE),0)
            , CompDeductibleMean = round(mean(Deductible, na.rm = TRUE),0)
            , CompOopmHighest = round(max(OOPM, na.rm = TRUE),0)
            , CompOopmLowest = round(min(OOPM, na.rm = TRUE),0)
            , CompOopmMean = round(mean(OOPM, na.rm = TRUE),0)) %>%
  replace_with_na(replace = list(CompRateMean = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompRateLowest = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompCRateMean = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompCRateLowest = c("Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleHighest = c("Inf", "-Inf","NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompDeductibleMean = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmHighest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmLowest = c("Inf", "-Inf", "NaN"))) %>%
  replace_with_na(replace = list(CompOopmMean = c("Inf", "-Inf", "NaN")))

Salesconnect_RateData_2019_All <- Salesconnect_RateData_2019_KP %>%
  left_join(.,Salesconnect_RateData_2019_Comp, by = "RegionAccountNumber") %>%
  mutate(RP_Mean = round((CompRateMean/KPRateMean) - 1,2)) %>%
  mutate(RP_Lowest = round((CompRateLowest/KPRateLowest) - 1,2)) %>%
  mutate(CP_Mean = round(CompCRateMean - KPCRateMean,0)) %>%
  mutate(CP_Lowest = round(CompCRateLowest - KPCRateLowest,0))

Salesconnect_RateData_2019_final <- Salesconnect_RateData_2019 %>%
  left_join(.,Salesconnect_RateData_2019_All, by = c("RegionAccountNumber"))

remove(Salesconnect_RateData_2019_KP,Salesconnect_RateData_2019_Comp,Salesconnect_RateData_2019_All,Salesconnect_RateData_2019)

## Dropping variables no longer needed that was used to create the group level (GL) variables
## also dropping rows where the number of unique KP products == 0
## from the Salesfore data perspective, this means Kaiser was not offering
Salesconnect_RateData_2019_final <- Salesconnect_RateData_2019_final %>% 
  filter(GL_KPNumOfUniqueProducts != 0) %>%
  select(-PlanID, -OwnerName, -RenewalMonthNumber, -Type, -PlanName, -Exchange, -AccountRecordType, -ContractAccountNumber
         , -OpportunityName, -ThisPlanCovers, -Carrier, -Product, -OOPM, -Deductible, -PlanStatus, -Stage, -ContributionMethod
         , -T1MonthlyEmployerContribution, -FundingMechanism, -PaymentFrequency, -Primary, -RateType, -NetworkType, -ActualOfferingScenario
         , -OfferingScenario, -KPSoleCarrier, -TR_Slice, -AccountType, -T1MedicalRate, -T2MedicalRate, -T3MedicalRate, -T4MedicalRate
         , -T1MonthlyContribution, -Product_Chiro, -Product_DENTAL, -Product_DHMO, -Product_HDHP, -Product_HMO, -Product_Medicare
         , -Product_Other, -Product_POS, -Product_PPO, -Product_Vision, -Product_NA, -Aetna, -BCBS, -Other, -CalChoice, -Cigna
         , -KP, -HealthNet, -HMAA, -HMSA, -Humana, -Moda, -United, -Sharp, -Sutter, -UHA, -Dignity, -Carrier_NA, -FundingMechanism_Other
         , -FundingMechanism_FullyInsured, -FundingMechanism_None, -FundingMechanism_SelfFunded, -FundingMechanism_NA, -OfferingScenario_Slice
         , -OfferingScenario_Sole, -OfferingScenario_TR, -OfferingScenario_NA) %>%
  group_by(RegionAccountNumber) %>%
  filter(row_number(RegionAccountNumber) == 1)

### bind all years together
Salesconnect_RateData_AllYears_final <- rbind(Salesconnect_RateData_2012_final, Salesconnect_RateData_2013_final, Salesconnect_RateData_2014_final, Salesconnect_RateData_2015_final
      , Salesconnect_RateData_2016_final, Salesconnect_RateData_2017_final, Salesconnect_RateData_2018_final, Salesconnect_RateData_2019_final)

Salesconnect_RateData_AllYears_final <- Salesconnect_RateData_AllYears_final %>%
  group_by(RegionAccountNumber, EffectiveDate) %>%
  arrange(RegionAccountNumber, EffectiveDate)

##### Export Datasets
write.csv(Salesconnect_RateData_2012_final, "Y:\\Predictive Analytics\\Datasets\\SalesConnect\\Salesconnect_RateData_2012.csv")
write.csv(Salesconnect_RateData_2013_final, "Y:\\Predictive Analytics\\Datasets\\SalesConnect\\Salesconnect_RateData_2013.csv")
write.csv(Salesconnect_RateData_2014_final, "Y:\\Predictive Analytics\\Datasets\\SalesConnect\\Salesconnect_RateData_2014.csv")
write.csv(Salesconnect_RateData_2015_final, "Y:\\Predictive Analytics\\Datasets\\SalesConnect\\Salesconnect_RateData_2015.csv")
write.csv(Salesconnect_RateData_2016_final, "Y:\\Predictive Analytics\\Datasets\\SalesConnect\\Salesconnect_RateData_2016.csv")
write.csv(Salesconnect_RateData_2017_final, "Y:\\Predictive Analytics\\Datasets\\SalesConnect\\Salesconnect_RateData_2017.csv")
write.csv(Salesconnect_RateData_2018_final, "Y:\\Predictive Analytics\\Datasets\\SalesConnect\\Salesconnect_RateData_2018.csv")
write.csv(Salesconnect_RateData_2019_final, "Y:\\Predictive Analytics\\Datasets\\SalesConnect\\Salesconnect_RateData_2019.csv")
write.csv(Salesconnect_RateData_AllYears_final, "Y:\\Predictive Analytics\\Datasets\\SalesConnect\\Salesconnect_RateData_AllYears.csv")

