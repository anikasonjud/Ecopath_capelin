library(tidyverse)             
library(mfdb)
# remotes::install_github("gadget-framework/mfdb", ref = '7.x')
mdb<-mfdb('Iceland',db_params=list(host='mfdb.hafro.is'))
library(Rpath)
library(mar)
mar<-connect_mar()
library(data.table)
source("species_list.R")

# Configure functional groups
start_year <- list("1996" = 1996)
grouping_area <- list(area = NULL)  # We don't care, give us the whole area
#grouping_area <- list(area = mfdb_group(south = c( 1146, 1094, 1091, 1151, 1142, 1092, 1101, 1143, 1141, 1144, 1093, 1145, 1015, 1082, 1132, 1061, 1071, 1081, 1012, 1095, 1054, 1011, 1053, 1051, 1013, 1014), north = c(1021, 1131, 1052, 1031, 1022, 1023, 1041, 1032, 1133, 1042, 1112, 1111, 1121)))
#stanza species
grouping_FCD <- list(sampling_type ="IGFS-IND", year = "1996", predator_species = FCD,species = FCD,length = mfdb_group(FCD.juv=0:45, FCD.adult=46:200))
grouping_FHA <- list(year = "1996",predator_species = FHA,species = FHA, length = mfdb_group(FHA.juv=0:30, FHA.adult=31:120))
grouping_FSA <- list(year = "1996",predator_species = FSA,species = FSA, length = mfdb_group(FSA.juv=0:48, FSA.adult=49:120))

grouping_FCD_catch <- list( year = "1996",predator_species  = mfdb_group(FCD = FCD),species = mfdb_group(FCD = FCD), age = NULL)   
grouping_FHA_catch <- list( year = "1996",predator_species  = mfdb_group(FHA = FHA),species = mfdb_group(FHA =FHA), age = NULL) 
grouping_FSA_catch <- list( year = "1996",predator_species  = mfdb_group(FSA = FSA),species = mfdb_group(FSA = FSA), age = NULL) 


#Non-stanza
grouping_FHE <- list(year = "1996", predator_species  =mfdb_group(FHE = FHE),species = mfdb_group(FHE = FHE), age = NULL)   
grouping_FRF <- list(year = "1996", predator_species = mfdb_group(FRF = FRF),species = mfdb_group(FRF = FRF), age = NULL)   
grouping_FGH <- list(year = "1996", predator_species = mfdb_group(FGH = FGH),species = mfdb_group(FGH = FGH), age = NULL)  
grouping_FGHx <- list(year = "2002", predator_species = mfdb_group(FGH = FGH),species = mfdb_group(FGH = FGH), age = NULL)  

grouping_FCA22 <- list(year = "2000", predator_species = mfdb_group(FCA = FCA),species = mfdb_group(FCA =FCA), age = NULL) 
grouping_FCA <- list(year = "1996", predator_species = mfdb_group(FCA = FCA),species = mfdb_group(FCA =FCA), age = NULL) 
grouping_FMI <- list(year = "1996", predator_species = mfdb_group(FMI = FMI),species = mfdb_group(FMI =FMI), age = NULL) 

grouping_FFF <- list(year = "1996", predator_species = mfdb_group(FFF = FFF), species = mfdb_group(FFF = FFF), age = NULL) #Flatfish
grouping_FOC <- list(year = "1996", predator_species = mfdb_group(FOC = c("WHG","BLI", "LIN", "USK" )), species = mfdb_group(FOC = c("WHG","BLI", "LIN",  "USK")), age = NULL) # Other codfish
grouping_FOC1 <- list(year = "1996", predator_species = mfdb_group(FOC = c("WHG", "LIN", "USK" )), species = mfdb_group(FOC = c("WHG", "LIN",  "USK")), age = NULL) # Other codfish
grouping_FOC2 <- list(year = "2000", predator_species = mfdb_group(FOC2 = c("BLI")), species = mfdb_group(FOC2 = c("BLI")), age = NULL) # Other codfish

grouping_FDC <- list(year = "1996", predator_species = mfdb_group(FDC = c("CAA","ARU", "CAS", "LUM", "MON" )), species = mfdb_group(FDC = c("CAA", "ARU", "CAS", "LUM", "MON")), age = NULL) # Demersal commercial
grouping_FDC1 <- list(year = "1996", predator_species = mfdb_group(FDC = c("CAA", "CAS", "LUM", "MON" )), species = mfdb_group(FDC = c("CAA", "CAS", "LUM", "MON")), age = NULL) # Demersal commercial
grouping_FDC2 <- list(year = "1996", predator_species = mfdb_group(FDC2 = "ARU"), species = mfdb_group(FDC2 =  "ARU"), age = NULL) # Demersal commercial


grouping_SSR <- list(year = "1996", predator_species = mfdb_group(SSR = SSR), species = mfdb_group(SSR = SSR), age = NULL )
grouping_SSD <- list(year = "1996", predator_species = mfdb_group(SSD = SSD), species = mfdb_group(SSD = SSD), age = NULL )
grouping_SSH <- list(year = "1996", predator_species = mfdb_group(SSH = SSH), species = mfdb_group(SSH = SSH), age = NULL )
grouping_SB  <- list(year = "1996", predator_species = mfdb_group(SB  = SB),  species = mfdb_group(SB =   SB), age = NULL )
grouping_PIN <- list(year = "1996", predator_species = mfdb_group(PIN = PIN), species = mfdb_group(PIN = PIN), age = NULL )
grouping_WMW <- list(year = "1996", predator_species = mfdb_group(WMW = WMW), species = mfdb_group(WMW = WMW), age = NULL )
grouping_WHB <- list(year = "1996", predator_species = mfdb_group(WHB = WHB), species = mfdb_group(WHB = WHB), age = NULL )                                                                                      
grouping_WHT <- list(year = "1996", predator_species = mfdb_group(WHT = WHT), species = mfdb_group(WHT = WHT), age = NULL )
grouping_WTO <- list(year = "1996", predator_species = mfdb_group(WTO = WTO), species = mfdb_group(WTO = WHT), age = NULL )
grouping_LOB <- list(year = "1996", predator_species = mfdb_group(LOB = LOB), species = mfdb_group(LOB = LOB), age = NULL )
grouping_PWN <- list(year = "1996", predator_species = mfdb_group(PWN = PWN), species = mfdb_group(PWN = PWN), age = NULL )
grouping_FSD <- list(year = "1996", predator_species = mfdb_group(FSD = FSD), species = mfdb_group(FSD = FSD), age = NULL )
grouping_FDF <- list(year = "1996", predator_species = mfdb_group(FDF = FDF), species = mfdb_group(FDF = FDF), age =NULL)
grouping_FBP <- list(year = "1996", predator_species = mfdb_group(FBP = FBP), species = mfdb_group(FBP = FBP), age =NULL)
grouping_FLC <- list(year = "1996", predator_species = mfdb_group(FLC = FLC), species = mfdb_group(FLC = FLC), age =NULL)
grouping_FEP <- list(year = "1996", predator_species = mfdb_group(FEP = FEP), species = mfdb_group(FEP = FEP), age =NULL)
grouping_FIN <- list(year = "1996", predator_species = mfdb_group(FIN = FIN), species = mfdb_group(FIN = FIN), age =NULL)
grouping_ZL <-  list(year = "1996", predator_species = mfdb_group(ZL =   ZL), species = mfdb_group(ZL =   ZL), age =NULL)
grouping_ZS <-  list(year = "1996", predator_species = mfdb_group(ZS =   ZS), species = mfdb_group(ZS =   ZS), age =NULL)
grouping_ZG <-  list(year = "1996", predator_species = mfdb_group(ZG =   ZG), species = mfdb_group(ZG =   ZG), age =NULL)



###### Grouping for diet consumption - take the year out
#stanza species
grouping_FCDd <- list(predator_species = 'COD',species = 'COD',predator_length = mfdb_group(FCD.juv=0:45, FCD.adult=46:200))
grouping_FHAd <- list(predator_species = 'HAD',species = 'HAD',predator_length = mfdb_group(FHA.juv=0:30, FHA.adult=31:120))
grouping_FSAd <- list(predator_species = 'POK',species = 'POK',predator_length = mfdb_group(FSA.juv=0:48, FSA.adult=49:200))

grouping_FHEd <- list(predator_species = mfdb_group(FHE = FHE),species = mfdb_group(FHE = FHE), age = NULL)   
grouping_FRFd <- list(predator_species = mfdb_group(FRF = FRF),species = mfdb_group(FRF = FRF), age = NULL)    
grouping_FGHd <- list(predator_species = mfdb_group(FGH = FGH),species = mfdb_group(FGH = FGH), age = NULL)     
grouping_FCAd <- list(predator_species = mfdb_group(FCA = FCA),species = mfdb_group(FCA = FCA), age = NULL)   
grouping_FMId <- list(predator_species = mfdb_group(FMI = FMI),species = mfdb_group(FMI = FMI), age = NULL)   
grouping_FFFd <- list(predator_species = mfdb_group(FFF = FFF),species = mfdb_group(FFF = FFF), age = NULL)   
grouping_FOCd <- list(predator_species = mfdb_group(FOC = c("WHG", "LIN", "BLI", "USK" )), species = mfdb_group(FOC = c("WHG", "LIN", "BLI", "USK")), age = NULL) # Other codfish
grouping_FDCd <- list(predator_species = mfdb_group(FDC = c("CAA", "CAS", "ARU", "LUM", "MON" )), species = mfdb_group(FDC = c("CAA", "CAS", "ARU", "LUM", "MON")), age = NULL) 
grouping_SSRd <- list(predator_species = mfdb_group(SSR = SSR),species = mfdb_group(SSR = SSR), age = NULL)   
grouping_SSDd <- list(predator_species = mfdb_group(SSD = SSD),species = mfdb_group(SSD = SSD), age = NULL)   
grouping_SSHd <- list(predator_species = mfdb_group(SSH = SSH),species = mfdb_group(SSH = SSH), age = NULL)   
grouping_LOBd <- list(predator_species = mfdb_group(LOB = LOB),species = mfdb_group(LOB = LOB), age = NULL)   
grouping_PWNd <- list(predator_species = mfdb_group(PWN = PWN),species = mfdb_group(PWN = PWN), age = NULL)   
grouping_FSDd <- list(predator_species = mfdb_group(FSD = FSD),species = mfdb_group(FSD = FSD), age = NULL)   
grouping_FDFd <- list(predator_species = mfdb_group(FDF = FDF),species = mfdb_group(FDF = FDF), age = NULL)   
grouping_FBPd <- list(predator_species = mfdb_group(FBP = FBP),species = mfdb_group(FBP = FBP), age = NULL)   
grouping_SBd  <- list(predator_species = mfdb_group(SB =   SB),species = mfdb_group(SB = SB), age = NULL) 
grouping_PINd <- list(predator_species = mfdb_group(PIN = PIN),species = mfdb_group(PIN = PIN), age = NULL) 
grouping_WMWd <- list(predator_species = mfdb_group(WMW = WMW),species = mfdb_group(WMW = WMW), age = NULL) 
grouping_WHBd <- list(predator_species = mfdb_group(WHB = WHB),species = mfdb_group(WHB = WHB), age = NULL)                                                                                 
grouping_WHTd <- list(predator_species = mfdb_group(WHT = WHT),species = mfdb_group(WHT = WHT), age = NULL) 
grouping_WTOd <- list(predator_species = mfdb_group(WTO = WTO),species = mfdb_group(WTO = WTO), age = NULL) 



# Tracer
grouping_tracer_cap = list( sampling_type = "WCAPS")
grouping_tracer_her = list(sampling_type= "")
grouping_tracer = list(sampling_type = c('IGFS-IND','ADH', 'AUT'))  # Make sure we only fetch tracer data
grouping_tracer_AUT = list(sampling_type = 'AUT')  # Make sure we only fetch tracer data
grouping_tracer_ADH = list(sampling_type ='ADH')  # Make sure we only fetch tracer data
grouping_tracer_IGFS = list(sampling_type = 'IGFS-IND' )  # Make sure we only fetch tracer data
grouping_tracer_LOBS = list(sampling_type = 'LOBS')
grouping_vessel = list( sampling_type = 'LND' , gear = mfdb_group(PELAGIC = c("PGT","PSE"),TRAWLS = c("BMT", "NPT", "SHT", "DRD"), LONGLINE = "LLN", SEINERS = c("DSE"), GILLNETS = "GIL", OTHER = c('HLN',"VAR", "TRP"), HARPOON = 'HAR'))

# Grouping prey into functional groups similar/like Atlantis

grouping_prey <- list(
                      prey_species = mfdb_group(FKR = FKR, FEP = FEP, FLC = FLC, FIN = FIN, ZL = ZL, ZS = ZS, ZG = ZG,
                                                CEP = CEP, PWN = PWN, PIN = PIN, SSR = SSR, SSH = SSH, SSD = SSD, LOB = LOB,
                                                FSD = FSD, FDF = FDF, FBP = FBP, FRF = FRF, FHE = FHE, FCA = FCA, FOC = FOC, 
                                                FDC = FDC, FFF = FFF, FGH = FGH, FMI = FMI, SB = SB, WMW = WMW, WHB = WHB,
                                                WHT = WHT, WTO = WTO, FCD.juv= FCD, FSA.juv=FSA, FHA.juv=FHA),
                      year = mfdb_group(all=c(1971:2022)), 
                                                                                    
                      species = mfdb_group(FKR = FKR, FEP = FEP, FLC = FLC, FIN = FIN, ZL = ZL, ZS = ZS, ZG = ZG,
                                           CEP = CEP, PWN = PWN, PIN = PIN, SSR = SSR, SSH = SSH, SSD = SSD, LOB = LOB,
                                           FSD = FSD, FDF = FDF, FBP = FBP, FRF = FRF, FHE = FHE, FCA = FCA, FOC = FOC, 
                                           FDC = FDC, FFF = FFF, FGH = FGH, FMI = FMI, SB = SB, WMW = WMW, WHB = WHB,
                                           WHT = WHT, WTO = WTO, FCD.juv= FCD, FSA.juv=FSA, FHA.juv=FHA),
                      year = mfdb_group(all=c(1971:2022)))



# Query data and group together

## Survey data
#FOCFDC<-data.frame(year=rep(1996,4), step=rep("all",4), area = rep(c("north", "south"),2), species = c("FOC", "FOC", "FDC","FDC") ,  total_weight = c(8403.58, 20136858, 13582149,60012851))
FOCFDC<-data.frame(year=rep(1996,2), step=rep("all",2), area = rep("all",2), species = c("FOC", "FDC"),  total_weight = c( (46153380), ( 138212700)))

survey_stanza <- mfdb_concatenate_results(     
  mfdb_sample_totalweight(mdb, c('length'), c(grouping_area, grouping_FCD, grouping_tracer_IGFS))[[1]], 
  mfdb_sample_totalweight(mdb, c('length'), c(grouping_area, grouping_FHA, grouping_tracer_IGFS))[[1]],
  mfdb_sample_totalweight(mdb, c('length'), c(grouping_area, grouping_FSA, grouping_tracer_IGFS))[[1]])  %>% 
  rename(species=length)

STW_data <- data.frame(year=rep(1996,6), step=rep("all",6), area = rep("all",6),  species=c("CEP", "WHT", "WMW", "WTO", "WHB", "SB"),  total_weight = c(1,75725000,70889000,44118000, 434857000,2093000))

survey_data <- mfdb_concatenate_results(
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_FGHx, grouping_tracer_AUT))[[1]], 
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_FRF, grouping_tracer_IGFS))[[1]], 
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_FHE, grouping_tracer_ADH))[[1]], 
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_FCA22, grouping_tracer_cap))[[1]], 
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_FMI, grouping_tracer_ADH))[[1]], 
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_FFF, grouping_tracer_IGFS))[[1]], 
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_SSR, grouping_tracer_AUT))[[1]], 
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_SSD, grouping_tracer_AUT))[[1]],
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_SSH, grouping_tracer))[[1]],
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_PINd, grouping_tracer))[[1]], 
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_WMW, grouping_tracer))[[1]], 
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_WHB, grouping_tracer))[[1]], 
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_WHT, grouping_tracer))[[1]],
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_LOB, grouping_tracer))[[1]],
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_WTO, grouping_tracer))[[1]],
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_FSD, grouping_tracer))[[1]],
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_FDF, grouping_tracer))[[1]],
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_FBP, grouping_tracer))[[1]],
  mfdb_sample_totalweight(mdb, c('species'), c(grouping_area, grouping_PWN, grouping_tracer))[[1]]) %>% 
  mutate(year=1996)  %>% full_join(survey_stanza) %>% full_join(FOCFDC)  %>% full_join(STW_data) %>% 
  mutate(total_weight = total_weight/1e+3) %>% slice(17,16,19,18,21,20,1:15,22:29)
#slice(27,29,26,28,31,33,30,32,35,37,34,36,1:25,38,39)

## Catch data


TW_data <- data.frame(year=rep(1996,6), step=rep("all",6), area = rep("all",6),  species=c("PIN","WHT", "WMW", "WTO", "WHB", "SB"), gear = c("GILLNETS", "HARPOON","HARPOON", "GILLNETS","HARPOON","GILLNETS"),  total_weight = c(177905, 100,1, 485, 45740,  169246))
juv.data <-data.frame(year = c(rep(1996,15)), step = c(rep("all", 15)), area = c(rep("all",15)), species = c(rep("FCD.juv", 5),rep("FHA.juv",5), rep("FSA.juv",5)),gear = rep(c("GILLNETS", "LONGLINE", "OTHER", "SEINERS", "TRAWLS"),3),total_weight = c(418.2346,399.47435,195.52456,130.50040,675.79229,27.52254,77.90312,1.54144,56.89612,424.46182,92.37173,2.89867,17.39849,20.23685,263.00799))
phytoplankton  <-data.frame(year = c(rep("all",6)), step = c(rep("all", 6)), area = c(rep("all",6)), predator_species = c("PWN", "FEP", "ZL", "ZS", "ZG", "FIN"),  prey_species = c(rep("Phytoplankton", 6)), ratio =  c(0,0,0,0,0,0 ))

juv.data.imput<- mfdb_concatenate_results(
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_FCD_catch, grouping_vessel))[[1]],
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_FHA_catch, grouping_vessel))[[1]],
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_FSA_catch, grouping_vessel))[[1]])  %>% 
  mutate(species = case_when((species == "FHA") ~ "FHA.juv", (species == "FSA" ) ~ "FSA.juv",(species == "FCD" ) ~  "FCD.juv", TRUE ~ species)) %>% 
  mutate(total_weight = (total_weight*0.01)/1e+3) 

catch_stanza<- mfdb_concatenate_results(
  mfdb_sample_totalweight(mdb, c('species','age', 'gear'), c(grouping_area, grouping_FCD_catch, grouping_vessel))[[1]],
  mfdb_sample_totalweight(mdb, c('species','age', 'gear'), c(grouping_area, grouping_FHA_catch, grouping_vessel))[[1]],
  mfdb_sample_totalweight(mdb, c('species','age', 'gear'), c(grouping_area, grouping_FSA_catch, grouping_vessel))[[1]])  %>% 
  mutate(species = case_when((species == "FHA") ~ "FHA.adult", (species == "FSA" ) ~ "FSA.adult",(species == "FCD" ) ~  "FCD.adult", TRUE ~ species)) %>% 
  mutate(total_weight = (total_weight*0.99)/1000) %>% select(-age) %>% full_join(juv.data.imput)

catch_data<- mfdb_concatenate_results(
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_FGH, grouping_vessel))[[1]],
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_FRF, grouping_vessel))[[1]],
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_FHE, grouping_vessel))[[1]],
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_FCA, grouping_vessel))[[1]],
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_FMI, grouping_vessel))[[1]],
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_FFF, grouping_vessel))[[1]],
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_SSR, grouping_vessel))[[1]], 
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_SSD, grouping_vessel))[[1]], 
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_SSH, grouping_vessel))[[1]], 
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_LOB, grouping_vessel))[[1]], 
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_PWN, grouping_vessel))[[1]], 
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_FEP, grouping_vessel))[[1]],
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_FIN, grouping_vessel))[[1]],
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_FLC, grouping_vessel))[[1]],
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_FDF, grouping_vessel))[[1]],
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_FOC, grouping_vessel))[[1]],
  mfdb_sample_totalweight(mdb, c('species', 'gear'), c(grouping_area, grouping_FDC, grouping_vessel))[[1]]) %>% 
  mutate( year=1996) %>% full_join(TW_data)  %>% mutate(total_weight = total_weight/1e+3) %>% 
  full_join(catch_stanza) %>% rename(vessel = gear) %>% slice(91:96,73:78,97:102,79:84,103:108,85:89,1:72)

## Consumption data

consumption_stanza <- mfdb_concatenate_results(
  mfdb_stomach_preyweightratio(mdb, c('predator_length', 'prey_species'), c(grouping_area, grouping_FCDd, grouping_prey))[[1]], 
  mfdb_stomach_preyweightratio(mdb, c('predator_length', 'prey_species'), c(grouping_area, grouping_FHAd, grouping_prey))[[1]],
  mfdb_stomach_preyweightratio(mdb, c('predator_length', 'prey_species'), c(grouping_area, grouping_FSAd, grouping_prey))[[1]]) %>% 
  rename(predator_species = predator_length) %>% filter(!is.na(ratio)) 

consumption_data <- mfdb_concatenate_results(
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_FGHd, grouping_prey))[[1]], 
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_FRFd, grouping_prey))[[1]],
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_FHEd, grouping_prey))[[1]],
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_FCAd, grouping_prey))[[1]], 
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_FMId, grouping_prey))[[1]], 
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_FFFd, grouping_prey))[[1]], 
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_FOCd, grouping_prey))[[1]], 
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_FDCd, grouping_prey))[[1]], 
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_SSRd, grouping_prey))[[1]], 
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_SSDd, grouping_prey))[[1]], 
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_SSHd, grouping_prey))[[1]],
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_FSDd, grouping_prey))[[1]], 
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_FDFd, grouping_prey))[[1]],
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_FFFd, grouping_prey))[[1]],
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_SBd,  grouping_prey))[[1]], 
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_PINd, grouping_prey))[[1]],
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_WMWd, grouping_prey))[[1]],
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_WHBd, grouping_prey))[[1]],
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_WHTd, grouping_prey))[[1]],
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_LOBd, grouping_prey))[[1]],
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_PWNd, grouping_prey))[[1]],
  mfdb_stomach_preyweightratio(mdb, c('predator_species', 'prey_species'), c(grouping_area, grouping_WTOd, grouping_prey))[[1]]) %>% 
  full_join(phytoplankton) %>% full_join(consumption_stanza) %>% filter(!is.na(ratio)) %>% collect 


# It would be good to divide area my N, S E and W
area_data <- mfdb_area_size(mdb, grouping_area)[[1]]
area_data<-area_data[1,] %>% mutate( size=1) 




save(catch_data, file = "catch_data2.RData")
save(consumption_data, file = "consumption_data.RData")
get(load("catch_data2.RData"))
get(load("survey_data.RData"))
get(load("consumption_data.RData"))




# Biomass
RReco.params <- mfdb_rpath_params(
  area_data,
  survey_data,
  catch_data,
  consumption_data,
  create_rpath_params = Rpath::create.rpath.params)


# Biomass
RReco.params$model[Group %in% c('FCD.adult'), Biomass := 5.176328e+05 ]# 158421*1.1]# assessment_biomass.R
RReco.params$model[Group %in% c('FHA.adult'), Biomass :=  108323]# assessment_biomass.R increased to balance. 
RReco.params$model[Group %in% c('FSA.adult'), Biomass := 1.279456e+05] # assessment_biomass.R


RReco.params$model[Group %in% c('FRF'), Biomass :=  496681.4]# 6.718996e+05] # 324124 + 165271 + 7287 #assessment_biomass.R
RReco.params$model[Group %in% c('FHE'), Biomass := 322215]# 4440215 ] #501683.1*1.16]# hækkað til að balancera bergmálsvísitölur sinnum meðalþyngd eftir aldri
RReco.params$model[Group %in% c('FGH'), Biomass :=  147582.3]# assessment
RReco.params$model[Group %in% c('WMW'), Biomass := 70889] #From Gísli víkings
RReco.params$model[Group %in% c('WHB'), Biomass := 434857] #from NASS 1995
RReco.params$model[Group %in% c('WHT'), Biomass := 75725] #From Gísli víkings
RReco.params$model[Group %in% c('WTO'), Biomass := 44118] #From Gísli víkings 
RReco.params$model[Group %in% c('PIN'), Biomass := 2093] #From hafró 814.68 + 1500
RReco.params$model[Group %in% c('Phytoplankton'), Biomass := 12151000]#20201842] #seaaroundus.org tonnes per km2 per year
RReco.params$model[Group %in% c('SB'), Biomass := 2500.841]  #see mammal_ip.R
RReco.params$model[Group %in% c('FMI'), Biomass := 600000]#2210376] # ssb from assessment
RReco.params$model[Group %in% c('LOB'), Biomass := 12625]#41440.63*1.1] #12625] ( 75.8 milljón recriuts sem eru 20gr + 6+(11109tonn +)) https://www.hafogvatn.is/static/research/files/hv2018-25.pdf

RReco.params$model[Group %in% c('SB'), Biomass := 4612] #41440.63*1.1] #12625] ( 75.8 milljón recriuts sem eru 20gr + 6+(11109tonn +)) https://www.hafogvatn.is/static/research/files/hv2018-25.pdf

# Unknown biomass, clean if any bio from survey
RReco.params$model[Group %in% c('FBP'), Biomass := NA]
RReco.params$model[Group %in% c('FSD'), Biomass := NA]
RReco.params$model[Group %in% c('CEP'), Biomass := NA]
RReco.params$model[Group %in% c('SSH'), Biomass := 1000] 
RReco.params$model[Group %in% c('SSR'), Biomass := 50000]
RReco.params$model[Group %in% c('SSD'), Biomass := 5000]
RReco.params$model[Group %in% c('FDF'), Biomass := NA]
RReco.params$model[Group %in% c('FCA'), Biomass := 1680052*1.05]# 2670527]# 2270527]#1277600] #1927000]# veiðar + 400000 tonn árið 1996

# RReco.params$model[Group %in% c('FCD.juv'), Biomass := 104637.167] 
# RReco.params$model[Group %in% c('FHA.juv'), Biomass := NA ]# GUESTIMATE
# RReco.params$model[Group %in% c('FSA.juv'), Biomass := 28457.512 ] 

RReco.params$model[Group %in% c('PWN'), Biomass := NA]
RReco.params$model[Group %in% c('FFF'), Biomass := NA]# 71917.66] #59929.9]# 
RReco.params$model[Group %in% c('FOC'), Biomass := 46153.38]
RReco.params$model[Group %in% c('FDC'), Biomass := 138212.7] 


# Catch

#RReco.params$model[Group %in% c('FCA'), SEINERS := 600000]# breyta
# RReco.params$model[Group %in% c('SB'), GILLNETS := 169.246]  #see SB-catch.R
# RReco.params$model[Group %in% c('WTO'), GILLNETS := 4.85] 
RReco.params$model[Group %in% c('WHB'), HARPOON := 0.001]#63.57]
RReco.params$model[Group %in% c('WHT'), HARPOON := 0.001] #45.74]
RReco.params$model[Group %in% c('WHO'), HARPOON := 0.001] #45.74]
RReco.params$model[Group %in% c('WMW'), HARPOON := 0.001] #45.74]


 RReco.params$model[Group %in% c('FBP'), SEINERS := 0.1] 
# RReco.params$model[Group %in% c('PIN'), GILLNETS := 2093*0.085] #estimated bycatch from Guðjón. 6-12% á landsel og 8-25% útsel (nær neðra gildi). en tek meðaltal sinnum hlutfall lífmassa
# RReco.params$model[Group %in% c('PIN'), LONGLINE := 0] 
# RReco.params$model[Group %in% c('PIN'), SEINERS := 0] 
# RReco.params$model[Group %in% c('PIN'), OTHER := 0] 
# RReco.params$model[Group %in% c('PIN'), TRAWLS := 0] 

#Add catch and biomass to epifaunal group (urchins, whelks and other)
RReco.params$model[Group %in% c('FEP'), OTHER := 524.3] #whelk from traps
RReco.params$model[Group %in% c('FEP'), TRAWLS := 491.3] # urchins

# QB not known for invertebrates and p/Q according to Christensen, 1995
RReco.params$model[Group %in% c('FEP',  'PWN', 'LOB', 'FLC'), ProdCons:= 0.15]
RReco.params$model[Group %in% c('FIN'), ProdCons:= 0.3]
RReco.params$model[Group %in% c('FEP'), ProdCons:= 0.25]

RReco.params$model[Group %in% c('ZG'), ProdCons:= 0.45]
RReco.params$model[Group %in% c('ZS', 'ZL', 'FKR'), ProdCons:= 0.3]

# Add discards
RReco.params$model$GILLNETS[1:37][is.na(RReco.params$model$GILLNETS[1:37])] <- 0
RReco.params$model$PELAGIC[1:37][is.na(RReco.params$model$PELAGIC[1:37])] <- 0

RReco.params$model$SEINERS[1:37][is.na(RReco.params$model$SEINERS[1:37])] <- 0
RReco.params$model$TRAWLS[1:37][is.na(RReco.params$model$TRAWLS[1:37])] <- 0
RReco.params$model$HARPOON[1:37][is.na(RReco.params$model$HARPOON[1:37])] <- 0
RReco.params$model$LONGLINE[1:37][is.na(RReco.params$model$LONGLINE[1:37])] <- 0
RReco.params$model$OTHER[1:37][is.na(RReco.params$model$OTHER[1:37])] <- 0

#ASSUMING discard is 1%
RReco.params$model$PELAGIC.disc[1:37] = as.numeric(RReco.params$model$PELAGIC[1:37])*0.01
RReco.params$model$GILLNETS.disc[1:37] = as.numeric(RReco.params$model$GILLNETS[1:37])*0.01
RReco.params$model$LONGLINE.disc[1:37] = as.numeric(RReco.params$model$LONGLINE[1:37])*0.01
RReco.params$model$TRAWLS.disc[1:37] = as.numeric(RReco.params$model$TRAWLS[1:37])*0.01
RReco.params$model$SEINERS.disc[1:37] = as.numeric(RReco.params$model$SEINERS[1:37])*0.01
RReco.params$model$OTHER.disc[1:37] = as.numeric(RReco.params$model$OTHER[1:37])*0.01
RReco.params$model$HARPOON.disc[1:37] = as.numeric(RReco.params$model$HARPOON[1:37])*0.0
RReco.params$model[Group %in% c("PIN"), GILLNETS.disc := 0]
RReco.params$model[Group %in% c("WHT"), HARPOON.disc := 0]

RReco.params$model[Group %in% c("Detritus"), GILLNETS.disc := NA]
RReco.params$model[Group %in% c("Detritus"), LONGLINE.disc := NA]
RReco.params$model[Group %in% c("Detritus"), TRAWLS.disc := NA]
RReco.params$model[Group %in% c("Detritus"), OTHER.disc := NA]
RReco.params$model[Group %in% c("Detritus"), SEINERS.disc := NA]
RReco.params$model[Group %in% c("Detritus"), HARPOON.disc := NA]
RReco.params$model[Group %in% c("Detritus"), PELAGIC.disc := NA]

# Model input

#bioaccu fyrir flatfish er -0.47% = -0.0047 eða 2.33% þá -0.0233

RReco.params$model[, Type := c(rep(0, 36),1,2, rep(3,7))]
RReco.params$model[, Discards := c(rep(0, 38), rep(1,7))]
RReco.params$model[, Detritus := c(rep(1, 37) , rep(0,8))]
RReco.params$model[, BioAcc := c(rep(0, 11), 0, rep(0, 26) ,rep(NA, 7))] #NA for gear
# prófa að breyta BA fyrir grálúðu (lífmassi er að aukast)
# Unassimmilated consumption - Percentage of consumed food not utilized for growth
RReco.params$model[Group %in% c('WMW', 'WHB', 'WHT', 'WTO', 'SB', 'PIN', 'FRF', 'FHA.adult', 'FHA.juv', 'FCD.juv', 'FCD.adult', 'FSA.juv', 'FSA.adult', 'FSD', 'FDF', 'FBP', 'FOC', 'FDC', 'FFF', 'FGH', 'CEP', 'SSR', 'SSH', 'SSD'), Unassim := 0.2]# 0.112]
RReco.params$model[Group %in% c('FCA', 'FMI', 'FHE', 'LOB', 'PWN', "FLC"), Unassim := 0.2]
RReco.params$model[Group %in% c( "FIN", "FEP",'ZL', 'ZS', 'ZG', 'FKR'), Unassim := 0.35]
RReco.params$model[Group %in% c('Phytoplankton', 'Detritus'), Unassim := 0]



RReco.params$model[Group %in% c("CEP",'FEP', 'FIN', "FKR",'ZL', 'ZS', 'ZG', "PWN", "FLC"), EE := 0.95]
RReco.params$model[Group %in% c("FDF","FBP", "FSD" ), EE := 0.9]
RReco.params$model[Group %in% c(  "FFF"), EE := 0.7]
# taka út top predators og gefa þeim lífmassa. Færa FFF og FGH í lægra EE eða reyna að finna lífmassa. Spurja bjarka um F á FGH


# P:B
# Christensen 2001  Christensen, Villy & Ellertsen, Bjørnar & Kvamme, Cecilie & Melle, Webjørn & Nøttestad, Leif & Tjelmeland, Sigurd. (2001). An Ecopath model for the Norwegian Sea and Barents Sea. Fisheries impacts on North Atlantic ecosystems: models and analyses. Fisheries Centre Research. 9. 
RReco.params$model[Group %in% c('FCA'), PB := 1.293693*1.65]#1.402046] # data base
RReco.params$model[Group %in% c('FCA'), QB :=  5.23] #5.475103] #  input_other.R 

RReco.params$model[Group %in% c('FMI'), PB := 0.5406] # input_other.R
RReco.params$model[Group %in% c('FMI'), QB :=  5.18]#5.658894] # data+base 

RReco.params$model[Group %in% c('FHE'), PB := 0.8438*1.6]#0.6866881*1.95]# input_other.R
RReco.params$model[Group %in% c('FHE'), QB :=   4.5389] # input_other.R skoða parametra  


RReco.params$model[Group %in% c('WMW'), PB := 0.03]  
RReco.params$model[Group %in% c('WMW'), QB := 6.580281] # mammal_ip.R

RReco.params$model[Group %in% c('WHB'), PB := 0.03]
RReco.params$model[Group %in% c('WHB'), QB := 4.41] # mammal_ip.R

RReco.params$model[Group %in% c('WHT'), PB := 0.04]  
RReco.params$model[Group %in% c('WHT'), QB := 5.730417]  #mammal_ip.R

RReco.params$model[Group %in% c('WTO'), PB := 0.03] 
RReco.params$model[Group %in% c('WTO'), QB :=  10.27753] # mammal_ip.R



RReco.params$model[Group %in% c('PIN'), PB := 0.142]   
RReco.params$model[Group %in% c('PIN'), QB :=  14.4576] # mammal_ip.R

RReco.params$model[Group %in% c('SB'), PB := 0.11] #erla
RReco.params$model[Group %in% c('SB'), QB := 38.44] # mammal_ip.R




RReco.params$model[Group %in% c('FRF'), PB := 0.4075*1.2 ]#0.7182958]# 0.768514] #0.768514*1.7] # input_other.R. ATH vegið WINF með
RReco.params$model[Group %in% c('FRF'), QB := 3.10] #  input_other.R  

RReco.params$model[Group %in% c('FGH'), PB := 0.3071349] #0.266]# 1.041467*1.2]#0.416587*2.5] # data base. Increased to balance
RReco.params$model[Group %in% c('FGH'), QB :=  2.13 ]#  input_other.R  

RReco.params$model[Group %in% c('FCD.adult'), PB :=   0.4915192]#0.729]# 0.8253531] #FCD_input_parameter með Winf jöfnu
RReco.params$model[Group %in% c('FCD.adult'), QB := 1.8178] # FCD_input_parameters.R balance decr 20%
RReco.params$model[Group %in% c('FCD.juv'), PB := 1.271548]#0.6010808*1.2]# 0.8543146] #FCD_input_parameter með Winf jöfnu
RReco.params$model[Group %in% c('FCD.juv'), QB := 3.151926]#3.12] # FCD_input_parameters.R

RReco.params$model[Group %in% c('FHA.adult'), PB :=0.81877] #  FHA_input_parameters.R
RReco.params$model[Group %in% c('FHA.adult'), QB := 2.47] #  FHA_input_parameters.R
RReco.params$model[Group %in% c('FHA.juv'), QB := 3.44] # FHA_input_parameters.R
RReco.params$model[Group %in% c('FHA.juv'), PB := 1.5] #  FHA_input_parameters.R # Increased to balance

RReco.params$model[Group %in% c('FSA.adult'), PB :=  0.5957]# 0.6017473] # FSA.input_parameters.R
RReco.params$model[Group %in% c('FSA.adult'), QB := 1.97499]#2.03856] # FSA_input_parameters.R
RReco.params$model[Group %in% c('FSA.juv'), QB := 3.034276] # FSA_input_parameters.R
RReco.params$model[Group %in% c('FSA.juv'), PB :=  0.87]#0.403]


RReco.params$model[Group %in% c('FDF'), PB := 0.263*5] # PM_FDF_FBF.R 
RReco.params$model[Group %in% c('FDF'), QB := 3.1]#3.1] # PM_FDF_FBF.R 

RReco.params$model[Group %in% c('FBP'), PB := 0.6027] # PM_FDF_FBF.R
RReco.params$model[Group %in% c('FBP'), QB := 6.52]#5.5693149] #PM_FDF_FBF.R


RReco.params$model[Group %in% c('FFF'), PB := 0.6617178]  # 
RReco.params$model[Group %in% c('FFF'), QB :=  2.398135]#1.74]# 3.313522]  # input_other.R 
 
RReco.params$model[Group %in% c('FOC'), PB := 0.6049*1.15] # increased to BALANCE 
RReco.params$model[Group %in% c('FOC'), QB := 2.53]  # input_other.R 


RReco.params$model[Group %in% c('FDC'), PB := 0.2966008*1.55]  # increased to balance, from input.other.R. 
RReco.params$model[Group %in% c('FDC'), QB := 2.41*0.9] # input_other.R  #DECREASED to BALANCE


RReco.params$model[Group %in% c('FSD'), PB := 0.4] #PM_FSD.R
RReco.params$model[Group %in% c('FSD'), QB := 4.9]# 4.51] #PM_FSD.R


RReco.params$model[Group %in% c('SSR'), PB := 0.276293] #PM_SSR.R
RReco.params$model[Group %in% c('SSR'), QB := 3.12]# PM_SSR.R

RReco.params$model[Group %in% c('SSD'), PB := 0.11] 
RReco.params$model[Group %in% c('SSD'), QB := 2.24] # PM_SSD.R


RReco.params$model[Group %in% c('SSH'), PB := 0.06] # PM_SSH.R
RReco.params$model[Group %in% c('SSH'), QB := 1.19] # PM_SSH.R


RReco.params$model[Group %in% c('CEP'), PB := 2.44] # christensen 2001 
RReco.params$model[Group %in% c('CEP'), QB := 12] # christensen 2001 

# No QB parameters. 
RReco.params$model[Group %in% c('LOB'), PB := 2.5]# # north atlantic-gunétte
RReco.params$model[Group %in% c('PWN'), PB := 1.25]# 0.61]#0.6454782]# christensen 2001 
RReco.params$model[Group %in% c('FKR'), PB := 2.5] # christensen 2001 
RReco.params$model[Group %in% c('Phytoplankton'), PB :=  243] #117]#243.47] ribeiro, #christiansen 2001
RReco.params$model[Group %in% c('ZL'),  PB := 5] # christensen 2001 
RReco.params$model[Group %in% c('ZG'),  PB := 10] # christensen 2001 
RReco.params$model[Group %in% c('ZS'),  PB := 13] # christensen 2001 
RReco.params$model[Group %in% c('FLC'),  PB := 2.5] # macinsson 2002
RReco.params$model[Group %in% c('FIN'),  PB := 0.7565] # 2] # lækka til að balance
RReco.params$model[Group %in% c('FEP'),  PB := 0.78] # 1.04] # equation 13 - >  (0.24 + (0.96*log(6.82251e+06))-0.21*log(0.005) + (0.03*4.5) - 0.16*log(1500)) svo exp(x)/biomass








#######################
## Stanza parameters ##
#######################

RReco.params$stanzas$stgroups[, Wmat     := c( 0.09124642 , 0.1758729,0.2458) ]
RReco.params$stanzas$stgroups[, VBGF_Ksp := c(0.1178,0.21,0.2 )]
RReco.params$stanzas$stindiv[, First  := c(0,36,0,24,0,36)]
RReco.params$stanzas$stindiv[, Last  := c(35,400,23,400,35,400)]
RReco.params$stanzas$stindiv[, Leading := rep(c(FALSE, TRUE), 3)]
RReco.params$stanzas$stindiv[, StanzaNum := rep(0, 6)]

#RReco.params$stanzas$stindiv[, Z :=c(0.60108,0.729, 1.5,0.6447693,0.6530875, 0.6007695)]

RReco.params$stanzas$stindiv[, Z :=c(1.2715480,0.4985203,1.5,0.896006,0.87, 0.5957)]


##########
## Diet ##
##########

#weighted biomasss mean for prey in group WHB. @ diet_modification.R

#Clear existing
RReco.params$diet$WHT <-NA
#add new
RReco.params$diet[Group == 'FRF', WHT := 0.0000902]
RReco.params$diet[Group == 'FSD', WHT := 0.00055]
RReco.params$diet[Group == 'FCA', WHT := 0.001518]
RReco.params$diet[Group == 'FCD.adult', WHT :=  0.0001]

RReco.params$diet[Group == 'FHA.adult', WHT :=  0.0000291]
RReco.params$diet[Group == 'CEP', WHT :=  0.99761]
RReco.params$diet[Group == 'FBP', WHT :=  0.0000291]



# Clear existing
RReco.params$diet$WTO <-NA
#new
RReco.params$diet[Group == 'FCA', WTO := 0.015]
RReco.params$diet[Group == 'FHE', WTO :=  0.35712*0.8]
RReco.params$diet[Group == 'FMI', WTO :=  0.0345]
RReco.params$diet[Group == 'FOC', WTO := 0.00037]
RReco.params$diet[Group == 'FCD.adult', WTO :=  0.02253]
RReco.params$diet[Group == 'FHA.adult', WTO :=  0.0045]
RReco.params$diet[Group == 'FSA.adult', WTO :=  0.0152]
RReco.params$diet[Group == 'CEP', WTO := 0.5317]
RReco.params$diet[Group == 'FBP', WTO :=   0.0178]
RReco.params$diet[Group == 'PWN', WTO :=  0.0178]


# one halibut ate one adult saithe (43cm) remove
RReco.params$diet[Group == 'FSA.juv', FFF :=  NA]

#fix self predation redfish
RReco.params$diet[Group == 'FRF', FRF := 0.002]
# minnka afrán á FRF (blágóma étið 4 karfa)
#RReco.params$diet[Group == 'FGH', FDF := 0.0000001]

#take out flatfish from blue withing stomack.
RReco.params$diet[Group == 'FFF', FMI := NA]
RReco.params$diet[Group == 'FFF', FFF := 0.007354695*0.5]

# From mette skern. gadoid ratio between groups split according to ratio from data base
RReco.params$diet$WMW <-NA
# 
# RReco.params$diet[Group == "FCD.adult", WMW := 0.007]
# RReco.params$diet[Group == "FKR", WMW  := 0.38]
# RReco.params$diet[Group == "FHE", WMW  := 0.007]
# RReco.params$diet[Group == "FSD", WMW  := 0.36]
# RReco.params$diet[Group == "FCA", WMW  := 0.25]

RReco.params$diet[Group == "FCD.adult", WMW := 0.007]
RReco.params$diet[Group == "FKR", WMW  := 0.35]
RReco.params$diet[Group == "FHE", WMW  := 0.007]
RReco.params$diet[Group == "FSD", WMW  := 0.33]
RReco.params$diet[Group == "FCA", WMW  := 0.23]

#biomass weighted average

RReco.params$diet[Group == 'FMI',  WHB := NA]
RReco.params$diet[Group == 'FSD', WHB := 0.001145158]
RReco.params$diet[Group == 'FCA', WHB := 0.1668431]
RReco.params$diet[Group == 'FDC', WHB := 0.00057]
RReco.params$diet[Group == 'FKR', WHB := 0.7192136]
RReco.params$diet[Group == 'ZL', WHB := 0.112225]


# capelin
# ON THE FOOD OF CAPELIN IN THE SUBARCTIC WATERS NORTH OF ICELAND OLAFUR SVAVAR ASTTHORSSON & ASTTHOR GISLASON
RReco.params$diet$FCA <-NA
RReco.params$diet[Group == 'ZL', FCA := 0.92]
RReco.params$diet[Group == 'FKR', FCA := 0.08]


## FIXING DIET 

# long rough dag eating alot of wolffish in 1992. might be eggs but I remove. I leave FFF ratio to halibut eating FDC
RReco.params$diet[Group == 'FDC', FFF := 0.0003468959]

#adult-juv ratio in cod stomacch
RReco.params$diet[Group == 'FHA.juv', FCD.adult := 0.01505778]
RReco.params$diet[Group == 'FHA.adult', FCD.adult := 0.005290571]


# some fishstomachs with large sharks in. take out. 
RReco.params$diet[Group == 'SSH', FRF := NA]
RReco.params$diet[Group == 'SSH', SSR := NA]
RReco.params$diet[Group == 'SSH', FCD.adult := NA]
RReco.params$diet[Group == 'SSH', FGH := NA]
# smaller
RReco.params$diet[Group == 'SSD', FGH := NA]
RReco.params$diet[Group == 'SSD',  FOC:= NA]
RReco.params$diet[Group == 'SSD', FCD.adult := NA]
RReco.params$diet[Group == 'SSD', FRF := NA]
#seals -from mfri database - biomass weighted average. 

RReco.params$diet[Group == 'FCD.juv', PIN :=0.3534066 ]
RReco.params$diet[Group == 'FDC', PIN := 0.1501758]
RReco.params$diet[Group == 'FCA', PIN :=  0.02101099] 
RReco.params$diet[Group == 'FFF', PIN := 0.07740659] 
RReco.params$diet[Group == 'FHE', PIN := 0.0372967] 
RReco.params$diet[Group == 'FSA.juv', PIN :=  0.09582418]

RReco.params$diet[Group == 'FHA.juv', PIN := 0.009494505]
RReco.params$diet[Group == 'FDF', PIN := 0.02527473]
RReco.params$diet[Group == 'FOC', PIN := 0.009450549]
RReco.params$diet[Group == 'FRF', PIN := 0.03938462]
RReco.params$diet[Group == 'FSD', PIN :=0.1591209]


# Seabirds - from mfri database - count*mean weight see SB_diet.R
RReco.params$diet[Group == 'SB', FDC :=NA] #tek út eina ritu í maga á hlýra

RReco.params$diet[Group == 'FSD', SB :=0.468 ]
RReco.params$diet[Group == 'FCA', SB :=0.220 ]
RReco.params$diet[Group == 'FKR', SB := 0.0892  ]
RReco.params$diet[Group == 'CEP', SB := 0.0682]
RReco.params$diet[Group == 'FDF', SB :=0.0580 ]
RReco.params$diet[Group == 'ZL', SB :=0.0450  ]
RReco.params$diet[Group == 'FMI', SB := 0.0344 ]
RReco.params$diet[Group == 'FSA.juv', SB :=0.0151 ]
RReco.params$diet[Group == 'FBP', SB :=0.0121   ]
RReco.params$diet[Group == 'FCD.juv', SB := 0.0103 ]
RReco.params$diet[Group == 'FRF', SB := 0.0100 ]
RReco.params$diet[Group == 'FOC', SB := 0.00824 ]
RReco.params$diet[Group == 'FFF', SB :=0.00285  ]
RReco.params$diet[Group == 'FHE', SB := 0.00283 ]
RReco.params$diet[Group == 'FIN', SB :=0.00249 ]
RReco.params$diet[Group == 'PWN', SB := 0.00161 ]
RReco.params$diet[Group == 'FHA.juv', SB :=0.000943 ]
RReco.params$diet[Group == 'FEP', SB :=0.000626 ]
RReco.params$diet[Group == 'ZG', SB :=0.000351  ]
RReco.params$diet[Group == 'FLC', SB :=0.000192 ]
RReco.params$diet[Group == 'LOB', SB :=0.0000877]
RReco.params$diet[Group == 'FDC', SB := 0.0000598 ]
RReco.params$diet[Group == 'ZS', SB := 0.0000345 ]


# FBP (small pelagic fish - Gulldepla (Maurolicus muelleri), Horse mackerel (Trachurus trachurus) )
# Heimild: Fishbase.org 
RReco.params$diet$FBP <-NA
#https://www.frontiersin.org/articles/10.3389/fmars.2023.1086607/full


RReco.params$diet[Group == 'FKR', FBP := 0.52]
RReco.params$diet[Group == 'ZL', FBP := 0.45]
RReco.params$diet[Group == 'ZS', FBP := 0.03]


#SSR Minnka sjálfsafrán úr 0.06579156
RReco.params$diet[Group == 'SSR', SSR := 0.006579156]
RReco.params$diet[Group == 'FSA.juv', SSR := 0.07597605*0.2]

# frá christiansen
RReco.params$diet[Group == 'FBP', CEP := 0.07]
RReco.params$diet[Group == 'FKR', CEP := 0.153]
RReco.params$diet[Group == 'ZL', CEP := 0.463+0.153]
RReco.params$diet[Group == 'ZS', CEP := 0.153]

# Krill
#An Ecopath model for the Norwegian Sea and Barents Sea January 2001 Villy ChristensenVilly ChristensenBjørnar EllertsenCecilie KvammeCecilie KvammeShow all 6 authorsSigurd Tjelmeland
RReco.params$diet[Group == 'Phytoplankton', FKR := 0.5]
RReco.params$diet[Group == 'Detritus', FKR := 0.25]
RReco.params$diet[Group == 'ZL', FKR := 0.25]

# PWN - shrimps
#macison 
RReco.params$diet[Group == 'FEP', PWN := 0.150]
RReco.params$diet[Group == 'FIN', PWN := 0.1]
RReco.params$diet[Group == 'Phytoplankton', PWN := 0.27]
RReco.params$diet[Group == 'PWN', PWN := 0.01]
RReco.params$diet[Group == 'ZL', PWN := 0.15]
RReco.params$diet[Group == 'Detritus', PWN := 0.320]

# FEP - epifaunal. 
# We used Trites et al.’s (1999) estimates of Q/B from
# the Bering Sea: for ‘Infauna’ this was 12.0 and for
# ‘Epifauna’ it was 5.77. 

# We used the P/Bs given for the Bering Sea by Trites et al. (1999).
# The diet composition for epifauna is based on trites et al. which bases it on each of these groups was
# based on descriptions in Zatsepin and Rittikh (1968)
# and the models mentioned above.
# I used mean pb and qb for epi and infaunal

RReco.params$diet[Group == 'FIN', FEP := 0.45]
RReco.params$diet[Group == 'Phytoplankton', FEP := 0.25]
RReco.params$diet[Group == 'FEP', FEP := 0.10]
RReco.params$diet[Group == 'Detritus', FEP := 0.2]

# FIN - infaunal

RReco.params$diet[Group == 'Detritus', FIN := 1]


# Q/B values for both groups were assumed to be the
# same as in the Bering Sea model, i.e. 22 yr-1. Locally
# derived P/B values of 1.5 for carnivorous zooplankton
# and 4 for herbivorous zooplankton were available from
# Sakshaug (1997), i.e.. P/B values in the range 1.6 - 11.6
# have been used for cold water zooplankton in other
# models (Trites et al., 1999; Bundy et al., 2000 and Okey
#         and Pauly, 1999). Diet composition for zooplankton
# groups were based on Mackinson (2002)

# ZL - Carnivorus zooplancton
RReco.params$diet[Group == 'ZL', ZL := 0.05]
RReco.params$diet[Group == 'ZS', ZL := 0.55]
RReco.params$diet[Group == 'Phytoplankton', ZL := 0.25]
RReco.params$diet[Group == 'Detritus', ZL := 0.15]

# ZS - small herbivorus zooplankton
RReco.params$diet[Group == 'Phytoplankton', ZS := 0.95]
RReco.params$diet[Group == 'Detritus', ZS := 0.05]


# ZG - Gelatinous zooplankton
# The gelatinous zooplankton are jellyfish, i.e. moon jellyfish (Aurelia aurita), blue jellyfish (Cyanea
# lamarckii) and lion’s mane jellyfish (Cyanea capillata).
# We assumed that the gelatinous zooplankton
# biomass, production and production-consumption ratio 
# in the southern North Sea are the same as from
# the southern North Sea model (Stäbler et al., 2016). T
# he biomass is 0.09 t km-2y-1, the production is 2.86y-1 and 
# the production-consumption ratio is 0.45.
# https://www.researchgate.net/profile/Steven-Pint/publication/349494264_Ecopath_model_of_the_southern_North_Sea/links/6033848da6fdcc37a842b7a8/Ecopath-model-of-the-southern-North-Sea.pdf

RReco.params$diet[Group == 'CEP', ZG := 0.0736]
RReco.params$diet[Group == 'ZL', ZG := 0.294]
RReco.params$diet[Group == 'ZS', ZG := 0.294]
RReco.params$diet[Group == 'PWN', ZG := 0.02516]
RReco.params$diet[Group == 'FLC', ZG := 0.0485]
RReco.params$diet[Group == 'FEP', ZG := 0.018]
RReco.params$diet[Group == 'Phytoplankton', ZG := 0.147]

#trites et al - https://open.library.ubc.ca/soa/cIRcle/collections/facultyresearchandpublications/52383/items/1.0348097
#FLCL large crabs # https://www.researchgate.net/profile/Steven-Pint/publication/349494264_Ecopath_model_of_the_southern_North_Sea/links/6033848da6fdcc37a842b7a8/Ecopath-model-of-the-southern-North-Sea.pdf

# Published Q/B estimates for this group are 3.0
# (Mackinson, 2002) and 1.480 (Trites et al., 1999). The
# latter was used for an initial input. We used a P/B
# value of 7.69 (Trites et al., 1999), based on data from
# the Bering Sea.
RReco.params$diet[Group == 'FDF', FLC := 0.240+0.222]

RReco.params$diet[Group == 'FLC', FLC := 0.021]
RReco.params$diet[Group == 'FEP', FLC := 0.287+0.020+0.049]
RReco.params$diet[Group == 'FIN', FLC := 0.293+0.104]
RReco.params$diet[Group == 'PWN', FLC := 0.001]
RReco.params$diet[Group == 'Discards', FLC := 0.128]
RReco.params$diet[Group == 'Phytoplankton', FLC := 0.049+0.049]


# RReco.params$diet[Group == 'ZL', FLC := 1.75e-05]
# RReco.params$diet[Group == 'ZS', FLC := 3.49e-05]
# RReco.params$diet[Group == 'FLC', FLC := 2.02e-03]
# RReco.params$diet[Group == 'FEP', FLC := 9.4e-01]
# RReco.params$diet[Group == 'PWN', FLC := 4.18e-04]
# RReco.params$diet[Group == 'Detritus', FLC := 9.43e-05]
# RReco.params$diet[Group == 'Discards', FLC := 5.73e-02]

# LOBster
# Diet composition was assumed to be
# the same as in the North Sea (Mackinson, 2002).
RReco.params$diet[Group == 'FEP', LOB := 0.250]
RReco.params$diet[Group == 'FIN', LOB := 0.450]
RReco.params$diet[Group == 'Phytoplankton', LOB := 0.2]
RReco.params$diet[Group == 'Detritus', LOB := 0.05]
RReco.params$diet[Group == 'FDF', LOB := 0.1]

#minnka afrán flatfiska á 

RReco.params$diet[Group == 'FHA.juv', FFF := 0.000001]
RReco.params$diet[Group == 'FCD.juv', FFF := 0.000001]

RReco.params$diet[Group == 'FDC', FDF := NA]
RReco.params$diet[Group == 'FRF', FDF := NA]



#Fix ratio between juv and adult in FDC stomachs
RReco.params$diet[Group == 'FHA.juv', FDC := 0.01054922]
RReco.params$diet[Group == 'FHA.adult', FDC := 0.02794003]

# additional

#take out 1979 in haddock juv diet. Add mean values

RReco.params$diet$FHA.juv <-NA

RReco.params$diet[Group == 'CEP', FHA.juv := 0.0244787525]
RReco.params$diet[Group == 'FBP', FHA.juv := 0.0206094712]
RReco.params$diet[Group == 'FCA', FHA.juv := 0.0805481629]
RReco.params$diet[Group == 'FCD.juv', FHA.juv := 0.0074236572]
RReco.params$diet[Group == 'FDC', FHA.juv := 0.0022742779]
RReco.params$diet[Group == 'FDF', FHA.juv := 0.0087959133]
RReco.params$diet[Group == 'FEP', FHA.juv := 0.0171059039]
RReco.params$diet[Group == 'FFF', FHA.juv :=0.0049766525 ]
RReco.params$diet[Group == 'FHA.juv', FHA.juv := 0.0046034460]
RReco.params$diet[Group == 'FHE', FHA.juv := 0.0066829713]
RReco.params$diet[Group == 'FIN', FHA.juv := 0.3148198792]
RReco.params$diet[Group == 'FKR', FHA.juv := 0.1048004127]
RReco.params$diet[Group == 'FLC', FHA.juv := 0.0201625662]
RReco.params$diet[Group == 'FMI', FHA.juv := 0.0028558262]
RReco.params$diet[Group == 'FOC', FHA.juv := 0.0000000000]
RReco.params$diet[Group == 'FRF', FHA.juv := 0.0071632441]
RReco.params$diet[Group == 'FSD', FHA.juv := 0.0926316824]
RReco.params$diet[Group == 'LOB', FHA.juv := 0.0065213620]
RReco.params$diet[Group == 'PWN', FHA.juv := 0.0849665924]
RReco.params$diet[Group == 'ZG', FHA.juv := 0.0072587295]
RReco.params$diet[Group == 'ZL', FHA.juv := 0.0328991558]
RReco.params$diet[Group == 'ZS', FHA.juv := 0.0000486934]

RReco.params$diet[Group == 'FRF', FFF := 0.0000000000001]

RReco.params$diet[Group == 'FDF', FHE := NA]


RReco.params$diet$FGH <-NA

RReco.params$diet[Group == 'CEP', FGH := 5.258917e-02]
RReco.params$diet[Group == 'FBP', FGH := 6.325694e-03]
RReco.params$diet[Group == 'FCA', FGH := 2.312088e-01]
RReco.params$diet[Group == 'FCD.juv', FGH := 2.926810e-03]
RReco.params$diet[Group == 'FDC', FGH := 7.537532e-03]
RReco.params$diet[Group == 'FDF', FGH := 9.415324e-02]
RReco.params$diet[Group == 'FEP', FGH :=7.006420e-04 ]
RReco.params$diet[Group == 'FFF', FGH := 2.299908e-04]
RReco.params$diet[Group == 'FGH', FGH := 2.024867e-03]
RReco.params$diet[Group == 'FHA.juv', FGH := 1.368089e-03]

RReco.params$diet[Group == 'FHE', FGH := 7.066075e-02]
RReco.params$diet[Group == 'FIN', FGH := 2.318876e-04]
RReco.params$diet[Group == 'FKR', FGH := 4.649519e-02]
RReco.params$diet[Group == 'FLC', FGH := 2.205066e-05]
RReco.params$diet[Group == 'FMI', FGH := 1.129914e-01 ]
RReco.params$diet[Group == 'FOC', FGH := 1.154696e-04]
RReco.params$diet[Group == 'FRF', FGH :=  7.525914e-03]
RReco.params$diet[Group == 'FSA.juv', FGH := 2.221663e-03]
RReco.params$diet[Group == 'PWN', FGH :=3.632242e-02 ]
RReco.params$diet[Group == 'SSR', FGH := 2.190840e-03]
RReco.params$diet[Group == 'ZG', FGH :=1.095420e-04 ]
RReco.params$diet[Group == 'ZL', FGH := 3.598715e-03]



RReco.params$diet[is.na(RReco.params$diet)] <- 0
# Sum diet to 1
# for following groups:
colSums(as.data.frame(RReco.params$diet)[,2:dim(RReco.params$diet)[2]])

RReco.params$diet$FGH <- 100 / colSums(as.data.frame(RReco.params$diet$FGH)) * RReco.params$diet$FGH / 100
RReco.params$diet$FCD.juv <- 100 / colSums(as.data.frame(RReco.params$diet$FCD.juv)) * RReco.params$diet$FCD.juv / 100
RReco.params$diet$FCD.adult <- 100 / colSums(as.data.frame(RReco.params$diet$FCD.adult)) * RReco.params$diet$FCD.adult / 100
RReco.params$diet$FHA.juv <- 100 / colSums(as.data.frame(RReco.params$diet$FHA.juv)) * RReco.params$diet$FHA.juv / 100
RReco.params$diet$FHA.adult <- 100 / colSums(as.data.frame(RReco.params$diet$FHA.adult)) * RReco.params$diet$FHA.adult / 100
RReco.params$diet$FSA.juv <- 100 / colSums(as.data.frame(RReco.params$diet$FSA.juv )) * RReco.params$diet$FSA.juv  / 100
RReco.params$diet$FSA.adult <- 100 / colSums(as.data.frame(RReco.params$diet$FSA.adult )) * RReco.params$diet$FSA.adult  / 100


RReco.params$diet$CEP <- 100 / colSums(as.data.frame(RReco.params$diet$CEP)) * RReco.params$diet$CEP / 100
RReco.params$diet$FRF <- 100 / colSums(as.data.frame(RReco.params$diet$FRF)) * RReco.params$diet$FRF / 100
RReco.params$diet$FHE <- 100 / colSums(as.data.frame(RReco.params$diet$FHE)) * RReco.params$diet$FHE / 100
RReco.params$diet$FCA <- 100 / colSums(as.data.frame(RReco.params$diet$FCA)) * RReco.params$diet$FCA / 100
RReco.params$diet$FMI <- 100 / colSums(as.data.frame(RReco.params$diet$FMI)) * RReco.params$diet$FMI / 100
RReco.params$diet$FFF <- 100 / colSums(as.data.frame(RReco.params$diet$FFF)) * RReco.params$diet$FFF / 100
RReco.params$diet$SSR <- 100 / colSums(as.data.frame(RReco.params$diet$SSR)) * RReco.params$diet$SSR / 100
RReco.params$diet$SSD <- 100 / colSums(as.data.frame(RReco.params$diet$SSD)) * RReco.params$diet$SSD / 100
RReco.params$diet$SSH <- 100 / colSums(as.data.frame(RReco.params$diet$SSH)) * RReco.params$diet$SSH / 100
RReco.params$diet$PIN <- 100 / colSums(as.data.frame(RReco.params$diet$PIN)) * RReco.params$diet$PIN / 100
RReco.params$diet$WMW <- 100 / colSums(as.data.frame(RReco.params$diet$WMW)) * RReco.params$diet$WMW / 100
RReco.params$diet$WHB <- 100 / colSums(as.data.frame(RReco.params$diet$WHB)) * RReco.params$diet$WHB / 100
RReco.params$diet$WHT <- 100 / colSums(as.data.frame(RReco.params$diet$WHT)) * RReco.params$diet$WHT / 100
RReco.params$diet$LOB <- 100 / colSums(as.data.frame(RReco.params$diet$LOB)) * RReco.params$diet$LOB / 100
RReco.params$diet$FDF <- 100 / colSums(as.data.frame(RReco.params$diet$FDF)) * RReco.params$diet$FDF / 100
RReco.params$diet$FBP <- 100 / colSums(as.data.frame(RReco.params$diet$FBP)) * RReco.params$diet$FBP / 100
RReco.params$diet$FOC <- 100 / colSums(as.data.frame(RReco.params$diet$FOC)) * RReco.params$diet$FOC / 100
RReco.params$diet$FDC <- 100 / colSums(as.data.frame(RReco.params$diet$FDC)) * RReco.params$diet$FDC / 100
RReco.params$diet$FSD <- 100 / colSums(as.data.frame(RReco.params$diet$FSD)) * RReco.params$diet$FSD / 100
RReco.params$diet$FIN <- 100 / colSums(as.data.frame(RReco.params$diet$FIN)) * RReco.params$diet$FIN / 100
RReco.params$diet$FEP <- 100 / colSums(as.data.frame(RReco.params$diet$FEP)) * RReco.params$diet$FEP / 100
RReco.params$diet$FLC <- 100 / colSums(as.data.frame(RReco.params$diet$FLC)) * RReco.params$diet$FLC / 100
RReco.params$diet$ZG <- 100 / colSums(as.data.frame(RReco.params$diet$ZG)) * RReco.params$diet$ZG / 100
RReco.params$diet$ZL <- 100 / colSums(as.data.frame(RReco.params$diet$ZL)) * RReco.params$diet$ZL / 100
RReco.params$diet$ZS <- 100 / colSums(as.data.frame(RReco.params$diet$ZS)) * RReco.params$diet$ZS / 100
RReco.params$diet$SB <- 100 / colSums(as.data.frame(RReco.params$diet$SB)) * RReco.params$diet$SB / 100
RReco.params$diet$FKR <- 100 / colSums(as.data.frame(RReco.params$diet$FKR)) * RReco.params$diet$FKR / 100
RReco.params$diet$WTO <- 100 / colSums(as.data.frame(RReco.params$diet$WTO)) * RReco.params$diet$WTO / 100
RReco.params$diet[is.na(RReco.params$diet)] <- 0



RReco.params <- rpath.stanzas(RReco.params)
#RReco.params<- get(load("RReco.params_sep2.RData"))

source('pedigree.R')



# Biomass accumilation
# bio.index<- read.csv("fit_biomass_jan.csv") %>% dplyr::select("RPATH" = "Group", "YEAR"= "Year", "B"= "Value") %>% data.table()
# bio.index %>% filter(RPATH=="FCD.juv", YEAR %in% c(1,2)) %>% summarise(B[2]-B[1])
# bio.index %>% filter(RPATH=="FCD.adult", YEAR %in% c(1,2)) %>% summarise(B[2]-B[1])
# bio.index %>% filter(RPATH=="FHA.juv", YEAR %in% c(1,2)) %>% summarise(B[2]-B[1])
# bio.index %>% filter(RPATH=="FHA.adult", YEAR %in% c(1,2)) %>% summarise(B[2]-B[1])
# bio.index %>% filter(RPATH=="FSA.juv", YEAR %in% c(1,2)) %>% summarise(B[2]-B[1])
# bio.index %>% filter(RPATH=="FSA.adult", YEAR %in% c(1,2)) %>% summarise(B[2]-B[1])
# bio.index %>% filter(RPATH=="FFF", YEAR %in% c(1,2)) %>% summarise(B[2]-B[1])
# bio.index %>% filter(RPATH=="FGH", YEAR %in% c(1,2)) %>% summarise(B[2]-B[1])
# bio.index %>% filter(RPATH=="FRF", YEAR %in% c(1,2)) %>% summarise(B[2]-B[1])
# bio.index %>% filter(RPATH=="FOC", YEAR %in% c(1,2)) %>% summarise(B[2]-B[1])
# bio.index %>% filter(RPATH=="FDC", YEAR %in% c(1,2)) %>% summarise(B[2]-B[1])
# bio.index %>% filter(RPATH=="FHE", YEAR %in% c(1,2)) %>% summarise(B[2]-B[1])
# bio.index %>% filter(RPATH=="FCA", YEAR %in% c(1,2)) %>% summarise(B[2]-B[1])


RReco.params$model[Group %in% c("FCD.juv"), BioAcc := -9939.059/ 165513.080]
RReco.params$model[Group %in% c("FCD.adult"), BioAcc := 41878.39/517632.800 ]  
RReco.params$model[Group %in% c("FHA.juv"), BioAcc := 1725.2/39982.043]
RReco.params$model[Group %in% c("FHA.adult"), BioAcc :=  -49925.87/108323.000]
RReco.params$model[Group %in% c("FSA.juv"), BioAcc :=  -14.99713/54185.863]
RReco.params$model[Group %in% c("FSA.adult"), BioAcc :=  -8598.562/127945.600]
RReco.params$model[Group %in% c("FGH"), BioAcc := -1925.85/147582.300]
RReco.params$model[Group %in% c("FRF"), BioAcc := -20303.93/496681.400]
RReco.params$model[Group %in% c("FOC"), BioAcc :=  -8579.192/46153.380]
RReco.params$model[Group %in% c("FDC"), BioAcc := -1952.816/138212.700]
RReco.params$model[Group %in% c("FHE"), BioAcc := -55639/322215.000]
RReco.params$model[Group %in% c("FCA"), BioAcc :=   45940.14/1764054.600 ]

#save(RReco.params, file = "REco.params_final.RData")

#save(RReco.params, file = "REco.params_januar.RData")

##load("RReco.params.RData")

stanzaplot(RReco.params, StanzaGroup = 2)
check.rpath.params(RReco.params)

REco <- Rpath::rpath(RReco.params, eco.name = 'Iceland Ecosystem')
REco
save(RReco.params, file = "REco.params_Iceland.RData")



# Web plot



REco$Group["FCD.juv"] <- "Cod (0-3)"
REco$Group["FCD.adult"] <- "Cod (4+)"
REco$Group["FHA.juv"] <- "Haddock (0-2)"
REco$Group["FHA.adult"] <- "Haddock (3+)"
REco$Group["FSA.adult"] <- "Saithe (4+)"
REco$Group["FSA.juv"] <- "Saithe (0-3)"
REco$Group["FGH"] <- "Greenland halibut"
REco$Group["FRF"] <- "Redfish"
REco$Group["FHE"] <- "Herring"
REco$Group["FCA"] <- "Capelin"
REco$Group["FFF"] <- "Flatfish"
REco$Group["SSR"] <- "Skates & rays"
REco$Group["SSD"] <- "Small sharks"
REco$Group["SSH"] <- "Large sharks"
REco$Group["PIN"] <- "Seals"
REco$Group["PWN"] <- "Shrimp"
REco$Group["FOC"] <- "Other Codfish"
REco$Group["FDC"] <- "Dem. comm. fish"
REco$Group["FDF"] <- "Demersal fish"
REco$Group["FSD"] <- "Sandeel"
REco$Group["FBP"] <- "Small pelagic fish"
REco$Group["CEP"] <- "Cephalopods"
REco$Group["WMW"] <- "Minke whale"
REco$Group["FMI"] <- "Migratory fish"
REco$Group["FEP"] <- "Epifauna"
REco$Group["FIN"] <- "Infauna"
REco$Group["FKR"] <- "Krill"
REco$Group["ZL"] <- "Zooplankton large"
REco$Group["ZS"] <- "Zooplankton small"
REco$Group["ZG"] <- "Gelatinous zooplankton"
REco$Group["FLC"] <- "Lobsters & crabs"
REco$Group["WHB"] <- "Baleen whale"
REco$Group["WHT"] <- "Tooth whale"
REco$Group["WTO"] <- "Delphinidae"
REco$Group["SB"] <- "Seabirds"
REco$Group["GILLNETS"] <- "Gillnets"
REco$Group["LONGLINE"] <- "Longlines"
REco$Group["HARPOON"] <- "Harpoon"
REco$Group["TRAWLS"] <- "Trawls"
REco$Group["OTHER"] <- "Other gear"
REco$Group["LOB"] <- "Nephrops"
REco$Group["SEINERS"] <- "Seiners"
REco$Group["PELAGIC"] <- "Pelagic gear"

webplot_mod <- function(
    Rpath.obj,
    eco.name = attr(Rpath.obj, "eco.name"),
    line.col = "grey",
    highlight = NULL,
    highlight.col = c("black","red","orange"),
    labels = FALSE,
    label.pos = NULL,
    label.num = FALSE,
    label.cex = 1,
    fleets = FALSE,
    type.col = "black",
    box.order = NULL,
    # --- new args ---
    link.by = c("none","DC","flow","logflow"),  # link width mapping
    lwd.range = c(0.5, 6),                      # min/max line width
    flow.eps = 1e-12                            # tiny offset to avoid log(0)
){
  link.by <- match.arg(link.by)
  TL <- TLlevel <- type <- n <- x.space <- x.offset <- Group <- x.pos <- GroupNum <- NULL
  
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar), add = TRUE)
  
  pointmap <- data.table(
    GroupNum = 1:length(Rpath.obj$TL),
    Group    = Rpath.obj$Group,
    type     = Rpath.obj$type,
    TL       = Rpath.obj$TL,
    Biomass  = Rpath.obj$Biomass
  )
  
  # Bin TL to rows
  pointmap[TL < 2,                `:=`(TLlevel, 1)]
  pointmap[TL >= 2   & TL < 3,    `:=`(TLlevel, 2)]
  pointmap[TL >= 3   & TL < 3.5,  `:=`(TLlevel, 3)]
  pointmap[TL >= 3.5 & TL < 4,    `:=`(TLlevel, 4)]
  pointmap[TL >= 4   & TL < 4.5,  `:=`(TLlevel, 5)]
  pointmap[TL >= 4.5 & TL < 5,    `:=`(TLlevel, 6)]
  pointmap[TL >= 5,                `:=`(TLlevel, 7)]
  
  if (!is.null(box.order)) pointmap <- pointmap[box.order, ]
  if (fleets == FALSE)      pointmap <- pointmap[type < 3, ]
  
  nTL <- table(pointmap[, TLlevel])
  pointmap[, `:=`(n, nTL[which(names(nTL) == TLlevel)]), by = TLlevel]
  pointmap[, `:=`(x.space, 1/n)]
  pointmap[, `:=`(x.offset, x.space/2)]
  
  # Horizontal positions within each TL row
  x.count.all <- NULL
  for (i in 1:max(pointmap[, TLlevel])) {
    x.count <- pointmap[TLlevel == i, .(Group)]
    if (nrow(x.count) > 0) {
      x.count[, x.count := .I]
      x.count.all <- rbind(x.count.all, x.count)
    }
  }
  pointmap <- merge(pointmap, x.count.all, by = "Group", all.x = TRUE)
  pointmap[x.count == 1, `:=`(x.pos, x.offset + rnorm(1, 0, 0.01))]
  pointmap[x.count != 1, `:=`(x.pos, x.offset + x.space * (x.count - 1) + rnorm(1, 0, 0.01))]
  pointmap[, c("TLlevel","n","x.offset","x.space","x.count") := NULL]
  
  # Plot canvas
  ymin <- min(pointmap[, TL]) - 0.1 * min(pointmap[, TL])
  ymax <- max(pointmap[, TL]) + 0.1 * max(pointmap[, TL])
  plot(0, 0, ylim = c(ymin, ymax), xlim = c(0, 1), typ = "n", xlab = "", ylab = "", axes = FALSE)
  if (!is.null(eco.name)) mtext(3, text = eco.name, cex = 1.5)
  axis(2, las = TRUE); box()
  mtext(2, text = "Trophic Level", line = 2)
  
  # Matrices we’ll use
  tot.catch <- Rpath.obj$Landings + Rpath.obj$Discards        # [groups x gears]
  DC        <- Rpath.obj$DC                                    # [prey x predator]
  B         <- Rpath.obj$Biomass                               # length = groups
  QB        <- Rpath.obj$QB                                    # length = groups
  
  # --------- Build link weights for width scaling ----------
  # Predator → prey links
  Wpred <- NULL
  if (link.by == "DC") {
    Wpred <- DC
  } else if (link.by %in% c("flow","logflow")) {
    # consumption by predator j on prey i: DC[i,j] * (QB[j] * B[j])
    cons.pred <- QB * B
    cons.pred[!is.finite(cons.pred)] <- 0
    Wpred <- sweep(DC, 2, cons.pred, "*")
  }
  
  # Fleet → prey links (use catch to represent intensity)
  Wgear <- if (link.by == "none") NULL else tot.catch
  
  # Range for scaling (global)
  make_scaler <- function(Wpred, Wgear, lwd.range, link.by, eps){
    if (link.by == "none") {
      return(function(x) rep(1, length(x)))
    }
    vals <- c(if(!is.null(Wpred)) as.numeric(Wpred) else NULL,
              if(!is.null(Wgear)) as.numeric(Wgear) else NULL)
    vals <- vals[is.finite(vals) & vals > 0]
    if (length(vals) == 0) {
      return(function(x) rep(mean(lwd.range), length(x)))
    }
    if (link.by == "logflow") {
      vals <- log10(vals + eps)
      vmin <- min(vals); vmax <- max(vals)
      function(x){
        x <- log10(x + eps)
        if (vmax == vmin) return(rep(mean(lwd.range), length(x)))
        lwd.range[1] + (x - vmin) * diff(lwd.range) / (vmax - vmin)
      }
    } else {
      vmin <- min(vals); vmax <- max(vals)
      function(x){
        if (vmax == vmin) return(rep(mean(lwd.range), length(x)))
        lwd.range[1] + (x - vmin) * diff(lwd.range) / (vmax - vmin)
      }
    }
  }
  scale_lwd <- make_scaler(Wpred, Wgear, lwd.range, link.by, flow.eps)
  
  # --------- Draw all links (thin/variable by weight) ----------
  pred.idx <- pointmap[!type %in% 1:2, GroupNum]  # predators + fleets
  for (i in pred.idx) {
    pred.x <- pointmap[GroupNum == i, x.pos]
    pred.y <- pointmap[GroupNum == i, TL]
    
    if (pointmap[GroupNum == i, type] == 0) {
      # biological predator
      prey <- which(DC[, i] > 0)
      if (length(prey)) {
        prey.x <- pointmap[GroupNum %in% prey, x.pos]
        prey.y <- pointmap[GroupNum %in% prey, TL]
        w <- switch(link.by,
                    "none"    = rep(1, length(prey)),
                    "DC"      = DC[prey, i],
                    "flow"    = Wpred[prey, i],
                    "logflow" = Wpred[prey, i])
        lwdv <- scale_lwd(w)
        for (j in seq_along(prey)) {
          lines(c(pred.x, prey.x[j]), c(pred.y, prey.y[j]),
                col = line.col, lwd = lwdv[j])
        }
      }
    }
    
    if (pointmap[GroupNum == i, type] == 3) {
      # fishing gear
      gear.num <- i - (Rpath.obj$NUM_GROUPS - Rpath.obj$NUM_GEARS)
      prey <- which(tot.catch[, gear.num] > 0)
      if (length(prey)) {
        prey.x <- pointmap[GroupNum %in% prey, x.pos]
        prey.y <- pointmap[GroupNum %in% prey, TL]
        w <- if (link.by == "none") rep(1, length(prey)) else tot.catch[prey, gear.num]
        lwdv <- scale_lwd(w)
        for (j in seq_along(prey)) {
          lines(c(pred.x, prey.x[j]), c(pred.y, prey.y[j]),
                col = line.col, lwd = lwdv[j])
        }
      }
    }
  }
  
  # --------- Highlight block (kept colors; now widths also scale) ----------
  if (!is.null(highlight)) {
    if (is.character(highlight)) highlight <- which(Rpath.obj$Group == highlight)
    
    pred.x <- pointmap[GroupNum == highlight, x.pos]
    pred.y <- pointmap[GroupNum == highlight, TL]
    
    if (pointmap[GroupNum == highlight, type] < 1) {
      prey       <- which(DC[, highlight] > 0)
      group.pred <- which(DC[highlight, ] > 0)
      fleet.pred <- which(tot.catch[highlight, ] > 0)
    }
    if (pointmap[GroupNum == highlight, type] %in% c(1:2)) {
      prey       <- NULL
      group.pred <- which(DC[highlight, ] > 0)
      fleet.pred <- which(tot.catch[highlight, ] > 0)
    }
    if (pointmap[GroupNum == highlight, type] == 3) {
      gear.num   <- highlight - (Rpath.obj$NUM_GROUPS - Rpath.obj$NUM_GEARS)
      prey       <- which(tot.catch[, gear.num] > 0)
      group.pred <- NULL
      fleet.pred <- NULL
    }
    
    if (!is.null(prey) && length(prey)) {
      prey.x <- pointmap[GroupNum %in% prey, x.pos]
      prey.y <- pointmap[GroupNum %in% prey, TL]
      w <- switch(link.by,
                  "none"    = rep(1, length(prey)),
                  "DC"      = DC[prey, highlight],
                  "flow"    = Wpred[prey, highlight],
                  "logflow" = Wpred[prey, highlight])
      lwdv <- scale_lwd(w)
      for (j in seq_along(prey)) {
        lines(c(pred.x, prey.x[j]), c(pred.y, prey.y[j]),
              col = highlight.col[1], lwd = lwdv[j])
      }
    }
    
    if (!is.null(group.pred) && length(group.pred)) {
      gp.x <- pointmap[GroupNum %in% group.pred, x.pos]
      gp.y <- pointmap[GroupNum %in% group.pred, TL]
      # weights for incoming pred->highlight links are DC[highlight, predator]
      w <- switch(link.by,
                  "none"    = rep(1, length(group.pred)),
                  "DC"      = DC[highlight, group.pred],
                  "flow"    = Wpred[highlight, group.pred],
                  "logflow" = Wpred[highlight, group.pred])
      lwdv <- scale_lwd(w)
      for (j in seq_along(group.pred)) {
        lines(c(pred.x, gp.x[j]), c(pred.y, gp.y[j]),
              col = highlight.col[2], lwd = lwdv[j])
      }
    }
    
    if (!is.null(fleet.pred) && length(fleet.pred)) {
      gear.num <- fleet.pred + (Rpath.obj$NUM_GROUPS - Rpath.obj$NUM_GEARS)
      fl.x <- pointmap[GroupNum %in% gear.num, x.pos]
      fl.y <- pointmap[GroupNum %in% gear.num, TL]
      w <- if (link.by == "none") rep(1, length(fleet.pred)) else tot.catch[highlight, fleet.pred]
      lwdv <- scale_lwd(w)
      for (j in seq_along(fleet.pred)) {
        lines(c(pred.x, fl.x[j]), c(pred.y, fl.y[j]),
              col = highlight.col[3], lwd = lwdv[j])
      }
    }
    
    legend("bottomleft", legend = c("Prey","Predator","Fleet"),
           lty = 1, col = highlight.col, lwd = 2, ncol = 3, xpd = TRUE, inset = c(0, -0.1))
    legend("topright", legend = pointmap[GroupNum == highlight, Group], bty = "n")
  }
  
  # --------- Points & legend ----------
  if (!is.null(label.pos) | labels == FALSE) {
    if (length(type.col) == 4) {
      legend("bottomright", legend = c("living","primary","detrital","fleet"),
             pch = 16, col = type.col, ncol = 4, xpd = TRUE, inset = c(0, -0.1))
    }
    if (length(type.col) < 4) type.col <- rep(type.col[1], 4)
    points(pointmap[type < 1, x.pos], pointmap[type < 1, TL], pch = 16, col = type.col[1])
    points(pointmap[type == 1, x.pos], pointmap[type == 1, TL], pch = 16, col = type.col[2])
    points(pointmap[type == 2, x.pos], pointmap[type == 2, TL], pch = 16, col = type.col[3])
    points(pointmap[type == 3, x.pos], pointmap[type == 3, TL], pch = 16, col = type.col[4])
  }
  
  if (labels) {
    if (!label.num) {
      text(pointmap[, x.pos], pointmap[, TL], pointmap[, Group], pos = label.pos, cex = label.cex, font =2)
    } else {
      text(pointmap[, x.pos], pointmap[, TL], pointmap[, GroupNum], pos = label.pos, cex = label.cex)
    }
  }
}


webplot_mod(REco, highlight = "Capelin", fleets = TRUE, labels = TRUE,
            link.by = "flow", lwd.range = c(1, 3))
