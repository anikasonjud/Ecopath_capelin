## Fitting functions from Aydin in Rpath version 1.2.0

################################################################################
#'@useDynLib Rpath
#'@export
read.fitting.biomass <- function(SCENE, filename){
  
  # TODO KYA 7/24/24: add intelligent warning if missing column (e.g. Type)
  
  # Base variables
  SIM   <- SCENE
  years <- as.numeric(row.names(SCENE$fishing$ForcedFRate))
  species <- SIM$params$spname
  
  cdat  <-read.csv(filename)
  missing_sp <- unique(cdat$Group[which(!(cdat$Group %in% species))])
  if(length(missing_sp)>0){
    warning("Following species in BIOMASS fit data not in model, dropped: ",missing_sp)
  }
  
  cmdat <- cdat[cdat$Group %in% species,]
  c0dat <- cmdat[!is.na(cmdat$Value)& cmdat$Year %in% years ,]
  ccdat <- c0dat[c0dat$Value>0 & c0dat$Stdev>0 ,]
  
  #type <- as.character(rep("absolute",length(ccdat$YEAR)))
  ccdat$Year  <- as.character(ccdat$Year)
  ccdat$Group <- as.character(ccdat$Group) 
  obs  <- ifelse(as.numeric(ccdat$Scale)<0, as.numeric(ccdat$Value),
                 as.numeric(ccdat$Value) * as.numeric(ccdat$Scale))   
  sd   <- ifelse(as.numeric(ccdat$Scale)<0, as.numeric(ccdat$Stdev),
                 as.numeric(ccdat$Stdev) * as.numeric(ccdat$Scale))   
  wt   <- rep(1,length(obs))
  initial_q   <- rep(1,length(obs))
  SIM$fitting$Biomass <- cbind(ccdat,obs,sd,initial_q,wt)
  
  return(SIM)
}

################################################################################
#'@export
read.fitting.catch <- function(SCENE, filename){
  
  # TODO KYA 7/24/24: add intelligent warning if missing column 
  
  SIM   <- SCENE
  years <- as.numeric(row.names(SCENE$fishing$ForcedFRate))
  # Columns needed
  #  Group	Year	Value	SD	Scale   
  cdat  <- read.csv(filename)
  missing_sp <- unique(cdat$Group[which(!(cdat$Group %in% SIM$params$spname))])
  if(length(missing_sp)>0){
    warning("Following species in CATCH fit data not in model, dropped: ",missing_sp)
  }
  
  cmdat <- cdat[cdat$Group %in% SIM$params$spname,]
  ccdat <- cmdat[!is.na(cmdat$Value) & cmdat$Year %in% years,] 
  ccdat$Year  <- as.character(ccdat$Year)
  ccdat$Group <- as.character(ccdat$Group) 
  obs  <- as.numeric(ccdat$Value) * as.numeric(ccdat$Scale)   
  sd   <- as.numeric(ccdat$Stdev) * as.numeric(ccdat$Scale)  
  wt   <- rep(1,length(obs))
  SIM$fitting$Catch <- cbind(ccdat,obs,sd,wt)
  #sdat  <- aggregate(as.numeric(ccdat$Value)*as.numeric(ccdat$Scale),list(ccdat$Year,ccdat$Group),"sum")
  #sd    <- 0.1*sdat$x
  #colnames(SIM$fitting$CATCH) <- c("year","species","obs","sd","wt")
  
  # Apply fit fishing to matrix
  #SIM$fishing$ForcedEffort[] <- 0
  #SIM$fishing$ForcedCatch[matrix(c(SIM$fitting$Catch$Year, SIM$fitting$Catch$Group),
  #                        length(SIM$fitting$Catch$Year),2)] <- SIM$fitting$Catch$obs
  return(SIM)
}

################################################################################
#'@export
fitcatch.to.forcecatch <- function(SCENE){
  SIM <- SCENE
  #SIM$fishing$ForcedEffort[] <- 0
  SIM$fishing$ForcedCatch[matrix(c(SIM$fitting$Catch$Year, SIM$fitting$Catch$Group),
                                 length(SIM$fitting$Catch$Year),2)] <- SIM$fitting$Catch$obs
  return(SIM)
}

################################################################################
#'@export
rsim.plot.catch <- function(scene, run, species){
  qdat <- scene$fitting$Catch[scene$fitting$Catch$Group==species,]
  mn   <- qdat$obs
  up   <- mn + 1.96*qdat$sd
  dn   <- mn - 1.96*qdat$sd 
  tot <- 0 #sum(qdat$fit)
  plot(as.numeric(rownames(run$annual_Catch)),run$annual_Catch[,species],type="l",
       ylim=c(0,max(up,run$annual_Catch[,species])),xlab=tot,ylab="")
  mtext(side=2, line=2.2, paste(species,"catch"), font=2, cex=1.0)
  points(as.numeric(qdat$Year),mn)
  segments(as.numeric(qdat$Year),y0=up,y1=dn)
}

################################################################################
#'@export
rsim.plot.biomass <- function(scene, run, species){
  bio.obj <- rsim.fit.obj(scene,run)$Biomass 
  qdat <- bio.obj[bio.obj$Group==species,]
  #survey_q <- 1
  mn   <- qdat$obs_scaled #* qdat$survey_q   #/survey_q
  up   <- mn + 1.96*qdat$sd * qdat$survey_q #/survey_q
  dn   <- mn - 1.96*qdat$sd * qdat$survey_q #/survey_q 
  tot  <- sum(qdat$fit)
  plot(as.numeric(rownames(run$annual_Biomass)),run$annual_Biomass[,species],type="l",
       ylim=c(0,max(up,run$annual_Biomass[,species])),xlab=tot,ylab="")
  mtext(side=2, line=2.2, paste(species,"biomass"), font=2, cex=1.0)
  points(as.numeric(qdat$Year),mn)
  segments(as.numeric(qdat$Year),y0=up,y1=dn)
}
#################################################################################
#'@export
rsim.fit.obj <- function(SIM,RES,verbose=TRUE){
  FLOGTWOPI <- 0.5*log(2*pi) #0.918938533204672
  epsilon <- 1e-36
  
  OBJ <- list()
  OBJ$tot <- 0
  
  # BIOMASS to NON-RESCALED "Actual" biomass estimate
  est <- RES$annual_Biomass[matrix(c(as.character(SIM$fitting$Biomass$Year),as.character(SIM$fitting$Biomass$Group)),
                                   ncol=2)] + epsilon
  obs <- SIM$fitting$Biomass$obs + epsilon
  sd  <- SIM$fitting$Biomass$sd  + epsilon
  wt  <- SIM$fitting$Biomass$wt
  initial_q <- SIM$fitting$Biomass$initial_q
  # We need to get variance-weighted survey means by species, for
  # calculating mean values needed for setting best-fit q
  
  # Formula for weighted average q: 
  # q = exp(sum(w * log(obs/est))/sum(w)) where w is wt/sd  
  logdiff       <- log(obs/est)
  sdlog         <- sqrt(log(1.0+sd*sd/(obs*obs))) # sigma^2 of lognormal dist 
  wt_sd_inverse <- wt/sdlog# sd
  wt_logdiffsum <- tapply(logdiff*wt_sd_inverse, as.character(SIM$fitting$Biomass$Group),sum)
  wt_sum        <- tapply(wt_sd_inverse,         as.character(SIM$fitting$Biomass$Group),sum)
  q_est         <- exp(wt_logdiffsum/wt_sum) # need ifelse here for 0 weights?
  survey_q      <- ifelse(SIM$fitting$Biomass$Type=="index", 
                          q_est[as.character(SIM$fitting$Biomass$Group)], initial_q)
  ## Jan 2023 incorrect code
  #inv_var <- 1.0/(sd*sd)
  #obs_sum <- tapply(obs*inv_var*wt, as.character(SIM$fitting$Biomass$Group),sum)
  #inv_sum <- tapply(inv_var*wt,     as.character(SIM$fitting$Biomass$Group),sum)
  #obs_mean <- obs_sum/inv_sum
  #est_mean <- tapply(est,as.character(SIM$fitting$Biomass$Group),mean)
  #survey_q <- ifelse(SIM$fitting$Biomass$Type=="absolute", 1.0,
  #            #(obs_mean/est_mean)[as.character(SIM$fitting$Biomass$Group)])
  #            (est_mean/obs_mean)[as.character(SIM$fitting$Biomass$Group)])
  #obs_scaled <-obs*survey_q 
  #sdlog  <- sqrt(log(1.0+sd*sd*survey_q*survey_q/(obs_scaled*obs_scaled)))
  sdiff  <- log((obs/survey_q)/est)/sdlog
  fit    <- wt * (FLOGTWOPI + log(sdlog) + 0.5*sdiff*sdiff)
  
  if (verbose){
    obs_scaled  <- obs/survey_q
    OBJ$Biomass <- cbind(SIM$fitting$Biomass,est,survey_q,obs_scaled,sdiff,fit)
  } else {
    OBJ$tot <- OBJ$tot + sum(fit)
  }
  
  # Catch compared (assumes all catch is clean, absolute values)
  est <- RES$annual_Catch[matrix(c(as.character(SIM$fitting$Catch$Year),as.character(SIM$fitting$Catch$Group)),
                                 ncol=2)] + epsilon
  obs <- SIM$fitting$Catch$obs + epsilon
  sd  <- SIM$fitting$Catch$sd  + epsilon
  sdlog  <- sqrt(log(1.0+sd*sd/(obs*obs)))
  sdiff  <- log(obs/est)/sdlog
  fit    <- SIM$fitting$Catch$wt * (log(sdlog) + FLOGTWOPI + 0.5*sdiff*sdiff)
  if (verbose){
    OBJ$Catch <- cbind(SIM$fitting$Catch,est,sdiff,fit)
  } else {
    OBJ$tot <- OBJ$tot + sum(fit)
  }
  
  # # RATION
  # obs <- SIM$fitting$ration$obs + epsilon
  # sd  <- SIM$fitting$ration$sd  + epsilon
  # inv_var <- (1.0/sd)*(1.0/sd)
  # obs_sum <- tapply(obs*inv_var,as.character(SIM$fitting$ration$Group),sum)
  # inv_sum <- tapply(inv_var,as.character(SIM$fitting$ration$Group),sum)
  # obs_mean <- obs_sum/inv_sum
  # est <- RES$annual_QB[matrix(c(as.character(SIM$fitting$ration$Year),as.character(SIM$fitting$ration$Group)),
  #                             ncol=2)] + epsilon
  # est_mean <- tapply(est,as.character(SIM$fitting$ration$Group),mean)
  # survey_q <- (obs_mean/est_mean)[as.character(SIM$fitting$ration$Group)]
  # est_scaled <-est*survey_q 
  # sdlog  <- sqrt(log(1.0+sd*sd/(obs*obs)))
  # sdiff  <- (log(obs)-log(est_scaled))/sdlog
  # fit    <- SIM$fitting$ration$wt * (log(sdlog) + FLOGTWOPI + 0.5*sdiff*sdiff)
  # OBJ$ration <- cbind(GOA_SIM$fitting$ration,est,survey_q,est_scaled,sdiff,fit)
  # 
  # # Diet proportions estimation
  # linklook   <- matrix(c(as.character(SIM$fitting$diets$Year),as.character(SIM$fitting$diets$simlink)),ncol=2)
  # totlook    <- matrix(c(as.character(SIM$fitting$diets$Year),as.character(SIM$fitting$diets$pred)),ncol=2) 
  # dietTot    <- tapply(RES$annual_Qlink[linklook],list(SIM$fitting$diets$Year,SIM$fitting$diets$pred),sum)
  # dietProp   <- RES$annual_Qlink[linklook]/dietTot[totlook]
  # logest     <- log(dietProp)
  # #NEGATIVE log likelihood now
  # fit        <- -SIM$fitting$diets$wt * (SIM$fitting$diets$log_diff + SIM$fitting$diets$alphaM1*logest)  
  # OBJ$diet   <- cbind(SIM$fitting$diets,dietProp,logest,fit)
  
  # Final summation and return
  if(verbose){
    OBJ$tot <- sum(OBJ$Biomass$fit, OBJ$Catch$fit)# , OBJ$ration$fit, OBJ$diet$fit)
    return(OBJ)
  }
  else{
    return(OBJ$tot)
  }
}
#################################################################################
#'@export
rsim.fit.table <- function(SIM,RES){
  fitobj  <- rsim.fit.obj(SIM,RES,verbose=T)
  Btmp <- tapply(fitobj$Biomass$fit,fitobj$Biomass$Group,sum)
  Ctmp <- tapply(fitobj$Catch$fit,fitobj$Catch$Group,sum)
  out <- rep(NA,length(SIM$params$spname)); names(out)<- SIM$params$spname
  Biomass <- out; Biomass[names(Btmp)] <- Btmp
  Catch <- out;   Catch[names(Ctmp)] <- Ctmp
  return(data.frame(Biomass,Catch))
}
#################################################################################
#'@export
rsim.fit.obj.species <- function(SIM,RES,species=NULL){
  OBJ <- list()
  fitobj <- rsim.fit.obj(SIM,RES,verbose=T)
  OBJ$Biomass <- fitobj$Biomass[fitobj$Biomass$Group%in%species,] 
  OBJ$Catch   <- fitobj$Catch[fitobj$Catch$Group%in%species,]
  return(OBJ)
}

#################################################################################
#Internal Only
rsim.fit.apply <- function(values, species, vartype, scene.params){
  mzerodiff <- values[vartype=="mzero"]
  mzero.sp  <- species[vartype=="mzero"]
  
  predvuls <- values[vartype=="predvul"]
  names(predvuls) <- species[vartype=="predvul"]   
  preddiff <- as.numeric(predvuls[scene.params$spname[scene.params$PreyTo+1]])
  preddiff[is.na(preddiff)] <- 0
  
  preyvuls <- values[vartype=="preyvul"]
  names(preyvuls) <- species[vartype=="preyvul"]   
  preydiff <- as.numeric(preyvuls[scene.params$spname[scene.params$PreyFrom+1]])
  preydiff[is.na(preydiff)] <- 0
  
  scene.params$MzeroMort[mzero.sp] <- scene.params$MzeroMort[mzero.sp] + mzerodiff
  scene.params$VV <- (1 + exp(log(scene.params$VV-1) + preddiff + preydiff))
  
  return(scene.params)
}
#################################################################################
#'@export
rsim.fit.run <- function(values, species, vartype, scene, run_method, verbose=F, ...){
  scene$params <- rsim.fit.apply(values, species, vartype, scene$params)
  run.out <- rsim.run(scene, method=run_method, ...)
  if(!verbose){ return(rsim.fit.obj(scene, run.out, FALSE))}
  else{         return(run.out)}
}
#################################################################################
#'@export
rsim.fit.update <- function(values, species, vartype, scene){
  scene$params <- rsim.fit.apply(values, species, vartype, scene$params) 
  return(scene)
}

#################################################################################

################################################################################
## 01  LIBRARIES, CONNECTIONS AND SOURCING
################################################################################

library(tidyverse)             
library(mfdb)
library(Rpath)
library(mar)
library(data.table)
library(patchwork)

mdb<-mfdb('Iceland',db_params=list(host='mfdb.hafro.is'))
mar<-connect_mar()
tyr <- lubridate::year(Sys.Date())

begyear<- 1996
endyear <-2024
fit.years <-1:100
REco.params<- get(load("REco.params_Iceland.RData"))
source("pedigree.R")
unbal<-REco.params
bal <- rpath(unbal, eco.name = 'R Ecosystem')
scene0 <- readRDS("scene0.rds")
scene0 <- rsim.scenario(bal,unbal, years=1:100)
scene0<-read.fitting.biomass(scene0, "minke_fit.csv") #indices.R
scene0<-read.fitting.catch(scene0, "fit_landings_oktober25.csv") #indices.R
scene0 <- adjust.fishing(scene0, "ForcedEffort", rpath.gears(bal), fit.years, value=0.0)

# Fit recruitment

df <- tbl_mar(mar, 'ops$will."advice_assessment"') %>%
  filter(species == 1, year %in% c(begyear:endyear), assessment_year == tyr) %>%
  group_by(year) %>% select(year, value = median_recruitment) %>%
  ungroup() %>% mutate(mean_rec = mean(value),
         rec_relative_to_mean = value / mean_rec) %>% collect() 

for(i in 1:nrow(df)){
  year <- df$year[i] - 1995  
  value <- df$rec_relative_to_mean[i]  
    scene0 <- adjust.forcing(scene0, 'ForcedRecs', 'FCD.adult', sim.year = year, value = value)
}

df2 <- tbl_mar(mar, 'ops$will."advice_assessment"') %>%
  filter(species == 2, year %in% c(begyear:endyear), assessment_year == tyr) %>%
  group_by(year) %>%
  select(year, value = median_recruitment) %>%
  ungroup() %>%
  mutate(mean_rec = mean(value),
         rec_relative_to_mean = value / mean_rec) %>%
  collect()  

for(i in 1:nrow(df2)){
  year <- df2$year[i] - 1995 
  value <- df2$rec_relative_to_mean[i] 
  scene0 <- adjust.forcing(scene0, 'ForcedRecs', 'FHA.adult', sim.year = year, value = value)
}

df3 <- tbl_mar(mar, 'ops$will."advice_assessment"') %>%
  filter(species == 3, year %in% c(begyear:endyear), assessment_year == tyr) %>%
  group_by(year) %>%
  select(year, value = median_recruitment) %>%
  ungroup() %>%
  mutate(mean_rec = mean(value),
         rec_relative_to_mean = value / mean_rec) %>%
  collect()  

for(i in 1:nrow(df3)){
  year <- df3$year[i] - 1995 
  value <- df3$rec_relative_to_mean[i] 
  scene0 <- adjust.forcing(scene0, 'ForcedRecs', 'FSA.adult', sim.year = year, value = value)
}



# For species without catch, reapply Ecopath F (originally through gears) to ForcedFRate
F_equil <- (rowSums(bal$Landings) + rowSums(bal$Discards))/(bal$Biomass) 
Equil_species <- c("SSR", "SSD", "SSH", "PIN", "WHB", "WHT", "FSD", "FDF", "FBP", "PWN", "FEP", "FIN", "FLC", "WTO", "CEP", "FKR", "ZG", "ZL", "ZS", "SB", "Phytoplankton", "Detritus")
for (sp in Equil_species){
  scene0 <- adjust.fishing(scene0, 'ForcedFRate', sp, fit.years, value=F_equil[sp])
}


# Fishing mortality 1996-2024 + mean fishing mortality for the rest of the years
update_forced_f_rate <- function(scene, catch_data, start_year = 1, end_year = 29) {
  unique_groups <- unique(catch_data$Group)
  
  for (group in unique_groups) {
    group_indices <- which(catch_data$Group == group)
    f_values <- catch_data$Fvalue[group_indices]
    
    # Update F values for the first part (1:end_year)
    scene$fishing$ForcedFRate[, match(group, colnames(scene$fishing$ForcedFRate))][1:end_year] <- f_values[1:end_year]
    
    # Apply two-year mean for FCA group, five-year mean for others
    if (group == "FCA") {
      mean_f_value <- mean(f_values[(length(f_values)-1):length(f_values)], na.rm = TRUE)
    } else if (length(f_values) > 5) {
      mean_f_value <- mean(f_values[(length(f_values)-4):length(f_values)], na.rm = TRUE)
    }
    
    # Update F values for the second part (end_year + 1:100)
    scene$fishing$ForcedFRate[, match(group, colnames(scene$fishing$ForcedFRate))][(end_year + 1):100] <- mean_f_value
  }
  
  return(scene)
}



scene0 <- update_forced_f_rate(
  scene0, scene0$fitting$Catch)


# Optimizing

test_sp <- c("FCD.juv", "FCD.adult","FHA.juv","FHA.adult","FSA.juv","FSA.adult", "FGH","FRF","FHE","FCA", "FOC","FDC")
data_type <- "index"
# Set data weightings for all data input low (zeros not allowed)
scene0$fitting$Biomass$wt[] <- 1e-36
scene0$fitting$Catch$wt[]   <- 1e-36
# Set data type for test species
scene0$fitting$Biomass$Type[scene0$fitting$Biomass$Group %in% test_sp] <- data_type
# Set data weighting for one species to fit to 1
scene0$fitting$Biomass$wt[scene0$fitting$Biomass$Group %in% test_sp]   <- 1
scene0$fitting$Catch$wt[scene0$fitting$Catch$Group %in% test_sp]   <- 1

#sharks
scene0$params$FtimeAdj[15]<-0.5
scene0$params$FtimeAdj[16]<-0.5
scene0$params$FtimeAdj[66]<-0.5
# Marine mammals
scene0$params$FtimeAdj[17]<-0.5
scene0$params$FtimeAdj[26]<-0.5
scene0$params$FtimeAdj[27]<-0.5
scene0$params$FtimeAdj[28]<-0.5
scene0$params$FtimeAdj[29]<-0.5



fit_values   <- c(rep(0,length(test_sp)),rep(0,length(test_sp))) 
fit_species  <- c(test_sp,test_sp)
fit_vartype  <- c(rep("predvul",length(test_sp)),
                  rep("preyvul",length(test_sp)))


# Run optimization
fit.optim    <- optim(fit_values, rsim.fit.run, #lower=-3, upper=3, 
                      species=fit_species, vartype=fit_vartype, scene=scene0,   
                      run_method='AB', years=fit.years) 

out_values <- fit.optim$par

scene_new_compare <- rsim.fit.update(out_values, fit_species, fit_vartype, scene0)
saveRDS(scene_new_compare, file = "scene_new_compare25.rds")
scene_new_compare <- readRDS("scene_new_compare25.rds")  # fitted Ecosim scenario

run_0 <- rsim.run(scene_new_compare, method='AB', years=fit.years)
fitcatch.to.forcecatch(scene_new_compare)

# Plotting

REco$Group["FCD.juv"]   <- "Cod (0–3)"
REco$Group["FCD.adult"] <- "Cod (4+)"
REco$Group["FHA.juv"]   <- "Haddock (0–2)"
REco$Group["FHA.adult"] <- "Haddock (3+)"
REco$Group["FSA.juv"]   <- "Saithe (0–3)"
REco$Group["FSA.adult"] <- "Saithe (4+)"
REco$Group["FGH"]       <- "Greenland halibut"
REco$Group["FRF"]       <- "Redfish"
REco$Group["FHE"]       <- "Herring"
REco$Group["FCA"]       <- "Capelin"
REco$Group["FFF"]       <- "Flatfish"
REco$Group["SSR"]       <- "Skates & rays"
REco$Group["SSD"]       <- "Small sharks"
REco$Group["SSH"]       <- "Large sharks"
REco$Group["PIN"]       <- "Seals"
REco$Group["PWN"]       <- "Shrimp"
REco$Group["FOC"]       <- "Other codfish"
REco$Group["FDC"]       <- "Dem. comm. fish"
REco$Group["FDF"]       <- "Demersal fish"
REco$Group["FSD"]       <- "Sandeel"
REco$Group["FBP"]       <- "Small pelagic fish"
REco$Group["CEP"]       <- "Cephalopods"
REco$Group["WMW"]       <- "Minke whale"
REco$Group["FMI"]       <- "Migratory fish"
REco$Group["FEP"]       <- "Epifauna"
REco$Group["FIN"]       <- "Infauna"
REco$Group["FKR"]       <- "Krill"
REco$Group["ZL"]        <- "Zooplankton (large)"
REco$Group["ZS"]        <- "Zooplankton (small)"
REco$Group["ZG"]        <- "Gelatinous zooplankton"
REco$Group["FLC"]       <- "Lobsters & crabs"
REco$Group["WHB"]       <- "Baleen whales"
REco$Group["WHT"]       <- "Tooth whales"
REco$Group["WTO"]       <- "Delphinidae"
REco$Group["SB"]        <- "Seabirds"
REco$Group["LOB"]       <- "Nephrops"
REco$Group["GILLNETS"]  <- "Gillnets"
REco$Group["LONGLINE"]  <- "Longlines"
REco$Group["HARPOON"]   <- "Harpoon"
REco$Group["TRAWLS"]    <- "Trawls"
REco$Group["OTHER"]     <- "Other gear"
REco$Group["SEINERS"]   <- "Seiners"
REco$Group["PELAGIC"]   <- "Pelagic gear"



bio_rel <- sweep(run_0$annual_Biomass, 2, run_0$annual_Biomass[1, ], "/")
species_list <- setdiff(colnames(bio_rel), c("Outside", "Detritus", "Phytoplankton"))
bio_fit <- rsim.fit.obj(scene_new_compare, run_0)$Biomass

name_map <- REco$Group
if (is.null(names(name_map))) names(name_map) <- names(REco$Group)

plots <- lapply(species_list, function(sp) {
  qdat <- subset(bio_fit, Group == sp)
  title_name <- ifelse(sp %in% names(name_map), name_map[[sp]], sp)
  
  p <- ggplot() +
    # modelled relative biomass
    geom_line(
      aes(
        x = as.numeric(rownames(bio_rel)) + 1995,
        y = bio_rel[, sp]
      ),
      color = "black", linewidth = 0.6
    ) +
    labs(title = title_name, x = "", y = "") +
    scale_x_continuous(
      limits = c(1996, 2024),
      breaks = seq(1996, 2024, by = 5)
    ) +
    theme_light(base_size = 11) +
    theme(
      plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4)
    )
  
  # add observed data 
  if (nrow(qdat) > 0 && !all(is.na(qdat$obs_scaled))) {
    p <- p +
      geom_point(
        aes(
          x = as.numeric(qdat$Year) + 1995,
          y = qdat$obs_scaled / run_0$annual_Biomass[1, sp]
        ),
        color = "#1F78B4",
        size = 1
      )
  }
  
  p
})

combined_plot <- wrap_plots(plots[1:36], ncol = 3)
valid_catch_groups <- names(which(run_0$annual_Catch[1, ] > 0 &
                                  colSums(run_0$annual_Catch, na.rm = TRUE) > 0))


ggsave("files_for_capelin_paper/Figures/Relative_Biomass_AllSpecies_fit26.png",
       combined_plot, width = 10, height = 12, dpi = 300)


# 1. Relative catches 
catch_rel <- sweep(run_0$annual_Catch, 2, run_0$annual_Catch[1, ], "/")

catch_species <- setdiff(colnames(catch_rel), c("Outside", "Detritus", "Phytoplankton"))

# 2. Observed catch fits from Rpath
catch_fit <- rsim.fit.obj(scene_new_compare, run_0)$Catch

name_map <- REco$Group
if (is.null(names(name_map))) names(name_map) <- names(REco$Group)

# 3. Plot loop 
catch_plots <- lapply(catch_species, function(sp) {
  
  qdat <- subset(catch_fit, Group == sp)
  
  title_name <- ifelse(sp %in% names(name_map), name_map[[sp]], sp)
  
  p <- ggplot() +
    # modelled relative catch
    geom_line(
      aes(
        x = as.numeric(rownames(catch_rel)) + 1995,
        y = catch_rel[, sp]
      ),
      color = "black", linewidth = 0.6
    ) +
    labs(title = title_name, x = "", y = "") +
    scale_x_continuous(
      limits = c(1996, 2024),
      breaks = seq(1996, 2024, by = 5)
    ) +
    theme_light(base_size = 11) +
    theme(
      plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4)
    )
  
  # ==== Add observed catch points =====
  if (nrow(qdat) > 0 && !all(is.na(qdat$obs))) {
    p <- p +
      geom_point(
        aes(
          x = as.numeric(qdat$Year) + 1995,
          y = qdat$obs/ run_0$annual_Catch[1, sp]
        ),
        color = "#E31A1C",   
        size = 1
      )
  }
  
  p
})

#  4. Combine 
combined_catch_plot <- wrap_plots(catch_plots, ncol = 4)
combined_catch_plot

ggsave("Relative_Catch_AllSpecies_fit.png",
       combined_catch_plot, width = 10, height = 12, dpi = 300)
