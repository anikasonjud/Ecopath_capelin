library(tidyverse)             
library(mfdb)
mdb<-mfdb('Iceland',db_params=list(host='mfdb.hafro.is'))
library(Rpath)
library(mar)
mar<-connect_mar()
library(data.table)

tyr <- lubridate::year(Sys.Date())
begyear<- 1996
endyear <-2023
fit.years <-1:100
REco.params<- get(load("REco.params_Iceland.RData"))
source("pedigree.R")
REco.params$model[Group %in% c('FCA'), QB :=  5.23*0.333] #5.475103] #  input_other.R 


scene0 <- readRDS("scene0.rds")


scene0<-read.fitting.biomass(scene0, "fit_biomass_november24.csv") #indices.R
scene0<-read.fitting.biomass(scene0, "fit_bio_w_minke.csv") #indices.R
scene0<-read.fitting.catch(scene0, "fit_land_w_minke.csv") #indices.R



unbal<-REco.params
bal <- rpath(unbal, eco.name = 'R Ecosystem')
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
Equil_species <- c("Phytoplankton","Detritus","FMI","WMW","SSR", "SSD", "SSH", "PIN", "WHB", "WHT",  "LOB", "FSD", "FDF", "FBP", "PWN", "FEP", "FIN", "FLC", "WTO", "CEP", "FKR", "ZG", "ZL", "ZS", "SB", "Phytoplankton", "Detritus")
for (sp in Equil_species){
  scene0 <- adjust.fishing(scene0, 'ForcedFRate', sp, fit.years, value=F_equil[sp])
}

# Fishing mortality 1996-2023 + mean fishing mortality for the rest of the years
update_forced_f_rate <- function(scene, catch_data, start_year = 1, end_year = 28) {
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


## new
update_forced_f_rate_moderate <- function(
    scene,
    catch_data,
    start_year = 1,
    end_year   = 28,          # historical years (1996–2023 in your setup)
    post_method = c("median", "trimmed10", "mean_window", "last"),
    window      = 10,         # for "mean_window"
    damp        = 1.00,       # e.g., 0.90 to reduce F by 10% post-history
    cap_quantile   = 0.80,    # cap F to <= 80th percentile of historical
    floor_quantile = 0.20,    # floor F to >= 20th percentile of historical
    f_min = 0,                # absolute lower bound
    f_max = Inf,              # absolute upper bound
    special = list(           # optional per-group overrides
      FCA = list(method = "mean_window", window = 2, damp = 1.00,
                 cap_q = 0.80, floor_q = 0.20)
    )
) {
  post_method <- match.arg(post_method)
  
  # Helper to compute a moderate baseline from historical F
  compute_baseline <- function(f_hist, method, window, damp, cap_q, floor_q) {
    f_hist <- f_hist[is.finite(f_hist)]  # guard against NAs
    if (length(f_hist) == 0) return(0)
    
    base <- switch(method,
                   median      = median(f_hist, na.rm = TRUE),
                   trimmed10   = mean(f_hist, trim = 0.10, na.rm = TRUE),
                   mean_window = {
                     w <- min(window, length(f_hist))
                     mean(tail(f_hist, w), na.rm = TRUE)
                   },
                   last        = tail(f_hist, 1)
    )
    
    # cap within historical quantile band
    hi <- as.numeric(quantile(f_hist, probs = cap_q,   na.rm = TRUE, names = FALSE))
    lo <- as.numeric(quantile(f_hist, probs = floor_q, na.rm = TRUE, names = FALSE))
    base <- max(min(base, hi), lo)
    
    # optional dampening
    base <- base * damp
    
    # hard bounds
    base <- max(min(base, f_max), f_min)
    base
  }
  
  # loop over caught groups
  for (grp in unique(catch_data$Group)) {
    idx <- which(catch_data$Group == grp)
    f_values <- catch_data$Fvalue[idx]
    
    # write historical F into years 1:end_year
    j <- match(grp, colnames(scene$fishing$ForcedFRate))
    if (is.na(j)) next
    
    scene$fishing$ForcedFRate[, j][start_year:end_year] <- f_values[start_year:end_year]
    
    # choose group-specific strategy if provided
    if (!is.null(special[[grp]])) {
      m  <- special[[grp]]$method  %||% post_method
      w  <- special[[grp]]$window  %||% window
      d  <- special[[grp]]$damp    %||% damp
      cq <- special[[grp]]$cap_q   %||% cap_quantile
      fq <- special[[grp]]$floor_q %||% floor_quantile
    } else {
      m  <- post_method
      w  <- window
      d  <- damp
      cq <- cap_quantile
      fq <- floor_quantile
    }
    
    # baseline from historical ONLY
    f_hist <- f_values[start_year:end_year]
    baseF  <- compute_baseline(f_hist, m, w, d, cq, fq)
    
    # fill post-history years
    if (end_year + 1 <= nrow(scene$fishing$ForcedFRate)) {
      scene$fishing$ForcedFRate[, j][(end_year + 1):nrow(scene$fishing$ForcedFRate)] <- baseF
    }
  }
  
  scene
}

# small helper for defaulting
`%||%` <- function(x, y) if (!is.null(x)) x else y


scene0 <- update_forced_f_rate_moderate(
  scene0, scene0$fitting$Catch,
  post_method = "median",
  damp        = 0.90              # reduce F by 10% after history
)


# Optimizing

test_sp <- c("SSR","WMW","FCD.juv", "FCD.adult","FHA.juv","FHA.adult","FSA.juv","FSA.adult", "FGH","FRF","FHE","FCA", "FOC","FDC" ,"PIN")
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




# not mzero
fit_values   <- c(rep(0,length(test_sp)),rep(0,length(test_sp))) 
fit_species  <- c(test_sp,test_sp)
fit_vartype  <- c(rep("predvul",length(test_sp)),
                  rep("preyvul",length(test_sp)))


# Run optimization
fit.optim    <- optim(fit_values, rsim.fit.run2, #lower=-3, upper=3, 
                      species=fit_species, vartype=fit_vartype, scene=scene0,   
                      run_method='AB', years=fit.years) 


out_values <- fit.optim$par


rsim.fit.update2<-function(values, species, vartype, scene){
  scene$params <- rsim.fit.apply2(values, species, vartype, scene$params) 
  return(scene)
}

scene_new_compare <- rsim.fit.update2(out_values, fit_species, fit_vartype, scene0)

saveRDS(scene_new_compare, file = "scene_new_compare2.rds")


