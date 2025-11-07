library(tidyverse)             
library(mfdb)
mdb<-mfdb('Iceland',db_params=list(host='mfdb.hafro.is'))
library(Rpath)
library(mar)
mar<-connect_mar()
library(data.table)

tyr <- lubridate::year(Sys.Date())
begyear<- 1996
endyear <-2024
fit.years <-1:100
REco.params<- get(load("REco.params_Iceland.RData"))
source("pedigree.R")

scene0 <- readRDS("scene0.rds")
scene0<-read.fitting.biomass(scene0, "minke_fit.csv") #indices.R
scene0<-read.fitting.catch(scene0, "fit_landings_okt25.csv") #indices.R

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


# small helper for defaulting
`%||%` <- function(x, y) if (!is.null(x)) x else y


scene0 <- update_forced_f_rate(
  scene0, scene0$fitting$Catch)


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
saveRDS(scene_new_compare, file = "scene_new_compare.rds")
run_0 <- rsim.run(scene_new_compare, method='AB', years=fit.years)


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
REco$Group["WHT"]       <- "Toothed whales"
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
  
  # add observed data (if available)
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

combined_plot <- wrap_plots(plots, ncol = 3)
combined_plot

ggsave("Relative_Biomass_AllSpecies_fit.png",
       combined_plot, width = 10, height = 12, dpi = 300)
