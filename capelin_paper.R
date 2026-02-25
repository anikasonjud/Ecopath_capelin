library(data.table)
library(Rpath)
library(mfdb)
library(mar)
library(dplyr)
library(tidyr)
library(purrr)

mdb <- mfdb('Iceland', db_params = list(host='mfdb.hafro.is'))
mar <- connect_mar()

fit.years <- 1:100
mccores <- 40

REco.params <- get(load("REco.params_Iceland.RData"))
source("pedigree.R")
REco <- rpath(REco.params, eco.name = 'R Ecosystem')

ISL_unbal <- REco.params
ISL_bal   <- rpath(ISL_unbal, eco.name = 'R Ecosystem')

scene_new_compare <- readRDS("scene_new_compare.rds")  # fitted Ecosim scenario
scene_base        <- scene_new_compare                  

# post-history F based on five-year mean
update_forced_f_rate_5yrmean <- function(scene,
                                         catch_data = NULL,
                                         end_year = 29,
                                         damp = 1.00,
                                         special = list(FCA = list(method = "mean_window",
                                                                   window = 2, damp = 1.00))) {
  
  stopifnot(!is.null(scene$fishing$ForcedFRate))
  FF <- scene$fishing$ForcedFRate
  if (!is.matrix(FF)) FF <- as.matrix(FF)
  
  nT <- nrow(FF)
  nG <- ncol(FF)
  if (nT < end_year + 1) {
    stop("ForcedFRate has only ", nT, " rows; end_year=", end_year,
         " requires at least ", end_year + 1, " rows.")
  }
  
  # compute mean of last 5 years 
  get_postF <- function(x) {
    hist <- x[seq_len(end_year)]
    hist <- hist[is.finite(hist)]
    if (!length(hist)) return(0)
    # use last 5 years or shorter if less available
    w <- min(5, length(hist))  
    mean(tail(hist, w), na.rm = TRUE) * damp
  }
  
  #Default post-history F for all groups
  default_post <- apply(FF, 2, get_postF)
  
  # Special treatment for Capelin (FCA)
  if (length(special)) {
    for (nm in names(special)) {
      if (nm %in% colnames(FF)) {
        spec <- special[[nm]]
        hist <- FF[seq_len(end_year), nm, drop = TRUE]
        hist <- hist[is.finite(hist)]
        if (!length(hist)) next
        
        if (identical(spec$method, "mean_window")) {
          w <- max(1, min(length(hist), spec$window %||% 2))
          v <- mean(tail(hist, w), na.rm = TRUE)
          v <- v * (spec$damp %||% 1.0)
        } else {
          v <- get_postF(FF[, nm, drop = TRUE])
        }
        default_post[nm] <- v
      }
    }
  }
  
  #  Apply post-history F values to projection years
  if (end_year < nT) {
    FF[(end_year + 1):nT, ] <- matrix(rep(default_post, each = nT - end_year),
                                      nrow = nT - end_year, ncol = nG,
                                      byrow = FALSE,
                                      dimnames = list(NULL, colnames(FF)))
  }
  
  scene$fishing$ForcedFRate <- FF
  return(scene)
}


# Apply post-history F
scene_new_compare <- update_forced_f_rate_5yrmean(
  scene_new_compare, scene_new_compare$fitting$Catch,
  end_year = 29, damp = 1)


NUM_RUNS <- 100000
set.seed(666)
initial_bio <- REco$Biomass

max_allowed <- rep(Inf, length(initial_bio))
names(max_allowed) <- names(initial_bio)

if ("SB" %in% names(max_allowed)) max_allowed["SB"] <- 1.25       #+25% allowed
if ("WMW" %in% names(max_allowed)) max_allowed["WMW"] <- 1.25    #+25% allowed

results <- 
  1:NUM_RUNS %>%
  parallel::mclapply(function(i){
    ISLsense <- scene_new_compare
    parlist <- rsim.sense(
      ISLsense, ISL_unbal,
      Vvary = c(-0.5, 0.5),           
      Dvary = c(log(0.6), log(1.4))  
    )
    
    # Use a *screening* burn‑in to reject unstable parameter sets
    ISLsense$start_state$Biomass <- parlist$B_BaseRef
    parlist$BURN_YEARS <- 50
    ISLsense$params <- parlist
    
    test <- rsim.run(ISLsense, method = "AB", years = 1:100)
    
    # reject NAs
    if (any(is.na(test$end_state$Biomass))) {
      out <- list(kept = FALSE, parlist = NULL)
      return(out)
    }
    
    bio_traj <- test$annual_Biomass
    keep_groups <- setdiff(colnames(bio_traj), c("Outside", "Detritus", "Phytoplankton"))
    base <- pmax(bio_traj[51, keep_groups, drop = TRUE], 1e-12)
    ratio <- sweep(bio_traj[51:100, keep_groups, drop = FALSE], 2, base, "/")
    
    bio_min <- apply(ratio, 2, min, na.rm = TRUE)
    bio_max <- apply(ratio, 2, max, na.rm = TRUE)
    
    max_thresh <- max_allowed[keep_groups]
    min_thresh <- 1 / max_thresh
    
    kept <- !(any(bio_min < min_thresh, na.rm = TRUE) || any(bio_max > max_thresh, na.rm = TRUE))
    if (!kept) cat(i, ": rejected\n") else cat(i, ": kept\n") 
    out <- list(kept = kept,
                parlist = parlist)
    return(out)
    
  } 
  , mc.cores = mccores)


kept <- do.call("c", map(results, "kept"))
KEPT <- which(kept)
parlist <- map(results, "parlist")
isl.sense_final <- parlist[KEPT]

warm_start_scene <- function(scene, years = 1:30) {
  pre <- rsim.run(scene, method = "AB", years = years)
  scene$start_state <- pre$end_state
  scene$params$BURN_YEARS <- 0
  scene
}

check_stable <- function(bio_mat,
                         groups_exclude = c("Outside","Detritus","Phytoplankton"),
                         window = 5,
                         threshold = 0.02) {
  
  # Keep only biological groups
  keep_groups <- setdiff(colnames(bio_mat), groups_exclude)
  B <- as.matrix(bio_mat[, keep_groups, drop = FALSE])
  
  # Year-to-year absolute proportional change
  delta <- abs(diff(B) / pmax(B[-nrow(B), ], 1e-12))
  
  # Rolling 5-year max change
  library(zoo)
  roll_max <- apply(delta, 2, function(x) {
    zoo::rollapply(x, width = window, FUN = max,
                   align = "left", fill = NA)
  })
  
  stable_index <- which(apply(roll_max < threshold, 1, all))[1]
  
  if (is.na(stable_index)) {
    return(list(stable = FALSE, year = NA))
  }
  
  # +1 because diff removed first year
  return(list(stable = TRUE, year = stable_index + 1))
}
test <- rsim.run(scene_new_compare, method = "AB", years = 1:100)

stab <- check_stable(test$annual_Biomass)
set.seed(123)

BURN_IN <- 0  
perturb_start <- stab$year[[1]]   # perturbations start here 2041 (biomass has stabilized - fluctuates less than 2% between years)

results_capelin <-
  do.call("c",
          parallel::mclapply(1:length(KEPT), function(irun){
            
            out <- list()
            run.scene <- scene_new_compare
            run.scene$params <- isl.sense_final[[irun]]
            
            # start biomass = this draw’s Ecopath equilibrium
            run.scene$start_state$Biomass <- run.scene$params$B_BaseRef
            
            # ensure key groups are integrated 
            run.scene$params$NoIntegrate[c("FCD.adult","FCD.juv",
                                           "FHA.adult","FHA.juv",
                                           "FSA.adult","FSA.juv",
                                           "FCA")] <- 0
            
            # warm‑start (pre‑run to align state with params/forcings)
            run.scene <- warm_start_scene(run.scene, years = 1:29)
            
            ## --- Status Quo
            sq <- rsim.run(run.scene, method = "AB", years = fit.years)
            sq_dt <- as.data.table(sq$annual_Biomass)[, Year := 1:100][Year > BURN_IN]
            out[[paste0("Status_Quo_", irun)]] <- melt(
              sq_dt, id.vars = "Year", variable.name = "Group", value.name = "Biomass"
            )
            
            ## --- Reference FCA biomass (years 91–100, already > BURN_IN)
            bio.ref <- sq_dt[Year %in% 91:100, mean(FCA, na.rm = TRUE)]
            
            ## --- Capelin -50%
            s50 <- copy(run.scene)
            s50 <- adjust.forcing(s50, parameter = "ForcedBio", group = "FCA",
                                  sim.year = perturb_start:100, value = bio.ref * 0.5)
            
            
            r50 <- rsim.run(s50, method = "AB", years = fit.years)
            r50_dt <- as.data.table(r50$annual_Biomass)[, Year := 1:100][Year > BURN_IN]
            out[[paste0("Down_50_", irun)]] <- melt(
              r50_dt, id.vars = "Year", variable.name = "Group", value.name = "Biomass"
            )
            
            ## --- Capelin ~0
            s0 <- copy(run.scene)
            s0 <- adjust.forcing(s0, parameter = "ForcedBio", group = "FCA",
                                 sim.year = perturb_start:100, value = bio.ref * 1e-5)
            
            
            r0 <- rsim.run(s0, method = "AB", years = fit.years)
            r0_dt <- as.data.table(r0$annual_Biomass)[, Year := 1:100][Year > BURN_IN]
            out[[paste0("Down_Zero_", irun)]] <- melt(
              r0_dt, id.vars = "Year", variable.name = "Group", value.name = "Biomass"
            )  
            
            return(out)
            
          }, mc.cores = mccores)        
  )

#  Extract and save raw lists
results_down50 <- results_capelin[grep("Down_50_",  names(results_capelin))]
results_down0  <- results_capelin[grep("Down_Zero_", names(results_capelin))]
results_sq     <- results_capelin[grep("Status_Quo_", names(results_capelin))]

save(results_down50, file = "files_for_capelin_paper/results_down50.RData")
save(results_down0,  file = "files_for_capelin_paper/results_down0.RData")
save(results_sq,     file = "files_for_capelin_paper/results_sq.RData")

## load
load("files_for_capelin_paper/results_down50.RData")  # loads results_down50
load("files_for_capelin_paper/results_down0.RData")   # loads results_down0
load("files_for_capelin_paper/results_sq.RData")      # loads results_sq

#  Build wide data frames from raw lists
res_m1_down50 <- purrr::map_dfr(names(results_down50), function(irun) {
  results_down50[[irun]] |>
    tidyr::pivot_wider(names_from = Group, values_from = Biomass) |>
    mutate(Run = irun) |>
    rename_with(~paste0("Biomass.", .), -c(Year, Run))
}, .id = "Run_ID")

res_m1_down0 <- purrr::map_dfr(names(results_down0), function(irun) {
  results_down0[[irun]] |>
    tidyr::pivot_wider(names_from = Group, values_from = Biomass) |>
    mutate(Run = irun) |>
    rename_with(~paste0("Biomass.", .), -c(Year, Run))
}, .id = "Run_ID")

res_m1_sq <- purrr::map_dfr(names(results_sq), function(irun) {
  results_sq[[irun]] |>
    tidyr::pivot_wider(names_from = Group, values_from = Biomass) |>
    mutate(Run = irun) |>
    rename_with(~paste0("Biomass.", .), -c(Year, Run))
}, .id = "Run_ID")

#  Label scenarios
res_m1_sq     <- res_m1_sq     |> mutate(scenario = "Status Quo")
res_m1_down50 <- res_m1_down50 |> mutate(scenario = "Capelin -50%")
res_m1_down0  <- res_m1_down0  |> mutate(scenario = "Capelin zero")



as_long_runs <- function(df, label) {
  df |>
    pivot_longer(starts_with("Biomass."), names_to="Biomass_Type", values_to="Biomass") |>
    mutate(Biomass_Type = sub("^Biomass\\.", "", Biomass_Type),
           scenario = label)
}

# TL metadata
TL_table <- data.frame(
  Biomass_Type = paste0("Biomass.", names(REco$TL)),
  TL           = as.numeric(REco$TL),
  GroupType    = REco$Group
) |>
  filter(!Biomass_Type %in% c("Biomass.GILLNETS","Biomass.LONGLINE","Biomass.OTHER",
                              "Biomass.PELAGIC","Biomass.SEINERS","Biomass.TRAWLS",
                              "Biomass.HARPOON","Biomass.FCA")) |>
  mutate(TL_group = case_when(
    TL < 2 ~ "1–2",
    TL < 3 ~ "2–3",
    TL < 4 ~ "3–4",
    TRUE   ~ "4–5+"
  ),
  TL_group = factor(TL_group, levels = c("1–2","2–3","3–4","4–5+")))

# Long form across scenarios
sq_long    <- as_long_runs(res_m1_sq,     "Status Quo")
d50_long   <- as_long_runs(res_m1_down50, "Capelin -50%")
dzero_long <- as_long_runs(res_m1_down0,  "Capelin zero")

runs_long  <- bind_rows(sq_long, d50_long, dzero_long) |>
  mutate(Biomass_Type_key = paste0("Biomass.", Biomass_Type),
         Year_actual = Year + 1995)


## Filtering out unrealistic runs
# 1. Compute Rel for all runs
pred_runs_comp <- runs_long %>%
  group_by(Run_ID, Biomass_Type) %>%
  mutate(
    base = mean(Biomass[Year %in% 1:29], na.rm = TRUE),
    Rel  = Biomass / pmax(base, 1e-12)
  ) %>%
  ungroup()

# Compute max and min relative biomass per scenario × group × run
group_run_minmax <- pred_runs_comp %>%
  group_by(scenario, Biomass_Type, Run_ID) %>%
  summarise(
    maxRel = max(Rel, na.rm = TRUE),
    minRel = min(Rel, na.rm = TRUE),
    .groups = "drop"
  )

# Define upper (99%) AND lower (1%) extreme cutoffs
bad_runs_per_scenario <- group_run_minmax %>%
  group_by(scenario, Biomass_Type) %>%
  mutate(
    cutoff_high = quantile(maxRel, 0.975, na.rm = TRUE),
    cutoff_low  = quantile(minRel, 0.025, na.rm = TRUE)
  ) %>%
  filter(
    maxRel > cutoff_high |   # extreme spikes
      minRel < cutoff_low    # extreme collapses
  ) %>%
  distinct(Run_ID, scenario)


# list of bad runs (remove from scenarios)
bad_runs <- unique(bad_runs_per_scenario$Run_ID)
runs_long_filtered <- runs_long %>%
  filter(!Run_ID %in% bad_runs)

# 5. print results
cat("Total runs:             ", n_distinct(runs_long$Run_ID), "\n")
cat("Removed (spikes):       ", length(bad_runs), "\n")
cat("Kept:                   ", n_distinct(runs_long_filtered$Run_ID), "\n")
cat("Percent removed:        ",
    round(100 * length(bad_runs) / n_distinct(runs_long$Run_ID), 1), "%\n\n")


# FIGURE 1 — Capelin (relative, IQR only)
cap_runs <- runs_long_filtered %>%
  filter(Biomass_Type == "FCA") %>%
  group_by(scenario, Run_ID) %>%
  mutate(base = mean(Biomass[Year %in% 1:29], na.rm = TRUE),
         Rel  = Biomass / pmax(base, 1e-12)) %>%
  ungroup()

cap_q <- cap_runs %>%
  group_by(scenario, Year_actual) %>%
  summarize(
    Med = median(Rel, na.rm = TRUE),
    Q25 = quantile(Rel, 0.025, na.rm = TRUE),
    Q75 = quantile(Rel, 0.975, na.rm = TRUE),
    .groups = "drop")
pal <- c("Status Quo"   = "#2ca25f",
         "Capelin -50%" = "#2b6cb0",
         "Capelin zero" = "#e11d48")

Fig1 <- ggplot(cap_q, aes(x = Year_actual, y = Med, color = scenario)) +
  geom_ribbon(
    data = cap_q %>% filter(scenario == "Status Quo"),
    aes(ymin = Q25, ymax = Q75, fill = "95% CI"),
    alpha = 0.18, color = "NA",
    fill = "#2ca25f") +
  geom_line(size = 0.8) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = c("95% CI" = pal["Status Quo"])) +
  labs(x = "Year",
       y = expression("Relative biomass (B / B"[mean] * ")"),
       color = "Scenario",
       fill  = "") +
  scale_x_continuous(limits = c(1995, 2100)) +
  theme_bw(base_size = 13) +
  theme(legend.position = "top",
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())
Fig1

ggsave("files_for_capelin_paper/Figures/Fig1_Capelin_relative_IQR.png",
       Fig1, width = 120, height = 100, units = "mm", dpi = 350)

# FIGURE 2 — TL bins, per-run normalized (relative), IQR only
runs_tl <- runs_long_filtered %>%
  left_join(TL_table, by = c("Biomass_Type_key" = "Biomass_Type")) %>%
  filter(!is.na(TL_group)) %>%
  group_by(scenario, Run_ID, TL_group, Year_actual) %>%
  summarize(TLsum = sum(Biomass, na.rm = TRUE), .groups = "drop")

runs_tl_rel <- runs_tl %>%
  group_by(scenario, Run_ID, TL_group) %>%
  mutate(base = mean(TLsum[(Year_actual - 1995) %in% 1:29], na.rm = TRUE),
         Rel  = TLsum / pmax(base, 1e-12)) %>%
  ungroup()

tl_rel_q <- runs_tl_rel %>%
  group_by(scenario, TL_group, Year_actual) %>%
  summarize(
    Med = median(Rel, na.rm = TRUE),
    Q25 = quantile(Rel, 0.025, na.rm = TRUE),
    Q75 = quantile(Rel, 0.975, na.rm = TRUE),
    .groups = "drop"
  )
# --- plot function ---
capelin_plots <- function(df, x, yM, yL, yU,
                          color = "scenario", fill = "scenario",
                          title = "", ylab = "", facet = NULL, ncol = 2,
                          hline = NA, limits_year = c(1995, 2100),
                          baseline_scenario = "Status Quo",
                          perturb_start_year = 2041) {
  
  g <- ggplot(df, aes(x = .data[[x]], y = .data[[yM]])) +
    geom_ribbon(data = df %>%dplyr::filter(.data[[color]] == baseline_scenario),
                aes(ymin = .data[[yL]],ymax = .data[[yU]],fill = .data[[fill]]),
                alpha = 0.25,color = NA) +
    geom_ribbon(
      data = df %>%dplyr::filter(.data[[color]] != baseline_scenario,.data[[x]] >= perturb_start_year),
      aes(ymin = .data[[yL]],ymax = .data[[yU]],fill = .data[[fill]]),alpha = 0.18,color = NA) +
    geom_line(aes(color = .data[[color]]),size = 0.8) +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    labs(title = title,x = "Year",y = ylab,color = "Scenario",fill = "Scenario") +
    theme_bw(base_size = 13) +
    theme(legend.position = "top",strip.text = element_text(face = "bold"),panel.grid.minor = element_blank())
  if (!is.na(hline))
    g <- g + geom_hline(yintercept = hline, linetype = "dotted")
  if (!is.null(facet))
    g <- g + facet_wrap(as.formula(paste("~", facet)),
                        ncol = ncol, scales = "free_y")
  if (!is.null(limits_year))
    g <- g + scale_x_continuous(limits = limits_year)
  g
}


Fig2 <- capelin_plots(tl_rel_q %>% filter(Year_actual >= 1996),
                      x="Year_actual", yM="Med", yL="Q25", yU="Q75",
                      title = "",
                      ylab = expression("Relative biomass (B / B"[mean] * ")"),facet = "TL_group", ncol = 2, hline = 1)
Fig2


ggsave("files_for_capelin_paper/Figures/Fig2_TLbins_relative_IQR.png", Fig2, width = 10, height = 5.2, dpi = 350)



# FIGURE 3 — Key predators (relative)
key_pred_groups <- c("FCD.adult","FCD.juv","FHA.adult","FSA.adult","WMW","SB")  # <- final set

pred_runs <- runs_long_filtered%>%
  filter(Biomass_Type %in% key_pred_groups) %>%
  group_by(scenario, Run_ID, Biomass_Type) %>%
  mutate(base = mean(Biomass[Year %in% 1:29], na.rm = TRUE),
         Rel  = Biomass / pmax(base, 1e-12)) %>%
  ungroup()

pred_q <- pred_runs %>%
  group_by(scenario, Biomass_Type, Year_actual) %>%
  summarize(
    Med = median(Rel, na.rm = TRUE),
    Q25 = quantile(Rel, 0.025, na.rm = TRUE),
    Q75 = quantile(Rel, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Group = recode(Biomass_Type,
                        "FCD.juv"="Cod (juvenile)", "FCD.adult"="Cod (adult)",
                        "FHA.adult"="Haddock (adult)", "FSA.adult"="Saithe (adult)",
                        "WMW"="Minke whales", "SB"="Seabirds"))

Fig3 <- capelin_plots(pred_q %>% filter(Year_actual >= 1996),
                      x="Year_actual", yM="Med", yL="Q25", yU="Q75",
                      title = "",
                      ylab = expression("Relative biomass (B / B"[mean] * ")"),facet = "Group", ncol = 3, hline = 1)
Fig3
ggsave("files_for_capelin_paper/Figures/Fig3_KeyPredators_relative_IQR.png", Fig3, width = 10, height = 5, dpi = 350)

# FIGURE 4 — Ecosystem indicators (relative/delta), combined

runs_with_TL <- runs_long_filtered %>% 
  left_join(select(TL_table, Biomass_Type, TL), by = c("Biomass_Type_key" = "Biomass_Type")) %>% 
  filter(!is.na(TL))

ind_per_run <- runs_with_TL %>% group_by(scenario, Run_ID, Year_actual) %>% 
  summarize( TotalB = sum(Biomass, na.rm = TRUE), 
             MTLbio = sum(Biomass * TL, na.rm = TRUE) / pmax(sum(Biomass, na.rm = TRUE), 1e-12), 
             PredB = sum(Biomass[TL >= 3], na.rm = TRUE), 
             PreyB = sum(Biomass[TL < 3], na.rm = TRUE), 
             PP = PredB / pmax(PreyB, 1e-12), .groups = "drop" )
#without capelin detritus and phyto
ind_per_run <- runs_with_TL %>% filter(!Biomass_Type %in% c("Capelin", "Detritus", "Phytoplankton"))%>%
  group_by(scenario, Run_ID, Year_actual) %>% 
  summarize( TotalB = sum(Biomass, na.rm = TRUE), 
             MTLbio = sum(Biomass * TL, na.rm = TRUE) / pmax(sum(Biomass, na.rm = TRUE), 1e-12), 
             PredB = sum(Biomass[TL >= 3], na.rm = TRUE), 
             PreyB = sum(Biomass[TL < 3], na.rm = TRUE), 
             PP = PredB / pmax(PreyB, 1e-12), .groups = "drop" )

ind_base <- ind_per_run %>% filter((Year_actual - 1995) %in% 1:29) %>% 
  group_by(scenario, Run_ID) %>% 
  summarize( base_TotalB = mean(TotalB, na.rm = TRUE), 
             base_MTL = mean(MTLbio, na.rm = TRUE), 
             base_PP = mean(PP, na.rm = TRUE), .groups = "drop" )
ind_rel <- ind_per_run %>% left_join(ind_base, by = c("scenario","Run_ID")) %>% 
  mutate( RelTotalB = TotalB / pmax(base_TotalB, 1e-12), 
          DeltaMTL = MTLbio - base_MTL, 
          RelPP = PP / pmax(base_PP, 1e-12) )

ind_q <- ind_rel %>%
  group_by(scenario, Year_actual) %>% 
  summarise(
    TB_M = median(RelTotalB, na.rm = TRUE),
    TB_L = quantile(RelTotalB, 0.025, na.rm = TRUE),
    TB_U = quantile(RelTotalB, 0.975, na.rm = TRUE),
    MTL_M = median(DeltaMTL, na.rm = TRUE),
    MTL_L = quantile(DeltaMTL, 0.025, na.rm = TRUE),
    MTL_U = quantile(DeltaMTL, 0.975, na.rm = TRUE),
    PP_M = median(RelPP, na.rm = TRUE),
    PP_L = quantile(RelPP, 0.025, na.rm = TRUE),
    PP_U = quantile(RelPP, 0.975, na.rm = TRUE),
    .groups = "drop") 


# Summarize over the last 10 years
end_summary <- ind_q %>%
  filter(Year_actual >= max(Year_actual) - 9) %>%   # last 10 years
  group_by(scenario) %>%
  summarise(
    TB_M = mean(TB_M, na.rm = TRUE),
    TB_L = mean(TB_L, na.rm = TRUE),
    TB_U = mean(TB_U, na.rm = TRUE),
    MTL_M = mean(MTL_M, na.rm = TRUE),
    MTL_L = mean(MTL_L, na.rm = TRUE),
    MTL_U = mean(MTL_U, na.rm = TRUE),
    PP_M = mean(PP_M, na.rm = TRUE),
    PP_L = mean(PP_L, na.rm = TRUE),
    PP_U = mean(PP_U, na.rm = TRUE))

# Reshape for plotting
end_summary_long <- end_summary %>%
  pivot_longer(
    cols = -scenario,
    names_to = c("Indicator", ".value"),
    names_pattern = "(.*)_(M|L|U)") %>%
  mutate(Indicator = recode(Indicator,
                            TB  = "Relative total biomass",
                            MTL = "Δ Mean trophic level",
                            PP  = "Predator : Prey ratio"))

facet_levels <- c("Relative total biomass",
                  "Δ Mean trophic level",
                  "Predator : Prey ratio")

end_summary_long <- end_summary_long %>%
  mutate(
    Indicator = factor(Indicator, levels = facet_levels, ordered = TRUE),
    FacetID = as.integer(factor(Indicator, levels = facet_levels)))

tag_df <- data.frame(
  FacetID = 1:3,
  tag = c("a)", "b)", "c)"))

Fig4<-ggplot(end_summary_long,
             aes(x = scenario, y = M, color = scenario)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = L, ymax = U), width = 0.15, size = 0.5) +
  facet_wrap(~ FacetID, scales = "free_y", nrow = 1,
             labeller = as_labeller(setNames(facet_levels, 1:3))) +
  geom_hline(aes(yintercept = ifelse(Indicator == "Δ Mean trophic level", 0, 1)),
             linetype = "dotted", color = "grey40") +
  geom_text(
    data = tag_df,
    aes(label = tag),
    x = -Inf, y = Inf,
    hjust = -0.2, vjust = 1.5,
    size = 4.5, fontface = "bold",
    inherit.aes = FALSE) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01, trim = TRUE),
    expand = expansion(mult = c(0.05, 0.05)))+
  scale_color_manual(values = c(
    "Capelin -50%" = "#3C79F4",
    "Capelin zero" = "#E83E4E",
    "Status Quo"   = "#2CA25F" )) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "black"),
    strip.text = element_text(face = "bold")) +
  labs(y = NULL, x = NULL, color = "Scenario")
Fig4
ggsave("files_for_capelin_paper/Figures/Fig4_Indicators_combined.png", Fig4, width = 10, height = 4, dpi = 350)


# FIGURE 5 — “Competitors of capelin” (relative), IQR only
#   (Built from diet overlap)
prey_name <- "FCA"

diet_df    <- as.data.frame(REco.params$diet)
pred_names <- setdiff(names(diet_df), "Group")
pred_names <- intersect(pred_names, names(REco$QB))
diet_mat   <- as.matrix(diet_df[, pred_names, drop = FALSE])
diet_mat   <- sweep(diet_mat, 2, pmax(colSums(diet_mat, na.rm = TRUE), 1e-12), `/`)

p_cap <- as.numeric(diet_mat[, prey_name])
B  <- as.numeric(REco$Biomass[pred_names])
QB <- as.numeric(REco$QB[pred_names])
eat_tot <- B * QB

overlap <- sapply(pred_names, function(k) {
  p_k <- diet_mat[, k]; 1 - 0.5 * sum(abs(p_cap - p_k), na.rm = TRUE)
})
shared_frac <- sapply(pred_names, function(k) {
  p_k <- diet_mat[, k]; sum(pmin(p_cap, p_k), na.rm = TRUE)
})
shared_consumption <- shared_frac * eat_tot

comp_tbl <- tibble(
  Group            = pred_names,
  Biomass          = B,
  QB               = QB,
  Overlap_with_FCA = overlap,
  SharedDietFrac   = shared_frac,
  SharedCons       = shared_consumption
) %>%
  filter(Group != prey_name, QB > 0, Biomass > 0) %>%
  mutate(SharedConsShare = SharedCons / pmax(sum(SharedCons), 1e-12)) %>%
  arrange(desc(Overlap_with_FCA))

# Use ONLY top 6 
comp_groups <- c("CEP","FHE","FBP","ZG","FSD","FKR")
name_map_comp <- c("CEP"="Cephalopods","FHE"="Herring","FBP"="Small pelagic fish",
                   "ZG"="Gelatinous zooplankton","FSD"="Sandeel","FKR"="Krill")

pred_runs_comp <- runs_long_filtered%>%
  filter(Biomass_Type %in% comp_groups) %>%
  group_by(scenario, Run_ID, Biomass_Type) %>%
  mutate(base = mean(Biomass[Year %in% 1:29], na.rm = TRUE),
         Rel  = Biomass / pmax(base, 1e-12)) %>%
  ungroup()

pred_q_comp <- pred_runs_comp %>% 
  group_by(scenario, Biomass_Type, Year_actual) %>%
  summarize(
    Med = median(Rel, na.rm = TRUE),
    Q25 = quantile(Rel, 0.025, na.rm = TRUE),
    Q75 = quantile(Rel, 0.975, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(Group = factor(recode(Biomass_Type, !!!name_map_comp),
                        levels = recode(comp_groups, !!!name_map_comp)))
pred_runs_comp %>%
  filter(Year_actual >= 1996, scenario == "Status Quo") %>%
  mutate(
    Group = factor(recode(Biomass_Type, !!!name_map_comp),
                   levels = recode(comp_groups, !!!name_map_comp))) %>%
  ggplot(aes(x = Year_actual, y = Rel, group = Run_ID, color = scenario)) +
  geom_line(alpha = 0.15, linewidth = 0.4) +   # many faint lines
  facet_wrap(~ Group, ncol = 3, scales = "free_y") +
  geom_hline(yintercept = 1, linetype = "dotted", color = "grey40") +
  scale_color_manual(values = c(
    "Capelin -50%" = "#3C79F4",
    "Capelin zero" = "#E83E4E",
    "Status Quo"   = "#2CA25F")) +
  labs(
    x = "Year",
    y = "Relative biomass (B/B₀)",
    color = "Scenario") +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank())

Fig5 <- capelin_plots(pred_q_comp %>% filter(Year_actual >= 1996),
                      x="Year_actual", yM="Med", yL="Q25", yU="Q75",
                      title = "",
                      ylab = expression("Relative biomass (B / B"[mean] * ")"), facet = "Group", ncol = 3, hline = 1)
Fig5
ggsave("files_for_capelin_paper/Figures/Fig5_Competitors_relative_IQR.png", Fig5, width = 10, height = 5, dpi = 350)



## Capelin prey 
comp_groups <- c("ZL","FKR")
name_map_comp <- c("ZL" = "Large zooplankton","FKR"="Krill")

prey_runs_comp <- runs_long_filtered %>%
  filter(Biomass_Type %in% comp_groups) %>%
  group_by(scenario, Run_ID, Biomass_Type) %>%
  mutate(base = mean(Biomass[Year %in% 1:25], na.rm = TRUE),
         Rel  = Biomass / pmax(base, 1e-12)) %>%
  ungroup()

prey_q_comp <- prey_runs_comp %>%
  group_by(scenario, Biomass_Type, Year_actual) %>%
  summarize(
    Med = median(Rel, na.rm = TRUE),
    Q25 = quantile(Rel, 0.025, na.rm = TRUE),
    Q75 = quantile(Rel, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Group = factor(recode(Biomass_Type, !!!name_map_comp),
                        levels = recode(comp_groups, !!!name_map_comp)))

Fig6 <- capelin_plots(prey_q_comp %>% filter(Year_actual >= 1996),
                      x="Year_actual", yM="Med", yL="Q25", yU="Q75",
                      title = "",
                      ylab = expression("Relative biomass (B / B"[mean] * ")"), facet = "Group", ncol = 3, hline = 1)
Fig6
ggsave("files_for_capelin_paper/Figures/Fig6_capelin_prey_relative_IQR.png", Fig6, width = 10, height = 4, dpi = 350)



# TABLES

## % change vs Status Quo (Years 91–100)

# relative biomass (B/B0) for all groups

runs_rel <- runs_long_filtered %>%
  group_by(Run_ID, Biomass_Type) %>%
  mutate(
    base = mean(Biomass[Year %in% 1:29], na.rm = TRUE),
    Rel  = Biomass / pmax(base, 1e-12)) %>%
  ungroup()

# last 10 years (Years 91–100)

dat_rel_last10 <- runs_rel %>%
  filter(Year %in% 91:100) %>%
  group_by(scenario, Run_ID, Biomass_Type) %>%
  summarise(MeanLast10 = mean(Rel, na.rm = TRUE), .groups = "drop")

# Extract Status Quo baselines per run/group

sq_rel <- dat_rel_last10 %>%
  filter(scenario == "Status Quo") %>%
  select(Run_ID, Biomass_Type, SQ = MeanLast10)

# % difference vs Status Quo (relative biomass)

dat_rel_comp <- dat_rel_last10 %>%
  filter(scenario != "Status Quo") %>%
  left_join(sq_rel, by = c("Run_ID","Biomass_Type")) %>%
  mutate(
    Pct_vs_SQ = 100 * (MeanLast10 / SQ - 1))  

# summary stats per group per scenario

table_all_groups <- dat_rel_comp %>%
  group_by(Biomass_Type, scenario) %>%
  summarise(
    Median = median(Pct_vs_SQ, na.rm = TRUE),
    Q025   = quantile(Pct_vs_SQ, 0.025, na.rm = TRUE),
    Q975   = quantile(Pct_vs_SQ, 0.975, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(
    Stat = sprintf("%.1f (%.1f, %.1f)", Median, Q025, Q975)) %>%
  select(Biomass_Type, scenario, Stat) %>%
  pivot_wider(names_from = scenario, values_from = Stat) %>%
  arrange(Biomass_Type)




# table for trophic-level 
tl_last10 <- runs_tl_rel %>%
  filter(Year_actual %in% 2086:2095) %>%     # Years 91–100
  group_by(scenario, Run_ID, TL_group) %>%
  summarise(MeanLast10 = mean(Rel, na.rm = TRUE), .groups = "drop")

sq_tl_last10 <- tl_last10 %>%
  filter(scenario == "Status Quo") %>%
  select(Run_ID, TL_group, SQ = MeanLast10)

table_tl_wide <- tl_last10 %>%
  filter(scenario != "Status Quo") %>%
  left_join(sq_tl_last10, by = c("Run_ID","TL_group")) %>%
  mutate(Pct_vs_SQ = 100 * (MeanLast10 / pmax(SQ, 1e-12) - 1)) %>%
  group_by(TL_group, scenario) %>%
  summarise(
    Median = median(Pct_vs_SQ, na.rm = TRUE),
    Q25    = quantile(Pct_vs_SQ, 0.025, na.rm = TRUE),
    Q75    = quantile(Pct_vs_SQ, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Stat = sprintf("%.1f (%.1f, %.1f)", Median, Q25, Q75)) %>%
  select(TL_group, scenario, Stat) %>%
  pivot_wider(names_from = scenario, values_from = Stat) %>%
  arrange(TL_group)

## supplementary figures

# Extract Ecopath components 
diet_mat    <- REco$DC      
QB          <- REco$QB       
B           <- REco$Biomass   
group_names <- REco$Group    
codes       <- names(B)      

# Capelin as prey ---
prey <- "FCA"

# Diet share on capelin for each predator
d_cap <- diet_mat[ prey,]

# Keep only biological consumers (B > 0, QB > 0)
valid_preds <- which(B > 0 & QB > 0)

# Compute metrics
Ik <- QB[valid_preds] * d_cap[valid_preds]                     # relative dependence (per biomass rate)
Fk <- B[valid_preds]  * QB[valid_preds] * d_cap[valid_preds]   # absolute removal (biomass flux)

# dependence table 
dependence_table <- data.frame(
  Predator   = codes[valid_preds],
  GroupName  = group_names[valid_preds],
  Ik         = Ik,
  Fk         = Fk
) %>%
  filter(Ik > 0 | Fk > 0) %>%          # drop non-predators of capelin
  arrange(desc(Ik))                    # rank by relative dependence

#  top 6 
dependence_table <- dependence_table %>%
  mutate(Top6 = Predator %in% head(Predator, 6))

# Plot Relative dependence (Ik) 
p1 <- dependence_table %>%
  top_n(15, Ik) %>%   # show top 15 predators
  ggplot(aes(x = reorder(GroupName, Ik), y = Ik, fill = Top6)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "grey70")) +
  labs(x = "Predator", y = "Relative dependence (Ig)", fill = "Top 6") +
  theme_bw(base_size = 14) +
  theme(legend.position = "")

# Plot Absolute removal (Fk)
p2 <- dependence_table %>%
  top_n(15, Fk) %>%   # show top 15 predators
  ggplot(aes(x = reorder(GroupName, Fk), y = Fk, fill = Top6)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "grey70")) +
  labs(x = "Predator", y = "Absolute removal (Fg, biomass units)", fill = "Top 6") +
  theme_bw(base_size = 14) +
  theme(legend.position = "")

#  side by side
Fig_S1 <- p1 | p2
ggsave("files_for_capelin_paper/Figures/Fig_S1_CapelinPredators_Ik_Fk.png",
       Fig_S1, width = 12, height = 5.5, units = "in", dpi = 350)


# supplementary fig2

diet_df    <- as.data.frame(REco.params$diet)        
pred_names <- setdiff(names(diet_df), "Group")
diet_mat   <- as.matrix(diet_df[, pred_names, drop = FALSE])
diet_mat   <- sweep(diet_mat, 2, pmax(colSums(diet_mat, na.rm = TRUE), 1e-12), `/`)  

cap <- "FCA"  # Capelin as predator 
p_cap <- as.numeric(diet_mat[, cap])

B  <- as.numeric(REco$Biomass[pred_names])
QB <- as.numeric(REco$QB[pred_names])
names(B)  <- pred_names
names(QB) <- pred_names

# Keep only valid consumers
valid <- pred_names[B[pred_names] > 0 & QB[pred_names] > 0 & pred_names != cap]

# Schoener diet overlap and shared consumption on common resources
schoener <- sapply(valid, function(sp){
  p_k <- diet_mat[, sp]
  1 - 0.5 * sum(abs(p_cap - p_k), na.rm = TRUE)
})

shared_frac <- sapply(valid, function(sp){
  p_k <- diet_mat[, sp]
  sum(pmin(p_cap, p_k), na.rm = TRUE)
})

shared_Q    <- shared_frac * (B[valid] * QB[valid])   # overlap × consumer demand proxy

scho_tbl <- tibble::tibble(
  Group = valid,
  SchoenerD = as.numeric(schoener),
  SharedFrac = as.numeric(shared_frac),
  SharedQ = as.numeric(shared_Q),
  Biomass = B[valid],
  QB = QB[valid],
  Label = REco$Group[valid]
) %>%
  dplyr::arrange(dplyr::desc(SchoenerD))

top6_ids <- c("FHE","FSD","FBP","CEP","FKR","ZG")  
plot_dat  <- scho_tbl %>% mutate(SharedQ_Mt = SharedQ/1e6, is_top6 = Group %in% top6_ids)


top6_labels <- scho_tbl %>%
  filter(Group %in% top6_ids) %>%
  distinct(Group, Label) %>%
  arrange(Label) %>%                    
  pull(Label)

cols_top6 <- setNames(scales::hue_pal()(length(top6_labels)), top6_labels)

# Top-N by Schoener
topN <- 12
pA_dat <- scho_tbl %>%
  arrange(desc(SchoenerD)) %>%
  slice_head(n = topN) %>%
  mutate(fill_label = ifelse(Label %in% names(cols_top6), Label, "Other"))

pA <- ggplot(pA_dat, aes(x = reorder(Label, SchoenerD), y = SchoenerD, fill = fill_label)) +
  geom_col( width = 0.8) +
  geom_hline(yintercept = c(0.6, 0.8), linetype = 3, color = "grey40") +
  coord_flip() +
  scale_fill_manual(
    values = c(Other = "grey80", cols_top6),
    drop   = FALSE
  ) +
  labs(x = "Potential competitor", y = "Schoener diet overlap with capelin (D)") +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none"  
  )
pB <- ggplot(plot_dat, aes(SchoenerD, SharedQ_Mt)) +
  geom_point(data = subset(plot_dat, !is_top6), color="grey75", alpha=.6, size=2) +
  geom_point(data = subset(plot_dat,  is_top6), aes(color = Label), size=3) +
  ggrepel::geom_text_repel(
    data = subset(plot_dat, is_top6),
    aes(label = Label, color = Label),
    size = 3.2, box.padding = .5, max.overlaps = 100, seed = 42
  ) +
  geom_vline(xintercept = c(0.6, 0.8), linetype = 3) +
  scale_color_manual(values = cols_top6) +
  labs(x = "Schoener overlap with capelin (D)",
       y = "Shared consumption on common prey (million tonnes · yr⁻¹)") +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none"   
  )

Fig_S22 <- pA | pB

ggsave("files_for_capelin_paper/Figures/Fig_S22_competition_schoener.png", Fig_S22, width = 14, height = 10, dpi = 350)

# supplementary figures 1 and 2 (filtered and non-filtered runs)
name_map <- c(
  "FCD.juv" = "Cod (0-3)",
  "FCD.adult" = "Cod (4+)",
  "FHA.juv" = "Haddock (0-2)",
  "FHA.adult" = "Haddock (3+)",
  "FSA.juv" = "Saithe (0-3)",
  "FSA.adult" = "Saithe (4+)",
  "FGH" = "Greenland halibut",
  "FRF" = "Redfish",
  "FHE" = "Herring",
  "FCA" = "Capelin",
  "FFF" = "Flatfish",
  "SSR" = "Skates & rays",
  "SSD" = "Small sharks",
  "SSH" = "Large sharks",
  "PIN" = "Seals",
  "PWN" = "Shrimp",
  "FOC" = "Other Codfish",
  "FDC" = "Dem. comm. fish",
  "FDF" = "Demersal fish",
  "FSD" = "Sandeel",
  "FBP" = "Small pelagic fish",
  "CEP" = "Cephalopods",
  "WMW" = "Minke whale",
  "FMI" = "Migratory fish",
  "FEP" = "Epifauna",
  "FIN" = "Infauna",
  "FKR" = "Krill",
  "ZL" = "Zooplankton large",
  "ZS" = "Zooplankton small",
  "ZG" = "Gelatinous zooplankton",
  "FLC" = "Lobsters & crabs",
  "LOB" = "Norway lobster",
  "WHB" = "Baleen whale",
  "WHT" = "Tooth whale",
  "WTO" = "Delphinidae",
  "SB" = "Seabirds")
comp_groups <- c(
  "FCD.juv","FCD.adult","FHA.juv","FHA.adult", 
  "FSA.juv","FSA.adult","FGH","FRF","FHE","FCA",
  "FFF","SSR","SSD","SSH","PIN","PWN","FOC","FDC",
  "FDF","FSD","FBP","CEP","WMW","FMI","FEP","FIN",
  "FKR","ZL","ZS","ZG","FLC","LOB","WHB","WHT","WTO","SB")

# for plot without filtering, switch 
# runs_long_filtered with runs_long

pred_runs_comp <- runs_long %>%
  filter(Biomass_Type %in% comp_groups) %>%
  group_by(scenario, Run_ID, Biomass_Type) %>%
  mutate(base = mean(Biomass[Year %in% 1:29], na.rm = TRUE),
         Rel  = Biomass / pmax(base, 1e-12)) %>%
  ungroup()

pred_q_comp <- pred_runs_comp %>% 
  group_by(scenario, Biomass_Type, Year_actual) %>%
  summarize(
    Med = median(Rel, na.rm = TRUE),
    Q25 = quantile(Rel, 0.025, na.rm = TRUE),
    Q75 = quantile(Rel, 0.975, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(Group = factor(recode(Biomass_Type, !!!name_map),
                        levels = recode(comp_groups, !!!name_map)))
supp_wo_filter<- pred_runs_comp %>%
  filter(Year_actual >= 1996) %>%
  mutate(
    Group = factor(recode(Biomass_Type, !!!name_map),
                   levels = recode(comp_groups, !!!name_map))) %>%
  ggplot(aes(x = Year_actual, y = Rel, group = Run_ID, color = scenario)) +
  geom_line(alpha = 0.15, linewidth = 0.4) +   # many faint lines
  facet_wrap(~ Group, ncol = 5, scales = "free_y") +
  geom_hline(yintercept = 1, linetype = "dotted", color = "grey40") +
  scale_color_manual(values = c(
    "Capelin -50%" = "#3C79F4",
    "Capelin zero" = "#E83E4E",
    "Status Quo"   = "#2CA25F")) +
  labs(
    x = "Year",
    y = expression("Relative biomass (B / B"[mean]*")"),
    color = "Scenario") +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank())
ggsave("files_for_capelin_paper/Figures/no_filtering.png", supp_wo_filter, width = 10, height = 10, dpi = 350)
ggsave("files_for_capelin_paper/Figures/with_filtering.png", supp_w_filter, width = 10, height = 10, dpi = 350)



