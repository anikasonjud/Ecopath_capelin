
library(tidyverse)
library(data.table)
library(Rpath)
library(mfdb)
library(mar)

mdb <- mfdb('Iceland', db_params = list(host='mfdb.hafro.is'))
mar <- connect_mar()

fit.years <- 1:100

REco.params <- get(load("REco.params_Iceland.RData"))
source("pedigree.R")
REco <- rpath(REco.params, eco.name = 'R Ecosystem')

ISL_unbal <- REco.params
ISL_bal   <- rpath(ISL_unbal, eco.name = 'R Ecosystem')

scene_new_compare <- readRDS("scene_new_compare2.rds")  # fitted Ecosim scenario
scene_base        <- scene_new_compare                  



# Remove/neutralize any forcings after historical fit (year 28)
neutralize_posthistory_forcings <- function(scene, end_year = 28, n_years = 100) {
  # Recruitment forcing -> 1 after history
  if (!is.null(scene$forcings$ForcedRecs)) {
    scene$forcings$ForcedRecs[pmax(end_year+1,1):n_years, ] <- 1
  }
  # Effort forcing -> 0 after history
  if (!is.null(scene$fishing$ForcedEffort)) {
    scene$fishing$ForcedEffort[pmax(end_year+1,1):n_years, ] <- 0
  }
  # Biomass forcing -> NA after history
  if (!is.null(scene$forcings$ForcedBio)) {
    scene$forcings$ForcedBio[pmax(end_year+1,1):n_years, ] <- NA
  }
  # Migration forcing -> 0 after history
  if (!is.null(scene$forcings$ForcedMigrate)) {
    scene$forcings$ForcedMigrate[pmax(end_year+1,1):n_years, ] <- 0
  }
  scene
}

# Moderate post‑history F (robust baseline + optional damp)
update_forced_f_rate_moderate <- function(scene,
                                          catch_data = NULL,   # kept for API compatibility
                                          end_year = 28,
                                          post_method = c("median", "mean", "trimmed10"),
                                          damp = 1.00,
                                          cap_quantile = 0.80,
                                          floor_quantile = 0.20,
                                          special = list(FCA = list(method = "mean_window",
                                                                    window = 2, damp = 1.00))) {
  stopifnot(!is.null(scene$fishing$ForcedFRate))
  FF <- scene$fishing$ForcedFRate
  if (!is.matrix(FF)) FF <- as.matrix(FF)
  
  nT <- nrow(FF); nG <- ncol(FF)
  if (nT < end_year + 1) {
    stop("ForcedFRate has only ", nT, " rows; end_year=", end_year,
         " requires at least ", end_year + 1, " rows.")
  }
  
  post_method <- match.arg(post_method)
  
  # --- helper to compute post-F from historical series (rows 1:end_year)
  get_postF <- function(x) {
    hist <- x[seq_len(end_year)]
    hist <- hist[is.finite(hist)]
    if (!length(hist)) return(0)
    
    v <- switch(post_method,
                median    = median(hist, na.rm = TRUE),
                mean      = mean(hist, na.rm = TRUE),
                trimmed10 = mean(hist, trim = 0.10, na.rm = TRUE))
    
    # cap extremes relative to history; fallback if too short
    if (length(hist) >= 3) {
      hi <- as.numeric(stats::quantile(hist, probs = cap_quantile,   na.rm = TRUE, type = 7))
      lo <- as.numeric(stats::quantile(hist, probs = floor_quantile, na.rm = TRUE, type = 7))
      v <- max(min(v, hi), lo)
    }
    v * damp
  }
  
  # --- default for every group
  default_post <- apply(FF, 2, get_postF)
  
  # --- per-group specials (e.g., FCA)
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
          # fall back to global method with capping
          v <- get_postF(FF[, nm, drop = TRUE])
        }
        default_post[nm] <- v
      }
    }
  }
  
  # --- write post‑history values to years (end_year+1):nT for all groups
  if (end_year < nT) {
    FF[(end_year + 1):nT, ] <- matrix(rep(default_post, each = nT - end_year),
                                      nrow = nT - end_year, ncol = nG,
                                      byrow = FALSE,
                                      dimnames = list(NULL, colnames(FF)))
  }
  
  scene$fishing$ForcedFRate <- FF
  return(scene)
}

`%||%` <- function(a,b) if (is.null(a)) b else a

# Warm‑start to kill the early surge
warm_start_scene <- function(scene, years = 1:30) {
  pre <- rsim.run(scene, method = "AB", years = years)
  scene$start_state <- pre$end_state
  scene$params$BURN_YEARS <- 0
  scene
}


# Clear any hidden post‑history forcings
scene_new_compare <- neutralize_posthistory_forcings(scene_new_compare, end_year = 28, n_years = 100)

# Set moderate post‑history F (median + mild damp)
scene_new_compare <- update_forced_f_rate_moderate(
  scene_new_compare, scene_new_compare$fitting$Catch,
  end_year = 28, post_method = "median", damp = 1,
  cap_quantile = 0.80, floor_quantile = 0.20,
  special = list(FCA = list(method = "mean_window", window = 2, damp = 1.00))
)



NUM_RUNS <- 13500
parlist   <- vector("list", NUM_RUNS)
kept      <- logical(NUM_RUNS)
set.seed(666)

# group‑specific acceptance thresholds (relative to year 51 biomass)
initial_bio <- REco$Biomass
max_allowed <- rep(NA_real_, length(initial_bio))
max_allowed[initial_bio < 1e3]                       <- 100
max_allowed[initial_bio >= 1e3  & initial_bio < 1e4] <- 50
max_allowed[initial_bio >= 1e4  & initial_bio < 1e5] <- 25
max_allowed[initial_bio >= 1e5  & initial_bio < 1e6] <- 15
max_allowed[initial_bio >= 1e6  & initial_bio < 1e7] <- 10
max_allowed[initial_bio >= 1e7]                      <- 6
names(max_allowed) <- names(initial_bio)

for (i in 1:NUM_RUNS) {
  ISLsense <- scene_new_compare
  
  # narrower V/D than before to avoid systemic SQ drift
  parlist[[i]] <- rsim.sense(
    ISLsense, ISL_unbal,
    Vvary = c(-0.5, 0.5),           # ~√10 range
    Dvary = c(log(0.8), log(1.25))  # tighter handling‑time range
  )
  
  # Use a *screening* burn‑in to reject unstable parameter sets
  ISLsense$start_state$Biomass <- parlist[[i]]$B_BaseRef
  parlist[[i]]$BURN_YEARS <- 50
  ISLsense$params <- parlist[[i]]
  
  test <- rsim.run(ISLsense, method = "AB", years = 1:100)
  
  # reject NAs
  if (any(is.na(test$end_state$Biomass))) {
    kept[i] <- FALSE
    next
  }
  
  bio_traj <- test$annual_Biomass
  keep_groups <- setdiff(colnames(bio_traj), c("Outside", "Detritus", "Phytoplankton"))
  base <- pmax(bio_traj[51, keep_groups, drop = TRUE], 1e-12)
  ratio <- sweep(bio_traj[51:100, keep_groups, drop = FALSE], 2, base, "/")
  
  bio_min <- apply(ratio, 2, min, na.rm = TRUE)
  bio_max <- apply(ratio, 2, max, na.rm = TRUE)
  
  max_thresh <- max_allowed[keep_groups]
  min_thresh <- 1 / max_thresh
  
  kept[i] <- !(any(bio_min < min_thresh, na.rm = TRUE) || any(bio_max > max_thresh, na.rm = TRUE))
  if (!kept[i]) cat(i, ": rejected\n") else cat(i, ": kept\n")
}

KEPT  <- which(kept)
nkept <- length(KEPT)
cat("Kept runs:", nkept, "of", NUM_RUNS, " (", round(100*nkept/NUM_RUNS,1), "%)\n", sep = "")
isl.sense_final <- parlist[KEPT]



## Final simulations (Status Quo + Capelin scenarios)

results_capelin <- list()
set.seed(123)

BURN_IN <- 0          # we warm‑start, so no formal burn‑in here
perturb_start <- 53   # perturbations start here (post‑history)

for (irun in 1:nkept) {
  message("Running simulation for ", irun)
  run.scene <- scene_new_compare
  run.scene$params <- isl.sense_final[[irun]]
  
  # start biomass = this draw’s Ecopath equilibrium
  run.scene$start_state$Biomass <- run.scene$params$B_BaseRef
  
  # ensure key groups are integrated (your original set)
  run.scene$params$NoIntegrate[c("FCD.adult","FCD.juv",
                                 "FHA.adult","FHA.juv",
                                 "FSA.adult","FSA.juv",
                                 "FCA")] <- 0
  
  # warm‑start (pre‑run to align state with params/forcings)
  run.scene <- warm_start_scene(run.scene, years = 1:30)
  
  ## --- Status Quo
  sq <- rsim.run(run.scene, method = "AB", years = fit.years)
  sq_dt <- as.data.table(sq$annual_Biomass)[, Year := 1:100][Year > BURN_IN]
  results_capelin[[paste0("Status_Quo_", irun)]] <- melt(
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
  results_capelin[[paste0("Down_50_", irun)]] <- melt(
    r50_dt, id.vars = "Year", variable.name = "Group", value.name = "Biomass"
  )
  
  ## --- Capelin ~0
  s0 <- copy(run.scene)
  s0 <- adjust.forcing(s0, parameter = "ForcedBio", group = "FCA",
                       sim.year = perturb_start:100, value = bio.ref * 1e-5)
  r0 <- rsim.run(s0, method = "AB", years = fit.years)
  r0_dt <- as.data.table(r0$annual_Biomass)[, Year := 1:100][Year > BURN_IN]
  results_capelin[[paste0("Down_Zero_", irun)]] <- melt(
    r0_dt, id.vars = "Year", variable.name = "Group", value.name = "Biomass"
  )
}


# --- Save raw lists ---
results_down50 <- results_capelin[grep("Down_50_",  names(results_capelin))]
results_down0  <- results_capelin[grep("Down_Zero_", names(results_capelin))]
results_sq     <- results_capelin[grep("Status_Quo_", names(results_capelin))]

save(results_down50, file = "files_for_capelin_paper/results_down50.RData")
save(results_down0,  file = "files_for_capelin_paper/results_down0.RData")
save(results_sq,     file = "files_for_capelin_paper/results_sq.RData")

# --- Build wide data frames from raw lists ---
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

# --- Stability filter (Status Quo only) to define shared runs ---
filter_stable_runs <- function(data, deviation_multiplier = 2,
                               year_threshold = 0.25, group_threshold = 8) {
  data_long <- data |>
    pivot_longer(starts_with("Biomass."), names_to = "Biomass_Type", values_to = "Biomass_Value") |>
    mutate(Group = sub("^Biomass\\.", "", Biomass_Type))
  
  deviation_flagged <- data_long |>
    group_by(Run_ID, Group) |>
    mutate(med = median(Biomass_Value, na.rm = TRUE),
           iqr = IQR(Biomass_Value, na.rm = TRUE),
           abs_dev = abs(Biomass_Value - med),
           is_outlier = abs_dev > (deviation_multiplier * iqr))
  
  run_group_summary <- deviation_flagged |>
    group_by(Run_ID, Group) |>
    summarise(prop_outlier_years = mean(is_outlier, na.rm = TRUE), .groups = "drop")
  
  run_rejections <- run_group_summary |>
    group_by(Run_ID) |>
    summarise(num_groups_exceeding = sum(prop_outlier_years > year_threshold), .groups = "drop") |>
    mutate(reject = num_groups_exceeding > group_threshold)
  
  run_rejections |>
    filter(!reject) |>
    pull(Run_ID)
}

shared_runs <- filter_stable_runs(res_m1_sq, deviation_multiplier = 2,
                                  year_threshold = 0.25, group_threshold = 8)

# --- Filter all scenarios to shared runs ---
res_m1_sq_f     <- res_m1_sq     |> filter(Run_ID %in% shared_runs)
res_m1_down50_f <- res_m1_down50 |> filter(Run_ID %in% shared_runs)
res_m1_down0_f  <- res_m1_down0  |> filter(Run_ID %in% shared_runs)

cat("✅ Systems kept after filtering Status_Quo:", length(shared_runs),
    "of", length(unique(res_m1_sq$Run_ID)), "\n")

# --- Helpers (define once) ---
mk_IQR_plot <- function(df, x, yM, yL, yU, color = "scenario", fill = "scenario",
                        title = "", ylab = "", facet = NULL, ncol = 2,
                        hline = NA, limits_year = c(2000, 2100)) {
  g <- ggplot(df, aes(x = .data[[x]], y = .data[[yM]],
                      color = .data[[color]], fill = .data[[fill]])) +
    geom_ribbon(aes(ymin = .data[[yL]], ymax = .data[[yU]]), alpha = 0.18, color = NA) +
    geom_line(size = 1.1) +
    scale_color_manual(values = pal) + scale_fill_manual(values = pal) +
    labs(title = title, x = "Year", y = ylab, color = "Scenario", fill = "Scenario") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top",
          strip.text = element_text(face = "bold"),
          panel.grid.minor = element_blank())
  if (!is.na(hline)) g <- g + geom_hline(yintercept = hline, linetype = "dotted")
  if (!is.null(facet)) g <- g + facet_wrap(as.formula(paste("~", facet)), ncol = ncol, scales = "free_y")
  if (!is.null(limits_year)) g <- g + scale_x_continuous(limits = limits_year)
  g
}

as_long_runs <- function(df, label) {
  df |>
    pivot_longer(starts_with("Biomass."), names_to="Biomass_Type", values_to="Biomass") |>
    mutate(Biomass_Type = sub("^Biomass\\.", "", Biomass_Type),
           scenario = label)
}

# --- TL metadata (once) ---
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

# --- Long form across scenarios (filtered sets only) ---
sq_long    <- as_long_runs(res_m1_sq_f,     "Status Quo")
d50_long   <- as_long_runs(res_m1_down50_f, "Capelin -50%")
dzero_long <- as_long_runs(res_m1_down0_f,  "Capelin zero")

runs_long  <- bind_rows(sq_long, d50_long, dzero_long) |>
  mutate(Biomass_Type_key = paste0("Biomass.", Biomass_Type),
         Year_actual = Year + 1995)

# =========================================================
# FIGURE 1 — Capelin (relative, IQR only)
# =========================================================
cap_runs <- runs_long %>%
  filter(Biomass_Type == "FCA") %>%
  group_by(scenario, Run_ID) %>%
  mutate(base = mean(Biomass[Year %in% 1:25], na.rm = TRUE),
         Rel  = Biomass / pmax(base, 1e-12)) %>%
  ungroup()

cap_q <- cap_runs %>%
  group_by(scenario, Year_actual) %>%
  summarize(
    Med = median(Rel, na.rm = TRUE),
    Q25 = quantile(Rel, 0.25, na.rm = TRUE),
    Q75 = quantile(Rel, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

Fig1 <- mk_IQR_plot(cap_q, x="Year_actual", yM="Med", yL="Q25", yU="Q75",
                    title = "",
                    ylab = "Relative biomass", hline = 1, limits_year = c(2000, 2100))
ggsave("files_for_capelin_paper/Figures/Fig1_Capelin_relative_IQR.png", Fig1, width = 7.2, height = 4.6, dpi = 350)

# =========================================================
# FIGURE 2 — TL bins, per-run normalized (relative), IQR only
# =========================================================
runs_tl <- runs_long %>%
  left_join(TL_table, by = c("Biomass_Type_key" = "Biomass_Type")) %>%
  filter(!is.na(TL_group)) %>%
  group_by(scenario, Run_ID, TL_group, Year_actual) %>%
  summarize(TLsum = sum(Biomass, na.rm = TRUE), .groups = "drop")

runs_tl_rel <- runs_tl %>%
  group_by(scenario, Run_ID, TL_group) %>%
  mutate(base = mean(TLsum[(Year_actual - 1995) %in% 1:25], na.rm = TRUE),
         Rel  = TLsum / pmax(base, 1e-12)) %>%
  ungroup()

tl_rel_q <- runs_tl_rel %>%
  group_by(scenario, TL_group, Year_actual) %>%
  summarize(
    Med = median(Rel, na.rm = TRUE),
    Q25 = quantile(Rel, 0.25, na.rm = TRUE),
    Q75 = quantile(Rel, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

Fig2 <- mk_IQR_plot(tl_rel_q %>% filter(Year_actual >= 2000),
                    x="Year_actual", yM="Med", yL="Q25", yU="Q75",
                    title = "",
                    ylab = "Relative biomass", facet = "TL_group", ncol = 2, hline = 1)
ggsave("files_for_capelin_paper/Figures/Fig2_TLbins_relative_IQR.png", Fig2, width = 10, height = 5.2, dpi = 350)

# =========================================================
# FIGURE 3 — Key predators (relative), IQR only
#   (Choose only the ones you actually show in the paper.)
# =========================================================
key_pred_groups <- c("FCD.adult","FCD.juv","FHA.adult","FSA.adult","WMW","SB")  # <- final set

pred_runs <- runs_long %>%
  filter(Biomass_Type %in% key_pred_groups) %>%
  group_by(scenario, Run_ID, Biomass_Type) %>%
  mutate(base = mean(Biomass[Year %in% 1:25], na.rm = TRUE),
         Rel  = Biomass / pmax(base, 1e-12)) %>%
  ungroup()

pred_q <- pred_runs %>%
  group_by(scenario, Biomass_Type, Year_actual) %>%
  summarize(
    Med = median(Rel, na.rm = TRUE),
    Q25 = quantile(Rel, 0.25, na.rm = TRUE),
    Q75 = quantile(Rel, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Group = recode(Biomass_Type,
                        "FCD.juv"="Cod (juvenile)", "FCD.adult"="Cod (adult)",
                        "FHA.adult"="Haddock (adult)", "FSA.adult"="Saithe (adult)",
                        "WMW"="Minke whales", "SB"="Seabirds"))

Fig3 <- mk_IQR_plot(pred_q %>% filter(Year_actual >= 2000),
                    x="Year_actual", yM="Med", yL="Q25", yU="Q75",
                    title = "",
                    ylab = "Relative biomass", facet = "Group", ncol = 3, hline = 1)
ggsave("files_for_capelin_paper/Figures/Fig3_KeyPredators_relative_IQR.png", Fig3, width = 10, height = 5, dpi = 350)

# =========================================================
# FIGURE 4 — Ecosystem indicators (relative/delta), combined
# =========================================================
runs_with_TL <- runs_long %>%
  left_join(select(TL_table, Biomass_Type, TL), by = c("Biomass_Type_key" = "Biomass_Type")) %>%
  filter(!is.na(TL))

ind_per_run <- runs_with_TL %>%
  group_by(scenario, Run_ID, Year_actual) %>%
  summarize(
    TotalB = sum(Biomass, na.rm = TRUE),
    MTLbio = sum(Biomass * TL, na.rm = TRUE) / pmax(sum(Biomass, na.rm = TRUE), 1e-12),
    PredB  = sum(Biomass[TL >= 3], na.rm = TRUE),
    PreyB  = sum(Biomass[TL <  3], na.rm = TRUE),
    PP     = PredB / pmax(PreyB, 1e-12),
    .groups = "drop"
  )

ind_base <- ind_per_run %>%
  filter((Year_actual - 1995) %in% 1:25) %>%
  group_by(scenario, Run_ID) %>%
  summarize(
    base_TotalB = mean(TotalB, na.rm = TRUE),
    base_MTL    = mean(MTLbio, na.rm = TRUE),
    base_PP     = mean(PP, na.rm = TRUE),
    .groups = "drop"
  )

ind_rel <- ind_per_run %>%
  left_join(ind_base, by = c("scenario","Run_ID")) %>%
  mutate(
    RelTotalB = TotalB / pmax(base_TotalB, 1e-12),
    DeltaMTL  = MTLbio - base_MTL,
    RelPP     = PP / pmax(base_PP, 1e-12)
  )

ind_q <- ind_rel %>%
  group_by(scenario, Year_actual) %>%
  summarize(
    TB_M = median(RelTotalB, na.rm = TRUE),
    TB_L = quantile(RelTotalB, 0.25, na.rm = TRUE),
    TB_U = quantile(RelTotalB, 0.75, na.rm = TRUE),
    MTL_M = median(DeltaMTL, na.rm = TRUE),
    MTL_L = quantile(DeltaMTL, 0.25, na.rm = TRUE),
    MTL_U = quantile(DeltaMTL, 0.75, na.rm = TRUE),
    PP_M = median(RelPP, na.rm = TRUE),
    PP_L = quantile(RelPP, 0.25, na.rm = TRUE),
    PP_U = quantile(RelPP, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>% filter(Year_actual >= 2000)

pA <- mk_IQR_plot(ind_q, "Year_actual", "TB_M","TB_L","TB_U",
                  title="", ylab="Relative total biomass", hline = 1) +
  theme(legend.position = "none")
pB <- mk_IQR_plot(ind_q, "Year_actual", "MTL_M","MTL_L","MTL_U",
                  title="", ylab="Δ Mean trophic level", hline = 0) +
  theme(legend.position = "none")
pC <- mk_IQR_plot(ind_q, "Year_actual", "PP_M","PP_L","PP_U",
                  title="", ylab="Predator : Prey ratio", hline = 1) +
  theme(legend.position = "none")

Fig4 <- (pA | pB | pC) + plot_layout(guides = "collect") +
  plot_annotation(title = "",
                  theme = theme(plot.title = element_text(face = "bold", hjust = 0.5))) & 
  theme(legend.position = "top")
ggsave("files_for_capelin_paper/Figures/Fig4_Indicators_combined.png", Fig4, width = 10, height = 4, dpi = 350)

# =========================================================
# FIGURE 5 — “Competitors of capelin” (relative), IQR only
#   (Built from diet overlap; keep ONLY the N you show.)
# =========================================================
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

# Use ONLY the 6 you decided to show (order preserved)
comp_groups <- c("CEP","FHE","FBP","ZG","FSD","FKR")
name_map_comp <- c("CEP"="Cephalopods","FHE"="Herring","FBP"="Small pelagic fish",
                   "ZG"="Gelatinous zooplankton","FSD"="Sandeel","FKR"="Krill")

pred_runs_comp <- runs_long %>%
  filter(Biomass_Type %in% comp_groups) %>%
  group_by(scenario, Run_ID, Biomass_Type) %>%
  mutate(base = mean(Biomass[Year %in% 1:25], na.rm = TRUE),
         Rel  = Biomass / pmax(base, 1e-12)) %>%
  ungroup()

pred_q_comp <- pred_runs_comp %>%
  group_by(scenario, Biomass_Type, Year_actual) %>%
  summarize(
    Med = median(Rel, na.rm = TRUE),
    Q25 = quantile(Rel, 0.25, na.rm = TRUE),
    Q75 = quantile(Rel, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Group = factor(recode(Biomass_Type, !!!name_map_comp),
                        levels = recode(comp_groups, !!!name_map_comp)))

Fig5 <- mk_IQR_plot(pred_q_comp %>% filter(Year_actual >= 2000),
                    x="Year_actual", yM="Med", yL="Q25", yU="Q75",
                    title = "",
                    ylab = "Relative biomass", facet = "Group", ncol = 3, hline = 1)
ggsave("files_for_capelin_paper/Figures/Fig5_Competitors_relative_IQR.png", Fig5, width = 10, height = 5, dpi = 350)



## Capelin prey ##
comp_groups <- c("ZL","FKR")
name_map_comp <- c("ZL" = "Large zooplankton","FKR"="Krill")

prey_runs_comp <- runs_long %>%
  filter(Biomass_Type %in% comp_groups) %>%
  group_by(scenario, Run_ID, Biomass_Type) %>%
  mutate(base = mean(Biomass[Year %in% 1:25], na.rm = TRUE),
         Rel  = Biomass / pmax(base, 1e-12)) %>%
  ungroup()

prey_q_comp <- prey_runs_comp %>%
  group_by(scenario, Biomass_Type, Year_actual) %>%
  summarize(
    Med = median(Rel, na.rm = TRUE),
    Q25 = quantile(Rel, 0.25, na.rm = TRUE),
    Q75 = quantile(Rel, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Group = factor(recode(Biomass_Type, !!!name_map_comp),
                        levels = recode(comp_groups, !!!name_map_comp)))

Fig6 <- mk_IQR_plot(prey_q_comp %>% filter(Year_actual >= 2000),
                    x="Year_actual", yM="Med", yL="Q25", yU="Q75",
                    title = "",
                    ylab = "Relative biomass", facet = "Group", ncol = 3, hline = 1)
ggsave("files_for_capelin_paper/Figures/Fig6_capelin_prey_relative_IQR.png", Fig6, width = 10, height = 4, dpi = 350)


# TABLES

#Key predators: % change last decade vs. SQ ----
report_groups <- c("FCA","FCD.adult","FCD.juv","FHA.adult","FSA.adult","SB","WMW")
last10 <- runs_long %>%
  filter(Biomass_Type %in% report_groups, Year %in% 91:100) %>%
  group_by(scenario, Run_ID, Biomass_Type) %>%
  summarize(MeanLast10 = mean(Biomass, na.rm = TRUE), .groups = "drop")

sq_last10 <- last10 %>% filter(scenario == "Status Quo") %>%
  select(Run_ID, Biomass_Type, SQ = MeanLast10)

pct_per_run <- last10 %>%
  filter(scenario != "Status Quo") %>%
  left_join(sq_last10, by = c("Run_ID","Biomass_Type")) %>%
  mutate(PercentChange = 100 * (MeanLast10 - SQ) / pmax(SQ, 1e-12))

table_pred_wide <- pct_per_run %>%
  group_by(Biomass_Type, scenario) %>%
  summarize(
    Median = median(PercentChange, na.rm = TRUE),
    Q25    = quantile(PercentChange, 0.25, na.rm = TRUE),
    Q75    = quantile(PercentChange, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Stat = sprintf("%.1f (%.1f, %.1f)", Median, Q25, Q75)) %>%
  select(Biomass_Type, scenario, Stat) %>%
  pivot_wider(names_from = scenario, values_from = Stat) %>%
  arrange(match(Biomass_Type, report_groups))

# Competitors table 
common_runs_comp <- runs_long %>%
  filter(Biomass_Type %in% comp_groups) %>%
  group_by(scenario) %>% summarise(runs = list(unique(Run_ID)), .groups = "drop") %>%
  pull(runs) %>% Reduce(intersect, .)

dat_comp <- runs_long %>%
  filter(Run_ID %in% common_runs_comp,
         Biomass_Type %in% comp_groups, Year %in% 91:100) %>%
  group_by(scenario, Run_ID, Biomass_Type) %>%
  summarise(MeanLast10 = mean(Biomass, na.rm = TRUE), .groups = "drop")

sq_comp <- dat_comp %>% filter(scenario == "Status Quo") %>%
  select(Run_ID, Biomass_Type, SQ = MeanLast10)

table_comp_wide <- dat_comp %>% filter(scenario != "Status Quo") %>%
  left_join(sq_comp, by = c("Run_ID","Biomass_Type")) %>%
  mutate(Pct_vs_SQ = 100 * (MeanLast10 / pmax(SQ, 1e-12) - 1)) %>%
  group_by(Biomass_Type, scenario) %>%
  summarise(
    Median = median(Pct_vs_SQ, na.rm = TRUE),
    Q25    = quantile(Pct_vs_SQ, 0.25, na.rm = TRUE),
    Q75    = quantile(Pct_vs_SQ, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Stat = sprintf("%.1f (%.1f, %.1f)", Median, Q25, Q75),
         Group = recode(Biomass_Type, !!!name_map_comp)) %>%
  select(Group, scenario, Stat) %>%
  pivot_wider(names_from = scenario, values_from = Stat) %>%
  arrange(match(Group, recode(comp_groups, !!!name_map_comp)))

# Krill & Large zooplankton 
target_groups <- c("FKR", "ZL")
name_map_kz   <- c("FKR"="Krill","ZL"="Large zooplankton")

common_runs_kz <- runs_long %>%
  filter(Biomass_Type %in% target_groups) %>%
  group_by(scenario) %>% summarise(runs = list(unique(Run_ID)), .groups = "drop") %>%
  pull(runs) %>% Reduce(intersect, .)

dat_kz <- runs_long %>%
  filter(Run_ID %in% common_runs_kz,
         Biomass_Type %in% target_groups, Year %in% 91:100) %>%
  group_by(scenario, Run_ID, Biomass_Type) %>%
  summarise(MeanLast10 = mean(Biomass, na.rm = TRUE), .groups = "drop")

sq_kz <- dat_kz %>% filter(scenario == "Status Quo") %>%
  select(Run_ID, Biomass_Type, SQ = MeanLast10)

table_kz_wide <- dat_kz %>% filter(scenario != "Status Quo") %>%
  left_join(sq_kz, by = c("Run_ID","Biomass_Type")) %>%
  mutate(Pct_vs_SQ = 100 * (MeanLast10 / pmax(SQ, 1e-12) - 1)) %>%
  group_by(Biomass_Type, scenario) %>%
  summarise(
    Median = median(Pct_vs_SQ, na.rm = TRUE),
    Q25    = quantile(Pct_vs_SQ, 0.25, na.rm = TRUE),
    Q75    = quantile(Pct_vs_SQ, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Stat = sprintf("%.1f (%.1f, %.1f)", Median, Q25, Q75),
         Group = recode(Biomass_Type, !!!name_map_kz)) %>%
  select(Group, scenario, Stat) %>%
  pivot_wider(names_from = scenario, values_from = Stat) %>%
  arrange(match(Group, unname(recode(target_groups, !!!name_map_kz))))


# Also save CSVs if you need them
write_csv(table_pred_wide, "Table_KeyPredators_LastDecade_PercentChange_vsSQ_medianIQR.csv")
write_csv(table_comp_wide, "Table_Competitors_LastDecade_PercentChange_vsSQ_medianIQR.csv")
write_csv(table_kz_wide,   "Table_Krill_LZ_LastDecade_PercentChange_vsSQ_medianIQR.csv")








