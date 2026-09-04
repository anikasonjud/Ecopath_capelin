################################################################################
##
##  Capelin decline in Icelandic waters - marine food web analysis
##  Sonjudottir et al.  |  Ecological Modelling  |  ECOMOD-26-757
##
##  CONTENTS
##    01  Libraries
##    02  Settings, paths and constants
##    03  Lookups (group names, palettes, scenario labels)
##    04  Data connections and base model objects
##    05  Helper functions
##    06  Ensemble generation (Ecosense)
##    07  Scenario simulations
##    08  Post-processing and plausibility screening
##    09  Main figures
##    10  Main tables
##    11  Additional analyses (revision)
##    12  Supplementary figures
##    13  Filter sensitivity (reviewer Q3)
##    14  Trophic-level bin sensitivity (S5)
##
##
################################################################################


################################################################################
## 01  LIBRARIES
################################################################################

library(data.table)
library(Rpath)
library(mfdb)
library(mar)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(ggplot2)
library(patchwork)      # p1 | p2 panel composition
library(ggrepel)        # geom_text_repel
library(scales)         # hue_pal, label_number
library(zoo)            # rollapply (was loaded inside check_stable)
library(parallel)       # mclapply


################################################################################
## 02  SETTINGS, PATHS AND CONSTANTS
################################################################################

## Output locations -------------------------------------------------------------
DIR  <- "files_for_capelin_paper"
FIGS <- file.path(DIR, "Figures")

## Simulation settings ----------------------------------------------------------
fit.years     <- 1:100
mccores       <- 40
NUM_RUNS      <- 100000

BURN_IN       <- 0
perturb_start <- 46          # simulation year 46 = 2041 (see check_stable)

## Random seeds -----------------------------------------------------------------
SEED_SENSE <- 666            # ensemble generation
SEED_SIM   <- 123            # scenario simulations

## Reference periods ------------------------------------------------------------
BASE_YEARS   <- 1:29         # 1996-2024, fitted period (normalisation baseline)
LAST10_YEARS <- 91:100       # 2086-2095, response window

## Absolute plausibility thresholds (section 08, alternative screen) -------------
CEIL  <- 10                  # > 10x baseline  = runaway
FLOOR <- 0.1                 # < 10% baseline  = functional collapse


################################################################################
## 03  LOOKUPS (GROUP NAMES, PALETTES)
################################################################################

## Functional group codes -> display names ---------------------------------------
## Moved here from the supplementary section: it is used by section 12.1,
## which previously ran before this object existed.
name_map <- c(
  "FCD.juv"   = "Cod (0-3)",
  "FCD.adult" = "Cod (4+)",
  "FHA.juv"   = "Haddock (0-2)",
  "FHA.adult" = "Haddock (3+)",
  "FSA.juv"   = "Saithe (0-3)",
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
  "ZL"  = "Zooplankton large",
  "ZS"  = "Zooplankton small",
  "ZG"  = "Gelatinous zooplankton",
  "FLC" = "Lobsters & crabs",
  "LOB" = "Norway lobster",
  "WHB" = "Baleen whale",
  "WHT" = "Tooth whale",
  "WTO" = "Delphinidae",
  "SB"  = "Seabirds")

## Full ordered group list used in the supplementary trajectory figures ----------
comp_groups_all <- c(
  "FCD.juv","FCD.adult","FHA.juv","FHA.adult",
  "FSA.juv","FSA.adult","FGH","FRF","FHE","FCA",
  "FFF","SSR","SSD","SSH","PIN","PWN","FOC","FDC",
  "FDF","FSD","FBP","CEP","WMW","FMI","FEP","FIN",
  "FKR","ZL","ZS","ZG","FLC","LOB","WHB","WHT","WTO","SB")

## Scenario labels ---------------------------------------------------------------

SCEN_SQ    <- "Status Quo"
SCEN_50    <- "Capelin -50%"
SCEN_ZERO  <- "Capelin near-zero"

## Canonical palette (colour-blind safe, Okabe-Ito) -------------------------------
pal <- c("Status Quo"        = "#2ca25f",
         "Capelin -50%"      = "#2b6cb0",
         "Capelin near-zero" = "#e11d48")

pal_ok <- c("Status Quo"        = "#009E73",
            "Capelin -50%"      = "#0072B2",
            "Capelin near-zero" = "#D55E00")


################################################################################
## 04  DATA CONNECTIONS AND BASE MODEL OBJECTS
################################################################################

mdb <- mfdb('Iceland', db_params = list(host = 'mfdb.hafro.is'))
mar <- connect_mar()

REco.params <- get(load("REco.params_Iceland.RData"))
source("pedigree.R")
REco <- rpath(REco.params, eco.name = 'R Ecosystem')

ISL_unbal <- REco.params
ISL_bal   <- rpath(ISL_unbal, eco.name = 'R Ecosystem')

scene_new_compare <- readRDS("scene_new_compare25.rds")  # fitted Ecosim scenario
scene_base        <- scene_new_compare


################################################################################
## 05  HELPER FUNCTIONS
################################################################################

## ---- 05.1  Post-history F based on five-year mean ----------------------------

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


## ---- 05.2  Warm start --------------------------------------------------------
## Aligns the model state with a given parameter draw before the analysis run.
##
## Each Ecosense draw has its own B_BaseRef, so a scene assembled from a new
## parlist starts at a state that is consistent with the balanced model but not
## with that draw's parameters or the imposed forcings. Running the scene
## forward once and carrying its end_state over as the new start_state lets the
## initial transient play out in a throwaway pre-run instead of contaminating
## the first decades of the projection.
##
## BURN_YEARS is then set to 0 because that adjustment has already happened;
## leaving it on would repeat the same settling twice.
##
## Called with years = 1:29 (the fitted period, 1996-2024), so the projection
## begins from a state consistent with historical F and recruitment forcing.
## This is an alignment step, not a guarantee of equilibrium - hence the
## separate stabilisation test in 05.3, which is what sets perturb_start.

warm_start_scene <- function(scene, years = 1:30) {
  pre <- rsim.run(scene, method = "AB", years = years)
  scene$start_state <- pre$end_state
  scene$params$BURN_YEARS <- 0
  scene
}


## ---- 05.3  Stability check ---------------------------------------------------
## Defines the perturbation start year: first year at which every retained
## biological group changes by < 2% per year across a rolling 5-year window.

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


## ---- 05.4  Wide -> long conversion -------------------------------------------

to_wide <- function(lst) {
  purrr::map_dfr(names(lst), function(irun) {
    lst[[irun]] |>
      tidyr::pivot_wider(names_from = Group, values_from = Biomass) |>
      dplyr::mutate(Run = irun) |>
      dplyr::rename_with(~paste0("Biomass.", .), -c(Year, Run))
  }, .id = "Run_ID")
}

as_long_runs <- function(df, label) {
  df |>
    pivot_longer(starts_with("Biomass."), names_to="Biomass_Type", values_to="Biomass") |>
    mutate(Biomass_Type = sub("^Biomass\\.", "", Biomass_Type),
           scenario = label)
}


## ---- 05.5  Standard scenario plot function --------------------------------------------

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


## ---- 05.6  % change vs Status Quo, any run set (used in section 13) ----------

pct_vs_sq <- function(runs, by = c("group", "tl")) {
  by <- match.arg(by)
  
  x <- runs |>
    dplyr::group_by(Run_ID, Biomass_Type) |>
    dplyr::mutate(base = mean(Biomass[Year %in% 1:29], na.rm = TRUE),
                  Rel  = Biomass / pmax(base, 1e-12)) |>
    dplyr::ungroup()
  
  if (by == "tl") {
    x <- x |>
      dplyr::left_join(dplyr::select(TL_table_v2, Biomass_Type_key, TL_group),
                       by = "Biomass_Type_key") |>
      dplyr::filter(!is.na(TL_group)) |>
      dplyr::group_by(scenario, Run_ID, TL_group, Year) |>
      dplyr::summarise(TLsum = sum(Biomass, na.rm = TRUE), .groups = "drop") |>
      dplyr::group_by(scenario, Run_ID, TL_group) |>
      dplyr::mutate(base = mean(TLsum[Year %in% 1:29], na.rm = TRUE),
                    Rel  = TLsum / pmax(base, 1e-12)) |>
      dplyr::ungroup() |>
      dplyr::rename(unit = TL_group)
  } else {
    x <- dplyr::rename(x, unit = Biomass_Type)
  }
  
  last10 <- x |>
    dplyr::filter(Year %in% 91:100) |>
    dplyr::group_by(scenario, Run_ID, unit) |>
    dplyr::summarise(M = mean(Rel, na.rm = TRUE), .groups = "drop")
  
  sq <- last10 |>
    dplyr::filter(scenario == "Status Quo") |>
    dplyr::select(Run_ID, unit, SQ = M)
  
  last10 |>
    dplyr::filter(scenario != "Status Quo") |>
    dplyr::left_join(sq, by = c("Run_ID", "unit")) |>
    dplyr::mutate(Pct = 100 * (M / pmax(SQ, 1e-12) - 1)) |>
    dplyr::group_by(scenario, unit) |>
    dplyr::summarise(Median = median(Pct, na.rm = TRUE),
                     Q025   = quantile(Pct, 0.025, na.rm = TRUE),
                     Q975   = quantile(Pct, 0.975, na.rm = TRUE),
                     n_runs = dplyr::n_distinct(Run_ID),
                     .groups = "drop")
}


## ---- 05.7  TL bin helpers (used in section 14) -------------------------------

## bin totals per run, normalised to the 1996-2024 mean, for any TL table
build_tl_rel <- function(tlt) {
  runs_long_filtered |>
    dplyr::left_join(dplyr::select(tlt, Biomass_Type_key, TL_group),
                     by = "Biomass_Type_key") |>
    dplyr::filter(!is.na(TL_group)) |>
    dplyr::group_by(scenario, Run_ID, TL_group, Year_actual) |>
    dplyr::summarise(TLsum = sum(Biomass, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(scenario, Run_ID, TL_group) |>
    dplyr::mutate(base = mean(TLsum[(Year_actual - 1995) %in% 1:29], na.rm = TRUE),
                  Rel  = TLsum / pmax(base, 1e-12)) |>
    dplyr::ungroup()
}

## % change vs Status Quo over the final decade
pct_last10 <- function(tl_rel) {
  x <- tl_rel |>
    dplyr::filter(Year_actual %in% 2086:2095) |>
    dplyr::group_by(scenario, Run_ID, TL_group) |>
    dplyr::summarise(M = mean(Rel, na.rm = TRUE), .groups = "drop")
  
  sq <- x |>
    dplyr::filter(scenario == "Status Quo") |>
    dplyr::select(Run_ID, TL_group, SQ = M)
  
  x |>
    dplyr::filter(scenario != "Status Quo") |>
    dplyr::left_join(sq, by = c("Run_ID", "TL_group")) |>
    dplyr::mutate(Pct = 100 * (M / pmax(SQ, 1e-12) - 1)) |>
    dplyr::group_by(scenario, TL_group) |>
    dplyr::summarise(Median = median(Pct, na.rm = TRUE),
                     Q025   = quantile(Pct, 0.025, na.rm = TRUE),
                     Q975   = quantile(Pct, 0.975, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::mutate(Stat = sprintf("%.1f (%.1f, %.1f)", Median, Q025, Q975))
}

## bin re-assignment with shifted boundaries
rebin <- function(tl, shift = 0) {
  cut(tl, breaks = c(-Inf, 2, 3, 4, Inf) + shift, right = FALSE,
      labels = c("1-2", "2-3", "3-4", "4-5+"))
}


## ---- 05.8  Supplementary ensemble trajectory figure --------------------------

make_ens_fig <- function(runs, groups) {
  
  d <- runs |>
    dplyr::filter(Biomass_Type %in% groups) |>
    dplyr::group_by(scenario, Run_ID, Biomass_Type) |>
    dplyr::mutate(base = mean(Biomass[Year %in% 1:29], na.rm = TRUE),
                  Rel  = Biomass / pmax(base, 1e-12)) |>
    dplyr::ungroup() |>
    dplyr::filter(Year_actual >= 1996) |>
    dplyr::mutate(
      Group    = factor(dplyr::recode(Biomass_Type, !!!name_map),
                        levels = dplyr::recode(groups, !!!name_map)),
      scenario = factor(scenario, levels = names(pal_ok)))
  
  stopifnot(!any(is.na(d$scenario)), !any(is.na(d$Group)))
  
  med <- d |>
    dplyr::group_by(Group, scenario, Year_actual) |>
    dplyr::summarise(Med = median(Rel, na.rm = TRUE), .groups = "drop")
  
  ggplot(d, aes(Year_actual, Rel)) +
    geom_line(aes(group = Run_ID, colour = scenario),
              alpha = 0.35, linewidth = 0.2, show.legend = FALSE) +
    geom_hline(yintercept = 1, linetype = "dotted", colour = "grey40") +
    geom_line(data = med, aes(y = Med), colour = "black", linewidth = 0.5) +
    facet_grid(Group ~ scenario, scales = "free_y", switch = "y") +
    scale_colour_manual(values = pal_ok) +
    scale_x_continuous(breaks = c(2000, 2040, 2080)) +
    scale_y_continuous(n.breaks = 3) +
    labs(x = "Year",
         y = expression("Relative biomass (B / B"[mean]*")"),
         colour = "Scenario") +
    theme_bw(base_size = 9) +
    theme(legend.position = "top",
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.spacing.y = unit(0.25, "lines"),
          strip.text.y.left = element_text(angle = 0, hjust = 1, size = 8),
          strip.text.x = element_text(face = "bold"),
          strip.background.y = element_blank(),
          strip.placement = "outside",
          axis.text = element_text(size = 7))
}


################################################################################
## 06  ENSEMBLE GENERATION (ECOSENSE)
################################################################################

## ---- 06.1  Apply post-history F ----------------------------------------------

scene_new_compare <- update_forced_f_rate_5yrmean(
  scene_new_compare, scene_new_compare$fitting$Catch,
  end_year = 29, damp = 1)


## ---- 06.2  Monte Carlo draws and screening burn-in ---------------------------
## Vvary: +/- 0.5 log10 units.  Dvary: log(0.6) to log(1.4), i.e. 0.6x-1.4x.

set.seed(SEED_SENSE)
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
    
    # Use a *screening* burn-in to reject unstable parameter sets
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


## ---- 06.3  Extract, save and reload accepted parameter sets -------------------

kept <- do.call("c", map(results, "kept"))
KEPT <- which(kept)
parlist <- map(results, "parlist")
isl.sense_final <- parlist[KEPT]
saveRDS(isl.sense_final, file = "isl_sense_final.rds")

# Reload later with:
isl.sense_final <- readRDS("isl_sense_final.rds")


## ---- 06.4  Determine the stabilisation year ----------------------------------

test <- rsim.run(scene_new_compare, method = "AB", years = 1:100)
stab <- check_stable(test$annual_Biomass)
stab                              # perturb_start (section 02) is set from this


################################################################################
## 07  SCENARIO SIMULATIONS
################################################################################

## ---- 07.1  Run Status Quo, -50% and near-zero for every accepted draw --------

set.seed(SEED_SIM)

results_capelin <-
  do.call("c",
          parallel::mclapply(1:length(KEPT), function(irun){
            
            out <- list()
            run.scene <- scene_new_compare
            run.scene$params <- isl.sense_final26[[irun]]
            
            # start biomass = this draw's Ecopath equilibrium
            run.scene$start_state$Biomass <- run.scene$params$B_BaseRef
            
            # ensure key groups are integrated
            run.scene$params$NoIntegrate[c("FCD.adult","FCD.juv",
                                           "FHA.adult","FHA.juv",
                                           "FSA.adult","FSA.juv",
                                           "FCA")] <- 0
            
            # warm-start (pre-run to align state with params/forcings)
            run.scene <- warm_start_scene(run.scene, years = 1:29)
            
            ## --- Status Quo
            sq <- rsim.run(run.scene, method = "AB", years = fit.years)
            sq_dt <- as.data.table(sq$annual_Biomass)[, Year := 1:100][Year > BURN_IN]
            out[[paste0("Status_Quo_", irun)]] <- melt(
              sq_dt, id.vars = "Year", variable.name = "Group", value.name = "Biomass"
            )
            
            ## --- Reference FCA biomass (years 91-100, already > BURN_IN)
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


## ---- 07.2  Split by scenario and save ----------------------------------------

results_down50 <- results_capelin[grep("Down_50_",  names(results_capelin))]
results_down0  <- results_capelin[grep("Down_Zero_", names(results_capelin))]
results_sq     <- results_capelin[grep("Status_Quo_", names(results_capelin))]

save(results_down50, file = "files_for_capelin_paper/results_down50.RData")
save(results_down0,  file = "files_for_capelin_paper/results_down0.RData")
save(results_sq,     file = "files_for_capelin_paper/results_sq.RData")


################################################################################
## 08  POST-PROCESSING AND PLAUSIBILITY SCREENING
################################################################################

## ---- 08.1  Reload raw output -------------------------------------------------

load("files_for_capelin_paper/results_down50.RData")  # loads results_down50
load("files_for_capelin_paper/results_down0.RData")   # loads results_down0
load("files_for_capelin_paper/results_sq.RData")      # loads results_sq


## ---- 08.2  Build wide data frames --------------------------------------------

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


## ---- 08.3  Label scenarios ---------------------------------------------------

res_m1_sq     <- res_m1_sq     |> mutate(scenario = "Status Quo")
res_m1_down50 <- res_m1_down50 |> mutate(scenario = "Capelin -50%")
res_m1_down0  <- res_m1_down0  |> mutate(scenario = "Capelin near-zero")


## ---- 08.4  Trophic-level lookup ----------------------------------------------

TL_table <- data.frame(
  Biomass_Type = paste0("Biomass.", names(REco$TL)),
  TL           = as.numeric(REco$TL),
  GroupType    = REco$Group
) |>
  filter(!Biomass_Type %in% c("Biomass.GILLNETS","Biomass.LONGLINE","Biomass.OTHER",
                              "Biomass.PELAGIC","Biomass.SEINERS","Biomass.TRAWLS",
                              "Biomass.HARPOON","Biomass.FCA", "Biomass.Detritus")) |>
  mutate(TL_group = case_when(
    TL < 2 ~ "1-2",
    TL < 3 ~ "2-3",
    TL < 4 ~ "3-4",
    TRUE   ~ "4-5+"
  ),
  TL_group = factor(TL_group, levels = c("1-2","2-3","3-4","4-5+")))


## ---- 08.5  Long form across scenarios ----------------------------------------

sq_long    <- as_long_runs(res_m1_sq,     "Status Quo")
d50_long   <- as_long_runs(res_m1_down50, "Capelin -50%")
dzero_long <- as_long_runs(res_m1_down0,  "Capelin near-zero")

runs_long  <- bind_rows(sq_long, d50_long, dzero_long) |>
  mutate(Biomass_Type_key = paste0("Biomass.", Biomass_Type),
         Year_actual = Year + 1995)


## ---- 08.6  PRIMARY SCREEN: two-sided percentile thresholds -------------------

pred_runs_comp <- runs_long %>%
  group_by(Run_ID, Biomass_Type) %>%
  mutate(
    base = mean(Biomass[Year %in% 1:29], na.rm = TRUE),
    Rel  = Biomass / pmax(base, 1e-12)
  ) %>%
  ungroup()

# Compute max and min relative biomass per scenario x group x run
group_run_minmax <- pred_runs_comp %>%
  group_by(scenario, Biomass_Type, Run_ID) %>%
  summarise(
    maxRel = max(Rel, na.rm = TRUE),
    minRel = min(Rel, na.rm = TRUE),
    .groups = "drop"
  )

# Define upper AND lower extreme cutoffs
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

cat("Total runs:             ", n_distinct(runs_long$Run_ID), "\n")
cat("Removed (spikes):       ", length(bad_runs), "\n")
cat("Kept:                   ", n_distinct(runs_long_filtered$Run_ID), "\n")
cat("Percent removed:        ",
    round(100 * length(bad_runs) / n_distinct(runs_long$Run_ID), 1), "%\n\n")



## ---- 08.8  Derived objects used throughout sections 09-14 --------------------

## relative biomass (B/Bmean) for all groups
runs_rel <- runs_long_filtered %>%
  group_by(Run_ID, Biomass_Type) %>%
  mutate(
    base = mean(Biomass[Year %in% 1:29], na.rm = TRUE),
    Rel  = Biomass / pmax(base, 1e-12)) %>%
  ungroup()

## trophic-level bin totals, normalised per run
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

## sanity checks
cat("runs kept:", dplyr::n_distinct(runs_long_filtered$Run_ID),
    "of", dplyr::n_distinct(runs_long$Run_ID), "\n")
stopifnot(nrow(runs_rel) > 0, nrow(runs_tl_rel) > 0)


################################################################################
## 09  MAIN FIGURES
################################################################################

## ---- FIGURE 1  Capelin trajectories ------------------------------------------

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

ggsave(file.path(FIGS, "Fig1_Capelin_relative_IQR.png"),
       Fig1, width = 120, height = 100, units = "mm", dpi = 350)
ggsave(file.path(FIGS, "Fig1_Capelin_relative_IQR.tiff"),   # TIFF for submission
       Fig1,
       width = 140, height = 100, units = "mm", dpi = 350)


## ---- FIGURE 2  Trophic-level bins --------------------------------------------

tl_rel_q <- runs_tl_rel %>%
  group_by(scenario, TL_group, Year_actual) %>%
  summarize(
    Med = median(Rel, na.rm = TRUE),
    Q25 = quantile(Rel, 0.025, na.rm = TRUE),
    Q75 = quantile(Rel, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

Fig2 <- capelin_plots(tl_rel_q %>% filter(Year_actual >= 1996),
                      x="Year_actual", yM="Med", yL="Q25", yU="Q75",
                      title = "",
                      ylab = expression("Relative biomass (B / B"[mean] * ")"),
                      facet = "TL_group", ncol = 2, hline = 1)
Fig2

ggsave(file.path(FIGS, "Fig2_TLbins_relative_IQR.png"),
       Fig2, width = 10, height = 5.2, dpi = 350)
ggsave(file.path(FIGS, "Fig2_TLbins_relative_IQR.tiff"),
       Fig2, width = 130, height = 150, units = "mm", dpi = 350)


## ---- FIGURE 3  Key predators -------------------------------------------------

key_pred_groups <- c("FCD.adult","FCD.juv","FHA.adult","FSA.juv","WMW","SB")

pred_runs <- runs_long_filtered %>%
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
                        "FHA.adult"="Haddock (adult)", "FSA.juv"="Saithe (juvenile)",
                        "WMW"="Minke whales", "SB"="Seabirds"))

Fig3 <- capelin_plots(pred_q %>% filter(Year_actual >= 1996),
                      x="Year_actual", yM="Med", yL="Q25", yU="Q75",
                      title = "",
                      ylab = expression("Relative biomass (B / B"[mean] * ")"),
                      facet = "Group", ncol = 3, hline = 1)
Fig3
ggsave(file.path(FIGS, "Fig3_KeyPredators_relative_IQR.png"),
       Fig3, width = 10, height = 5, dpi = 350)


## ---- FIGURE 4  Ecosystem-level indicators ------------------------------------

runs_with_TL <- runs_long_filtered %>%
  left_join(select(TL_table, Biomass_Type, TL), by = c("Biomass_Type_key" = "Biomass_Type")) %>%
  filter(!is.na(TL))

## NOTE: the second assignment (excluding capelin, detritus, phytoplankton)
## is the one used. The first is kept for reference only.
ind_per_run <- runs_with_TL %>% group_by(scenario, Run_ID, Year_actual) %>%
  summarize( TotalB = sum(Biomass, na.rm = TRUE),
             MTLbio = sum(Biomass * TL, na.rm = TRUE) / pmax(sum(Biomass, na.rm = TRUE), 1e-12),
             PredB = sum(Biomass[TL >= 3], na.rm = TRUE),
             PreyB = sum(Biomass[TL < 3], na.rm = TRUE),
             PP = PredB / pmax(PreyB, 1e-12), .groups = "drop" )

# without capelin, detritus and phytoplankton  <- USED
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
  filter(Year_actual >= max(Year_actual) - 9) %>%
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
                            MTL = "\u0394 Mean trophic level",
                            PP  = "Predator : Prey ratio"))

facet_levels <- c("Relative total biomass",
                  "\u0394 Mean trophic level",
                  "Predator : Prey ratio")

end_summary_long <- end_summary_long %>%
  mutate(
    Indicator = factor(Indicator, levels = facet_levels, ordered = TRUE),
    FacetID = as.integer(factor(Indicator, levels = facet_levels)))

tag_df <- data.frame(
  FacetID = 1:3,
  tag = c("a)", "b)", "c)"))

Fig4 <- ggplot(end_summary_long,
               aes(x = scenario, y = M, color = scenario)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = L, ymax = U), width = 0.15, size = 0.5) +
  facet_wrap(~ FacetID, scales = "free_y", nrow = 1,
             labeller = as_labeller(setNames(facet_levels, 1:3))) +
  geom_hline(aes(yintercept = ifelse(Indicator == "\u0394 Mean trophic level", 0, 1)),
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
    "Capelin near-zero" = "#E83E4E",
    "Status Quo"   = "#2CA25F" )) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "black"),
    strip.text = element_text(face = "bold")) +
  labs(y = NULL, x = NULL, color = "Scenario")
Fig4
ggsave(file.path(FIGS, "Fig4_Indicators_combined.png"),
       Fig4, width = 10, height = 4, dpi = 350)


## ---- FIGURE 5  Competitors of capelin (diet overlap) -------------------------

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

# Top 6 competitors
comp_groups   <- c("CEP","FHE","FBP","ZG","FSD","FKR")
name_map_comp <- c("CEP"="Cephalopods","FHE"="Herring","FBP"="Small pelagic fish",
                   "ZG"="Gelatinous zooplankton","FSD"="Sandeel","FKR"="Krill")

pred_runs_comp <- runs_long_filtered %>%
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

Fig5 <- capelin_plots(pred_q_comp %>% filter(Year_actual >= 1996),
                      x="Year_actual", yM="Med", yL="Q25", yU="Q75",
                      title = "",
                      ylab = expression("Relative biomass (B / B"[mean] * ")"),
                      facet = "Group", ncol = 3, hline = 1)
Fig5
ggsave(file.path(FIGS, "Fig5_Competitors_relative_IQR.png"),
       Fig5, width = 10, height = 5, dpi = 350)


## ---- FIGURE 6  Capelin prey --------------------------------------------------
## NOTE: comp_groups / name_map_comp are reassigned here. Re-run the
## Figure 5 block before Figure 5 if you execute out of order.

comp_groups   <- c("ZL","FKR", "ZS")
name_map_comp <- c("ZL" = "Large zooplankton","FKR"="Krill", "ZS" = "Small Zooplankton")

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
                      ylab = expression("Relative biomass (B / B"[mean] * ")"),
                      facet = "Group", ncol = 3, hline = 1)
Fig6
ggsave(file.path(FIGS, "Fig6_capelin_prey_relative_IQR.png"),
       Fig6, width = 10, height = 4, dpi = 350)


################################################################################
## 10  MAIN TABLES
################################################################################

## ---- 10.1  Group-level % change vs Status Quo (years 91-100) -----------------

dat_rel_last10 <- runs_rel %>%
  filter(Year %in% 91:100) %>%
  group_by(scenario, Run_ID, Biomass_Type) %>%
  summarise(MeanLast10 = mean(Rel, na.rm = TRUE), .groups = "drop")

# Status Quo baselines per run/group
sq_rel <- dat_rel_last10 %>%
  filter(scenario == "Status Quo") %>%
  select(Run_ID, Biomass_Type, SQ = MeanLast10)

# % difference vs Status Quo
dat_rel_comp <- dat_rel_last10 %>%
  filter(scenario != "Status Quo") %>%
  left_join(sq_rel, by = c("Run_ID","Biomass_Type")) %>%
  mutate(
    Pct_vs_SQ = 100 * (MeanLast10 / SQ - 1))

# summary stats per group per scenario  -> Table S7
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


## ---- 10.2  Trophic-level bin % change vs Status Quo  -> Table S6 -------------

tl_last10 <- runs_tl_rel %>%
  filter(Year_actual %in% 2086:2095) %>%     # Years 91-100
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


################################################################################
## 11  ADDITIONAL ANALYSES (REVISION)
################################################################################
## Exploratory versions of the analyses formalised in section 14. Objects here
## duplicate names used elsewhere (runs_long, runs_long_filtered, sq_rel,
## grp_med, resp_dep). 

## ---- 11.1  Bin-edge fragility: distance to nearest bin boundary --------------

TL_table |>
  dplyr::mutate(dist_to_edge = pmin(TL - floor(TL), ceiling(TL) - TL)) |>
  dplyr::arrange(dist_to_edge) |>
  dplyr::select(Biomass_Type, GroupType, TL, TL_group, dist_to_edge) |>
  head(40)


## ---- 11.2  Continuous TL: % change without bins ------------------------------

tl_lookup <- TL_table |>
  dplyr::transmute(Biomass_Type = sub("^Biomass\\.", "", Biomass_Type), TL, TL_group)

grp_change <- runs_rel |>
  dplyr::filter(Year %in% 91:100) |>
  dplyr::group_by(scenario, Run_ID, Biomass_Type) |>
  dplyr::summarise(MeanLast10 = mean(Rel, na.rm = TRUE), .groups = "drop") |>
  dplyr::left_join(sq_rel, by = c("Run_ID", "Biomass_Type")) |>
  dplyr::mutate(Pct_vs_SQ = 100 * (MeanLast10 / SQ - 1)) |>
  dplyr::inner_join(tl_lookup, by = "Biomass_Type") |>
  dplyr::filter(scenario != "Status Quo")

stopifnot(nrow(grp_change) > 0, !any(is.na(grp_change$TL)))

grp_med <- grp_change |>
  dplyr::group_by(scenario, Biomass_Type, TL) |>
  dplyr::summarise(Med = median(Pct_vs_SQ, na.rm = TRUE),
                   Q025 = quantile(Pct_vs_SQ, 0.025, na.rm = TRUE),
                   Q975 = quantile(Pct_vs_SQ, 0.975, na.rm = TRUE),
                   .groups = "drop")

ggplot(grp_med, aes(x = TL, y = Med, colour = scenario)) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey40") +
  geom_vline(xintercept = c(2, 3, 4), linetype = "dashed", colour = "grey80") +
  geom_linerange(aes(ymin = Q025, ymax = Q975), alpha = 0.35) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE, span = 1, linewidth = 0.7) +
  scale_colour_manual(values = pal) +
  facet_wrap(~ scenario, ncol = 2) +
  labs(x = "Trophic level (balanced base model)",
       y = "% change from Status Quo (2086-2095)", colour = "Scenario") +
  theme_bw(base_size = 13) +
  theme(legend.position = "top", panel.grid.minor = element_blank())


## ---- 11.3  Capelin dependence vs trophic level -------------------------------

codes <- names(REco$Biomass)
d_cap <- REco$DC["FCA", ]
pred_codes <- names(d_cap)

dep <- tibble::tibble(
  Biomass_Type = pred_codes,
  B     = as.numeric(REco$Biomass[pred_codes]),
  QB    = as.numeric(REco$QB[pred_codes]),
  d_cap = as.numeric(d_cap)
) |>
  dplyr::mutate(Ig = QB * d_cap)

stopifnot(!any(is.na(dep$B)), !any(is.na(dep$QB)))

resp_dep <- grp_med |>
  dplyr::inner_join(dep, by = "Biomass_Type") |>
  dplyr::mutate(dep_class = dplyr::case_when(
    d_cap >= 0.10 ~ "capelin predator",
    TRUE          ~ "other"))

ggplot(resp_dep, aes(x = Ig, y = Med, colour = TL, size = B)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_point(alpha = 0.85) +
  ggrepel::geom_text_repel(aes(label = Biomass_Type), size = 3,
                           show.legend = FALSE, max.overlaps = 20) +
  scale_colour_viridis_c(option = "C") +
  scale_size_continuous(range = c(1.5, 8), guide = "none") +
  facet_wrap(~ scenario) +
  labs(x = expression("Capelin dependence  I"[g]*" = (Q/B)"[g]*" \u00d7 d"[g*",cap"]*"  (yr"^-1*")"),
       y = "% change from Status Quo (2086-2095)", colour = "TL") +
  theme_bw(base_size = 13)

## does dependence explain more variance than trophic level?
summary(lm(Med ~ Ig, data = dplyr::filter(resp_dep, scenario == "Capelin zero")))
summary(lm(Med ~ TL, data = dplyr::filter(resp_dep, scenario == "Capelin zero")))

resp_class <- resp_dep |>
  dplyr::mutate(class = dplyr::case_when(
    d_cap >= 0.25 ~ "Major capelin predator",
    d_cap >= 0.05 ~ "Minor capelin predator",
    TRUE          ~ "Non-predator"))

resp_class |>
  dplyr::group_by(scenario, class) |>
  dplyr::summarise(n = dplyr::n(),
                   Med_response = median(Med),
                   IQR_low  = quantile(Med, 0.25),
                   IQR_high = quantile(Med, 0.75),
                   .groups = "drop")

kruskal.test(Med ~ factor(class), data = dplyr::filter(resp_class, scenario == "Capelin zero"))

for (hi in c(0.20, 0.25, 0.30)) for (lo in c(0.03, 0.05, 0.10)) {
  rc <- resp_dep |>
    dplyr::filter(scenario == "Capelin zero") |>
    dplyr::mutate(cl = dplyr::case_when(d_cap >= hi ~ "major",
                                        d_cap >= lo ~ "minor", TRUE ~ "non"))
  kt <- kruskal.test(Med ~ factor(cl), data = rc)
  cat(sprintf("hi=%.2f lo=%.2f  p=%.2e  n=%s\n", hi, lo, kt$p.value,
              paste(table(rc$cl), collapse = "/")))
}

sort(dep$d_cap[dep$d_cap > 0])


################################################################################
## 12  SUPPLEMENTARY FIGURES
################################################################################

## ---- 12.1  Predator dependence (Ig, ARg)  -> Figure S3 -----------------------

diet_mat    <- REco$DC
QB          <- REco$QB
B           <- REco$Biomass
group_names <- REco$Group
codes       <- names(B)

prey <- "FCA"                      # capelin as prey

# Diet share on capelin for each predator
d_cap <- diet_mat[ prey,]

# Keep only biological consumers (B > 0, QB > 0)
valid_preds <- which(B > 0 & QB > 0)

Ik <- QB[valid_preds] * d_cap[valid_preds]                     # relative dependence
Fk <- B[valid_preds]  * QB[valid_preds] * d_cap[valid_preds]   # absolute removal

dependence_table <- data.frame(
  Predator   = codes[valid_preds],
  GroupName  = group_names[valid_preds],
  Ik         = Ik,
  Fk         = Fk
) %>%
  filter(Ik > 0 | Fk > 0) %>%          # drop non-predators of capelin
  arrange(desc(Ik))                    # rank by relative dependence

dependence_table <- dependence_table %>%
  mutate(Top6 = Predator %in% head(Predator, 6))

dependence_table_plot <- dependence_table %>%
  mutate(
    GroupName_label = recode(
      as.character(GroupName),
      !!!name_map,
      .default = as.character(GroupName)
    )
  )

# Relative dependence (Ik)
p1 <- dependence_table_plot %>%
  slice_max(order_by = Ik, n = 15) %>%
  ggplot(aes(x = reorder(GroupName_label, Ik), y = Ik, fill = Top6)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "grey70")) +
  labs(x = "Predator", y = "Relative dependence (Ig)", fill = "Top 6") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

# Absolute removal (Fk)
p2 <- dependence_table_plot %>%
  slice_max(order_by = Fk, n = 15) %>%
  ggplot(aes(x = reorder(GroupName_label, Fk), y = Fk, fill = Top6)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "grey70")) +
  labs(x = "Predator", y = "Absolute removal (ARg, biomass units)", fill = "Top 6") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

Fig_S1 <- p1 | p2
ggsave(file.path(FIGS, "Fig_S1_CapelinPredators_Ik_Fk.png"),
       Fig_S1, width = 12, height = 5.5, units = "in", dpi = 350)


## ---- 12.2  Schoener diet overlap  -> Figure S4 -------------------------------

diet_df    <- as.data.frame(REco.params$diet)
pred_names <- setdiff(names(diet_df), "Group")
diet_mat   <- as.matrix(diet_df[, pred_names, drop = FALSE])
diet_mat   <- sweep(diet_mat, 2, pmax(colSums(diet_mat, na.rm = TRUE), 1e-12), `/`)

cap <- "FCA"                       # capelin as consumer
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

shared_Q <- shared_frac * (B[valid] * QB[valid])   # overlap x consumer demand proxy

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
plot_dat <- scho_tbl %>% mutate(SharedQ_Mt = SharedQ/1e6, is_top6 = Group %in% top6_ids)

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
       y = "Shared consumption on common prey (million tonnes \u00b7 yr\u207b\u00b9)") +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

Fig_S22 <- pA | pB
ggsave(file.path(FIGS, "Fig_S22_competition_schoener.png"),
       Fig_S22, width = 14, height = 10, dpi = 350)


## ---- 12.3  Ensemble trajectories, unfiltered and filtered  -> Figs S1, S2 ----
## Reviewer comment S3: legible panels, colour-blind safe palette, split into
## chunks of 18 groups so each panel is large enough to read.

chunks <- split(comp_groups_all, ceiling(seq_along(comp_groups_all) / 18))

for (i in seq_along(chunks)) {
  ggsave(file.path(FIGS, sprintf("xFigS1_unfiltered_%d.png", i)),
         make_ens_fig(runs_long, chunks[[i]]),
         width = 7.5, height = 10, dpi = 400)
  
  ggsave(file.path(FIGS, sprintf("xFigS2_filtered_%d.png", i)),
         make_ens_fig(runs_long_filtered, chunks[[i]]),
         width = 7.5, height = 10, dpi = 400)
}


################################################################################
## 13  FILTER SENSITIVITY (REVIEWER Q3)
################################################################################
## Compares filtered (n = 668) against unfiltered (n = 2,937) ensembles.

## ---- 13.1  TL lookup used by pct_vs_sq() -------------------------------------

drop_from_TL <- c("GILLNETS","LONGLINE","OTHER","PELAGIC","SEINERS","TRAWLS",
                  "HARPOON","FCA","Detritus")

TL_table_v2 <- tibble::tibble(
  Biomass_Type = names(REco$TL),
  TL           = as.numeric(REco$TL),
  GroupName    = REco$Group
) |>
  dplyr::filter(!Biomass_Type %in% drop_from_TL) |>
  dplyr::mutate(
    Biomass_Type_key = paste0("Biomass.", Biomass_Type),
    TL_group = cut(TL, breaks = c(-Inf, 2, 3, 4, Inf), right = FALSE,
                   labels = c("1-2", "2-3", "3-4", "4-5+")),
    B0 = as.numeric(REco$Biomass[Biomass_Type]))

stopifnot(!any(is.na(TL_table_v2$TL_group)), !any(is.na(TL_table_v2$B0)))
nrow(TL_table_v2)   # expect 36: 45 groups minus 6 gears, HARPOON, FCA and Detritus


## ---- 13.2  Trophic-level bins ------------------------------------------------

tl_filt <- pct_vs_sq(runs_long_filtered, "tl") |> dplyr::mutate(ensemble = "Filtered")
tl_unf  <- pct_vs_sq(runs_long,          "tl") |> dplyr::mutate(ensemble = "Unfiltered")

tl_compare <- dplyr::bind_rows(tl_filt, tl_unf) |>
  dplyr::mutate(Stat = sprintf("%.1f (%.1f, %.1f)", Median, Q025, Q975)) |>
  dplyr::select(scenario, unit, ensemble, Stat) |>
  tidyr::pivot_wider(names_from = ensemble, values_from = Stat)

tl_compare |> print(n = 20)

## how far do the medians move?
tl_shift <- dplyr::left_join(
  dplyr::select(tl_filt, scenario, unit, Med_filt = Median, L_filt = Q025, U_filt = Q975),
  dplyr::select(tl_unf,  scenario, unit, Med_unf  = Median, L_unf  = Q025, U_unf  = Q975),
  by = c("scenario", "unit")) |>
  dplyr::mutate(
    diff_median  = Med_unf - Med_filt,
    width_filt   = U_filt - L_filt,
    width_unf    = U_unf  - L_unf,
    width_ratio  = width_unf / width_filt,
    same_sign    = sign(Med_filt) == sign(Med_unf))

tl_shift |>
  dplyr::select(scenario, unit, Med_filt, Med_unf, diff_median, width_ratio, same_sign) |>
  print(n = 20)


## ---- 13.3  Group level -------------------------------------------------------

grp_filt <- pct_vs_sq(runs_long_filtered, "group") |> dplyr::mutate(ensemble = "Filtered")
grp_unf  <- pct_vs_sq(runs_long,          "group") |> dplyr::mutate(ensemble = "Unfiltered")

grp_shift <- dplyr::left_join(
  dplyr::select(grp_filt, scenario, unit, Med_filt = Median),
  dplyr::select(grp_unf,  scenario, unit, Med_unf  = Median),
  by = c("scenario", "unit")) |>
  dplyr::mutate(diff = Med_unf - Med_filt, same_sign = sign(Med_filt) == sign(Med_unf))

## headline numbers for the write-up
cat("Groups keeping the same sign of response:",
    sum(grp_shift$same_sign, na.rm = TRUE), "of", nrow(grp_shift), "\n")
cat("Median absolute shift in group median (pp):",
    round(median(abs(grp_shift$diff), na.rm = TRUE), 2), "\n")
cat("Largest shifts:\n")
grp_shift |> dplyr::arrange(dplyr::desc(abs(diff))) |> head(8) |> print()


## ---- 13.4  Supplementary figure ----------------------------------------------

fig_filt <- dplyr::bind_rows(tl_filt, tl_unf) |>
  ggplot(aes(x = unit, y = Median, colour = ensemble)) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey40") +
  geom_pointrange(aes(ymin = Q025, ymax = Q975),
                  position = position_dodge(width = 0.45), size = 0.4) +
  scale_colour_manual(values = c(Filtered = "#2C7FB8", Unfiltered = "#B8632C")) +
  facet_wrap(~ scenario, ncol = 2) +
  labs(x = "Trophic-level bin", y = "% change from Status Quo (2086-2095)",
       colour = "Ensemble") +
  theme_bw(base_size = 13) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave(file.path(FIGS, "FigSX_filter_sensitivity.png"), fig_filt,
       width = 9, height = 4.5, dpi = 350)

save(tl_compare, tl_shift, grp_shift,
     file = file.path(DIR, "filter_sensitivity.RData"))


################################################################################
## 14  TROPHIC-LEVEL BIN SENSITIVITY (S5)
##
##   Answers Reviewer #3 question Q2 and Editor point E3.
##
##     14.1  groups close to a bin boundary          -> Table S5.1
##     14.2  bin boundaries shifted by +/- 0.25 TL   -> Table S5.2
##     14.3  continuous TL + dependence classes      -> Figures S5.1, S5.2
##
##   Requires: REco, runs_long_filtered, TL_table_v2, name_map, DIR, FIGS
##   Helpers build_tl_rel(), pct_last10() and rebin() are in section 05.7.
################################################################################

## ---- 14.1  Groups close to a bin boundary ------------------------------------
## NB: 5 is NOT a boundary. The top bin is open, so groups above TL 4 cannot
## cross an upper edge.

bin_edges <- c(2, 3, 4)

near_edge <- TL_table_v2 |>
  dplyr::mutate(
    GroupName = dplyr::recode(Biomass_Type, !!!name_map),
    dist_edge = vapply(TL, function(x) min(abs(x - bin_edges)), numeric(1))) |>
  dplyr::group_by(TL_group) |>
  dplyr::mutate(bin_share = 100 * B0 / sum(B0, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::arrange(dist_edge)

## Table S5.1: groups within 0.15 TL of a real boundary
table_S5_1 <- near_edge |>
  dplyr::filter(dist_edge < 0.15) |>
  dplyr::transmute(
    Group        = GroupName,
    TL           = round(TL, 3),
    `Distance to boundary` = round(dist_edge, 3),
    `Baseline biomass (t)` = formatC(B0, format = "d", big.mark = ","),
    `Bin`        = as.character(TL_group),
    `Share of bin biomass (%)` = round(bin_share, 1)) |>
  dplyr::arrange(`Distance to boundary`)

print(table_S5_1, n = 20)

## how much of each bin sits near, or exactly on, a boundary
bin_exposure <- near_edge |>
  dplyr::group_by(TL_group) |>
  dplyr::summarise(
    n_groups        = dplyr::n(),
    share_on_edge   = round(sum(bin_share[dist_edge == 0]), 1),
    share_near_edge = round(sum(bin_share[dist_edge > 0 & dist_edge < 0.15]), 1),
    .groups = "drop")

print(bin_exposure)
## -> use these percentages in the S5.1 text


## ---- 14.2  Sensitivity to bin boundary placement -----------------------------

pct_by_shift <- function(shift) {
  tlt <- TL_table_v2 |> dplyr::mutate(TL_group = rebin(TL, shift))
  pct_last10(build_tl_rel(tlt)) |> dplyr::mutate(shift = shift)
}

shift_results <- purrr::map_dfr(c(-0.25, 0, 0.25), pct_by_shift)

table_S5_2 <- shift_results |>
  dplyr::select(scenario, TL_group, shift, Stat) |>
  tidyr::pivot_wider(names_from = shift, values_from = Stat) |>
  dplyr::rename(`Boundaries -0.25` = `-0.25`,
                `Boundaries as used` = `0`,
                `Boundaries +0.25` = `0.25`)

print(table_S5_2, n = 20)

## CHECK: the middle column must reproduce table_tl_wide (section 10.2) exactly
## before any of this is interpreted.

## which groups actually move under each shift?
moved <- TL_table_v2 |>
  dplyr::transmute(
    Group = dplyr::recode(Biomass_Type, !!!name_map),
    TL = round(TL, 3),
    base  = as.character(rebin(TL, 0)),
    minus = as.character(rebin(TL, -0.25)),
    plus  = as.character(rebin(TL,  0.25))) |>
  dplyr::filter(base != minus | base != plus)

print(moved, n = 30)
cat("\nGroups changing bin under at least one shift:", nrow(moved), "\n")

## does the direction of the headline contrast survive?
shift_results |>
  dplyr::filter(TL_group %in% c("3-4", "4-5+")) |>
  dplyr::select(scenario, TL_group, shift, Median) |>
  tidyr::pivot_wider(names_from = shift, values_from = Median) |>
  print()


## ---- 14.3  Continuous TL and dependence classes ------------------------------

## group-level % change with baseline TL attached
runs_rel <- runs_long_filtered |>
  dplyr::group_by(Run_ID, Biomass_Type) |>
  dplyr::mutate(base = mean(Biomass[Year %in% 1:29], na.rm = TRUE),
                Rel  = Biomass / pmax(base, 1e-12)) |>
  dplyr::ungroup()

last10 <- runs_rel |>
  dplyr::filter(Year %in% 91:100) |>
  dplyr::group_by(scenario, Run_ID, Biomass_Type) |>
  dplyr::summarise(M = mean(Rel, na.rm = TRUE), .groups = "drop")

sq_rel <- last10 |>
  dplyr::filter(scenario == "Status Quo") |>
  dplyr::select(Run_ID, Biomass_Type, SQ = M)

tl_lookup <- TL_table_v2 |> dplyr::select(Biomass_Type, TL, TL_group, B0)

grp_med <- last10 |>
  dplyr::filter(scenario != "Status Quo") |>
  dplyr::left_join(sq_rel, by = c("Run_ID", "Biomass_Type")) |>
  dplyr::mutate(Pct = 100 * (M / pmax(SQ, 1e-12) - 1)) |>
  dplyr::inner_join(tl_lookup, by = "Biomass_Type") |>
  dplyr::group_by(scenario, Biomass_Type, TL, TL_group, B0) |>
  dplyr::summarise(Med  = median(Pct, na.rm = TRUE),
                   Q025 = quantile(Pct, 0.025, na.rm = TRUE),
                   Q975 = quantile(Pct, 0.975, na.rm = TRUE),
                   .groups = "drop") |>
  dplyr::mutate(GroupName = dplyr::recode(Biomass_Type, !!!name_map))

stopifnot(nrow(grp_med) > 0, !any(is.na(grp_med$TL)))

## capelin dependence, from the base-year diet matrix
d_cap      <- REco$DC["FCA", ]
pred_codes <- names(d_cap)

dep <- tibble::tibble(
  Biomass_Type = pred_codes,
  B     = as.numeric(REco$Biomass[pred_codes]),
  QB    = as.numeric(REco$QB[pred_codes]),
  d_cap = as.numeric(d_cap)) |>
  dplyr::mutate(Ig = QB * d_cap,
                Fg = B * QB * d_cap)

resp_dep <- grp_med |> dplyr::inner_join(dep, by = "Biomass_Type")

## explanatory power: trophic level vs dependence  (central finding, Fig. 5)
r2 <- resp_dep |>
  dplyr::group_by(scenario) |>
  dplyr::summarise(
    R2_TL    = summary(lm(Med ~ TL))$r.squared,
    R2_diet  = summary(lm(Med ~ d_cap))$r.squared,
    R2_Ig    = summary(lm(Med ~ Ig))$r.squared,
    .groups = "drop")
print(r2)

## dependence classes
resp_class <- resp_dep |>
  dplyr::mutate(class = factor(dplyr::case_when(
    d_cap >= 0.25 ~ "Major capelin predator",
    d_cap >= 0.05 ~ "Minor capelin predator",
    TRUE          ~ "Non-predator"),
    levels = c("Major capelin predator", "Minor capelin predator", "Non-predator")))

class_summary <- resp_class |>
  dplyr::group_by(scenario, class) |>
  dplyr::summarise(n = dplyr::n(),
                   Median = round(median(Med), 1),
                   Q25 = round(quantile(Med, 0.25), 1),
                   Q75 = round(quantile(Med, 0.75), 1),
                   .groups = "drop")
print(class_summary)

for (sc in unique(resp_class$scenario)) {
  kt <- kruskal.test(Med ~ class, data = dplyr::filter(resp_class, scenario == sc))
  cat(sc, ": chi2 =", round(kt$statistic, 2),
      " df =", kt$parameter, " p =", format.pval(kt$p.value, digits = 3), "\n")
}

## threshold sensitivity (response letter only, not the paper)
thr_grid <- expand.grid(hi = c(0.20, 0.25, 0.30), lo = c(0.03, 0.05, 0.10)) |>
  purrr::pmap_dfr(function(hi, lo) {
    out <- lapply(unique(resp_dep$scenario), function(sc) {
      rc <- resp_dep |>
        dplyr::filter(scenario == sc) |>
        dplyr::mutate(cl = dplyr::case_when(d_cap >= hi ~ "major",
                                            d_cap >= lo ~ "minor",
                                            TRUE        ~ "non"))
      tibble::tibble(scenario = sc, hi = hi, lo = lo,
                     p = kruskal.test(Med ~ factor(cl), data = rc)$p.value,
                     n = paste(table(rc$cl), collapse = "/"))
    })
    dplyr::bind_rows(out)
  })
print(thr_grid, n = 30)

## natural breaks in the diet-share distribution
dd <- sort(dep$d_cap[dep$d_cap > 0])
gaps <- data.frame(lower = head(dd, -1), upper = tail(dd, -1), gap = diff(dd)) |>
  dplyr::arrange(dplyr::desc(gap))
print(head(gaps, 5))


## ---- 14.4  Scenario relabelling ----------------------------------------------

rename_scen <- function(df) {
  df |> dplyr::mutate(scenario = dplyr::recode(scenario,
                                               "Capelin zero" = "Capelin near-zero"))
}

runs_long          <- rename_scen(runs_long)
runs_long_filtered <- rename_scen(runs_long_filtered)
runs_rel           <- rename_scen(runs_rel)
grp_med            <- rename_scen(grp_med)
resp_class         <- rename_scen(resp_class)
resp_dep           <- rename_scen(resp_dep)

unique(resp_class$scenario)   # confirm before saving


## ---- 14.5  Figure S5.1: % change as a continuous function of trophic level ---

fig_tl <- ggplot(grp_med, aes(TL, Med)) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey40") +
  geom_vline(xintercept = c(2, 3, 4), linetype = "dashed", colour = "grey85") +
  geom_linerange(aes(ymin = Q025, ymax = Q975, colour = scenario), alpha = 0.3) +
  geom_point(aes(colour = scenario, size = B0)) +
  ggrepel::geom_text_repel(aes(label = GroupName), size = 2.4,
                           max.overlaps = 12, seed = 42, colour = "grey25") +
  scale_colour_manual(values = pal, guide = "none") +
  scale_size_continuous(range = c(1.2, 6), guide = "none") +
  facet_wrap(~ scenario, ncol = 2) +
  labs(x = "Trophic level (balanced base model)",
       y = "% change from Status Quo (2086-2095)") +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave(file.path(FIGS, "FigS5_1_TL_continuous.png"), fig_tl,
       width = 10, height = 4.8, dpi = 400)


## ---- 14.6  Figure S5.2: response by dependence class -------------------------

fig_class <- ggplot(resp_class, aes(class, Med)) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey40") +
  geom_boxplot(outlier.shape = NA, width = 0.5, colour = "grey45", fill = NA) +
  geom_jitter(aes(colour = scenario), width = 0.13, size = 2, alpha = 0.85) +
  scale_colour_manual(values = pal, guide = "none") +
  facet_wrap(~ scenario, ncol = 2) +
  labs(x = NULL, y = "% change from Status Quo (2086-2095)") +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 18, hjust = 1))

ggsave(file.path(FIGS, "FigS5_2_dependence_class.png"), fig_class,
       width = 9, height = 4.5, dpi = 400)


## ---- 14.7  Save S5 outputs ---------------------------------------------------

save(table_S5_1, bin_exposure, table_S5_2, moved, grp_med, resp_class,
     class_summary, r2, thr_grid, gaps,
     file = file.path(DIR, "S5_outputs.RData"))

################################################################################
## END
################################################################################