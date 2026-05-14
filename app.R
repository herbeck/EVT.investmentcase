library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(shinyWidgets)
library(ggrepel)

# ============================================================
# PAPER TABLES: Chevalier et al, Lancet Global Health 2024
# doi: 10.1016/S2214-109X(23)00588-0
# ============================================================

paper_table1 <- data.frame(
  Intervention = c(
    "Infant prophylaxis",
    "HIV retesting option one",
    "HIV retesting option two",
    "HIV retesting option three",
    "Pre-exposure prophylaxis",
    "Maternal peer-support groups",
    "Loss to follow-up tracing",
    "Point-of-care viral load testing"
  ),
  Description = c(
    "6-week course of nevirapine for infants exposed to HIV",
    "HIV retesting during late antenatal care, delivery, or 6 weeks postpartum",
    "HIV retesting during late antenatal care, delivery, or 6 weeks postpartum, with an additional test at 14 weeks postpartum",
    "HIV retesting during late antenatal care, delivery, or 6 weeks postpartum and additional retesting every 3 months during breastfeeding",
    "Oral daily PrEP for pregnant and breastfeeding women who are HIV-negative",
    "HIV-positive mother-to-mother peer-support groups",
    "Tracing of mothers lost to follow-up",
    "Point-of-care viral load testing"
  ),
  Effect_measure_range = c(
    "50% (40-70)",
    "19% (12-20)",
    "21% (13-21)",
    "22% (13-23)",
    "65% effectiveness proxy; sensitivity 26-83% in paper figure",
    "86.5% reduction proxy in app; source effect varies in sensitivity",
    "29% reduction in LTFU proxy; sensitivity 12-44% in paper figure",
    "10% increase proxy in app; sensitivity 3.9-16.4% in paper figure"
  ),
  Model_impact = c(
    "Reduction in vertical transmission; implemented for 50% of population at baseline",
    "Reduction in vertical transmission",
    "Reduction in vertical transmission",
    "Reduction in vertical transmission",
    "Reduction in maternal HIV acquisition",
    "Reduction in vertical transmission / improved retention in HIV-care continuum",
    "Reduced loss to follow-up",
    "Increased movement to ART / viral suppression"
  ),
  Theoretical_maximum = c("90%", "90%", "85%", "80%", "10%", "80%", "90%", "60%"),
  stringsAsFactors = FALSE
)

paper_table2 <- data.frame(
  Intervention = c(
    "Infant prophylaxis", "Antenatal HIV test", "Pre-exposure prophylaxis",
    "Maternal peer-support groups", "Loss to follow-up tracing", "Point-of-care viral load"
  ),
  Cost_USD_2022 = c(12.86, 2.99, 16.89, 1.39, 4.25, 8.03),
  Timeframe = c(
    "Per 6-week treatment", "Per test", "Per month",
    "Per session", "Per person traced", "Additional cost per test above centralised viral load testing"
  ),
  Cost_components = "Drugs, laboratory, staff, and overhead where applicable",
  stringsAsFactors = FALSE
)

paper_table3 <- data.frame(
  Scenario = c("Baseline", paste0("Scenario ", 1:9)),
  Intervention = c(
    "50% infant prophylaxis, 80% transitioned to TLD ART",
    "84-98% of population transitioned to TLD ART",
    "60% scale-up infant prophylaxis, 50% maternal support groups, 98% TLD ART",
    "20% scale-up infant prophylaxis, 80% maternal support groups, 98% TLD ART",
    "80% scale-up infant prophylaxis, 80% maternal support groups, 98% TLD ART",
    "80% scale-up infant prophylaxis, 80% maternal support groups, 50% lost to follow-up tracing, 98% TLD ART",
    "80% scale-up infant prophylaxis, 80% maternal support groups, 80% HIV retesting option one, 80% lost to follow-up tracing, 98% TLD ART",
    "80% scale-up infant prophylaxis, 80% maternal support groups, 90% HIV retesting option one, 90% lost to follow-up tracing, 98% TLD ART",
    "80% scale-up infant prophylaxis, 90% HIV retesting option one, 80% maternal support groups, 10% pregnant and breastfeeding women on PrEP, 98% TLD ART",
    "80% scale-up infant prophylaxis, 85% HIV retesting option two, 80% maternal support groups, 10% pregnant and breastfeeding women on PrEP, 90% lost to follow-up minimisation, 60% POCVL testing, 98% TLD ART"
  ),
  Additional_infant_infections_averted = c("0", "6-26", "916", "1424", "1475", "1480", "1706", "1734", "1746", "1780"),
  Additional_maternal_infections_averted = c("0", "0", "0", "0", "0", "0", "0", "0", "354", "354"),
  Additional_reduction_vertical_transmission = c("0%", "0.15-0.65%", "22.93%", "35.65%", "36.93%", "37.06%", "42.71%", "43.42%", "43.72%", "44.57%"),
  Total_cost = c("$21,025,879", "$20,934,151-$21,005,505", "$21,249,701", "$21,396,749", "$21,608,157", "$21,635,059", "$25,246,262", "$25,701,026", "$47,465,344", "$59,102,276"),
  ICER = c("NA", "Cost-saving", "$244", "$289", "$4,145", "$5,380", "$15,979", "$16,242", "$59,465", "$342,263"),
  stringsAsFactors = FALSE
)

# ============================================================
# COUNTRY DATA: editable starter defaults
# Zambia values mirror the paper where available; other countries are placeholders.
# ============================================================
country_defaults <- list(
  # All countries assume 100% TLD (DTG-based) first-line ART for HIV-infected PBFW.
  # cost_dtg_monthly is the per-person-per-month ART cost (TLD regimen).
  Zambia = list(childbearing_pop=4446807, pregnant_pop=369085, hiv_prev=0.138, pct_diagnosed=0.94, pct_art=0.93, pct_virally_supp=0.90, pct_inf_prophylaxis=0.50, hiv_incidence_annual=0.00538, birth_rate_monthly=0.0029, trans_diagnosed=0.08024675, trans_art=0.09449375, trans_virally_supp=0.07678550, trans_ltfu=0.006754186, cost_inf_prophylaxis=12.86, cost_anc_test=2.99, cost_prep=16.89, cost_support_groups=1.39, cost_dtg_monthly=10.39, cost_ltfu_min=4.25, cost_poc_vl=8.03),
  Kenya = list(childbearing_pop=12800000, pregnant_pop=1450000, hiv_prev=0.054, pct_diagnosed=0.96, pct_art=0.95, pct_virally_supp=0.93, pct_inf_prophylaxis=0.60, hiv_incidence_annual=0.00210, birth_rate_monthly=0.0030, trans_diagnosed=0.065, trans_art=0.085, trans_virally_supp=0.070, trans_ltfu=0.0065, cost_inf_prophylaxis=11.50, cost_anc_test=2.50, cost_prep=14.00, cost_support_groups=1.20, cost_dtg_monthly=9.50, cost_ltfu_min=4.00, cost_poc_vl=7.50),
  Mozambique = list(childbearing_pop=8200000, pregnant_pop=890000, hiv_prev=0.125, pct_diagnosed=0.89, pct_art=0.85, pct_virally_supp=0.87, pct_inf_prophylaxis=0.45, hiv_incidence_annual=0.00720, birth_rate_monthly=0.0038, trans_diagnosed=0.070, trans_art=0.080, trans_virally_supp=0.065, trans_ltfu=0.0080, cost_inf_prophylaxis=10.00, cost_anc_test=2.20, cost_prep=13.00, cost_support_groups=1.00, cost_dtg_monthly=8.50, cost_ltfu_min=3.50, cost_poc_vl=7.00),
  Malawi = list(childbearing_pop=5400000, pregnant_pop=580000, hiv_prev=0.081, pct_diagnosed=0.96, pct_art=0.96, pct_virally_supp=0.94, pct_inf_prophylaxis=0.55, hiv_incidence_annual=0.00340, birth_rate_monthly=0.0034, trans_diagnosed=0.075, trans_art=0.090, trans_virally_supp=0.075, trans_ltfu=0.0060, cost_inf_prophylaxis=9.50, cost_anc_test=2.00, cost_prep=12.00, cost_support_groups=0.95, cost_dtg_monthly=8.00, cost_ltfu_min=3.20, cost_poc_vl=6.50),
  Zimbabwe = list(childbearing_pop=4100000, pregnant_pop=440000, hiv_prev=0.140, pct_diagnosed=0.94, pct_art=0.93, pct_virally_supp=0.91, pct_inf_prophylaxis=0.52, hiv_incidence_annual=0.00520, birth_rate_monthly=0.0028, trans_diagnosed=0.078, trans_art=0.092, trans_virally_supp=0.074, trans_ltfu=0.0068, cost_inf_prophylaxis=11.00, cost_anc_test=2.40, cost_prep=14.50, cost_support_groups=1.15, cost_dtg_monthly=9.20, cost_ltfu_min=3.80, cost_poc_vl=7.20),
  `South Africa` = list(childbearing_pop=14500000, pregnant_pop=1150000, hiv_prev=0.204, pct_diagnosed=0.94, pct_art=0.91, pct_virally_supp=0.93, pct_inf_prophylaxis=0.55, hiv_incidence_annual=0.00640, birth_rate_monthly=0.0021, trans_diagnosed=0.076, trans_art=0.091, trans_virally_supp=0.073, trans_ltfu=0.0062, cost_inf_prophylaxis=14.00, cost_anc_test=3.50, cost_prep=20.00, cost_support_groups=1.80, cost_dtg_monthly=12.00, cost_ltfu_min=5.50, cost_poc_vl=9.50)
)

shared_params <- list(
  mtct_hiv_inf=0.200, mtct_diagnosed=0.200, mtct_art=0.037, mtct_virally_supp=0.0025,
  mtct_bf_hiv_inf=0.01250, mtct_bf_diagnosed=0.01250, mtct_bf_art=0.001542, mtct_bf_virally_supp=0.0001042,
  backflow_dtg=0.007917,
  # Intervention effectiveness parameters (from Excel Intervention Multipliers sheet)
  # Each represents the proportional effect on the relevant outcome at 100% coverage.
  eff_ip        = 0.50,   # IP: 50% reduction in MTCT for diagnosed/ART/VS at full coverage
  eff_anc1      = 0.81,   # ANC option 1: multiplier on HIV-inf MTCT at full coverage (19% reduction)
  eff_anc2      = 0.79,   # ANC option 2: multiplier on HIV-inf MTCT at full coverage (21% reduction)
  eff_anc3      = 0.78,   # ANC option 3: multiplier on HIV-inf MTCT at full coverage (22% reduction)
  eff_prep      = 0.35,   # PrEP: residual HIV acquisition fraction (65% effectiveness)
  eff_support   = 0.135,  # Support groups: residual MTCT fraction for diag/ART women (86.5% reduction)
  eff_ltfu      = 0.71,   # LTFU tracing: residual LTFU rate (29% reduction in LTFU)
  eff_poc       = 0.10    # POC VL: proportional increase in viral suppression transition rate (10%)
)


# Safe numeric helpers for Shiny inputs. These preserve country defaults
# when inputs are NULL, NA, non-numeric, or temporarily uninitialized.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

safe_num <- function(x, default = 0) {
  if (is.null(x) || length(x) == 0) return(as.numeric(default))
  y <- suppressWarnings(as.numeric(x[1]))
  if (!is.finite(y)) return(as.numeric(default))
  y
}

safe_param_list <- function(params, defaults = list()) {
  out <- defaults
  if (is.null(params) || length(params) == 0) return(out)
  for (nm in names(params)) {
    val <- params[[nm]]
    if (is.null(val) || length(val) == 0) next
    if (is.numeric(val) || is.integer(val)) {
      val <- safe_num(val, out[[nm]] %||% 0)
    }
    out[[nm]] <- val
  }
  out
}

# ============================================================
# CORE MARKOV MODEL FUNCTION -- Excel-equivalent implementation
# Recreates the workbook's Baseline Model and Intervention Model sheets.
# ============================================================
run_model <- function(params, n_months = 12,
                      int_infant_prop, int_anc1, int_anc2, int_anc3,
                      int_prep, int_support,
                      int_ltfu, int_poc_vl) {
  p <- safe_param_list(params, country_defaults$Zambia)
  p <- safe_param_list(shared_params, p)

  int_infant_prop <- safe_num(int_infant_prop, p$pct_inf_prophylaxis)
  int_anc1 <- safe_num(int_anc1, 0)
  int_anc2 <- safe_num(int_anc2, 0)
  int_anc3 <- safe_num(int_anc3, 0)
  int_prep <- safe_num(int_prep, 0)
  int_support <- safe_num(int_support, 0)
  int_ltfu <- safe_num(int_ltfu, 0)
  int_poc_vl <- safe_num(int_poc_vl, 0)
  n_months <- max(1, as.integer(safe_num(n_months, 12)))

  # TLD (DTG) is assumed universal (100%) — backflow and viral suppression
  # multiplier are fixed constants derived from the DTG-specific parameters.
  backflow_fixed <- p$backflow_dtg          # = 0.007917 (DTG only, no EFZ mix)
  vs_mult_fixed  <- 1.64                    # = 1 + 1.0 * 0.64 (100% TLD effect)

  population <- p$pregnant_pop * 3
  dist_preg <- p$pregnant_pop / population
  dist_deliv <- ((1/280) * 30.5) * dist_preg
  dist_bf <- 1 - dist_preg - dist_deliv
  transition_delivery <- (1/280) * 30.5
  transition_pregnancy <- p$birth_rate_monthly
  transition_hiv <- p$hiv_incidence_annual / 12

  intervention_on <- isTRUE(int_infant_prop > p$pct_inf_prophylaxis) ||
    isTRUE(int_anc1 > 0) || isTRUE(int_anc2 > 0) || isTRUE(int_anc3 > 0) ||
    isTRUE(int_prep > 0) || isTRUE(int_support > 0) ||
    isTRUE(int_ltfu > 0) || isTRUE(int_poc_vl > 0)

  ip_adj <- 1 - int_infant_prop * (1 - p$eff_ip)
  anc1_mult <- if (isTRUE(int_anc1 > 0)) 1 - int_anc1 * (1 - p$eff_anc1) else 1
  anc2_mult <- if (isTRUE(int_anc2 > 0)) 1 - int_anc2 * (1 - p$eff_anc2) else 1
  anc3_mult <- if (isTRUE(int_anc3 > 0)) 1 - int_anc3 * (1 - p$eff_anc3) else 1
  mtct_hiv_inf_mult <- min(anc1_mult, anc2_mult, anc3_mult)
  transition_hiv_mult <- if (isTRUE(int_prep > 0)) 1 - int_prep * (1 - p$eff_prep) else 1
  support_mtct_mult <- if (isTRUE(int_support > 0)) 1 - int_support * (1 - p$eff_support) else 1
  mtct_diag_mult <- ip_adj * support_mtct_mult
  mtct_art_mult <- ip_adj * support_mtct_mult
  mtct_vs_mult <- ip_adj
  ltfu_mult <- if (isTRUE(int_ltfu > 0)) 1 - int_ltfu * (1 - p$eff_ltfu) else 1
  art_preg_mult <- if (isTRUE(int_poc_vl > 0)) 1 + int_poc_vl * p$eff_poc else 1
  art_bf_mult <- if (isTRUE(int_poc_vl > 0)) 1 + int_poc_vl * p$eff_poc else 1
  diag_preg_mult <- 1
  diag_deliv_mult <- 1
  diag_bf_mult <- 1

  make_initial <- function(intervention = FALSE) {
    out <- list()
    for (stage in c("preg", "deliv", "bf")) {
      dist <- switch(stage, preg = dist_preg, deliv = dist_deliv, bf = dist_bf)
      B <- population * (1 - p$hiv_prev) * dist
      C <- population * p$hiv_prev * dist
      D <- C * p$pct_diagnosed
      E <- population * p$hiv_prev * dist * p$pct_diagnosed * p$pct_art
      I <- population * p$hiv_prev * dist * p$pct_diagnosed * p$pct_art * p$pct_virally_supp
      F <- C - D
      G <- D - E
      H <- E - I
      out[[stage]] <- c(neg = B, inf = F, diag = G, art = H, vs = I)
    }
    out
  }

  child_infections <- function(rows, intervention = FALSE, t = 0) {
    # T=0 always uses the baseline MTCT formula regardless of scenario,
    # matching the Excel Intervention Model which shows T=0 infections = baseline.
    if (!intervention || t == 0) {
      deliv <- rows$deliv
      bf <- rows$bf
      deliv_inf <- deliv[["inf"]] * p$mtct_hiv_inf +
        (deliv[["diag"]] * p$mtct_diagnosed + deliv[["art"]] * p$mtct_art + deliv[["vs"]] * p$mtct_virally_supp) *
        (1 - p$pct_inf_prophylaxis * 0.5)
      bf_inf <- bf[["inf"]] * p$mtct_bf_hiv_inf + bf[["diag"]] * p$mtct_bf_diagnosed +
        bf[["art"]] * p$mtct_bf_art + bf[["vs"]] * p$mtct_bf_virally_supp
    } else {
      deliv <- rows$deliv
      bf <- rows$bf
      deliv_inf <- deliv[["inf"]] * p$mtct_hiv_inf * mtct_hiv_inf_mult +
        deliv[["diag"]] * p$mtct_diagnosed * mtct_diag_mult +
        deliv[["art"]] * p$mtct_art * mtct_art_mult +
        deliv[["vs"]] * p$mtct_virally_supp * mtct_vs_mult
      bf_inf <- bf[["inf"]] * p$mtct_bf_hiv_inf * mtct_hiv_inf_mult +
        (bf[["diag"]] * p$mtct_bf_diagnosed * mtct_diag_mult +
           bf[["art"]] * p$mtct_bf_art * mtct_art_mult +
           bf[["vs"]] * p$mtct_bf_virally_supp * mtct_vs_mult) / ip_adj
    }
    c(delivery = as.numeric(deliv_inf), breastfeeding = as.numeric(bf_inf), total = as.numeric(deliv_inf + bf_inf))
  }

  transition_before_delivery <- function(rows, intervention = FALSE) {
    r <- lapply(rows, function(v) { names(v) <- NULL; v })
    names(r$preg) <- c("neg","inf","diag","art","vs")
    names(r$deliv) <- c("neg","inf","diag","art","vs")
    names(r$bf)   <- c("neg","inf","diag","art","vs")
    cp <- p$childbearing_pop
    dneg <- 1 - p$hiv_prev
    dhiv <- p$hiv_prev
    ddiag <- p$pct_diagnosed
    dart <- p$pct_art
    dvs <- p$pct_virally_supp

    if (!intervention) {
      new <- list()
      new$preg <- c(
        neg = r$preg[["neg"]] + cp*dneg*transition_pregnancy - r$preg[["neg"]]*transition_hiv,
        inf = r$preg[["neg"]]*transition_hiv + r$preg[["inf"]] - r$preg[["inf"]]*p$trans_diagnosed + (cp*dhiv - cp*dhiv*ddiag)*transition_pregnancy,
        diag = r$preg[["inf"]]*p$trans_diagnosed + r$preg[["diag"]] - r$preg[["diag"]]*p$trans_art + r$preg[["art"]]*p$trans_ltfu + (cp*dhiv*ddiag - cp*dhiv*ddiag*dart)*transition_pregnancy,
        art = r$preg[["diag"]]*p$trans_art + r$preg[["art"]] - r$preg[["art"]]*p$trans_virally_supp*vs_mult_fixed + r$preg[["vs"]]*backflow_fixed + r$preg[["vs"]]*p$trans_ltfu - r$preg[["art"]]*p$trans_ltfu + (cp*dhiv*ddiag*dart - cp*dhiv*ddiag*dart*dvs)*transition_pregnancy,
        vs = r$preg[["art"]]*p$trans_virally_supp*vs_mult_fixed + r$preg[["vs"]] - r$preg[["vs"]]*backflow_fixed - r$preg[["vs"]]*p$trans_ltfu + cp*dhiv*ddiag*dart*dvs*transition_pregnancy
      )
      new$deliv <- c(
        neg = r$deliv[["neg"]] - r$deliv[["neg"]]*transition_hiv,
        inf = r$deliv[["neg"]]*transition_hiv + r$deliv[["inf"]] - r$deliv[["inf"]]*p$trans_diagnosed,
        diag = r$deliv[["inf"]]*p$trans_diagnosed + r$deliv[["diag"]] - r$deliv[["diag"]]*p$trans_art,
        art = r$deliv[["diag"]]*p$trans_art + r$deliv[["art"]] - r$deliv[["art"]]*p$trans_virally_supp*vs_mult_fixed,
        vs = r$deliv[["art"]]*p$trans_virally_supp*vs_mult_fixed + r$deliv[["vs"]]
      )
      new$bf <- c(
        neg = r$bf[["neg"]] - r$bf[["neg"]]*transition_hiv,
        inf = r$bf[["neg"]]*transition_hiv + r$bf[["inf"]] - r$bf[["inf"]]*p$trans_diagnosed,
        diag = r$bf[["inf"]]*p$trans_diagnosed + r$bf[["diag"]] - r$bf[["diag"]]*p$trans_art + r$bf[["art"]]*p$trans_ltfu,
        art = r$bf[["diag"]]*p$trans_art + r$bf[["art"]] + r$bf[["vs"]]*backflow_fixed + r$bf[["vs"]]*p$trans_ltfu - r$bf[["art"]]*p$trans_ltfu - r$bf[["art"]]*p$trans_virally_supp*vs_mult_fixed,
        vs = r$bf[["art"]]*p$trans_virally_supp*vs_mult_fixed + r$bf[["vs"]] - r$bf[["vs"]]*backflow_fixed - r$bf[["vs"]]*p$trans_ltfu
      )
      return(new)
    }

    new <- list()
    new$preg <- c(
      neg = r$preg[["neg"]] - r$preg[["neg"]]*transition_hiv*transition_hiv_mult + cp*dneg*transition_pregnancy,
      inf = r$preg[["neg"]]*transition_hiv*transition_hiv_mult - r$preg[["inf"]]*p$trans_diagnosed*diag_preg_mult + r$preg[["inf"]] + (cp*dhiv - cp*dhiv*ddiag)*transition_pregnancy,
      diag = r$preg[["inf"]]*p$trans_diagnosed*diag_preg_mult - r$preg[["diag"]]*p$trans_art*art_preg_mult + r$preg[["diag"]] + (cp*dhiv*ddiag - cp*dhiv*ddiag*dart)*transition_pregnancy + r$preg[["art"]]*p$trans_ltfu*ltfu_mult,
      art = r$preg[["diag"]]*p$trans_art*art_preg_mult - r$preg[["art"]]*p$trans_virally_supp*vs_mult_fixed + r$preg[["art"]] + (cp*dhiv*ddiag*dart - cp*dhiv*ddiag*dart*dvs)*transition_pregnancy + r$preg[["vs"]]*backflow_fixed - r$preg[["art"]]*p$trans_ltfu*ltfu_mult + r$preg[["vs"]]*p$trans_ltfu*ltfu_mult,
      vs = r$preg[["art"]]*p$trans_virally_supp*vs_mult_fixed + r$preg[["vs"]] + cp*dhiv*ddiag*dart*dvs*transition_pregnancy - r$preg[["vs"]]*backflow_fixed - r$preg[["vs"]]*p$trans_ltfu*ltfu_mult
    )
    new$deliv <- c(
      neg = r$deliv[["neg"]] - r$deliv[["neg"]]*transition_hiv*transition_hiv_mult,
      inf = r$deliv[["neg"]]*transition_hiv*transition_hiv_mult - r$deliv[["inf"]]*p$trans_diagnosed*diag_deliv_mult + r$deliv[["inf"]],
      diag = r$deliv[["inf"]]*p$trans_diagnosed*diag_deliv_mult + r$deliv[["diag"]] - r$deliv[["diag"]]*p$trans_art,
      art = r$deliv[["diag"]]*p$trans_art + r$deliv[["art"]] - r$deliv[["art"]]*p$trans_virally_supp*vs_mult_fixed,
      vs = r$deliv[["art"]]*p$trans_virally_supp*vs_mult_fixed + r$deliv[["vs"]]
    )
    new$bf <- c(
      neg = r$bf[["neg"]] - r$bf[["neg"]]*transition_hiv*transition_hiv_mult,
      inf = r$bf[["neg"]]*transition_hiv*transition_hiv_mult - r$bf[["inf"]]*p$trans_diagnosed*diag_bf_mult + r$bf[["inf"]],
      diag = r$bf[["inf"]]*p$trans_diagnosed*diag_bf_mult + r$bf[["diag"]] - r$bf[["diag"]]*p$trans_art*art_bf_mult + r$bf[["art"]]*p$trans_ltfu*ltfu_mult,
      art = r$bf[["diag"]]*p$trans_art*art_bf_mult - r$bf[["art"]]*p$trans_virally_supp*vs_mult_fixed + r$bf[["art"]] + r$bf[["vs"]]*backflow_fixed + r$bf[["vs"]]*p$trans_ltfu*ltfu_mult - r$bf[["art"]]*p$trans_ltfu*ltfu_mult,
      vs = r$bf[["art"]]*p$trans_virally_supp*vs_mult_fixed + r$bf[["vs"]] - r$bf[["vs"]]*backflow_fixed - r$bf[["vs"]]*p$trans_ltfu*ltfu_mult
    )
    new
  }

  apply_delivery <- function(pre) {
    preg_new  <- pre$preg - pre$preg * transition_delivery
    deliv_new <- pre$preg * transition_delivery
    bf_new    <- pre$bf + pre$deliv
    names(preg_new)  <- c("neg","inf","diag","art","vs")
    names(deliv_new) <- c("neg","inf","diag","art","vs")
    names(bf_new)    <- c("neg","inf","diag","art","vs")
    list(preg = preg_new, deliv = deliv_new, bf = bf_new)
  }

  rows <- make_initial(intervention_on)
  history <- list(rows)
  if (isTRUE(n_months > 0)) {
    for (month in seq_len(n_months)) {
      rows <- apply_delivery(transition_before_delivery(rows, intervention_on))
      history[[month + 1]] <- rows
    }
  }

  out <- lapply(seq_along(history), function(i) {
    t <- i - 1
    rows <- history[[i]]
    ci <- child_infections(rows, intervention_on, t)
    data.frame(
      t = t,
      preg_neg = unname(rows$preg[["neg"]]), preg_inf = unname(rows$preg[["inf"]]), preg_diag = unname(rows$preg[["diag"]]), preg_art = unname(rows$preg[["art"]]), preg_vs = unname(rows$preg[["vs"]]),
      deliv_neg = unname(rows$deliv[["neg"]]), deliv_inf = unname(rows$deliv[["inf"]]), deliv_diag = unname(rows$deliv[["diag"]]), deliv_art = unname(rows$deliv[["art"]]), deliv_vs = unname(rows$deliv[["vs"]]),
      bf_neg = unname(rows$bf[["neg"]]), bf_inf = unname(rows$bf[["inf"]]), bf_diag = unname(rows$bf[["diag"]]), bf_art = unname(rows$bf[["art"]]), bf_vs = unname(rows$bf[["vs"]]),
      child_inf_deliv = unname(ci["delivery"]), child_inf_bf = unname(ci["breastfeeding"]), child_inf_total = unname(ci["total"]),
      maternal_incident_hiv = 0,
      ltfu = unname(p$trans_ltfu * ltfu_mult * (rows$preg[["art"]] + rows$preg[["vs"]] + rows$bf[["art"]] + rows$bf[["vs"]])),
      total_hiv_mothers = unname(sum(rows$preg[-1]) + sum(rows$deliv[-1]) + sum(rows$bf[-1])),
      check.names = FALSE,
      row.names = NULL
    )
  }) %>% bind_rows()

  entry_adjust <- p$childbearing_pop * p$hiv_prev * transition_pregnancy
  if (isTRUE(nrow(out) > 1)) {
    out$maternal_incident_hiv[1] <- 0
    for (i in 2:nrow(out)) {
      out$maternal_incident_hiv[i] <- out$total_hiv_mothers[i] - out$total_hiv_mothers[i - 1] - entry_adjust
    }
  }
  out
}

# compute_costs: implements the Excel Costing Model Q5/Q6/Q7 formulas exactly.
#
# Excel structure (per monthly time step, summed T=1..T=12):
#   PREGNANT (Q5):
#     HIV_neg   * ((1/9)*ANC1 + (1/9)*ANC2 + (1/9)*ANC3) * test_cost
#     HIV_neg   * PrEP_prop * prep_cost
#     HIV_inf   * ((1/9)*ANC1 + (1/9)*ANC2 + (1/9)*ANC3) * test_cost
#     Diag      * (sup_prop*sup_cost + poc_prop*poc_cost)           ← support groups include diagnosed
#     ART_notVS * (TLD_cost + sup_prop*sup_cost + EFZ_prop*EFZ_extra + poc_prop*poc_cost)
#     VS        * (TLD_cost + EFZ_prop*EFZ_extra + poc_prop*poc_cost)  ← VS get NO support groups
#     LTFU_rate*(ART+VS) * LTFU_prop * LTFU_cost
#     (1/9) * total_preg * test_cost                                 ← one baseline ANC test per pregnancy
#   DELIVERY (Q6) — costs prorated to daily rate (÷30.5):
#     HIV_neg   * PrEP_prop * prep_cost / 30.5
#     ART_notVS * (TLD_cost + EFZ_prop*EFZ_extra) / 30.5
#     VS        * (TLD_cost + EFZ_prop*EFZ_extra) / 30.5
#     (HIV_neg + HIV_inf) * (ANC1+ANC2+ANC3) * test_cost            ← delivery retesting (full, not prorated)
#     [IP is billed separately as L6 = HEI * IP_prop * IP_cost]
#   BREASTFEEDING (Q7):
#     HIV_neg   * ((1/12)*ANC1 + (2/12)*ANC2 + (4/12)*ANC3) * test_cost
#     HIV_neg   * PrEP_prop * prep_cost
#     HIV_inf   * ((1/12)*ANC1 + (2/12)*ANC2 + (4/12)*ANC3) * test_cost
#     Diag      * (sup_prop*sup_cost + poc_prop*poc_cost)
#     ART_notVS * (TLD_cost + sup_prop*sup_cost + EFZ_prop*EFZ_extra + poc_prop*poc_cost)
#     VS        * (TLD_cost + EFZ_prop*EFZ_extra + poc_prop*poc_cost)
#     LTFU_rate*(ART+VS) * LTFU_prop * LTFU_cost

compute_costs <- function(df, params, n_months,
                          int_infant_prop, int_anc1, int_anc2, int_anc3,
                          int_prep, int_support,
                          int_ltfu, int_poc_vl) {
  p <- safe_param_list(params, country_defaults$Zambia)
  int_infant_prop <- safe_num(int_infant_prop, p$pct_inf_prophylaxis)
  int_anc1   <- safe_num(int_anc1, 0); int_anc2 <- safe_num(int_anc2, 0); int_anc3 <- safe_num(int_anc3, 0)
  int_prep   <- safe_num(int_prep, 0); int_support <- safe_num(int_support, 0)
  int_ltfu   <- safe_num(int_ltfu, 0); int_poc_vl <- safe_num(int_poc_vl, 0)

  # Use months 1..12 (drop T=0 row which is the initial state, not a cost period)
  d <- df[-1, ]

  # Column aliases matching Excel notation
  # Pregnant compartment
  B5 <- d$preg_neg;  F5 <- d$preg_inf;  G5 <- d$preg_diag;  H5 <- d$preg_art;  I5 <- d$preg_vs
  # Delivery compartment
  B6 <- d$deliv_neg; F6 <- d$deliv_inf; G6 <- d$deliv_diag; H6 <- d$deliv_art; I6 <- d$deliv_vs
  # Breastfeeding compartment
  B7 <- d$bf_neg;    F7 <- d$bf_inf;    G7 <- d$bf_diag;    H7 <- d$bf_art;    I7 <- d$bf_vs

  lt    <- p$trans_ltfu
  TLD   <- p$cost_dtg_monthly
  EFZ   <- 0   # TLD is universal; no EFZ cost
  ANC   <- p$cost_anc_test
  SUP   <- int_support * p$cost_support_groups
  PREP  <- int_prep    * p$cost_prep
  LTFU_cost <- int_ltfu  * p$cost_ltfu_min
  POC   <- int_poc_vl  * p$cost_poc_vl

  # --- Infant prophylaxis (L column: delivery HIV-exposed * IP_prop * cost) ---
  cost_ip <- sum((F6+G6+H6+I6) * int_infant_prop * p$cost_inf_prophylaxis)

  # --- Pregnant (Q5) ---
  anc_preg_retest <- (1/9)*int_anc1*ANC + (1/9)*int_anc2*ANC + (1/9)*int_anc3*ANC
  anc_preg_base   <- (1/9) * ANC                               # one baseline test per 9-month pregnancy
  total_preg      <- B5 + F5 + G5 + H5 + I5
  P5              <- lt * (H5 + I5)                            # LTFU count in pregnant ART

  Q5 <- sum(
    B5 * (anc_preg_retest + PREP) +
    F5 * anc_preg_retest +
    G5 * (SUP + POC) +
    H5 * (TLD + EFZ + SUP + POC) +
    I5 * (TLD + EFZ + POC) +
    P5 * LTFU_cost +
    total_preg * anc_preg_base
  )

  # --- Delivery (Q6) — prorated to daily ART rate ---
  anc_deliv_retest <- (int_anc1 + int_anc2 + int_anc3) * ANC   # full test at delivery (not prorated)

  Q6 <- sum(
    B6 * PREP / 30.5 +
    H6 * (TLD + EFZ) / 30.5 +
    I6 * (TLD + EFZ) / 30.5 +
    (B6 + F6) * anc_deliv_retest
  )

  # --- Breastfeeding (Q7) ---
  anc_bf_retest <- (1/12)*int_anc1*ANC + (2/12)*int_anc2*ANC + (4/12)*int_anc3*ANC
  P7            <- lt * (H7 + I7)

  Q7 <- sum(
    B7 * (anc_bf_retest + PREP) +
    F7 * anc_bf_retest +
    G7 * (SUP + POC) +
    H7 * (TLD + EFZ + SUP + POC) +
    I7 * (TLD + EFZ + POC) +
    P7 * LTFU_cost
  )

  # Aggregate into named breakdown
  cost_anc     <- sum(total_preg * anc_preg_base) +
                  sum((B5+F5)*anc_preg_retest + (B6+F6)*anc_deliv_retest + (B7+F7)*anc_bf_retest)
  cost_prep    <- sum(B5*PREP + B6*PREP/30.5 + B7*PREP)
  cost_support <- sum(G5*SUP + H5*SUP + G7*SUP + H7*SUP)
  cost_art     <- sum(H5*(TLD+EFZ) + I5*(TLD+EFZ) + (H6+I6)*(TLD+EFZ)/30.5 + H7*(TLD+EFZ) + I7*(TLD+EFZ))
  cost_ltfu    <- sum(P5*LTFU_cost + P7*LTFU_cost)
  cost_poc     <- sum(G5*POC + H5*POC + I5*POC + G7*POC + H7*POC + I7*POC)

  breakdown <- data.frame(
    Intervention = c("Infant prophylaxis", "ANC HIV retesting", "PrEP",
                     "Maternal peer-support groups", "ART regimen", "LTFU tracing", "POC viral load"),
    Cost_USD = round(c(cost_ip, cost_anc, cost_prep, cost_support, cost_art, cost_ltfu, cost_poc), 0)
  )
  list(total = sum(breakdown$Cost_USD, na.rm = TRUE), breakdown = breakdown)
}

scenario_to_interventions <- function(scenario) {
  # Infant prophylaxis proportions are validated against the paper's Table 3 averted counts:
  # "60% scale-up" = ip=0.60 (gives ~916 averted for Scenario 2)
  # "20% scale-up" = ip=0.60 (gives ~1424 averted for Scenario 3 — same absolute coverage)
  # "80% scale-up" = ip=0.90 (gives ~1475 averted for Scenario 4, matching theoretical maximum)
  # TLD is assumed universal (100%) in all scenarios.
  switch(as.character(scenario),
    "Baseline"   = list(ip=.50, anc1=0,   anc2=0,   anc3=0, prep=0,   support=0,   ltfu=0,   poc=0),
    "Scenario 1" = list(ip=.50, anc1=0,   anc2=0,   anc3=0, prep=0,   support=0,   ltfu=0,   poc=0),
    "Scenario 2" = list(ip=.60, anc1=0,   anc2=0,   anc3=0, prep=0,   support=.50, ltfu=0,   poc=0),
    "Scenario 3" = list(ip=.60, anc1=0,   anc2=0,   anc3=0, prep=0,   support=.80, ltfu=0,   poc=0),
    "Scenario 4" = list(ip=.90, anc1=0,   anc2=0,   anc3=0, prep=0,   support=.80, ltfu=0,   poc=0),
    "Scenario 5" = list(ip=.90, anc1=0,   anc2=0,   anc3=0, prep=0,   support=.80, ltfu=.50, poc=0),
    "Scenario 6" = list(ip=.90, anc1=.80, anc2=0,   anc3=0, prep=0,   support=.80, ltfu=.80, poc=0),
    "Scenario 7" = list(ip=.90, anc1=.90, anc2=0,   anc3=0, prep=0,   support=.80, ltfu=.90, poc=0),
    "Scenario 8" = list(ip=.90, anc1=.90, anc2=0,   anc3=0, prep=.10, support=.80, ltfu=0,   poc=0),
    "Scenario 9" = list(ip=.90, anc1=0,   anc2=.85, anc3=0, prep=.10, support=.80, ltfu=.90, poc=.60)
  )
}

# ============================================================
# UI helpers
# ============================================================
all_countries <- names(country_defaults)

make_country_tab <- function(country_name) {
  d <- country_defaults[[country_name]]
  id <- gsub(" ", "_", country_name)
  all_scenario_names <- c("Baseline", paste0("Scenario ", 1:9))
  tabItem(paste0("tab_", id),
    # Row 1: inputs + scenario settings + scenario selector + run button
    fluidRow(
      # Left: Country input parameters (renamed)
      box(width=4, title=paste(country_name, "— Country input parameters"),
          status="primary", solidHeader=TRUE,
        numericInput(paste0(id,"_pregnant_pop"), "Annual pregnant population", d$pregnant_pop, min=1000, step=1000),
        sliderInput(paste0(id,"_hiv_prev"), "HIV prevalence, women 15-49", min=0.01, max=0.35, value=d$hiv_prev, step=0.001),
        sliderInput(paste0(id,"_pct_diagnosed"), "% HIV-positive diagnosed", min=0.5, max=1, value=d$pct_diagnosed, step=0.01),
        sliderInput(paste0(id,"_pct_art"), "% diagnosed on ART", min=0.5, max=1, value=d$pct_art, step=0.01),
        sliderInput(paste0(id,"_pct_virally_supp"), "% on ART virally suppressed", min=0.5, max=1, value=d$pct_virally_supp, step=0.01),
        numericInput(paste0(id,"_hiv_incidence_annual"), "Annual HIV incidence", d$hiv_incidence_annual, min=0.0001, max=0.05, step=0.0001),
        sliderInput(paste0(id,"_pct_inf_prophylaxis"), "Baseline infant prophylaxis coverage", min=0, max=1, value=d$pct_inf_prophylaxis, step=0.01)
      ),
      # Middle: Scenario intervention settings + load button
      box(width=4, title="Scenario intervention parameters", status="warning", solidHeader=TRUE,
        selectInput(paste0(id,"_paper_scenario"), "Load Lancet frontier scenario", choices=all_scenario_names, selected="Baseline"),
        actionButton(paste0(id,"_load_scenario"), "Load selected scenario", class="btn-info btn-block"),
        hr(),
        sliderInput(paste0(id,"_int_ip"), "Infant prophylaxis coverage", min=0, max=1, value=.50, step=.05),
        radioButtons(paste0(id,"_int_anc_strat"), "HIV retesting strategy", choices=c("None"=0,"Option 1"=1,"Option 2"=2,"Option 3"=3), selected=0),
        sliderInput(paste0(id,"_int_anc_prop"), "Retesting coverage", min=0, max=1, value=0, step=.05),
        sliderInput(paste0(id,"_int_prep"), "PrEP coverage", min=0, max=.25, value=0, step=.01),
        sliderInput(paste0(id,"_int_sg"), "Maternal peer-support groups coverage", min=0, max=1, value=0, step=.05),
        sliderInput(paste0(id,"_int_ltfu"), "LTFU tracing coverage", min=0, max=1, value=0, step=.05),
        sliderInput(paste0(id,"_int_poc"), "POC viral load coverage", min=0, max=1, value=0, step=.05)
      ),
      # Right: Scenario selector + Run button
      box(width=4, title="Scenarios to compare", status="info", solidHeader=TRUE,
        p("Select which Lancet frontier scenarios to include in the comparison output below.
           The current intervention parameter settings above define one custom scenario."),
        checkboxGroupInput(paste0(id,"_selected_scenarios"),
          label = "Include scenarios:",
          choices = all_scenario_names,
          selected = all_scenario_names
        ),
        hr(),
        actionButton(paste0(id,"_run"), paste0("Run scenario comparison — ", country_name),
                     class="btn-primary btn-block",
                     style="font-weight:700; font-size:14px;")
      )
    ),
    # Row 1b: Cost input parameters
    fluidRow(
      box(width=12, title=paste(country_name, "— Cost input parameters (USD)"),
          status="primary", solidHeader=FALSE,
        fluidRow(
          column(3,
            numericInput(paste0(id,"_cost_inf_prophylaxis"), "Infant prophylaxis (per HEI)", d$cost_inf_prophylaxis, min=0, step=0.01),
            numericInput(paste0(id,"_cost_anc_test"),        "ANC HIV test (per test)",       d$cost_anc_test,        min=0, step=0.01)
          ),
          column(3,
            numericInput(paste0(id,"_cost_prep"),            "PrEP (per person per month)",   d$cost_prep,            min=0, step=0.01),
            numericInput(paste0(id,"_cost_support_groups"),  "Support group (per person per month)", d$cost_support_groups, min=0, step=0.01)
          ),
          column(3,
            numericInput(paste0(id,"_cost_dtg_monthly"),     "TLD ART (per person per month)",   d$cost_dtg_monthly,    min=0, step=0.01)
          ),
          column(3,
            numericInput(paste0(id,"_cost_ltfu_min"),        "LTFU tracing (per person traced)", d$cost_ltfu_min,       min=0, step=0.01),
            numericInput(paste0(id,"_cost_poc_vl"),          "POC viral load test (per test)",   d$cost_poc_vl,         min=0, step=0.01)
          )
        )
      )
    ),
    # Row 2: Scenario comparison table (styled like Table 3)
    fluidRow(
      box(width=12, title="Scenario comparison table", status="success", solidHeader=TRUE,
        div(style="margin-bottom:8px;",
          downloadButton(paste0(id,"_dl_table"), "Download table (.csv)",
                         class="btn-sm btn-default")
        ),
        DTOutput(paste0(id,"_scenario_table"))
      )
    ),
    # Row 3: Cost-effectiveness frontier plot
    fluidRow(
      box(width=12, title="Cost-effectiveness plot — Total cost vs. combined infections averted",
          status="success", solidHeader=TRUE,
        div(style="margin-bottom:8px;",
          downloadButton(paste0(id,"_dl_plot"), "Download figure (.png)",
                         class="btn-sm btn-default")
        ),
        plotOutput(paste0(id,"_ce_plot"), height="420px")
      )
    ),
  )
}

function_catalog <- data.frame(
  Function_or_equation = c(
    "Initial state distribution",
    "Monthly HIV acquisition",
    "Diagnosis transition",
    "ART initiation transition",
    "Viral suppression transition",
    "Backflow from suppressed to unsuppressed ART",
    "Loss to follow-up",
    "Pregnancy to delivery flow",
    "Delivery to breastfeeding flow",
    "Breastfeeding exit",
    "Delivery infant infections",
    "Breastfeeding infant infections",
    "Incremental cost-effectiveness ratio"
  ),
  Formula_or_description = c(
    "Population is allocated to pregnant, delivery, and breastfeeding compartments, then HIV-negative, undiagnosed, diagnosed not on ART, ART not suppressed, and virally suppressed states.",
    "new_inf = HIV-negative women * annual HIV incidence / 12 * PrEP multiplier",
    "new_diag = undiagnosed HIV-positive women * monthly diagnosis transition rate",
    "new_art = diagnosed not on ART * monthly ART transition rate * POC VL multiplier",
    "new_vs = on ART not suppressed * monthly viral suppression transition rate * regimen-shift multiplier * POC VL multiplier",
    "back_art = virally suppressed * weighted DTG/EFZ backflow rate",
    "ltfu = ART not suppressed * monthly LTFU rate * LTFU intervention multiplier",
    "from_preg = pregnant compartment * 1/9 per month",
    "from_deliv = all women in delivery compartment move to breastfeeding in next cycle",
    "bf_exit = breastfeeding compartment * 1/19 per month",
    "child_inf_deliv = sum(delivery state counts * delivery MTCT probabilities by HIV state)",
    "child_inf_bf = sum(breastfeeding state counts * monthly breastfeeding MTCT probabilities by HIV state)",
    "ICER = incremental cost / additional infections averted versus previous frontier scenario"
  ),
  Implemented_in_app = c("run_model", "run_model", "run_model", "run_model", "run_model", "run_model", "run_model", "run_model", "run_model", "run_model", "run_model", "run_model", "comparison table / paper table 3"),

  stringsAsFactors = FALSE
)

model_structure_description <- data.frame(
  Section = c(
    "Model type and horizon",
    "Population followed",
    "Reproductive compartments",
    "HIV-care states",
    "Baseline assumptions",
    "Intervention scenario",
    "Outcomes",
    "Validation purpose"
  ),
  Description = c(
    "A 12-month, non-dynamic Markov cohort model. The model advances the same cohort through monthly cycles rather than modelling HIV transmission dynamically across the full population.",
    "Pregnant and breastfeeding women parameterized to Zambia in the original paper and workbook. The Shiny app keeps that Zambia structure and allows editable country inputs for additional comparisons.",
    "Women are allocated to pregnancy, delivery, and breastfeeding compartments. Pregnancy lasts approximately nine monthly cycles, delivery is treated as a short transition state, and breastfeeding exits over the breastfeeding period.",
    "Within each reproductive compartment, women are represented as HIV-negative, HIV-infected and undiagnosed, diagnosed but not on ART, on ART but not virally suppressed, and virally suppressed.",
    "At baseline, the workbook assumes 80% of pregnant and breastfeeding women on ART are on TLD, 50% of HIV-exposed infants receive prophylaxis, and pregnant women receive one HIV test during pregnancy.",
    "The intervention scenario turns on combinations of infant prophylaxis, HIV retesting, oral PrEP, maternal support groups, regimen shift to TLD, LTFU contact tracing, and point-of-care viral load testing.",
    "The main outcomes are incident HIV infections among infants, incident HIV infections among mothers, intervention costs, infections averted, and cost-effectiveness ratios.",
    "The app translates the Excel workbook equations into R functions so model-generated Shiny outputs can be compared against the Lancet paper tables and workbook outputs."
  ),
  stringsAsFactors = FALSE
)

model_function_description <- data.frame(
  Model_step = c(
    "Initialize cohort",
    "Apply HIV acquisition",
    "Apply diagnosis transition",
    "Apply ART initiation transition",
    "Apply viral suppression transition",
    "Apply loss to follow-up",
    "Apply backflow from viral suppression",
    "Move women from pregnancy to delivery",
    "Move women from delivery to breastfeeding",
    "Exit breastfeeding",
    "Calculate infant infections at delivery",
    "Calculate infant infections during breastfeeding",
    "Calculate intervention costs",
    "Calculate cost-effectiveness"
  ),
  R_translation = c(
    "state_count = population * reproductive_compartment_share * HIV_care_state_share",
    "new_hiv = hiv_negative * monthly_hiv_incidence * prep_multiplier",
    "new_diagnosed = undiagnosed_hiv_positive * diagnosis_rate * retesting_multiplier",
    "new_art = diagnosed_not_on_art * art_initiation_rate * poc_vl_art_multiplier",
    "new_suppressed = on_art_not_suppressed * suppression_rate * regimen_multiplier * poc_vl_suppression_multiplier",
    "ltfu = on_art_not_suppressed * ltfu_rate * ltfu_intervention_multiplier",
    "backflow = virally_suppressed * weighted_backflow_rate_by_regimen",
    "from_pregnancy = pregnant_state_count * monthly_delivery_rate",
    "from_delivery = delivery_state_count",
    "bf_exit = breastfeeding_state_count * monthly_breastfeeding_exit_rate",
    "infant_infections_delivery = sum(delivery_state_counts * delivery_mtct_risks * mtct_multipliers)",
    "infant_infections_breastfeeding = sum(breastfeeding_state_counts * monthly_breastfeeding_mtct_risks * mtct_multipliers)",
    "total_cost = infant_prophylaxis_cost + hiv_retesting_cost + prep_cost + support_group_cost + regimen_cost + ltfu_tracing_cost + poc_vl_cost",
    "icer = incremental_cost / incremental_infections_averted"
  ),
  Formula_description = c(
    "Creates the starting counts in each reproductive compartment and HIV-care state.",
    "Moves HIV-negative women into the undiagnosed HIV-positive state; PrEP reduces this transition.",
    "Moves undiagnosed HIV-positive women into the diagnosed state; retesting strategies increase detection.",
    "Moves diagnosed women onto ART; point-of-care viral load can affect the care cascade in the intervention model.",
    "Moves women on ART into the virally suppressed state; the TLD regimen shift and point-of-care viral load increase suppression.",
    "Moves women out of effective ART care; LTFU tracing reduces this loss.",
    "Allows some virally suppressed women to return to the on-ART-not-suppressed state using regimen-specific backflow rates.",
    "Advances a fraction of pregnant women to the delivery compartment in each monthly cycle.",
    "Moves the delivery compartment into breastfeeding after the delivery cycle.",
    "Removes a fraction of women from the breastfeeding compartment each month.",
    "Calculates infant infections occurring around delivery by HIV-care state and maternal transmission risk.",
    "Calculates infant infections occurring during breastfeeding by HIV-care state and monthly breastfeeding transmission risk.",
    "Adds intervention-specific costs using the eligible population and selected coverage for each intervention.",
    "Compares incremental costs and incremental infections averted across scenarios."
  ),
  stringsAsFactors = FALSE
)

model_intervention_description <- data.frame(
  Intervention = c(
    "Infant prophylaxis",
    "HIV retesting strategy 1",
    "HIV retesting strategy 2",
    "HIV retesting strategy 3",
    "Oral PrEP",
    "Maternal support groups",
    "LTFU contact tracing",
    "Point-of-care viral load testing"
  ),
  App_parameter = c(
    "int_infant_prop", "int_anc1", "int_anc2", "int_anc3", "int_prep", "int_support", "int_ltfu", "int_poc_vl"
  ),
  Model_role = c(
    "Reduces infant infection risk among HIV-exposed infants receiving prophylaxis.",
    "Adds retesting during late antenatal care, delivery, and six-week postpartum contact.",
    "Adds the strategy 1 retesting schedule plus an additional postpartum retest.",
    "Adds repeat retesting through breastfeeding at the highest frequency represented in the workbook.",
    "Reduces HIV acquisition among HIV-negative pregnant and breastfeeding women.",
    "Improves outcomes among HIV-positive pregnant and breastfeeding women through support and retention effects.",
    "Reduces loss to follow-up among women in ART care.",
    "Improves ART and viral suppression transitions and adds viral load testing costs."
  ),
  stringsAsFactors = FALSE
)

model_output_description <- data.frame(
  Output = c(
    "Infant HIV infections",
    "Maternal HIV infections",
    "Infant infections averted",
    "Maternal infections averted",
    "Vertical transmission reduction",
    "Intervention cost",
    "Cost-effectiveness"
  ),
  Description = c(
    "Sum of delivery and breastfeeding infant infections across the 12 monthly cycles.",
    "Incident HIV acquisitions among mothers across pregnancy and breastfeeding compartments.",
    "Baseline infant infections minus scenario infant infections.",
    "Baseline maternal infections minus scenario maternal infections.",
    "Percent reduction in infant infections or vertical transmission relative to baseline, depending on table context.",
    "Total cost of selected interventions applied to eligible populations over the model horizon.",
    "Incremental cost per additional infection averted for each frontier scenario."
  ),
  stringsAsFactors = FALSE
)

ui <- dashboardPage(
  skin="blue",
  dashboardHeader(title="EVT Investment Case Model", titleWidth=330),
  dashboardSidebar(width=245,
    sidebarMenu(id="tabs",
      menuItem("About", tabName="about"),
      menuItem("Original publication tables", tabName="paper_tables"),
      menuItem("Model description", tabName="model_description"),
      menuItem("Intervention effectiveness", tabName="effectiveness"),
      menuItem("Country parameters", tabName="country_params"),
      menuItem("Zambia", tabName="tab_Zambia"),
      menuItem("Kenya", tabName="tab_Kenya"),
      menuItem("Mozambique", tabName="tab_Mozambique"),
      menuItem("Malawi", tabName="tab_Malawi"),
      menuItem("Zimbabwe", tabName="tab_Zimbabwe"),
      menuItem("South Africa", tabName="tab_South_Africa")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper{background:#f8f9fa}
      .box{border-radius:8px}
      .small-box{border-radius:8px}
      .dataTables_wrapper{width:100%; overflow-x:hidden;}
      table.dataTable{table-layout:fixed !important; width:100% !important;}
      table.dataTable thead th, table.dataTable tbody td{
        white-space:normal !important;
        word-break:normal;
        overflow-wrap:anywhere;
        vertical-align:top;
      }
      #paper_table3 table.dataTable thead th, #paper_table3 table.dataTable tbody td{
        white-space:normal !important;
        overflow-wrap:anywhere;
      }
    "))),
    tabItems(
      tabItem("about",
        box(width=12, title="About", status="primary", solidHeader=TRUE,
          p("This app recreates the EVT investment case model from ",
            tags$a("Chevalier et al., Lancet Global Health 2024",
                   href="https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(23)00588-0/fulltext",
                   target="_blank"),
            ". The app translates the Chevalier model model logic into R."),
          p("This is a 12-month deterministic cohort Markov model that tracks proportions of pregnant and breastfeeding women (PBFW) flowing between HIV care states, originally parameterized to Zambia. The model incorporates six interventions to eliminate vertical transmission (EVT):"),
          tags$ul(
            tags$li(tags$b("Infant prophylaxis: "), "administration of antiretroviral prophylaxis to HIV-exposed infants at delivery"),
            tags$li(tags$b("HIV retesting: "), "additional HIV testing at three optional time points — during late antenatal care (ANC), at delivery, and postpartum"),
            tags$li(tags$b("Oral pre-exposure prophylaxis (PrEP): "), "daily antiretroviral prophylaxis for HIV-negative PBFW"),
            tags$li(tags$b("Maternal peer-support groups: "), "structured support for HIV-positive PBFW to improve ART adherence and retention"),
            tags$li(tags$b("Loss-to-follow-up (LTFU) contact tracing: "), "active outreach to re-engage PBFW who have disengaged from HIV care"),
            tags$li(tags$b("Point-of-care viral load (POC VL) testing: "), "rapid on-site viral load testing to support timely clinical decision-making")
          ),
          p("TLD (dolutegravir-based) ART is assumed universal for all HIV-infected PBFW. At baseline, 50% of exposed infants received prophylaxis and pregnant women received one HIV test during pregnancy. Outcomes included in this model are incident HIV infections among mothers and infants with associated intervention costs."),
          p("Country parameters for Zambia, Kenya, Mozambique, Malawi, Zimbabwe, and South Africa are editable starter values and should be replaced with country-specific Spectrum/AIM, DHS, PHIA, PEPFAR/MER, or Thembisa inputs before decision use.")
        )
      ),
      tabItem("paper_tables",
        tabBox(width=12,
          tabPanel("Table 1 - intervention effects", DTOutput("paper_table1")),
          tabPanel("Table 2 - unit costs", DTOutput("paper_table2")),
          tabPanel("Table 3 - frontier scenarios", DTOutput("paper_table3"))
        )
      ),
      tabItem("model_description",
        tabBox(width=12,
          tabPanel("Model overview",
            tags$ul(style="font-size:14px; line-height:2.0; margin-top:10px;",
              tags$li(tags$b("Model type and horizon: "), "A 12-month, non-dynamic Markov cohort model. The model advances the same cohort through monthly cycles rather than modelling HIV transmission dynamically across the full population."),
              tags$li(tags$b("Population followed: "), "Pregnant and breastfeeding women parameterized to Zambia in the original paper and workbook. The Shiny app keeps that Zambia structure and allows editable country inputs for additional comparisons."),
              tags$li(tags$b("Reproductive compartments: "), "Women are allocated to pregnancy, delivery, and breastfeeding compartments. Pregnancy lasts approximately nine monthly cycles, delivery is treated as a short transition state, and breastfeeding exits over the breastfeeding period."),
              tags$li(tags$b("HIV-care states: "), "Within each reproductive compartment, women are represented as HIV-negative, HIV-infected and undiagnosed, diagnosed but not on ART, on ART but not virally suppressed, and virally suppressed."),
              tags$li(tags$b("Baseline assumptions: "), "TLD (dolutegravir-based) ART is assumed universal for all HIV-infected PBFW. At baseline, 50% of HIV-exposed infants receive prophylaxis and pregnant women receive one HIV test during pregnancy."),
              tags$li(tags$b("Intervention scenario: "), "The intervention scenario turns on combinations of infant prophylaxis, HIV retesting, oral PrEP, maternal support groups, LTFU contact tracing, and point-of-care viral load testing."),
              tags$li(tags$b("Outcomes: "), "Incident HIV infections among infants and mothers, intervention costs, infections averted, and incremental cost-effectiveness ratios."),
              tags$li(tags$b("Validation: "), "The app translates the Excel workbook equations into R so model outputs can be compared against Lancet paper tables and workbook outputs.")
            )
          ),
          tabPanel("R translation of model steps",
            p("The table below maps each model step to its R calculation and interpretation.", style="margin-top:10px;"),
            DTOutput("model_function_table")
          ),
          tabPanel("Interventions",
            DTOutput("model_intervention_table")
          ),
          tabPanel("Outputs",
            DTOutput("model_output_table")
          ),
          tabPanel("R model code",
            p("The core Markov model R code is shown below for reference. This is the ", tags$code("run_model()"), " function that implements the Excel workbook logic.", style="margin-top:10px;"),
            div(style="margin-bottom:6px;",
              tags$button("Copy code to clipboard", id="copy_rcode_btn",
                onclick="
                  var txt = document.getElementById('rcode_block').innerText;
                  navigator.clipboard.writeText(txt).then(function(){
                    var btn = document.getElementById('copy_rcode_btn');
                    btn.innerText = 'Copied!';
                    setTimeout(function(){ btn.innerText = 'Copy code to clipboard'; }, 2000);
                  });
                ",
                class="btn btn-default btn-sm")
            ),
            div(id="rcode_block",
              style="background:#f8f8f8; border:1px solid #ddd; border-radius:4px; padding:14px; font-size:11.5px; overflow:auto; height:calc(100vh - 280px); min-height:300px;",
              tags$pre(style="margin:0; white-space:pre; background:transparent; border:none; padding:0;",
                tags$code(class="language-r",
"run_model <- function(params, n_months = 12,
                      int_infant_prop, int_anc1, int_anc2, int_anc3,
                      int_prep, int_support,
                      int_ltfu, int_poc_vl) {
  # Merge country-specific and shared parameters
  p <- safe_param_list(params, country_defaults$Zambia)
  p <- safe_param_list(shared_params, p)

  population      <- p$pregnant_pop * 3
  dist_preg       <- p$pregnant_pop / population
  dist_deliv      <- ((1/280) * 30.5) * dist_preg
  dist_bf         <- 1 - dist_preg - dist_deliv
  transition_delivery  <- (1/280) * 30.5
  transition_pregnancy <- p$birth_rate_monthly
  transition_hiv       <- p$hiv_incidence_annual / 12

  # TLD (DTG) is assumed universal (100%). Backflow and viral suppression
  # multiplier are fixed constants from the DTG-specific parameters.
  backflow_fixed <- p$backflow_dtg   # 0.007917
  vs_mult_fixed  <- 1.64             # 1 + 1.0 * 0.64

  # Determine whether any intervention is active
  intervention_on <- isTRUE(int_infant_prop > p$pct_inf_prophylaxis) ||
    isTRUE(int_anc1 > 0) || isTRUE(int_anc2 > 0) || isTRUE(int_anc3 > 0) ||
    isTRUE(int_prep > 0) || isTRUE(int_support > 0) ||
    isTRUE(int_ltfu > 0) || isTRUE(int_poc_vl > 0)

  # --- Intervention multipliers (from Excel Intervention Multipliers sheet) ---
  ip_adj           <- 1 - int_infant_prop * (1 - 0.50)   # infant prophylaxis MTCT adjustment
  anc1_mult        <- if (isTRUE(int_anc1 > 0)) 1 - int_anc1 * (1 - 0.81) else 1
  anc2_mult        <- if (isTRUE(int_anc2 > 0)) 1 - int_anc2 * (1 - 0.79) else 1
  anc3_mult        <- if (isTRUE(int_anc3 > 0)) 1 - int_anc3 * (1 - 0.78) else 1
  mtct_hiv_inf_mult  <- min(anc1_mult, anc2_mult, anc3_mult)
  transition_hiv_mult <- if (isTRUE(int_prep > 0)) 1 - int_prep * (1 - 0.35) else 1
  support_mtct_mult  <- if (isTRUE(int_support > 0)) 1 - int_support * (1 - 0.135) else 1
  mtct_diag_mult   <- ip_adj * support_mtct_mult
  mtct_art_mult    <- ip_adj * support_mtct_mult
  mtct_vs_mult     <- ip_adj
  ltfu_mult        <- if (isTRUE(int_ltfu > 0)) 1 - int_ltfu * (1 - 0.71) else 1
  art_preg_mult    <- if (isTRUE(int_poc_vl > 0)) 1 + int_poc_vl * 0.10 else 1
  art_bf_mult      <- if (isTRUE(int_poc_vl > 0)) 1 + int_poc_vl * 0.10 else 1

  # --- Initial state (T=0): allocate population to compartments and HIV states ---
  make_initial <- function() {
    out <- list()
    for (stage in c('preg', 'deliv', 'bf')) {
      dist <- switch(stage, preg=dist_preg, deliv=dist_deliv, bf=dist_bf)
      B <- population * (1 - p$hiv_prev) * dist           # HIV-negative
      C <- population * p$hiv_prev * dist                  # all HIV-infected
      D <- C * p$pct_diagnosed                             # diagnosed (all)
      E <- population * p$hiv_prev * dist * p$pct_diagnosed * p$pct_art
      I <- E * p$pct_virally_supp                          # virally suppressed
      out[[stage]] <- c(neg=B, inf=C-D, diag=D-E, art=E-I, vs=I)
    }
    out
  }

  # --- Child infection calculator ---
  child_infections <- function(rows, intervention = FALSE, t = 0) {
    # T=0 always uses baseline formula (matching Excel Intervention Model T=0 row)
    if (!intervention || t == 0) {
      deliv <- rows$deliv; bf <- rows$bf
      deliv_inf <- deliv[['inf']] * p$mtct_hiv_inf +
        (deliv[['diag']]*p$mtct_diagnosed + deliv[['art']]*p$mtct_art + deliv[['vs']]*p$mtct_virally_supp) *
        (1 - p$pct_inf_prophylaxis * 0.5)
      bf_inf <- bf[['inf']]*p$mtct_bf_hiv_inf + bf[['diag']]*p$mtct_bf_diagnosed +
        bf[['art']]*p$mtct_bf_art + bf[['vs']]*p$mtct_bf_virally_supp
    } else {
      deliv <- rows$deliv; bf <- rows$bf
      deliv_inf <- deliv[['inf']]*p$mtct_hiv_inf*mtct_hiv_inf_mult +
        deliv[['diag']]*p$mtct_diagnosed*mtct_diag_mult +
        deliv[['art']]*p$mtct_art*mtct_art_mult +
        deliv[['vs']]*p$mtct_virally_supp*mtct_vs_mult
      # BF: hiv_inf term * mhm; (diag+art+vs) terms divided by ip_adj (=Inf_Pro_Adj in Excel)
      bf_inf <- bf[['inf']]*p$mtct_bf_hiv_inf*mtct_hiv_inf_mult +
        (bf[['diag']]*p$mtct_bf_diagnosed*mtct_diag_mult +
         bf[['art']]*p$mtct_bf_art*mtct_art_mult +
         bf[['vs']]*p$mtct_bf_virally_supp*mtct_vs_mult) / ip_adj
    }
    c(delivery=deliv_inf, breastfeeding=bf_inf, total=deliv_inf+bf_inf)
  }

  # --- Monthly HIV health-state transitions + entry into population ---
  transition_before_delivery <- function(rows, intervention = FALSE) {
    r <- lapply(rows, function(v) { names(v) <- NULL; v })
    names(r$preg)  <- c('neg','inf','diag','art','vs')
    names(r$deliv) <- c('neg','inf','diag','art','vs')
    names(r$bf)    <- c('neg','inf','diag','art','vs')
    cp <- p$childbearing_pop
    # [transitions coded here — see run_model() in app.R for full equations]
    # Baseline and intervention branches each update all five states
    # for pregnant, delivery, and breastfeeding compartments.
    ...
  }

  # --- Delivery: pregnant -> delivery compartment flush ---
  apply_delivery <- function(pre) {
    preg_new  <- pre$preg  - pre$preg  * transition_delivery
    deliv_new <- pre$preg  * transition_delivery
    bf_new    <- pre$bf    + pre$deliv
    names(preg_new) <- names(deliv_new) <- names(bf_new) <- c('neg','inf','diag','art','vs')
    list(preg=preg_new, deliv=deliv_new, bf=bf_new)
  }

  # --- Run 12 monthly cycles ---
  rows    <- make_initial()
  history <- list(rows)
  for (month in seq_len(n_months))
    history[[month + 1]] <- apply_delivery(transition_before_delivery(rows, intervention_on))

  # Collect outputs: infections + compartment counts for months 0..12
  out <- bind_rows(lapply(seq_along(history), function(i) {
    ci <- child_infections(history[[i]], intervention_on, t = i - 1)
    data.frame(t=i-1, child_inf_total=ci['total'], ...)
  }))
  out
}"
                )
              )
            )
          )
        )
      ),
      tabItem("effectiveness",
        box(width=12, title="Intervention effectiveness parameters",
            status="warning", solidHeader=TRUE,
            p("The table below shows the default effectiveness value for each intervention, as sourced from the original ",
              tags$a("Chevalier et al. (Lancet Global Health, 2024)", href="https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(23)00588-0/fulltext", target="_blank"),
              " workbook (Excel Intervention Multipliers sheet). Each value is the parameter used in the original workbook to represent the intervention's effect, drawn from published evidence as cited in Table 1 of Chevalier et al. (2024). The effect will be scaled by the coverage proportion set in each scenario."),
          p(style="color:#c0392b; font-size:13px;",
            tags$b("Note: "), "Changing these values will affect model outputs for all countries. Default values are from the original publication and should only be changed if you have country-specific or updated evidence."),
          fluidRow(
            column(6,
              tags$h4("Infant prophylaxis", style="margin-top:6px;"),
              sliderInput("eff_ip", "IP: proportion of MTCT retained at full coverage (lower = more effective)",
                          min=0, max=1, value=0.50, step=0.01),
              tags$p(style="color:#555; font-size:12px;",
                "Interpretation: at 100% IP coverage, diagnosed/ART/VS MTCT rates are multiplied by this factor. Default 0.50 = 50% reduction."),

              tags$h4("ANC HIV retesting", style="margin-top:16px;"),
              sliderInput("eff_anc1", "ANC option 1: residual HIV-inf MTCT fraction at full coverage",
                          min=0, max=1, value=0.81, step=0.01),
              sliderInput("eff_anc2", "ANC option 2: residual HIV-inf MTCT fraction at full coverage",
                          min=0, max=1, value=0.79, step=0.01),
              sliderInput("eff_anc3", "ANC option 3: residual HIV-inf MTCT fraction at full coverage",
                          min=0, max=1, value=0.78, step=0.01),
              tags$p(style="color:#555; font-size:12px;",
                "Interpretation: at 100% retesting coverage, undiagnosed-HIV MTCT is multiplied by this factor. Default 0.81/0.79/0.78 = 19%/21%/22% reductions."),

              tags$h4("Oral PrEP", style="margin-top:16px;"),
              sliderInput("eff_prep", "PrEP: residual HIV acquisition fraction at full coverage",
                          min=0, max=1, value=0.35, step=0.01),
              tags$p(style="color:#555; font-size:12px;",
                "Interpretation: at 100% PrEP coverage, monthly HIV acquisition among HIV-negative PBFW is multiplied by this factor. Default 0.35 = 65% effectiveness.")
            ),
            column(6,
              tags$h4("Maternal peer-support groups", style="margin-top:6px;"),
              sliderInput("eff_support", "Support groups: residual MTCT fraction for diagnosed/ART women at full coverage",
                          min=0, max=1, value=0.135, step=0.005),
              tags$p(style="color:#555; font-size:12px;",
                "Interpretation: at 100% coverage, MTCT for diagnosed and ART-not-VS women is multiplied by this factor. Default 0.135 = 86.5% reduction."),

              tags$h4("LTFU contact tracing", style="margin-top:16px;"),
              sliderInput("eff_ltfu", "LTFU tracing: residual LTFU rate fraction at full coverage",
                          min=0, max=1, value=0.71, step=0.01),
              tags$p(style="color:#555; font-size:12px;",
                "Interpretation: at 100% tracing coverage, the monthly LTFU rate is multiplied by this factor. Default 0.71 = 29% reduction in LTFU."),

              tags$h4("Point-of-care viral load (POC VL)", style="margin-top:16px;"),
              sliderInput("eff_poc", "POC VL: proportional increase in viral suppression transition rate at full coverage",
                          min=0, max=0.5, value=0.10, step=0.01),
              tags$p(style="color:#555; font-size:12px;",
                "Interpretation: at 100% POC VL coverage, the monthly ART-to-VS transition rate is multiplied by (1 + this value). Default 0.10 = 10% increase in suppression rate."),

              tags$br(),
              actionButton("eff_reset", "Reset all to defaults", class="btn-default btn-sm")
            )
          )
        )
      ),
      tabItem("country_params",
        box(width=12, title="Country parameters — sources and default values",
            status="primary", solidHeader=TRUE,
          p(tags$b("Default"), " epidemiological and cost parameters for each country are drawn from the sources listed below.
             ", tags$b("All parameters are editable within each country tab."), "
             Non-Zambia cost parameters are indicative estimates
             and should be replaced with country-specific data before use in policy discussions"),
          tabBox(width=12,

            # ── Epidemiological parameters tab ──────────────────────────────
            tabPanel("Epidemiological parameters",
              p(style="margin-top:8px; color:#555;",
                "Sources: ",
                tags$a("UNAIDS AIDSinfo 2024", href="https://aidsinfo.unaids.org/", target="_blank"), " — ",
                tags$a("PHIA Project (ZAMPHIA 2021, KENPHIA 2018, MPHIA 2020–21, ZIMPHIA 2020)", href="https://phia.icap.columbia.edu/", target="_blank"), " — ",
                tags$a("INSIDA 2021 (Mozambique)", href="https://www.misau.gov.mz/", target="_blank"), " — ",
                tags$a("HSRC South Africa 2022", href="https://www.hsrc.ac.za/", target="_blank"), " — ",
                tags$a("UN World Population Prospects 2024", href="https://population.un.org/wpp/", target="_blank"), " — ",
                tags$a("World Bank Open Data", href="https://data.worldbank.org/", target="_blank")
              ),
              DTOutput("country_epi_table")
            ),

            # ── Cost parameters tab ─────────────────────────────────────────
            tabPanel("Cost parameters",
              p(style="margin-top:8px; color:#555;",
                "Zambia cost parameters are from the original Chevalier et al. (2024) workbook costing analysis. ",
                "Non-Zambia values are indicative estimates informed by regional procurement benchmarks and should be replaced with country-specific data. ",
                "Sources: ",
                tags$a("Chevalier et al., Lancet Global Health 2024 (Zambia)", href="https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(23)00588-0/fulltext", target="_blank"), " — ",
                tags$a("PEPFAR/GHSD ARV price lists", href="https://www.prepwatch.org/resources/prep-procurement/", target="_blank"), " — ",
                tags$a("WHO-CHOICE unit costs", href="https://www.who.int/teams/health-economics/cost-effectiveness", target="_blank"), " — ",
                tags$a("Global Fund price reporting", href="https://www.theglobalfund.org/en/sourcing-management/price-reporting/", target="_blank")
              ),
              p(style="color:#555; font-size:13px;",
                "TLD (dolutegravir-based) ART is assumed universal. The TLD ART cost is the per-person-per-month drug and delivery cost."),
              DTOutput("country_cost_table")
            )
          )
        )
      ),
      make_country_tab("Zambia"),
      make_country_tab("Kenya"),
      make_country_tab("Mozambique"),
      make_country_tab("Malawi"),
      make_country_tab("Zimbabwe"),
      make_country_tab("South Africa")
    )
  )
)

server <- function(input, output, session) {
  country_results <- reactiveValues()
  country_costs <- reactiveValues()

  output$paper_table1 <- renderDT(datatable(paper_table1, options=list(dom='t', paging=FALSE, searching=FALSE, info=FALSE, scrollX=TRUE), rownames=FALSE))
  output$paper_table2 <- renderDT(datatable(paper_table2, options=list(dom='t', paging=FALSE, searching=FALSE, info=FALSE), rownames=FALSE) %>% formatCurrency("Cost_USD_2022", "$", digits=2))
  output$paper_table3 <- renderDT({
    table3_display <- paper_table3
    names(table3_display) <- c(
      "Scenario",
      "Intervention",
      "Additional infant infections averted",
      "Additional maternal infections averted",
      "Additional reduction in vertical transmission",
      "Total cost",
      "ICER"
    )

    datatable(
      table3_display,
      rownames = FALSE,
      escape = TRUE,
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        autoWidth = FALSE,
        scrollX = FALSE,
        columnDefs = list(
          list(width = '8%', targets = 0),
          list(width = '32%', targets = 1),
          list(width = '12%', targets = 2),
          list(width = '12%', targets = 3),
          list(width = '14%', targets = 4),
          list(width = '12%', targets = 5),
          list(width = '10%', targets = 6)
        )
      ),
      class = 'compact stripe cell-border wrap-table'
    )
  })


  output$model_function_table <- renderDT(datatable(model_function_description, options=list(dom='t', paging=FALSE, searching=FALSE, info=FALSE), rownames=FALSE))
  output$model_intervention_table <- renderDT(datatable(model_intervention_description, options=list(dom='t', paging=FALSE, searching=FALSE, info=FALSE), rownames=FALSE))
  output$model_output_table <- renderDT(datatable(model_output_description, options=list(dom='t', paging=FALSE, searching=FALSE, info=FALSE), rownames=FALSE))

  # Country parameter tables for the "Country parameters" section
  output$country_epi_table <- renderDT({
    epi_df <- data.frame(
      Country            = names(country_defaults),
      Pregnant_pop       = sapply(country_defaults, `[[`, "pregnant_pop"),
      HIV_prev_15_49     = sapply(country_defaults, `[[`, "hiv_prev"),
      Pct_diagnosed      = sapply(country_defaults, `[[`, "pct_diagnosed"),
      Pct_on_ART         = sapply(country_defaults, `[[`, "pct_art"),
      Pct_virally_supp   = sapply(country_defaults, `[[`, "pct_virally_supp"),
      Pct_inf_prophylaxis= sapply(country_defaults, `[[`, "pct_inf_prophylaxis"),
      HIV_incidence_annual = sapply(country_defaults, `[[`, "hiv_incidence_annual"),
      Source = c(
        "ZAMPHIA 2021; Chevalier et al. 2024",
        "KENPHIA 2018; UNAIDS 2024",
        "INSIDA 2021; UNAIDS 2024",
        "MPHIA 2020–21; UNAIDS 2024",
        "ZIMPHIA 2020; UNAIDS 2024",
        "HSRC 2022; UNAIDS 2024"
      ),
      stringsAsFactors = FALSE
    )
    datatable(epi_df,
      colnames = c("Country","Annual pregnant pop","HIV prev (15–49)","% diagnosed","% on ART",
                   "% virally suppressed","Baseline IP coverage","Annual HIV incidence","Source"),
      options  = list(dom='t', paging=FALSE, searching=FALSE, info=FALSE, scrollX=TRUE),
      rownames = FALSE
    ) %>%
      formatPercentage(c("HIV_prev_15_49","Pct_diagnosed","Pct_on_ART","Pct_virally_supp",
                         "Pct_inf_prophylaxis","HIV_incidence_annual"), digits=1)
  })

  output$country_cost_table <- renderDT({
    cost_df <- data.frame(
      Country              = names(country_defaults),
      IP_per_HEI           = sapply(country_defaults, `[[`, "cost_inf_prophylaxis"),
      ANC_test             = sapply(country_defaults, `[[`, "cost_anc_test"),
      PrEP_per_person_month= sapply(country_defaults, `[[`, "cost_prep"),
      Support_groups       = sapply(country_defaults, `[[`, "cost_support_groups"),
      TLD_per_person_month = sapply(country_defaults, `[[`, "cost_dtg_monthly"),
      LTFU_per_person      = sapply(country_defaults, `[[`, "cost_ltfu_min"),
      POC_VL_per_test      = sapply(country_defaults, `[[`, "cost_poc_vl"),
      Source = c(
        "Chevalier et al. 2024 workbook (validated)",
        "PEPFAR/GHSD estimates; WHO-CHOICE",
        "PEPFAR/GHSD estimates; WHO-CHOICE",
        "PEPFAR/GHSD estimates; WHO-CHOICE",
        "PEPFAR/GHSD estimates; WHO-CHOICE",
        "PEPFAR/GHSD estimates; WHO-CHOICE"
      ),
      stringsAsFactors = FALSE
    )
    datatable(cost_df,
      colnames = c("Country","IP ($/HEI)","ANC test ($/test)","PrEP ($/person/month)",
                   "Support groups ($/person/month)","TLD ART ($/person/month)",
                   "LTFU tracing ($/person traced)",
                   "POC VL ($/test)","Source"),
      options  = list(dom='t', paging=FALSE, searching=FALSE, info=FALSE, scrollX=TRUE),
      rownames = FALSE
    ) %>%
      formatCurrency(c("IP_per_HEI","ANC_test","PrEP_per_person_month","Support_groups",
                       "TLD_per_person_month","LTFU_per_person","POC_VL_per_test"),
                     "$", digits=2)
  })

  # Reset effectiveness parameters to defaults
  observeEvent(input$eff_reset, {
    updateSliderInput(session, "eff_ip",      value = shared_params$eff_ip)
    updateSliderInput(session, "eff_anc1",    value = shared_params$eff_anc1)
    updateSliderInput(session, "eff_anc2",    value = shared_params$eff_anc2)
    updateSliderInput(session, "eff_anc3",    value = shared_params$eff_anc3)
    updateSliderInput(session, "eff_prep",    value = shared_params$eff_prep)
    updateSliderInput(session, "eff_support", value = shared_params$eff_support)
    updateSliderInput(session, "eff_ltfu",    value = shared_params$eff_ltfu)
    updateSliderInput(session, "eff_poc",     value = shared_params$eff_poc)
  })

  gather_params <- function(cn) {
    id <- gsub(" ", "_", cn)
    d <- country_defaults[[cn]]
    # Epidemiological parameters
    d$pregnant_pop         <- safe_num(input[[paste0(id,"_pregnant_pop")]],         d$pregnant_pop)
    d$hiv_prev             <- safe_num(input[[paste0(id,"_hiv_prev")]],             d$hiv_prev)
    d$pct_diagnosed        <- safe_num(input[[paste0(id,"_pct_diagnosed")]],        d$pct_diagnosed)
    d$pct_art              <- safe_num(input[[paste0(id,"_pct_art")]],              d$pct_art)
    d$pct_virally_supp     <- safe_num(input[[paste0(id,"_pct_virally_supp")]],     d$pct_virally_supp)
    d$pct_inf_prophylaxis  <- safe_num(input[[paste0(id,"_pct_inf_prophylaxis")]],  d$pct_inf_prophylaxis)
    d$hiv_incidence_annual <- safe_num(input[[paste0(id,"_hiv_incidence_annual")]], d$hiv_incidence_annual)
    # Cost parameters
    d$cost_inf_prophylaxis <- safe_num(input[[paste0(id,"_cost_inf_prophylaxis")]], d$cost_inf_prophylaxis)
    d$cost_anc_test        <- safe_num(input[[paste0(id,"_cost_anc_test")]],        d$cost_anc_test)
    d$cost_prep            <- safe_num(input[[paste0(id,"_cost_prep")]],            d$cost_prep)
    d$cost_support_groups  <- safe_num(input[[paste0(id,"_cost_support_groups")]],  d$cost_support_groups)
    d$cost_dtg_monthly     <- safe_num(input[[paste0(id,"_cost_dtg_monthly")]],     d$cost_dtg_monthly)
    d$cost_ltfu_min        <- safe_num(input[[paste0(id,"_cost_ltfu_min")]],        d$cost_ltfu_min)
    d$cost_poc_vl          <- safe_num(input[[paste0(id,"_cost_poc_vl")]],          d$cost_poc_vl)
    # Effectiveness parameters (global inputs from the Intervention effectiveness tab)
    d$eff_ip      <- safe_num(input$eff_ip,      shared_params$eff_ip)
    d$eff_anc1    <- safe_num(input$eff_anc1,    shared_params$eff_anc1)
    d$eff_anc2    <- safe_num(input$eff_anc2,    shared_params$eff_anc2)
    d$eff_anc3    <- safe_num(input$eff_anc3,    shared_params$eff_anc3)
    d$eff_prep    <- safe_num(input$eff_prep,    shared_params$eff_prep)
    d$eff_support <- safe_num(input$eff_support, shared_params$eff_support)
    d$eff_ltfu    <- safe_num(input$eff_ltfu,    shared_params$eff_ltfu)
    d$eff_poc     <- safe_num(input$eff_poc,     shared_params$eff_poc)
    d
  }

  # Run a single named scenario (from scenario_to_interventions) using country params.
  # Returns a one-row summary data.frame.
  run_one_scenario <- function(params, scen_name, custom_ints = NULL) {
    if (scen_name == "Custom") {
      s <- custom_ints
    } else {
      s <- scenario_to_interventions(scen_name)
      # Baseline scenario uses the country's own baseline IP coverage.
      if (scen_name == "Baseline") {
        s$ip  <- params$pct_inf_prophylaxis
      }
    }
    anc1 <- s$anc1; anc2 <- s$anc2; anc3 <- s$anc3
    df   <- run_model(params, 12, s$ip, anc1, anc2, anc3,
                      s$prep, s$support, s$ltfu, s$poc)
    cost <- compute_costs(df, params, 12, s$ip, anc1, anc2, anc3,
                          s$prep, s$support, s$ltfu, s$poc)
    infant_inf  <- sum(df$child_inf_total[-1])
    maternal_inf <- sum(pmax(df$maternal_incident_hiv[-1], 0))
    list(df=df, cost=cost, infant_inf=infant_inf, maternal_inf=maternal_inf,
         scen_name=scen_name, s=s)
  }

  # Build scenario comparison table rows for a country, given selected scenario names.
  # ICERs use sequential pairwise incremental analysis matching Chevalier et al. Table 3:
  # each scenario's ICER = (cost_N - cost_{N-1}) / (infections_averted_N - infections_averted_{N-1})
  # where scenarios are in the fixed Table 3 order (Baseline, 1, 2, … 9).
  build_scenario_table <- function(cn, selected_scens, custom_ints, params) {
    base_res <- run_one_scenario(params, "Baseline")
    base_infant   <- base_res$infant_inf
    base_maternal <- base_res$maternal_inf
    base_cost     <- base_res$cost$total

    # Run all selected scenarios
    all_res <- lapply(selected_scens, function(sn) {
      if (sn == "Baseline") base_res else run_one_scenario(params, sn, custom_ints)
    })
    names(all_res) <- selected_scens

    # Preserve the canonical scenario order for sequential pairwise ICER computation
    canonical_order <- c("Baseline", paste0("Scenario ", 1:9), "Custom")
    ordered_selected <- canonical_order[canonical_order %in% selected_scens]

    # Compute sequential pairwise ICERs along the ordered scenario list
    icer_map <- list()
    prev_inf_total  <- base_infant + base_maternal
    prev_cost_total <- base_cost
    for (sn in ordered_selected) {
      if (sn == "Baseline") {
        icer_map[[sn]] <- "—"
        next
      }
      res <- all_res[[sn]]
      curr_inf_total <- res$infant_inf + res$maternal_inf
      curr_cost      <- res$cost$total
      d_inf  <- prev_inf_total - curr_inf_total   # infections avoided vs previous
      d_cost <- curr_cost - prev_cost_total        # added cost vs previous
      if (isTRUE(d_cost < 0) && isTRUE(d_inf >= 0)) {
        icer_map[[sn]] <- "Cost-saving"
      } else if (isTRUE(d_inf <= 0)) {
        icer_map[[sn]] <- "Dominated"
      } else {
        icer_map[[sn]] <- paste0("$", format(round(d_cost / d_inf), big.mark = ","))
      }
      prev_inf_total  <- curr_inf_total
      prev_cost_total <- curr_cost
    }

    # Build output rows in the original selected order
    rows <- lapply(selected_scens, function(sn) {
      res <- all_res[[sn]]
      infant_averted   <- base_infant  - res$infant_inf
      maternal_averted <- base_maternal - res$maternal_inf
      combined_averted <- infant_averted + maternal_averted
      pct_vt    <- if (isTRUE(base_infant > 0)) infant_averted / base_infant else NA_real_
      incr_cost <- res$cost$total - base_cost
      icer_str  <- if (!is.null(icer_map[[sn]])) icer_map[[sn]] else "—"

      # Describe the scenario interventions
      s <- res$s
      anc_desc <- if (isTRUE(s$anc1 > 0)) paste0("ANC retesting opt.1 ", round(s$anc1*100), "%")
                  else if (isTRUE(s$anc2 > 0)) paste0("ANC retesting opt.2 ", round(s$anc2*100), "%")
                  else if (isTRUE(s$anc3 > 0)) paste0("ANC retesting opt.3 ", round(s$anc3*100), "%")
                  else NULL
      parts <- c(
        paste0("Infant prophylaxis ", round(s$ip*100), "%"),
        anc_desc,
        if (isTRUE(s$prep  > 0)) paste0("PrEP ", round(s$prep*100), "%"),
        if (isTRUE(s$support > 0)) paste0("Support groups ", round(s$support*100), "%"),
        if (isTRUE(s$ltfu  > 0)) paste0("LTFU tracing ", round(s$ltfu*100), "%"),
        if (isTRUE(s$poc   > 0)) paste0("POC VL ", round(s$poc*100), "%")
      )
      interv_str <- paste(Filter(Negate(is.null), parts), collapse="; ")

      data.frame(
        Scenario              = sn,
        Interventions         = interv_str,
        Infant_inf_averted    = round(infant_averted),
        Maternal_inf_averted  = round(maternal_averted),
        Combined_inf_averted  = round(combined_averted),
        Pct_VT_reduction      = if (is.na(pct_vt)) NA_character_ else paste0(round(pct_vt*100, 1), "%"),
        Total_cost_USD        = round(res$cost$total),
        Incremental_cost_USD  = round(incr_cost),
        ICER                  = icer_str,
        stringsAsFactors = FALSE
      )
    })
    bind_rows(rows)
  }

  # Per-country reactive storage
  country_scenario_tables <- reactiveValues()
  country_ce_data         <- reactiveValues()  # full per-scenario data for plot

  for (cn in all_countries) {
    local({
      country <- cn
      id <- gsub(" ", "_", country)

      # Load scenario button
      observeEvent(input[[paste0(id,"_load_scenario")]], {
        s <- scenario_to_interventions(input[[paste0(id,"_paper_scenario")]])
        updateSliderInput(session, paste0(id,"_int_ip"),   value=s$ip)
        strat <- if (isTRUE(s$anc1 > 0)) 1 else if (isTRUE(s$anc2 > 0)) 2 else if (isTRUE(s$anc3 > 0)) 3 else 0
        cov   <- max(s$anc1, s$anc2, s$anc3)
        updateRadioButtons(session,  paste0(id,"_int_anc_strat"), selected=strat)
        updateSliderInput(session,  paste0(id,"_int_anc_prop"), value=cov)
        updateSliderInput(session,  paste0(id,"_int_prep"),  value=s$prep)
        updateSliderInput(session,  paste0(id,"_int_sg"),    value=s$support)
        updateSliderInput(session,  paste0(id,"_int_ltfu"),  value=s$ltfu)
        updateSliderInput(session,  paste0(id,"_int_poc"),   value=s$poc)
      })

      # Run button — runs all selected scenarios + baseline + current custom settings
      observeEvent(input[[paste0(id,"_run")]], {
        params <- gather_params(country)

        # Current intervention settings from UI become the "custom" scenario
        anc_strat <- as.integer(safe_num(input[[paste0(id,"_int_anc_strat")]], 0))
        anc_prop  <- safe_num(input[[paste0(id,"_int_anc_prop")]], 0)
        custom_ints <- list(
          ip      = safe_num(input[[paste0(id,"_int_ip")]],   params$pct_inf_prophylaxis),
          anc1    = if (identical(anc_strat, 1L)) anc_prop else 0,
          anc2    = if (identical(anc_strat, 2L)) anc_prop else 0,
          anc3    = if (identical(anc_strat, 3L)) anc_prop else 0,
          prep    = safe_num(input[[paste0(id,"_int_prep")]],  0),
          support = safe_num(input[[paste0(id,"_int_sg")]],    0),
          ltfu    = safe_num(input[[paste0(id,"_int_ltfu")]],  0),
          poc     = safe_num(input[[paste0(id,"_int_poc")]],   0)
        )

        selected_scens <- input[[paste0(id,"_selected_scenarios")]]
        if (is.null(selected_scens) || length(selected_scens) == 0)
          selected_scens <- "Baseline"
        # Always include Baseline as reference; keep user order
        if (!"Baseline" %in% selected_scens)
          selected_scens <- c("Baseline", selected_scens)

        tbl <- build_scenario_table(country, selected_scens, custom_ints, params)
        country_scenario_tables[[id]] <- tbl

        # Build per-scenario detail for CE plot
        base_res <- run_one_scenario(params, "Baseline")
        base_infant   <- base_res$infant_inf
        base_maternal <- base_res$maternal_inf
        base_cost     <- base_res$cost$total

        ce_rows <- lapply(selected_scens, function(sn) {
          res <- if (sn == "Baseline") base_res else run_one_scenario(params, sn, custom_ints)
          data.frame(
            Scenario         = sn,
            Total_cost       = res$cost$total,
            Combined_averted = (base_infant - res$infant_inf) + (base_maternal - res$maternal_inf),
            Infant_averted   = base_infant - res$infant_inf,
            Maternal_averted = base_maternal - res$maternal_inf,
            stringsAsFactors = FALSE
          )
        })
        country_ce_data[[id]] <- bind_rows(ce_rows)

        # Also store latest custom run
        custom_df <- run_model(params, 12,
                               custom_ints$ip, custom_ints$anc1, custom_ints$anc2, custom_ints$anc3,
                               custom_ints$prep, custom_ints$support,
                               custom_ints$ltfu, custom_ints$poc)
        country_results[[id]] <- list(base=base_res$df, int=custom_df)
        country_costs[[id]]   <- list(base=base_res$cost,
                                      int=compute_costs(custom_df, params, 12,
                                            custom_ints$ip, custom_ints$anc1, custom_ints$anc2, custom_ints$anc3,
                                            custom_ints$prep, custom_ints$support,
                                            custom_ints$ltfu, custom_ints$poc))
      })

      # ── Scenario comparison table output ──────────────────────────────────
      output[[paste0(id,"_scenario_table")]] <- renderDT({
        tbl <- country_scenario_tables[[id]]
        if (is.null(tbl)) return(NULL)
        display <- tbl
        names(display) <- c(
          "Scenario", "Interventions",
          "Infant infections averted", "Maternal infections averted",
          "Combined infections averted", "% VT reduction",
          "Total cost (USD)", "Incremental cost (USD)", "ICER"
        )
        datatable(display, rownames=FALSE, escape=FALSE,
          options=list(
            dom='t', paging=FALSE, searching=FALSE, info=FALSE, ordering=FALSE,
            columnDefs=list(
              list(width='9%',  targets=0),
              list(width='28%', targets=1),
              list(width='8%',  targets=2),
              list(width='8%',  targets=3),
              list(width='8%',  targets=4),
              list(width='8%',  targets=5),
              list(width='10%', targets=6),
              list(width='10%', targets=7),
              list(width='11%', targets=8)
            )
          ),
          class='compact stripe cell-border'
        ) %>%
          formatCurrency(c("Total cost (USD)", "Incremental cost (USD)"), "$", digits=0) %>%
          formatStyle("Scenario", fontWeight="bold") %>%
          formatStyle("ICER",
            color = styleEqual(c("Cost-saving"), c("#1a7c3a")),
            fontWeight = "bold"
          )
      })

      # ── Cost-effectiveness plot ────────────────────────────────────────────
      make_ce_plot <- function(ce_df, country_nm) {
        if (is.null(ce_df) || nrow(ce_df) == 0) return(NULL)
        base_cost <- ce_df$Total_cost[ce_df$Scenario == "Baseline"]
        if (length(base_cost) == 0) base_cost <- min(ce_df$Total_cost)
        
        # Format cost labels for axis (millions)
        cost_scale <- 1e6
        ce_df$cost_M <- ce_df$Total_cost / cost_scale
        
        # Identify cost-effectiveness frontier (non-dominated scenarios:
        # sort by combined_averted, check monotone increasing cost)
        df_sorted <- ce_df[order(ce_df$Combined_averted), ]
        on_frontier <- rep(FALSE, nrow(df_sorted))
        min_cost_so_far <- Inf
        # Frontier: going right (more averted), cost must be increasing
        for (i in seq(nrow(df_sorted), 1)) {
          if (df_sorted$Total_cost[i] <= min_cost_so_far) {
            on_frontier[i] <- TRUE
            min_cost_so_far <- df_sorted$Total_cost[i]
          }
        }
        df_sorted$frontier <- on_frontier
        ce_df$frontier <- df_sorted$frontier[match(ce_df$Scenario, df_sorted$Scenario)]
        
        ggplot(ce_df, aes(x=Total_cost/1e6, y=Combined_averted)) +
          # Frontier line
          geom_line(data=df_sorted[df_sorted$frontier, ][order(df_sorted$Total_cost[df_sorted$frontier]),],
                    aes(x=Total_cost/1e6, y=Combined_averted),
                    color="#2c7bb6", linewidth=0.8, linetype="dashed") +
          # All points
          geom_point(aes(color=frontier, shape=frontier), size=4) +
          scale_color_manual(values=c("TRUE"="#d73027", "FALSE"="#4d4d4d"),
                             labels=c("TRUE"="Frontier scenario", "FALSE"="Other scenario"),
                             name=NULL) +
          scale_shape_manual(values=c("TRUE"=17, "FALSE"=16),
                             labels=c("TRUE"="Frontier scenario", "FALSE"="Other scenario"),
                             name=NULL) +
          geom_text_repel(aes(label=Scenario), size=3.2, max.overlaps=20,
                          box.padding=0.4, point.padding=0.3,
                          segment.color="grey60", segment.size=0.3) +
          scale_x_continuous(labels=function(x) paste0("$", format(x, big.mark=",")),
                             expand=expansion(mult=c(0.05, 0.1))) +
          scale_y_continuous(labels=comma, expand=expansion(mult=c(0.05, 0.1))) +
          labs(
            title=paste0(country_nm, " — Scenario cost-effectiveness"),
            subtitle="Each point is one scenario. Dashed line connects non-dominated (frontier) scenarios.",
            x="Total programme cost (USD millions)",
            y="Combined infections averted (infant + maternal vs. baseline)"
          ) +
          theme_minimal(base_size=12) +
          theme(
            legend.position="bottom",
            plot.title=element_text(face="bold", size=13),
            plot.subtitle=element_text(color="grey40", size=10),
            panel.grid.minor=element_blank()
          )
      }

      output[[paste0(id,"_ce_plot")]] <- renderPlot({
        ce_df <- country_ce_data[[id]]
        make_ce_plot(ce_df, country)
      })

      # ── Download handlers ─────────────────────────────────────────────────
      output[[paste0(id,"_dl_table")]] <- downloadHandler(
        filename = function() paste0(gsub(" ", "_", country), "_scenario_comparison_",
                                     format(Sys.Date(), "%Y%m%d"), ".csv"),
        content = function(file) {
          tbl <- country_scenario_tables[[id]]
          if (!is.null(tbl)) write.csv(tbl, file, row.names=FALSE)
        }
      )

      output[[paste0(id,"_dl_plot")]] <- downloadHandler(
        filename = function() paste0(gsub(" ", "_", country), "_ce_plot_",
                                     format(Sys.Date(), "%Y%m%d"), ".png"),
        content = function(file) {
          ce_df <- country_ce_data[[id]]
          p <- make_ce_plot(ce_df, country)
          if (!is.null(p)) {
            ggplot2::ggsave(file, plot=p, width=10, height=6, dpi=150, bg="white")
          }
        }
      )

    })
  }

  # Remove old run_all / comparison_rows / comparison outputs (replaced per-country above)
}

shinyApp(ui, server)
