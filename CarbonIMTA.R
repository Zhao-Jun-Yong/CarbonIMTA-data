# Freshwater tables and figures. Run from the repo root: reads data/*.rds, writes output/.
suppressPackageStartupMessages({
  library(tidyverse)
  library(openxlsx)
  library(ggpubr)
  library(Hmisc)
  library(corrplot)
  library(extrafont)
  library(aelab)
})
dir.create("output", showWarnings = FALSE)

# pdf(NULL) can't load Century Gothic under Rscript
options(ggpubr.null_device = cowplot::png_null_device)
cowplot::set_null_device(cowplot::png_null_device)

for (f in list.files("data", pattern = "\\.rds$", full.names = TRUE)) {
  assign(tools::file_path_sans_ext(basename(f)), readRDS(f))
}

µg_to_mg <- 1 / 1000

lab_co2 <- expression(bold(paste("C", O[2]) ~ "fluxes" ~ "(mg" ~ m^-2 ~ h^-1 * ")"))
lab_ch4_mg <- expression(bold(paste("C", H[4]) ~ "fluxes" ~ "(mg" ~ m^-2 ~ h^-1 * ")"))
lab_n2o <- expression(bold(paste(N[2], "O") ~ "fluxes" ~ "(µg" ~ m^-2 ~ h^-1 * ")"))
lab_temp <- expression(bold(paste("Temperature" ~ "(˚C)")))
lab_ph <- expression(bold(paste("pH")))
lab_ntu <- expression(bold(paste("Turbidity" ~ "(NTU)")))
lab_dic_conc <- expression(bold(paste("DIC" ~ "(mg" ~ L^-1, ")")))
lab_toc_conc <- expression(bold(paste("TOC" ~ "(mg" ~ L^-1, ")")))
lab_tn_conc <- expression(bold(paste("TN" ~ "(mg" ~ L^-1, ")")))
lab_po4_µM <- expression(bold(paste("PO"[4]^"3-", "-P" ~ "(µM)")))
lab_no3_µM <- expression(bold(paste("NO"[3]^"-", "-N" ~ "(µM)")))
lab_no2_µM <- expression(bold(paste("NO"[2]^"-", "-N" ~ "(µM)")))
lab_nh4_µM <- expression(bold(paste("NH"[4]^"+", "-N" ~ "(µM)")))

separate_culture_period <- function(df) {
  lv <- c("DP", "BS", "CP", "AH", "EM", "Other")
  bind_rows(
    df %>% filter(year == "2023") %>%
      mutate(period = case_when(sampling %in% c("2nd", "3rd", "4th", "5th", "6th") ~ "CP",
                                sampling == "1st" ~ "BS", TRUE ~ "Other")),
    df %>% filter(year == "2024") %>%
      mutate(period = case_when(sampling %in% c("2nd", "3rd", "4th", "5th", "6th", "7th", "8th") ~ "CP",
                                sampling == "1st" ~ "BS", sampling == "9th" ~ "DP", TRUE ~ "Other"))
  ) %>% mutate(period = factor(period, levels = lv))
}

# one scope for input and output: boundary flow + pond storage + exchange between ponds (the *_all terms)
budget <- budget %>%
  mutate(cb_input = cb_gpp + cb_ghg_in + cb_feed + cb_inflow_all + cb_stock + cb_rain,
         cb_output = cb_rday + cb_ghg_out + cb_sa + cb_outflow_all + cb_harvest,
         cb_budget = cb_input - cb_output)

# raw "water year name" -> pond code
POND_CODES <- c(
  "fw 2023 fish C1"  = "SB-M1", "fw 2023 fish C2"  = "SB-M2",
  "fw 2023 fish T1"  = "SB-I1", "fw 2023 fish T2"  = "SB-I2",
  "fw 2023 plant C1" = "TI-M1", "fw 2023 plant C2" = "TI-M2",
  "fw 2023 plant T1" = "TD-I1", "fw 2023 plant T2" = "TD-I2",
  "fw 2024 fish C"   = "SB-M2",  "fw 2024 fish T1"  = "SB-I1", "fw 2024 fish T2"  = "SB-I2",
  "fw 2024 plant C"  = "TI-M1",  "fw 2024 plant T1" = "TD-I1", "fw 2024 plant T2" = "TD-I2"
)

code_ponds <- function(df) {
  df %>% mutate(name = unname(POND_CODES[paste(water, year, name)]),
                name_2 = sub("[0-9]$", "", name))
}

df_envr <- envr %>%
  filter(sample == "water", con != "night") %>%
  code_ponds() %>%
  separate_culture_period()

months_since_first <- function(date) {
  (year(date) * 12 + month(date)) - (year(min(date)) * 12 + month(min(date)))
}

df_rda_fw <- raw_budget %>%
  code_ponds() %>%
  mutate(ghg_ch4_mg = ghg_ch4 * µg_to_mg) %>%
  select(year, sampling, name, name_2,
         ghg_co2, ghg_ch4_mg, ghg_n2o, nep, gpp, rday, c_sa, doc, dic, poc,
         envr_temp, envr_ec, envr_do_w, envr_ph, envr_ntu, envr_sal, envr_ss, envr_chla,
         envr_nh4_ugL, envr_no2_ugL, envr_no3_ugL, envr_po4_ugL, dtn, pon)

pond_levels <- c("SB-M1", "SB-M2", "SB-I1", "SB-I2", "TI-M1", "TI-M2", "TD-I1", "TD-I2")

as_num <- function(x) as.numeric(as.character(x))
excel_date <- function(x) as.Date(as_num(x), origin = "1899-12-30")
theme_house <- theme(text = element_text(family = "Century Gothic", color = "black"),
                     strip.text = element_text(face = "bold", color = "black"),
                     axis.text = element_text(face = "bold", color = "black"),
                     legend.text = element_text(face = "bold", color = "black"))
year_fill <- scale_fill_manual(values = c("2023" = "grey60", "2024" = "grey88"), name = NULL)

group_colours <- c("SB-M" = "#0072B2", "SB-I" = "#E69F00", "TI-M" = "#D55E00", "TD-I" = "#009E73")
group_shapes <- c("SB-M" = 21, "SB-I" = 22, "TI-M" = 24, "TD-I" = 25)

pseudo_log10 <- scales::pseudo_log_trans(sigma = 1, base = 10)
zero_line <- geom_hline(yintercept = 0, linewidth = 0.8, colour = "grey40")
flux_label <- function(gas, unit) bquote(bold(.(gas) ~ "flux (" * .(unit) ~ m^-2 ~ h^-1 * ")"))

fw_dates <- info %>%
  transmute(year = as_num(year), species, group,
            stock = excel_date(stock_date), harvest = excel_date(harvest_date))
fw_flux <- ghg_co2e %>%
  code_ponds() %>%
  filter(sample == "water", con %in% c("light", "day")) %>%
  mutate(year = as_num(year), sampling_date = as.Date(sampling_date), name = as.character(name)) %>%
  group_by(year, species, group, name, sampling_date) %>%
  summarise(co2 = mean(ghg_co2, na.rm = TRUE),
            ch4 = mean(ghg_ch4, na.rm = TRUE) / 1000,
            n2o = mean(ghg_n2o, na.rm = TRUE),
            co2e_co2 = mean(co2e_co2, na.rm = TRUE),
            co2e_ch4 = mean(co2e_ch4, na.rm = TRUE),
            co2e_n2o = mean(co2e_n2o, na.rm = TRUE),
            co2e = mean(co2e_ghg, na.rm = TRUE), .groups = "drop") %>%
  left_join(fw_dates, by = c("year", "species", "group")) %>%
  filter(sampling_date <= harvest) %>%
  mutate(days_after_stock = as.numeric(sampling_date - stock),
         pond = factor(name, pond_levels),
         year_f = factor(year),
         pond_group = factor(sub("[0-9]$", "", as.character(pond)), names(group_colours)),
         replicate = paste("Pond", sub(".*([0-9])$", "\\1", as.character(pond))))

fw_flux_cp <- fw_flux %>%
  filter(days_after_stock >= 0) %>%
  mutate(both_years = n_distinct(year) == 2, .by = pond) %>%
  mutate(x_pos = as.numeric(pond) + ifelse(both_years, ifelse(year == 2023, -0.2, 0.2), 0))
pond_x_scale <- scale_x_continuous(breaks = 1:8, labels = pond_levels,
                                   expand = expansion(add = 0.5))

envr_vars <- c("envr_temp", "envr_ec", "envr_do_pc", "envr_do_w", "envr_ph",
               "envr_ntu", "envr_sal", "envr_ss", "envr_chla")
df_envr_avg <- df_envr %>%
  filter(period == "CP") %>%
  group_by(year, sampling_date, sampling, water, name) %>%
  summarise(across(all_of(envr_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

tab_envr <- descriptive_statistic(df_envr_avg, vars = all_of(envr_vars),
                                  groups = c(water, year, name)) %>%
  select(-contains("min"))
colnames(tab_envr) <- gsub("_mean_sd", "", colnames(tab_envr))
write.xlsx(tab_envr, file = "output/Table2_environment.xlsx")

fw_ponds <- function(df, yr) {
  df %>%
    filter(year == yr) %>%
    code_ponds() %>%
    mutate(name = factor(name, unname(POND_CODES[startsWith(names(POND_CODES), paste("fw", yr))])))
}

build_chem <- function(yr) {
  chem_doc <- doc %>%
    filter(source == "water") %>%
    fw_ponds(yr) %>%
    group_by(sampling_date, name) %>%
    summarise(dic = mean(dic, na.rm = TRUE), doc = mean(doc, na.rm = TRUE),
              dtn = mean(tn, na.rm = TRUE), .groups = "drop")
  chem_poc <- poc %>%
    filter(source == "water") %>%
    fw_ponds(yr) %>%
    group_by(sampling_date, name) %>%
    summarise(poc = mean(poc, na.rm = TRUE), pon = mean(pon, na.rm = TRUE), .groups = "drop")
  chem_nutrient <- nutrient %>%
    fw_ponds(yr) %>%
    group_by(sampling_date, name, var = tolower(nutrient)) %>%
    summarise(value = mean(conc_µM, na.rm = TRUE), .groups = "drop")
  df_chem <- full_join(chem_doc, chem_poc, by = join_by(sampling_date, name)) %>%
    transmute(sampling_date, name, dic, toc = doc + poc, tn = dtn + pon) %>%
    pivot_longer(c(dic, toc, tn), names_to = "var") %>%
    bind_rows(chem_nutrient) %>%
    filter(!is.na(value)) %>%
    mutate(sampling_date = as.Date(sampling_date),
           months_after_bs = months_since_first(sampling_date),
           period = ifelse(months_after_bs == 0, "BS", paste0("A", months_after_bs, "M")),
           period = droplevels(factor(period, levels = c("BS", paste0("A", 1:12, "M")))),
           var = factor(var, levels = c("dic", "toc", "tn", "nh4", "no2", "no3", "po4")))
  df_chem
}

chem_table <- function(df_chem, ...) {
  tab <- df_chem %>%
    filter(period != "BS") %>%
    pivot_wider(id_cols = c(sampling_date, name), names_from = var, values_from = value) %>%
    descriptive_statistic(vars = c(...), groups = c(name)) %>%
    select(-contains("min"))
  colnames(tab) <- gsub("_mean_sd", "", colnames(tab))
  tab
}

df_chem <- build_chem("2024")
df_chem_2023 <- build_chem("2023")

tab_water_chem <- chem_table(df_chem, dic, toc, tn, nh4, no2, no3, po4)
write.xlsx(tab_water_chem, file = "output/Table3_water_chem.xlsx")
tab_water_chem_2023 <- chem_table(df_chem_2023, dic, toc, tn)
write.xlsx(tab_water_chem_2023, file = "output/TableS1_water_chem_2023.xlsx")

tab_s6 <- map(c("2023", "2024"), function(yr) {
  full_join(
    doc %>% filter(source == "water") %>% fw_ponds(yr) %>%
      summarise(doc = mean(doc, na.rm = TRUE), dtn = mean(tn, na.rm = TRUE), .by = c(sampling_date, name)),
    poc %>% filter(source == "water") %>% fw_ponds(yr) %>%
      summarise(poc = mean(poc, na.rm = TRUE), ptn = mean(pon, na.rm = TRUE), .by = c(sampling_date, name)),
    by = join_by(sampling_date, name)) %>%
    mutate(sampling_date = as.Date(sampling_date)) %>%
    filter(months_since_first(sampling_date) > 0) %>%
    descriptive_statistic(vars = c(doc, poc, dtn, ptn), groups = c(name)) %>%
    select(-contains("min")) %>%
    mutate(year = yr, .before = 1)
}) %>% bind_rows()
colnames(tab_s6) <- gsub("_mean_sd", "", colnames(tab_s6))
write.xlsx(tab_s6, file = "output/TableS6_water_chem_orgC_N.xlsx")

theme_ghg_fw <- theme_bw(base_size = 36) + theme_house +
  theme(strip.background = element_blank(), legend.key = element_blank(),
        axis.title = element_text(face = "bold", size = rel(0.85)))
strip_text <- function(show) if (show) element_text(face = "bold") else element_blank()

fw_line_panel <- function(gas, ylab, y_axis, bottom = FALSE, strip = TRUE) {
  ggplot(fw_flux, aes(days_after_stock, .data[[gas]], colour = pond_group,
                      shape = pond_group, linetype = replicate, group = pond)) +
    zero_line +
    geom_line(linewidth = 1) +
    geom_point(size = 3, stroke = 1, fill = "white") +
    facet_grid(. ~ year) +
    scale_colour_manual(values = group_colours, name = NULL) +
    scale_shape_manual(values = group_shapes, name = NULL) +
    scale_linetype_manual(values = c("Pond 1" = "solid", "Pond 2" = "22"), name = NULL) +
    guides(colour = guide_legend(order = 1), shape = guide_legend(order = 1),
           linetype = guide_legend(order = 2)) +
    theme_ghg_fw +
    theme(legend.key.width = unit(2, "cm"), strip.text = strip_text(strip)) +
    labs(x = if (bottom) "Days after stocking" else NULL, y = ylab) +
    y_axis
}
fw_box_panel <- function(gas, y_axis, bottom = FALSE, strip = TRUE) {
  pond_text <- if (bottom) element_text(angle = 90, hjust = 1, vjust = 0.5) else element_blank()
  plot_box(fw_flux_cp, x = pond, y = .data[[gas]], z = pond_group, facet = TRUE,
           facet_x = "year", base_size = 36, line_width = 0.8, outlier_size = 2.5) +
    zero_line +
    facet_grid(. ~ year, scales = "free_x", space = "free_x") +
    scale_fill_manual(values = group_colours, guide = "none") +
    xlab(NULL) +
    ylab(NULL) +
    theme_house +
    theme(legend.position = "none", axis.text.y = element_blank(),
          axis.ticks.y = element_blank(), strip.text = strip_text(strip),
          axis.text.x = pond_text) +
    y_axis
}

co2_axis <- coord_cartesian(ylim = c(-120, 420))

ch4_breaks <- c(0, 1, 3, 10, 30, 100)
ch4_line_axis <- list(scale_y_continuous(trans = pseudo_log10, breaks = ch4_breaks),
                      coord_cartesian(ylim = c(0, 200)))
ch4_box_axis <- list(coord_transform(y = pseudo_log10, ylim = c(0, 200)),
                     scale_y_continuous(breaks = ch4_breaks))
n2o_axis <- coord_cartesian(ylim = c(-25, 240))

fig1_ghg <- patchwork::wrap_plots(
  fw_line_panel("co2", flux_label(quote("CO"[2]), "mg"), co2_axis),
  fw_box_panel("co2", co2_axis),
  fw_line_panel("ch4", flux_label(quote("CH"[4]), "mg"), ch4_line_axis, strip = FALSE),
  fw_box_panel("ch4", ch4_box_axis, strip = FALSE),
  fw_line_panel("n2o", flux_label(quote("N"[2] * "O"), "µg"), n2o_axis,
                  bottom = TRUE, strip = FALSE),
  fw_box_panel("n2o", n2o_axis, bottom = TRUE, strip = FALSE),
  ncol = 2, widths = c(2, 1.3), guides = "collect") +
  patchwork::plot_annotation(tag_levels = list(c("A", "", "B", "", "C", ""))) &
  theme(legend.position = "bottom",
        plot.tag = element_text(size = 40, face = "bold", family = "Century Gothic"))
png("output/Fig1_ghg_flux.png", width = 22, height = 20, units = "in", res = 300)
print(fig1_ghg)
invisible(dev.off())

fmt_s2 <- function(x) sprintf("%.2f ± %.2f (%.2f [%.2f–%.2f])", mean(x), sd(x), median(x), min(x), max(x))
table_s2 <- fw_flux_cp %>%
  mutate(pond = as.character(pond)) %>%
  summarise(n = n(), CO2 = fmt_s2(co2), CH4 = fmt_s2(ch4), N2O = fmt_s2(n2o), CO2e = fmt_s2(co2e),
            share = sprintf("%.0f / %.0f / %.0f", mean(co2e_co2) / mean(co2e) * 100,
                            mean(co2e_ch4) / mean(co2e) * 100, mean(co2e_n2o) / mean(co2e) * 100),
            .by = c(year, pond)) %>%
  arrange(year, pond)

write.csv(table_s2, "output/TableS2_ghg_flux.csv", row.names = FALSE)

ebul_closures <- ghg_zb_flux %>%
  filter(gas == "CH4", con %in% c("light", "day"), date != as.Date("2023-06-03")) %>%
  mutate(year = format(date, "%Y"), species = sub(" .*", "", pond), group = sub(".* ", "", pond),
         ch4_total = flux / 1000,
         event = ch4_class != "linear",
         usable = ch4_class == "bubbly" & (split_ok %in% TRUE | ch4_split_flag %in% "ok"),
         ebul_frac = if_else(usable, ebul_ch4 * flux_per_slope / 1000 / ch4_total, NA_real_),
         ch4_diff = if_else(usable, diff_ch4 * flux_per_slope / 1000, ch4_total),
         pond = factor(unname(POND_CODES[paste("fw", year, pond)]), pond_levels)) %>%
  left_join(mutate(fw_dates, year = as.character(year)), by = c("year", "species", "group")) %>%
  filter(date >= stock, date <= harvest)
ebul_q <- function(x, p) if (all(is.na(x))) NA_real_ else unname(quantile(x, p, na.rm = TRUE))
ebul_summary <- function(d) summarise(d, closures = n(), events = sum(event),
  events_pct = 100 * mean(event), flux_in_events_pct = 100 * sum(ch4_total[event]) / sum(ch4_total),
  splits_usable = sum(usable), ebul_pct_median = 100 * ebul_q(ebul_frac, .5),
  ebul_pct_q25 = 100 * ebul_q(ebul_frac, .25), ebul_pct_q75 = 100 * ebul_q(ebul_frac, .75))

ebul_pond_mean <- ebul_closures %>%
  summarise(all = mean(ch4_total), noev = mean(ch4_diff), unsep = sum(ch4_total[event & !usable]) / n(),
            .by = c(year, pond, date)) %>%
  summarise(mean_flux = mean(all), mean_flux_without_ebullition = mean(noev), unsep_flux_pct = 100 * mean(unsep) / mean(all),
            .by = c(year, pond)) %>%
  mutate(pond = as.character(pond), flux_ratio = mean_flux / mean_flux_without_ebullition)
ebul_ch4 <- bind_rows(
  ebul_closures %>% group_by(year, pond = as.character(pond)) %>% ebul_summary() %>%
    left_join(ebul_pond_mean, by = c("year", "pond")),
  ebul_closures %>% group_by(year) %>% ebul_summary() %>% mutate(pond = "all"),
  ebul_closures %>% group_by(pond = if_else(str_detect(as.character(pond), "^SB"), "all Sea Bass", "all Tilapia")) %>%
    ebul_summary() %>% mutate(year = "both"),
  ebul_closures %>% ebul_summary() %>% mutate(year = "both", pond = "all")) %>%
  select(year, pond, everything()) %>%
  arrange(year == "both", year, pond == "all", pond)

table_s3 <- ebul_ch4 %>%
  ungroup() %>%
  transmute(Year = if_else(year == "both", "Both", year), Pond = sub("^all ", "", pond),
            `Measurements (n)` = closures, `Ebullition events (n)` = events,
            `Occurrence (%)` = sprintf("%.0f", events_pct), `Separable (n)` = splits_usable,
            `Contribution of ebullition (median [IQR])` = if_else(is.na(ebul_pct_median), "–",
              sprintf("%.0f [%.0f–%.0f]", ebul_pct_median, ebul_pct_q25, ebul_pct_q75)),
            `Average flux (mg m-2 h-1)` = if_else(is.na(mean_flux), "–", sprintf("%.2f", mean_flux)),
            `Average flux without ebullition (mg m-2 h-1)` =
              if_else(is.na(mean_flux_without_ebullition), "–", sprintf("%.2f", mean_flux_without_ebullition)),
            Ratio = if_else(is.na(flux_ratio), "–", sprintf("%.1f", flux_ratio)))

write.csv(table_s3, "output/TableS3_ebullition.csv", row.names = FALSE)

df_ghg_soil <- ghg %>%
  filter(year == "2024", sample %in% c("soil", "water"), species != "plant") %>%
  separate_culture_period() %>%
  code_ponds() %>%
  mutate(ghg_ch4_mg = ghg_ch4 * µg_to_mg,
         water = factor(water, labels = "Freshwater"))

df_ghg_soil_avg <- df_ghg_soil %>%
  group_by(year, sampling_date, sampling, water, period, name, sample) %>%
  summarise(across(c(ghg_co2, ghg_ch4_mg, ghg_n2o), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

no_x <- theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
gas_box_panel <- function(df, y, z, ylab, ylim) {
  plot_box(df, x = water, y = {{ y }}, z = {{ z }}) +
    xlab(NULL) +
    ylab(ylab) +
    see::scale_fill_okabeito() +
    coord_cartesian(ylim = ylim) +
    no_x
}
save_three_panels <- function(file, plots) {
  png(filename = file, width = 21, height = 7, units = "in", res = 300)
  print(ggarrange(plotlist = plots, ncol = 3, nrow = 1, labels = c("A", "B", "C"),
                  font.label = list(size = 20, family = "Century Gothic"),
                  common.legend = T, legend = "bottom"))
  invisible(dev.off())
}

soil_panels <- list(
  gas_box_panel(df_ghg_soil_avg, ghg_co2, period, lab_co2, c(-250, 750)),
  gas_box_panel(df_ghg_soil_avg, ghg_ch4_mg, period, lab_ch4_mg, c(-1, 300)),
  gas_box_panel(df_ghg_soil_avg, ghg_n2o, period, lab_n2o, c(-200, 400)))
save_three_panels("output/FigS5_ghg_flux_drainage.png", soil_panels)

diurnal_campaign <- function(df) {
  df %>%
    filter(year == "2024", sample == "water", sampling %in% c("7th", "8th")) %>%
    mutate(water = factor(water, labels = "Freshwater"),
           con = factor(con, levels = c("day", "night"), labels = c("Day", "Night")))
}

df_ghg_diurnal <- ghg %>%
  diurnal_campaign() %>%
  mutate(ghg_ch4_mg = ghg_ch4 * µg_to_mg) %>%
  group_by(water, sampling, name, con) %>%
  summarise(across(c(ghg_co2, ghg_ch4, ghg_ch4_mg, ghg_n2o), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

diurnal_panels <- list(
  gas_box_panel(df_ghg_diurnal, ghg_co2, con, lab_co2, c(-600, 600)),
  gas_box_panel(df_ghg_diurnal, ghg_ch4_mg, con, lab_ch4_mg, c(-0.01, 40)),
  gas_box_panel(df_ghg_diurnal, ghg_n2o, con, lab_n2o, c(-200, 400)))
save_three_panels("output/FigS3_diurnal.png", diurnal_panels)

gas_points <- fw_flux_cp %>%
  select(x_pos, pond, year_f, co2e_co2, co2e_ch4, co2e_n2o) %>%
  pivot_longer(starts_with("co2e_"), names_to = "gas", values_to = "flux") %>%
  mutate(gas = sub("co2e_", "", gas),
         x_gas = x_pos + c(co2 = -0.09, ch4 = 0, n2o = 0.09)[gas])
gas_shapes <- c(co2 = 16, ch4 = 17, n2o = 18)

co2e_means <- fw_flux_cp %>% summarise(co2e = mean(co2e), .by = c(x_pos, pond, year_f))
gas_jitter <- position_jitter(width = 0.03, height = 0, seed = 1)

fig2_co2e <- ggplot(fw_flux_cp, aes(x_pos, co2e)) +
  geom_vline(xintercept = c(2.5, 4.5, 6.5), colour = "grey85") +
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = 2, colour = "grey50") +
  geom_boxplot(aes(group = interaction(pond, year_f), fill = year_f), width = 0.35,
               outlier.shape = 16, outlier.size = 2, linewidth = 0.5, key_glyph = "polygon") +
  geom_point(data = gas_points, aes(x_gas, flux), shape = gas_shapes[gas_points$gas],
             colour = "grey20", size = 3.4, position = gas_jitter) +
  geom_point(data = gas_points, aes(x_gas, flux, colour = gas), shape = gas_shapes[gas_points$gas],
             size = 2.6, alpha = 0.9, position = gas_jitter) +
  geom_point(data = co2e_means, aes(x_pos, co2e, shape = "Mean"),
             size = 3, stroke = 1, fill = "white") +
  annotate("text", x = c(1.5, 3.5, 5.5, 7.5), y = 150, label = c("SB-M", "SB-I", "TI-M", "TD-I"),
           fontface = "bold", family = "Century Gothic", size = 7) +
  scale_colour_manual(values = c(co2 = "#e76f51", ch4 = "#ffbe0b", n2o = "#729aba"),
                      breaks = c("co2", "ch4", "n2o"),
                      labels = c(co2 = bquote(bold("CO"[2])), ch4 = bquote(bold("CH"[4])),
                                 n2o = bquote(bold("N"[2] * "O"))),
                      name = NULL) +
  scale_shape_manual(values = c(Mean = 22), name = NULL) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 1),
                     breaks = c(-1, 0, 1, 3, 10, 30, 100),
                     labels = scales::label_number(style_negative = "minus")) +
  pond_x_scale +
  year_fill +
  theme_classic(base_size = 25) +
  theme_house +
  labs(x = NULL, y = expression(bold("CO"[2] * "e flux (g CO"[2] * "e" ~ m^-2 ~ d^-1 * ")"))) +
  guides(fill = guide_legend(order = 1),
         shape = guide_legend(order = 2, override.aes = list(size = 5)),
         colour = guide_legend(order = 3,
                               override.aes = list(size = 5, shape = unname(gas_shapes)))) +
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.key.size = unit(0.9, "cm"), legend.spacing.x = unit(0.3, "cm"),
        legend.box.spacing = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 0, hjust = 0.5))
png("output/Fig2_co2e_flux.png", width = 14, height = 9, units = "in", res = 300)
print(fig2_co2e)
invisible(dev.off())

fw_years <- function(df) {
  df %>%
    mutate(year = as_num(year), sampling_date = as.Date(sampling_date), name = as.character(name)) %>%
    code_ponds()
}
wq_mean <- function(df, ...) {
  df %>%
    group_by(year, species, group, name, sampling_date) %>%
    summarise(..., .groups = "drop")
}

wq_envr <- envr %>%
  filter(sample == "water", con != "night") %>%
  fw_years() %>%
  wq_mean(across(c(envr_temp, envr_do_w, envr_ph, envr_ec, envr_ntu, envr_ss, envr_chla),
                 ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(starts_with("envr_"), names_to = "var")

wq_carbon <- full_join(
  doc %>% filter(source == "water") %>% fw_years() %>%
    wq_mean(dic = mean(dic, na.rm = TRUE), doc = mean(doc, na.rm = TRUE),
            dtn = mean(tn, na.rm = TRUE)),
  poc %>% filter(source == "water") %>% fw_years() %>%
    wq_mean(poc = mean(poc, na.rm = TRUE), pon = mean(pon, na.rm = TRUE)),
  by = c("year", "species", "group", "name", "sampling_date")) %>%
  transmute(year, species, group, name, sampling_date, dic, toc = doc + poc, tn = dtn + pon) %>%
  pivot_longer(c(dic, toc, tn), names_to = "var")
wq_nutrient <- nutrient %>%
  fw_years() %>%
  mutate(var = tolower(nutrient)) %>%
  group_by(year, species, group, name, sampling_date, var) %>%
  summarise(value = mean(conc_µM, na.rm = TRUE), .groups = "drop")

wq_labels <- list(envr_temp = lab_temp,
                  envr_do_w = expression(bold("DO (mg" ~ L^-1 * ")")),
                  envr_ph = lab_ph,
                  envr_ec = expression(bold("EC (µS" ~ cm^-1 * ")")),
                  envr_ntu = lab_ntu,
                  envr_ss = expression(bold("SS (mg" ~ L^-1 * ")")),
                  envr_chla = expression(bold("Chl" ~ italic("a") ~ "(µg" ~ L^-1 * ")")),
                  dic = lab_dic_conc, toc = lab_toc_conc,
                  tn = lab_tn_conc, nh4 = lab_nh4_µM, no2 = lab_no2_µM, no3 = lab_no3_µM,
                  po4 = lab_po4_µM)
df_wq <- bind_rows(wq_envr, wq_carbon, wq_nutrient) %>%
  filter(!is.na(value)) %>%
  left_join(fw_dates, by = c("year", "species", "group")) %>%
  filter(sampling_date <= harvest) %>%
  mutate(days_after_stock = as.numeric(sampling_date - stock),
         pond = factor(name, pond_levels),
         pond_group = factor(sub("[0-9]$", "", as.character(pond)), names(group_colours)),
         replicate = paste("Pond", sub(".*([0-9])$", "\\1", as.character(pond))),
         year = factor(year, c(2023, 2024)))

wq_panel <- function(v, bottom = FALSE) {
  d <- filter(df_wq, var == v)
  p <- ggplot(d, aes(days_after_stock, value, colour = pond_group, shape = pond_group,
                     linetype = replicate, group = pond)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3, stroke = 1, fill = "white") +
    facet_grid(. ~ year, drop = FALSE) +
    scale_x_continuous(limits = range(df_wq$days_after_stock)) +
    scale_colour_manual(values = group_colours, name = NULL) +
    scale_shape_manual(values = group_shapes, name = NULL) +
    scale_linetype_manual(values = c("Pond 1" = "solid", "Pond 2" = "22"), name = NULL) +
    guides(colour = guide_legend(order = 1), shape = guide_legend(order = 1),
           linetype = guide_legend(order = 2)) +
    theme_bw(base_size = 25) +
    theme_house +
    theme(strip.background = element_blank(), legend.key = element_blank(),
          legend.key.width = unit(2, "cm")) +
    labs(x = if (bottom) "Days after stocking" else NULL, y = wq_labels[[v]])
  if (!"2023" %in% d$year) {
    label_y <- if (v == "nh4") 40 else mean(range(d$value))
    p <- p + geom_text(data = data.frame(year = factor(2023, c(2023, 2024))),
                       aes(x = mean(range(df_wq$days_after_stock)), y = label_y),
                       label = "Not measured", inherit.aes = FALSE,
                       family = "Century Gothic", fontface = "bold", size = 7, colour = "grey50")
  }
  if (v == "nh4") {
    p <- p + scale_y_continuous(trans = pseudo_log10, breaks = c(0, 1, 3, 10, 30, 100, 300, 1000))
  }
  p
}
wq_fig <- lapply(seq_along(wq_labels), function(i) {
  wq_panel(names(wq_labels)[i], bottom = i > length(wq_labels) - 2)
})

png(filename = "output/FigS2_water_quality.png",
    width = 24, height = 32, units = "in", res = 300)
ggarrange(plotlist = wq_fig, ncol = 2, nrow = 7,
          labels = LETTERS[seq_along(wq_fig)],
          font.label = list(size = 26, family = "Century Gothic"),
          common.legend = T, legend = "bottom") |>
  print()
invisible(dev.off())

df_co2 <- pco2 %>%
  mutate(pond = factor(name), date = factor(paste(year, sampling)), dpco2_k = dpco2 / 1000) %>%
  group_by(pond) %>%
  mutate(dp_between = mean(dpco2_k, na.rm = TRUE), dp_within = dpco2_k - dp_between,
         do_between = mean(envr_do_w), do_within = envr_do_w - do_between) %>%
  ungroup()
df_co2_air <- filter(df_co2, !is.na(dpco2))

m_co2_dp <- lmerTest::lmer(ghg_co2 ~ dp_within + dp_between + year + (1 | pond) + (1 | date), data = df_co2_air)

pco2_fig_df <- df_co2 %>%
  left_join(distinct(fw_dates, year = as.character(year), species, stock), by = c("year", "species")) %>%
  mutate(days_after_stock = as.numeric(sampling_date - stock),
         pond = factor(name, pond_levels),
         pond_group = factor(sub("[0-9]$", "", name), names(group_colours)),
         replicate = paste("Pond", sub(".*([0-9])$", "\\1", name)))

pco2_air_band <- pco2_fig_df %>%
  summarise(air = mean(pco2_air), .by = c(year, days_after_stock)) %>%
  filter(!is.na(air))
pco2_aes <- aes(colour = pond_group, shape = pond_group)
pco2_scales <- list(scale_colour_manual(values = group_colours, name = NULL),
                    scale_shape_manual(values = group_shapes, name = NULL),
                    theme_ghg_fw)
no_key <- guides(colour = "none", shape = "none")
fe <- lme4::fixef(m_co2_dp)
pco2_fit <- tibble(dp_within = seq(min(df_co2_air$dp_within), max(df_co2_air$dp_within), length.out = 100))
pco2_x <- cbind(1, pco2_fit$dp_within, mean(df_co2_air$dp_between), mean(df_co2_air$year == "2024"))

pco2_se <- sqrt(rowSums((pco2_x %*% as.matrix(vcov(m_co2_dp))) * pco2_x))
pco2_fit <- mutate(pco2_fit, ghg_co2 = drop(pco2_x %*% fe),
                   lo = ghg_co2 - 1.96 * pco2_se, hi = ghg_co2 + 1.96 * pco2_se)

pco2_a <- ggplot(pco2_fig_df, aes(days_after_stock, pco2)) +
  geom_ribbon(data = pco2_air_band, aes(y = air, ymin = 0.8 * air, ymax = 1.2 * air), fill = "grey85") +
  geom_line(data = pco2_air_band, aes(y = air), linewidth = 1, colour = "grey40") +
  geom_line(aes(colour = pond_group, linetype = replicate, group = pond), linewidth = 1) +
  geom_point(pco2_aes, size = 3, stroke = 1, fill = "white") +
  facet_grid(. ~ year) +
  scale_y_log10(breaks = c(100, 300, 1000, 3000, 10000)) +
  scale_linetype_manual(values = c("Pond 1" = "solid", "Pond 2" = "22"), name = NULL) +
  guides(colour = guide_legend(order = 1), shape = guide_legend(order = 1),
         linetype = guide_legend(order = 2)) +
  pco2_scales +
  theme(legend.key.width = unit(2, "cm")) +
  labs(x = "Days after stocking", y = expression(bold(italic(p) * "CO"[2] ~ "(µatm, log scale)")))
pco2_b <- ggplot(df_co2_air %>% mutate(pond_group = factor(sub("[0-9]$", "", name), names(group_colours))),
                 aes(dp_within, ghg_co2)) +
  zero_line +
  geom_ribbon(data = pco2_fit, aes(ymin = lo, ymax = hi), fill = "grey85") +
  geom_point(pco2_aes, size = 3, stroke = 1, fill = "white") +
  geom_line(data = pco2_fit, linewidth = 1) +
  pco2_scales + no_key +
  labs(x = expression(bold("Within-pond" ~ Delta * italic(p) * "CO"[2] ~ "(1000 µatm)")),
       y = flux_label(quote("CO"[2]), "mg"))
pco2_c <- ggplot(pco2_fig_df, aes(nep, ghg_co2)) +
  zero_line +
  geom_point(pco2_aes, size = 3, stroke = 1, fill = "white") +
  pco2_scales + no_key +
  labs(x = expression(bold("NEP (g O"[2] ~ m^-2 ~ d^-1 * ")")), y = NULL)

png("output/FigS6_pco2.png", width = 22, height = 18, units = "in", res = 300)
print(patchwork::wrap_plots(A = pco2_a, B = pco2_b, C = pco2_c, design = "AA\nBC", guides = "collect") +
        patchwork::plot_annotation(tag_levels = "A") &
        theme(legend.position = "bottom",
              plot.tag = element_text(size = 40, face = "bold", family = "Century Gothic")))
invisible(dev.off())

pct_of <- function(x, total) ifelse(total != 0, x / total * 100, NA)

df_budget <- budget %>%
  select(year, water, name, contains("cb")) %>%
  code_ponds() %>%
  mutate(con_rday = pct_of(cb_rday, cb_output),
         con_ghg_out = pct_of(cb_ghg_out, cb_output),
         con_sa = pct_of(cb_sa, cb_output),
         con_outflow = pct_of(cb_outflow_all, cb_output),
         con_harvest = pct_of(cb_harvest, cb_output),
         con_gpp = pct_of(cb_gpp, cb_input),
         con_ghg_in = pct_of(cb_ghg_in, cb_input),
         con_feed = pct_of(cb_feed, cb_input),
         con_inflow = pct_of(cb_inflow_all, cb_input),
         con_stock = pct_of(cb_stock, cb_input),
         con_rain = pct_of(cb_rain, cb_input),
         pin_rday = pct_of(cb_rday, cb_input),
         pin_ch4_out = pct_of(cb_ghg_out, cb_input),
         pin_sa = pct_of(cb_sa, cb_input),
         pin_outflow = pct_of(cb_outflow_all, cb_input),
         pin_harvest = pct_of(cb_harvest, cb_input),
         pin_output = pct_of(cb_output, cb_input))

budget_out <- df_budget %>%
  group_by(year, water, name) %>%
  summarise(across(c(cb_rday, cb_ghg_out, cb_sa, cb_outflow_all, cb_harvest, cb_output),
                   ~ round(mean(.x), 2)))
write.xlsx(budget_out, file = "output/TableS5_budget_output.xlsx")

budget_in <- df_budget %>%
  group_by(year, water, name) %>%
  summarise(across(c(cb_gpp, cb_ghg_in, cb_feed, cb_inflow_all, cb_rain, cb_stock, cb_input),
                   ~ round(mean(.x), 2)))
write.xlsx(budget_in, file = "output/TableS4_budget_input.xlsx")

fw_budget <- budget %>%
  filter(species %in% c("fish", "plant")) %>%
  code_ponds() %>%
  mutate(pond = factor(as.character(name), rev(pond_levels)),
         year = as.character(year))

budget_long <- bind_rows(
  fw_budget %>%
    transmute(year, pond, direction = "Input",
              `Gross primary production` = cb_gpp, `Feed input` = cb_feed,
              `Inflow water` = cb_inflow_all, Rainwater = cb_rain,
              `Stocked biomass` = cb_stock, `Methane uptake` = cb_ghg_in) %>%
    pivot_longer(-c(year, pond, direction), names_to = "pathway"),
  fw_budget %>%
    transmute(year, pond, direction = "Output",
              `Ecosystem respiration` = cb_rday, `Sediment accumulation` = cb_sa,
              `Outflow water` = cb_outflow_all, `Harvested biomass` = cb_harvest,
              `Methane emission` = cb_ghg_out) %>%
    pivot_longer(-c(year, pond, direction), names_to = "pathway")) %>%
  mutate(value = coalesce(value, 0),
         con = 100 * value / sum(value), .by = c(year, pond, direction))

pathway_colours <- c(`Gross primary production` = "#009E73", `Feed input` = "#E69F00",
                     `Water (inflow / outflow)` = "#0072B2", Rainwater = "#56B4E9",
                     `Biomass (stocked / harvested)` = "#CC79A7",
                     `Ecosystem respiration` = "#D55E00", `Sediment accumulation` = "#999999",
                     `Methane (uptake / emission)` = "#ffbe0b")
pathway_labels <- setNames(as.list(names(pathway_colours)), names(pathway_colours))
pathway_labels$`Methane (uptake / emission)` <- expression(bold("CH"[4] ~ "(uptake / emission)"))
budget_long <- budget_long %>%
  mutate(pathway = forcats::fct_collapse(
           pathway,
           `Water (inflow / outflow)` = c("Inflow water", "Outflow water"),
           `Biomass (stocked / harvested)` = c("Stocked biomass", "Harvested biomass"),
           `Methane (uptake / emission)` = c("Methane uptake", "Methane emission")),
         pathway = factor(pathway, names(pathway_colours)))

CLIP <- 200
fig4_levels <- c(setdiff(names(pathway_colours), "Sediment accumulation"), "Sediment accumulation")
input_total <- budget_long %>% filter(direction == "Input") %>%
  summarise(input = sum(value), .by = c(year, pond))
fig4_long <- budget_long %>%
  left_join(input_total, by = c("year", "pond")) %>%
  mutate(con = 100 * value / input,
         pathway = factor(pathway, fig4_levels))

fig4_totals <- fig4_long %>%
  summarise(total = sum(value), pct = sum(con), .by = c(year, pond, direction))
fig4_plot <- fig4_long %>%
  mutate(other = sum(con[pathway != "Sediment accumulation"]), .by = c(year, pond, direction)) %>%
  mutate(con = ifelse(direction == "Output" & pathway == "Sediment accumulation",
                      pmin(con, pmax(CLIP - other, 0)), con))

panel_range <- data.frame(direction = c("Input", "Output"), x = c(100, CLIP),
                          year = "2023", pond = factor("SB-M1", rev(pond_levels)))
label_x <- c(Input = 101, Output = CLIP + 1)
fig4_totals <- fig4_totals %>%
  mutate(x = label_x[direction],
         label = ifelse(direction == "Input", sprintf("%.1f", total),
                        sprintf("%.1f (%.0f%%)", total, pct)))

fig4_budget <- plot_bar(fig4_plot, x = con, y = pond, z = pathway,
                        position = position_stack(reverse = TRUE), line_width = 0.5,
                        facet = TRUE, facet_y = "year", facet_x = "direction", style = "classic") +
  geom_vline(data = data.frame(direction = "Output", year = c("2023", "2024")), aes(xintercept = 100),
             linewidth = 1.4, linetype = 3, inherit.aes = FALSE) +
  geom_blank(data = panel_range, aes(x = x, y = pond), inherit.aes = FALSE) +
  geom_text(data = fig4_totals, aes(x = x, y = pond, label = label),
            hjust = 0, size = 5.4, family = "Century Gothic", fontface = "bold",
            inherit.aes = FALSE) +
  facet_grid(year ~ direction, scales = "free", space = "free_y") +
  scale_fill_manual(values = pathway_colours, labels = pathway_labels, name = NULL) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = function(l) seq(0, l[2], by = ifelse(l[2] > 100, 100, 25))) +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(ncol = 3)) +
  theme(legend.position = "bottom", panel.spacing.x = unit(3.2, "cm"),
        strip.text.y = element_text(margin = margin(l = 215)),
        plot.margin = margin(10, 60, 10, 10)) +
  ylab(NULL) +
  xlab(expression(bold("Contribution (% of total input)")))
png("output/Fig4_carbon_budget.png", width = 17, height = 14, units = "in", res = 300)
print(fig4_budget)
invisible(dev.off())

# corrplot with filled-circle significance marks instead of asterisks
corrplot <- local({
  txt <- paste(deparse(corrplot::corrplot, width.cutoff = 500L), collapse = "\n")
  swap <- function(from, to) {
    stopifnot(grepl(from, txt, fixed = TRUE))
    txt <<- gsub(from, to, txt, fixed = TRUE)
  }
  swap("function(sig.locs, point)", "function(sig.locs, pch_value, pch_col)")
  swap("text(pos.pNew[, 1][sig.locs], pos.pNew[, 2][sig.locs], labels = point, col = pch.col, cex = pch.cex, lwd = 2)",
       "points(pos.pNew[, 1][sig.locs], pos.pNew[, 2][sig.locs], pch = pch_value, bg = pch_col, cex = pch.cex)")
  swap('pch = "*"', 'pch_symbols <- c(21, 21, 21)\npch_colors <- c("white", "grey50", "black")')
  swap("point = pch)", "pch_value = pch_symbols[1], pch_col = pch_colors[1])")
  swap('pchTmp = paste(rep(pch, i), collapse = "")', "pchTmp = pch_symbols[i]\npchColTmp = pch_colors[i]")
  swap("point = pchTmp)", "pch_value = pchTmp, pch_col = pchColTmp)")
  swap("ind1 = which(col == colRange[1])", "ind1 = which(col == colRange[1])[1]")
  swap("ind2 = which(col == colRange[2])", "ind2 = which(col == colRange[2])[1]")
  eval(parse(text = txt), asNamespace("corrplot"))
})

cor_vars <- c("ghg_co2", "ghg_ch4_mg", "ghg_n2o", "nep", "gpp", "rday", "c_sa", "doc", "dic", "poc",
              "envr_temp", "envr_ec", "envr_sal", "envr_do_w", "envr_ph",
              "envr_ntu", "envr_ss", "envr_chla",
              "envr_nh4_ugL", "envr_no2_ugL", "envr_no3_ugL", "envr_po4_ugL", "dtn", "pon")
cor_labels <- c("$CO[2]", "$CH[4]", "$N[2]*O", "NEP", "GPP", "ER", "SA", "DOC", "DIC", "POC",
                "Temp", "EC", "Sal", "DO", "pH", "NTU", "SS", "Chla",
                "$NH[4]^\"+\"", "$NO[2]^\"-\"", "$NO[3]^\"-\"", "$PO[4]^\"3-\"",
                "DTN", "PTN")
m <- df_rda_fw %>% select(all_of(cor_vars)) %>% as.matrix()
colnames(m) <- cor_labels
results_cor_fw <- rcorr(m, type = "spearman")
diag(results_cor_fw$P) <- 0
results_cor_fw$P[is.na(results_cor_fw$P)] <- 1

cor_pal <- colorRampPalette(c("#56B4E9", "white", "#E69F00"))(500)
cor_stars <- list(col = cor_pal, insig = "label_sig", sig.level = c(0.001, 0.01, 0.05),
                  pch.cex = 1.5, pch.col = "black", cl.pos = "b", tl.pos = "d", method = "color")

png(filename = "output/FigS4_spearman.png",
    width = 10, height = 10, units = "in", res = 300)
do.call(corrplot, c(list(results_cor_fw$r, type = "upper", tl.col = "black",
                         p.mat = results_cor_fw$P), cor_stars))
par(family = "Arial Black")
corrplot(results_cor_fw$r, add = TRUE, method = "number", type = "lower", diag = FALSE,
         tl.pos = "n", cl.pos = "n", col = cor_pal, number.cex = 0.75, number.font = 1,
         p.mat = results_cor_fw$P, sig.level = 0.05, insig = "blank")
invisible(dev.off())

library(vegan)
set.seed(123)

df_rda_fw$species <- factor(ifelse(grepl("^SB", df_rda_fw$name_2), "sea_bass", "tilapia"))

df_rda_fw$pond <- factor(dplyr::recode(as.character(df_rda_fw$name),
                                       "SB-M" = "SB-M2", "TI-M" = "TI-M1"))
df_rda_fw$year <- droplevels(factor(df_rda_fw$year))
df_rda_fw$pond_year <- factor(paste(df_rda_fw$pond, df_rda_fw$year))

var_labels <- c(
  "doc" = "DOC", "dic" = "DIC", "poc" = "POC",
  "envr_temp" = "Temperature", "envr_ec" = "EC", "envr_sal" = "Salinity",
  "envr_do_w" = "DO", "envr_ph" = "pH",
  "envr_ntu" = "Turbidity", "envr_ss" = "SS", "envr_chla" = "Chl-a",
  "dtn" = "DTN", "pon" = "PTN"
)
candidate_fw <- c("doc", "dic", "poc", "envr_temp", "envr_ec", "envr_do_w",
                  "envr_ph", "envr_ntu", "envr_ss", "envr_chla", "dtn", "pon")

ghg_vars <- c("ghg_co2", "ghg_ch4_mg", "ghg_n2o")
cond <- "Condition(pond + year)"
complete_rows <- complete.cases(df_rda_fw[, c(ghg_vars, "pond", "year", candidate_fw)])
Y <- scale(df_rda_fw[complete_rows, ghg_vars])
X <- df_rda_fw[complete_rows, c("pond", "year", candidate_fw)]
X$pond <- droplevels(X$pond)
X$year <- droplevels(X$year)
perm_design <- how(blocks = factor(df_rda_fw$pond_year[complete_rows]), nperm = 9999)
upper_mod <- rda(as.formula(paste("Y ~", paste(candidate_fw, collapse = " + "), "+", cond)), data = X)
# the global test also draws permutations; keep it so the selection below is unchanged
invisible(anova.cca(upper_mod, permutations = perm_design))
selected <- ordiR2step(rda(as.formula(paste("Y ~", cond)), data = X), scope = formula(upper_mod),
                       permutations = perm_design, trace = FALSE)
predictors_fw <- setdiff(attr(terms(formula(selected)), "term.labels"), cond)
partial_rda_fw <- rda(as.formula(paste("Y ~", paste(predictors_fw, collapse = " + "), "+", cond)), data = X)

groups_fw_partial <- df_rda_fw$name_2[complete_rows]
rda_group_colours <- c("SB-M" = "#E69F00", "SB-I" = "#E69F00",
                   "TI-M" = "#56B4E9", "TD-I" = "#56B4E9")
rda_group_shapes <- c("SB-M" = 21, "SB-I" = 24, "TI-M" = 21, "TD-I" = 24)

axis_importance_fw <- summary(partial_rda_fw)$cont$importance

axis_names_fw <- colnames(axis_importance_fw)[1:2]
axis_pct_fw <- round(axis_importance_fw[2, 1:2] * 100, 1)

site_scores_fw <- scores(partial_rda_fw, display = "sites", scaling = 2)

png(filename = "output/Fig3_prda.png",
    width = 12, height = 10, units = "in", res = 300)

par(mar = c(5, 5, 4, 2), family = "Century Gothic")
par(font.axis = 2)

plot(partial_rda_fw, type = "n", scaling = 2,
     xlab = bquote(bold(.(axis_names_fw[1]) ~ "(") * .(axis_pct_fw[[1]]) * bold("%)")),
     ylab = bquote(bold(.(axis_names_fw[2]) ~ "(") * .(axis_pct_fw[[2]]) * bold("%)")),
     cex.lab = 1.3, cex.axis = 1.1)

species_fill_fw   <- c(sea_bass = adjustcolor("#E69F00", 0.10),
                       tilapia  = adjustcolor("#56B4E9", 0.10))
species_border_fw <- c(sea_bass = "#E69F00", tilapia = "#56B4E9")

species_fw_partial <- df_rda_fw$species[complete_rows]
ordiellipse(partial_rda_fw, groups = species_fw_partial, scaling = 2,
            kind = "sd", draw = "polygon", col = species_fill_fw, border = NA)
ordiellipse(partial_rda_fw, groups = species_fw_partial, scaling = 2,
            kind = "sd", draw = "lines", col = species_border_fw, lwd = 2)

for (group in names(rda_group_colours)) {
  group_idx <- which(groups_fw_partial == group)
  points(site_scores_fw[group_idx, , drop = FALSE],
         pch = rda_group_shapes[group], cex = 1.8, lwd = 1.5,
         bg = rda_group_colours[group], col = "black")
}

gas_label_xy <- scores(partial_rda_fw, display = "species", scaling = 2) * 1.9
gas_labels <- expression(CO[2], CH[4], N[2]*O)

halo_x <- strwidth("o", cex = 1.3) / 7
halo_y <- strheight("o", cex = 1.3) / 7
for (k in seq(0, 2 * pi, length.out = 13)[-13]) {
  text(gas_label_xy[, 1] + cos(k) * halo_x, gas_label_xy[, 2] + sin(k) * halo_y,
       labels = gas_labels, col = "white", cex = 1.3, font = 2)
}
text(gas_label_xy[, 1], gas_label_xy[, 2], labels = gas_labels, col = "black", cex = 1.3, font = 2)

predictor_arrows <- scores(partial_rda_fw, display = "bp")[predictors_fw, , drop = FALSE]

predictor_arrows <- predictor_arrows * ordiArrowMul(partial_rda_fw, display = "bp")
arrows(0, 0, predictor_arrows[, 1], predictor_arrows[, 2], length = 0.1, col = "grey20", lwd = 3)
text(predictor_arrows[, 1] * 1.2, predictor_arrows[, 2] * 1.2,
     var_labels[rownames(predictor_arrows)], col = "grey20", cex = 1, font = 2)

legend("topright",
       legend = c("Sea bass monoculture (SB-M)", "Sea bass integrated (SB-I)",
                  "Tilapia monoculture (TI-M)", "Tilapia+duckweed integrated (TD-I)",
                  "Species group (1 SD)"),
       pch    = c(21, 24, 21, 24, 22),
       pt.bg  = c("#E69F00", "#E69F00", "#56B4E9", "#56B4E9", adjustcolor("grey60", 0.3)),
       pt.cex = c(1.8, 1.8, 1.8, 1.8, 2.2),
       col    = c("black", "black", "black", "black", "grey50"),
       pt.lwd = 1.5, cex = 1, bty = "n", text.font = 2)

invisible(dev.off())
