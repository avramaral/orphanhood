source("R/header.R")
source("R/header_plotting.R")
source("R/aux.R")
source("R/aux_plot.R")

# Check `R/aux_plot.R` for the functions used to generate the plots

#################
### READ DATA ###
#################

file_name <- "_MEAN"

breaks_count <- c(0, 5, 10, 50, 100, 1000, 15001)
labels_count <- c("0-4", "5-9", "10-49", "50-99", "100-999", "1,000-15,000")

breaks_rate <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 3.0)
labels_rate <- c("[0.0%, 0.2%)", "[0.2%, 0.4%)", "[0.4%, 0.6%)", "[0.6%, 0.8%)", "[0.8%, 1.0%)", "[1.0%, 3.0%)")

breaks_fert_rate <- c(0, 0.02, 0.04, 0.06, 0.08, 0.10, 0.20)
labels_fert_rate <- c("[0.00, 0.02)", "[0.02, 0.04)", "[0.04, 0.06)", "[0.06, 0.08)", "[0.08, 0.10)", "[0.10, 0.20)")

breaks_mort_rate <- c(0, 0.004, 0.005, 0.00525, 0.00550, 0.00575, 0.020)
labels_mort_rate <- c("[0.00000, 0.00400)", "[0.00400, 0.00500)", "[0.00500, 0.00525)", "[0.00525, 0.00550)", "[0.00550, 0.00575)", "[0.00575, 0.02000)")

### MPI ###

data <- readRDS(file = "DATA/mortality_bias_data.RDS")

mpi_info <- data$mort %>% dplyr::select(mun, mpi) %>% distinct()
colombia <- data$colombia

geo_info <- readRDS(file = "DATA/geo_info.RDS")
geo_info <- geo_info %>% mutate(dep_name = ifelse(dep_name == "Archipelago de San Andrés, Providencia y Santa Catalina", "Arch. de SA, Prov. y St. Cat.", dep_name))

mpi_info <- mpi_info %>% left_join(y = colombia, by = "mun")

# Filter out islands for MPI
isl1_mpi <- mpi_info %>% filter( (mun %in% c(88001)))
isl2_mpi <- mpi_info %>% filter( (mun %in% c(88564)))
mpi_info <- mpi_info %>% filter(!(mun %in% c(88001, 88564)))

### INCIDENCE DATA ###

inc_orphans_count_raw <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/municipality_inc_orphans_abs", file_name, ".csv", sep = ""))
inc_orphans_count <- inc_orphans_count_raw %>% filter(year == 2021) %>% dplyr::select(-c(mun_name, year)) %>% group_by(mun) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% mutate(mun = factor(mun))
inc_orphans_count <- inc_orphans_count %>% mutate(n_orp_category = cut(n_orp, breaks = breaks_count, labels = labels_count, include.lowest = TRUE)) %>% mutate(n_orp_category = factor(n_orp_category))
inc_orphans_count <- inc_orphans_count %>% left_join(y = colombia, by = "mun")

inc_orphans_rate_raw <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/municipality_inc_orphans_per_1000", file_name, ".csv", sep = ""))
inc_orphans_rate <- inc_orphans_rate_raw %>% filter(year == 2021) %>% dplyr::select(-c(mun_name, year)) %>% group_by(mun) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% mutate(mun = factor(mun))
inc_orphans_rate <- inc_orphans_rate %>% mutate(n_orp = n_orp / 1000 * 100)
inc_orphans_rate <- inc_orphans_rate %>% mutate(n_orp_category = cut(n_orp, breaks = breaks_rate, labels = labels_rate, include.lowest = TRUE)) %>% mutate(n_orp_category = factor(n_orp_category))
inc_orphans_rate <- inc_orphans_rate %>% left_join(y = colombia, by = "mun")

### PREVALENCE DATA ###

pre_orphans_count_raw <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/department_pre_orphans_abs", file_name, ".csv", sep = ""))
pre_orphans_count <- pre_orphans_count_raw %>% group_by(dep) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% mutate(dep = factor(dep))
pre_orphans_count <- pre_orphans_count %>% left_join(y = distinct(geo_info[, c("dep", "dep_name", "reg_name")]))
pre_orphans_count <- pre_orphans_count %>% mutate(dep_name = factor(dep_name), reg_name = factor(reg_name))
pre_orphans_count <- pre_orphans_count %>% mutate(dep_name = factor(dep_name, levels = dep_name[order(n_orp, decreasing = TRUE)]))

pre_orphans_rate_raw <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/department_pre_orphans_per_100000", file_name, ".csv", sep = ""))
pre_orphans_rate <- pre_orphans_rate_raw %>% group_by(dep) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% mutate(dep = factor(dep))
pre_orphans_rate <- pre_orphans_rate %>% mutate(n_orp = n_orp / 100000)
pre_orphans_rate <- pre_orphans_rate %>% left_join(y = distinct(geo_info[, c("dep", "dep_name", "reg_name")]))
pre_orphans_rate <- pre_orphans_rate %>% mutate(dep_name = factor(dep_name), reg_name = factor(reg_name))
pre_orphans_rate <- pre_orphans_rate %>% mutate(dep_name = factor(dep_name, levels = dep_name[order(n_orp, decreasing = TRUE)]))

### Fertility and death rates ###

# Fertility
p_fert <- "fertility_v1.stan"
data_fert <- readRDS(file = "DATA/fertility_bias_data.RDS")
raw_births <- data_fert$fert

fit_births <- readRDS(paste("FITTED/DATA/count_", strsplit(p_fert, "\\.")[[1]][1], ".RDS", sep = ""))
fit_births <- fit_births %>% as_tibble() %>% rename(mun = Location, gender = Gender, year = Year, age = Age, births = Mean) %>% mutate(mun = factor(mun))
fit_births <- fit_births %>% left_join(y = data_fert$fert[, c("mun", "gender", "year", "age", "population")], by = c("mun", "gender", "year", "age"))
fit_births <- fit_births %>% mutate(fertility_rate = compute_rate(count = births, pop = population))
fit_births <- fit_births %>% dplyr::select(year, mun, gender, age, births, population, fertility_rate) %>% arrange(year, mun, gender, age)
fit_births <- fit_births %>% rename(fit_births = births)

fit_births_std <- fit_births %>% dplyr::select(year, mun, gender, age, fit_births) %>% rename(births = fit_births) %>% 
                                 arrange(year, mun, gender, age) %>% 
                                 left_join(y = raw_births[, c("year", "mun", "gender", "age", "population")], by = c("year", "mun", "gender", "age")) %>%
                                 mutate(births = ifelse(population == 0, 0, births)) %>% 
                                 mutate(fertility_rate = compute_rate(count = births, pop = population))

fit_births_std_2021 <- compute_std_rate(fit_births_std) %>% filter(year == 2021)
fit_births_std_2021 <- fit_births_std_2021 %>% mutate(std_rate_cat = cut(std_rate, breaks = breaks_fert_rate, labels = labels_fert_rate, include.lowest = TRUE)) %>% mutate(std_rate_cat = factor(std_rate_cat))
fit_births_std_2021 <- fit_births_std_2021 %>% left_join(y = colombia, by = "mun")

# Mortality
p_mort <- "mortality_v1.stan"
data_mort <- readRDS(file = "DATA/mortality_bias_data.RDS")
raw_deaths <- data_mort$mort

fit_deaths <- readRDS(paste("FITTED/DATA/count_", strsplit(p_mort, "\\.")[[1]][1], ".RDS", sep = ""))
fit_deaths <- fit_deaths %>% as_tibble() %>% rename(mun = Location, gender = Gender, year = Year, age = Age, deaths = Mean) %>% mutate(mun = factor(mun))
fit_deaths <- fit_deaths %>% left_join(y = data_mort$mort[, c("mun", "gender", "year", "age", "population")], by = c("mun", "gender", "year", "age"))
fit_deaths <- fit_deaths %>% mutate(death_rate = compute_rate(count = deaths, pop = population))
fit_deaths <- fit_deaths %>% dplyr::select(year, mun, gender, age, deaths, population, death_rate) %>% arrange(year, mun, gender, age)
fit_deaths <- fit_deaths %>% rename(fit_deaths = deaths)

fit_deaths_std <- fit_deaths %>% dplyr::select(year, mun, gender, age, fit_deaths) %>% rename(deaths = fit_deaths) %>% 
                                 arrange(year, mun, gender, age) %>% 
                                 left_join(y = raw_deaths[, c("year", "mun", "gender", "age", "population")], by = c("year", "mun", "gender", "age")) %>%
                                 mutate(deaths = ifelse(population == 0, 0, deaths)) %>% 
                                 mutate(mortality_rate = compute_rate(count = deaths, pop = population))

fit_deaths_std_2021 <- compute_std_rate(fit_deaths_std) %>% filter(year == 2021)
fit_deaths_std_2021 <- fit_deaths_std_2021 %>% mutate(std_rate_cat = cut(std_rate, breaks = breaks_mort_rate, labels = labels_mort_rate, include.lowest = TRUE)) %>% mutate(std_rate_cat = factor(std_rate_cat))
fit_deaths_std_2021 <- fit_deaths_std_2021 %>% left_join(y = colombia, by = "mun")

################################################################################
################################################################################

#################
### FIGURE 01 ###
#################

# Missing uncertainty bars for panel_1.1

inc_nat <- inc_orphans_count_raw %>% group_by(gender, year) %>% summarise(n_orp = sum(n_orp), .groups = "drop") %>% dplyr::select(gender, year, n_orp)
inc_nat_total <- inc_nat %>%  group_by(year) %>% summarise(n_orp = sum(n_orp), gender = "Total", .groups = "drop") %>% dplyr::select(gender, year, n_orp)
inc_nat_total <- bind_rows(inc_nat, inc_nat_total)

panel_1.1 <- plot_incidence_line(data = inc_nat_total, my_var = "n_orp")
panel_1.2 <- plot_orphans_prev(data = pre_orphans_count, nm_y = "Orphanhood prevalence in 2021 by Department\n(Total)",                         tt = NULL, is_count = TRUE )
panel_1.3 <- plot_orphans_prev(data = pre_orphans_rate,  nm_y = "Orphanhood prevalence in 2021 by Department\n(% of children aged 0-17 years)", tt = NULL, is_count = FALSE)

FIG1 <- panel_1.1 / ((panel_1.2 | panel_1.3) + plot_layout(guides = "collect")) + plot_layout(heights = c(0.75, 1))
ggsave(filename = paste("IMAGES/PAPER/FIG1.jpeg", sep = ""), plot = FIG1, width = 3000, height = 2200, units = c("px"), dpi = 300, bg = "white")

#################
### FIGURE 02 ###
#################

# Filter out islands 
inc_orphans_rate_isl1 <- inc_orphans_rate %>% filter( (mun %in% c(88001)))
inc_orphans_rate_isl2 <- inc_orphans_rate %>% filter( (mun %in% c(88564)))
inc_orphans_rate_main <- inc_orphans_rate %>% filter(!(mun %in% c(88001, 88564)))

fit_births_std_2021_isl1 <- fit_births_std_2021 %>% filter( (mun %in% c(88001)))
fit_births_std_2021_isl2 <- fit_births_std_2021 %>% filter( (mun %in% c(88564)))
fit_births_std_2021_main <- fit_births_std_2021 %>% filter(!(mun %in% c(88001, 88564)))

fit_deaths_std_2021_isl1 <- fit_deaths_std_2021 %>% filter( (mun %in% c(88001)))
fit_deaths_std_2021_isl2 <- fit_deaths_std_2021 %>% filter( (mun %in% c(88564)))
fit_deaths_std_2021_main <- fit_deaths_std_2021 %>% filter(!(mun %in% c(88001, 88564)))

panel_2.1 <- plot_maps_isl(data = mpi_info, isl1 = isl1_mpi, isl2 = isl2_mpi, my_var = "mpi", tt = "", nm_var = "MPI", ll = c(0, 100))
panel_2.2 <- plot_cat_map_isl(data = inc_orphans_rate_main, isl1 = inc_orphans_rate_isl1, isl2 = inc_orphans_rate_isl2, my_var = "n_orp_category", labels = labels_rate,  nm_var = "Orphanhood incidence in 2021\n(% of children aged 0-17 years)", tt = NULL)
panel_2.3 <- plot_cat_map_isl(data = fit_births_std_2021_main, isl1 = fit_births_std_2021_isl1, isl2 = fit_births_std_2021_isl2, my_var = "std_rate_cat", labels = labels_fert_rate,  nm_var = "Standardised fertility\nrate in 2021", tt = NULL)
panel_2.4 <- plot_cat_map_isl(data = fit_deaths_std_2021_main, isl1 = fit_deaths_std_2021_isl1, isl2 = fit_deaths_std_2021_isl2, my_var = "std_rate_cat", labels = labels_mort_rate,  nm_var = "Standardised mortality\nrate in 2021", tt = NULL)

FIG2 <- (panel_2.1 | panel_2.2) / (panel_2.3 | panel_2.4) + plot_layout(heights = c(1, 1))
ggsave(filename = paste("IMAGES/PAPER/FIG2.jpeg", sep = ""), plot = FIG2, width = 3000, height = 3000, units = c("px"), dpi = 300, bg = "white")

################
### TABLE 01 ###
################

### INCIDENCE ###
# COUNT
inc_orphans_count_dep <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/department_inc_orphans_abs", file_name, ".csv", sep = ""))
inc_orphans_count_reg <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/region_inc_orphans_abs",     file_name, ".csv", sep = ""))
inc_orphans_count_nat <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/national_inc_orphans_abs",   file_name, ".csv", sep = ""))
inc_orphans_count_dep_2021 <- inc_orphans_count_dep %>% filter(year == 2021) %>% dplyr::select(-c(dep, year)) %>% pivot_wider(names_from = gender, values_from = n_orp) %>% mutate(Total = Female + Male) %>% arrange(dep_name)
inc_orphans_count_reg_2021 <- inc_orphans_count_reg %>% filter(year == 2021) %>% dplyr::select(-c(reg, year)) %>% pivot_wider(names_from = gender, values_from = n_orp) %>% mutate(Total = Female + Male) %>% arrange(reg_name)
inc_orphans_count_nat_2021 <- inc_orphans_count_nat %>% filter(year == 2021) %>% dplyr::select(-c(nat, year)) %>% pivot_wider(names_from = gender, values_from = n_orp) %>% mutate(Total = Female + Male) %>% arrange(nat_name)
# RATE
inc_orphans_rate_dep <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/department_inc_orphans_per_100000", file_name, ".csv", sep = ""))
inc_orphans_rate_reg <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/region_inc_orphans_per_100000",     file_name, ".csv", sep = ""))
inc_orphans_rate_nat <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/national_inc_orphans_per_100000",   file_name, ".csv", sep = ""))
inc_orphans_rate_dep_2021 <- inc_orphans_rate_dep %>% filter(year == 2021) %>% dplyr::select(-c(dep, year)) %>% pivot_wider(names_from = gender, values_from = n_orp) %>% mutate(Total = Female + Male) %>% mutate(Female = Female / 1000, Male = Male / 1000, Total = Total / 1000) %>% rename("Female (%)" = Female, "Male (%)" = Male, "Total (%)" = Total) %>% arrange(dep_name)
inc_orphans_rate_reg_2021 <- inc_orphans_rate_reg %>% filter(year == 2021) %>% dplyr::select(-c(reg, year)) %>% pivot_wider(names_from = gender, values_from = n_orp) %>% mutate(Total = Female + Male) %>% mutate(Female = Female / 1000, Male = Male / 1000, Total = Total / 1000) %>% rename("Female (%)" = Female, "Male (%)" = Male, "Total (%)" = Total) %>% arrange(reg_name)
inc_orphans_rate_nat_2021 <- inc_orphans_rate_nat %>% filter(year == 2021) %>% dplyr::select(-c(nat, year)) %>% pivot_wider(names_from = gender, values_from = n_orp) %>% mutate(Total = Female + Male) %>% mutate(Female = Female / 1000, Male = Male / 1000, Total = Total / 1000) %>% rename("Female (%)" = Female, "Male (%)" = Male, "Total (%)" = Total) %>% arrange(nat_name)

inc_orphans_dep_2021 <- bind_cols(inc_orphans_count_dep_2021, inc_orphans_rate_dep_2021[, 2:4]) %>% rename(Location = dep_name)
inc_orphans_reg_2021 <- bind_cols(inc_orphans_count_reg_2021, inc_orphans_rate_reg_2021[, 2:4]) %>% rename(Location = reg_name)
inc_orphans_nat_2021 <- bind_cols(inc_orphans_count_nat_2021, inc_orphans_rate_nat_2021[, 2:4]) %>% rename(Location = nat_name)

# FINAL TABLE FOR INCIDENCE:
inc_orphas_2021 <- bind_cols(as_tibble(data.frame(Level = c("National", rep("Region", 6), rep("Department", 33)))), bind_rows(inc_orphans_nat_2021, inc_orphans_reg_2021, inc_orphans_dep_2021))

### PREVALENCE ###
# COUNT
pre_orphans_count_dep <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/department_pre_orphans_abs", file_name, ".csv", sep = ""))
pre_orphans_count_reg <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/region_pre_orphans_abs",     file_name, ".csv", sep = ""))
pre_orphans_count_nat <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/national_pre_orphans_abs",   file_name, ".csv", sep = ""))
pre_orphans_count_dep_2021 <- pre_orphans_count_dep %>% dplyr::select(-c(dep)) %>% pivot_wider(names_from = gender, values_from = n_orp) %>% mutate(Total = Female + Male) %>% arrange(dep_name)
pre_orphans_count_reg_2021 <- pre_orphans_count_reg %>% dplyr::select(-c(reg)) %>% pivot_wider(names_from = gender, values_from = n_orp) %>% mutate(Total = Female + Male) %>% arrange(reg_name)
pre_orphans_count_nat_2021 <- pre_orphans_count_nat %>% dplyr::select(-c(nat)) %>% pivot_wider(names_from = gender, values_from = n_orp) %>% mutate(Total = Female + Male) %>% arrange(nat_name)
# RATE
pre_orphans_rate_dep <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/department_pre_orphans_per_100000", file_name, ".csv", sep = ""))
pre_orphans_rate_reg <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/region_pre_orphans_per_100000",     file_name, ".csv", sep = ""))
pre_orphans_rate_nat <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/national_pre_orphans_per_100000",   file_name, ".csv", sep = ""))
pre_orphans_rate_dep_2021 <- pre_orphans_rate_dep %>% dplyr::select(-c(dep)) %>% pivot_wider(names_from = gender, values_from = n_orp) %>% mutate(Total = Female + Male) %>% mutate(Female = Female / 1000, Male = Male / 1000, Total = Total / 1000) %>% rename("Female (%)" = Female, "Male (%)" = Male, "Total (%)" = Total) %>% arrange(dep_name)
pre_orphans_rate_reg_2021 <- pre_orphans_rate_reg %>% dplyr::select(-c(reg)) %>% pivot_wider(names_from = gender, values_from = n_orp) %>% mutate(Total = Female + Male) %>% mutate(Female = Female / 1000, Male = Male / 1000, Total = Total / 1000) %>% rename("Female (%)" = Female, "Male (%)" = Male, "Total (%)" = Total) %>% arrange(reg_name)
pre_orphans_rate_nat_2021 <- pre_orphans_rate_nat %>% dplyr::select(-c(nat)) %>% pivot_wider(names_from = gender, values_from = n_orp) %>% mutate(Total = Female + Male) %>% mutate(Female = Female / 1000, Male = Male / 1000, Total = Total / 1000) %>% rename("Female (%)" = Female, "Male (%)" = Male, "Total (%)" = Total) %>% arrange(nat_name)

pre_orphans_dep_2021 <- bind_cols(pre_orphans_count_dep_2021, pre_orphans_rate_dep_2021[, 2:4]) %>% rename(Location = dep_name)
pre_orphans_reg_2021 <- bind_cols(pre_orphans_count_reg_2021, pre_orphans_rate_reg_2021[, 2:4]) %>% rename(Location = reg_name)
pre_orphans_nat_2021 <- bind_cols(pre_orphans_count_nat_2021, pre_orphans_rate_nat_2021[, 2:4]) %>% rename(Location = nat_name)

# FINAL TABLE FOR PREVALENCE:
pre_orphas_2021 <- bind_cols(as_tibble(data.frame(Level = c("National", rep("Region", 6), rep("Department", 33)))), bind_rows(pre_orphans_nat_2021, pre_orphans_reg_2021, pre_orphans_dep_2021))

### RESULTS ###
inc_orphas_2021 <- inc_orphas_2021 %>% rename("Inc. Female"  = "Female", "Inc. Male"  = "Male", "Inc. Total"  = "Total", "Inc. Female (%)"  = "Female (%)", "Inc. Male (%)"  = "Male (%)", "Inc. Total (%)"  = "Total (%)")
pre_orphas_2021 <- pre_orphas_2021 %>% rename("Prev. Female" = "Female", "Prev. Male" = "Male", "Prev. Total" = "Total", "Prev. Female (%)" = "Female (%)", "Prev. Male (%)" = "Male (%)", "Prev. Total (%)" = "Total (%)")
orphans_2021 <- bind_cols(inc_orphas_2021, pre_orphas_2021[3:8])
orphans_2021

write_csv(x = orphans_2021, file = paste("IMAGES/PAPER/TABLE1.csv", sep = ""))

################
### TABLE 02 ###
################

# First, notice that prevalence is **not** computed at the municipality level
# For this table, we do not have the credible intervals yet
# All in 2021

ranked_mun <- tibble("Rank" = 1:20, "Inc. location" = rep("", 20), "Inc. orphans" = rep(0, 20), "Inc. (%) location" = rep("", 20), "Inc. (%) orphans" = rep(0, 20))
ranked_inc_count <- inc_orphans_count_raw %>% filter(year == 2021) %>% pivot_wider(names_from = gender, values_from = n_orp) %>% mutate(Total = Female + Male) %>% arrange(desc(Total)) %>% dplyr::select(mun_name, Total)
ranked_inc_count <- ranked_inc_abs[1:20, ]
ranked_inc_rate  <- inc_orphans_rate_raw  %>% filter(year == 2021) %>% pivot_wider(names_from = gender, values_from = n_orp) %>% mutate(Total = Female + Male) %>% mutate(Female = Female / 10, Male = Male / 10, Total = Total / 10) %>% arrange(desc(Total)) %>% dplyr::select(mun_name, Total)
ranked_inc_rate  <- ranked_inc_rate[1:20, ]

ranked_mun[, c("Inc. location", "Inc. orphans")]         <- ranked_inc_count 
ranked_mun[, c("Inc. (%) location", "Inc. (%) orphans")] <- ranked_inc_rate 

ranked_mun
write_csv(x = ranked_mun, file = paste("IMAGES/PAPER/TABLE2.csv", sep = ""))
