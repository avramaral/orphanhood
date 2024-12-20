args <- commandArgs(trailingOnly = TRUE)
param1 <- as.numeric(args[1])
param2 <- args[2]

print(paste("MCMC count: ", param1, sep = ""))
print(paste("Type input: ", param2, sep = ""))

source("R/header.R")
source("R/header_orphanhood.R")
source("R/aux.R")
source("R/aux_orphanhood.R")

#############
### FLAGS ###
#############

#############################################
# I must change `MC_count` and `type.input` #
#############################################
MC_count   <- param1 # 1
type.input <- param2 # "Municipality"
#############################################

per1K <- TRUE
compute_prevalence <- ifelse(type.input != "Municipality", TRUE, FALSE)
per_n_children <- ifelse(type.input == "Municipality", 1000, 100000)

yys <- seq(ifelse(compute_prevalence, 2004, 2015), 2021)

# path_mortality <- "DATA/COUNTS/mort_mean.RDS"
# path_fertility <- "DATA/COUNTS/fert_mean.RDS"

path_mortality <- paste("DATA/COUNTS/MC/mort_", MC_count, ".RDS", sep = "")
path_fertility <- paste("DATA/COUNTS/MC/fert_", MC_count, ".RDS", sep = "")

if (type.input == "Municipality") { tmp_loc <- "mun" } else if (type.input == "Department") { tmp_loc <- "dep" } else if (type.input == "Region") { tmp_loc <- "reg" } else if (type.input == "National") { tmp_loc <- "nat" } else { stop("Choose a valid `type.input`.") }

#################
### READ DATA ###
#################

mortality_rates <- readRDS(file = path_mortality) %>% rename(loc = mun)
fertility_rates <- readRDS(file = path_fertility) %>% rename(loc = mun)
valid_muns <- as.numeric(as.character(c(unique(mortality_rates$loc))))

if (per1K) {
  mortality_rates <- mortality_rates %>% mutate(death_rate     = death_rate     * 1000)
  fertility_rates <- fertility_rates %>% mutate(fertility_rate = fertility_rate * 1000)
}

death_count <- mortality_rates %>% dplyr::select(year, loc, gender, age, deaths)

# Used to compute the total number of children, i.e., 0-17
population <- readRDS(file = "DATA/population_all.RDS")
population <- population %>% dplyr::select(year, mun, gender, age, population) %>% mutate(mun = as.numeric(as.character(mun)))
population <- population %>% filter(mun %in% valid_muns) %>% mutate(mun = factor(mun))
population <- population %>% filter(year %in% 1998:2021) %>% arrange(year, mun, gender, age)
population <- population %>% filter(age %in% c("0-9", "10-14", "15-19")) %>% rename(loc = mun)
prop_15_17 <- readRDS(file = "DATA/prop_15_17.RDS") %>% rename(loc = mun)

# Geo information
geo_info <- readRDS(file = "DATA/geo_info.RDS")

# Convert it to the desired resolution
if (type.input != "Municipality") {
  tmp_data <- convert_resolution(geo_info = geo_info, mortality_rates = mortality_rates, fertility_rates = fertility_rates, population = population, death_count = death_count, prop_15_17 = prop_15_17, type.input = type.input, per1K = per1K)

  mortality_rates <- tmp_data$mortality_rates
  fertility_rates <- tmp_data$fertility_rates
  population      <- tmp_data$population
  death_count     <- tmp_data$death_count
  prop_15_17      <- tmp_data$prop_15_17
}

# Add years if computing PREVALENCE
if (compute_prevalence) {
  
  prev_yys <- rev(2004:2014 - 17)
  
  tmp_mortality_rates <- mortality_rates %>% filter(year == (2015 - 17))
  tmp_fertility_rates <- fertility_rates %>% filter(year == (2015 - 17))
  tmp_population      <- population      %>% filter(year == (2015 - 17))
  
  for (prev_yy in prev_yys) {
    
    tmp_mortality_rates$year <- prev_yy
    tmp_fertility_rates$year <- prev_yy
    tmp_population$year      <- prev_yy
    
    mortality_rates <- bind_rows(tmp_mortality_rates, mortality_rates)
    fertility_rates <- bind_rows(tmp_fertility_rates, fertility_rates)
    population      <- bind_rows(tmp_population     , population     )
  }
  
  mortality_rates <- mortality_rates %>% arrange(year, loc, gender, age)
  fertility_rates <- fertility_rates %>% arrange(year, loc, gender, age)
  population      <- population      %>% arrange(year, loc, gender, age)
  
  death_count     <- mortality_rates %>% dplyr::select(year, loc, gender, age, deaths)
  
} else { print("At municipality level, we are not computing prevalence. This is *not* an error.") }

######################
# COMPUTE ORPHANHOOD #
######################

number_children <- list()
number_orphans  <- list()
for (yy in yys) {
  print(paste("Year: ", yy, " (until ", yys[length(yys)], ")", sep = ""))

  print("Processing number of children...")
  number_children[[as.character(yy)]] <- process_number_children_year(yy = yy, type.input = type.input, fertility_rates = fertility_rates, per1K = per1K)

  # Process number of orphans.
  print("Processing number of orphans...")
  number_orphans[[as.character(yy)]] <- process_nb_orphans_table_dep_national_year(yy = yy, type.input = type.input, death_count = death_count, number_children_yy_all = number_children[[as.character(yy)]]$children_all)

  print(paste("Done for year ", yy, ".", sep = ""))
}

# FINAL RESULTS ARE IN `ORPHANHOOD/SUMMARY/`
file_name <- gsub("^.*/|\\..*$", "", path_mortality)
file_name <- gsub("^.*_", "", file_name) %>% toupper()
file_name <- paste("_", file_name, sep = "")
orphan_table_res <- orphan_table(type.input = type.input, population = population, prop_15_17 = prop_15_17, geo_info = geo_info, per_n_children = per_n_children, file_name = file_name, number_orphans = number_orphans)
# saveRDS(object = orphan_table_res, file = paste("ORPHANHOOD/RESULTS/orphan_tables_", tmp_loc, "_per_", as.integer(per_n_children), file_name, ".RDS", sep = ""))

##########################
# INCIDENCE & PREVALENCE #
##########################

# It is better to compute these quantities here, as some intermediate results will be overwritten when fitting the model for different fertility and mortality datasets.

if (type.input == "Municipality") {
  inc_tab <- generate_incidence_table(type.input = "Municipality", per_n_children = per_n_children, geo_info = geo_info, should_round = FALSE, file_name = file_name, orphan_table_res = orphan_table_res)
  saveRDS(object = inc_tab, file = paste("ORPHANHOOD/RESULTS/", tolower(type.input), "_inc_orphans", file_name, ".RDS", sep = ""))
} else if (type.input == "Department") {
  pre_tab <- generate_prevalence_table(type.input = "Department",  per_n_children = per_n_children, geo_info = geo_info, should_round = FALSE, file_name = file_name, number_orphans = number_orphans)
  saveRDS(object = pre_tab, file = paste("ORPHANHOOD/RESULTS/", tolower(type.input), "_pre_orphans", file_name, ".RDS", sep = ""))
} else { print(":)") }
