### Load packages, define setup and graphics theme ----
source("02_code/00_packages_setup_theme.R") 

### Data from Our Wold in Data ----

# Main dataset with covid-19 related variables
owid_covid <- 
  read_csv(str_c("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")) %>%
  clean_names()

# Data on excess mortality
owid_em <- 
  read_csv(str_c("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/excess_mortality/excess_mortality.csv")) %>%
  clean_names()


### Data from Mexico's Health Ministry ----

## Daily data ----

# Import data
mex_covid <- 
  read_csv(str_c("../../../10 recursos/datos/ssa/dg_epidemiologia/covid_19/210301COVID19MEXICO.csv"), 
           locale = locale(encoding = "latin1"), 
           col_types = cols(.default = "c")) %>% 
  # "Clean" names
  clean_names() 

# Change variables types ----
mex_covid <- 
  mex_covid %>%
  # To date
  mutate_at(.vars = c("fecha_actualizacion", "fecha_ingreso", "fecha_sintomas", "fecha_def"), 
            as_date) %>% 
  # To numeric
  mutate_at(.vars = c("origen", "sector", "sexo", "tipo_paciente", "intubado", "neumonia", "edad", "nacionalidad", "embarazo", "habla_lengua_indig", "diabetes", "epoc", "asma", "inmusupr", "hipertension", "otra_com", "cardiovascular", "obesidad", "renal_cronica", "tabaquismo", "otro_caso", "resultado_lab", "migrante", "pais_nacionalidad", "pais_origen", "uci"), 
            as.numeric)

# Several changes necessary given the modifications made by the Health Ministry to the database through time ----
mex_covid <- 
  mex_covid %>% 
  # Rename column
  rename(resultado = clasificacion_final) %>% 
  # Change colum type
  mutate(resultado = as.numeric(resultado)) %>% 
  # Recode values
  mutate(resultado = case_when(resultado == 1 ~ 1,
                               resultado == 2 ~ 1,
                               resultado == 3 ~ 1))

# Create tibble with daily number of deaths ----
mex_deaths <- 
  mex_covid %>% 
  mutate(dummy_death = ifelse(!is.na(fecha_def) & resultado == 1, 1, 0)) %>% 
  filter(dummy_death == 1) %>% 
  group_by(fecha_def) %>% 
  summarise(num_deaths = n()) %>% 
  ungroup() 

# Remove mex_covid from Global Environment
rm(mex_covid)


### Excess mortality ----

# https://raw.githubusercontent.com/TheEconomist/covid-19-excess-deaths-tracker/master/output-data/interactive/interactive_table.csv and https://coronavirus.gob.mx/exceso-de-mortalidad-en-mexico; the last source was consulted on Feb 27, 2020.

mex_ex_mor <- 
  tibble(covid_deaths = 126507, # Source: https://youtu.be/y-mBbyPdAe8
         excess_deaths = 326609) # Source: https://coronavirus.gob.mx/exceso-de-mortalidad-en-mexico, consulted on Feb 27, 2020

### Data from The Economist ---

# Excess mortality ----
eco_ex_mor <- 
  read_csv(str_c("https://raw.githubusercontent.com/TheEconomist/covid-19-excess-deaths-tracker/master/output-data/interactive/interactive_table.csv")) %>%
  clean_names()

# Update Mexico's data
eco_ex_mor <- 
  eco_ex_mor %>% 
  # Update Mexico's figures using the official data for Dec. 31st, 2020.
  mutate(covid_deaths = ifelse(region == "Mexico", 126507, covid_deaths), # Source: https://youtu.be/y-mBbyPdAe8
         excess_deaths = ifelse(region == "Mexico", 326609, excess_deaths), # Source: https://coronavirus.gob.mx/exceso-de-mortalidad-en-mexico, consulted on Feb 27, 2020
         excess_deaths_per_100k = ifelse(region == "Mexico", round(excess_deaths/126014024*1e5, digits = 0), # Here I use the 2020 Census figure for total population (126,014,024), see: https://bit.ly/3q1h0WK
                                         excess_deaths_per_100k),
         end_date =  if_else(region == "Mexico", as_date("2020-12-31"), as_date(end_date))) %>% 
  arrange(-excess_deaths_per_100k)

# Create variable period
eco_ex_mor <- 
  eco_ex_mor %>% 
  mutate(period = str_c(month(start_date, label = T, abbr = T), " ", day(start_date), "-", month(end_date, label = T, abbr = T), " ", day(end_date)))



### Data from Mexico's SNSP ----

# Source: https://www.gob.mx/sesnsp/articulos/informacion-sobre-violencia-contra-las-mujeres-incidencia-delictiva-y-llamadas-de-emergencia-9-1-1-febrero-2019

# The PDF from which the data was obtain can be found at 01_data/snsp

# Import data
snsp <- 
  read_excel("01_data/snsp/select_variables_from_info_delictiva_violencia_contra_ las_mujeres_ene21.xlsx") %>% 
  mutate(crime = str_to_sentence(crime))

### Data from UNESCO ----

# Import data
school <- 
  read_csv("https://en.unesco.org/sites/default/files/covid_impact_education.csv",
           col_types = cols(Note = col_character())) %>%
  clean_names() 

# Change type to variable date and edit values for Bolivia and Dominican Republic ----
school <- 
  school %>% 
  mutate(date = dmy(date), 
         country = case_when(str_detect(country, "Bolivia") ~ "Bolivia", 
                             country == "Dominican Republic" ~ "Dom. Rep.",
                             TRUE ~ country),
         status = fct_relevel(status, "Fully open", "Partially open", "Closed due to COVID-19", "Academic break"))


### Data from IMF ----

# Import data 
imf <- 
  read_excel("01_data/imf/toitd-fiscalmeasuresdatabase-jan-update.xlsx",
             sheet = "Summary.Global", range = "b7:x203")

# Select and rename variables of interes
imf <- 
  imf %>% 
  select(country = `G20: Advanced economies`,
         # asfr: Additional spending or foregone revenues as % of GDP
         asfr_percent_gdp = ...14, 
         # eqlg: Equity injections, loans, asset purchase or debt assumptions as % of GDP
         eqlg_percent_gdp = ...19) 

# Remove rows that don't correspond to a country or territory or that have NAs for the two variables of interest
imf <- 
  imf %>% 
  # Remove rows that don't correspond to a country or territory
  filter(!country %in% c("G20: Emerging markets", "Other Selected Advanced Economies", "Other Selected Emerging Markets", "Selected Low-Income Developing Countries"),
         !is.na(country),
         # All the countries with missing values in asfr_percent_gdp also have missing values eqlg_percent_gdp
         !is.na(asfr_percent_gdp))  


# Create groups using rthe values in column B of the original file
imf <- 
  imf %>% 
  mutate(group = case_when(country %in% c("Australia", "Canada", "European Union", "France", "Germany", "Italy", "Japan", "Korea", "Spain", "United Kingdom", "United States") ~ "G20: Advanced economies",
                           country %in% c("Argentina", "Brazil", "China", "India", "Indonesia", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey") ~ "G20: Emerging markets",
                           country %in% c("Austria", "Belgium", "Cyprus", "Czech republic", "Denmark", "Estonia", "Finland", "Greece", "Hong Kong SAR", "Iceland", "Ireland", "Israel", "Latvia", "Lithuania", "Luxembourg", "Macao SAR", "Malta", "The Netherlands", "New Zealand", "Norway", "Portugal", "Singapore", "Slovak Republic", "Slovenia", "Sweden", "Switzerland") ~ "Other Selected Advanced Economies",
                           country %in% c("Albania", "Algeria", "Angola", "Antigua and Barbuda", "Armenia", "Aruba", "Azerbaijan", "Bahamas, The", "Bahrain", "Barbados", "Belarus", "Belize", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brunei Darussalam", "Bulgaria", "Cabo Verde", "Chile", "Colombia", "Costa Rica", "Croatia", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eswatini", "Fiji", "Gabon", "Georgia", "Grenada", "Guatemala", "Guyana", "Hungary", "Iran", "Iraq", "Jamaica", "Jordan", "Kazakhstan", "Kosovo", "Kuwait", "Lebanon1", "Libya", "Malaysia", "Maldives", "Mauritius", "Micronesia, Fed. States of", "Mongolia", "Montenegro, Rep. of", "Morocco", "Namibia", "Nauru", "North Macedonia", "Oman", "Pakistan", "Palau", "Panama", "Paraguay", "Peru", "Philippines", "Poland", "Qatar", "Romania", "Samoa", "Serbia", "Seychelles", "Sri Lanka", "St. Kitts and Nevis", "St. Lucia", "St. Vincent and the Grenadines", "Thailand", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkmenistan", "Tuvalu", "Ukraine", "United Arab Emirates", "Uruguay", "Vanuatu") ~ "Other Selected Emerging Markets",
                           country %in% c("Afghanistan", "Bangladesh", "Benin", "Bhutan", "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo, Republic of", "Côte d'Ivoire", "Democratic Republic of the Congo", "Djibouti", "Eritrea", "Ethiopia", "Gambia, The", "Ghana", "Guinea", "Guinea-Bissau", "Haiti", "Honduras", "Kenya", "Kiribati", "Kyrgyz Republic", "Lao P.D.R.", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Moldova", "Mozambique", "Myanmar", "Nepal", "Nicaragua", "Niger", "Nigeria", "Papua New Guinea", "Rwanda", "São Tomé and Príncipe", "Senegal", "Sierra Leone", "Solomon Islands", "Somalia", "South Sudan", "Sudan", "Tajikistan", "Tanzania", "Timor-Leste, Dem. Rep. of", "Togo", "Uganda", "Uzbekistan", "Vietnam", "Yemen", "Zambia", "Zimbabwe") ~ "Selected Low-Income Developing Countries"),
         group_gral = case_when(str_detect(group, "Advanced") ~ "Advanced economies",
                                str_detect(group, "Emerging") ~ "Emerging markets",
                                TRUE ~ "Selected Low-Income Developing Countries"))



### Data from the World Bank ----

# Import data 
wb <- 
  read_excel("01_data/wb/GlobalEconomicProspectsJanuary2021GDPgrowthdata.xlsx",
             sheet = "Statistical Appendix", 
             range = "a5:g151")

# Select and rename columns 
wb <- 
  wb %>% 
  select(eco_group = ...1, 
         region = ...2,
         unit = ...3, 
         `2020` = `2020e`) 

# Fill empty cells 
wb <- 
  wb %>% 
  fill(eco_group, .direction = "down") %>% 
  fill(region, .direction = "down") 

# Several transformations
wb <- 
  wb %>% 
  # Create variable 2019
  mutate(`2019` = 0) %>% 
  # Transform tibble's structure from wide to long
  pivot_longer(cols = c(`2019`, `2020`), names_to = "yr", values_to = "change_gdp") %>% 
  
  mutate(yr = as.numeric(yr), # Transform type of yr
         # Delete numbers from country names
         unit = str_replace_all(unit, "[0-9]+", ""), 
         # Delete extra white space from country names
         unit = str_trim(unit))



### Data from ECLAC/CEPAL ----

# Import data
eco <- 
  read_excel("01_data/cepal/eclac_covid19_economy.xlsx", 
             sheet = "country_measure") %>% 
  pivot_longer(-Subtheme, 
               names_to = "pais",
               values_to = "num_acciones") %>% 
  mutate(tema = "Economy") %>% 
  select(tema, subtema = Subtheme, pais, num_acciones)


edu <- 
  read_excel("01_data/cepal/eclac_covid19_education.xlsx", 
             sheet = "country_measure") %>% 
  pivot_longer(-Subtheme, 
               names_to = "pais",
               values_to = "num_acciones") %>% 
  mutate(tema = "Education") %>% 
  select(tema, subtema = Subtheme, pais, num_acciones)


genero <- 
  read_excel("01_data/cepal/eclac_covid19_gender.xlsx", 
             sheet = "country_measure") %>% 
  pivot_longer(-Subtheme, 
               names_to = "pais",
               values_to = "num_acciones") %>% 
  mutate(tema = "Gender") %>% 
  select(tema, subtema = Subtheme, pais, num_acciones)


salud <- 
  read_excel("01_data/cepal/eclac_covid19_health.xlsx", 
             sheet = "country_measure") %>% 
  pivot_longer(-Subtheme, 
               names_to = "pais",
               values_to = "num_acciones") %>% 
  mutate(tema = "Health") %>% 
  select(tema, subtema = Subtheme, pais, num_acciones)


empleo <- 
  read_excel("01_data/cepal/eclac_covid19_labor.xlsx", 
             sheet = "country_measure") %>% 
  pivot_longer(-Subtheme, 
               names_to = "pais",
               values_to = "num_acciones") %>% 
  mutate(tema = "Labor") %>% 
  select(tema, subtema = Subtheme, pais, num_acciones)


mov <- 
  read_excel("01_data/cepal/eclac_covid19_movements.xlsx", 
             sheet = "country_measure") %>% 
  pivot_longer(-Subtheme, 
               names_to = "pais",
               values_to = "num_acciones") %>% 
  mutate(tema = "Movements across and within countries") %>% 
  select(tema, subtema = Subtheme, pais, num_acciones)


pro_soc <- 
  read_excel("01_data/cepal/eclac_covid19_social_protection.xlsx", 
             sheet = "country_measure") %>% 
  pivot_longer(-Subtheme, 
               names_to = "pais",
               values_to = "num_acciones") %>% 
  mutate(tema = "Social protection") %>% 
  select(tema, subtema = Subtheme, pais, num_acciones)

vac <- 
  read_excel("01_data/cepal/eclac_covid19_vaccionation.xlsx", 
             sheet = "country_measure") %>% 
  pivot_longer(-Subtheme, 
               names_to = "pais",
               values_to = "num_acciones") %>% 
  mutate(tema = "Vaccination") %>% 
  select(tema, subtema = Subtheme, pais, num_acciones)


# Join data
cepal <- 
  bind_rows(eco, edu, genero, salud, empleo, pro_soc, vac)

# Calculate total measures by country and topic
cepal_by_topic <- 
  cepal %>% 
  group_by(tema, pais) %>% 
  summarise(num_acciones = sum(num_acciones)) %>% 
  ungroup()
