library(tidyverse)
install.packages("tidyverse")

clinic_raw = read_csv( "data/clinic_raw.csv", skip = 2)
head(clinic_raw)

1:20
seq(1, 20, by = 1)
seq(1, 20, by = 5)

str(clinic_raw)

sum(is.na(clinic_raw$age))
range(clinic_raw$age, na.rm =TRUE)
summary(clinic_raw)

clinic_clean = 
  clinic_raw %>%
  rename(id = patient_id)

clinic_clean = 
  clinic_clean %>%
  filter(age >= 18| is.na(age))

clinic_clean = 
  clinic_clean %>%
  select(id,site, sex, age,visit_date, blood_pressure, weight_kg, notes)

clinic_clean %>% arrange(age) %>% head()

clinic_clean %>% arrange(desc(age)) %>% head()

clinic_clean %>%
  group_by(site) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE)
    , n = n()
  )    

clinic_clean = 
  clinic_clean %>%
  mutate(
    site = tolower(trimws(site))
    , site = gsub("_", " ", site)
  )
table(clinic_clean$site)
)

substring("PT-0001", 4, 7)

library(stringr)  
clinic_clean = 
  clinic_clean %>%
  mutate(
    bp_upper = as.numeric(str_extract(blood_pressure,"^[0-9]+"))
    , bp_lower = as.numeric(str_extract(blood_pressure, "(?<=/)[0-9]+"))
  )
head(clinic_clean[, c("blood_pressure", "bp_upper", "bp_lower")])
)

gsub("[^0-9.]", "", "72.5 kg")

grep("follow", clinic_clean$notes, ignore.case = TRUE)

clinic_clean %>% filter(grepl("follow", notes, ignore.case = TRUE))

clinic_clean = 
  clinic_clean %>%
  mutate(
    weight_kg = na_if(weight_kg, "-")
    , weight_kg = as.numeric(weight_kg)
    , visit_date = as.Date(visit_date, format = "%d/%m/%Y")
  )
head(clinic_clean)
)

clinic_clean = 
  clinic_clean %>%
  mutate(
    sex =ifelse(
      toupper(sex) == "M", "Male"
      , ifelse(toupper(sex) == "F", "Female", NA)
    )
  )

clinic_clean = clinic_clean %>%
  mutate(
    bp_category = case_when(
      is.na(bp_upper) ~ NA_character_
      , bp_upper <120 ~ "Normal"
      , bp_upper <140 ~ "Elevated"
      , TRUE          ~ "High"
    )
  )
table(clinic_clean$bp_category, useNA = "always")

clean_text = function(x) {
  x = trimws(x)
  x = tolower (x)
  x
}

clean_text("FOLLOW UP")

clean_text = function(x) {
  x = trimws(x)  # strip whitespace
  x = tolower(x) # standardise case
  x
}

clean_text("  FOLLOW UP")
#> [1] "follow up"

clinic_clean = 
  clinic_clean %>%
  mutate(notes = clean_text(notes))
table(clinic_clean$notes, useNA ="always")

clinic_clean %>%
  select(id,bp_upper, bp_lower)%>%
  head(4) %>%
  tidyr::pivot_longer(
    cols = c(bp_upper, bp_lower)
    , names_to = "reading"
    , values_to = "value"
  )

clinic_clean %>%
  group_by(site, sex) %>%
  summarise(mean_age = mean(age, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = sex, values_from = mean_age)

clinic_clean %>%
  group_by(site, sex) %>%
  summarise(mean_age = mean(age, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = sex, values_from = mean_age)
#> # A tibble: 4 × 4
#>   site   Female  Male  `NA`
#>   <chr>   <dbl> <dbl> <dbl>
#> 1 site a   49.5  48.7  40  
#> 2 site b   53.2  51.5  55.5
#> 3 site c   49.5  52.8  59.7
#> 4 site d   52.0  58.5  47

site_lookup = data.frame(
  site = c("site a", "site b", "site c", "site d")
  , region = c("North", "North", "South", "South")
)

clinic_clean =
  clinic_clean %>%
  left_join(site_lookup, by = "site") # attach region, keep every clinic_clean row
head(clinic_clean)
#> # A tibble: 6 × 12
#>   id      site   sex      age visit_date blood_pressure weight_kg notes bp_upper
#>   <chr>   <chr>  <chr>  <dbl> <date>     <chr>              <dbl> <chr>    <dbl>
#> 1 PT-0001 site a <NA>      70 2023-02-06 180/64 mmHg         79.1 stab…      180
#> 2 PT-0002 site a Female    41 2023-09-13 97/74 mmHg          65.3 foll…       97
#> 3 PT-0003 site a <NA>      55 2023-12-12 133/72 mmHg         82.1 <NA>       133
#> 4 PT-0004 site a Female    37 2023-06-26 133/70 mmHg         62.5 stab…      133
#> 5 PT-0005 site b Male      50 2023-07-29 101/62 mmHg         50.4 disc…      101
#> 6 PT-0006 site d Female    53 2023-11-11 98/75 mmHg          84.9 foll…       98
#> # ℹ 3 more variables: bp_lower <dbl>, bp_category <chr>, region <chr>

clinic_clean %>% filter(bp_category == "High") %>% nrow()
#> [1] 86
clinic_clean |> filter(bp_category == "High") |> nrow()
#> [1] 86

write_csv(clinic_clean, "data/clinic_clean.csv")

renv::snapshot()

install.packages("usethis")
usethis::create_github_token()   # opens GitHub in your browser to generate a token
gitcreds::gitcreds_set()         # paste the token here to store it securely

usethis::use_git()

usethis::use_github()