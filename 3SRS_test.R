
library(tidyverse)
library(readxl)

rm(list=ls())

source("3SRS.R")
# Importer la base dummy
dummy_sf <-read_excel("data/dummy.xlsx")

length(unique(dummy_sf$partner))
length(unique(dummy_sf$project))
hh_N <- sum(dummy_sf$hh_beneficiaries)

unit_size<-200 # nombre de menages à tirer sur le plan theorique
ssu_size <-2


# Format sampling frame

dummy_sframe <- format_sampling_frame_3srs(
  sframe=dummy_sf,
  strata="country",
  col_psu="partner",
  col_ssu="project",
  col_pop="hh_beneficiaries"
)|> 
  filter(pop_numbers>=unit_size) # garder les ssu avec au minimum la taille des unités à tirer

hh_N <- sum(dummy_sframe$pop_numbers)

# Calcul de l'echantillon minimal
sampling_target <- create_targets(
  dummy_sframe,
  strata_id = strata_id,         
  pop_numbers = pop_numbers, 
  buffer = 0.1,  
  cl = 0.90,
  p = 0.5,
  E = 0.10,
  rho1 = 0.02, ##faible ressemblance des beneficiaires entre les projets
  rho2 = 0.04,
  m1 = ssu_size,
  m2 = unit_size
); sampling_target

# Faire 3SRS (alternative)

#target <- sampling_target$target.with.buffer

#psu_size <-ceiling(target/(ssu_size*unit_size))

theorical_size <- sampling_target |> 
  mutate(
    unit_size=unit_size,
    ssu_size=ssu_size,
    psu_size=ceiling(target/(ssu_size*unit_size))
  )

psu_list <- dummy_sframe |> 
  distinct(strata_id, psu_id)

psu_list <- psu_list |> 
  left_join(theorical_size |> select(strata_id, psu_size), by = "strata_id")

psu_sample <- psu_list |> 
  group_by(strata_id) |> 
  mutate(proba_selection_psu = psu_size / n()) |> 
  summarise(
    psu_id = sample(psu_id, size = min(first(psu_size), n()), replace = F),
    proba_selection_psu = first(proba_selection_psu)
  )

#psu_sample_id=psu_sample |> pull(psu_id)


ssu_list <- dummy_sframe |> 
  group_by(strata_id, psu_id,ssu_id) |>
  summarise(
    ssu_pop_numbers=sum(pop_numbers,na.rm = T)
  )
  
ssu_list <- ssu_list |> 
  semi_join(psu_sample, by = c("strata_id", "psu_id")) # garder seulement les psu_id par strata_id dans psu_sample_id 

ssu_list <- ssu_list |> 
  left_join(theorical_size |> select(strata_id, ssu_size), by = "strata_id")

ssu_list_to_sample <- ssu_list |> 
  #filter(psu_id %in% psu_sample_id) |> 
  group_by(strata_id, psu_id) |> 
  mutate(
    proba_selection_ssu = pmin(1, ssu_size * (ssu_pop_numbers / sum(ssu_pop_numbers, na.rm = TRUE))) # si ssu_size est plus grand que le nombre total de SSU disponibles dans PSU, on met proba 1
  ) |> 
  ungroup()

ssu_sample <- ssu_list_to_sample |> 
  group_by(strata_id,psu_id) |> 
  summarise(
    ssu_id=sample(
      ssu_id,
      size=min(first(ssu_size),length(ssu_id)),
      prob = proba_selection_ssu,
      replace=F
    )
  )

ssu_sample <-ssu_sample |> 
  left_join(ssu_list_to_sample |> select(strata_id, psu_id,ssu_id,ssu_pop_numbers,proba_selection_ssu),by=c("strata_id","psu_id","ssu_id"))

# distribuer les nombres d enquetes proportionnellement à la taille des ssu

ssu_sample <- ssu_sample |> 
  left_join(theorical_size |> select(strata_id, target.with.buffer), by = "strata_id") |>
  ungroup()

ssu_sample <- ssu_sample |> 
  group_by(strata_id) |> 
  mutate(
    survey_raw = target.with.buffer * (ssu_pop_numbers / sum(ssu_pop_numbers)),
    survey =  pmax(1, round(survey_raw))
  )|> 
  ungroup()


strata_list <- as.character(unique(ssu_sample$strata_id))

ssu_sample_table <- split(ssu_sample, ssu_sample$strata_id)

for (strat in strata_list) {
  
  ssu_sample_treat <- ssu_sample_table[[strat]]
  
  diff <- sum(ssu_sample_treat$survey) - first(ssu_sample_treat$target.with.buffer)
  
  # ajustement jusqu’à ce que la somme corresponde au target
  while (diff != 0) {
    idx <- sample(1:nrow(ssu_sample_treat), abs(diff), replace = TRUE)
    ssu_sample_treat$survey[idx] <- ssu_sample_treat$survey[idx] - sign(diff)
    
    diff <- sum(ssu_sample_treat$survey) - first(ssu_sample_treat$target.with.buffer)
  }
  
  # nettoyage
  ssu_sample_treat <- ssu_sample_treat %>% 
    select(-c(survey_raw, target.with.buffer, diff), everything())
  
  # on remet le tableau ajusté dans la liste
  ssu_sample_table[[strat]] <- ssu_sample_treat
}

ssu_sample <- bind_rows(ssu_sample_table) |> 
  select(-c(survey_raw, target.with.buffer, diff))

unit_prob <- ssu_sample |>
  mutate(
    proba_selection_unit=unit_size/ssu_pop_numbers
  )

sample <- ssu_sample |> 
  left_join(psu_sample,by=c("strata_id","psu_id"))

sample <- sample |> 
  left_join(unit_prob |> select(strata_id,psu_id,ssu_id,proba_selection_unit),by=c("strata_id","psu_id","ssu_id")) 

sample <- sample |>
  ungroup() |> 
  relocate(c(survey,ssu_pop_numbers,proba_selection_psu,proba_selection_ssu,proba_selection_unit),.after = ssu_id) |> 
  mutate(
    proba_selection=proba_selection_psu*proba_selection_ssu*proba_selection_unit
  )



