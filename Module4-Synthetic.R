syn_data = data.frame(
  PatientID=numeric(n_patients),
  Age=numeric(n_patients),
  Gender=character(n_patients),
  TreatmentGroup=character(n_patients),
  EnrollmentDate=lubridate::as_date(character(n_patients)),
  BloodPressure=numeric(n_patients),
  Cholesterol=numeric(n_patients),
  AdverseEvent=integer(n_patients)
)

#Variable 1: Patient ID
syn_data$PatientID = 1:n_patients

#Variable 2: Age
syn_data$Age = round(rnorm(n_patients, mean=45, sd=10), 1)

#Variable 3: Gender
syn_data$Gender = purrr::map_chr(sample(c("Male", "Female"), n_patients, replace=TRUE), as.character)

#Variable 4: treatment group
syn_data$TreatmentGroup = purrr::map_chr(sample(c("A", "B", "Placebo"),n_patients, replace=TRUE), as.character)

#Variable 5: Enrollment date
syn_data$EnrollmentDate = lubridate::as_date(sample(seq(from=lubridate::as_date("2022-01-01"), to=lubridate::as_date("2022-12-31"), by="days"), n_patients, replace=TRUE))

#Variable 6: Blood pressure
syn_data$BloodPressure = round(runif(n_patients, min=90, max=160), 1)

#Variable 7: Cholesterol
syn_data$Cholesterol[syn_data$TreatmentGroup == "A"] <- round(rnorm(sum(syn_data$TreatmentGroup == "A"), mean = 160, sd = 10), 1)
syn_data$Cholesterol[syn_data$TreatmentGroup == "B"] <- round(rnorm(sum(syn_data$TreatmentGroup == "B"), mean = 180, sd = 10), 1)
syn_data$Cholesterol[syn_data$TreatmentGroup == "Placebo"] <- round(rnorm(sum(syn_data$TreatmentGroup == "Placebo"), mean = 200, sd = 10), 1)

#variable 8: Adverse event
syn_data$AdverseEvent[syn_data$TreatmentGroup == "A"] <- purrr::map_int(sample(0:1, sum(syn_data$TreatmentGroup == "A"), replace = TRUE, prob = c(0.5, 0.5)), as.integer)
syn_data$AdverseEvent[syn_data$TreatmentGroup == "B"] <- purrr::map_int(sample(0:1, sum(syn_data$TreatmentGroup == "B"), replace = TRUE, prob = c(0.7, 0.3)), as.integer)
syn_data$AdverseEvent[syn_data$TreatmentGroup == "Placebo"] <- purrr::map_int(sample(0:1, sum(syn_data$TreatmentGroup == "Placebo"), replace = TRUE, prob = c(0.9, 0.1)), as.integer)

#print first few lines
head(syn_data)
