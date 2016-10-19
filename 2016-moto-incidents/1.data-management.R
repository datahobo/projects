####
# This file imports the NYS DMV Crash Vehicle data in order to analyze
# motorcycle crashes from that dataset. Motorcycles are identified in
# the original datasets using RegistrationClass == MOT

library(stringr)
# Read in the initial files
files.location <- "~/Desktop/moto/"
moto.registrations <- read.csv(paste0(files.location, "moto.registrations.csv"))

# Clean up the make information
#####
# Read in the updated mapping of the 5-character makes in the dataset,
## to the manually cleaned MakeNames provided
freq.moto.registrations <- read.csv(paste0(files.location, "freq.moto.registrations.csv"))
# We're only going to want two of the columns from that file
mapping.makes <- c("Make", "MakeName")
# so select those two columns
moto.makes <- freq.moto.registrations[mapping.makes]
# now merge them back into the main dataset
cleaned.moto.registrations <- merge(moto.registrations, moto.makes)
table(cleaned.moto.registrations$MakeName)
# now clean out the memory
rm(moto.registrations, freq.moto.registrations, moto.makes, mapping.makes)
#####

# Now figure out how to merge these two data sets

#####
cleaned.moto.registrations <- cleaned.moto.registrations[order(cleaned.moto.registrations$X), ]

moto.registration.ids <- as.data.frame(cleaned.moto.registrations[c("X", "MakeName", "Reg.Valid.Date")])
names(moto.registration.ids) <- c("Case.Vehicle.ID", "Make.Name", "Reg.Valid.Date")

raw.individual.data <- read.csv(paste0(files.location, "mvcrashes_individuals.csv"))
raw.case.data <- read.csv(paste0(files.location, "mvcrashes_cases.csv"))

temp.data <- merge(moto.registration.ids, raw.individual.data)
