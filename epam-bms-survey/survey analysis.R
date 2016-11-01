# check for required packages and install them if necessary
#####
# setup
list.of.packages <- c("Hmisc", "weights", "survey", "cluster")

InstallNeededPackages <- function (list.of.packages) {
  # this function reads from a list of packages, determines which ones
  # are not installed, and installs them
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  # is there a way to activate them all in this function? that would be nice...
}

library(survey)
library(stringr)
library(cluster)
library(ggplot2)

#####

# clean up the data frame
##### 
# bring in the files
files.location <- "~/Box Sync/projects/_Client Work/BMS/Survey/data/"
survey.file <- "survey-partial.csv"
setwd(files.location)
survey.data <- read.csv(paste0(files.location,survey.file))
rm(files.location, list.of.packages, new.packages, survey.file)
names(survey.data)
# need to remove the useless variables
variables.to.kill <- c("ip.address", "duplicate", "seq..number", 
                       "external.reference", "custom.variable.2", 
                       "custom.variable.3", "custom.variable.4",
                       "custom.variable.5", "respondent.email",
                       "email.list", "country.code", "region", "b1", "b2", "b3",
                       "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12",
                       "b13", "b14", "b15", "b16", "b17")
survey.data <- survey.data[, ! (names(survey.data) %in% variables.to.kill)]
rm(variables.to.kill)
# now clean up the org name
table(survey.data$organization)
# add it to a variable so that we can set up all the weights later on
org.levels <- names(table(survey.data$organization))
org.levels[1] <- "ABO"
org.levels[2] <- "CSD"
org.levels[3] <- "DPST"
levels(survey.data$organization) <- org.levels
# add rownames so that we have them for later merging
rownames(survey.data) <- survey.data$response.id
survey.data$response.id <- NULL

# Now we need to fix the levels of all the workaround questions:
# There are two different sets of labels - one for workarounds.planning, and one for all the others
# Now get the indices of all the planning variables
workarounds <- str_subset(names(survey.data), "workarounds")
# remove the other specifies
workarounds <- workarounds[-grep("other.specify", workarounds)]
# remove everything but the planning ones
workarounds <- workarounds[grep("planning", workarounds)]
# now get the list of indices
workarounds.variables <- which(colnames(survey.data) %in% workarounds)
# loop
for (i in 1:length(workarounds.variables)) {
  variable.to.relevel <- workarounds.variables[i]
  survey.data[, variable.to.relevel] <- 
    ordered(survey.data[, variable.to.relevel], levels = 
              c("Several times per day",
                "Daily \xd0 about once per day",
                "Weekly \xd0 about once per week",
                "Monthly \xd0 about once per month",
                "Less often",
                "Never",
                ""))
  levels(survey.data[, variable.to.relevel]) <- c("Several times a day",
                                                  "Daily", 
                                                  "Weekly",
                                                  "Monthly",
                                                  "Less often",
                                                  "Never",
                                                  "")
  
  }
table(survey.data$workarounds.planning.experiments.planning.repetitive.experiments)
# Now get the non-planning ones
table(survey.data$workarounds.gathering.data.save.data.to.sharepoint.or.shared.drives)
# start by copying the lines from above
# Now get the indices of all the planning variables
workarounds <- str_subset(names(survey.data), "workarounds")
# remove the other specifies
workarounds <- workarounds[-grep("other.specify", workarounds)]
# NOW remove the planning ones
workarounds <- workarounds[-grep("planning", workarounds)]
# now get the list of indices
workarounds.variables <- which(colnames(survey.data) %in% workarounds)
# loop
for (i in 1:length(workarounds.variables)) {
  variable.to.relevel <- workarounds.variables[i]
  survey.data[, variable.to.relevel] <- 
    ordered(survey.data[, variable.to.relevel], levels = 
              c("Several times a day",
                "Daily",
                "Weekly",
                "Monthly",
                "Less often",
                "Never",
                ""))
}
rm(workarounds, workarounds.variables, variable.to.relevel, i)
#####

# Experimenting with weighting based on time spent in each category
##### 

# what's the current state?
sum(table(survey.data$time.spent.experimentation))
sum(table(survey.data$time.spent.gathering.and.analyzing.data))
sum(table(survey.data$time.spent.creating.reports))
sum(table(survey.data$time.spent.managing.project.teams))
sum(table(survey.data$time.spent.other.responsibilities))

# first, clean the data.
# Need to add zeros to all the NA's in the time spent series.
# Use the other responsibilities column as the baseline.
survey.data$time.spent.experimentation[!(is.na(survey.data$time.spent.other.responsibilities)) & 
                                         is.na(survey.data$time.spent.experimentation)] <- 0
survey.data$time.spent.gathering.and.analyzing.data[!(is.na(survey.data$time.spent.other.responsibilities)) &
                                         is.na(survey.data$time.spent.gathering.and.analyzing.data)] <- 0
survey.data$time.spent.creating.reports[!(is.na(survey.data$time.spent.other.responsibilities)) &
                                          is.na(survey.data$time.spent.creating.reports)] <- 0
survey.data$time.spent.managing.project.teams[!(is.na(survey.data$time.spent.other.responsibilities)) &
                                                is.na(survey.data$time.spent.managing.project.teams)] <- 0

# It looks like we need a second run on the last two
survey.data$time.spent.managing.project.teams[!(is.na(survey.data$time.spent.experimentation)) &
                                                is.na(survey.data$time.spent.managing.project.teams)] <- 0
survey.data$time.spent.other.responsibilities[!(is.na(survey.data$time.spent.experimentation)) &
                                                is.na(survey.data$time.spent.other.responsibilities)] <- 0

# Calculate weights that will work across specific questions
survey.data$weight.experimentation <- survey.data$time.spent.experimentation / 
  mean(survey.data$time.spent.experimentation, na.rm = TRUE)
survey.data$weight.gathering.and.analyzing.data <- survey.data$time.spent.gathering.and.analyzing.data / 
  mean(survey.data$time.spent.gathering.and.analyzing.data, na.rm = TRUE)
survey.data$weight.creating.reports <- survey.data$time.spent.creating.reports /
  mean(survey.data$time.spent.creating.reports, na.rm = TRUE)
survey.data$weight.managing.project.teams <- survey.data$time.spent.managing.project.teams /
  mean(survey.data$time.spent.managing.project.teams, na.rm = TRUE)

# QA the weights - do they average to 1?
mean(survey.data$weight.experimentation, na.rm = TRUE)
mean(survey.data$weight.gathering.and.analyzing.data, na.rm = TRUE)
mean(survey.data$weight.creating.reports, na.rm = TRUE)
mean(survey.data$weight.managing.project.teams, na.rm = TRUE)

table(survey.data$time.spent.experimentation)
length(table(survey.data$time.spent.experimentation))

# How many respondents now have values in the weights?
sum(table(survey.data$time.spent.experimentation))
sum(table(survey.data$time.spent.gathering.and.analyzing.data))
sum(table(survey.data$time.spent.creating.reports))
sum(table(survey.data$time.spent.managing.project.teams))
sum(table(survey.data$time.spent.other.responsibilities))
# Now check the cross-tabs
sum(table(survey.data$time.spent.experimentation, survey.data$time.spent.gathering.and.analyzing.data))
sum(table(survey.data$time.spent.gathering.and.analyzing.data, survey.data$time.spent.creating.reports))
sum(table(survey.data$time.spent.creating.reports, survey.data$time.spent.managing.project.teams))
sum(table(survey.data$time.spent.managing.project.teams, survey.data$time.spent.other.responsibilities))
#####

# Evaluating the NA conditions around the weights.
#####
# need to evaluate if it's worth keeping anyone who is NA for the weights - updated for all the weights
table(is.na(survey.data$weight.experimentation))
table(is.na(survey.data$weight.experimentation) & is.na(survey.data$weight.gathering.and.analyzing.data) & 
      is.na(survey.data$weight.creating.reports) & is.na(survey.data$weight.managing.project.teams))
survey.data$has.no.time.weights <- (is.na(survey.data$weight.experimentation) & 
                                  is.na(survey.data$weight.gathering.and.analyzing.data) & 
                                  is.na(survey.data$weight.creating.reports) & 
                                  is.na(survey.data$weight.managing.project.teams))
temp.data <- survey.data[survey.data$has.no.time.weights, ]
# for now, we will remove the NAs
survey.data <- survey.data[!survey.data$has.no.time.weights, ]
rm(temp.data)
# Need to set up one weighting component by list distribution (organization)
# do this after we've removed the non-time responders
#####

# Now calculate the population weights
#####
# input all the population sizes:
pop.ABO <- 85
pop.CSD <- 217
pop.DPST <- 238
population <- c(pop.ABO, pop.CSD, pop.DPST)
names(population) <- org.levels
rm(pop.ABO, pop.CSD, pop.DPST)
# now calculate the population and the distributions
pop.total <- sum(population)
pop.distribution <- population / pop.total
rm(population, pop.total)

# What is the sample distribution?
sample.survey <- table(survey.data$organization)
sample.distribution <- sample.survey / nrow(survey.data)
# now calculate the weights based on the differences between the sample and the pop
(sample.weights <- as.data.frame(pop.distribution / sample.distribution))
# now make sure the variable names match up to what we want in the data frame
names(sample.weights) <- c("organization", "weight.org")
rm(org.levels, pop.distribution, sample.survey, sample.distribution)
# now need to add the sample weights into the data frame
survey.data <- merge(survey.data, sample.weights)
table(survey.data$weight.org)
rm(sample.weights)
#####

# Now that we have the survey and both sets of weights, time to figure out how to weight them.
#####

names(survey.data)
# setting the weights is somewhat complicated. From the "survey" package:
# Inside survey, there is the svydesign function that can be used to link 
# a data frame with a weight. This function takes many arguments, 
# three are of importance at this point: ids, data, and weights. 
# The latter two are easily explained as they specify the data frame 
# we seek to apply weights on and the vector containing the weights.
# The first argument, ids is a little bit more complex. 
# By default, survey assumes we use data generated in a multistage sampling procedure. 
# Then the ids argument would be used to specify which variables 
# encode sampling cluster membership. Clustered sampling is 
# another quite frequent social science sampling method, 
# but not one that is used (nor usable) for large general population surveys. 
# Therefore, we set ids = ~ 1 to indicate that all respondents originated from the same cluster.

# now we set the first weight:
survey.data.weighted.experimentation <- svydesign(ids = ~1, data = survey.data, 
                                                  weights = survey.data$weight.org)
# get some summary information
survey.data.weighted.experimentation
summary(survey.data.weighted.experimentation)
# can we get a mean for experimentation?
svymean(x = ~PD.activities.experimentation, design = survey.data.weighted.experimentation, na.rm = TRUE)
table(survey.data$PD.activities.experimentation) / NROW(survey.data)
# they look slightly different...
svymean(x = ~PD.activities.gathering.and.analyzing.data, design = survey.data.weighted.experimentation, na.rm = TRUE)
table(survey.data$PD.activities.gathering.and.analyzing.data) / NROW(survey.data) * 100

svymean(x = ~PD.activities.creating.reports, design = survey.data.weighted.experimentation, na.rm = TRUE)
table(survey.data$PD.activities.creating.reports) / NROW(survey.data) * 100      

svymean(x = ~PD.activities.managing.project.teams, design = survey.data.weighted.experimentation, na.rm = TRUE)
table(survey.data$PD.activities.managing.project.teams) / NROW(survey.data) * 100
#####

# Now time to do some clustering
#####
cluster.data <- survey.data[, c("time.spent.experimentation",
                                "time.spent.gathering.and.analyzing.data",
                                "time.spent.creating.reports",
                                "time.spent.managing.project.teams",
                                "time.spent.other.responsibilities")]
# now clean it

# what are the correlations?
time.correlations <- as.data.frame(cor(cluster.data, use = "pairwise.complete.obs"))
write.csv(time.correlations, "time.correlations.csv")
# now let's test with an agglomeration cluster
test2.cluster <- agnes(cluster.data, metric = "euclidean", stand = FALSE)
test2.cluster
plot(test2.cluster)


# Identify a couple of solutions and see what happens with them
#####
temp.two <- as.data.frame(cutree(test2.cluster, 2))
rownames(temp.two) <- rownames(survey.data)
names(temp.two) <- c("two.cluster.solution")
# now three
temp.three <- as.data.frame(cutree(test2.cluster, 3))
rownames(temp.three) <- rownames(survey.data)
names(temp.three) <- c("three.cluster.solution")
# now four
temp.four <- as.data.frame(cutree(test2.cluster, 4))
rownames(temp.four) <- rownames(survey.data)
names(temp.four) <- c("four.cluster.solution")
# now five
temp.five <- as.data.frame(cutree(test2.cluster, 5))
rownames(temp.five) <- rownames(survey.data)
names(temp.five) <- c("five.cluster.solution")

# now merge them in, sequentially
clustered.survey <- merge(survey.data, temp.two, by = 0)
# an additional column gets created - move it to the row names
rownames(clustered.survey) <- clustered.survey$Row.names
# now remove the extra column
clustered.survey$Row.names <- NULL
clustered.survey <- merge(clustered.survey, temp.three, by = 0)
rownames(clustered.survey) <- clustered.survey$Row.names
clustered.survey$Row.names <- NULL
clustered.survey <- merge(clustered.survey, temp.four, by = 0)
rownames(clustered.survey) <- clustered.survey$Row.names
clustered.survey$Row.names <- NULL
clustered.survey <- merge(clustered.survey, temp.five, by = 0)
rownames(clustered.survey) <- clustered.survey$Row.names
clustered.survey$Row.names <- NULL

table(clustered.survey$two.cluster.solution, clustered.survey$three.cluster.solution)
table(clustered.survey$three.cluster.solution, clustered.survey$four.cluster.solution)
View(test2.cluster$data)

# profiling the two clusters
tapply(clustered.survey$time.spent.experimentation, 
       clustered.survey$two.cluster.solution, mean, na.rm = TRUE)
tapply(clustered.survey$time.spent.gathering.and.analyzing.data, 
       clustered.survey$two.cluster.solution, mean, na.rm = TRUE)
tapply(clustered.survey$time.spent.creating.reports, 
       clustered.survey$two.cluster.solution, mean, na.rm = TRUE)
tapply(clustered.survey$time.spent.managing.project.teams, 
       clustered.survey$two.cluster.solution, mean, na.rm = TRUE)
tapply(clustered.survey$time.spent.other.responsibilities, 
       clustered.survey$two.cluster.solution, mean, na.rm = TRUE)

# profiling the three clusters
tapply(clustered.survey$time.spent.experimentation, 
       clustered.survey$three.cluster.solution, mean, na.rm = TRUE)
tapply(clustered.survey$time.spent.gathering.and.analyzing.data, 
       clustered.survey$three.cluster.solution, mean, na.rm = TRUE)
tapply(clustered.survey$time.spent.creating.reports, 
       clustered.survey$three.cluster.solution, mean, na.rm = TRUE)
tapply(clustered.survey$time.spent.managing.project.teams, 
       clustered.survey$three.cluster.solution, mean, na.rm = TRUE)
tapply(clustered.survey$time.spent.other.responsibilities, 
       clustered.survey$three.cluster.solution, mean, na.rm = TRUE)

# profiling the four clusters
tapply(clustered.survey$time.spent.experimentation, 
       clustered.survey$four.cluster.solution, mean, na.rm = TRUE)
tapply(clustered.survey$time.spent.gathering.and.analyzing.data, 
       clustered.survey$four.cluster.solution, mean, na.rm = TRUE)
tapply(clustered.survey$time.spent.creating.reports, 
       clustered.survey$four.cluster.solution, mean, na.rm = TRUE)
tapply(clustered.survey$time.spent.managing.project.teams, 
       clustered.survey$four.cluster.solution, mean, na.rm = TRUE)
tapply(clustered.survey$time.spent.other.responsibilities, 
       clustered.survey$four.cluster.solution, mean, na.rm = TRUE)

table(clustered.survey$four.cluster.solution)
test3.cluster <- kmeans(x = cluster.data[, 2:5], centers = 4)
#####

# Now try to plot some of the frequencies to see if I can see anything
#####
# basic ggplot2 scatterplot
ggplot(clustered.survey, aes(x = time.spent.experimentation, 
                             y = time.spent.gathering.and.analyzing.data)) +
  geom_point()
ggplot(clustered.survey, aes(x = time.spent.gathering.and.analyzing.data,
                             y = time.spent.creating.reports)) +
  geom_point()
ggplot(clustered.survey, aes(x = time.spent.creating.reports,
                             y = time.spent.other.responsibilities)) +
  geom_point()

test <- survey.data
survey.data <- test

# A function to calculate weighted variables if we need it
#####

WeightCategoryByTime <- function(survey.data, category.string,
                                 weighting.string) {
  # first, get a list of all the variable names that correspond to the challenges string
  challenges <- str_subset(names(survey.data), category.string)
  # now remove the other specifys from the list using grep, since they won't be weighted
  challenges <- challenges[-grep("other.specify", challenges)]
  # now that we have the list of variable names that we want, need to index them in the data frame:
  variables.to.weight <- which(names(survey.data) %in% challenges)
  # now that we have the indices, we need to create a new column in the data frame for each one with its weighted response
  
  weighting.variable <- which(names(survey.data) %in% weighting.string)
  # here's where we will loop through the array
  i <- 1
  for(i in 1:length(variables.to.weight)) {
    # where will we put the variable?
    new.variable.index <- length(survey.data) + 1
    # calculate the weighted result
    survey.data[, new.variable.index] <- survey.data[,challenges[[i]]] * survey.data[, weighting.variable]
    # now name the new variable
    names(survey.data)[[new.variable.index]] <- paste0("weighted.", challenges[[i]])
  }
  return(survey.data)  
}

# now run the function
category.string <- "challenges.planning"
weighting.string <- "weight.experimentation"
survey.data <- WeightCategoryByTime(survey.data, category.string, weighting.string)
# update
category.string <- "challenges.conducting"
survey.data <- WeightCategoryByTime(survey.data, category.string, weighting.string)
# update
category.string <- "challenges.data"
weighting.string <- "weight.gathering.and.analyzing.data"
survey.data <- WeightCategoryByTime(survey.data, category.string, weighting.string)
# update
category.string <- "challenges.reports"
weighting.string <- "weight.creating.reports"
survey.data <- WeightCategoryByTime(survey.data, category.string, weighting.string)
# update
category.string <- "challenges.managing"
weighting.string <- "weight.managing.project.teams"
survey.data <- WeightCategoryByTime(survey.data, category.string, weighting.string)
# now clean up the environment
rm(category.string, weighting.string)

# Now compare the challenges scores for each
mean(survey.data$challenges.planning.experiments.who.to.contact, na.rm = TRUE)
mean(survey.data$weighted.challenges.planning.experiments.who.to.contact, na.rm = TRUE)


#####

# how many responses do we have for each of the categories?
table(survey.data$PD.activities.experimentation)
table(survey.data$PD.activities.gathering.and.analyzing.data)
table(survey.data$PD.activities.creating.reports)
table(survey.data$PD.activities.managing.project.teams)


sum(table(survey.data$challenges.overall.planning.experiments))
sum(table(survey.data$challenges.overall.conducting.experiments))
sum(table(survey.data$challenges.overall.gathering.or.analyzing.data))
sum(table(survey.data$challenges.overall.creating.reports))
sum(table(survey.data$challenges.overall.managing.project.teams))

# scratch work
table(survey.data$workarounds.planning.experiments.planning.repetitive.experiments)
table(survey.data$workarounds.planning.experiments.ask.people.for.most.recent.literature)
table(survey.data$workarounds.planning.experiments.manually.look.through.many.information.stores.for.recent.literature)
table(survey.data$workarounds.planning.experiments.assume.i.have.everything.i.need)
table(survey.data$workarounds.planning.experiments.ask.team.members.for.an.expert)
table(survey.data$workarounds.planning.experiments.other.specify)
table(survey.data$workarounds.conducting.experiments.enter.lab.notes.on.anything.convenient)
table(survey.data$workarounds.conducting.experiments.maintain.my.own.experimental.supplies)
table(survey.data$workarounds.conducting.experiments.wait.until.i.need.data.to.document)
table(survey.data$workarounds.conducting.experiments.other.specify)
table(survey.data$workarounds.gathering.data.wait.until.experiment.is.complete.to.document)
table(survey.data$workarounds.gathering.data.save.data.to.sharepoint.or.shared.drives)
table(survey.data$workarounds.gathering.data.give.up.looking.for.other.data.that.would.improve.my.analysis)
table(survey.data$workarounds.gathering.data.ask.people.for.other.useful.data.or.results)
table(survey.data$workarounds.gathering.data.manually.look.through.other.sources.for.data)
table(survey.data$workarounds.gathering.data.other.specify)
table(survey.data$workarounds.creating.reports.create.new.report.from.scratch)
table(survey.data$workarounds.creating.reports.create.my.own.templates)
table(survey.data$workarounds.creating.reports.avoid.looking.for.more.analysis.beyond.whats.required)
table(survey.data$workarounds.creating.reports.ask.my.team.for.experts)
table(survey.data$workarounds.creating.reports.manually.look.through.other.sources.for.analysis)
table(survey.data$workarounds.creating.reports.ask.people.for.recent.literature)
table(survey.data$workarounds.creating.reports.other.specify)
table(survey.data$workarounds.managing.project.teamsrepeatedly.ask.what.people.are.working.on)
table(survey.data$workarounds.managing.project.teamscoordinate.in.spontaneous.meetings)
table(survey.data$workarounds.managing.project.teamsspend.more.time.than.necessary.discussing.status)
table(survey.data$workarounds.managing.project.teamsdocument.meetings.via.multiple.ways)
table(survey.data$workarounds.managing.project.teamsother.specify)