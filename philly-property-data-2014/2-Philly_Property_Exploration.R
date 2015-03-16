# Let's get rid of the data we don't need
rm("OPA.2014", "OPA.2015")

# Now let's see the value changes
OPA.Data$Market_Value_Change_14_15 <- OPA.Data$Market_Value_2015 - OPA.Data$Market_Value_2014

summary(OPA.Data)
OPA.Data <- OPA.Data[order(OPA.Data$Market_Value_Change_14_15), ]
OPA.Data <- OPA.Data[order(-OPA.Data$Market_Value_Change_14_15), ]

OPA.Change.Mean <- mean(OPA.Data$Market_Value_Change_14_15)
OPA.Change.StdDev <- sd(OPA.Data$Market_Value_Change_14_15)
OPA.Change.Ceiling <- OPA.Change.Mean + 3*(OPA.Change.StdDev)
OPA.Change.Floor <- OPA.Change.Mean - 3*(OPA.Change.StdDev)

OPA.Data.Normal <- subset(OPA.Data, (OPA.Data$Market_Value_Change_14_15 < OPA.Change.Ceiling & OPA.Data$Market_Value_Change_14_15 > OPA.Change.Floor))

summary(OPA.Data.Normal)
hist(OPA.Data.Normal$Market_Value_Change_14_15, breaks = 100)
OPA.Data.Normal$NoChange <- (OPA.Data.Normal$Market_Value_Change_14_15 == 0)
OPA.Data.Normal$NoChange
table(OPA.Data.Normal$NoChange)

# So, nearly all of them had no year-to-year change.