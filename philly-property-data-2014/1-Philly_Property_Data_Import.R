# This Script will do some analysis on Philly Property Values

OPA.2014 <- read.csv("~/Desktop/Tax Year 2014  Download File/OPA 2014 Certified Static Download File.txt")
OPA.2014$Year <- 2014
OPA.2015 <- read.csv("~/Desktop/Tax Year 2015  Download File/OPA 2015 Certified Static Download File.txt")
OPA.2015$Year <- 2015

# This is one way to do it, but the files each have pretty much the same records in them, so we won't do it this way.
# OPA.Certified.Data <- rbind(OPA.2014, OPA.2015)

# So let's get some column names set up.
column_Names <- names(OPA.2014)
column_Names <- c("Account_Number", "Address", "Unit", "Homestead_Exemption_2014",
                  "Property_Category_2014", "Property_Type_2014", "Market_Value_2014",
                  "Taxable_Land_2014", "Taxable_Building_2014", "Exempt_Land_2014",
                  "Exempt_Building_2014", "Longitude", "Latitude", "Year")
names(OPA.2014) <- column_Names
column_Names <- c("Account_Number", "Address", "Unit", "Homestead_Exemption_2015",
                  "Property_Category_2015", "Property_Type_2015", "Market_Value_2015",
                  "Taxable_Land_2015", "Taxable_Building_2015", "Exempt_Land_2015",
                  "Exempt_Building_2015", "Longitude", "Latitude", "Year")
names(OPA.2015) <- column_Names
OPA.Data <- NULL

# Now we need to sort the two data files appropriately
OPA.2014 <- OPA.2014[order(OPA.2014$Account_Number),]
OPA.2015 <- OPA.2015[order(OPA.2015$Account_Number),]
# Now we'll merge them by account number, which works when they're sorted.
OPA.Data <- merge(OPA.2014, OPA.2015, by.x = "Account_Number", by.y = "Account_Number")

# Now let's check some of the potentially duplicate colums
# Let's get rid of some of the duplicate columns.
OPA.Data$Address.y <- NULL
OPA.Data$Unit.y <- NULL
OPA.Data$Longitude.y <- NULL
OPA.Data$Latitude.y <- NULL
OPA.Data$Year.y <- NULL
OPA.Data$Year.x <- NULL

mytable <- table(OPA.Data$Homestead_Exemption_2015)
mytable
mytable <- table(OPA.Data$Homestead_Exemption_2015, OPA.Data$Homestead_Exemption_2014)
mytable

# so we can remove the second Homestead Exemption
OPA.Data$Homestead_Exemption_2015 <- NULL

mytable <- table(OPA.Data$Property_Type_2015, OPA.Data$Property_Type_2014)
mytable

# so we can remove the other property type
OPA.Data$Property_Type_2015 <- NULL


mytable <- table(OPA.Data$Property_Category_2015, OPA.Data$Property_Category_2014)
mytable

# so we can remove the other property category
OPA.Data$Property_Category_2015 <- NULL

