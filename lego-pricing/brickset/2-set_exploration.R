# let's explore the set data some, shall we?

# how many sets have prices?
table(setsByYear$USRetailPrice)
length(table(setsByYear$USRetailPrice))
table(setsByYear$USRetailPrice == '')
round(table(setsByYear$USRetailPrice == '') / nrow(setsByYear) * 100, 0)
