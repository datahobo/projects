# let's explore the set data some, shall we?
library(ggplot2)

brickSets$legoID <- paste0(brickSets$number, "-", brickSets$numberVariant)
# how many sets have prices?
table(brickSets$USRetailPrice)
round(table(!is.na(brickSets$USRetailPrice)) / nrow(brickSets) * 100, 0)
brickSetsPriced <- brickSets[!is.na(brickSets$USRetailPrice), ]
brickSetsPriceIds <- data.frame(brickSetsPriced$legoID)
names(brickSetsPriceIds) <- "SetID"
brickSetsPriceIds$brickset <- "brickset"
# peeron
table(is.na(peeronSets$MSRP))
round(table(is.na(peeronSets$MSRP)) / nrow(peeronSets) * 100, 0)
peeronSetsPriced <- peeronSets[!is.na(peeronSets$MSRP), ]
peeronSetsPriceIds <- data.frame(peeronSetsPriced$SetID)
names(peeronSetsPriceIds) <- "SetID"
peeronSetsPriceIds$peeron <- "peeron"
# now match them
bothPriced <- merge(brickSetsPriceIds, peeronSetsPriceIds)
table(bothPriced$peeron)
# what's our match rate?
nrow(bothPriced) / nrow(brickSetsPriceIds)
nrow(bothPriced) / nrow(peeronSetsPriceIds)
# brickset is still much better to use, even though the pricing ratio is the same.
nrow(bothPriced) - nrow(peeronSetsPriceIds)
# losing 223 sets, not bad
rm(brickSetsPriceIds, peeronSetsPriceIds, bothPriced)
# explore the brickSet set list
tapply(brickSetsPriced$USRetailPrice, brickSetsPriced$year, mean)
# scatter the year, price, number of pieces, number owned, number wanted, number of reviews, rating
ggplot(brickSetsPriced, aes(x = year, y = USRetailPrice)) +
  geom_point(shape = 1)
ggplot(brickSetsPriced, aes(x = year, y = pieces)) +
  geom_point(shape = 1)
cor(brickSetsPriced$USRetailPrice, brickSetsPriced$pieces, use = "complete.obs")
ggplot(brickSetsPriced, aes(x = USRetailPrice, y = pieces)) +
  geom_point(shape = 1)
