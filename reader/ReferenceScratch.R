

##create header vector
header <- colnames(categoryCoOccurence)

##check diagonal of the coOccurence matrix with diag()
sum(diag(as.matrix(categoryCoOccurence)))

##check diagonal of the coOccurence matrix with vectorization
nonzeroIndexes <- which(categoryCoOccurence[,1:1240]>0)
diagonalIndexes <- 1240 * 0:1239 + 1:1240
diagonalIndexes[diagonalIndexes %in% nonzeroIndexes]

##get the sum of co-occurence of a feature with others
apply(categoryCoOccurence[,1:1240],2,FUN=sum)

##sort th same in descending order
sort(apply(categoryCoOccurence[,1:1240],2,FUN=sum),decreasing = TRUE)

##Find the most associated features - Restaurants
nonZeroEntries <- categoryCoOccurence$Restaurants > 0
totalCoOccurenceCount <- sum(categoryCoOccurence$Restaurants[nonZeroEntries])
summaryStatistics <- summary(categoryCoOccurence$Restaurants[nonZeroEntries])
totalCoOccurenceCountTailDist <- sum(categoryCoOccurence$Restaurants[categoryCoOccurence$Restaurants > summaryStatistics["3rd Qu."]])
totalCoOccurenceCountTailDist  / totalCoOccurenceCount

RestaurntsFeatureGroup <- header[categoryCoOccurence$Restaurants > summaryStatistics["3rd Qu."]]


##Find the most associated features - Shopping
nonZeroEntries <- categoryCoOccurence$Shopping > 0
totalCoOccurenceCount <- sum(categoryCoOccurence$Shopping[nonZeroEntries])
summaryStatistics <- summary(categoryCoOccurence$Shopping[nonZeroEntries])
totalCoOccurenceCountTailDist <- sum(categoryCoOccurence$Shopping[categoryCoOccurence$Shopping > summaryStatistics["3rd Qu."]])
totalCoOccurenceCountTailDist  / totalCoOccurenceCount

ShoppingFeatureGroup <- header[categoryCoOccurence$Shopping > summaryStatistics["3rd Qu."]]


header[categoryCoOccurence$Shopping > summaryStatistics["3rd Qu."]]

##Find the most associated features - Food

View(table(attribute$name))

dim(table(attribute$name))


boxplot(business$stars ~ business$is_open)
pie(table(business$is_open),labels = c("closed","open"))


