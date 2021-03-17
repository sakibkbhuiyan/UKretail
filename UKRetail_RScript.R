
#************************************************

# Load libraries 

#************************************************


library(corrplot)
library(cluster)
library(ggplot2)
library(ClassDiscovery)
library(readxl)

dev.new(width=8, height=12, unit="in")


#************************************************

# Clean up the dataset & get it ready 
# for analysis.

#************************************************


eretail = read_excel("Online Retail.xlsx")
dim(eretail)
names(eretail)

eretail = eretail[eretail$Country != "Unspecified",] # remove 'unspecified' country
eretail = eretail[eretail$Quantity > 0,]             # remove returns/cancellations

IDtab = table(eretail$Country, eretail$CustomerID)   # crosstab country by customer ID
IDtab = apply(IDtab >0, 2, sum)                      # is any customer ID duplicated across countries?
duplicateIDs = names(IDtab[IDtab > 1])               # duplicate IDs to clean up
eretail = eretail[!is.element(eretail$CustomerID, duplicateIDs),]
rm(IDtab)

eretail$InvoiceMth = substr(eretail$InvoiceDate, 1, 7)         # extract month of invoice
eretail = eretail[as.vector(eretail$InvoiceMth) != "2011-12",] # remove December 2011 as it only covers first week

eretail$Amount = eretail$Quantity * eretail$UnitPrice           # compute amount per invoice item

eaggr = aggregate(Amount~Country+CustomerID, data=eretail, sum) # compute aggregate amount spent per customer
row.names(eaggr) = eaggr$CustomerID
eaggr = eaggr[,-2]
eaggr = cbind(eaggr, aggregate(InvoiceMth~CustomerID, data=eretail, min)[,-1]) # 1st month of customer interaction
names(eaggr)[3] = "FirstMth"
eaggr = cbind(eaggr, aggregate(InvoiceMth~CustomerID, data=eretail, max)[,-1]) # last month of cust. interaction
names(eaggr)[4] = "LastMth"

# relabel months and compute duration of customer interaction
levels(eaggr$FirstMth) = 1:12
levels(eaggr$LastMth) = 1:12
eaggr$FirstMth = as.numeric(as.vector(eaggr$FirstMth))
eaggr$LastMth = as.numeric(as.vector(eaggr$LastMth))
eaggr$Months = eaggr$LastMth - eaggr$FirstMth + 1

eaggr = cbind(eaggr, apply( table(eretail$CustomerID, eretail$InvoiceMth) , 1, sum ) )
names(eaggr)[6] = "Purchases"

# Some useful statistics 
eaggr$Amount.per.Purchase = eaggr$Amount / eaggr$Purchases
eaggr$Purchases.per.Month = eaggr$Purchases / eaggr$Months
eaggr$Amount.per.Month = eaggr$Amount / eaggr$Months

eaggr[1:30,]


# Export data frames to CSV
write.csv(eretail, "eretail.csv")
write.csv(eaggr, "eaggr.csv")


#************************************************

# Exploratory Data Analysis

#************************************************

eaggr_quant = eaggr[, -1]

# Convert to numeric variables 
eaggr_quant$Purchases = as.numeric(eaggr_quant$Purchases)
str(eaggr_quant)

# Correlation plot of eaggr data frame (quantitative variables )
corr = cor(eaggr_quant, method = "pearson")

# Correlation plot 
corrplot(corr, 
         type = "upper",
         method = "square", 
         outline = T, 
         order = "alphabet")


# Create data frame that will be considered in clustering algorithms
X = eaggr[, c(1, 2, 4, 5, 7:9)]

# Convert Country to factor
X$Country = as.factor(X$Country)
str(X)

# Correlation Plot
X.corr = cor(X[, -1], method = "pearson")
corrplot(X.corr, 
         type = "upper",
         method = "square", 
         outline = T, 
         order = "alphabet")


# Scale the variabales 
X.scale = X
X.scale[, -1] = scale(X.scale[, -1])
str(X.scale)


# Summary statistics of eaggr data frame 
summary(eaggr)

# Histograms 
hist.Amount = qplot(X$Amount,
                    geom="histogram",
                    binwidth = 200, 
                    main = "Amount", 
                    xlab = "Amount",  
                    fill=I("deepskyblue"), 
                    col=I("navy"), 
                    alpha=I(.2), 
                    xlim = c(0, 15000))

hist.AmountperPurchase = qplot(X$Amount.per.Purchase,
                               geom="histogram",
                               binwidth = 5, 
                               main = "Amount.per.Purchase", 
                               xlab = "Amount.per.Purchase",  
                               fill=I("deepskyblue"), 
                               col=I("navy"), 
                               alpha=I(.2), 
                               xlim = c(0, 300))
hist.PurchasesperMonth = qplot(X$Purchases.per.Month,
                               geom="histogram",
                               binwidth = 2, 
                               main = "Purchases.per.Month", 
                               xlab = "Purchases.per.Month",  
                               fill=I("deepskyblue"), 
                               col=I("navy"), 
                               alpha=I(.2), 
                               xlim = c(0, 100))

hist.AmountperMonth = qplot(X$Amount.per.Month,
                            geom="histogram",
                            binwidth = 25, 
                            main = "Amount.per.Month", 
                            xlab = "Amount.per.Month",  
                            fill=I("deepskyblue"), 
                            col=I("navy"), 
                            alpha=I(.2), 
                            xlim = c(0, 1500))


hist.Amount
hist.AmountperPurchase
hist.PurchasesperMonth
hist.AmountperMonth


#************************************************

# K-means clustering

#************************************************

# Use the elbow method to find the optimal number of clusters
set.seed(1234)
wcss = vector()

# For loop to test different values of "K" 
for (i in 1:10) wcss[i] = sum(kmeans(X.scale[, -1], i, nstart = 50)$withinss) # Exclude Country from k-means algorithm b/c it is categorical
plot(1:10,
     wcss,
     type = 'b',
     main = paste('Within Cluster Sum-of-squares vs # of Clusters'),
     xlab = '# of clusters',
     ylab = 'WCSS')


# Fitting K-Means to the dataset
set.seed(1234)
kmeans = kmeans(x = X.scale[, -1], centers = 5)
y_kmeans = kmeans$cluster
X$Cluster = y_kmeans
table(X$Months, X$Cluster)
table(X$LastMth, X$Cluster)
table(X$Country, X$Cluster)


# Visualising the clusters using Principal Components 
set.seed(1234)
clusplot(X.scale[, -1],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 4,
         plotchar = TRUE,
         span = TRUE,
         col.p = 	"gray60", 
         xlab = "Principal Component 1", 
         ylab = "Principal Component 2", 
         main = "Clusters: PC2 vs. PC1"
)


#************************************************

# Hierarchical clustering

#************************************************

# Using the dendrogram to find the optimal number of clusters
dendrogram = hclust(d = dist(X.scale[, -1], method = 'euclidean'), method = 'complete')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean distances',
     labels = F, 
     sub="")


# Fitting Hierarchical Clustering to the dataset
set.seed(1234)
hc = hclust(d = dist(X.scale[, -1], method = 'euclidean'), method = 'complete')

# Cut the dendogram to 4 clusters 
y_hc = cutree(hc, 4)

X$Cluster.HC = y_hc
table(X$Months, X$Cluster.HC)
table(X$LastMth, X$Cluster.HC)
table(X$Country, X$Cluster.HC)

# Visualising the clusters using Principal Components 
set.seed(1234)
clusplot(X.scale[, -1],
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 4,
         plotchar = TRUE,
         span = TRUE,
         col.p = 	"gray60", 
         xlab = "Principal Component 1", 
         ylab = "Principal Component 2", 
         main = "Clusters: PC2 vs. PC1"
)
