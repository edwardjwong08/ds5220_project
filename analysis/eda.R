# DS 5220 Project - Exploratory Analysis

c1 <- rgb(red=0,green=0.1,blue=0.9,alpha=0.4)
c2 <- rgb(red=0.9,green=0,blue=0.05,alpha=0.4)

headers = read.csv("default of credit card clients.csv", header = F, nrows = 1, as.is = T)
default = read.csv("default of credit card clients.csv", skip = 2, header = F)
headers[1] <- 'ID'
colnames(default)= headers
print(head(default))


# Create a table of counts for each value (0 and 1)
counts <- table(default$Y)

# Create the bar plot
barplot(counts, 
        main = "Frequency of Credit Default Outcomes the Following Month", 
        xlab = "Credit Default the Following Month", 
        ylab = "Frequency", 
        col = c(c1,c2), 
        names.arg = c("No", "Yes"),
        ylim = c(0,25000))
