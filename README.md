library(tidyverse)
library(ggplot2)


1.To read data set

OSE <- read.csv("Onlineshoppingnew.csv")
attach(OSE)
view(OSE)

2.Check for missing values in the entire data set
missing_data <- sum(is.na(OSE))
print(paste("Total missing data:", missing_data))

3. Check for missing values in specific columns
missing_per_column <- data.frame(colSums(is.na(OSE)))
print(missing_per_column)

4.check null values in the data set
null_data <- sum(is.null(OSE))
print(paste("Total null data:", null_data))

5.Assign numerical values for ship mode

ship_mode_new <- c("Delivery Truck"=1,"Express Air"=2,"Regular Air"=3)
OSE$ship_mode_numerical <- ship_mode_new[OSE$SHIPMODE]

Customer_segment_new <- c("Consumer"=1,"Corporate"=2,"Home Office"=3,"Small Business"=4)
OSE$customer_segment_numerical <- Customer_segment_new[OSE$CUSTOMERSEGMENT]

product_category_new <- c("Furniture"=1,"Office Supplies"=2,"Technology"=3)
OSE$product_category_numerical <- product_category_new[OSE$PRODUCTCATEGORY]

6.Model for original data set
model<-lm(SALES~DISCOUNT+ship_mode_numerical+UNITPRICE+SHIPPINGCOSTS+customer_segment_numerical+product_category_numerical+SHIPPINGDURATION,data = OSE)
model
summary(model)
Before <- summary(model)$r.squared

7.Use log for dependent
OSE$SALES_LOG <- log(OSE$SALES)
OSE$SALES_LOG


8.New model with log(sales)
model2<-lm(SALES_LOG~DISCOUNT+ship_mode_numerical+UNITPRICE+SHIPPINGCOSTS+customer_segment_numerical+product_category_numerical+SHIPPINGDURATION,data = OSE)
model2
summary(model2)
After <- summary(model2)$r.squared
After 


9.Get the model residuals
model_residuals = model2$residuals
model_residuals
     # Plot the histogram
hist(model_residuals,type = "s") 
hist(model$residuals,
 	main = "Histogram of model residuals [(sales growth)]",
 	xlab = "Residuals",
 	ylab = "Frequency",
 	breaks = 30,
 	ylim = c(0, 0.40),
 	col = "lightblue",
 	probability = TRUE)  # Set probability = TRUE for density
     # Compute the density
density_data <- density(model2$residuals)
    # Add a normal distribution fit line
x <- seq(min(model2$residuals), max(model2$residuals), length = 100)
y <- dnorm(x, mean = mean(model2$residuals), sd = sd(model2$residuals))
lines(x, y, col = "black",lwd=2)

10.Get the model2 residuals
model2_residuals = model2$residuals
model2_residuals
# Plot the histogram
hist(model2_residuals,type = "s")
 hist(model2$residuals,
 	main = "Histogram of model residuals [log(sales growth)]",
 	xlab = "Residuals",
 	ylab = "Frequency",
 	breaks = 30,
 	ylim = c(0, 0.40),
 	col = "lightblue",
 	probability = TRUE)  # Set probability = TRUE for density
 
# Compute the density
density_data <- density(model2$residuals)
# Add a normal distribution fit line
x <- seq(min(model2$residuals), max(model2$residuals), length = 100)
y <- dnorm(x, mean = mean(model2$residuals), sd = sd(model2$residuals))
lines(x, y, col = "black",lwd=2)

10. Q-Q plot
  data <- rnorm(100)  # Replace with your actual data
  # Normal Q-Q plot for normality check
qqnorm(model2_residuals, main = "Normal Q-Q Plot (After get log)", xlab = "Quantiles of Data", ylab = "Quantiles of Standard Normal")
qqline(data, col = "red", linetype = "dashed")  # Add reference line with customization
 
11.Linearity
# Create scatter plot for independent
 library(ggplot2)
# Define the plot DISCOUN
plot_DISCOUNT <- ggplot(OSE , aes(x =DISCOUNT, y = SALES_LOG)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
	title = "Scatter Plot of SALES_LOG vs DISCOUNT",
	x = "DISCOUNT",
	y = "SALES_LOG"
  ) +
  theme_minimal()
plot_DISCOUNT
 # Define the plot UNITPRICE
plot_UNITPRICE <- ggplot(OSE , aes(x =UNITPRICE, y = SALES_LOG)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
	title = "Scatter Plot of SALES_LOG vs UNITPRICE",
	x = "UNITPRICE",
	y = "SALES_LOG"
  ) +
  theme_minimal()
plot_UNITPRICE
 
 # Define the plot SHIPPINGCOST
plot_SHIPPINGCOST <- ggplot(OSE , aes(x =SHIPPINGCOSTS, y = SALES_LOG)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
	title = "Scatter Plot of SALES_LOG vs SHIPPINGCOST",
	x = "SHIPPINGCOST",
	y = "SALES_LOG"
  ) +
  theme_minimal()
plot_SHIPPINGCOST

 # Define the plot SHIPPINGDURATION
 
plot_SHIPPINGDURATION<- ggplot(OSE , aes(x =SHIPPINGDURATION, y = SALES_LOG)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
	title = "Scatter Plot of SALES_LOG vs SHIPPINGDURATION",
	x = "SHIPPINGDURATION",
	y = "SALES_LOG"
  ) +
  theme_minimal()
plot_SHIPPINGDURATION

12.Draw histrogram
hist(OSE$DISCOUNT,
 	main = "Histogram of DISCOUNT",
 	xlab = "DISCOUNT",
 	ylab = "Frequency",
 	breaks = 12,
 	col = "lightblue",
 	probability = TRUE)  # Set probability = TRUE for density
 
# Compute the density
density_data <- density(OSE$DISCOUNT)
 
# Add a normal distribution fit line
x <- seq(min(OSE$DISCOUNT), max(OSE$DISCOUNT), length = 100)
y <- dnorm(x, mean = mean(OSE$DISCOUNT), sd = sd(OSE$DISCOUNT))
lines(x, y, col = "red",lwd=2)
 
hist(OSE$UNITPRICE,
 	main = "Histogram of UNITPRICE",
 	xlab = "UNITPRICE",
 	ylab = "Frequency",
 	breaks = 30,
	
 	col = "lightblue",
 	probability = TRUE)  # Set probability = TRUE for density
 
# Compute the density
density_data <- density(OSE$UNITPRICE)
 
# Add a normal distribution fit line
x <- seq(min(OSE$UNITPRICE), max(OSE$UNITPRICE), length = 100)
y <- dnorm(x, mean = mean(OSE$UNITPRICE), sd = sd(OSE$UNITPRICE))
lines(x, y, col = "red",lwd=2)
 
hist(OSE$SHIPPINGCOSTS,
 	main = "Histogram of SHIPPINGCOSTS",
 	xlab = "SHIPPINGCOSTS",
 	ylab = "Frequency",
 	breaks = 30,
	
 	col = "lightblue",
 	probability = TRUE)  # Set probability = TRUE for density
 
# Compute the density
density_data <- density(OSE$SHIPPINGCOSTS)
 
# Add a normal distribution fit line
x <- seq(min(OSE$SHIPPINGCOSTS), max(OSE$SHIPPINGCOSTS), length = 100)
y <- dnorm(x, mean = mean(OSE$SHIPPINGCOSTS), sd = sd(OSE$SHIPPINGCOSTS))
lines(x, y, col = "red",lwd=2)
 
 

13.multicollinearity
# Compute correlation matrix
correlation_matrix <- cor(OSE[, c("UNITPRICE","SHIPPINGCOSTS", "DISCOUNT","SHIPPINGDURATION","SALES_LOG")])
print(correlation_matrix)

