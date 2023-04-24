#environment+database setup
library(dplyr)
library(stargazer)
gov<-read.csv("https://raw.githubusercontent.com/Migueldesanta/309project/main/gov.csv")
edu<-read.csv("https://raw.githubusercontent.com/Migueldesanta/309project/main/education.csv")
Merged<-merge(x=gov, y=edu,by = "countryname", all=TRUE)
Merged <- Merged %>%mutate_at(vars(rol,dec,govex,ex,sl,coc,ge,psaaov,rq,vaa,gdpper), as.numeric)
summary(Merged)

#The appropriate measures of central tendency (mean/median/mode) of each variable
selected_columns <- c("rol","govex","ex","gdpper","dec","coc","ge","psaaov","rq","vaa","sl")

appropriate_central_tendency <- function(column) {
  mean_val <- NULL
  median_val <- NULL
  mode_val <- NULL
  
  if (is.numeric(column)) {
    mean_val <- round(mean(column, na.rm = TRUE), digits = 2)
    median_val <- round(median(column, na.rm = TRUE), digits = 2)
    
    frequency_table <- table(column)
    mode_val <- as.numeric(names(frequency_table)[which.max(frequency_table)])
    mode_val <- round(mode_val, digits = 2)
  } else if (is.factor(column) || is.character(column)) {
    frequency_table <- table(column)
    mode_val <- names(frequency_table)[which.max(frequency_table)]
  }
  
  return(list(mean = mean_val, median = median_val, mode = mode_val))
}
result <- lapply(Merged[selected_columns], appropriate_central_tendency)


#histogram
hist(Merged$rol, main = "Histogram of Rule of Law: Estimate",
     xlab = "Rule of Law: Estimate", ylab = "Frequency", col = "blue", ylim = c(0, 60))
hist(Merged$sl, main = "Histogram of School Life Expectancy",
     xlab = "School Life Expectancy (years)", ylab = "Frequency", col = "blue", breaks = seq(5, 20, by = 1),ylim = c(0, 40))
max(Merged$rol)

#scatterplot
plot(Merged$rol, Merged$sl, 
     main = "Relationship between Rule of Law and School life expectancy",
     xlab = "Rule of Law",
     ylab = "School life expectancy")
model <- lm(Merged$sl~ Merged$rol, data = Merged)
abline(model, col = "blue")
coefficients(model)
a <- coefficients(model)["x"]
b <- coefficients(model)["(Intercept)"]
cat("y =", a, "* x +", b)

#Measures of dispersion for each variable
selected_columns1 <- c("rol","govex","ex","gdpper","dec","coc","ge","psaaov","rq","vaa","sl")
dispersion_measures <- function(column) {
  if (is.numeric(column)) {
    range_val <- round(max(column, na.rm = TRUE) - min(column, na.rm = TRUE), digits = 2)
    iqr_val <- round(IQR(column, na.rm = TRUE), digits = 2)
    variance_val <- round(var(column, na.rm = TRUE), digits = 2)
    sd_val <- round(sd(column, na.rm = TRUE), digits = 2)
    
    return(list(range = range_val, iqr = iqr_val, variance = variance_val, sd = sd_val))
  } else {
    return(NULL)
  }
}

result1 <- lapply(Merged[selected_columns1], dispersion_measures)

#A measure of association (correlation, cross-tab, etc.) describing the relationship between your main dependent variable and key independent variable(s)
#correlation
correlation_coefficient <- cor(Merged$rol, Merged$sl, method = "pearson", use = "complete.obs")
print(paste0("Pearson correlation coefficient: ", round(correlation_coefficient, digits = 2)))

result2 <- cor.test(Merged$rol, Merged$sl)
print(result2)
summary(result2)
cor_coefficient <- result2$estimate
t_statistic <- result2$statistic
p_value <- result2$p.value
conf_interval <- result2$conf.int
print(result2$p.value)
print(result2)
cat("Correlation Coefficient:", cor_coefficient, "\n")
cat("T-test Statistic:", t_statistic, "\n")
cat("P-value:", p_value, "\n")
cat("Confidence Interval:", conf_interval, "\n")


#A bivariate hypothesis test evaluating the relationship between your main dependent variable and key independent variable(s)

regression1<-lm(formula = sl ~rol, data =
                  Merged)

#multivariate linear regression

regression2<-lm(formula = sl ~rol+govex+ex+gdpper+dec, data =
                  Merged)

regression3<-lm(formula = sl ~rol+coc+ge+psaaov+rq+vaa, data =
                  Merged)

regression4<-lm(formula = sl ~rol+govex+ex+gdpper+dec+coc+ge+psaaov+rq+vaa, data =
                  Merged)
#regression table
stargazer(regression1,regression2,regression3,regression4,type= "text", title= "Linear Regression Table", 
          covariate.labels=c("Rule of Law: Estimate", "Government expenditure on education, US$ (millions)", 
                             "Expenditure on education as % of total government expenditure (%)","GDP per capita (current US$)",
"Duration of compulsory education (years)","Control of Corruption: Estimate","Government Effectiveness: Estimate","Political Stability and Absence of Violence/Terrorism: Estimate",
"Regulatory Quality: Estimate"," Voice and Accountability: Estimate" ), dep.var.labels=" School life expectancy")

#predict
#model1
predict(regression1, newdata = data.frame(rol = max(Merged$rol, na.rm = TRUE)))
predict(regression1, newdata = data.frame(rol = min(Merged$rol, na.rm = TRUE)))
#model2
predict(regression2, newdata = data.frame(rol = max(Merged$rol, na.rm = TRUE),govex = max(Merged$govex, na.rm = TRUE),
ex= max(Merged$ex, na.rm = TRUE),gdpper = max(Merged$gdpper, na.rm = TRUE),dec= max(Merged$dec, na.rm = TRUE))) 
predict(regression2, newdata = data.frame(rol = min(Merged$rol, na.rm = TRUE),govex = min(Merged$govex, na.rm = TRUE),
                                          ex= min(Merged$ex, na.rm = TRUE),gdpper = min(Merged$gdpper, na.rm = TRUE),dec= min(Merged$dec, na.rm = TRUE))) 
#model3
predict(regression3, newdata = data.frame(rol = max(Merged$rol, na.rm = TRUE),coc = max(Merged$coc, na.rm = TRUE),
                                          ge= max(Merged$ge, na.rm = TRUE),psaaov= max(Merged$psaaov, na.rm = TRUE),rq= max(Merged$rq, na.rm = TRUE),
                                          vaa= max(Merged$vaa, na.rm = TRUE))) 
predict(regression3, newdata = data.frame(rol = min(Merged$rol, na.rm = TRUE),coc = min(Merged$coc, na.rm = TRUE),
                                          ge= min(Merged$ge, na.rm = TRUE),psaaov= min(Merged$psaaov, na.rm = TRUE),rq= min(Merged$rq, na.rm = TRUE),
                                          vaa= min(Merged$vaa, na.rm = TRUE))) 
#model4
predict(regression4, newdata = data.frame(rol = max(Merged$rol, na.rm = TRUE),coc = max(Merged$coc, na.rm = TRUE),
                                          ge= max(Merged$ge, na.rm = TRUE),psaaov= max(Merged$psaaov, na.rm = TRUE),rq= max(Merged$rq, na.rm = TRUE),
                                          vaa= max(Merged$vaa, na.rm = TRUE),rol = max(Merged$rol, na.rm = TRUE),govex = max(Merged$govex, na.rm = TRUE),
                                          ex= max(Merged$ex, na.rm = TRUE),gdpper = max(Merged$gdpper, na.rm = TRUE),dec= max(Merged$dec, na.rm = TRUE))) 
predict(regression4, newdata = data.frame(rol = min(Merged$rol, na.rm = TRUE),coc = min(Merged$coc, na.rm = TRUE),
                                          ge= min(Merged$ge, na.rm = TRUE),psaaov= min(Merged$psaaov, na.rm = TRUE),rq= min(Merged$rq, na.rm = TRUE),
                                          vaa= min(Merged$vaa, na.rm = TRUE),rol = min(Merged$rol, na.rm = TRUE),govex = min(Merged$govex, na.rm = TRUE),
                                          ex= min(Merged$ex, na.rm = TRUE),gdpper = min(Merged$gdpper, na.rm = TRUE),dec= min(Merged$dec, na.rm = TRUE))) 


