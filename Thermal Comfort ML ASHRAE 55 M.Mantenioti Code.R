#Loading the data

library(tidyverse)

ashraedb201<- read_csv("C:/Users/Mante/OneDrive/Υπολογιστής/Kaggle Stuff/kaggleashraecomfort.zip")

ashraedb201 <- ashraedb201[!(ashraedb201$`Thermal comfort`== "1.3"),]

View(ashraedb201)

table(ashraedb201$`Thermal comfort`)

# ASHRAE has provided 6 pointer grading scale for thermal comfort, which
# ranges from 1(very uncomfortable) to 6 (very comfortable), so we convert
# these datapoints into integers (1-6)

# Cleaning the data

colSums(is.na(ashraedb201))


#picking the variables we will use

columns <- c("Thermal sensation", "Thermal sensation acceptability",
             "Thermal preference", "Thermal comfort", "PMV", "PPD", "SET",
             "Air movement acceptability", "Air movement preference",
             "Air temperature (C)", "Relative humidity (%)", "Humidity sensation",
             "Cooling startegy_building level", "Heating strategy_building level",
             "Radiant temperature (C)", "Air velocity (m/s)",
             "Met", "Clo", "Operative temperature (C)", "Building type",
             "Season", "City", "Country", "Climate", "Year")

data <- ashraedb201[, columns]

View(data)

colSums(is.na(data))

data1<- data[!apply(is.na(data),1, all),]
View(data1)

colSums(is.na(data1))

colSums(is.na(data1)) / nrow(data1) * 100

# Υπολογίζουμε το ποσοστό των missing values για κάθε στήλη
missing_percentage <- colSums(is.na(data1)) / nrow(data1) * 100

# Επιλογή μόνο των στηλών με λιγότερο (έστω) από 80% έλλειψη
data_clean <- data1[, missing_percentage <= 80]

# Έλεγχος των αποτελεσμάτων
colSums(is.na(data_clean))
View(data_clean)

colSums(is.na(data_clean)) / nrow(data_clean) * 100



# Φιλτράρουμε τις γραμμές με περισσότερες από 50% κενές τιμές
threshold <- 0.5
data_clean_filtered <- data_clean[!apply(is.na(data_clean), 1, function(x) mean(is.na(x)) > threshold), ]

# Εμφανίζουμε τις διαστάσεις του καθαρισμένου πια συνόλου δεδομένων
dim(data_clean_filtered)

View(data_clean_filtered)

# Αφαίρεση γραμμών που περιέχουν missing values στη στήλη 'Season'
data_clean_filtered <- data_clean_filtered[!is.na(data_clean_filtered$Season), ]

# Ελέγχουμε αν οι γραμμές με missing values στο 'Season' έχουν αφαιρεθεί όπως ορίσαμε
colSums(is.na(data_clean_filtered))

# Εντολή για να μετατρέψουμε όλες τις character μεταβλητές σε factors
data_clean_filtered[sapply(data_clean_filtered, is.character)] <- 
  lapply(data_clean_filtered[sapply(data_clean_filtered, is.character)], as.factor)

# Ελέγχουμε αν οι μεταβλητές έχουν μετατραπεί σε factors
str(data_clean_filtered)

# Μετατροπή της "Thermal sensation acceptability" σε factor
data_clean_filtered$`Thermal sensation acceptability` <- 
  factor(data_clean_filtered$`Thermal sensation acceptability`, levels = c(0, 1), labels = c("No", "Yes"))

# Ελέγχουμε τη δομή του dataset για να βεβαιωθούμε για τη μετατροπή
str(data_clean_filtered)

# Εφαρμογή της μετατροπής για την μεταβλητή "Thermal comfort" και στρογγυλοποίηση σε ακέραιο
min_value <- min(data_clean_filtered$`Thermal comfort`, na.rm = TRUE)
max_value <- max(data_clean_filtered$`Thermal comfort`, na.rm = TRUE)

data_clean_filtered$`Thermal comfort` <- 
  1 + ((data_clean_filtered$`Thermal comfort` - min_value) * (6 - 1)) / (max_value - min_value)

# Στρογγυλοποίηση σε ακέραιες τιμές
data_clean_filtered$`Thermal comfort` <- round(data_clean_filtered$`Thermal comfort`)

# Ελέγχουμε τις πρώτες τιμές για να βεβαιωθούμε
head(data_clean_filtered$`Thermal comfort`)

# Στρογγυλοποίηση προς τα κάτω
data_clean_filtered$`Thermal comfort` <- floor(data_clean_filtered$`Thermal comfort`)


# HANDLING MISSING VALUES

# Φιλτράρισμα για καταχωρήσεις του San Francisco
san_francisco_data <- data_clean_filtered[data_clean_filtered$City == "San Francisco", ]

# Εμφάνιση του φιλτραρισμένου πίνακα δεδομένων (είχα μια υποψία ότι οι καταχωρήσεις του SF είναι κυρίως ελλιπείς)
View(san_francisco_data)
colSums(is.na(san_francisco_data))

colSums(is.na(san_francisco_data)) / nrow(san_francisco_data) * 100


# MICE IMPUTATION: Προτιμήθηκε Multiple Imputation by Chained Equations γιατί συμπληρώνει τις κενές τιμές χρησιμοποιώντας τις υπόλοιπες συμπληρωμένες τιμές μεταβλητών για "πρόβλεψη" 

library(mice)

# Επιλογή μεταβλητών για το imputation: 
vars_for_imputation <- c("Thermal sensation",
                         "Thermal sensation acceptability",
                         "Thermal preference",
                         "Thermal comfort",
                         "PMV",
                         "PPD",
                         "SET",
                         "Air movement preference",
                         "Air temperature (C)",
                         "Relative humidity (%)",
                         "Cooling startegy_building level",
                         "Radiant temperature (C)",
                         "Air velocity (m/s)",
                         "Met",
                         "Clo",
                         "Operative temperature (C)",
                         "Building type",
                         "Season",
                         "City",
                         "Country",
                         "Climate",
                         "Year")

# Εκτέλεση MICE. Το προτιμήσαμε διότι οι μεταβλητές που δεν λείπουν, θα λειτουργήσουν ως predictors για τις υπόλοιπες.
imputed_data <- mice(data_clean_filtered[, vars_for_imputation], method = "pmm", m = 3, seed = 123) #τυχαίο seed

completed_data <- complete(imputed_data, 1)

View(completed_data)

colSums(is.na(completed_data))

data_clean_filtered[,columns]

# δεν υπάρχουν πια missing values. Προχωράμε:

# DISTRIBUTIONS
# εδώ κάνω χρήση μιας function που είχα κατασκευάσει για προηγούμενα προβλήματα: μου δίνει το box plot, το histogram και το (αριθμητικό) skewness 
# ξαναγράφω τη σύνταξή της παρακάτω

library(e1071)

check_distribution <- function(variable, var_name = "Variable") {
  if (!requireNamespace("e1071", quietly = TRUE)) {
    install.packages("e1071")
  }
  library(e1071)
  
  par(mfrow = c(2, 2))  # 2x2 plots
  
  # 1. Box plot
  boxplot(variable,
          main = paste("Boxplot of", var_name),
          col = "lightblue",
          border = "darkblue")
  
  # 2. Histogram
  hist(variable,
       main = paste("Histogram of", var_name),
       col = "slateblue",
       border = "white",
       breaks = 30)
  
  # 3. Q-Q plot
  qqnorm(variable, main = paste("Q-Q Plot of", var_name))
  qqline(variable, col = "red")
  
  # 4. Skewness value
  sk <- skewness(variable, na.rm = TRUE)
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Skewness")
  text(1, 1, labels = paste("Skewness =", round(sk, 3)), cex = 1.5)
  
  # Reset plot layout
  par(mfrow = c(1, 1))
}

# Αρχίζουμε να ελέγχουμε κυρτότητα και outliers

check_distribution(completed_data$`Thermal sensation`, "Thermal sensation") #skewness = 0.143
check_distribution(completed_data$`Thermal comfort`,"Thermal comfort") # skewness= -0.63

check_distribution(completed_data$`PMV`,"PMV") # skewness = 0.269
check_distribution(completed_data$`PPD`,"PPD") #skewness = 0.296 most values are gathered at the edges
check_distribution(completed_data$`SET`,"SET") #skewness = 0.028, though the histogram looks odd, so does the q-q plot
hist(completed_data$`SET`)

check_distribution(completed_data$`Air temperature (C)`,"Air temperature (C)") # skewness = -1.026 but with lots of outliers, both below and over the boxplot
check_distribution(completed_data$`Relative humidity (%)`,"Relative humidity (%)") # skewness = -0.025
check_distribution(completed_data$`Radiant temperature (C)`,"Relative temperature (C)") # skewness = -0.423 

check_distribution(completed_data$`Air velocity (m/s)`,"Air velocity (m/s)") # skewness = 1.621

check_distribution(completed_data$`Met`,"Met") # skewness 3.679 with lots of outliers
check_distribution(completed_data$`Clo`,"Clo") # skewness = 1.559 with lots of outliers
check_distribution(completed_data$`Operative temperature (C)`,"Operative temperature (C)") # skewness = -0.446, weird looking histogram
check_distribution(completed_data$`Year`,"Year") # skewness = -0.597


summary(completed_data)
sort(unique(completed_data$Country))
sort(unique(completed_data$City))

#TRANSFORMATIONS (προσαρμογή της κυρτότητας)

#Log for Met
completed_data$Met_log <- log1p(completed_data$Met)

check_distribution(completed_data$`Met_log`,"Met_log") # skewness =2.908

#sqrt for Met

completed_data$Met_sqrt <- sqrt(completed_data$Met)
check_distribution(completed_data$`Met_sqrt`,"Met_sqrt") #3.109. worse (as expected, since sqrt is a more mild transformation than the log)

# Winsorize το Met σε 5th and 95th percentiles
library(DescTools)
completed_data$`Met_wins` <- Winsorize(completed_data$`Met`, probs = c(0.05, 0.95))

# Then re-check skewness
check_distribution(completed_data$`Met_wins`, "Met - Winsorized") # skewness = 0.929 with many outliers

# Let's try yeo - johnson instead

# bestNormalize
library(bestNormalize)

# Εφαρμογή Yeo-Johnson στο Met στο dataset του completed_data
yj_obj <- yeojohnson(completed_data$`Met`)
completed_data$`Met_yj` <- predict(yj_obj)

# Έλεγχος κατανομής με την υπάρχουσα συνάρτηση check_distribution
check_distribution(completed_data$`Met_yj`, "Met - Yeo-Johnson") # skewness = 0.266

# MET: από [-6, 3.5] --> [0.6, 5]
met_trans <- completed_data$`Met_yj`
met_shifted <- met_trans - min(met_trans)  # μετατόπιση ώστε να ξεκινάει από 0
met_scaled <- met_shifted / max(met_shifted)  # scaling στο [0, 1]
completed_data$`Met (transformed)` <- met_scaled * (5 - 0.6) + 0.6  # scale στο [0.6, 5]

completed_data$`Met (transformed)` <- round(completed_data$`Met (transformed)`, 1)
check_distribution(completed_data$`Met (transformed)`, "Met (transformed)") # skewness = 0.217

mean(completed_data$SET)

# Transforming again

completed_data$PPD_log <- log1p(completed_data$PPD)

check_distribution(completed_data$`PPD_log`,"PPD_log") # skewness = -0.185, q-q is symmetrical, looks like the uniform distribution could be a good fit


# Yeo - Johnson 

library(caret)

# Δημιουργία μετασχηματισμού
yj <- preProcess(completed_data[, "PPD", drop = FALSE], method = "YeoJohnson")
# Εφαρμογή του μετασχηματισμού
completed_data$PPD_transformed <- predict(yj, completed_data[, "PPD", drop = FALSE])$PPD


check_distribution(completed_data$PPD_transformed, "PPD_transformed") #skewness = -0.091

# rescalling την κλίμακα
completed_data$PPD_yj_scaled <- scales::rescale(completed_data$PPD_transformed, to = c(0, 100))

check_distribution(completed_data$PPD_yj_scaled, "PPD_yj_scaled") #-0.091


# Βρίσκουμε τις 2 ανώτατες τιμές (απέχουν ακραία από τις υπόλοιπες καταχωρήσεις(50-κάτι ενώ η αμέσως επόμενη μέγιστη είναι 7 και κάτι), σκοπεύω να τις αφαιρέσω)
top_2_values <- tail(sort(completed_data$`Air velocity (m/s)`), 2)

# Αφαιρούμε τις γραμμές που περιέχουν αυτές τις τιμές
completed_data1 <- completed_data[!(completed_data$`Air velocity (m/s)` %in% top_2_values), ]

View(completed_data1)
check_distribution(completed_data1$`Met (transformed)`, "Met (transformed)") #0.217


# Δημιουργία yj μετασχηματισμού
yj <- preProcess(completed_data1[, "Air velocity (m/s)", drop = FALSE], method = "YeoJohnson")

# Εφαρμογή του yj μετασχηματισμού
completed_data1$`Air velocity (m/s)_transformed` <- predict(yj, completed_data1[, "Air velocity (m/s)", drop = FALSE])$`Air velocity (m/s)`

# Έλεγχος κατανομής μετά τον μετασχηματισμό
check_distribution(completed_data1$`Air velocity (m/s)_transformed`, "Air velocity (m/s)_transformed") # skewness = 0.71

# Υπολογισμός του IQR για την μεταβλητή Air velocity (m/s)
# Q1 <- quantile(completed_data1$`Air velocity (m/s)`, 0.25)
# Q3 <- quantile(completed_data1$`Air velocity (m/s)`, 0.75)
#3 IQR_value <- Q3 - Q1

# Ορισμός των ορίων για τα outliers
# lower_bound <- Q1 - 1.5 * IQR_value
# upper_bound <- Q3 + 1.5 * IQR_value

# Φιλτράρισμα των γραμμών που είναι εκτός των ορίων
completed_data1_no_outliers <- completed_data1
# [completed_data1$`Air velocity (m/s)` >= lower_bound & completed_data1$`Air velocity (m/s)` <= upper_bound, ]

# Έλεγξε το μέγεθος του νέου dataset
nrow(completed_data1_no_outliers)

View(completed_data1_no_outliers)
check_distribution(completed_data1_no_outliers$`Air velocity (m/s)`, "Air velocity (m/s)") #skewness = 1.133 ! WORSE!

completed_data1_no_outliers$`Air velocity log` <- log1p(completed_data1_no_outliers$`Air velocity (m/s)`)
check_distribution(completed_data1_no_outliers$`Air velocity log`,"Air velocity log") #1.016

# Δημιουργία μετασχηματισμού
yj <- preProcess(completed_data1_no_outliers[, "Air velocity (m/s)", drop = FALSE], method = "YeoJohnson")

# Εφαρμογή του μετασχηματισμού
completed_data1_no_outliers$`Air velocity (m/s)_transformed` <- predict(yj, completed_data1_no_outliers[, "Air velocity (m/s)", drop = FALSE])$`Air velocity (m/s)`

# Έλεγχος κατανομής μετά τον μετασχηματισμό
check_distribution(completed_data1_no_outliers$`Air velocity (m/s)_transformed`, "Air velocity (m/s)_transformed") # skewness = 0.71

check_distribution(completed_data1_no_outliers$`Radiant temperature (C)`, "Radiant temperature (C)") #-0.423

check_distribution(completed_data1_no_outliers$`Clo`, "Clo") # 1.559 many outliers

# Δημιουργία μετασχηματισμού
yj <- preProcess(completed_data1_no_outliers[, "Clo", drop = FALSE], method = "YeoJohnson")

# Εφαρμογή του μετασχηματισμού
completed_data1_no_outliers$`Clo_transformed` <- predict(yj, completed_data1_no_outliers[, "Clo", drop = FALSE])$`Clo`

# Έλεγχος κατανομής μετά τον μετασχηματισμό
check_distribution(completed_data1_no_outliers$`Clo_transformed`, "Clo_transformed") # skewness = 0.07

columns1<-c("Air temperature (C)", 
            "Relative humidity (%)", 
            "Radiant temperature (C)", 
            "Met (transformed)", 
            "Clo_transformed", 
            "Operative temperature (C)", 
            "Thermal comfort", 
            "Season", 
            "City", 
            "Country", 
            "Climate")



data3<-completed_data1_no_outliers[,columns1]

View(data3)


#  είναι πρόβλημα παλινδρόμησης, εφόσον η μεταβλητή thermal comfort είναι συνεχής (κλίμακα ακέραιων 1 - 6 )
# μπορούμε να χρησιμοποιήσουμε random forest regression. Παρ'όλο που χρησιμοποιείται σε classification problems (κατηγορικές μεταβλητές υπό εξέταση),
# μπορεί να χρησιμοποιηθεί και για μοντέλο παλινδρόμησης και επιπλέον διαχειρίζεται καλά τα outliers.


# Εγκατάσταση
install.packages("randomForest")

# Φόρτωμα των βιβλιοθηκών
library(randomForest)
library(caret)


colnames(data3)


# Μετονομασία όλων των στηλών του data3 σε "ασφαλή" ονόματα (χωρίς κενά και σύμβολα)
colnames(data3) <- make.names(colnames(data3), unique = TRUE)

colnames(data3)

# Καθαρίζουμε τα ονόματα των στηλών στο data3
colnames(data3) <- gsub("\\s+", "_", colnames(data3))
colnames(data3) <- gsub("[^[:alnum:]_]", "", colnames(data3))


# checking ότι η σειρά των γραμμών είναι η ίδια:
data3$Airvelocityms_transformed <- completed_data1_no_outliers$`Air velocity (m/s)_transformed`

# Είχα ήδη κάνει clean τα ονόματα των μεταβλητών:
colnames(data3) <- gsub("\\s+", "_", colnames(data3))
colnames(data3) <- gsub("[^[:alnum:]_]", "", colnames(data3))




# Ορισμός output και input vectors (προσαρμοσμένα στα καθαρισμένα ονόματα)
output_vector <- "Thermalcomfort"  # π.χ., αν έχει γίνει make.names, τα κενά αντικαθίστανται με _
# input_vector <- c("AirtemperatureC", "Relativehumidity",
                  # "RadianttemperatureC", "Mettransformed", "Clo_transformed",
                  # "OperativetemperatureC", "Season", "City", "Country", "Climate")

# το random forest δε μπορεί να διαχειριστεί κατηγορικές μεταβλητές με παραπάνω από 53 επίπεδα, οπότε αφαιρέσαμε τις Country & City

input_vector <- c("AirtemperatureC", "Airvelocityms_transformed", "Relativehumidity",
                  "RadianttemperatureC", "Mettransformed", "Clo_transformed",
                  "OperativetemperatureC", "Season", "Climate")

# Χωρισμός των δεδομένων σε training και test set
library(caret)
set.seed(123)
trainIndex <- createDataPartition(data3[[output_vector]], p = 0.8, list = FALSE)
train_data <- data3[trainIndex, ]
test_data <- data3[-trainIndex, ]

# Εκπαίδευση μοντέλου με Random Forest
library(randomForest)
formula_rf <- as.formula(paste(output_vector, "~", paste(input_vector, collapse = " + ")))
rf_model <- randomForest(formula_rf, data = train_data, ntree = 100, importance = TRUE)

# Προβολή αποτελεσμάτων

print(rf_model)

varImpPlot(rf_model)


# θα δοκιμάσω ξανά random forest για regression μέσω ενός πιο light πακέτου (ranger)

install.packages("ranger")

library(ranger)

rf_ranger_model <-ranger(
  formula = formula_rf,
  data = train_data,
  num.trees = 100,
  max.depth = 8,
  importance = "impurity",
  write.forest = TRUE,
  seed = 123
)

print(rf_ranger_model)


# R- squared (OOB) ~ 0.248 το μοντέλο εξηγεί περίπου το 25% της διακύμανσης θερμικής άνεσης
# MSE (OOB) ~1.226 ελαφρώς χειρότερο από το προηγούμενο random forest (1.206)


# Προβλέψεις στο test set
preds <- predict(rf_ranger_model, data = test_data)$predictions

# Υπολογισμός metrics
install.packages("Metrics")
install.packages("MetricsWeighted")
library(Metrics)
library(MetricsWeighted) # αν χρειαστεί για r2

true_vals <- test_data[[output_vector]]

# Μetrics σε test set
mse_val <- mse(true_vals, preds)
rmse_val <- rmse(true_vals, preds)
r2_val <- 1 - sum((true_vals - preds)^2) / sum((true_vals - mean(true_vals))^2)

# Εκτύπωση
cat("Test Set Evaluation:\n")
cat("MSE: ", round(mse_val, 4), "\n")
cat("RMSE:", round(rmse_val, 4), "\n")
cat("R-squared:", round(r2_val, 4), "\n")

# Σταθερή απόδοση σε training και test set (το μοντέλο λοιπόν δεν έχει over-fitting)
# Το R^2 είναι ~ 0.24 (αναμενόμενο για υποκειμενικά δεδομένα (όπως η θερμική άνεση))
# RMSE = 1.113 δηλαδή η μέση απόκλιση είναι περίπου 1 μονάδα θερμικής άνεσης (όχι και τραγικό για 6βάθμια κλίμακα)

#Δεν είναι καλό μοντέλο, θα ξαναπροσπαθήσω φτιάχνοντας αλλιώς τα data sets


#^ Ας τις κάνουμε όλες αριθμητικές μεταβλητές. Έχουμε:

data4 <- data3  # Δημιουργία αντιγράφου για ασφαλείς δοκιμές

# Εύρεση κατηγορικών στηλών (factor ή character)
categorical_cols <- names(data4)[sapply(data4, function(x) is.factor(x) | is.character(x))]
print(categorical_cols)

# Διαγραφή στηλών που δεν θέλεις
columns_to_drop <- c("City", "Country", "Climate","Season")  # Π.χ., αν έχουν πολλές κατηγορίες
data4 <- data4[, !(names(data4) %in% columns_to_drop)]

sapply(data4, class)  # Να δείχνει "numeric" για όλες τις στήλες

View(data4)

# απλή γραμμική παλινδρόμηση για τεστ μόνο! Διερευνητικός σκοπός!

inp_vector = c("AirtemperatureC", "Airvelocityms_transformed", "Relativehumidity",
               "RadianttemperatureC", "Mettransformed", "Clo_transformed",
               "OperativetemperatureC")

library(dplyr)

model_lm <- lm(Thermalcomfort ~ ., data = data4[, c(inp_vector, "Thermalcomfort")])

# Περίληψη μοντέλου
summary(model_lm)

library(car)
vif(model_lm)  # Τιμές >10 θα υποδεικνύουν πρόβλημα


colnames(completed_data1_no_outliers)


# νέο dataset με τις μεταβλητές που μας αφορούν για την τελική ανάλυση:

library(dplyr)

# Δημιουργία του data5 με τις σωστές εκδοχές μεταβλητών
data5 <- completed_data1_no_outliers %>%
  select(
    # Target
    `Thermal comfort`,
    
    # Βασικές μεταβλητές (με τις σωστές εκδοχές)
    `Air temperature (C)`,
    `Air velocity log`,          # Αντί για Air velocity (m/s)_transformed
    `Relative humidity (%)`,
    `Radiant temperature (C)`,
    `Met (transformed)`,         # Αντί για Met_log, Met_wins, κλπ.
    `Clo_transformed`,           # Εδώ δεν υπάρχει διφορούμενη εκδοχή
    `Operative temperature (C)`,
    
    # Επιστημονικοί δείκτες
    PMV,
    `PPD_yj_scaled`,             # Αντί για PPD_transformed ή PPD_log
    SET,
    
    # Προτιμήσεις χρηστών
    `Thermal sensation acceptability`,
    `Thermal preference`,
    `Air movement preference`,
    
    # Προαιρετικές
    Season,
    Climate,
    `Building type`
  ) %>%
  distinct()  # Διαγραφή duplicate γραμμών (αν υπάρχουν να τις αντιμετωπίσουμε)

# Έλεγχος ονομάτων στηλών
colnames(data5)

View(data5)


# Θα κατασκευάσω 3 μοντέλα (random forest regression) 
# για να δω ποιοι παράγοντες επηρεάζουν τη θερμική άνεση περισσότερο

#ΚΑΤΑΣΚΕΥΗ ΔΙΑΝΥΣΜΑΤΩΝ ΕΙΣΟΔΟΥ

# Ομάδα Α (μόνο αισθητήρες)

input_vector_A <- c(
  "Air temperature (C)", "Air velocity log", "Relative humidity (%)",
  "Radiant temperature (C)", "Met (transformed)", "Clo_transformed",
  "Operative temperature (C)"
)

# Ομάδα Β: Αισθητήρες και επιστημονικοί δείκτες

input_vector_B <- c(input_vector_A, "PMV", "PPD_yj_scaled", "SET")

# Ομάδα Γ (ΟΛΑ!)

input_vector_C <- c(
  input_vector_B,
  "Thermal sensation acceptability", "Thermal preference", "Air movement preference",
  "Climate_encoded", "Building_type_encoded", "Season"
)


#Επαλήθευση

sapply(data5, class)

summary(data5)

# ΚΑΤΑΛΛΗΛΗ κωδικοποίηση των κατηγορικών μεταβλητών μας

# Binary variable
data5$`Thermal sensation acceptability` <- ifelse(data5$`Thermal sensation acceptability` == "yes", 1, 0)

# Ordinal Variables
data5 <- data5 %>%
  mutate(
    `Thermal preference` = case_when(
      `Thermal preference` == "warmer" ~ -1,
      `Thermal preference` == "no change" ~ 0,
      `Thermal preference` == "cooler" ~ 1
    ),
    `Air movement preference` = case_when(
      `Air movement preference` == "less" ~ -1,
      `Air movement preference` == "no change" ~ 0,
      `Air movement preference` == "more" ~ 1
    )
  )


# Nominal variables

library(caret)

# One-hot για Season (έχει <= 5 κατηγορίες)
dummy <- dummyVars(~ Season, data = data5)
season_encoded <- predict(dummy, newdata = data5)
data5 <- cbind(data5, season_encoded) %>%
  select(-Season)


# One-hot για Building type (έχει <= 5 κατηγορίες)
dummy <- dummyVars(~ `Building type`, data = data5)
Building_type_encoded <- predict(dummy, newdata = data5)
data5 <- cbind(data5, Building_type_encoded) %>%
  select(-`Building type`)


# Target encoding για Climate (πολλές κατηγορίες (15) )
library(dplyr)

target_encode <- function(data, column, target) {
  means <- data %>%
    group_by(!!sym(column)) %>%
    summarise(encoded = mean(!!sym(target), na.rm = TRUE))
  data <- data %>%
    left_join(means, by = setNames(column, column)) %>%
    rename(!!paste0(column, "_encoded") := encoded) %>%
    select(-!!sym(column))
  return(data)
}

data5 <- target_encode(data5, "Climate", "Thermal comfort")



# Επαλήθευση

sapply(data5, class)



# ΕΚΠΑΙΔΕΥΣΗ ΜΟΝΤΕΛΩΝ

# Αρχικό dataset: αντικατάσταση όλων των ειδικών χαρακτήρων και κενών με _ (δεν τα διάβαζαν, προηγουμένως, οι εντολές τα κενά τύπου space)
names(data5) <- gsub("[^[:alnum:]]", "_", names(data5))  # Αντικαθιστά ΟΛΟΥΣ τους μη αλφαβητικούς και μη αριθμητικούς χαρακτήρες 
names(data5) <- gsub("__+", "_", names(data5))  # Αντικαθιστά διπλά underscores __ με μονά
names(data5) <- gsub("^_|_$", "", names(data5))  # Αφαιρεί underscores _ από αρχή/τέλος

# Τα νέα μας ονόματα
colnames(data5)


# Ορίζουμε τα vectors με νέα ονόματα (όλα με "_" πλέον)
input_vector_A <- c(
  "Air_temperature_C",
  "Air_velocity_log",
  "Relative_humidity",
  "Radiant_temperature_C",
  "Met_transformed",
  "Clo_transformed",
  "Operative_temperature_C"
)

input_vector_B <- c(input_vector_A, "PMV", "PPD_yj_scaled", "SET")

input_vector_C <- c(
  input_vector_B,
  "Thermal_sensation_acceptability",
  "Thermal_preference",
  "Air_movement_preference",
  "Season_Autumn", "Season_Spring", "Season_Summer", "Season_Winter",
  "X_Building_type_Classroom", "X_Building_type_Office", 
  "X_Building_type_Others", "X_Building_type_Senior_center",
  "Climate_encoded"
)

# Έλεγχος ότι όλα τα ονόματα υπάρχουν στο data5
stopifnot(all(input_vector_C %in% colnames(data5)))



# ΚΑΤΑΣΚΕΥΗ ΜΟΝΤΕΛΩΝ ΓΙΑ RANDOM FOREST REGRESSION


library(ranger)

# Μοντέλο 1: Ομάδα Α
rf_model_A <- ranger(
  formula = as.formula(paste("Thermal_comfort~", paste(input_vector_A, collapse = " + "))),
  data = data5,
  num.trees = 100,
  importance = "impurity",
  seed = 123
)

# Μοντέλο 2: Ομάδα Β
rf_model_B <- ranger(
  formula = as.formula(paste("Thermal_comfort ~", paste(input_vector_B, collapse = " + "))),
  data = data5,
  num.trees = 100,
  importance = "impurity",
  seed = 123
)

# Μοντέλο 3: Ομάδα Γ
rf_model_C <- ranger(
  formula = as.formula(paste("Thermal_comfort ~", paste(input_vector_C, collapse = " + "))),
  data = data5,
  num.trees = 100,
  importance = "impurity",
  seed = 123
)

# Metrics των μοντέλων

install.packages("Metrics")
library(Metrics)

# Συνάρτηση για αξιολόγηση (για τη διευκόλυνσή μας (να μην επαναλαμβάνουμε εντολές))
get_model_stats <- function(model, data, features, target) {
  preds <- predict(model, data = data[, features])$predictions
  rmse_val <- rmse(data[[target]], preds)
  r2_val <- cor(data[[target]], preds)^2
  return(list(RMSE = rmse_val, R2 = r2_val))
}

# Σύγκριση
results <- data.frame(
  Model = c("Ομάδα Α (Αισθητήρες)", "Ομάδα Β (+PMV/PPD/SET)", "Ομάδα Γ (Πλήρες)"),
  RMSE = c(
    get_model_stats(rf_model_A, data5, input_vector_A, "Thermal_comfort")$RMSE,
    get_model_stats(rf_model_B, data5, input_vector_B, "Thermal_comfort")$RMSE,
    get_model_stats(rf_model_C, data5, input_vector_C, "Thermal_comfort")$RMSE
  ),
  R2 = c(
    get_model_stats(rf_model_A, data5, input_vector_A, "Thermal_comfort")$R2,
    get_model_stats(rf_model_B, data5, input_vector_B, "Thermal_comfort")$R2,
    get_model_stats(rf_model_C, data5, input_vector_C, "Thermal_comfort")$R2
  )
)

print(results)


# Τελειώσαμε και χωρίς over-fitting. Μερικές λεπτομέρειες:

# Feature importance (για την Β)
importance <- rf_model_B$variable.importance
importance_df <- data.frame(
  Variable = names(importance),
  Importance = importance
) %>%
  arrange(desc(Importance))

# Γράφημα Top 10 Μεταβλητών
library(ggplot2)
ggplot(importance_df[1:10, ], aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(title = "Οι κορυφαίες 10 Σημαντικές Μεταβλητές (Ομάδα Β)", x = "", y = "Importance")
















