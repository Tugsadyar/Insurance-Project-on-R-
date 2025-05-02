#Kazım Tuğşad YAR
# Kodu kontrol için küçük notlar :
# 1. Bilgisayarım 8. yılına girdiği için çok yavaş, 
# veride yaş için KNN kullandığım için sürekli çalıştırmak çok uzun sürüyordu, bu yüzden veriyi tekrar tekrar her algoritma için kopyaladım.Ve
# NeuralNet paketi saatlerce sürdüğü için nnet paketi ile tek gizli katmanla hallettim.
# 2. Görselleştirme kullanırken colorspace paketinin choose_palette fonksiyonunu kullandım. Pie chartlar için. Önünüze çıkan UI'da tek tek seçmeniz gerekiyor.
# 3. R nedense Türkçe harflerimi alttaki kodu yazmadan algılayamıyor,sıkıntı çıkar gibi olursa alttaki kodu deneyebilirsiniz
#  setlocale Sys.setlocale(category = "LC_ALL", locale = "Turkish")
# Loading the data

library(openxlsx)
library(dplyr)
library(pROC)
library(VIM)
library(ggplot2)
library(gridExtra)
library(colorspace)
library(scales)
library(treemap)
library(plotly)
library(dplyr)
library(plotly)
library(GGally)
library(stats)
library(MASS)
library(car)
library(pROC)
library(caret)
library(e1071)
library(kernlab)
library(themis)
library(LiblineaR)
library(ROSE)
library(rpart)
library(rpart.plot)
library(MLmetrics)
library(pROC)
library(randomForest)
library(Metrics)
library(xgboost)
library(Matrix)
library(nnet) 
library(neuralnet) 
library(NeuralNetTools)
library(keras)
library(tensorflow)
data <- read.xlsx("Case_Study_Data.xlsx")
data <- as.data.frame(data)
colnames(data) <- tolower(colnames(data)) #Data column başlıklarını küçük harflere çevirme
colnames(data)


# Datanın yapısı ve özet istatistikleri
#Yapısı

str(data)
# Teklif onay durumunu 0,1 lere çeviriyoruz. Böylece tahmin yaparken işimiz kolaylaşacak.
data$teklif.onay.durumu <- ifelse(data$teklif.onay.durumu== "T" ,0,1)
data$teklif.onay.durumu <- as.factor(data$teklif.onay.durumu)
str(data)

# Özet istatistikleri
summary(data)


# 1- Yaş için minimu-m değer 0 gözüküyor. Bir imputation tekniği ile 0 değerlerini değiştirebiliriz. En yakın komşu imputasyonu iyi olacaktır (KNN).
# 2- hasarsızlık basamağı 4ten başlamalı, ama minimum değer 1.  Değeri 4ten küçük olanları 1 e çevirmemiz gerekiyor.
# 3- Teklif primi çok fazla 

table(is.na(data$ilçe))

# yaş imputasyonu 


data$yaş[data$yaş == 0] <- NA 
table(is.na(x = data$yaş)) # 5255 adet yaşı 0 girilmiş müşteri var.


data$yaş <- kNN(data, variable = "yaş", k = 5)[, "yaş"]
table(is.na(x = data$yaş)) # artık 0 veya NA değerimiz kalmadı. 

# Hasarsızlık basamağı düzeltmesi 
data$trafik.basamak.kodu[data$trafik.basamak.kodu < 4  ] <- 4
summary(data)

# Teklif priminin log versiyonunu ekleme.
data_log <- data %>%
  mutate(teklif.primi.log = log(teklif.primi + 1))


# Explaratory Data Analysis


# distributions 


# Grafik düzeni için par fonksiyonu
par(mfrow=c(3, 2))


# Histogramları 
hist(data$hasarsizlik.indirimi.kademesi, main = "Hasarsızlık İndirimi Kademesi", col = "skyblue", xlab = "Hasarsızlık İndirimi Kademesi",breaks = 5)
hist(data$trafik.basamak.kodu, main = "Trafik Basamak Kodu", col = "lightgreen", xlab = "Trafik Basamak Kodu",breaks = 5)
hist(data$araç.yaşi, main = "Araç Yaşı", col = "lightcoral", xlab = "Araç Yaşı",breaks = 5)
hist(data$model.yili, main = "Model Yılı", col = "lightyellow", xlab = "Model Yılı",breaks = 5)
hist(data$yaş, main = "Sigortalı Yaşı", col = "lightpink", xlab = "Yaş",breaks = 5)
hist(data_log$teklif.primi.log,main = "Teklif Primi (Log dönüşümü uygulanmış)", xlab = "Log Dönüşümlü değerler", ylab = "Frekans", col = "lightblue2",breaks = 5)






par(mfrow=c(2, 2))
#Pie Chart of Insurance type
sigortatable <- table(data$sigortali.tipi)
names(sigortatable) <- gsub("^O$", "Ozel", names(sigortatable))  #Ozel
names(sigortatable) <- gsub("^T$", "Tuzel", names(sigortatable))  # Tuzel
sigortalabels <- paste(names(sigortatable),sep = "")
 palette1 <- choose_palette()
pie(sigortatable,labels = sigortalabels,col = palette1(2),main= " Pie chart of Insurance Type")


#Pie chart of Brands
markatable <- table(data$marka)
markapct <- round(markatable/sum(markatable)*100)
markalabels <- paste(names(markatable),"\n",sep = "")
markalabels <- paste(markalabels,markapct)
markalabels <- paste(markalabels," % ",sep = "")
 palette0 <- choose_palette()
pie(markatable,labels = markalabels,col = palette0(6),main = "Pie Chart of Brands")

# Pie chart of Fuel type 
fueltable <- table(data$yakit.tipi)
fuelpct <- round(fueltable/ sum(fueltable)*100)
fuellabels <- paste(names(fueltable),"\n",sep = "")
fuellabels <- paste(fuellabels,fuelpct)
fuellabels <- paste(fuellabels," % ",sep="")
palette1 <- choose_palette()
pie(fueltable,labels = fuellabels,col = palette1(2),main = "Pie Chart of Fuel type ")
# Pie Chart of Discount Level
indirimtable <- table(data$hasarsizlik.indirimi.kademesi)
indirimpct <- round(indirimtable/ sum(indirimtable)*100)
indirimlabels <- paste(names(indirimtable),"\n",sep = "")
indirimlabels <- paste(indirimlabels,indirimpct)
indirimlabels <- paste(indirimlabels," % ",sep="")
#palette2 <- choose_palette()
pie(indirimtable,labels = indirimlabels,col = palette2(6),main = "Pie Chart of Discount Level")



#violin plotlar
trafik_prim <- ggplot(data = data_log,mapping = aes(x=as.factor(trafik.basamak.kodu),y = teklif.primi.log))+
  geom_violin(fill = "seagreen2",alpha = 0.67)+
  labs(title = "Trafik Basamağı ve Teklif Primi İlişkisi", x = "Trafik Basamağı", y = "Teklif Primi")
trafik_prim

marka_prim <- ggplot <- ggplot(data_log, aes(x = marka, y = teklif.primi.log)) +
  geom_violin(fill = "lightyellow") +
  labs(title = "Marka ve Teklif Primi İlişkisi", x = "Marka", y = "Teklif Primi")
marka_prim

model_prim <- ggplot(data_log, aes(x = as.factor(model.yili), y = teklif.primi.log)) +
  geom_violin(fill = "lightcoral") +
  labs(title = "Model Yılı ve Teklif Primi İlişkisi", x = "Model Yılı", y = "Teklif Primi")
model_prim

portföy_prim <- ggplot(data_log, aes(x = portföy.ayrimi, y = teklif.primi.log)) +
  geom_violin(fill = "sienna1") +
  labs(title = "Portföy  ve Teklif Primi İlişkisi", x = "Portföy Alanı", y = "Teklif Primi")
portföy_prim
grid.arrange(trafik_prim, marka_prim, model_prim, portföy_prim ,ncol = 2)

#Boxplots 

indirim_prim <- ggplot(data_log, aes(x = as.factor(hasarsizlik.indirimi.kademesi), y = teklif.primi.log)) +
  geom_boxplot(fill = "plum2") +
  labs(title = "Hasarsızlık İndirimi ve Teklif Primi İlişkisi", x = "Hasarsızlık İndirimi Kademesi", y = "Teklif Primi")
indirim_prim

ayaş_prim <- ggplot(data_log, aes(x = as.factor(araç.yaşi) , y = teklif.primi.log)) +
  geom_boxplot(fill = "plum2") +
  labs(title = "Araç yaşı ve Teklif Primi İlişkisi", x = "Araç yaşı", y = "Teklif Primi")
ayaş_prim

il_prim <- ggplot(data_log, aes(x = as.factor(il) , y = teklif.primi.log)) +
  geom_boxplot(fill = "plum2") +
  labs(title = "İllere göre Teklif Primi", x = "İller", y = "Teklif Primi")
il_prim

grid.arrange(il_prim, ayaş_prim, indirim_prim, ncol = 3)

#other plots
#Treemap
treemap(data,
        index = c("marka","il"),
        vSize = "teklif.primi",
        title = "İl ve markaya göre Teklif primleri",
        palette = "Set2",
        fontsize.labels = c(12,10),
        fontcolor.labels = c("white","black"),
        fontface.labels = c(2,3),
        align.labels = list(
          c("center", "center"),
          c("center", "bottom")), 
        overlap.labels = 0.3, 
        inflate.labels = F, 
        border.col = c("black", "white"),
        border.lwds = c(3, 2)
        )


#Sunburst chart
# Hiyerarşi



data_ilce <- data %>%
  filter(!is.na(ilçe) & ilçe != "") %>%
  group_by(marka, il, ilçe) %>%
  summarise(prim = sum(teklif.primi, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    ids = paste(marka, il, ilçe, sep = "-"),
    labels = ilçe,
    parents = paste(marka, il, sep = "-")
  )



data_il <- data %>%
  group_by(marka, il) %>%
  summarise(prim = sum(teklif.primi, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    ids = paste(marka, il, sep = "-"),
    labels = il,
    parents = marka
  )


data_marka <- data %>%
  group_by(marka) %>%
  summarise(prim = sum(teklif.primi, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    ids = marka,
    labels = marka,
    parents = ""
  )

#birleştir
sunburst_data <- bind_rows(data_marka, data_il, data_ilce)

#plot
sunburst <- plot_ly(
  ids = sunburst_data$ids,
  labels = sunburst_data$labels,
  parents = sunburst_data$parents,
  values = sunburst_data$prim,
  type = "sunburst",
  branchvalues = "total"
)

sunburst


################ 


# Örnek: veriniz 
ggpairs(data[, c("yaş", "araç.yaşi", "model.yili", "teklif.primi", "hasarsizlik.indirimi.kademesi", "trafik.basamak.kodu")])



################################ Modelileme  #####################################


################### Generalized Linear Regression Model


glmmodel <- glm(data = data,formula = teklif.onay.durumu ~ hasarsizlik.indirimi.kademesi + trafik.basamak.kodu + marka + araç.yaşi + yakit.tipi + il + yaş + teklif.primi + portföy.ayrimi + sigortali.tipi, family = binomial())
summary(glmmodel)


stepmodel <- stepAIC(glmmodel,direction = "both",trace = TRUE)
summary(stepmodel)
# Step fonksiyonları (LRT ve elimination methodlarıyla).
stepmodel2 <- step(glmmodel,test= "LRT")
summary(stepmodel2)

# Final model 
glmmodel2 <- glm(data = data,formula = teklif.onay.durumu ~ hasarsizlik.indirimi.kademesi + marka + araç.yaşi + yakit.tipi + il + yaş + teklif.primi + portföy.ayrimi + sigortali.tipi, family = binomial())

#  multicollinearity problem.
vif(glmmodel2) # Multicollinearity sorunu yok




#  AUC değeri & RoC eğrisi.
RoC <- roc(data$teklif.onay.durumu, predict(glmmodel2, type = "response"))
auc(RoC) # AUC > 0.7 : good ,,,, AUC > 0.8 : better than good , Output: 0.746

#Roc curve
roc_df <- data.frame(
  specificity = rev(RoC$specificities),
  sensitivity = rev(RoC$sensitivities)
)

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "#1f77b4", size = 1.5) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(
    title = paste("ROC Curve - GLM Model (AUC =", round(auc(RoC), 3), ")"),
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  theme_minimal(base_size = 14)

#Cross validation  GLM 
data$teklif.onay.durumu <- factor(data$teklif.onay.durumu, levels = c(0,1), labels = c("T", "O"))
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
train(teklif.onay.durumu ~ hasarsizlik.indirimi.kademesi + marka + araç.yaşi + yakit.tipi + il + yaş + teklif.primi + portföy.ayrimi + sigortali.tipi, data = data, method = "glm", family = "binomial", trControl = ctrl, metric = "ROC")
# Teklif onay durumunu tekrar 0-1e çeviriyoruz :)
data$teklif.onay.durumu <- ifelse(data$teklif.onay.durumu== "T" ,0,1)


#RMSE ve MAE değerleri 

y_true_glm <- as.numeric(data$teklif.onay.durumu) # 0/1 hale getir
y_pred_glm <- predict(glmmodel2, type = "response")

rmse_glm <- sqrt(mean((y_true_glm - y_pred_glm)^2))
mae_glm <- mean(abs(y_true_glm - y_pred_glm))
rmse_glm ;  mae_glm   #[1] 0.3122745 , [1] 0.1949671
# ConfusionMatrix


glmpredictions <- predict(object = glmmodel2 , type = "response") 
glm_pred_class <- ifelse(glmpredictions > 0.5,1 ,0)
confmatrixglm <- confusionMatrix(as.factor(glm_pred_class), as.factor(data$teklif.onay.durumu))
confmatrixglm



# # # # # # # # # # # # # #  SVM (Support Vektör Machine) # # # # # # # # # # # # # # # # # # # 

#data for svm
data_svm <- data 
#Data ayrımı 
splitIndex_svm <- createDataPartition(data_svm$teklif.onay.durumu, p = 0.7, list = FALSE)
train_data_svm <- data_svm[splitIndex_svm, ]
test_data_svm <- data_svm[-splitIndex_svm, ]

# Model

data_svm$teklif.onay.durumu <- as.factor(ifelse(data_svm$teklif.onay.durumu == 1, "Onaylandı", "Onaylanmadı"))
train_data_svm$teklif.onay.durumu <- as.factor(ifelse(train_data_svm$teklif.onay.durumu == 1, "Onaylandı", "Onaylanmadı"))
test_data_svm$teklif.onay.durumu  <- as.factor(ifelse(test_data_svm$teklif.onay.durumu == 1, "Onaylandı", "Onaylanmadı"))

svm_grid <- expand.grid(
  sigma = c(0.01, 0.05),
  C = c(1, 10)
)
  
ctrl_svm <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,                
  summaryFunction = twoClassSummary
)

svm_model <- train(
  teklif.onay.durumu ~ hasarsizlik.indirimi.kademesi + trafik.basamak.kodu + marka +
    araç.yaşi + yakit.tipi + il + yaş + teklif.primi + portföy.ayrimi + sigortali.tipi,
  data = train_data_svm,
  method = "svmRadial",
  trControl = ctrl_svm,            
  metric = "ROC",
  preProcess = c("center", "scale"),
  tuneGrid = svm_grid
)

#Predictions ve Conf.Matrix
svm_pred_class <- predict(svm_model, newdata = test_data_svm)
svm_pred_prob <- predict(svm_model, newdata = test_data_svm, type = "prob")[, "Onaylandı"]
confusionMatrix(svm_pred_class, test_data_svm$teklif.onay.durumu, positive = "Onaylandı")


#4. AUC &  ROC eğrisi 
actual_svm <- test_data_svm$teklif.onay.durumu
RoC_svm <- roc(actual_svm, svm_pred_prob, levels = c("Onaylanmadı", "Onaylandı"), direction = "<")

auc(RoC_svm) # AUC :  0.6484
plot(RoC_svm, col = "blue", lwd = 2, main = "SVM ROC Curve")


#MAE , RMSE 
actual_binary_svm <- ifelse(test_data_svm$teklif.onay.durumu == "Onaylandı", 1, 0)
rmse_svm<- sqrt(mean((svm_pred_prob - actual_binary_svm)^2))
mae_svm <- mean(abs(svm_pred_prob - actual_binary_svm))
mae_svm ; rmse_svm #[1] 0.2124238 ; [1] 0.3262529

# F1 ve precision
f1_score_svm <- caret:: F_meas(svm_pred_class, test_data_svm$teklif.onay.durumu, relevant = "Onaylandı") # [1] 0.1076345
precision_svm <- posPredValue(svm_pred_class, test_data_svm$teklif.onay.durumu, positive = "Onaylandı") # [1] 0.4942529


cat_vars <- c("hasarsizlik.indirimi.kademesi", "trafik.basamak.kodu", "marka", "yakit.tipi",
              "il", "portföy.ayrimi", "sigortali.tipi", "teklif.onay.durumu")

## # # # # # # # # # # # # # # #  SVM with SMOTE (Synthetic Minority Over-sampling Technique)# # # # # # # # # # # # # # # # # # # 

train_data_svm <- train_data_svm[, !(names(train_data_svm) %in% "ilçe")]
test_data_svm <- test_data_svm[, !(names(test_data_svm) %in% "ilçe")]
train_data_svm[cat_vars] <- lapply(train_data_svm[cat_vars], as.factor)
test_data_svm[cat_vars] <- lapply(test_data_svm[cat_vars], as.factor)
balanced_data_svm <- ROSE(teklif.onay.durumu ~ hasarsizlik.indirimi.kademesi + trafik.basamak.kodu + marka +
                            araç.yaşi + yakit.tipi + il + yaş + teklif.primi + portföy.ayrimi + sigortali.tipi, data = train_data_svm, seed = 33)$data
svm_model2 <- svm(teklif.onay.durumu ~ hasarsizlik.indirimi.kademesi + trafik.basamak.kodu + marka +
                   araç.yaşi + yakit.tipi + il + yaş + teklif.primi + portföy.ayrimi + sigortali.tipi,
                 data = balanced_data_svm,
                 kernel = "radial",  # veya "linear", "polynomial", "sigmoid"
                 cost = 100,
                 scale = TRUE)

svm_predictions2 <- predict(svm_model2, newdata = test_data_svm)
confmatrix_svm <- confusionMatrix(svm_predictions2, test_data_svm$teklif.onay.durumu)
confmatrix_svm

#F1 ve Precision SVM SMOTE
precision_svm2 <- confmatrix_svm$byClass['Precision']
f1_score_svm2 <- confmatrix_svm$byClass['F1']
precision_svm2 ; f1_score_svm2 #Precision 0.2321692 ; F1 0.343898 

# MAE ve MSE 
prednumeric<- ifelse(svm_predictions2 == "Onaylandı", 1, 0)  # "Onaylandı" -> 1, "Onaylanmadı" -> 0
actnumeric <- ifelse(test_data_svm$teklif.onay.durumu == "Onaylandı", 1, 0)

# Root  Mean Squared Error (MSE)
rmse_svm2 <- sqrt(mean((prednumeric - actnumeric)^2)) # 0.5608789
mae_svm2 <- mean(abs(prednumeric - actnumeric)) # 0.3145852 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # #  DECISION TREE


#Data
data_dt <- data 
#Data ayrımı 
splitIndex_dt <- createDataPartition(data_dt$teklif.onay.durumu, p = 0.7, list = FALSE)
train_data_dt<- data_dt[splitIndex_dt, ]
test_data_dt <- data_dt[-splitIndex_dt, ]

cat_vars <- c("hasarsizlik.indirimi.kademesi", "trafik.basamak.kodu", "marka", "yakit.tipi",
              "il", "portföy.ayrimi", "sigortali.tipi", "teklif.onay.durumu")
train_data_dt <- train_data_dt[, !(names(train_data_dt) %in% "ilçe")]
test_data_dt <- test_data_dt[, !(names(test_data_dt) %in% "ilçe")]
train_data_dt[cat_vars] <- lapply(train_data_dt[cat_vars], as.factor)
test_data_dt[cat_vars] <- lapply(test_data_dt[cat_vars], as.factor)
for (var in cat_vars) {
  levels(test_data_dt[[var]]) <- levels(train_data_dt[[var]])
}

balanced_data_dt <- ROSE(teklif.onay.durumu ~ hasarsizlik.indirimi.kademesi + trafik.basamak.kodu + marka +
                            araç.yaşi + yakit.tipi + il + yaş + teklif.primi + portföy.ayrimi + sigortali.tipi, data = train_data_svm, seed = 333)$data

dtmodel <- rpart::rpart(formula = teklif.onay.durumu ~ hasarsizlik.indirimi.kademesi + trafik.basamak.kodu + marka + araç.yaşi + yakit.tipi + il + yaş + teklif.primi + portföy.ayrimi + sigortali.tipi,
                 data=balanced_data_dt ,
                 method= "class")
summary(dtmodel)
rpart.plot::rpart.plot(dtmodel)

# Tahminleri elde etme
predictions_td <- predict(dtmodel, test_data_dt, type = "class")
predictions_td <- as.factor(predictions_td)
# Gerçek değerlerin faktör formatına dönüştürülmesi (eğer gerekliyse)
test_data_dt$teklif.onay.durumu <- as.factor(test_data_dt$teklif.onay.durumu)




levels(predictions_td) <- levels(test_data_dt$teklif.onay.durumu)
# Confusion matrix
confmatrix_dt <- caret::confusionMatrix(predictions_td, test_data_dt$teklif.onay.durumu)
confmatrix_dt


# MAE (Mean Absolute Error)
mse_dt<- mean((as.numeric(predictions_td ) - as.numeric(test_data_dt$teklif.onay.durumu))^2)
mae_dt <- mean(abs(as.numeric(predictions_td ) - as.numeric(test_data_dt$teklif.onay.durumu)))
mse_dt ; mae_dt #[1] 0.3294323  ;  [1] 0.3294323

#F1 ve Precision
f1_dt <- F1_Score(y_true = as.character(test_data_dt$teklif.onay.durumu), 
         y_pred = as.character(predictions_td), 
         positive = "1")  # Pozitif sınıfın ismini senin verine göre ayarla

precision_dt <- Precision(y_true = as.character(test_data_dt$teklif.onay.durumu), 
          y_pred = as.character(predictions_td), 
          positive = "1")

f1_dt ; precision_dt #[1] 0.3465003  ;   [1] 0.2299908

# ROC ve AUC

pred_probs_dt <- predict(dtmodel, test_data_dt, type = "prob")[,2]  
RoC_dt <- roc(test_data_dt$teklif.onay.durumu, pred_probs_dt,levels = c("0","1"),
              direction = "<")
auc_dt <- auc(RoC_dt)
auc_dt # 0.7289
plot(RoC_dt, col = "#1c61b6", main = "ROC Curve - Decision Tree")





# # # # # # # # # # # # # # # # # # # # # # # # # # # # #  Random Forest


#Data
data_rf <- data 
#Data ayrımı 
splitIndex_rf <- createDataPartition(data_rf $teklif.onay.durumu, p = 0.7, list = FALSE)
train_data_rf<- data_rf[splitIndex_rf, ]
test_data_rf <- data_rf[-splitIndex_rf, ]

cat_vars <- c("hasarsizlik.indirimi.kademesi", "trafik.basamak.kodu", "marka", "yakit.tipi",
              "il", "portföy.ayrimi", "sigortali.tipi", "teklif.onay.durumu")
train_data_rf <- train_data_rf[, !(names(train_data_rf) %in% "ilçe")]
test_data_rf <- test_data_rf[, !(names(test_data_rf) %in% "ilçe")]
train_data_rf[cat_vars] <- lapply(train_data_rf[cat_vars], as.factor)
test_data_rf[cat_vars] <- lapply(test_data_rf[cat_vars], as.factor)



balanced_data_rf <- ROSE(teklif.onay.durumu ~ hasarsizlik.indirimi.kademesi + trafik.basamak.kodu + marka +
                             araç.yaşi + yakit.tipi + il + yaş + teklif.primi + portföy.ayrimi + sigortali.tipi, data = train_data_rf, seed = 3333)$data
#Model 

rf_model <- randomForest(teklif.onay.durumu ~ hasarsizlik.indirimi.kademesi + trafik.basamak.kodu + marka +
                           araç.yaşi + yakit.tipi + il + yaş + teklif.primi + portföy.ayrimi + sigortali.tipi,
                         data= balanced_data_rf,
                         ntree = 300,
                         mtry = 3,
                         importance = TRUE)

print(rf_model)

#Predictions
pred_class_rf <- predict(rf_model, test_data_rf, type = "class")
pred_prob_rf  <- predict(rf_model, test_data_rf, type = "prob")[, "1"]


confmatrix_rf <- confusionMatrix(pred_class_rf , test_data_rf$teklif.onay.durumu)
confmatrix_rf


# AUC ve ROC eğrisi
RoC_rf <- roc(response = test_data_rf$teklif.onay.durumu,
              predictor = pred_prob_rf ,
              levels = c("0", "1")) 

auc_rf <- auc(RoC_rf)
auc_rf # 0.7453 

plot(RoC_rf, main = "ROC Curve - Random Forest", col = "#2c7fb8", lwd = 2)

# F1 ve Precision 
f1_rf <- F1_Score(y_true = as.character(test_data_rf$teklif.onay.durumu), 
         y_pred = as.character(pred_class_rf), 
         positive = "1")

precision_rf <- Precision(y_true = as.character(test_data_rf$teklif.onay.durumu), 
          y_pred = as.character(pred_class_rf), 
          positive = "1")
f1_rf ; precision_rf  #[1] 0.3634855 ;  [1] 0.2579505

#RMSE ve MAE
actual_rf <- ifelse(test_data_rf$teklif.onay.durumu == "1", 1, 0)
pred_prob_rf  <- predict(rf_model, test_data_rf, type = "prob")[, "1"]

rmse_rf <- rmse(actual_rf, pred_prob_rf)
mae_rf <- mae(actual_rf, pred_prob_rf)

rmse_rf ; mae_rf  # [1] 0.4300324 ;  [1] 0.368984
 






# # # # # # # # # # # # # # # # # # # # # # # # # # # # #  Xgboost




#Data
data_xg <- data 
#Data ayrımı 
splitIndex_xg <- createDataPartition(data_xg$teklif.onay.durumu, p = 0.7, list = FALSE)
train_data_xg<- data__xg[splitIndex_xg , ]
test_data_xg <- data__xg[-splitIndex_xg , ]

cat_vars <- c("hasarsizlik.indirimi.kademesi", "trafik.basamak.kodu", "marka", "yakit.tipi",
              "il", "portföy.ayrimi", "sigortali.tipi", "teklif.onay.durumu")
train_data_xg<- train_data_xg[, !(names(train_data_xg) %in% "ilçe")]
test_data_xg <- test_data_xg[, !(names(test_data_xg) %in% "ilçe")]
train_data_xg[cat_vars] <- lapply(train_data_xg[cat_vars], as.factor)
test_data_xg[cat_vars] <- lapply(test_data_xg[cat_vars], as.factor)



# Kategorik değişkenler
cat_vars <- c("hasarsizlik.indirimi.kademesi", "trafik.basamak.kodu", "marka", "yakit.tipi",
              "il", "portföy.ayrimi", "sigortali.tipi", "teklif.onay.durumu")

# ROSE ile veri dengeleme
balanced_data_xg <- ROSE(teklif.onay.durumu ~ hasarsizlik.indirimi.kademesi + trafik.basamak.kodu + marka +
                           araç.yaşi + yakit.tipi + il + yaş + teklif.primi + portföy.ayrimi + sigortali.tipi,
                         data = train_data_xg, seed = 33333)$data

# Kategorik değişkenleri faktöre çevir
balanced_data_xg[cat_vars] <- lapply(balanced_data_xg[cat_vars], as.factor)
test_data_xg[cat_vars] <- lapply(test_data_xg[cat_vars], as.factor)

# Label (0/1) dönüştür
train_label <- as.numeric(balanced_data_xg$teklif.onay.durumu) - 1
test_label <- as.numeric(test_data_xg$teklif.onay.durumu) - 1

# Dummy encoding (model matrix)
train_matrix_xg <- model.matrix(teklif.onay.durumu ~ . -1, data = balanced_data_xg)
test_matrix_xg <- model.matrix(teklif.onay.durumu ~ . -1, data = test_data_xg)

# Sütun uyumsuzluklarını düzelt
train_colnames <- colnames(train_matrix_xg)
missing_cols <- setdiff(train_colnames, colnames(test_matrix_xg))
for (col in missing_cols) {
  test_matrix_xg <- cbind(test_matrix_xg, setNames(data.frame(0), col))
}
test_matrix_xg <- test_matrix_xg[, train_colnames]

# DMatrix oluştur
train_xg <- xgb.DMatrix(data = train_matrix_xg, label = train_label)
test_xg <- xgb.DMatrix(data = test_matrix_xg, label = test_label)

# Model parametreleri
params_xg <- list(
  objective = "binary:logistic",
  eval_metric = "error",
  max_depth = 6,
  eta = 0.1
)

# Model eğitimi
xgb_model <- xgb.train(params = params_xg,
                       data = train_xg,
                       nrounds = 100,
                       watchlist = list(train = train_xg),
                       verbose = 0)

# Tahmin ve sınıflandırma
pred_probs_xg <- predict(xgb_model, test_xg)
pred_classes_xg <- ifelse(pred_probs_xg > 0.5, 1, 0)

# Confusion Matrix
confusionMatrix(factor(pred_classes_xg), factor(test_label), positive = "1")


# 1. Precision ve F1 
precision_xg <- Precision(y_pred = pred_classes_xg, y_true = test_label)
f1_xg <- F1_Score(y_pred = pred_classes_xg, y_true = test_label)
precision_xg ; f1_xg  #[1] 0.9387003   [1] 0.8096211
# 3. RMSE & MAE 
rmse_xg <- rmse(actual = test_label, predicted = pred_probs_xg)
mae_xg <- mae(actual = test_label, predicted = pred_probs_xg)
rmse_xg ; mae_xg #[1] 0.4451772  [1] 0.3978563
# 5. AUC &  ROC Eğrisi
RoC_xg <- pROC::roc(test_label, pred_probs_xg)
auc_xg <- pROC::auc(RoC_xg)
auc_xg # 0.7588

plot(RoC_xg, main = "XGBoost ROC Curve", col = "#2C3E50", lwd = 2)





# # # # # # # # # # # # # # # # # # # #  ANN




#Data
data_neural <- data 
#Data ayrımı 
splitIndex_neural <- createDataPartition(data_neural$teklif.onay.durumu, p = 0.7, list = FALSE)
train_data_neural<- data_neural[splitIndex_neural , ]
test_data_neural <- data_neural[-splitIndex_neural , ]

cat_vars <- c("hasarsizlik.indirimi.kademesi", "trafik.basamak.kodu", "marka", "yakit.tipi",
              "il", "portföy.ayrimi", "sigortali.tipi", "teklif.onay.durumu")
train_data_neural<- train_data_neural[, !(names(train_data_neural) %in% "ilçe")]
test_data_neural <- test_data_neural[, !(names(test_data_neural) %in% "ilçe")]
train_data_neural[cat_vars] <- lapply(train_data_neural[cat_vars], as.factor)
test_data_neural[cat_vars] <- lapply(test_data_neural[cat_vars], as.factor)

balanced_data_neural <- ROSE(teklif.onay.durumu ~ hasarsizlik.indirimi.kademesi + trafik.basamak.kodu + marka +
                           araç.yaşi + yakit.tipi + il + yaş + teklif.primi + portföy.ayrimi + sigortali.tipi,
                         data = train_data_neural, seed = 333333)$data


## NNET grid search

grid_nnet <- expand.grid(size = c(3, 5, 7), decay = c(0, 0.01, 0.1))
ctrl_nnet <- trainControl(method = "cv", number = 5)

# caret ile eğitim
nnet_griddy_model <- train(
  teklif.onay.durumu ~ .,
  data = balanced_data_neural, 
  method = "nnet",
  trControl = ctrl_nnet,
  tuneGrid = grid_nnet,
  maxit = 500,
  trace = FALSE
)
print(nnet_griddy_model)

common_levels <- levels(test_data_neural$teklif.onay.durumu)
pred_probs_neural <- predict(nnet_griddy_model, newdata = test_data_neural)
# predicted ve actual vektörleri aynı seviyelere sahip faktörlere çevir
pred_probs_neural <- factor(as.character(pred_probs_neural), levels = common_levels)
actual_neural <- factor(test_data_neural$teklif.onay.durumu, levels = common_levels)

# Confusion Matrix oluştur
confusionMatrix(pred_probs_neural, actual_neural)


# Final model with best optimization

final_nnet_model <- nnet(teklif.onay.durumu ~ ., 
                         data = balanced_data_neural, 
                         size = 7,       # Gizli katman nöron sayısı
                         decay = 0.05,    # Aşırı öğrenmeyi engellemek için
                         maxit = 500)
# Plot of the final model
plotnet(final_nnet_model,
        alpha = 0.6,          # çizgi saydamlığı
        circle_cex = 3,       # düğüm boyutu
        pos_col = "darkgreen",
        neg_col = "skyblue",
        intercept = TRUE,
        bias = TRUE)

pred_probs_neural2 <- predict(final_nnet_model, newdata = test_data_neural, type = "class")
common_levels2 <- levels(test_data_neural$teklif.onay.durumu)
pred_probs_neural2 <- factor(pred_probs_neural2, levels = common_levels2)
actual_neural2 <- factor(test_data_neural$teklif.onay.durumu, levels = common_levels2)

# Confusion Matrix hesapla
confusion_matrix_result2 <- confusionMatrix(pred_probs_neural2, actual_neural2,positive ="1")
print(confusion_matrix_result2)

# Precision, Recall, F1-score
precision_neural <- confusion_matrix_result2$byClass["Precision"]
f1_neural <- confusion_matrix_result2 $byClass["F1"]
precision_neural ; f1_neural #Precision-0.2244434 ; F1-0.3391692 
# MSE / RMSE / MAE için:
pred_probs_prob <- predict(final_nnet_model, newdata = test_data_neural, type = "raw")
actual_numeric <- as.numeric(as.character(test_data_neural$teklif.onay.durumu))

rmse <- sqrt(mean((pred_probs_prob - actual_numeric)^2))
mae <- mean(abs(pred_probs_prob - actual_numeric))
rmse ; mae #[1] 0.4714856 ; [1] 0.4213879






"# Neuralnet model ? 

#Çalışmıyor, çok uzun sürüyor


#Data
data_neural2 <- data 
#Data ayrımı 
splitIndex_neural2 <- createDataPartition(data_neural2$teklif.onay.durumu, p = 0.7, list = FALSE)
train_data_neural2<- data_neural2[splitIndex_neural2 , ]
test_data_neural2 <- data_neural2[-splitIndex_neural2 , ]

train_data_neural2<- train_data_neural2[, !(names(train_data_neural2) %in% "ilçe")]
test_data_neural2 <- test_data_neural2[, !(names(test_data_neural2) %in% "ilçe")]





# Hedef değişkeni sayısal yapma (0-1 olacak şekilde)
train_data_neural2$teklif.onay.durumu <- as.numeric(as.character(train_data_neural2$teklif.onay.durumu))
test_data_neural2$teklif.onay.durumu <- as.numeric(as.character(test_data_neural2$teklif.onay.durumu))

# Dummy encoding (model.matrix)
train_model2 <- model.matrix(teklif.onay.durumu ~ . , train_data_neural2) %>% as.data.frame()
test_model2 <- model.matrix(teklif.onay.durumu ~ . , test_data_neural2) %>% as.data.frame()

# Hedefi sona ekle
train_model2$target <- train_data_neural2$teklif.onay.durumu
test_model2$target <- test_data_neural2$teklif.onay.durumu

# Güvenli normalize (NA'lerden kaçınmak için)
safe_normalize <- function(x) {
  if (min(x) == max(x)) return(rep(0, length(x)))
  return((x - min(x)) / (max(x) - min(x)))
}

train_model2 <- as.data.frame(lapply(train_model2, safe_normalize))
test_model2 <- as.data.frame(lapply(test_model2, safe_normalize))

# Sadece sabit olmayan değişkenleri tut
train_model2 <- train_model2[, sapply(train_model2, function(x) length(unique(x)) > 1)]
test_model2  <- test_model2[, names(train_model2)]  # Aynı kolonlar



# NA'siz
train_model2 <- train_model2[complete.cases(train_model2), ]
train_model2 <- train_model2[, sapply(train_model2, function(x) length(unique(x)) > 1)]

balanced_data <- upSample(
  x = train_model2[, -ncol(train_model2)],  # target dışındaki değişkenler
  y = as.factor(train_model2$target),       # hedef
  yname = "target"                          # tekrar hedef kolonu oluştur
)
#model
model_nn_2 <- neuralnet(target ~ .,
                        data = balanced_data,
                        hidden = c(4,3),
                        linear.output = F,
                        stepmax = 10000)

plotnet(model_nn_2)
"


















