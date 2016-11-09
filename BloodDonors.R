library(caret)
set.seed(3456)

trainIndex <- createDataPartition(data$Made.Donation.in.March.2007, p = .7, 
                                  list = FALSE, 
                                  times = 1)
training = data[trainIndex,]
test = data[-trainIndex,]

modelLinear = lm(Made.Donation.in.March.2007 ~ Months.since.Last.Donation+Number.of.Donations+Total.Volume.Donated..c.c..+Months.since.First.Donation, data = training)

plot(training)

summary(modelLinear)

#Se encontraron NA en los coeficientes, eso quiere decir que hay relaciones lineales en los datos


preProcValues <- preProcess(training, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, training)

modelLinear = lm(Made.Donation.in.March.2007 ~ Months.since.Last.Donation+Number.of.Donations+Total.Volume.Donated..c.c..+Months.since.First.Donation, data = training)

# Con esto se encuentran relaciones lineales.

comboInfo <- findLinearCombos(training)
comboInfo

training = training[,-comboInfo$remove]
modelLinear = lm(Made.Donation.in.March.2007 ~ Months.since.Last.Donation+Number.of.Donations+Months.since.First.Donation, data = training)
predicted = predict(modelLinear, training[,-c(1,5)])
actual = training$Made.Donation.in.March.2007

#Agregar una variable para promedio de donaciones por mes
training$Avg.Donations = training$Months.since.First.Donation / training$Number.of.Donations
modelLinear = lm(Made.Donation.in.March.2007 ~ Months.since.Last.Donation+Number.of.Donations+Months.since.First.Donation+Avg.Donations, data = training)
predicted = predict(modelLinear, training[,-c(1,5)])
actual = training$Made.Donation.in.March.2007



#Hay que transformar las predicciones, para que vayan de un rango de 0 a 1

pred_trans = (1/(1+exp(-predicted)))
pred_binomial = pred_trans
pred_binomial = (pred_binomial >= 0.7) * 1

# La sensitividad de la distribucion esta bien, pero hay muy pocos positivos
# Hay que ajustar el umbral de confianza

pred_binomial = pred_trans
pred_binomial = (pred_binomial >= 0.6) * 1


LogLossBinary = function(actual, predicted, eps = 1e-15) {
  predicted = pmin(pmax(predicted, eps), 1-eps)
     - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}

