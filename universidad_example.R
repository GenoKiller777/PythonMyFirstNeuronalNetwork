#########################
#     Implementando     #
# Redes Neuronales en R #
#########################
# Deep Neural Network #
#########################
# BINARY CLASSIFICATION #
#########################

install.packages("ISLR")

library(neuralnet)
library(ISLR) #data

data = College
head(data)

max_data = apply(data[,2:18], 2, max) ##valor maximo
min_data = apply(data[,2:18], 2, min) ##valor minimo

##data normalizada
data_scaled = scale(data[,2:18], center = min_data, scale = max_data - min_data)

Private = as.numeric(College$Private)-1
data_scaled = cbind(Private, data_scaled)
head(data_scaled)

index = sample(1:nrow(data), round(0.70*nrow(data))) #crear división 70:30 

#train_set
train_data <- as.data.frame(data_scaled[index,])
#test_set
test_data <- as.data.frame(data_scaled[-index,])

n = names(train_data)

#Primero recuperamos todos los nombres de variables usando la función de nombres

head(n)

f <- as.formula(paste("Private~", paste(n[!n %in% "Private"], collapse = " + ")))

### Simple profunda neural network

deep_net = neuralnet(f, data = train_data, hidden= c(5,3), linear.output = F)

# configuramos la capa oculta para que contenga el vector (5,3)
# que corresponde a dos niveles ocultos con sus respectivos
# cinco neuronas en la primera capa oculta y
#tres neuronas en el segundo

plot(deep_net)

predicted_data <- compute(deep_net, test_data[,2:18])
print(head(predicted_data$net.result))

predicted_data$net.result <- sapply(predicted_data$net.result, round, digits = 0)

# use la función sapply () para redondearlos a cero o una clase

table(test_data$Private, predicted_data$net.result) # matrix de confusión

oa = (60+157)/(60+157+7+1)

#install.packages("e1071")

library("e1071")
library("caret")

confusionMatrix(factor(test_data$Private), factor(predicted_data$net.result)) #caret