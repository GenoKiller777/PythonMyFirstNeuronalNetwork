#########################
#     Implementando     #
# Redes Neuronales en R #
################################
# MLP - BINARY CLASSIFICATION  #
################################

# install package
#install.packages("neuralnet")

# creando un conjunto de datos de entrenamiento

TKS = c(20,10,30,20,80,30)
CSS = c(90,20,40,50,50,80)
Placed = c(1,0,0,0,1,1)

# Aquí, combinará múltiples columnas o características en un solo conjunto de datos

df = data.frame(TKS, CSS, Placed)

#cargar libreria

require(neuralnet)

# vamos a construir un modelo de clasificador NN usando la biblioteca neuralnet

nn = neuralnet(Placed~TKS+CSS, data = df, hidden = 3, act.fct = "logistic", linear.output = FALSE)

#######################################################################
# Colocado ~ TKS + CSS, Colocado es la etiqueta y TKS y CSS son características. #
# hidden = 3: representa una sola capa con 3 neuronas respectivamente. #
# act.fct = "logistic" utilizado para suavizar el resultado. #
# linear.ouput = FALSE: establezca FALSE para aplicar act.fct de lo contrario TRUE #
#######################################################################

# Graficar nn

plot(nn)


# creando set de datos

TKS=c(30,40,85)
CSS=c(85,50,40)
test=data.frame(TKS,CSS)

## Predicción usa neural network

Predict=compute(nn,test)

Predict$net.result


# Conversión de probabilidades en clases binarias estableciendo el nivel de umbral 0.5

prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred


# Como ellas trabajan?
# https://playground.tensorflow.org/#activation=tanh&batchSize=10&dataset=circle&regDataset=reg-plane&learningRate=0.03&regularizationRate=0&noise=0&networkShape=&seed=0.14894&showTestData=false&discretize=false&percTrainData=50&x=true&y=false&xTimesY=false&xSquared=false&ySquared=false&cosX=false&sinX=false&cosY=false&sinY=false&collectStats=false&problem=classification&initZero=false&hideText=false
