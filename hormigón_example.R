#########################
#     Implementando     #
# Redes Neuronales en R #
#########################
# MLP - REGRESSION  #
#####################

#install.packages("readxl")

require(caret)
require(neuralnet)
require(readxl)

con = read_xls("Concrete_Data.xls")

head(con)

colnames(con) = c("cement", "slag", "ash", "water", "superplastic", "coarseagg", "fineagg", "age", "strength")

typeof(con)

con = as.data.frame(con)

typeof(con)


head(con)

# normalizar todas las variables
# todos los valores están entre 0 y 1
# función de normalización estándar

normal = function(x){
  return((x-min(x))/(max(x)-min(x)))}

# aplique la función normal a cada columna
con_norm = as.data.frame(lapply(con, normal))

head(con_norm)

# preproceso
#caret para preprocesamiento

trainIndex = createDataPartition(con$strength,p = .75, list = F)
train_set = con[trainIndex,]
test_set = con[-trainIndex,]

#RNA simple con una sola neurona oculta

concrete_model <- neuralnet(formula = strength ~ cement + slag + ash +
                              water + superplastic + coarseagg + fineagg + age,
                            data = train_set)

plot(concrete_model)

con_r = compute(concrete_model, test_set[1:8])


#implementar nn en el conjunto de pruebas

predicted_strength = con_r$net.result

srmse = sqrt(mean((predicted_strength - test_set$strength)^2))

## MEJORA EL MODELO
## agrega más neuronas, no más capas

# RNA simple con solo 5 neuronas ocultas
# más neuronas se ajustan mejor a datos complejos y no lineales,
#pero no a una gran cantidad de neuronas

concrete_model2 = neuralnet(formula = strength ~ cement + slag + ash +
                              water + superplastic + coarseagg + fineagg + age,
                            data = train_set, hidden = 5)

plot(concrete_model2)

con_r2 = compute(concrete_model2, test_set[1:8])

predicted_strength2 = con_r2$net.result

srmse2 = sqrt(mean((predicted_strength2 - test_set$strength)^2))


#no se produce mejoras.