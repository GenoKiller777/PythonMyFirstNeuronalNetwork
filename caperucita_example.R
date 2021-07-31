
# Red Neuranal R ----------------------------------------------------------


#########################
#     Implementando     #
# Redes Neuronales en R #
#########################

install.packages("NeuralNetTools")
install.packages("neuralnet")


library(neuralnet)
library(NeuralNetTools)

# cualidades y acciones del código

qualities <- matrix (c(1, 1, 1, 0, 0, 0,
                       0, 1, 0, 1, 1, 0,
                       1, 0, 0, 1, 0, 1), byrow = TRUE, nrow = 3)

colnames(qualities) <- c("big_ears", "big_eyes", "big_teeth", "kindly", "wrinkled", "handsome")
rownames(qualities) <- c("wolf", "grannie", "woodcutter")

qualities

actions <- matrix (c(1, 1, 1, 0, 0, 0, 0,
                     0, 0, 0, 1, 1, 1, 0,
                     0, 0, 0, 1, 0, 1, 1), byrow = TRUE, nrow = 3)

colnames(actions) <- c("run_away", "scream", "look_for_woodcutter", "kiss_on_cheek", "approach", "offer_food", "flirt_with")
rownames(actions) <- rownames(qualities)

actions


data <- cbind(qualities, actions)

# Entrenar la neural network (NN)
set.seed(123) # para reproductabilidad
neuralnetwork <- neuralnet(run_away + scream + look_for_woodcutter + kiss_on_cheek + approach + 
                             offer_food + flirt_with ~ 
                             big_ears + big_eyes + big_teeth + kindly + wrinkled + handsome,
                           data = data, hidden = 3, linear.output = FALSE)

# Graficar la  NN
par_bkp <- par(mar = c(0, 0, 0, 0)) # establecer un margen diferente para minimizar el texto de cutoff
plotnet(neuralnetwork, bias = FALSE)

par(par_bkp)

# ¿Qué acciones aprendió la red?
round(neuralnetwork$net.result[[1]])