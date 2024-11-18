data <- read.table("final.txt", header = TRUE)
head(data)
summary(data)
dim(data)
str(data)

numerovariables <- dim(data)[2]
cat("Numero de variables:", numerovariables, "\n")
numerotto <- length(unique(data$Tratamiento))
cat("Numero de tratamientos:", numerotto, "\n")

par(mfrow = c(1,3))
boxplot(Wildtype ~ Tratamiento, data=data,
	col = "orange",
	main = "Wildtype",
	xlab = "Tratamiento",
	ylab = "Value")
boxplot(Sequia ~ Tratamiento, data=data,
	col="green",
	main= "Sequia",
	xlab = "Tratamiento",
	ylab = "Value")
boxplot(ExcesoRiego ~ Tratamiento, data=data,
	col= "red",
	main= "ExcesoRiego",
	xlab = "Tratamiento",
	ylab = "Value")
#3 
data$Tratamiento <- as.factor(data$Tratamiento)
plot(data$Wildtype, data$Sequia,
	col= data$Tratamiento,
	pch= 19,
	xlab= "Wildtype",
	ylab= "Sequia",
	main="Sequia-Wildtype")
legend("bottomright", legend = levels(data$Tratamiento),
	col= 1:length(levels(data$Tratamiento)),
	pch = 19, title= "Tratamientos")

plot(data$Wildtype, data$ExcesoRiego,
	col= data$Tratamiento,
	pch=19,
	xlab="Wildtype",
	main="Exceso Riego-Wildtype")
legend("bottomright", legend = levels(data$Tratamiento),
	col= 1:length(levels(data$Tratamiento)),
	pch = 19, title= "Tratamientos")
#4 legendas(en apartado anterior)
#5
par(mfrow= c(3,1))
hist(data$Wildtype,
	col = "orange",
	main = "Histograma WT",
	xlab= "Wildtype",
	ylab= "Frecuencia")
hist(data$Sequia,
	col = "green",
	main = "Histograma Sequia",
	xlab= "Sequia",
	ylab= "Frecuencia")
hist(data$ExcesoRiego,
	col = "red",
	main = "Histograma ER",
	xlab= "ExcesoRiego",
	ylab= "Frecuencia")

#6 La he creado en el ejercicio 3 ¿?
tratamientoo <- factor(data$Tratamiento)
print(tratamientoo)

#7
mediaWT <- aggregate(Wildtype ~ Tratamiento, data, mean)
mediaSec <- aggregate(Sequia ~ Tratamiento, data, mean)
mediaER <- aggregate (ExcesoRiego ~ Tratamiento,data, mean)
print (mediaWT)
print (mediaSec)
print (mediaER)

desviacionWT <- aggregate(Wildtype ~ Tratamiento, data, sd)
desviacionSec <- aggregate(Sequia ~ Tratamiento, data, sd)
desviacionER <- aggregate(ExcesoRiego ~ Tratamiento, data, sd)
print (desviacionWT)
print (desviacionSec)
print (desviacionER)

#8
cuentas <- table(tratamientoo)
print (cuentas)

#9
T1 <- subset(data, Tratamiento==1)
T4 <- subset(data, Tratamiento==4)
print (T1)
print (T4)

#10
T5 <- subset(data, Tratamiento ==5)

pruebaNT1WT <- shapiro.test(T1$Wildtype)
pruebaNT1Sequia <- shapiro.test(T1$Sequia)
pruebaNT1ER <- shapiro.test(T1$ExcesoRiego)

pruebaNT5WT <- shapiro.test(T5$Wildtype)
pruebaNT5Sequia <- shapiro.test(T5$Sequia)
pruebaNT5ER <- shapiro.test(T5$ExcesoRiego)

print("Los resultados de la prueba de normalidad son:")
print(pruebaNT1WT)
print(pruebaNT1Sequia)
print(pruebaNT1ER)
print(pruebaNT5WT)
print(pruebaNT5Sequia)
print(pruebaNT5ER)

var.test(T1$Wildtype, T1$Sequia)
var.test(T1$Wildtype, T1$ExcesoRiego)
var.test(T1$Sequia, T1$ExcesoRiego)
var.test(T5$Wildtype, T5$Sequia)
var.test(T5$Wildtype, T5$ExcesoRiego)
var.test(T5$Sequia, T5$ExcesoRiego)
#Comparo Wildtype y Sequia :)
if(shapiro.test(T1$Wildtype)$p.value > 0.05 &&shapiro.test(T1$Sequia)$p.value >0.05) {
	if (var.test(T1$Wildtype, T1$Sequia)$p.value > 0.05){
	t.test(T1$Wildtype, T1$Sequia, var.equal= TRUE)
	}else {
	t.test(T1$Wildtype, T1$Sequia, var.equal= FALSE)
	}
} else {
	wilcox.test(T1$Wildtype, T1$Sequia)
}
#Comparo Wildtype y ExcesoRiego :)
if(shapiro.test(T1$Wildtype)$p.value > 0.05 &&shapiro.test(T1$ExcesoRiego)$p.value >0.05) {
	if (var.test(T1$Wildtype, T1$ExcesoRiego)$p.value > 0.05){
	t.test(T1$Wildtype, T1$ExcesoRiego, var.equal= TRUE)
	}else {
	t.test(T1$Wildtype, T1$ExcesoRiego, var.equal= FALSE)
	}
} else {
	wilcox.test(T1$Wildtype, T1$ExcesoRiego)
}

#Comparo Sequia y ExcesoRiego :)
if(shapiro.test(T1$Sequia)$p.value > 0.05 &&shapiro.test(T1$ExcesoRiego)$p.value >0.05) {
	if (var.test(T1$Sequia, T1$ExcesoRiego)$p.value > 0.05){
	t.test(T1$Sequia, T1$ExcesoRiego, var.equal= TRUE)
	}else {
	t.test(T1$Sequia, T1$ExcesoRiego, var.equal= FALSE)
	}
} else {
	wilcox.test(T1$Sequia, T1$ExcesoRiego)
}

#Comparo Wildtype y Sequia pero del 5 :)
if(shapiro.test(T5$Wildtype)$p.value > 0.05 &&shapiro.test(T5$Sequia)$p.value >0.05) {
	if (var.test(T5$Wildtype, T5$Sequia)$p.value > 0.05){
	t.test(T5$Wildtype, T5$Sequia, var.equal= TRUE)
	}else {
	t.test(T5$Wildtype, T5$Sequia, var.equal= FALSE)
	}
} else {
	wilcox.test(T5$Wildtype, T5$Sequia)
}
#Comparo Wildtype y ExcesoRiego pero del 5 :)
if(shapiro.test(T5$Wildtype)$p.value > 0.05 &&shapiro.test(T5$ExcesoRiego)$p.value >0.05) {
	if (var.test(T5$Wildtype, T5$ExcesoRiego)$p.value > 0.05){
	t.test(T5$Wildtype, T5$ExcesoRiego, var.equal= TRUE)
	}else {
	t.test(T5$Wildtype, T5$ExcesoRiego, var.equal= FALSE)
	}
} else {
	wilcox.test(T5$Wildtype, T5$ExcesoRiego)
}
#Comparo Sequia y ExcesoRiego pero del 5 :)
if(shapiro.test(T5$Sequia)$p.value > 0.05 &&shapiro.test(T5$ExcesoRiego)$p.value >0.05) {
	if (var.test(T5$Sequia, T5$ExcesoRiego)$p.value > 0.05){
	t.test(T5$Sequia, T5$ExcesoRiego, var.equal= TRUE)
	}else {
	t.test(T5$Sequia, T5$ExcesoRiego, var.equal= FALSE)
	}
} else {
	wilcox.test(T5$Sequia, T5$ExcesoRiego)
}
11#

T1WT <- T1$Wildtype
T1S <- T1$Sequia
T1E <- T1$ExcesoRiego
print (T1WT)
print(T1S)
print(T1E)

datosAnova <- data.frame("Condicion" = c("W","W","W","W","W","W","W","W","W","W","S","S","S","S","S","S","S","S","S","S","E","E","E","E","E","E","E","E","E","E"),"Valor"=c(T1WT, T1S, T1E))
anova <- aov(Valor ~ Condicion, data=datosAnova)
summary(anova)

