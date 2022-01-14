getwd()
setwd("C:/Users/VSBAG/Desktop/DSE_Milan/2nd_Sem_Subjects/Lab/nutritional")
data<-load("baseline_database_RA_group3.RData")
RA<-data.frame(tot_bl_DSE)

food_groups<-RA[,c(355:359,413:415,469:499)]

red_meat<-rowMeans(food_groups[,c(1,3,5)])
white_meat<-rowMeans(food_groups[,c(2,4)])
salt<-rowMeans(food_groups[,c(6,7)])
sugar<-rowMeans(food_groups[,c(8,13,14,32)])
fish<-rowMeans(food_groups[,c(9:11)])
eggs<-food_groups[,c(12)]
dried_fruits_nuts<-rowMeans(food_groups[,c(15:16)])
oil<-rowMeans(food_groups[,c(17:18)])
vegetables<-rowMeans(food_groups[,c(19:21)])
vegetal_proteins<-rowMeans(food_groups[,c(22,24:25)])
onion_garlic<-food_groups[,c(23)]
fruits<-rowMeans(food_groups[,c(26:28)])
unsweetened_beverages<-food_groups[,c(29)]
coffee<-food_groups[,c(30)]
alcohol<-food_groups[,c(31)]
refined_grains_potatoes<-rowMeans(food_groups[,c(33:34)])
whole_grains<-food_groups[,c(35)]
dairy<-rowMeans(food_groups[,c(36:39)])

collapsed_food_groups<-data.frame(red_meat, white_meat, salt, sugar, fish, eggs, dried_fruits_nuts, oil, vegetables, vegetal_proteins, onion_garlic, fruits, unsweetened_beverages, coffee, alcohol,  refined_grains_potatoes, whole_grains, dairy)

#n rows, p columns
n <- nrow(collapsed_food_groups)
p <- ncol(collapsed_food_groups)

#Correlation matrix
rho<-cor(collapsed_food_groups)

#Eigenvalues and eigenvectors
eigen(rho)
eigenval<-eigen(rho)$values
eigenvec<-eigen(rho)$vectors

#proportion of variance explained
pvarsp = eigenval/p
pvarspcum = cumsum(pvarsp)
pvarsp

#Scree Diagram
plot(eigenval, type="b", main="Scree Diagram", xlab="Number of Components", ylab="Eigenvalues")
abline(h=1, lwd=3, col="red")

#We choose three components

#matrix of the components
comp<-round(cbind(eigen(rho)$vectors[,1]*sqrt(eigenval[1]),eigen(rho)$vectors[,2]*sqrt(eigenval[2]),eigen(rho)$vectors[,3]*sqrt(eigenval[3])),3)
rownames(comp)<-colnames(collapsed_food_groups)
colnames(comp)<-c("Comp1","Comp2","Comp3")
comp

#Add the commonality

commonality<-comp[,1]^2+comp[,2]^2+comp[,3]^2
comp<-cbind(comp,commonality)
comp

#plot of the components

#Comp1 vs Comp2

plot(comp[,1:2], main="Components Plot",
     xlab="comp1",ylab="comp2", xlim=range(-1,1))
text(comp[,1:2], rownames(comp))
abline(v=0,h=0,col="red")

#Comp2 vs Comp3

plot(comp[,2:3], main="Components Plot",
     xlab="comp2",ylab="comp3", xlim=range(-1,1))
text(comp[,2:3], rownames(comp))
abline(v=0,h=0,col="red")

#Comp1 vs Comp3

plot(comp[,1:3], main="Components Plot",
     xlab="comp1",ylab="comp3", xlim=range(-1,1))
text(comp[,1:3], rownames(comp))
abline(v=0,h=0,col="red")

#Scores computation 
collapsed_food_groups.scale <- scale(collapsed_food_groups, T, T)
scores <- collapsed_food_groups.scale%*%eigenvec[,1:3]

#normalize dividing by the sqrt of the eigenval
scoresz<-round(cbind(scores[,1]/sqrt(eigenval[1]),scores[,2]/sqrt(eigenval[2]),scores[,3]/sqrt(eigenval[3])),3)

#patterns labelings
pattern_names<-c("low_vegetables_pattern","healthy_pattern","meat_rich_pattern")
colnames(scoresz)<-pattern_names


