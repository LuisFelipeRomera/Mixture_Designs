install.packages("mixexp")
library(mixexp)
data("etch")
View(etch)

#quadratic scheffé model
lin.mod <- lm(erate ~ -1 + x1 + x2 + x3 +
                    x1:x2 + x1:x3 + x2:x3,
              data = etch)
summary(lin.mod)                  

#quadratic scheffé model
res1 <- MixModel(etch, "erate",
                 mixcomps = c("x1", "x2", "x3"),
                 model = 2)

#special cubic scheffé model
res2 <- MixModel(etch, "erate",
                 mixcomps = c("x1", "x2", "x3"),
                 model = 4)

#studentized residuals vs predicted response

par(mfrow = c(1,2)) #graphics will be exibited sideways

colcol <- rep("black") #values are black
colcol[duplicated(etch[,1:3])] <- "red" #values from the first 3 columns
                                        #which are duplicated are red

par(mfrow = c(1,2))

#quadratic model

plot(predict(res1), #predict output from res1
     rstudent(res1), #computes studentized residuals 
     ylim = c(-4, 3), #y axis traces range
     xlab = expression(paste("predicted response", hat(y))), #x axis label
     ylab = "studentized residuals", type = "n", # y axis label
     main = expression(paste("(", frac(paste("y -", hat(y)), sigma), ") ~ ",
                             hat(y) # graph label
                             )
                       )
     )

text(predict(res1),
     rstudent(res1),
     label = 1:nrow(etch),
     col = colcol)

abline(h = c(-2, 0, 2),
       lty = c(2, 1, 2))

grid()

#special cubic model

plot(predict(res2), #predict output from res2
     rstudent(res2), #computes studentized residuals 
     ylim = c(-4, 3), #y axis traces range
     xlab = expression(paste("predicted response", hat(y))), #x axis label
     ylab = "studentized residuals", type = "n", # y axis label
     main = expression(paste("(", frac(paste("y -", hat(y)), sigma), ") ~ ",
                             hat(y) # graph label
     )
     )
)

text(predict(res2),
     rstudent(res2),
     label = 1:nrow(etch),
     col = colcol)

abline(h = c(-2, 0, 2),
       lty = c(2, 1, 2))

grid()

ModelPlot(model = res2,
          dimensions = list(
            x1 = "x1", 
            x2 = "x2", 
            x3 = "x3"),
          contour = TRUE,
          cuts = 6,
          fill = TRUE)

#cox directions

#making a ternary graph with plot directions
install.packages("Ternary")
library(Ternary)

par(mfrow = c(1,2))

?TernaryPlot

#adding the graph
TernaryPlot(atip = "x1", #title corners
            btip = "x2",
            ctip = "x3",
            axis.cex = 1.2, #axis font size
            lab.cex = 1.3, #labels font size
            axis.labels = seq(0, #starting label value
                              1, #finishing label value
                              0.1)) #ratio range/n axis
#adding the cox lines
TernaryArrows(
  c(0, 0.5, 0.5), #point starting the line, coordinates clockwise starting
                  #on the left
  c(1, 0, 0), #finishing point of the line
  col = "red", #color of the line
  length = 0.15, #length of the arrow
  lwd = 2 # width of the line
)

TernaryArrows(
  c(0.5, 0, 0.5), 
  c(0, 1, 0), 
  col = "red", 
  length = 0.15, 
  lwd = 2 
)

TernaryArrows(
  c(0.5, 0.5, 0), 
  c(0, 0, 1), 
  col = "red", 
  length = 0.15, 
  lwd = 2 
)

#adding the intersection point
AddToTernary(
  points,
  list( #point coordenate
    x1 = rep(0.33,3), #first argument the value 
    # second the repetition
    x2 = rep(0.33,3),
    x3 = rep(0.33,3)
             ),
  pch = 16, #point format
  col = "black", #point color
  cex = 1.5 #point size
  )

title(
  main = "Cox Direction", 
  cex.main = 1.5 #title size
  )

#effect plot
ModelEff(
  nfac = 3, #n of factors
  mod = 4, #scheffe model
  dir = 2, #type of direction (2 = cox)
  ufunc = res2, #the model made
  dimensions = list("x1", "x2", "x3") #components names
)
