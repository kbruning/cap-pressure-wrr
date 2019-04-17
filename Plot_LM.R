##Run the LM in separate script, then use this script to plot

require(plotly)

count <- length(epn)
Number = seq(1, count, 1)

en_Real <- epn
en_Predicted <- fit$fitted.values

data3 <- data.frame(Number,  en_Real, en_Predicted)

plot(en_Real, en_Predicted, xlab="", ylab = "",xlim = c(0,0.5),ylim=c(0,0.5), cex.axis = 1.25,pch=".",col="#00CED1" ,cex=3)
segments(0, 0, 1, 1, col = "black")
title( xlab="Actual Non-Wetting Phase Volume Fraction", 
       ylab="Predicted Non-Wetting Phase Volume Fraction",col.lab="black", cex.lab=1.25)
