
speedInc <- c(8.502, 1.148, 0.777)
barplot(speedInc,  names.arg = c("lmBoot", "lmboot Par", "lmBoot Imp"),
        col = "steelblue", border = "black", density = c(90, 70, 50), 
        main = "increase in speed", xlab = "lmBoot", ylab = "time mls")

