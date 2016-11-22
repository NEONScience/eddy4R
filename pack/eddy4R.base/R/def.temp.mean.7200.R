# average temperature in irga cell 
data$irga$T_cell_7200 <- ff::as.ff(0.2 * data$irga$tempCellIn + 0.8 * data$irga$tempCellOut)
base::attr(x = data$irga$T_cell_7200, which = "unit") <- base::attr(x = data$irga$tempCellIn, which = "unit")
