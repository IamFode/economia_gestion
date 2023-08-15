
#Libreria
library(openxlsx)

# PARTE I
dataframes <- list("P = M1Y (100 percent)"=P_M.Y100per,
                   "P = M1Y (50 percent)"=P_M.Y50per,
                   "P = M1Y (20 percent)"=P_M.Y20per,
                   "P = M1Y (10percent)"=P_M.Y10per)
write.xlsx(dataframes,file = "paso2/dataOut/I_Dwyer&Fisher_figura3.xlsx")

#PARTE II
dataframes <- list("P = M1Y (5 años)"=P_M.Y5años,
                   "P = M1Y (10 años)"=P_M.Y10años,
                   "P = M1Y (25 años)"=P_M.Y25años,
                   "P = M1Y (40 años)"=P_M.Y40años)
write.xlsx(dataframes, file = "paso2/dataOut/II_PeriodosAmplios.xlsx")

# PARTE III
dataframes = list("P = M1Y (dato final e inicial)"=P_M.Y_FI)
write.xlsx(dataframes, file = "paso2/dataOut/III_Dwyer&Fisher_figura4_FvsI.xlsx")


#REMOVE
rm(dataframes)
