

Numeric_Exploration <- function(DF_Train = NULL, DF_Test = NULL, DV_Name = NULL, DV_Type = 'ordinal', Predictor_Names = NULL, Imputation_Rules = list(), Max_Fraction = 0.99, Polynomial_Order = 4, Plot = TRUE) {
  
  DF_Train_MOD		<- DF_Train
  DF_Test_MOD			<- DF_Test
  J				<- length(Predictor_Names)
  Variables_to_Drop		<- c()
  if(!is.null(DV_Name)) {
    Dep_Variable		<- DF_Train[[DV_Name]]
  }
  
  cat('\nSUMMARY OF ORIGINAL NUMERIC PREDICTORS:\n')
  print(summary(DF_Train[Predictor_Names]))
  
  cat('\nASSESSING POTENTIAL USEFULNESS OF NUMERIC PREDICTORS:\n')
  if(J > 0) {
    for(j in 1:J) {
      Name 				<- Predictor_Names[j]
      Variable			<- DF_Train[[Name]]
      if(!is.null(DF_Test)) {
        Variable_Test		<- DF_Test[[Name]]
      }
      NA_Flag			<- is.na(Variable)
      Sum_NA_Flag			<- sum(NA_Flag)
      cat(paste('\n', j, '] ', Name, ':', sep = ''))
      
      # Recoding missing values.
      if(Sum_NA_Flag > 0 && !is.null(Imputation_Rules[[Name]]) && Imputation_Rules[[Name]] != 'NA') {
        if(typeof(Imputation_Rules[[Name]]) == 'character') {
          Imputted_Val 			<- switch(Imputation_Rules[[Name]], 
                                    'mean' = mean(Variable, na.rm = TRUE), 
                                    'median' = median(Variable, na.rm = TRUE), 
                                    'min' = min(Variable, na.rm = TRUE), 
                                    'max' = max(Variable, na.rm = TRUE), 
          ) 
        } else {
          Imputted_Val 			<- Imputation_Rules[[Name]]
        }
        Variable[NA_Flag]			<- Imputted_Val
        DF_Train_MOD[[Name]]		<- Variable
        cat(paste("\n   In the training sample, ", Sum_NA_Flag, " 'NA' are recoded into ", Imputted_Val,".", sep = ""))
        
        if(!is.null(DF_Test)) {
          NA_Flag_Test			<- is.na(Variable_Test)
          Variable_Test[NA_Flag_Test]	<- Imputted_Val
          DF_Test_MOD[[Name]]		<- Variable_Test
          cat(paste("\n   In the test sample, ", sum(NA_Flag_Test), " 'NA' are recoded into ", Imputted_Val,".", sep = ""))
        }
        
        cat('\n')
      }
      
      Frequencies 		<- as.numeric(xtabs(~Variable))
      N				<- sum(Frequencies)
      Sample_Max_Fraction	<- max(Frequencies) / N
      
      if(Sample_Max_Fraction > Max_Fraction) {
        # The sample distribution is close to being degenerate
        Variables_to_Drop				<- c(Variables_to_Drop, Name)
      } else {
        # Exploring potential association with the dependent variable.
        X						<- c()
        for(power in 1:Polynomial_Order) {
          X						<- cbind(X, Variable^power)
        }	
        summary_obj					<- summary(lm(Dep_Variable ~ X))
        P_value					<- 1 - pf(summary_obj$fstatistic[1], summary_obj$fstatistic[2], summary_obj$fstatistic[3])
        cat(paste('\n   F-test for ', DV_Name, ' ~ polynomial(', Name, ') has p-value = ', sprintf('%1.2e', P_value), '. Adjusted R-square = ', round(summary_obj$adj.r.squared * 100, 2), '%.\n', sep = '')) 	
        
        # Plotting
        if(Plot) {
          if(DV_Type == 'scale') {
            jpeg(paste('SCPL ', DV_Name, ' ~ ', Name, '.jpeg', sep=''), width = 480 * 3, height = 240 * 3)
            plot(Dep_Variable ~ Variable, xlab = Name, ylab = DV_Name, col = j)
            dev.off()
          } else if(DV_Type == 'ordinal') {
            jpeg(paste('BXPL ', DV_Name, ' ~ ', Name, '.jpeg', sep=''), width = 480 * 3, height = 240 * 3)
            boxplot(Variable ~ as.factor(Dep_Variable), xlab = DV_Name, ylab = Name, col = 1 + as.numeric(levels(as.factor(Dep_Variable))))
            dev.off()
          }
        }
      }
    }
  }
  
  cat('\nSUMMARY OF MODIFIED NUMERIC PREDICTORS:\n')
  print(summary(DF_Train_MOD[Predictor_Names]))
  cat('\n')
  
  list(DF_Train_MOD = DF_Train_MOD, DF_Test_MOD = DF_Test_MOD, Variables_to_Drop = Variables_to_Drop) 
  
}