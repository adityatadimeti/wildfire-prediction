Categorical_Exploration <- function(DF_Train = NULL, DF_Test = NULL, DV_Name = NULL, DV_Type = 'ordinal', Predictor_Names = NULL, Max_Fraction = 0.99, Min_Count = 60, Plot = TRUE) {
  
  DF_Train_MOD		<- DF_Train
  DF_Test_MOD			<- DF_Test
  J				<- length(Predictor_Names)
  Variables_to_Drop		<- c()
  if(!is.null(DV_Name)) {
    Dep_Variable		<- DF_Train[[DV_Name]]
  }
  
  cat('\nASSESSING POTENTIAL USEFULNESS OF CATEGORICAL PREDICTORS:\n')
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
      if(Sum_NA_Flag > 0) {
        levels(Variable) 			<- c(levels(Variable), 'MISSING')
        Variable[is.na(Variable)] 	<- 'MISSING'
        DF_Train_MOD[[Name]]		<- Variable
        cat(paste("\n   In the training sample, ", Sum_NA_Flag, " 'NA' are recoded into 'MISSING'.", sep = ""))
        
        if(!is.null(DF_Test)) {
          levels(Variable_Test) 		<- c(levels(Variable_Test), 'MISSING')
          NA_Flag_Test			<- is.na(Variable_Test)
          Variable_Test[NA_Flag_Test]	<- 'MISSING'
          DF_Test_MOD[[Name]]		<- Variable_Test
          cat(paste("\n   In the test sample, ", sum(NA_Flag_Test), " 'NA' are recoded into 'MISSING'.", sep = ""))
        }
        
        cat('\n')
      }
      
      # Merging categories with low count.
      Table				<- xtabs(~Variable)
      Frequencies 		<- as.numeric(Table)
      if(min(Frequencies) < Min_Count) {
        I				<- length(Frequencies)
        Level_Names			<- names(Table)
        levels(Variable)		<- c(levels(Variable), 'LOW_COUNT')
        if(!is.null(DF_Test)) {
          levels(Variable_Test)	<- c(levels(Variable_Test), 'LOW_COUNT')
        }
        
        for(i in 1:I) {
          if(Frequencies[i] < Min_Count) {
            Variable[Variable == Level_Names[i]]		<- 'LOW_COUNT'
            if(!is.null(DF_Test)) {
              Variable_Test[Variable_Test == Level_Names[i]]	<- 'LOW_COUNT'
            }
          }
        }
        
        Variable			<- factor(Variable)			# Removing 0-frequency categories.
        DF_Train_MOD[[Name]]	<- Variable
        if(!is.null(DF_Test)) {
          DF_Test_MOD[[Name]]	<- factor(Variable_Test)		# Removing 0-frequency categories.
        }
        Frequencies 		<- as.numeric(xtabs(~Variable))
      }
      N				<- sum(Frequencies)
      Sample_Max_Fraction	<- max(Frequencies) / N
      
      if(Sample_Max_Fraction > Max_Fraction) {	
        # The sample distribution is close to being degenerate
        Variables_to_Drop				<- c(Variables_to_Drop, Name)
      } else {
        # Displaying the frequency table.
        cat('\n')
        print(xtabs(~Variable))
        
        # Exploring potential association with the dependent variable.	
        summary_obj					<- summary(lm(Dep_Variable ~ as.factor(Variable)))
        P_value					<- 1 - pf(summary_obj$fstatistic[1], summary_obj$fstatistic[2], summary_obj$fstatistic[3])
        cat(paste('\n   F-test for ', DV_Name, ' ~ ', Name, ' has p-value = ', sprintf('%1.2e', P_value), '. Adjusted R-square = ', round(summary_obj$adj.r.squared * 100, 2), '%.\n\n', sep = '')) 	
        
        # Plotting
        if(Plot) {
          if(DV_Type == 'scale') {
            jpeg(paste('BXPL ', DV_Name, ' ~ ', Name, '.jpeg', sep=''), width = 480 * 3, height = 240 * 3)
            boxplot(Dep_Variable ~ as.factor(Variable), xlab = Name, ylab = DV_Name, col = 1 + as.numeric(levels(as.factor(Variable))))
            dev.off()
          } else if(DV_Type == 'ordinal') {
            jpeg(paste('BRCH ', DV_Name, ' ~ ', Name, '.jpeg', sep=''), width = 480 * 3, height = 240 * 3)
            barplot(table(as.factor(Dep_Variable), as.factor(Variable)), main = paste(DV_Name, ' by ', Name, sep = ''), xlab = Name, ylab = DV_Name, 
                    col = 1 + as.numeric(levels(as.factor(Dep_Variable))), legend = levels(as.factor(Dep_Variable)), beside = TRUE)
            dev.off()
          }
        }
      }
    }
  }
  cat('\n')
  
  list(DF_Train_MOD = DF_Train_MOD, DF_Test_MOD = DF_Test_MOD, Variables_to_Drop = Variables_to_Drop) 
  
}