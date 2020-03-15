NN_Repeated <- function(Data, DV_Name, Activation = 'tanh', Hidden = c(6, 4, 2), Algorithm = 'rprop+', Stepmax = 3e4, Threshold = 1, Rep = 5, Seed = 1, Trace = TRUE, Plot = TRUE) {
  
  library('neuralnet')
  
  #############################################################################################################
  # PRELIMIARIES
  
  if(Seed >= 0) {
    set.seed(Seed)
  }
  
  Formula					<- formula(paste(DV_Name, ' ~ .', sep = ''))
  
  #############################################################################################################
  # 
  
  Successful_Repeats			<- 0
  Opt_Error					<- +Inf
  
  while(Successful_Repeats < Rep) {
    NN_Output 					<- neuralnet(Formula, data = Data[complete.cases(Data),], act.fct = Activation, hidden = Hidden, linear.output = TRUE, algorithm = Algorithm, stepmax = Stepmax, threshold = Threshold)
    
    if(!is.null(NN_Output$result.matrix)) {
      Successful_Repeats			<- Successful_Repeats + 1
      Error						<- as.numeric(NN_Output$result.matrix[1,1])
      
      if(Error < Opt_Error) {
        Opt_Error					<- Error
        Opt_NN_Output				<- NN_Output		
      }
      
      if(Trace) {
        cat(paste('\n      Processed repetition ', Successful_Repeats, '.', sep = ''))
      }
    } else if(Trace) {
      cat('\n      No convergence for the utilized starting values.')
    }
  }
  if(Trace) {
    cat('\n') 
  }
  if(Plot) {
    plot(NN_Output)
  }
  
  list(Model_obj = Opt_NN_Output, Error = Opt_Error)
  
}