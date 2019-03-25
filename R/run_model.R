#' @title A function that runs a DSAIRM/DSAIDE app
#'
#' @description This function runs a model based on information
#' provided in the modelsettings list passed into it.
#'
#' @param modelsettings a list with model settings. Required list elements are: \cr
#' List elements with names and values for all inputs expected by simulation function. If not provided, defaults are used. \cr
#' modelsettings$simfunction - name of simulation function in variable  \cr
#' modelsettings$modeltype - specify what kind of model should be run.
#' Currently one of ( _ode_, _discrete_, _stochastic_, _odeandstochastic_, _usanalysis_, _modelexploration_, _fit_ ). \cr
#' modelsettings$nreps - needed for stochastic models to indicate numer of repeat simulations. \cr
#' modelsettings$plottype - 'Boxplot' or 'Scatterplot' needed for US app \cr
#' modelsettings$plotscale - indicate which axis should be on a log scale (x, y or both), \cr
#' modelsettings$nplots -  indicate number of plots that should be produced (number of top list elements in result) \cr
#' @return A vectored list named "result" with each main list element containing the simulation results in a dataframe called dat and associated metadata required for processing by generate_ggplot/plotly/text functions. Most often there is only one main list entry (result[[1]]) for a single plot/text.
#' @details This function runs a model for specific settings.
#' @importFrom utils head tail
#' @importFrom stats reshape
#' @export

run_model <- function(modelsettings) {

  #short function to call/run model
  runsimulation <- function(modelsettings)
  {
    #extract modeslettings inputs needed for simulator function
    currentmodel = modelsettings$currentmodel
    #match values provided from UI with those expected by function
    settingsvec = unlist(modelsettings)
    currentargs = settingsvec[match(names(unlist(formals(currentmodel))), names(settingsvec))]
    #get rid of NA that might happen because inputs are not supplied for certain function inputs.
    #in that case we use the function defaults
    currentargs <- currentargs[!is.na(currentargs)]
    #make a list
    arglist = as.list(currentargs)
    #convert arguments for function call to numeric if possible
    #preserve those that can't be converted
    numind = suppressWarnings(!is.na(as.numeric(arglist))) #find numeric values
    arglist[numind] = as.numeric(currentargs[numind])
    #run simulation, try command catches error from running code.
    #any arguments to function that are not supplied lead to use of default values for each function
    simresult <- try( do.call(currentmodel, args = arglist ) )
    return(simresult)
  }


  simfunction = modelsettings$simfunction

  ##################################
  #dynamical model execution
  ##################################
  if (grepl('_stochastic_',modelsettings$modeltype))
  {
    #replicate modelsettings as list based on number of reps for stochastic
    modelsettings$currentmodel = simfunction[grep('_stochastic',simfunction)] #list of model functions, get the ode function
    allmodset=rep(list(modelsettings),times = modelsettings$nreps)
    rngvec = seq(modelsettings$rngseed,modelsettings$rngseed+modelsettings$nreps-1)
    #give the rngseed entry in each list a consecutive value
    xx = purrr::map2(allmodset, rngvec, ~replace(.x, "rngseed", .y))
  }
  if (grepl('_odeandstochastic_',modelsettings$modeltype))
  {
    #stochastic part
    #replicate modelsettings as list based on number of reps for stochastic
    modelsettings$currentmodel = simfunction[grep('_stochastic',simfunction)] #list of model functions, get the ode function
    allmodset=rep(list(modelsettings),times = modelsettings$nreps)
    rngvec = seq(modelsettings$rngseed,modelsettings$rngseed+modelsettings$nreps-1)
    #give the rngseed entry in each list a consecutive value
    xx1 = purrr::map2(allmodset, rngvec, ~replace(.x, "rngseed", .y))
    #ODE part
    modelsettings$currentmodel = simfunction[grep('_ode',simfunction)] #list of model functions, get the ode function
    xx2 = rep(list(modelsettings),times = 1) #don't really replicate list, but get it into right structure
    xx = c(xx1,xx2)
  }
  if (grepl('_ode_',modelsettings$modeltype))
  {
    modelsettings$currentmodel = simfunction[grep('_ode',simfunction)] #list of model functions, get the ode function
    xx = rep(list(modelsettings),times = 1) #don't really replicate list, but get it into right structure
  }
  if (grepl('_discrete_',modelsettings$modeltype))
  {
    modelsettings$currentmodel = simfunction[grep('_discrete',simfunction)] #list of model functions, get the ode function
    xx = rep(list(modelsettings),times = 1) #don't really replicate list, but get it into right structure
  }
  if (grepl('_fit_',modelsettings$modeltype))
  {
    modelsettings$currentmodel = simfunction[grep('_fit',simfunction)]
    xx = rep(list(modelsettings),times = 1) #don't really replicate list, but get it into right structure
  }
  if (grepl('_modelexploration_',modelsettings$modeltype))
  {
    modelsettings$currentmodel = simfunction[grep('_modelexploration',simfunction)]
    xx = rep(list(modelsettings),times = 1) #don't really replicate list, but get it into right structure
  }
  if (grepl('_usanalysis_',modelsettings$modeltype))
  {
    modelsettings$currentmodel  = simfunction[grep('_usanalysis',simfunction)]
    xx = rep(list(modelsettings),times = 1) #don't really replicate list, but get it into right structure
  }

  #run all simulations for each modelsetting, store in list simresult
  #since the simulation is returned as list, extract data frame only
  simresult <- purrr::map(xx,runsimulation)
  simresult <- unlist(simresult, recursive = FALSE, use.names = TRUE)
  if (class(simresult)!="list")
  {
    result <- 'Model run failed. Maybe unreasonable parameter values?'
    return(result)
  }


  ##################################
  #take data from all simulations and turn into list structure format
  #needed to generate plots and text
  #this applies to simulators that run dynamical models
  #other simulation functions need output processed differently and will overwrite some of these settings
  #each other simulator function has its own code block below
  ##################################

  #save all results to a list for processing plots and text
  listlength = modelsettings$nplots
  #here we do all simulations in the same figure
  result = vector("list", listlength) #create empty list of right size for results

  result[[1]]$simres = simresult

  ##################################
  #default for text display, used by most basic simulation models
  #can/will be potentially overwritten below for specific types of models
  ##################################

  result[[1]]$maketext = TRUE #indicate if we want the generate_text function to process data and generate text
  result[[1]]$showtext = NULL #text can be added here which will be passed through to generate_text and displayed for EACH plot
  result[[1]]$finaltext = 'Numbers are rounded to 2 significant digits.' #text can be added here which will be passed through to generate_text and displayed once

  #Meta-information for each plot
  result[[1]]$plottype = "Lineplot"
  result[[1]]$xlab = "Time"
  result[[1]]$ylab = "Numbers"
  result[[1]]$legend = "Compartments"

  #set x or y axis to logarithmic if indicated
  plotscale = ifelse(is.null(modelsettings$plotscale), "none", modelsettings$plotscale)
  result[[1]]$xscale = ifelse( (plotscale == 'x' | plotscale == 'both'),  'log10', 'identity' )
  result[[1]]$yscale = ifelse( (plotscale == 'y' | plotscale == 'both'),  'log10',  'identity' )

  ##################################
  #model exploration code block
  ##################################
  if (grepl('_modelexploration_',modelsettings$modeltype))
  {
    #these 3 settings are only needed for the shiny UI presentation
    result[[1]]$maketext = FALSE #if true we want the generate_text function to process data and generate text, if 0 no result processing will occur insinde generate_text
    result[[1]]$showtext = NULL #text for each plot can be added here which will be passed through to generate_text and displayed for each plot
    result[[1]]$finaltext = paste("System might not have reached steady state", length(result[[1]]$dat$steady) - sum(result[[1]]$dat$steady), "times")

    #Meta-information for each plot
    result[[1]]$plottype = "Scatterplot"
    result[[1]]$xlab = modelsettings$samplepar
    result[[1]]$ylab = "Outcomes"
    result[[1]]$legend = "Outcomes"
    result[[1]]$linesize = 3
  }

  ##################################
  #model fitting code block
  ##################################
  if (grepl('_fit_',modelsettings$modeltype))
  {

    #Meta-information for each plot
    result[[1]]$plottype = "Mixedplot"
    result[[1]]$maketext = FALSE
    result[[1]]$showtext = NULL
  }

  #return result structure to calling function (app.R)
  #results are a list, with sublists for each plot, each sublist itself is a list containing data and meta-information
  #see the generate_ggplot/plotly/text functions for details on the form of the required input
  return(result)

}

