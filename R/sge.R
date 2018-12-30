


#' Title Preparing parameters to send to Job
#'
#' @param parameters is a list with all parameters needed by the function
#' to run within the object
#' @return The RDS file that stores the parameters
#' @export
#'
#' @examples
packJobParameters = function(parameters,wd="~/tmp/"){
  fname = paste0(wd,"/_",as.character(signif(runif(1),5)),".rds")
  saveRDS(parameters,fname)
  return(fname)
}

#' Title
#'
#' @param token
#'
#' @return
#' @export
#'
#' @examples
runJob = function(token){
  cat("Running job with token",token,"\n")
  myparams <- readRDS(token)
  targetFunc = myparams$fun
  myparams$fun = NULL
  toSave = list(result=do.call(what="targetFunc",args=myparams),
                params=myparams)
  saveRDS(toSave,paste0(token,"_out.rds"))
}


#' Title
#'
#' @param job
#'
#' @return
#' @export
#'
#' @examples
jobRunning = function(job,wd="~/tmp"){
  tmpfile = paste0(wd,"/qstat.out")
  system(paste0("qstat > ",tmpfile))
  queue = read.delim(tmpfile)
  if(nrow(queue) > 0){
    isit = length(unlist(apply(queue,1,function(x){ grep(job,x)}))) > 0
    file.remove(tmpfile)
    return(isit)
  }
  return(T)
}

#' Title
#'
#' @param parameters
#' @param clParams
#' @param prefix
#'
#' @return
#' @export
#'
#' @examples
launchJob = function(parameters,
                     clParams="",
                     wd="~/tmp/",
                     prefix=NULL){

  token = packJobParameters(parameters,wd=wd)
  expid = paste0("J_",ifelse(is.null(prefix),"U",prefix),"_",as.character(signif(runif(1),5)))
  logfile.log = paste0(wd,"/",expid,".log")
  logfile.e = paste0(wd,"/",expid,".e")
  logFiles = list(log=logfile.log,e=logfile.e)
  command = paste0("echo \"Rscript -e \\\"library(sgefacilities);",
                   "runJob(token=\\\\\\\"",token,"\\\\\\\")",
                   "\\\"\" | qsub -S /bin/bash -N ",expid," ",
                   clParams,
                   " -o ",logfile.log," -e ",logfile.e)
  cat("The command is\n",command,"\n")
  system(command)
  Sys.sleep(0.5)
  if(!jobRunning(expid,wd=wd)){
    cat("Something went wrong with the Job????\n")
  }else
    cat("Job",expid,"is queued\n")
  return(list(paramfile=token,
              jobname=expid,
              outfile=paste0(token,"_out.rds"),
              logfile=logfile.log,
              errfile=logfile.e))
}

#' Title
#'
#' @param handlers
#' @param timeLimit
#' @param increment
#' @param removeLogs
#'
#' @return
#' @export
#'
#' @examples
waitForJobs = function(handlers,
                       timeLimit= 24*3600,
                       increment=30,
                       removeLogs=T,
                       removeData=T,
                       wd="~/tmp/",
                       qstatworks=F){

  waitForReady = rep(F,length(handlers))
  elapsed = 0
  responses = NULL
  while(sum(waitForReady) < length(handlers) & elapsed < timeLimit){
    indexes = which(!waitForReady)
    for(index in indexes){
      if(!is.null(handlers[[index]])){
        cat("Checking for",handlers[[index]]$outfile,"\n")
        if(file.exists(handlers[[index]]$outfile)){
          cat("Job from",handlers[[index]]$outfile,"finished\n")
          Sys.sleep(5)
          responses[[index]] = readRDS(handlers[[index]]$outfile)
          waitForReady[index] = T
          if(removeData){
            file.remove(handlers[[index]]$outfile)
            file.remove(handlers[[index]]$paramfile)
          }
          if(removeLogs){
            file.remove(handlers[[index]]$logfile)
            file.remove(handlers[[index]]$errfile)
          }
        }else{
          cat("Checking state of",handlers[[index]]$jobname,"\n")

          if(qstatworks){
            if(!jobRunning(handlers[[index]]$jobname,wd=wd)){
              cat("No results and no job running:",handlers[[index]]$jobname,"\n")
              responses[[index]] = NULL
              waitForReady[index] = T
            }
          }

        }
      }

    }
    Sys.sleep(increment)
    elapsed = elapsed + increment
    cat("Waking up, still",sum(!waitForReady),"files to read out of",length(waitForReady),
        " and ",timeLimit-elapsed," seconds to go\n")
  }
  cat("Done with the",length(handlers),"handlers\n")
  return(responses)

}

collectResults = function(handlers,
                          removeLogs=T,
                          removeData=T){

  if(typeof(handlers) == "character")
    handlers = readRDS(handlers)
  results = finished = NULL

  for(index in 1:length(handlers)){
    if(!is.null(handlers[[index]])){
      if(file.exists(handlers[[index]]$outfile)){
        #cat("Job from",handlers[[index]]$outfile,"finished\n")
        results[[index]] = readRDS(handlers[[index]]$outfile)
        finished = c(finished,index)
      }
    }
  }

  return(list(indexes=finished,results=results))
}

reportOnJobs = function(handlers,wd="~/tmp/"){
  if(typeof(handlers) == "character")
    handlers = readRDS(handlers)

  running = finished = wrong = NULL

  for(index in 1:length(handlers)){
    if(!is.null(handlers[[index]])){
      if(file.exists(handlers[[index]]$outfile)){
        #cat("Job from",handlers[[index]]$outfile,"finished\n")
        finished = c(finished,index)
      }else{
        if(!jobRunning(handlers[[index]]$jobname,wd=wd)){
          #cat("No results and no job running:",handlers[[index]]$jobname,"\n")
          wrong = c(wrong,index)
        }else{
          running = c(running,index)
        }
      }
    }

  }
  cat("Total Jobs",length(handlers),"were launched\n")
  if(!is.null(running))
     cat(length(running),"jobs running\n")
     else
         cat("No jobs running\n")

  if(!is.null(wrong))
         cat(length(wrong),"jobs wrong, neither running nor results available\n")
  else
    cat("No wrong jobs so far\n")

  if(!is.null(finished))
    cat(length(finished),"jobs done\n")
  else
         cat("No jobs finished\n")

  missing = length(handlers) - length(finished) - length(running) - length(wrong)
  if(missing)
    cat(missing,"jobs are missing\n")

  cat("Done with the",length(handlers),"handlers\n")
  return(list(finished=handlers[finished],
              running=handlers[running],
              wrong=handlers[wrong]))

}


