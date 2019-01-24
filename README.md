---
title: "Introduction to SGEfacilities software"
author: "Juan A. Botía"
date: "08/12/2018"
output: html_document
---


# Intro

This is a software to help submitting, controlling and recovering results from jobs in an SGE environment. Just started with the implementation but this will grow and get more stable soon. This readme has three parts. The installation and example of use sections, introducing the R package and how to use it. The third part is a section on how to use a cluster from Unix-like systems. There is a lack of standardization in this area and very irregular documentation. I try to summaryse all the basics you need to survive sending your scripts to the cluster. 

# Install

To install, just do it from the R shell

```r
devtools::install_github(repo="juanbot/sgefacilities")`
```

And if you wanted to install the software locally, just try this, but with 

```r
withr::with_libpaths(new="~/R/x86_64-redhat-linux-gnu-library/3.5/",
  devtools::install_github(repo="juanbot/sgefacilities"))`
```

# An example on how to use it

First of all, we need to define the function to launch in the cluster, for example, 

```r
myFunction = function(infile="",
                      tpm=0.5,
                      visibility=0.8,
                      samps,
                      tissue,
                      its=20,
                      outfolder="/tmp/"){
  source("~/coexp/coexpression.R",chdir=T,verbose=F)
  cat("Calling myFunction with following parameters\n")
  cat("Data file",infile,"\n")
  cat("TPM threshold",tpm,"\n")
  cat("Visibility threshold",visibility,"\n")
  cat("Using ",length(samps)," to create the network\n")
  cat("Tissue",tissue,"\n")
  cat("k-means iterations",its,"\n")
  cat("We'll write results in",outfolder,"\n")

  datain = read.delim(infile,
                      row.names=NULL,stringsAsFactors=F,header=T)
  datain = datain[,-1]
  print(dim(datain))
  #for(i in ncol(datain)) datain[,i] = as.numeric(datain[,i])
  datain = log2(1 + t(as.matrix(datain)))
  activeGenesMask = colSums(datain > tpm) > visibility*(nrow(datain))
  cat(sum(activeGenesMask)," expressed for TPM > ",tpm,"\n")
  datain = datain[,activeGenesMask]

  datain = datain[samps,]

  net = coexp.getDownstreamNetwork(tissue=tissue,
                                   n.iterations=its,
                                   net.type = "signed",
                                   debug=F,
                                   expr.data=datain,
                                   job.path=outfolder)
  return(net)
}
```

As you see, this function creates a co-expression network using the coexp package, with a list of parameters for creating and saving it. It also returns the network filename.

This other function launches a series of jobs using `myFunction()` as the main code within the Job.

```r
mainFunction = function(infile,outfolder,tissue){
  datain = read.delim(infile,
                      row.names=NULL,
                      stringsAsFactors=F,
                      header=T)

  datain = datain[,-1]
  nsamples = ncol(datain)
  results = NULL
  params = NULL
  params$visibility = 0.8
  params$tpm = 5
  params$its = 20
  params$outfolder = outfolder
  params$infile = infile
  handlers = NULL
  #nsamples = 5
  for(i in 1:nsamples){
    ltissue =
      params$tissue = paste0(tissue,"_minus_",i)
    params$samps = (1:nsamples)[-i]
    params$fun = myFunction
    hanlders = c(handlers,launchJob(params,"","MyExperiment"))
  }
  saveRDS(handlers,paste0(outfolder,"/handlers.rds"))
}
```

The function iterates over a series of samples an launches a network job with `launchJob` using as arguments the list of parameters expected by `myFunction`, an empty string for the queue system (no parameters needed in this case) and an ID for the experiment to distinguish it from others. We store the handers in a list for latter gathering of results.

# Basic manual for the cluster survival

This section is of interest if you want to learn the basics of SGE-like clusters. Not many complications as you will see if you are and R user.

Working in a cluster environment is rather special. If you want to get the most out of it, you have to firstly understand what a cluster is. A cluster has three main parts. One is the mother node, MON. The second one are the computing nodes, CON. The third one is the jobs queue. 

The MON node is the one normally we do ssh to connect to. This is the only node we have access to, directly. We can normally work on the MON as it were a conventional linux machine but one should never use it to run heavy scripts. It is not for that. The MON is used to launch your scripts at the CONs. 

A CON node is a node that can be physically located anywhere (actually we don´t care) and has the same operating system than you have in the MON. But most importantly, as a node, it sees the same filesystem the CONs sees. Which means that if a CON node tries to run a script which is located in one of your folders, it will run without problems as far as it has the right invocation parameters to use with the script. Note we do not have to be aware of any CON node. This is managed by the cluster software for us.

Finally, the jobs queue is an structure we can access to see the state of our submitted jobs: waiting, running, and finished with errors. If the job is not there is because it finished anyway (maybe also with errors). We can only read the queue, not modify it.

We can do, from the MON node

```
> qstat 
```

And it will print something like this

```
[juanbot@cluster]$ qstat
Job ID                    Name             User            Time Use S Queue
------------------------- ---------------- --------------- -------- - -----
31402.scc_ser_1            Ph_COelS.job     artuesp         8338:24: R batch          
36347.scc_ser_1            COelS.job        artuesp         4204:37: R batch          
36698.scc_ser_1            ...ccpvtz_gas_of jpablo          3815:56: R batch          
37649.scc_ser_1            ...as_of_confor4 jpablo          2908:16: R batch          
37650.scc_ser_1            ...as_of_confor5 jpablo          2918:18: R batch          
37654.scc_ser_1            ...as_of_confor9 jpablo          3035:42: R batch          
37655.scc_ser_1            ...s_of_confor10 jpablo          3036:22: R batch          
37656.scc_ser_1            ...s_of_confor11 jpablo          3040:31: R batch          
37658.scc_ser_1            ...s_of_confor13 jpablo          3039:23: R batch
```

We see that there are two users with jobs right now, artuesp and jpablo. The jobs they are running are distinguished by the JOB id, at the 1st column. The Names of the jobs are set by you when submitting (we´ll see how), Time refers to the running time and S to the state (all them are running in the example). In this clusters there is only one queue active, called “batch”. 

The simplest example on how to launch a job to the cluster can be the following
```
> echo “ls -lt” | qsub -S /bin/bash -N MyExampleJob -o ~/mylogfolder/logfile.o -e ~/mylogfolder/logfile.e
```

If we run this and immediately after we do

```
> qstat
```

We see our job running

```
40188.scc_ser_1            irc+4.job        artuesp         01:13:39 R batch          
40189.scc_ser_1            irc-4.job        artuesp         01:12:52 R batch          
40190.scc_ser_1            CCelSc.job       artuesp         00:49:14 R batch          
40191.scc_ser_1            ..._def2TZVP.job alicia          00:05:09 R batch          
40192.scc_ser_1            MyExampleJob     juanbot                0 R batch   
```

And after some seconds it will finish and disappear. What did the job run actually? The key is the qsub command. It accepts jobs from files but also from the standard input (very useful feature indeed). So using the pipeline operator | one can send the command “ls -lt” that simply lists all files is at my home folder, ordered by modification time and in long format. The echo command is used to generate the output to the standard input of qsub.

But where is the result of the ls command? As we indicated with the -o flag, all the standard output of the job will be stored at ~/mylogfolder/logfile.o. And the errors that might occur can be found at ~/mylogfolder/logfile.e. You can use whichever names and location you want for your files. Obviously, the folder must exist before you run the job. So if we do

```
> ls ~/mylogfolder/
logfile.e  logfile.o
```

And the files are there. And if we see what is inside we´ll see the result of the ls command. Note that if the result of your code is not something that should go to the standard output as, for example, a matrix or a data.frame, then that should be permanently stored in a file before the script ends. This is not done by the cluster for you. You have to do it at your code. As in this other example: 

```
echo "ls -lt > ~/mylogfolder/myresult.txt" | qsub -S /bin/bash -N MyExampleJob -o ~/mylogfolder/logfile.o -e ~/mylogfolder/logfile.e
```

You will see now there is nothing at the .o file as the result was stored at the myresult.txt file. 

The only ways in which we can modify the queue is deleting jobs you do not need. For example, if there is a job with id (first column of the results to qstat) 37650.scc_ser_1 and it belongs to you, then you can simply do

```
> qdel 37650.scc_ser_1
```

And the job would be stopped. Normally when you launch many jobs in a row, they have correlative ID numbers as in 37650.scc_ser_1, 37651.scc_ser_1, 37652.scc_ser_1, …

You can kill a bunch of correlative jobs with this simple shell command

```
> for i in {37650.. 37652}; do qdel $i.scc_ser_1; done
```

Normally you would have to do this when you realise that you did some mistake specifying the parameters to jobs or that they won´t work anyway. Better not to waste resources in the cluster waiting for them to finish. 

Now you want to run an R script which is self-contained, i.e. you would normally do, from the R console, 

```r
source(“myscript.R”)
```

And it would work. But you want to do it in the cluster. You can use, from your linux shell

```
> echo "Rscript ~/myscript.R" | qsub -N MyScriptJob -o ~/mylogfolder/Job.o -e ~/mylogfolder/Job.e
```

And that would do the job. If the script is so simple you don´t want to write one R file just for that, then you can do the following. In this example, we first load some code I need for the rest to work properly, then a library and then I call a function with parameters. Note how we use the scape character “\” so the different softwares that process the string do not confound the inner “ characters with the end of the string.

```
> echo "Rscript -e \"source(\\\"~/someSourceCodeIneed.R\\\"); library(MyLibrary); myFunction(input_a=\\\”examplestring\\\”,param_b=3)\"" | qsub -S /bin/bash -N MyJob -o ~/mylogfolder/Job.o -e ~/mylogfolder/Job.e
```




