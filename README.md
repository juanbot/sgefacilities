---
title: "Introduction to SGEfacilities software"
author: "Juan A. Botía"
date: "08/12/2018"
output: html_document
---


# Intro

This is a software to help submitting, controlling and recovering results from jobs in an SGE environment. Just started with the implementation but this will grow and get more stable soon.

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

#An example on how to use it

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

