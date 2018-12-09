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

> devtools::install_github(repo="juanbot/sgefacilities")

> withr::with_libpaths(new="~/R/x86_64-redhat-linux-gnu-library/3.5/",devtools::install_github(repo="juanbot/sgefacilities"))


