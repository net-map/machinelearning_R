#!/bin/bash
kill $(pgrep Rserve)
Rscript  initService.r
