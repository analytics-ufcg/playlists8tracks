#!/bin/bash

inputFile=$1

outputFile=$2

errorFile=$3

CLASSPATH=coletaecho.jar:jEN.jar:json_simple-1.1.jar     

java -cp $CLASSPATH coleta.CriaBaseSongs ${inputFile} ${outputFile} ${errorFile}  