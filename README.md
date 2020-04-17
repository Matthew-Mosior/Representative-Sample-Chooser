# Representative-Sample-Chooser: A Hierarchical Filtering Tool

## Introduction

**Representative-Sample-Chooser (RSC)** is a computational tool for hierarchically filtering tab-delimited files.<br/>
This haskell script takes in an identifier field string, a hierarchical filter string and a tsv file in order capture **best** record for each identifier.<br/><br/>

**RSC** outputs a single, filtered tsv file on successful exit, or an error message/log depending on the issue (more on this to come).<br/>

## Prerequisites

**rsc.hs** assumes you have a the [GHC](https://www.haskell.org/ghc/) compiler and packages installed that it imports.  The easiest way to do this is to download the [Haskell Platform](https://www.haskell.org/platform/).<br/><br/>

## Installing required packages

To install the peripheral packages **rsc.hs** requires, you can call the following command assuming you have [cabal](https://www.haskell.org/cabal/), a package manager and build system for Haskell, installed on your system (it comes with the [Haskell Platform](https://www.haskell.org/platform/)).<br/><br/>
`$ cabal install [packagename]`<br/><br/>

**Required packages**
- Control.DeepSeq
- Data.ByteString
- Data.ByteString.Char8
- Data.ByteString.Lazy
- Data.Char
- Data.Functor
- Data.List
- Data.List.Split
- Data.Ord
- System.Console.GetOpt
- System.Process
- System.Environment
- System.Exit
- System.IO 
- System.IO.Temp 
- Text.PrettyPrint.Boxes
- Text.Regex.TDFA

## Input

**RSC** requires three inputs:<br/><br/>

1. **Identifier Field String** - This string will define the field (column) on which the data compression occurs.<br/>

2. **Hierarchical Filter String** - This string will define a hierarchy of fields upon which to filter on.  Within each field, a hierarchy of values will define the way in which filtering will occur (see example below).<br/>

3. **TSV file** - This is tab-delimited file on which the hierarchical filtering will occur.<br/>

## Usage

**rsc.hs** is easy to use.<br/><br/>
You can call it using the **runghc** command provided by the GHC compiler as such:<br/>
`$ runghc rsc.hs -o name_of_filtered_file.tsv ";UPN_clinical;" ";Clinical_T_sequenced:T1>=T2>=T3>=T4>=T5>=T6>=T7>=T9>=Tn>=NA;model_group_reagent:combined_exome_capture>=exome>=capture_v2>=capture;common_name:relapse flow sorted>=relapse>=tumor>=normal;mean_depth:FLOAT.>;" ../path/to/input/file.tsv`<br/><br/>
For maximum performance, please compile and run the source code as follows:<br/>
`$ ghc -O2 -o RSC rsc.hs`<br/>
`$ ./RSC -o name_of_filtered_file.tsv ";UPN_clinical;" ";Clinical_T_sequenced:T1>=T2>=T3>=T4>=T5>=T6>=T7>=T9>=Tn>=NA;model_group_reagent:combined_exome_capture>=exome>=capture_v2>=capture;common_name:relapse flow sorted>=relapse>=tumor>=normal;mean_depth:FLOAT.>;" ../path/to/input/file.tsv`<br/><br/>

## Arguments

**RSC** has few different command line arguments:<br/>
```
Representative Sample Chooser, Copyright (c) 2020 Matthew Mosior.
Usage: rsc [-vV?o] [Identifier Field String] [Hierarchical Filter String] [TSV file]

  -v          --verbose             Output on stderr.
  -V, -?      --version             Show version number.
  -o OUTFILE  --outputfile=OUTFILE  The output file to which the results will be printed.
              --nonexhaustive       First sample will be returned for identifiers with
                                    non-exhaustive hierarchical filtering values.
              --help                Print this help message.
```
The `-v` option, the `verbose` option, will provide a full error message.<br/>
The `-V` option, the `version` option, will show the version of `rsc` in use.<br/>
The `-o` option, the `outputfile` option, is used to specify the file in which the filtered lines will be printed to.<br/>
The `--nonexhaustive` option specifies to print the first record for all given identifiers in which non-exhaustive filtering occured.<br/>
Finally, the `--help` option outputs the `help` message seen above.<br/><br/>

## Some Examples

## Docker

A docker container exists that contains all the necessary software to run **RSC**: `matthewmosior/representativesamplechooser:final`<br/><br/>

## Credits

Documentation was added April 2020.<br/>
Author : [Matthew Mosior](https://github.com/Matthew-Mosior)
