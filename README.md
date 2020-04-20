# Representative-Sample-Chooser: A Hierarchical Filtering Tool

## Introduction

**Representative-Sample-Chooser (RSC)** is a computational tool for hierarchically filtering tab-delimited files.<br/>
This haskell script takes in an identifier field string, a hierarchical filter string and a tsv file in order capture **best** record for each identifier.<br/><br/>

**RSC** outputs a single, filtered tsv file on successful exit, or an error message/log depending on the issue (more on this later).<br/>

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

1. **Identifier Field String** - This string defines the field (column) on which the data compression occurs.<br/><br/>

The **Identifier Field String** has the following structure:<br/>
`;[Identifier Field String];`<br/><br/>

2. **Hierarchical Filter String** - This string defines a hierarchy of fields upon which to filter on.  Within each field, a hierarchy of values will define the way in which filtering will occur (see example below).<br/><br/>

The **Hierarchical Filter String** has the following structure:<br/>
`;[FIELDNAME1]:[FIELDVALUE1]>=[FIELDVALUE2]>=...>=[FIELDVALUEN];[FIELDNAME2]:[FIELDVALUE1]>=[FIELDVALUE2]>=...>=[FIELDVALUEN];`<br/><br/>

The order in which the user provides the field names and corresponding field values is **important**, the first field name specified will be the first field used for comparison, and so on.<br/><br/>

The following keywords can be used to identify fields which are numeric in nature (float or int):<br/>
- Float -> **FLOAT**
- Int   -> **INT**<br/><br/>

The following keycharacters can be used with the above keywords to define the **type** of comparison:<br/>
- Maximum -> **>**
- Minimum -> **<**<br/><br/>

If the user has not specified all possible values for a given field, the program will exit early, and print a file named `rsc_ERROR.log`, detailing all field(s) with missing values, and what those missing values are:<br/>
`Name_of_Field_(Column) Values_Found_in_Input_TSV_Not_Found_in_Hierarchical_Filter_String Values_Found_in_Hierarchical_Filter_String_Not_Found_in_Input_TSV`<br/><br/>

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

The following examples will help illustrate the way the hierarchical filtering algorithm chooses a **best** record for each given identifier.<br/><br/>

The following two examples assume the following inputs:<br/><br/> 
**Identifier Field String**: `;Sample_Group_ID;`<br/>
**Hierarchical Filter String**: `;Time_point:T1>=T2>=T3;Type_of_data:complex>=simple>=NA;Data_depth:FLOAT.>;`<br/><br/>

Each of the following examples are illustrating the hierarchical filtering on a **single** identifier for simplicity sake.<br/><br/>

### Example 1: 

This example will illustrate a scenario where a single record is returned (user-defined hierarchical filter determined it was the best record for said identifier).<br/><br/>

The hierarchical filtering starts on the most important field as described by the **Hierarchical Filter String**, **Time_point**.<br/><br/>

|   | Sample_Group_ID| Time_point     | Type_of_data| Data_depth|
|:-:|:--------------:|:--------------:|:-----------:|:---------:|
|1  | 200ABC         | T1<br/>**tied**| simple      | 100.19    |
|2  | 200ABC         | T1<br/>**tied**| complex     | 65.32     |          
|3  | 200ABC         | T1<br/>**tied**| complex     | 106.78    |          

There is a three way tie between all three lines due to the values in the **Time_point** field, so the filtering then moves onto the next most important field as described by the **Hierarchical Filter String**, **Type_of_data**, and all three lines are still being compared.<br/><br/> 

|   | Sample_Group_ID| Time_point     | Type_of_data        | Data_depth|
|:-:|:--------------:|:--------------:|:-------------------:|:---------:|
|1  | 200ABC         | T1<br/>**tied**| simple              | 100.19    |
|2  | 200ABC         | T1<br/>**tied**| complex<br/>**tied**| 65.32     |          
|3  | 200ABC         | T1<br/>**tied**| complex<br/>**tied**| 106.78    |

There is a two-way tie between lines 2 and 3 due to the values in the **Type_of_data** field, so the filtering then moves on to the next most important field as described by the **Hierarchical Filter String**, **Data_depth**, and is restricted to just lines 2 and 3.<br/><br/>

|   | Sample_Group_ID| Time_point     | Type_of_data        | Data_depth          |
|:-:|:--------------:|:--------------:|:-------------------:|:-------------------:|
|1  | 200ABC         | T1<br/>**tied**| simple              | 100.19              | 
|2  | 200ABC         | T1<br/>**tied**| complex<br/>**tied**| 65.32               |          
|3  | 200ABC         | T1<br/>**tied**| complex<br/>**tied**| 106.78<br/>**wins** |

Because the **Hierarchical Filter String** was defined as `FLOAT.>`, the largest float value in the field would win the comparison between lines 2 and 3.<br/><br/>

So **line 3** was the choosen record for this given identifier.<br/><br/>

### Example 2:

This example will illustrate a scenario where no record is returned (user-defined hierarchical filter could not determine a best record for said identifier).<br/><br/>

The hierarchical filtering starts on the most important field as described by the **Hierarchical Filter String**, **Time_point**.<br/><br/>

|   | Sample_Group_ID| Time_point     | Type_of_data| Data_depth|
|:-:|:--------------:|:--------------:|:-----------:|:---------:|
|1  | 200ABC         | T1<br/>**tied**| complex     | 101.10    |
|2  | 200ABC         | T1<br/>**tied**| complex     | 101.10    |          
|3  | 200ABC         | T1<br/>**tied**| complex     | 101.10    |

There is a three way tie between all three lines due to the values in the **Time_point** field, so the filtering then moves onto the next most important field as described by the **Hierarchical Filter String**, **Type_of_data**, and all three lines are still being compared.<br/><br/>

|   | Sample_Group_ID| Time_point     | Type_of_data             | Data_depth|
|:-:|:--------------:|:--------------:|:------------------------:|:---------:|
|1  | 200ABC         | T1<br/>**tied**| complex<br/>**tied**     | 101.10    |
|2  | 200ABC         | T1<br/>**tied**| complex<br/>**tied**     | 101.10    |          
|3  | 200ABC         | T1<br/>**tied**| complex<br/>**tied**     | 101.10    |

There is a three way tie between all three lines due to the values in the **Type_of_data** field, so the filtering then moves onto the next most important field as described by the **Hierarchical Filter String**, **Data_depth**, and all three lines are still being compared.<br/><br/>

|   | Sample_Group_ID| Time_point     | Type_of_data             | Data_depth            |
|:-:|:--------------:|:--------------:|:------------------------:|:---------------------:|
|1  | 200ABC         | T1<br/>**tied**| complex<br/>**tied**     | 101.10<br/>**tied**   |
|2  | 200ABC         | T1<br/>**tied**| complex<br/>**tied**     | 101.10<br/>**tied**   |          
|3  | 200ABC         | T1<br/>**tied**| complex<br/>**tied**     | 101.10<br/>**tied**   |

There is a three way tie between all three lines due to the values in the **Data_depth** field, so there is no **best** record for this identifier.<br/><br/>
By default, in this scenario, no record will be returned for this identifier.<br/><br/>
In this scenario, the `--nonexhaustive` option (**optional**) will grab the first record for this identifier and return it.<br/><br/>

## Docker

A docker container exists that contains all the necessary software to run **RSC**: `matthewmosior/representativesamplechooser:final`<br/><br/>

## Credits

Documentation was added April 2020.<br/>
Author : [Matthew Mosior](https://github.com/Matthew-Mosior)
