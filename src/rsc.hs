{-=RepresentativeSampleChooser (RSC): A Haskell-based solution to=-}
{-=selecting a single sample for each identifier that=-}
{-=may have multiple samples.=-}
{-=Author: Matthew Mosior=-}
{-=Version: 1.0=-}
{-=Synopsis:  This Haskell Script will take in=-}
{-=a user defined tsv file, a user defined identifier string,=-}
{-=a user defined hierarchical filtering.  The identifer string=-}
{-=will be used to group records together, and a single record=-}
{-=will be kept for each identifier based on the hierarchical filters=-}
{-=defined by the user.  An appropriately row-filtered tsv file=-}
{-=will be returned.=-} 


{-Lanuguage Extension.-}

{-# LANGUAGE MultiWayIf #-}

{----------------------}


{-Imports-}

import Control.DeepSeq as CD
import Data.ByteString as DB
import Data.ByteString.Char8 as DBC
import Data.ByteString.Lazy as DBL
import Data.Char as DC
import Data.Functor as DF
import Data.List as DL
import Data.List.Split as DLS
import Data.Ord as DO
import System.Console.GetOpt as SCG
import System.Process as SP
import System.Environment as SE
import System.Exit as SX
import System.IO as SIO
import System.IO.Temp as SIOT
import Text.PrettyPrint.Boxes as TPB
import Text.Regex.TDFA as TRTDFA

{---------}


{-Global variables.-}

--Reserved keywords for RSC hierarchical filter string.
keywords :: [String]
keywords = ["FLOAT","INT"]
-------------------------------------------------------

--keycharacters -> Accepted characters that are allowed
--to make up hierarchical filter string.
keycharacters :: [String]
keycharacters = [";",":",">=","FLOAT","INT",">","<"]
-------------------------------------------------------
----------------------------------------

{-------------------}


{-Custom CML Option Datatype.-}

data Flag
    = Verbose           -- -v
    | Version           -- -V -?
    | OutputFile String -- -o
    | NonExhaustive     -- --nonexhaustive
    | Help              -- --help
    deriving (Eq,Ord,Show)

{-----------------------------}


{-Custom bool functions for Flag Datatype.-}

--isOutputFile -> This function will
--test for OutputFile flag.
isOutputFile :: Flag -> Bool
isOutputFile (OutputFile _) = True
isOutputFile _              = False

--isNonExhaustive -> This function will
--test for nonExhaustive flag.
isNonExhaustive :: Flag -> Bool
isNonExhaustive NonExhaustive = True
isNonExhaustive _             = False

{------------------------------------------}


{-Custom extraction functions for Flag Datatype.-}

--extractOutputFile -> This function will
--extract the string associated with
--OutputFile.
extractOutputFile :: Flag -> String
extractOutputFile (OutputFile x) = x

{------------------------------------------------}


{-Option Description function relating to datatype above.-}

--options -> This function will
--describe flags.
options :: [OptDescr Flag]
options =
    [ Option ['v']     ["verbose"]                 (NoArg Verbose)               "Output on stderr.",
      Option ['V','?'] ["version"]                 (NoArg Version)               "Show version number.",
      Option ['o']     ["outputfile"]              (ReqArg OutputFile "OUTFILE") "The output file to which the results will be printed.",
      Option []        ["nonexhaustive"]           (NoArg NonExhaustive)         "First sample will be return for identifiers with\n\ 
                                                                                 \non-exhaustive hierarchical filtering values.",
      Option []        ["help"]                    (NoArg Help)                  "Print this help message."
    ]

--compilerOpts -> This function will
--parse incoming command line arguments.
compilerOpts :: [String] -> IO ([Flag],[String])
compilerOpts argv = 
    case getOpt Permute Main.options argv of
        (args,files,[]) -> do  
            --If user provides input.
            if ((not (DL.null files)) && (DL.length files == 3))
                --Run hierarchicalCompletenessTest (if file arguments are provided).
                then do hierarchicalCompletenessResult <- hierarchicalCompletenessTest (files DL.!! 2) (files DL.!! 1)
                        if hierarchicalCompletenessResult
                            then do SIO.hPutStrLn stderr (hcompleteerror
                                                       ++ github
                                                       ++ SCG.usageInfo header Main.options)
                                    hierarchicalCompletenessPrint (files DL.!! 2) (files DL.!! 1)
                                    SX.exitWith (SX.ExitFailure 1)
                            else return (DL.nub args, files)
                --If 3 file arguments aren't provided.
                else if ((not (DL.null files)) && (DL.length files /= 3))
                    then do SIO.hPutStrLn stderr (flerror
                                        ++ github
                                        ++ SCG.usageInfo header Main.options)
                            SX.exitWith (SX.ExitFailure 1) 
                    --Walk through cases.
                    else if DL.elem Help args
                        then do SIO.hPutStrLn stderr (greeting 
                                                   ++ SCG.usageInfo header Main.options)
                                SX.exitWith SX.ExitSuccess
                        else if DL.elem Version args
                            then do SIO.hPutStrLn stderr (greeting 
                                                       ++ version 
                                                       ++ SCG.usageInfo header Main.options)
                                    SX.exitWith SX.ExitSuccess
                            else if (not (identifierFieldCheck (files DL.!! 0)))
                                then do SIO.hPutStrLn stderr (iserror 
                                                           ++ github 
                                                           ++ SCG.usageInfo header Main.options)
                                        SX.exitWith (SX.ExitFailure 1)
                                else return (DL.nub args, files)
        (_,_,errors) -> do
            SIO.hPutStrLn stderr (DL.concat errors ++ SCG.usageInfo header Main.options)
            SX.exitWith (SX.ExitFailure 1)
        where
            greeting        = "Representative Sample Chooser, Copyright (c) 2020 Matthew Mosior.\n"
            header          = "Usage: rsc [-vV?o] [Identifier Field String] [Hierarchical Filter String] [TSV file]\n"
            version         = "Representative Sample Chooser (RSC), Version 1.0.\n"
            github          = "Please see https://github.com/Matthew-Mosior/Representative-Sample-Chooser/wiki for more information.\n"
            flerror         = "Incorrect number of input arguments:\n\
                              \Please provide:\n\
                              \First argument  -> Identifier Field String\n\
                              \Second argument -> Hierarchical Filter String\n\
                              \Third argument  -> TSV file\n"  
            ismisserror     = "\nIdentifier string missing.\n\
                              \Please provide the identifier string with the following structure:\n\
                              \;[FIELDNAME];\n"
            iserror         = "\nIncorrect structure of identifier string.\n\
                              \Please provide the identifier string with the following structure:\n\
                              \;[Identifier string];\n"
            ismulterror     = "\nYou have provided to many identifier strings.\n\
                              \Please provide only one identifier string.\n"
            hmisserror      = "\nHierarchical filter string missing.\n\
                              \Please provide the hierarchical filter string with the following structure:\n\
                              \;[FIELDNAME1]:[FIELDVALUE1]->[FIELDVALUE2]->...[FIELDVALUEN];[FIELDNAME2]:[FIELDVALUE1]->[FIELDVALUE2]->...[FIELDVALUEN];...;\n"
            herror          = "\nIncorrect structure of hierarchical filter string.\n\
                              \Please provide the hierarchical filter string with the following structure:\n\
                              \;[FIELDNAME1]:[FIELDVALUE1]->[FIELDVALUE2]->...[FIELDVALUEN];[FIELDNAME2]:[FIELDVALUE1]->[FIELDVALUE2]->...[FIELDVALUEN];...;\n"
            hcompleteerror  = "\nHierarchical filter string provided doesn't cover all possible values\n\
                              \for the fields found in the column of output below (non-empty) named\n\ 
                              \Values Found in Input TSV Not Found in Hierarchical Filter String:\n\n"

{---------------------------------------------------------}


{-Identifier Field Functions.-}

--identifierFieldCheck -> This function will
--check that the identifier field string
--provided by the user has the correct structure.
identifierFieldCheck :: String -> Bool
identifierFieldCheck xs = if DL.head xs == ';' &&
                             DL.last xs == ';' &&
                             DL.length (DL.filter (\x -> x == ';') xs) == 2
                                 then True
                                 else False  

{-----------------------------}


{-Hierarchical String Check Functions.-}  

--hierarchicalCompletenessTest -> This function will
--check that each user defined hierarchical
--field contains total coverage of possible entries.
hierarchicalCompletenessTest :: String -> String -> IO Bool
hierarchicalCompletenessTest []        []          = return False
hierarchicalCompletenessTest _         []          = return False
hierarchicalCompletenessTest []        _           = return False
hierarchicalCompletenessTest inputfile hierarchstr = do 
    --Read in the file.
    readinputfile <- SIO.readFile inputfile
    --Apply lineFeedTabOnly function to inputfile.
    let processedfile = lineFeedTabOnly readinputfile
    --Apply transposition to processedfile.
    let transposedfile = DL.transpose processedfile
    --Compare processedfile and hierarchstr.
    let hierarchicalfinaltest = hierarchicalCompletenessSmall transposedfile onlydefinitions
    --Test hierarchicalfinaltest.
    if not (DL.null (DL.filter (\(_,b,c) -> (not (DL.null b) || DL.null c) ||
                                            (DL.null b || not (DL.null c)) ||
                                            (not (DL.null b) || not (DL.null c))) hierarchicalfinaltest))
        then return True
        else return False 
            where
                --Local definitions.--
                --onlydefinitions -> Just the hierarchical filter definitions.
                onlydefinitions = DLS.splitOn ";" (DL.init (DL.tail hierarchstr))
                ----------------------

--hierarchicalCompletenessPrint -> This function will
--print the results of hierarchicalCompleteness.
hierarchicalCompletenessPrint :: String -> String -> IO ()
hierarchicalCompletenessPrint []        []          = return ()
hierarchicalCompletenessPrint _         []          = return ()
hierarchicalCompletenessPrint []        _           = return ()
hierarchicalCompletenessPrint inputfile hierarchstr = do
    --Read in the file.
    readinputfile <- SIO.readFile inputfile
    --Apply lineFeedTabOnly function to inputfile.
    let processedfile = lineFeedTabOnly readinputfile
    --Apply transposition to processedfile.
    let transposedfile = DL.transpose processedfile
    --Compare processedfile and hierarchstr.
    let hierarchicalfinallist = hierarchicalCompletenessSmall transposedfile onlydefinitions
    --Create datalist to take input of tuples and turn into list of lists.
    let datalist = DL.map (\(a,b,c) -> [a,DL.intercalate "," b,DL.intercalate "," c]) hierarchicalfinallist
    --Create finaldatalist that adds a header describing columns of datalist.
    let finaldatalist = ["Name_of_Field_(Column)",
                         "Values_Found_in_Input_TSV_Not_Found_in_Hierarchical_Filter_String",
                         "Values_Found_in_Hierarchical_Filter_String_Not_Found_in_Input_TSV"] : datalist
    --mapNotLast tabs and newlines in finaldatalist.
    let finaldatalisttabsandnewlinesadded = DL.map (mapNotLast (++ "\t")) finaldatalist
    --Write the output to the user-specified filename.
    SIO.putStrLn "Please check rsc_ERROR.log for more details.\n"
    SIO.writeFile ("rsc_ERROR.log") $
                  (TPB.render $
                  (TPB.hsep 0 TPB.left . DL.map (TPB.vcat TPB.left) . DL.map (DL.map (TPB.text)))
                  (DL.transpose finaldatalisttabsandnewlinesadded))
    where
        --Local definitions.--
        --onlydefinitions -> Just the hierarchical filter definitions.
        onlydefinitions = DLS.splitOn ";" (DL.init (DL.tail hierarchstr))
        ----------------------

--hierarchicalCompletenessSmall -> This function will
--check that each hierarchical field is fully
--satisfied.
hierarchicalCompletenessSmall :: [[String]] -> [String] -> [(String,[String],[String])]
hierarchicalCompletenessSmall [] [] = []
hierarchicalCompletenessSmall [] _  = []
hierarchicalCompletenessSmall _  [] = []
hierarchicalCompletenessSmall xs ys = hierarchicalLists zippedinputfile zippedfields
    where
        --Local definitions.--
        --zippedinputfile -> To hold inputfileheaders and inputfileunique
        --(without keyword fields if applicable).
        zippedinputfile = DL.filter (not . (\(x,_) -> x `DL.elem` fieldstoremove))
                                    (DL.zip inputfileheaders inputfilehieruniquedata)
        --zippedfields -> To hold fieldheaders and fielddata
        --(without keyword fields if applicable).
        zippedfields = DL.filter (not . (\(x,_) -> x `DL.elem` keywords))
                                 (DL.zip fieldheaders fielddata) 
        --inputfileheaders -> To grab just the column headers from
        --xs for each of the specified columns hierarchical filter string.
        inputfileheaders = DL.map (DL.head) inputfilehieralldata
        --inputfilehieruniquedata -> To grab just the unique values
        --for each of the specified columns from inputfileheaders,
        inputfilehieruniquedata =  DL.map (DL.nub) (DL.map (DL.tail) inputfilehieralldata)
        --inputfilehieralldata -> To grab the columns that user
        --specified within the hierarchical filter string.
        inputfilehieralldata = DL.filter (\x -> (DL.head x) `DL.elem` fieldheaders) xs
        --fieldheaders -> To hold only the field headers of piece
        --of the hierarchical filter string.
        fieldheaders = DL.map (DL.head) (DL.map (DLS.splitOn ":") ys)
        --fielddata -> To hold the data associated with
        --each field header.
        fielddata = DL.map (splitOnAnyOf [">=","."]) (DL.map (DL.last)
                               (DL.map (DLS.splitOn ":") ys))
        --fieldstoremove -> To hold the fields that will
        --not be used in comparison.
        fieldstoremove = DL.map (fst) (DL.filter (\(_,y) -> y `isSubsetOf` keywords) zippedfields)
        ----------------------

--hierarchicalLists -> This function will
--compare the zipped input lists and return
--the list difference for directions of
--comparison.
hierarchicalLists :: [(String,[String])] -> [(String,[String])] -> [(String,[String],[String])]
hierarchicalLists []     [] = []
hierarchicalLists _      [] = []
hierarchicalLists []     _  = []
hierarchicalLists (x:xs) ys = [(fst x,(snd x) DL.\\ (DL.concat (DL.map (snd) (DL.filter (\(a,_) -> (fst x) == a) ys))),
                               (DL.concat (DL.map (snd) (DL.filter (\(a,_) -> (fst x) == a) ys))) DL.\\ (snd x))]
                           ++ (hierarchicalLists xs ys)

{--------------------------------}


{-General Utility Functions.-}

--lineFeed -> This function will
--read the file in and split on
--whitespace, returning a list
--of lists.
lineFeed :: String -> [[String]]
lineFeed [] = []
lineFeed xs = DL.map DL.words (DL.lines xs)

--lineFeedTabOnly -> This function will
--read a file in a split on tab
--characters, returning a list
--of lists.
lineFeedTabOnly :: String -> [[String]]
lineFeedTabOnly [] = []
lineFeedTabOnly xs = DL.map (DLS.splitOn "\t") (DL.lines xs)

--mapNotLast -> This function will
--work like the traditional map 
--function in Data.List, but not
--map to the last element of a list.
mapNotLast :: (a -> a) -> [a] -> [a]
mapNotLast fn []     = []
mapNotLast fn [x]    = [x]
mapNotLast fn (x:xs) = fn x : mapNotLast fn xs

--splitOnAnyOf -> This function will
--split a string on any of the characters.
splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf xs ys = DL.foldl' (\zs x -> zs >>= DLS.splitOn x) [ys] xs

--strongEqual -> This function will
--test if two lists contain same elements
--regardless of order.
strongEq :: (Eq a) => [a] -> [a] -> Bool
strongEq x y = DL.null (x \\ y) && DL.null (y \\ x)

--singleunnest -> This function will
--unnest a list.
singleunnest :: [a] -> a
singleunnest [a] = a

--isSubsetOf -> This function will
--be used to check if any element of
--one list is found in another list.
xs `isSubsetOf` ys = DL.any (`DL.elem` ys) xs

--cmpIndex -> To reorder a list based
--on another list.
cmpIndex :: [(String,[String])] -> (Int,String) -> (Int,String) -> Ordering
cmpIndex example s1 s2 = compare (DL.findIndex (snd s1 ==) (DL.map (fst) example)) 
                                 (DL.findIndex (snd s2 ==) (DL.map (fst) example))

--customZipWith3 -> This function will
--perform custom zipWith function on list of lists. 
customZipWith3 :: [[String]] -> [(Int,String,[String])] -> [[(Int,String,String)]]
customZipWith3 []     [] = []
customZipWith3 _      [] = []
customZipWith3 []     _  = []
customZipWith3 (x:xs) ys = [DL.map (\(a,(b,c)) -> (a,b,c))
                           (DL.zip correctindex
                           (DL.zip (DL.map (tripletSnd) ys) 
                           (DL.zipWith (DL.!!) (DL.repeat x) (DL.map (tripletFst) ys))))] 
                        ++ (customZipWith3 xs ys)
    where
        --Local definitions.--
        --correctindex -> To hold the correctindex.
        correctindex = DL.concat (DL.map (\a -> if ((tripletThd a) `isSubsetOf` ["FLOAT","INT","<",">"]) 
                                                    then [-1] 
                                                    else x DL.!! (tripletFst a) 
                                                        `DL.elemIndices` (tripletThd a)) correctlist)
        --correctlist -> To hold the correct sublists of ys for comparison.
        correctlist = DL.filter (\a -> ((x DL.!! (tripletFst a)) `DL.elem` (tripletThd a)) 
                                    || ((tripletThd a) `isSubsetOf` ["FLOAT","INT","<",">"])) ys
        ----------------------

{----------------------------}


{-Custom -uple extraction functions.-}

--tripletFst -> To extract the first element of a triplet.
tripletFst :: (a,b,c) -> a
tripletFst (a,_,_) = a

--tripletSnd -> To extract the second element of a triplet.
tripletSnd :: (a,b,c) -> b
tripletSnd (_,b,_) = b

--tripletThd -> To extract the third element of a triplet.
tripletThd :: (a,b,c) -> c
tripletThd (_,_,c) = c

--quadrupletFst -> To extract the first element of a quadruplet.
quadrupletFst :: (a,b,c,d) -> a
quadrupletFst (a,_,_,_) = a

--quadrupletSnd -> To extract the second element of a quadruplet.
quadrupletSnd :: (a,b,c,d) -> b
quadrupletSnd (_,b,_,_) = b

--quadrupletThd -> To extract the third element of a quadruplet.
quadrupletThd :: (a,b,c,d) -> c
quadrupletThd (_,_,c,_) = c

--quadrupletFrh -> To extract the fourth element of a quadruplet.
quadrupletFrh :: (a,b,c,d) -> d
quadrupletFrh (_,_,_,d) = d

--complexComplexTupleFst -> To extract the first element of a very complex tuple.
complexComplexTupleFst :: (Int, [([(Int,Int,String,String)],Int)]) -> Int
complexComplexTupleFst (a,_) = a

--complexComplexTupleFst -> To extract the second element of a very complex tuple.
complexComplexTupleSnd :: (Int, [([(Int,Int,String,String)],Int)]) -> [([(Int,Int,String,String)],Int)]
complexComplexTupleSnd (_,b) = b

--complexTupleFst -> To extract the first element of a complex tuple.
complexTupleFst :: ([(Int,Int,String,String)],Int) -> [(Int,Int,String,String)]
complexTupleFst (a,_) = a

--complexTupleSnd -> To extract the second element of a complex tuple.
complexTupleSnd :: ([(Int,Int,String,String)],Int) -> Int
complexTupleSnd (_,b) = b

{------------------------------------}


{-HIERARCHICAL FILTERING FUNCTIONS-}

--hierarchicalFiltering -> This function will
--perform hierarchical filtering on the input tsv
--file using the hierarchical filter string. 
hierarchicalFiltering :: String -> String -> [[String]] -> [[String]] -> [Flag] -> [[String]]
hierarchicalFiltering [] [] [] [] []    = []
hierarchicalFiltering as bs cs ds flags = hierarchicalFilter identifierstrgroups fieldcomplete flags
    where
        --Local definitions.--
        --identifierstrgroups -> Lines of cs grouped by identifer string.
        identifierstrgroups = DL.groupBy (\x y -> (DL.head x) == (DL.head y)) (DL.tail cs)
        --fieldcomplete -> To hold both the fieldheaderindices, fieldheaders and fielddata.
        fieldcomplete = DL.map (\((x,y),z) -> (x,y,z)) (DL.zip sortedfieldheaderindices (DL.map (snd) alldata)) 
        --sortedfieldheaderindices -> To order fieldheaderindices using alldata.
        sortedfieldheaderindices = DL.sortBy (cmpIndex alldata) fieldheaderindices
        --fieldheaderindices -> Grab the indices of the fieldheaders in the header line of cs 
        fieldheaderindices = DL.zip (DL.findIndices (\x -> x `DL.elem` fieldheaders) (DL.head cs)) 
                                    (DL.map (\x -> (DL.head cs) DL.!! x) 
                                    (DL.findIndices (\x -> x `DL.elem` fieldheaders) (DL.head cs)))
        --alldata -> To hold fieldheaders and fielddata.
        alldata = DL.zip fieldheaders fielddata
        --fielddata -> To hold the data associated with
        --each field header.
        fielddata = DL.map (splitOnAnyOf [">=","."]) (DL.map (DL.last)
                               (DL.map (DLS.splitOn ":") onlydefinitions))
        --fieldheaders -> To hold only the field headers of piece
        --of the hierarchical filter string.
        fieldheaders = DL.map (DL.head) (DL.map (DLS.splitOn ":") onlydefinitions)
        --onlydefinitions -> Just the hierarchical filter definitions.
        onlydefinitions = DLS.splitOn ";" (DL.init (DL.tail bs))
        ----------------------

--hierarchicalFilter -> This function will perform
--the requisite comparisons to choose a representative
--sample for each identifier.        
hierarchicalFilter :: [[[String]]] -> [(Int,String,[String])] -> [Flag] -> [[String]]
hierarchicalFilter []     []    []    = [] 
hierarchicalFilter []     []    (_:_) = []
hierarchicalFilter []     (_:_) _     = []
hierarchicalFilter (x:xs) ys    flags = (fieldComparator x ys flags) ++ (hierarchicalFilter xs ys flags)

--fieldComparator -> This function will compare
--the values of the fields with all possible values.
fieldComparator :: [[String]] -> [(Int,String,[String])] -> [Flag] -> [[String]]
fieldComparator [] [] []    = []
fieldComparator xs ys flags = --If all possible comparison fields had duplicate values,
                              --check for nonExhaustive flag.
                              --take first record using finalcomparisonlist.
                              if (DL.null (customComparator finalcomparisonlist ys xs))
                                  then if (DL.length (DL.filter (isNonExhaustive) flags) > 0)
                                      --Take best possible record for current identifier.
                                      then nonExhaustiveIdentifier xs
                                                                   (DL.map (DL.sortOn (\(_,b,_,_) -> b)) 
                                                                   (DL.map (DL.concat) 
                                                                   (DL.map (DL.map (complexTupleFst)) 
                                                                   (DL.map (complexComplexTupleSnd) finalcomparisonlist))))
                                      --Do not return a record for this identifier.
                                      else []
                                  --If customComparator finds best record for current
                                  --identifier, return it.
                                  else if (not (DL.null (customComparator finalcomparisonlist ys xs)))
                                      then customComparator finalcomparisonlist ys xs 
                                      --Else, return nothing.
                                      else [] 
    where
        --Local definitions.--
        --finalcomparisonlist -> To hold the list to be used in customComparator function.
        finalcomparisonlist = DL.zip (DL.map (DL.length) transposedcustomzippedvaluesfinal) 
                                     (DL.map (DL.map (\x -> (x,DL.length x))) 
                                     (DL.map (DL.groupBy (\(_,_,_,d) (_,_,_,h) -> d == h)) 
                                     (DL.map (DL.sortOn (\(_,_,_,d) -> d)) 
                                      transposedcustomzippedvaluesfinal)))
        --sortedtransposedcustomzippedvaluesfinal -> To hold sorted transposedcustomzippedvaluesfinal.
        --sortedtransposedcustomzippedvaluesfinal = DL.map (DL.sortOn (\(a,b,c,d) -> b)) transposedcustomzippedvaluesfinal
        --transposedcustomzippedvaluesfinal -> To hold transposed customzippedvaluesfinal.
        transposedcustomzippedvaluesfinal = DL.transpose customzippedvaluesfinal 
        --customzippedvaluesfinal -> To hold customzippedvalues with line numbers added.
        customzippedvaluesfinal = DL.map (\(a,b) -> DL.map (\(c,d,e) -> (a,c,d,e)) b) 
                                         (DL.zip [0..] customzippedvalues)
        --customzippedvalues -> To hold all values of current identifier.
        customzippedvalues = customZipWith3 xs ys
        ----------------------

--customComparator -> This function will
--compare values from each group of
--applicable records per identifier
--and decide the best record to choose.
customComparator :: [(Int,[([(Int,Int,String,String)],Int)])] -> [(Int,String,[String])] -> [[String]] -> [[String]] 
customComparator []     []    []    = []
customComparator []     []    (_:_) = []
customComparator []     (_:_) _     = []
customComparator (x:xs) ys    zs    = if --If the first element of this list is singular (wins comparison),
                                         --and its second element of the second element IS NOT a -1
                                         --(FLOAT or INT), then it wins.
                                         | (((DL.head (DL.map (DL.length) 
                                           (DL.groupBy (\(_,_,_,d) (_,_,_,h) -> d == h) 
                                           (DL.sortOn (\(_,b,_,_) -> b) 
                                           (DL.concat (DL.map (complexTupleFst) (complexComplexTupleSnd x))))))) == 1) && 
                                           (-1 `DL.notElem` 
                                           (DL.concat 
                                           ((DL.map ((DL.map (\(_,b,_,_) -> b))) 
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x))))))) ->
                                           [zs DL.!! 
                                           (quadrupletFst (DL.head (DL.sortOn (\(_,b,_,_) -> b) 
                                           (DL.concat (DL.map (complexTupleFst) (complexComplexTupleSnd x))))))]
                                         --If the first element of this list is singular (wins comparison),
                                         --and its second element of the second element IS a -1
                                         --(FLOAT or INT), then compare using proper comparator in ys.
                                         --If user-supplied operator is <,
                                         --and the comparison is of FLOAT,
                                         --and there are no duplicated comparison elements.        
                                         | (((DL.head (DL.map (DL.length) 
                                           (DL.groupBy (\(_,_,_,d) (_,_,_,h) -> d == h) 
                                           (DL.sortOn (\(_,b,_,_) -> b) 
                                           (DL.concat (DL.map (complexTupleFst) (complexComplexTupleSnd x))))))) == 1) &&
                                           (-1 `DL.elem` 
                                           (DL.concat 
                                           ((DL.map ((DL.map (\(_,b,_,_) -> b))) 
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x))))))     &&
                                           (("<" `DL.elem` 
                                           (tripletThd (singleunnest (DL.filter (\(_,b,_) -> 
                                           (DL.concat (DL.head (DL.map (DL.map (quadrupletThd)) 
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x))))) == b) ys)))) &&
                                           ("FLOAT" `DL.elem` 
                                           (tripletThd (singleunnest (DL.filter (\(_,b,_) -> 
                                           (DL.concat (DL.head (DL.map (DL.map (quadrupletThd)) 
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x))))) == b) ys)))) && 
                                           ((DL.length 
                                           (DL.nub (DL.concat (DL.map (DL.map (quadrupletFrh)) 
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x)))))) == 
                                           (DL.length (DL.concat (DL.map (DL.map (quadrupletFrh)) 
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x)))))))) ->
                                           --Grab the line that contains the minimum comparison element.
                                           [zs DL.!! 
                                           (quadrupletFst (DL.minimumBy (\(_,_,_,d) (_,_,_,h) -> compare 
                                           (read d :: Float) (read h :: Float)) 
                                           (DL.concat (DL.map (complexTupleFst) (complexComplexTupleSnd x)))))]
                                         --If the first element of this list is singular (wins comparison),
                                         --and its second element of the second element IS a -1
                                         --(FLOAT or INT), then compare using proper comparator in ys.
                                         --If user-supplied operator is <,
                                         --and the comparison is of INT,
                                         --and there are no duplicated comparison elements.
                                         | (((DL.head (DL.map (DL.length)
                                           (DL.groupBy (\(_,_,_,d) (_,_,_,h) -> d == h)
                                           (DL.sortOn (\(_,b,_,_) -> b)
                                           (DL.concat (DL.map (complexTupleFst) (complexComplexTupleSnd x))))))) == 1) &&
                                           (-1 `DL.elem`
                                           (DL.concat
                                           ((DL.map ((DL.map (\(_,b,_,_) -> b)))
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x))))))     &&
                                           (("<" `DL.elem` 
                                           (tripletThd (singleunnest (DL.filter (\(_,b,_) -> 
                                           (DL.concat (DL.head (DL.map (DL.map (quadrupletThd)) 
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x))))) == b) ys)))) &&
                                           ("INT" `DL.elem` 
                                           (tripletThd (singleunnest (DL.filter (\(_,b,_) -> 
                                           (DL.concat (DL.head (DL.map (DL.map (quadrupletThd)) 
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x))))) == b) ys)))) &&
                                           ((DL.length 
                                           (DL.nub (DL.concat (DL.map (DL.map (quadrupletFrh)) 
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x)))))) == 
                                           (DL.length (DL.concat (DL.map (DL.map (quadrupletFrh)) 
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x)))))))) ->
                                           --Grab the line that contains the minimum comparison element.
                                           [zs DL.!! 
                                           (quadrupletFst (DL.minimumBy (\(_,_,_,d) (_,_,_,h) -> compare 
                                           (read d :: Int) (read h :: Int)) 
                                           (DL.concat (DL.map (complexTupleFst) (complexComplexTupleSnd x)))))]
                                         --If the first element of this list is singular (wins comparison),
                                         --and its second element of the second element IS a -1
                                         --(FLOAT or INT), then compare using proper comparator in ys.
                                         --If user-supplied operator is >,
                                         --and the comparison is of FLOAT,
                                         --and there are no duplicated comparison elements.
                                         | (((DL.head (DL.map (DL.length)
                                           (DL.groupBy (\(_,_,_,d) (_,_,_,h) -> d == h)
                                           (DL.sortOn (\(_,b,_,_) -> b)
                                           (DL.concat (DL.map (complexTupleFst) (complexComplexTupleSnd x))))))) == 1) &&
                                           (-1 `DL.elem`
                                           (DL.concat
                                           ((DL.map ((DL.map (\(_,b,_,_) -> b)))
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x))))))     &&  
                                           ((">" `DL.elem` 
                                           (tripletThd (singleunnest (DL.filter (\(_,b,_) -> 
                                           (DL.concat (DL.head (DL.map (DL.map (quadrupletThd)) 
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x))))) == b) ys)))) &&
                                           ("FLOAT" `DL.elem` 
                                           (tripletThd (singleunnest (DL.filter (\(_,b,_) -> 
                                           (DL.concat (DL.head (DL.map (DL.map (quadrupletThd)) 
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x))))) == b) ys)))) &&
                                           ((DL.length 
                                           (DL.nub (DL.concat (DL.map (DL.map (quadrupletFrh)) 
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x)))))) == 
                                           (DL.length (DL.concat (DL.map (DL.map (quadrupletFrh)) 
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x)))))))) ->
                                           --Grab the line that contains the maximum comparison element.
                                           [zs DL.!! 
                                           (quadrupletFst (DL.maximumBy (\(_,_,_,d) (_,_,_,h) -> compare 
                                           (read d :: Float) (read h :: Float)) 
                                           (DL.concat (DL.map (complexTupleFst) (complexComplexTupleSnd x)))))]
                                         --If the first element of this list is singular (wins comparison),
                                         --and its second element of the second element IS a -1
                                         --(FLOAT or INT), then compare using proper comparator in ys.
                                         --If user-supplied operator is >,
                                         --and the comparison is of INT,
                                         --and there are no duplicated comparison elements.
                                         | (((DL.head (DL.map (DL.length)
                                           (DL.groupBy (\(_,_,_,d) (_,_,_,h) -> d == h)
                                           (DL.sortOn (\(_,b,_,_) -> b)
                                           (DL.concat (DL.map (complexTupleFst) (complexComplexTupleSnd x))))))) == 1) &&
                                           (-1 `DL.elem`
                                           (DL.concat
                                           ((DL.map ((DL.map (\(_,b,_,_) -> b)))
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x))))))     &&
                                           ((">" `DL.elem` 
                                           (tripletThd (singleunnest (DL.filter (\(_,b,_) -> 
                                           (DL.concat (DL.head (DL.map (DL.map (quadrupletThd)) 
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x))))) == b) ys)))) &&
                                           ("INT" `DL.elem` 
                                           (tripletThd (singleunnest (DL.filter (\(_,b,_) -> 
                                           (DL.concat (DL.head (DL.map (DL.map (quadrupletThd)) 
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x))))) == b) ys)))) &&
                                           ((DL.length (DL.nub (DL.concat (DL.map (DL.map (quadrupletFrh)) 
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x)))))) == 
                                           (DL.length 
                                           (DL.concat (DL.map (DL.map (quadrupletFrh)) 
                                           (DL.map (complexTupleFst) (complexComplexTupleSnd x)))))))) ->
                                           --Grab the line that contains the maximum comparison element.
                                           [zs DL.!! 
                                           (quadrupletFst (DL.maximumBy (\(_,_,_,d) (_,_,_,h) -> compare 
                                           (read d :: Int) (read h :: Int)) 
                                           (DL.concat (DL.map (complexTupleFst) (complexComplexTupleSnd x)))))]
                                         --Recurse.
                                         | otherwise -> customComparator xs ys zs 

--nonExhaustiveIdentifier -> This function will
--hold all of the identifiers in which there
--were in-exhaustive hierarchical fields supplied
--in order to elucidate a single sample.
--Take the first sample.
nonExhaustiveIdentifier :: [[String]] -> [[(Int,Int,String,String)]] -> [[String]]
nonExhaustiveIdentifier [] [] = []
nonExhaustiveIdentifier xs ys = [xs DL.!! (quadrupletFst (DL.head (DL.head ys)))] 

{----------------------------------}


{-Printing functions.-}

--tempFileCreation -> This function will
--print the file to stdout using
--readProcess of the unix tool cat.
catFile :: [[String]] -> IO ()
catFile [] = return ()
catFile xs = do
    --Open a temporary file.
    (tempfile,temph) <- SIOT.openTempFile "." "temp.txt"
    --Intercalate a tab, and then a newline into xs.
    let intercalatedxs = DL.intercalate "\n" (DL.map (DL.intercalate "\t") xs)
    --Add intercalatedxs to temp.txt.
    SIO.hPutStrLn temph intercalatedxs
    --Close the temporary file's handle.
    hClose temph
    --Print out the contents of tempfile to the screen using cat unix tool.
    (_,_,_,ph) <- SP.createProcess (SP.proc "cat" [tempfile])
    ec <- SP.waitForProcess ph
    case ec of
        SX.ExitSuccess   -> do _ <- SP.readProcess "rm" [tempfile] []
                               return ()
        SX.ExitFailure _ -> do _ <- error "Could not cat file."
                               _ <- SP.readProcess "rm" [tempfile] []
                               return ()

--printFile -> This function will
--print the file to either stdout
--or to a output file based on
--command-lines options provided.
printFile :: [Flag] -> [[String]] -> IO ()
printFile [] [] = return ()
printFile [] _  = return ()
printFile _  [] = return ()
printFile opts xs = do
    --Grab just "OUTFILE".
    let outfile = DL.head (DL.filter (isOutputFile) opts)
    --Extract the string from FilterFields.
    let outfilestring = extractOutputFile outfile
    --mapNotLast tabs and newlines in xs.
    let tabsandnewlinesadded = DL.map (mapNotLast (++ "\t")) xs
    --Write the output to the user-specified filename.
    SIO.writeFile (outfilestring) $
                  (TPB.render $
                  (TPB.hsep 0 TPB.left . DL.map (TPB.vcat TPB.left) . DL.map (DL.map (TPB.text)))
                  (DL.transpose tabsandnewlinesadded)) 

{---------------------}


{-RSC Specific Functions.-}

--processArgsAndFiles -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFiles :: ([Flag],[String]) -> IO ()
processArgsAndFiles ([],[]) = return () 
processArgsAndFiles (options,inputfiles) = do
    --Read in the file.
    readinputfile <- SIO.readFile (inputfiles DL.!! 2)
    --Apply lineFeed function to inputfile.
    let processedfile = lineFeedTabOnly readinputfile 
    --Apply transposition to group them correctly.
    let transposedfile = DL.transpose processedfile 
    --Perform hierarchical filtering on transposedfile.
    let hierarchicalfiltered = hierarchicalFiltering (inputfiles DL.!! 0) 
                                                     (inputfiles DL.!! 1) 
                                                     processedfile 
                                                     transposedfile
                                                     options 
    --Print the file to stdout (cat) or to a file.
    if DL.length (DL.filter (isOutputFile) options) > 0 
        then printFile options hierarchicalfiltered
        else catFile hierarchicalfiltered

{-------------------------}


{-Main function.-}

main :: IO ()
main = do
    --Get command line arguments.
    (args,files) <- SE.getArgs >>= compilerOpts
    --Run args and files through processArgsandFiles.
    processArgsAndFiles (args,files)

{----------------}
