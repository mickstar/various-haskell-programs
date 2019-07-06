import System.IO
import Data.Text as T

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

numToWords :: Int -> String
numToWords n 
    | n < 10 = singleDigit n
    | n < 100 = doubleDigit n
    | n < 1000 = hundredsToWords n
    | n < 1000000 = thousandsToWords n
    | n < 1000000000 = millionsToWords n
    | otherwise = billionsToWords n

singleDigit :: Int -> String
singleDigit a = case a of
    0 -> "Zero"
    1 -> "One"
    2 -> "Two"
    3 -> "Three"
    4 -> "Four"
    5 -> "Five"
    6 -> "Six"
    7 -> "Seven"
    8 -> "Eight"
    9 -> "Nine"

doubleDigit :: Int -> String
doubleDigit a
    | a < 20 = case a of
        10 -> "Ten"
        11 -> "Eleven"
        12 -> "Twelve"
        13 -> "Thirteen"
        14 -> "Fourteen"
        15 -> "Fifteen"
        16 -> "Sixteen"
        17 -> "Seventeen"
        18 -> "Eighteen"
        19 -> "Nineteen"
    | a < 100 = (doubleDigitPreface a) ++ " " ++ (
        case a `mod` 10 of
            0 -> ""
            n -> numToWords n )
    | otherwise = "error"

doubleDigitPreface :: Int -> String
doubleDigitPreface a
    = case a `quot` 10 of
        2 -> "Twenty"
        3 -> "Thirty"
        4 -> "Fourty"
        5 -> "Fifty"
        6 -> "Sixty"
        7 -> "Seventy"
        8 -> "Eighty"
        9 -> "Ninety"
        otherwise -> "error"

hundredsToWords :: Int -> String
hundredsToWords a = (numToWords (a `quot` 100)) ++ " Hundred " ++
    case a `mod` 100 of
        0 -> ""
        x -> "and " ++ numToWords x

thousandsToWords :: Int -> String
thousandsToWords a = (numToWords (a `quot` 1000)) ++ " Thousand " ++
    case a `mod` 1000 of
        0 -> ""
        x -> numToWords x

millionsToWords :: Int -> String
millionsToWords a = (numToWords (a `quot` 1000000)) ++ " Million " ++
    case a `mod` 1000000 of
        0 -> ""
        x -> numToWords x

billionsToWords :: Int -> String
billionsToWords a = (numToWords (a `quot` 1000000000)) ++ " Billion " ++
    case a `mod` 1000000000 of
        0 -> ""
        x -> numToWords x

main = do
    inputS <- prompt "Input a Number >> " 
    let n = read inputS :: Int
    putStrLn (T.unpack $ T.strip $ T.pack $ numToWords n)