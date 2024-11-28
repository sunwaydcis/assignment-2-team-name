import Text.CSV
import Data.List
import Data.Maybe 

data HospitalData = HospitalData {
    state :: String,
    beds :: Int,
    bedsCovid :: Int,
    admittedPUI :: Int,
    admittedCovid :: Int
} deriving (Show)

parseRow :: [String] -> Maybe HospitalData
parseRow row
    | length row < 13 = Nothing
    | otherwise = Just $ HospitalData {
        state = row !! 1,               
        beds = read (row !! 2),         
        bedsCovid = read (row !! 3),    
        admittedPUI = read (row !! 5),  
        admittedCovid = read (row !! 6)
    }

parseRecords :: CSV -> [HospitalData]
parseRecords rows = 
    map (fromMaybe (HospitalData "" 0 0 0 0) . parseRow) (tail rows)

highestBedsState :: [HospitalData] -> (String, Int)
highestBedsState records =
    let grouped = groupBy (\a b -> state a == state b) (sortOn state records)
        stateTotals = map (\grouping -> (state (head grouping), sum (map beds grouping))) grouped
    in maximumBy (\(_, beds1) (_, beds2) -> compare beds1 beds2) stateTotals

covidBedRatio :: [HospitalData] -> (Double, Int, Int)
covidBedRatio records =
    let totalBeds = sum (map beds records)
        covidBeds = sum (map bedsCovid records)
        ratio = if totalBeds == 0 then 0 else fromIntegral covidBeds / fromIntegral totalBeds
    in (ratio, totalBeds, covidBeds)

averageByState :: (HospitalData -> Int) -> [HospitalData] -> [(String, Double)]
averageByState field records =
    let grouped = groupBy (\a b -> state a == state b) (sortOn state records)
    in map (\grouping -> 
            let stateName = state (head grouping)
                total = sum (map field grouping)
                count = fromIntegral (length grouping)
            in (stateName, fromIntegral total / count)
           ) grouped

averagesByCategory :: [HospitalData] -> [(String, Double, Double)]
averagesByCategory records =
    let puiAverages = averageByState admittedPUI records
        covidAverages = averageByState admittedCovid records
    in zipWith (\(s1, avgPUI) (_, avgCovid) -> 
                    (s1, avgPUI, avgCovid)
                ) puiAverages covidAverages

validRecords :: [HospitalData] -> [HospitalData]
validRecords = filter (\rec -> not (null (state rec)))

main :: IO ()
main = do
    result <- parseCSVFromFile "hospital.csv"
    case result of
        Left err -> putStrLn $ "Error parsing CSV: " ++ show err
        Right rows -> do
            let records = validRecords (parseRecords rows)
            if null records
                then putStrLn "No valid records found!"
                else do
                    let (maxState, maxBeds) = highestBedsState records
                    putStrLn "Question 1: "
                    putStrLn ""
                    putStrLn $ "State with the highest total hospital beds: " ++ maxState
                    putStrLn $ maxState ++ " has a total of " ++ show maxBeds ++ " beds"

                    let (ratio, totalBeds, covidBeds) = covidBedRatio records
                    putStrLn ""
                    putStrLn "Question 2: "
                    putStrLn ""
                    putStrLn $ "Total number of hospital beds: " ++ show totalBeds
                    putStrLn $ "Total number of COVID-19 beds: " ++ show covidBeds
                    putStrLn $ "Ratio of COVID-19 beds to total hospital beds: " ++ show ratio

                    let averages = averagesByCategory records
                    putStrLn ""
                    putStrLn "Question 3: "
                    putStrLn ""
                    putStrLn "Averages of individuals being admitted to hospitals by category:"
                    mapM_ (\(st, avgPUI, avgCovid) -> do
                        putStrLn $ st ++ ": "
                        putStrLn $ "  Average suspected/probable: " ++ show avgPUI
                        putStrLn $ "  Average Covid-19 positive: " ++ show avgCovid
                        ) averages

