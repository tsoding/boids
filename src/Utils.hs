module Utils (dosedLists) where


-- dosedLists 2 [1..6] = [[1,2],[3,4],[5,6]]
-- dosedLists 3 [1..6] = [[1,2,3],[4,5,6]]
dosedLists :: (Num a) => Int -> [a] -> [[a]]
dosedLists doseSize list = dose list []
  where dose list result
          | length taken < doseSize = result
          | otherwise = taken : dose (drop doseSize list) result
            where taken = take doseSize list
