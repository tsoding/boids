module Utils (projectToRange) where


-- Transforms value between 0.0 and 1.0 to value in specific range.
-- 0.0 means minimal value (from).
-- 1.0 means maximum value (to).
-- Usage examples:
--   projectToRange 0   (20, 30) = 20.0
--   projectToRange 0.2 (20, 30) = 22.0
--   projectToRange 1   (20, 30) = 30.0
projectToRange :: Float -> (Float, Float) -> Float
projectToRange salt (from, to) = salt * (to - from) + from
