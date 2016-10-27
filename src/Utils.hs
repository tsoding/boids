module Utils (saltedRange) where


-- Get random value in range based on salt value between 0.0 and 1.0.
-- 0.0 mens minimal value.
-- 1.0 means maximum value.
-- Supposed to be used to get random value in specific range using item of
-- infinite random values (number between 0.0 and 1.0) list.
-- Usage examples:
--   saltedRange 0.2 (20, 30) = 22.0
--   saltedRange 1   (20, 30) = 30.0
saltedRange :: Float -> (Float, Float) -> Float
saltedRange salt (from, to) = salt * (to - from) + from
