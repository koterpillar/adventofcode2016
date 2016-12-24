type Elf = Int

winner :: [Int] -> Int
winner (elf1:rest@(elf2:elf3:_)) | elf1 == elf3 = elf1
                                 | otherwise = winner rest

looped :: ([Int] -> [Int]) -> [Int] -> [Int]
looped fn seed = let result = fn (seed ++ result) in result

steal :: [Int] -> [Int]
steal (elf1:elf2:rest) = elf1:steal rest
