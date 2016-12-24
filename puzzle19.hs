type Elf = Int

winner :: [Elf] -> Elf
winner (elf1:rest@(elf2:elf3:_)) | elf1 == elf3 = elf1
                                 | otherwise = winner rest

looped :: ([Elf] -> [Elf]) -> [Elf] -> [Elf]
looped fn seed = let result = fn (seed ++ result) in result

steal :: [Elf] -> [Elf]
steal (elf1:elf2:rest) = elf1:steal rest
