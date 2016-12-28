import Assembunny

demo :: Program
demo =
  parse $ ["cpy 2 a", "tgl a", "tgl a", "tgl a", "cpy 1 a", "dec a", "dec a"]
