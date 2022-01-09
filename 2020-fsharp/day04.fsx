#load "./lib.fsx"
open Lib

let as_lines = String.split '\n'

let example = as_lines "
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
"

let example_valid = as_lines "
pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
"

let example_invalid = as_lines "
eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007
"

let rules = [
  fun strict -> function
  | ParseRegex "byr:(\d{4}) " [Integer byr] -> Some (1920 <= byr && byr <= 2002)
  | _ -> None

  fun strict -> function
  | ParseRegex "iyr:(\d{4}) " [Integer iyr] -> Some (2010 <= iyr && iyr <= 2020)
  | _ -> None

  fun strict -> function
  | ParseRegex "eyr:(\d{4}) " [Integer eyr] -> Some (2020 <= eyr && eyr <= 2030)
  | _ -> None

  fun strict -> function
  | ParseRegex "hgt:(\d+)(cm|in) " [Integer value; String unit] ->
    Some (
      (unit = "cm" && 150 <= value && value <= 193)
      || (unit = "in" && 59 <= value && value <= 76)
    )
  | _ -> None

  fun strict -> function
  | ParseRegex "hcl:#[0-9a-f]{6} " [] -> Some true
  | _ -> None

  fun strict -> function
  | ParseRegex "ecl:(?:amb|blu|brn|gry|grn|hzl|oth) " [] -> Some true
  | _ -> None

  fun strict -> function
  | ParseRegex "pid:\d{9} " [] -> Some true
  | _ -> None
]

let check strict record =
  let valid rule =
    match rule strict (record + " ") with
    | Some result -> result
    | None -> false
  List.forall valid rules

let non_empty = (<>) ""
let to_records = List.chunkBy non_empty >> List.map (String.concat " ")
let count p = List.filter p >> List.length
let part1 = check false
let part2 = check true

example
|> to_records
|> count part1
|> Assert.must_be 2

example_valid
|> to_records
|> count part2
|> Assert.must_be 4

example_invalid
|> to_records
|> count part2
|> Assert.must_be 0

open System.IO
File.ReadAllLines "./day04.input"
|> List.ofArray
|> to_records
|> count part2
|> printfn "Part1: %A"
