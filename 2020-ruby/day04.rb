example = <<-EOF
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
EOF
.split("\n")

example_invalid = <<-EOF
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
EOF
.split("\n")

example_valid = <<-EOF
pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
EOF
.split("\n")

def parse_records(lines)
  lines
  .chunk{|l|l.empty?}
  .filter_map{|sep,lines|lines.join(" ") unless sep}
  .map{|rec|rec.split(" ").map{|field|field.split(":")}.to_h}
  .to_a
end

class Hash
  Rules = {
    :byr => ->(byr) { (1920..2002).cover?(byr.to_i) },
    :iyr => ->(iyr) { (2010..2020).cover?(iyr.to_i) },
    :eyr => ->(eyr) { (2020..2030).cover?(eyr.to_i) },
    :hgt => ->(hgt) {
      case hgt
        when /(\d+)in/ then (59..76).cover?($~[1].to_i)
        when /(\d+)cm/ then (150..193).cover?($~[1].to_i)
        else false
      end
    },
    :hcl => ->(hcl) { hcl =~ /^#[0-9a-f]{6}$/ },
    :ecl => ->(ecl) { %w(amb blu brn gry grn hzl oth).include?(ecl) },
    :pid => ->(pid) { pid =~ /^\d{9}$/ },
  }
  def part1()
    Rules.keys.map(&:to_s).all?{|key|self.has_key? key}
  end
  def part2()
    self.part1 && Rules.all?{|field,rule|rule.call(self[field.to_s])}
  end
end

def process(lines, &validate)
  parse_records(lines).select(&validate).count
end

raise "part1 example failed" unless process(example, &:part1) == 2

input = File.readlines("./day04.input").map(&:chomp)

puts "Part1: #{process(input, &:part1)}"

raise "part2 example (invalid) failed" unless process(example_invalid, &:part2) == 0
raise "part2 example (valid) failed" unless process(example_valid, &:part2) == 4

puts "Part2: #{process(input, &:part2)}"
