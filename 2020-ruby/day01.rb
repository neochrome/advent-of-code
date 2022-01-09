describe "day01", :skip do
 
    example = [
      1721,
      979,
      366,
      299,
      675,
      1456,
    ]
  
    input = File.readlines("./day01.input").map &:to_i
    
    def part1(input)
      input.combination(2).select{|a,b|a+b==2020}.flatten.reduce &:*
    end
  
    def part2(input)
      input.combination(3).select{|a,b,c|a+b+c==2020}.flatten.reduce &:*
    end
  
    it "part1: example" do
      expect(part1(example)).to be 514579
    end
  
    it "part1: input" do
      puts "Part1: #{part1 input}"
    end
  
    it "part2: example" do
      expect(part2(example)).to be 241861950
    end
  
    it "part2: input" do
      puts "Part2: #{part2 input}"
    end
  
  end