example = [
  "..##.......",
  "#...#...#..",
  ".#....#..#.",
  "..#.#...#.#",
  ".#...##..#.",
  "..#.##.....",
  ".#.#.#....#",
  ".#........#",
  "#.##...#...",
  "#...##....#",
  ".#..#...#.#",
]

class Array
  def tree_at?(x,y)
    self[y][x % self[y].size] == "#"
  end
  def traverse(sx, sy)
    (sy...self.size)
      .step(sy)
      .each_with_index.map{|y,i|
        x = (i + 1) * sx
        self[y][x % self[y].size]
      }
  end
end

raise "part1 example failed" unless example.traverse(3, 1).count("#") == 7

input = File.readlines("./day03.input").map(&:chomp)

puts "Part1: #{input.traverse(3, 1).count("#")}"

slopes = [
  [1,1],
  [3,1],
  [5,1],
  [7,1],
  [1,2],
]

raise "part2 examples failed" unless slopes.map{|sx,sy|example.traverse(sx,sy).count("#")}.reduce(&:*) == 336

puts "Part2: #{slopes.map{|sx,sy|input.traverse(sx,sy).count("#")}.reduce(&:*)}"
