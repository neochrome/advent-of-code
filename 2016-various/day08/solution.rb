class LCD

  def initialize(width, height)
    @width = width
    @height = height
    @leds = Array.new(height) { Array.new(width, false) }
  end

  def rect(width, height)
    (0..height - 1).each { |row| (0..width - 1).each { |col| @leds[row][col] = true } }
  end

  def ror(row, steps)
    @leds[row].rotate!(-steps)
  end

  def roc(col, steps)
    Array
      .new(@height) { |row| @leds[row][col] }
      .rotate(-steps)
      .each.with_index { |cell, row| @leds[row][col] = cell }
  end
  
  def lit()
    @leds.flatten.count { |lit| lit }
  end

  def render()
    puts "┏#{"━" * @width}┓"
    @leds.each { |row| print "┃";row.each { |c| print c ? "█": " "};print "┃\n" }
    puts "┗#{"━" * @width}┛"
  end

end

require "scanf"

display = LCD.new(50, 6)
File.open("./input").each do |line|
  line.scanf("rect %dx%d") do |width, height| display.rect(width, height) end
  line.scanf("rotate column x=%d by %d") do |col, steps| display.roc(col, steps) end
  line.scanf("rotate row y=%d by %d") do |row, steps| display.ror(row, steps) end
end
puts "part1: #{display.lit()}"
display.render()
