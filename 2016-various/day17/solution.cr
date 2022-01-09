require "digest/md5"
require "spec"

class Room
  def initialize(@passcode : String)
  end

  private def is_open?(d)
    case d
      when 'b'..'f'
        return true
      else
        return false
    end
  end

  def open_doors(path)
    hash = Digest::MD5.hexdigest(@passcode + path)
    {
      "U" => is_open?(hash[0]),
      "D" => is_open?(hash[1]),
      "L" => is_open?(hash[2]),
      "R" => is_open?(hash[3]),
    }.select{|_,o|o}.map{|dir,_|dir}
  end
end

describe Room do
  it "has all locked doors" do
    Room.new("hijkl").open_doors("DR").should eq [] of String
  end
  it "only right open" do
    Room.new("hijkl").open_doors("DU").should eq ["R"]
  end
  it "up, left and right open" do
    Room.new("hijkl").open_doors("D").should eq ["U", "L", "R"]
  end
end

struct Position
  property x, y
  def initialize(@x = 1, @y = 1)
  end
  def at_vault?
    @x == 4 && @y == 4
  end
  def move(dir)
    case dir
      when "U"
        Position.new @x, @y - 1
      when "D"
        Position.new @x, @y + 1
      when "L"
        Position.new @x - 1, @y
      when "R"
        Position.new @x + 1, @y
      else
        raise "Invalid direction: #{dir}"
    end
  end
  def ok?
    @x > 0 && @x < 5 && @y > 0 && @y < 5
  end
end

class PathFinder
  def initialize(@room : Room)
    @initial_state = { pos: Position.new, path: "" }
  end

  def available_from(state)
    @room
      .open_doors(state[:path])
      .select{ |d| state[:pos].move(d).ok? }
      .map { |d| { pos: state[:pos].move(d), path: state[:path] + d } }
  end
end

module Shortest
  def find
    states = available_from @initial_state
    while states.any?
      state = states.shift
      return state[:path] if state[:pos].at_vault?
      states.concat available_from state
    end
    raise "no path found"
  end
end

class ShortestPath < PathFinder
  include Shortest
end

describe ShortestPath do
  it "works" do
    ShortestPath.new(Room.new("ihgpwlah")).find.should eq "DDRRRD"
  end
end

module Longest
  def find
    states = available_from @initial_state
    longest = ""
    while states.any?
      state = states.pop
      if state[:pos].at_vault?
        longest = state[:path] if state[:path].size > longest.size
      else
        states.concat available_from state
      end
    end
    longest
  end
end

class  LongestPath < PathFinder
  include Longest
end

describe LongestPath do
  it "works" do
    LongestPath.new(Room.new("ihgpwlah")).find.size.should eq 370
  end
end

room = Room.new "bwnlcvfs"
puts ""
puts "part1: #{ShortestPath.new(room).find}"
puts "part2: #{LongestPath.new(room).find.size}"
