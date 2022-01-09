example = [
  "Step C must be finished before step A can begin.",
  "Step C must be finished before step F can begin.",
  "Step A must be finished before step B can begin.",
  "Step A must be finished before step D can begin.",
  "Step B must be finished before step E can begin.",
  "Step D must be finished before step E can begin.",
  "Step F must be finished before step E can begin.",
]

def parse (l)
  dep,step = l.scan(/Step (\w) .* step (\w).*/)[0].captures.map(&.to_s)
  {step,dep}
end


# edges = example.map{|l|parse l}
edges = File.read_lines("./day07.input").map{|l| parse l}
graph = Hash(String,Array(String)).new
edges.each do |(step,dep)|
  graph[step] = graph.fetch(step, [] of String) << dep
end
vertices = edges.reduce(Set(String).new){|v,(step,dep)|v.add(step).add(dep)}
ins = vertices.reduce(Hash(String,Int32).new){|ins,v|ins[v]=0;ins}
graph.each do |step,deps|
  deps.each do |dep|
    ins[dep] += 1
  end
end
queue = [] of String
vertices.each do |v|
  graph[v] = graph.fetch(v, [] of String)
  queue.push v if ins[v] == 0
end
order = [] of String

while queue.any?
  u = queue.pop
  order << u
  graph[u].each do |dep|
    ins[dep]-=1
    queue.push dep if ins[dep] == 0
  end
end
puts order.reverse.join()
