class Intcode
  property memory : Array(Int32)

  enum State
    Running
    Halted
  end

  def initialize (program : Array(Int32))
    @memory = program.clone
    @pc = 0
    @input = Channel(Int32).new
    @output = Channel(Int32).new
    @state = State::Running
  end

  def input (value)
    @input.send value
  end

  def output
    @output.receive
  end

  def is_running?
    @state == State::Running
  end

  def read_pos (idx)
    @memory[@memory[idx]]
  end

  def read_imm (idx)
    @memory[idx]
  end

  def write_pos (idx, value)
    @memory[idx] = value
  end

  def ins (dst : Proc(Int32,Int32,Int32), *args : Proc(Int32,Int32), &block)
    res = yield *(args.map_with_index{|a,i|a.call(@pc+1+i)})
    dst.call(@pc+args.size, res)
    2 + args.size
  end

  def ins (dst : Proc(Int32,Int32,Int32), &block)
    res = yield
    dst.call(@pc+1, res)
    2
  end

  def ins (*args : Proc(Int32,Int32), &block)
    yield *(args.map_with_index{|a,i|a.call(@pc+1+i)})
    1 + args.size
  end

  def ins_pc (*args : Proc(Int32,Int32), &block)
    yield *(args.map_with_index{|a,i|a.call(@pc+1+i)})
  end

  def step
    op_code = @memory[@pc] % 100
    readi = ->read_imm(Int32)
    readp = ->read_pos(Int32)
    write = ->write_pos(Int32,Int32)
    case op_code
      when 1 then @pc+=ins(write,readi,readi) {|a,b|a+b}
      when 2 then @pc+=ins(write,readi,readi) {|a,b|a*b}
      when 3 then @pc+=ins(write) {@input.receive}
      when 4 then @pc+=ins(readi) {|a|@output.send(a)}
      when 5 then @pc=ins_pc(readi,readi) {|a,b|a != 0 ? b : @pc+3}
      when 6 then @pc=ins_pc(readi,readi) {|a,b|a == 0 ? b : @pc+3}
      when 7 then @pc+=ins(write,readi,readi) {|a,b|a < b ? 1 : 0}
      when 8 then @pc+=ins(write,readi,readi) {|a,b|a == b ? 1 : 0}
      when 99 then @state = State::Halted
      else raise "unsupported op_code: #{op_code}"
    end
  end

end

class Amp
  def initialize (program, @phase : Int32 = phase)
    @machine = Intcode.new program
    spawn do
      @machine.input phase
    end
  end

  def run
    spawn do
      while @machine.is_running?
        puts "#{self} stepping"
        @machine.step
      end
    end
    self
  end

  def input (value)
    @machine.input value
  end

  def output
    @machine.output
  end

  def pipe (other : Amp)
    puts "#{self} >> #{other}"
    spawn do
      loop do
        other.input self.output
      end
    end
    self
  end

  def is_running?
    @machine.is_running?
  end

  def to_s
    "Amp(#{@phase})"
  end

  def to_s (io)
    io.print to_s
  end

end


class Circuit
  property amps : Array(Amp)
  def initialize (program, phases)
    @amps = phases.map{|phase|Amp.new(program, phase)}
    @amps.each_cons_pair{|o,i|i.pipe o}
  end

  def run
    @amps.each &.run
  end

  def input (value)
    @amps.first.input value
  end

  def output
    @amps.last.output
  end
end

prg = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
c = Circuit.new prg, [4,3,2,1,0]
spawn c.run
spawn do
  c.input 0
end
Fiber.yield
puts c.output
