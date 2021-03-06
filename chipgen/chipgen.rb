
class Pin
    attr_reader :dir, :num, :label, :type, :af

    def initialize(dir, num, *args)
        @dir, @num, @label, @type, @af = dir, num, args.shift, args.shift, args
        @af = [] if @af.nil?
    end

    def print(x, y)
        return if @num == "-"

        case @dir
        when "l"
            puts "P %d %d %d %d 1 0 0" % [x - 300, y, x, y]
            puts "{"
            puts "T %d %d 5 16 0 0 0 0 1" % [x - 200, y]
            puts "pintype=%s" % [@type]
            puts "T %d %d 5 16 0 0 0 0 1" % [x - 200, y]
            puts "pinseq=%s" % [@num]
            puts "T %d %d 5 16 %d %d 0 6 1" % [x - 100, y + 50, $sheet ? 0 : 1, $sheet ? 0 : 1]
            puts "pinnumber=%s" % [@num]
            puts "T %d %d 9 16 1 1 0 0 1" % [x +  50, y]
            puts "pinlabel=%s" % [@label]
            @af.each do |af|
                puts "T %d %d 9 16 %d 1 0 2 1" % [x + 50, y - 50, (af =~ /^\*/) ? 1 : 0]
                puts "af=%s" % [af.gsub(/^\*/, "")]
            end
            puts "}"
        when "r"
            puts "P %d %d %d %d 1 0 0" % [x + 300, y, x, y]
            puts "{"
            puts "T %d %d 5 16 0 0 0 6 1" % [x + 200, y]
            puts "pintype=%s" % [@type]
            puts "T %d %d 5 16 0 0 0 6 1" % [x + 200, y]
            puts "pinseq=%s" % [@num]
            puts "T %d %d 5 16 %d %d 0 0 1" % [x + 100, y + 50, $sheet ? 0 : 1, $sheet ? 0 : 1]
            puts "pinnumber=%s" % [@num]
            puts "T %d %d 9 16 1 1 0 6 1" % [x -  50, y]
            puts "pinlabel=%s" % [@label]
            @af.each do |af|
                puts "T %d %d 9 16 %d 1 0 8 1" % [x - 50, y - 50, (af =~ /^\*/) ? 1 : 0]
                puts "af=%s" % [af.gsub(/^\*/, "")]
            end
            puts "}"
        when "t"
            puts "P %d %d %d %d 1 0 0" % [x, y + 300, x, y]
            puts "{"
            puts "T %d %d 5 16 0 0 90 0 1" % [x, y + 200]
            puts "pintype=%s" % [@type]
            puts "T %d %d 5 16 0 0 90 0 1" % [x, y + 200, y]
            puts "pinseq=%s" % [@num]
            puts "T %d %d 5 16 %d %d 0 0 1" % [x + 100, y + 50, $sheet ? 0 : 1, $sheet ? 0 : 1]
            puts "pinnumber=%s" % [@num]
            puts "T %d %d 9 16 1 1 90 6 1" % [x, y - 50]
            puts "pinlabel=%s" % [@label]
            @af.each do |af|
                puts "T %d %d 9 16 %d 1 90 8 1" % [x + 50, y - 50, (af =~ /^\*/) ? 1 : 0]
                puts "af=%s" % [af.gsub(/^\*/, "")]
            end
            puts "}"
        when "b"
            puts "P %d %d %d %d 1 0 0" % [x, y - 300, x, y]
            puts "{"
            puts "T %d %d 5 16 0 0 90 0 1" % [x, y - 200]
            puts "pintype=%s" % [@type]
            puts "T %d %d 5 16 0 0 90 0 1" % [x, y - 200, y]
            puts "pinseq=%s" % [@num]
            puts "T %d %d 5 16 %d %d 0 2 1" % [x + 100, y - 50, $sheet ? 0 : 1, $sheet ? 0 : 1]
            puts "pinnumber=%s" % [@num]
            puts "T %d %d 9 16 1 1 90 0 1" % [x, y + 50]
            puts "pinlabel=%s" % [@label]
            @af.each do |af|ruby regex
                puts "T %d %d 9 16 %d 1 90 2 1" % [x + 50, y + 50, (af =~ /^\*/) ? 1 : 0]
                puts "af=%s" % [af.gsub(/^\*/, "")]
            end
            puts "}"
        end
    end
end

puts "v 20121203 2"

$sheet = false
pins = { "l" => [], "r" => [], "t" => [], "b" => [] }
$stdin.each_line do |line|
    line = line.chomp
    if line == "sheet=1" then
        $sheet = true
    else
        pin = Pin.new(*line.split(","))
        pins[pin.dir].push(pin)
    end
end
#pins.each { |dir| dir.sort! { |a,b| a.pinseq <=> b.pinseq } }
vlen = [pins["l"].length, pins["r"].length, 4].max
hlen = [pins["t"].length, pins["b"].length, 4].max
pins["l"].each_index { |i| pins["l"][i].print(1000, 1000 + (vlen - i) * 1000) }
pins["r"].each_index { |i| pins["r"][i].print(1000 + (hlen + 1) * 1000, 1000 + (vlen - i) * 1000) }
pins["t"].each_index { |i| pins["t"][i].print(2000 + i * 1000, 1000 + (vlen + 1) * 1000) }
pins["b"].each_index { |i| pins["b"][i].print(2000 + i * 1000, 1000) }

if $sheet then
    puts "T %d %d 8 32 0 0 0 4 1" % [1000 + (hlen + 1) * 500, 1500 + (vlen + 1) * 500]
    puts "device=none"
    puts "T 1000 %d 8 32 1 1 0 0 1" % [1100 + (vlen + 1) * 1000]
    puts "refdes=S?"
    puts "T 1000 900 8 16 1 1 0 2 1"
    puts "source=schematic.sch"
else
    puts "T %d %d 8 32 1 1 0 4 1" % [1000 + (hlen + 1) * 500, 1500 + (vlen + 1) * 500]
    puts "device=DEVICE"
    puts "T %d %d 8 32 1 1 0 4 1" % [1000 + (hlen + 1) * 500, 500 + (vlen + 1) * 500]
    puts "refdes=U?"
end
puts "B 1000 1000 %d %d 3 10 1 0 -1 -1 0 -1 -1 -1 -1 -1" % [(hlen + 1) * 1000, (vlen + 1) * 1000]

