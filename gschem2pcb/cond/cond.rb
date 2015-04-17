require 'optparse'
require_relative '../gEDA'
require_relative 'cond.tab'

$options = {}
$options[:defines] = []

OptionParser.new do |opts|
    opts.banner = "Usage: cond.rb [options]"

    opts.on("-D", "--define <variable>",
            "Add <variables> to the list of conditional variables") do |var|
        $options[:defines] << var
    end
end.parse!

parser = CondParser.new($options[:defines])

page = GEDA::Page.new("untitled_1.sch")
page.read!($stdin.read)
page.contents.select { |object| object.is_a? GEDA::Component}.each do |object|
    object.attribs.each do |attrib|
        if attrib.value =~ /^\[([^\]]*)\]\s*(.*)/
            pred = $1
            value = $2
            if parser.parse(pred)
#            if pred.split(/,\s*/).include? "14"
                target = object.attribs.find { |a| a.value == "*" }
                if target
                    target.value = value
                    attrib.detach!
                    page.remove! attrib
                else
                    attrib.value = value
                end
            else
                attrib.detach!
                page.remove! attrib
            end
        end
    end
end
$stdout.write(page.write)

