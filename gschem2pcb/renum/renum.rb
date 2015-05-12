require 'libgedaruby'

delta = ARGV.first.to_i
refdes_re = /^(?<des>[[:upper:]]{1,3})(?<num>[[:digit:]]+)$/

page = GEDA::Page.new("untitled_1.sch")
page.read!($stdin.read)
page.contents.select { |object| object.is_a? GEDA::Component}.each do |component|
    component.attribs.select { |attrib| attrib.name == "refdes" }.each do |refdes|
        if m = refdes_re.match(refdes.value) then
            refdes.value = "%s%d" % [m[:des], m[:num].to_i + delta]
        end
    end
end
$stdout.write(page.write)

