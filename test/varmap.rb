require 'gEDA'

page = GEDA::Page.new("schematic.sch")
page.read!($stdin.read)
page.contents.select { |object| object.is_a? GEDA::Component}.each do |object|
    object.attribs.each do |attrib|
        if attrib.value =~ /^\[([^\]]*)\]\s*(.*)/
            pred = $1
            value = $2
            if pred.split(/,\s*/).include? "14"
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

