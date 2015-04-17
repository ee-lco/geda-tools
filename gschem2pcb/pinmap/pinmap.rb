require 'libgedaruby'

def pinmap(component, oldpin, newpin)
    component.contents.select { |object| object.is_a? GEDA::Pin}.each do |pin|
        if pin.attribs.find { |attrib| (attrib.name == "pinnumber" || attrib.name == "pinlabel") && attrib.value == oldpin }
            attrib = pin.attribs.find { |attrib| attrib.name == "pinnumber" }
            if attrib
                attrib.value = newpin
                attrib.visible = true
                attrib.attrib_mode = :value
            end
        end
    end
end

page = GEDA::Page.new("untitled_1.sch")
page.read!($stdin.read)
page.contents.select { |object| object.is_a? GEDA::Component}.each do |component|
    component.attribs.select { |attrib| attrib.name == "pinmap" }.each do |attrib|
        if attrib.value =~ /^([^=]+)=(.*)$/
            oldpin=$1
            newpin=$2
            pinmap(component, oldpin, newpin)
        end
    end
end
$stdout.write(page.write)

