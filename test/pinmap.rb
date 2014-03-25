require 'gEDA'

def pinmap(component, oldpin, newpin)
    component.contents.select { |object| object.is_a? GEDA::Pin}.each do |pin|
        pin.attribs.select { |attrib| attrib.name == "pinnumber" && attrib.value == oldpin }.each do |attrib|
            attrib.value=newpin
        end
    end
end

page = GEDA::Page.new("schematic.sch")
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

