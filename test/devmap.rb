require 'gEDA'

Paths = ['../../../ps/lib/dev/']

def devmap(component, attrib, devmap)
    Paths.each do |path|
        if File.exists?("#{path}#{devmap}")
            f = File.new("#{path}#{devmap}")
            f.each_line do |line|
                if line =~ /([A-Za-z0-9_-]+)\s+(.*)/
                    cmd = $1
                    arg = $2
                    case cmd
                    when "attr"
                        newattr = GEDA::Text.new(attrib.anchor, :lower_left, 0, arg, 10, false, :both)
                        component.page.append! newattr
                        component.attach_attribs! newattr
                    when "delattr"
                        component.detach_attribs!
                    when "include"
                        devmap(component, attrib, "#{arg}")
                    end
                end
            end
        end
    end
end

page = GEDA::Page.new("schematic.sch")
page.read!($stdin.read)
page.contents.select { |object| object.is_a? GEDA::Component}.each do |component|
    component.attribs.select { |attrib| attrib.name == "devmap" }.each do |attrib|
        devmap(component, attrib, "#{attrib.value}")
    end
end
$stdout.write(page.write)

