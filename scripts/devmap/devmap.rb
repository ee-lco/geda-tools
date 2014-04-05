require 'gEDA'
require 'optparse'

$options = {}
$options[:paths] = []

OptionParser.new do |opts|
    opts.banner = "Usage: devmap.rb [options]"

    opts.on("-P", "--path <directory>",
            "Add <directory> to the devmap search path") do |dir|
        $options[:paths] << dir
    end
end.parse!

def devmap(component, attrib, devmap)
    $options[:paths].each do |path|
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
                        devmap(component, attrib, arg)
                    end
                end
            end
        end
    end
end

page = GEDA::Page.new("untitled_1.sch")
page.read!($stdin.read)
page.contents.select { |object| object.is_a? GEDA::Component}.each do |component|
    component.attribs.select { |attrib| attrib.name == "devmap" }.each do |attrib|
        devmap(component, attrib, attrib.value)
    end
end
$stdout.write(page.write)

