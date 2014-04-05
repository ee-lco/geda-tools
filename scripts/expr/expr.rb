require 'gEDA'
require 'expr.tab'

parser = ExprParser.new

page = GEDA::Page.new("untitled_1.sch")
page.read!($stdin.read)
page.contents.select { |object| object.is_a? GEDA::Component}.each do |object|
    object.attribs.each do |attrib|
        attrib.value = parser.parse(object, attrib.value)
    end
end
$stdout.write(page.write)

