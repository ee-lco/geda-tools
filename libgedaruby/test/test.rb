require 'libgedaruby'

v1 = GEDA::Vector.new(12, 34)
puts("#{v1.x},#{v1.y}")
v2 = GEDA::Vector.new([12, 34])
puts("#{v2.x},#{v2.y}")
v3 = GEDA::Vector.new(v2)
puts("#{v3.x},#{v3.y}")

p1 = GEDA::Page.new("my-schem1.sch")
p2 = GEDA::Page.new("my-schem2.sch")
p3 = GEDA::Page.new("../../test/e361xa-1.sch")
p3.read!(File.read(p3.filename))

GEDA::Page.pages { |page| puts(page.filename) }
puts(GEDA::Page[0].filename)
#puts(toplevel[2].filename)
#puts(toplevel[toplevel[1].filename].filename)
p3.contents { |object|
    if (object.is_a?(GEDA::Text))
#        puts("#{object.string} @ (#{object.anchor.x},#{object.anchor.y})")
    end
    if (object.class == GEDA::Attrib)
#        puts("#{object.name}=#{object.value}")
    end
    if (object.class == GEDA::Component)
        puts("#{object.basename}")
    end
}

