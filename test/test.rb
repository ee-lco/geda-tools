require 'libgedaruby'

toplevel = GEDA::Toplevel.instance()
toplevel2 = GEDA::Toplevel.instance()
p1 = GEDA::Page.new("my-schem1.sch")
p2 = GEDA::Page.new("my-schem2.sch")

pages = toplevel.active_pages()
for i in 0...pages.length
    puts(pages[i].filename)
end

