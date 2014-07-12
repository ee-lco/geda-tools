module IPC7351
    class GedaPCB < Renderer
        def initialize
            super(".fp")
            @f = "% .3fmm"
        end

        #def render(footprint, io, layers = ["copper", "silkscreen"])
        def render(footprint, io, layers = nil)
            super(footprint, io, layers)
        end

        def render_drawable(object)
            object.each_cons(2) do |pair|
                @io.puts %Q{\tElementLine[#{@f} #{@f} #{@f} #{@f} #{@f}]\n} % [pair[0].x, -pair[0].y, pair[1].x, -pair[1].y, object.lw]
            end
        end

        def render_pad(pad)
            lw = [pad.l, pad.w].min
            x1 = pad.c.x - (pad.l - lw) / 2.0
            y1 = pad.c.y - (pad.w - lw) / 2.0
            x2 = pad.c.x + (pad.l - lw) / 2.0
            y2 = pad.c.y + (pad.w - lw) / 2.0
            #@io.puts %Q{\tPad[#{@f} #{@f} #{@f} #{@f} #{@f} 0.250mm 1.500mm "" "%s" "square"]\n} % [x1, -y1, x2, -y2, lw, pad.name]
            @io.puts %Q{\tPad[#{@f} #{@f} #{@f} #{@f} #{@f} 0.250mm 1.500mm "" "%s" "square"] # cx=% .2f cy=% .2f l=% .2f w=% .2f\n} % [x1, -y1, x2, -y2, lw, pad.name, pad.c.x, pad.c.y, pad.l, pad.w]
        end

        def render_layer(layer)
            @io.puts %Q{\t# %s\n} % [layer.name]
            super(layer)
        end

        def render_footprint(footprint, layers)
            @io.puts %Q{Element["" "%s" "%s1" "" 0.000mm 0.000mm 0.000mm 0.000mm 0 100 ""]\n} % [footprint.name, footprint.refdes]
            @io.puts %Q{(\n}
            super(footprint, layers)
            @io.puts %Q{)\n}
        end
    end
end

