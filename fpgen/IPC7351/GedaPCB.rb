require_relative 'Renderer'

module IPC7351
    class GedaPCB < Renderer
        def initialize
            super(".fp")
            @f = "% .4fmm"
        end

        def render(footprint, io, layers = ["copper", "silkscreen"])
            super(footprint, io, layers)
        end

        def render_path(footprint, layer, path)
            path.each_cons(2) do |pair|
                @io.puts %Q{\tElementLine[#{@f} #{@f} #{@f} #{@f} #{@f}]\n} % [pair[0].x, pair[0].y, pair[1].x, pair[1].y, path.lw]
            end
        end

        def render_circle(footprint, layer, circle)
            if circle.fill?
                @io.puts %Q{\tElementLine[#{@f} #{@f} #{@f} #{@f} #{@f}]\n} % [circle.c.x, circle.c.y, circle.c.x, circle.c.y, circle.r * 2]
            else
                @io.puts %Q{\tElementArc[#{@f} #{@f} #{@f} #{@f} %d %d #{@f}]\n} % [circle.c.x, circle.c.y, circle.r, circle.r, 0, 360, circle.lw]
            end
        end

        def render_pad(footprint, layer, pad)
            lw = [pad.l, pad.w].min
            x1 = pad.c.x - (pad.l - lw) / 2.0
            y1 = pad.c.y - (pad.w - lw) / 2.0
            x2 = pad.c.x + (pad.l - lw) / 2.0
            y2 = pad.c.y + (pad.w - lw) / 2.0
            clearance = footprint.settings["pads.clearance.pad"] * 2
            expansion = lw + footprint.settings["soldermask.expansion"] * 2
            @io.puts %Q{\tPad[#{@f} #{@f} #{@f} #{@f} #{@f} #{@f} #{@f} "" "%s" "square"]\n} % [x1, y1, x2, y2, lw, clearance, expansion, pad.name]
        end

        def render_layer(footprint, layer)
            @io.puts %Q{\t# %s\n} % [layer.name]
            super(footprint, layer)
        end

        def render_footprint(footprint, layers)
            @io.puts %Q{Element["" "%s" "%s1" "" 0.000mm 0.000mm 0.000mm 0.000mm 0 100 ""]\n} % [footprint.name, footprint.refdes]
            @io.puts %Q{(\n}
            super(footprint, layers)
            @io.puts %Q{)\n}
        end
    end
end

