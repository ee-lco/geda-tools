require_relative 'Renderer'

module IPC7351
    class SVG < Renderer
        def initialize
            super(".svg")
        end

        def render(footprint, io, layers = nil)
            super(footprint, io, layers)
        end

        def render_polygon(poly)
            @io.puts %Q{\t\t<path d="M %s" style="fill:none;stroke-width:%.3f;stroke-linecap:round"/>} %
                [poly.map { |p| "%.3f,%.3f" % [p.x, p.y] }.join(" "), poly.lw]
        end

        def render_circle(circle)
            if circle.fill
                @io.puts %Q{\t\t<circle cx="%.3f" cy="%.3f" r="%.3f" style="stroke:none"/>} % [circle.c.x, circle.c.y, circle.r]
            else
                @io.puts %Q{\t\t<circle cx="%.3f" cy="%.3f" r="%.3f" style="fill:none;stroke-width:%.3f;s"/>} % [circle.c.x, circle.c.y, circle.r, circle.lw]
            end
        end

        def render_pad(pad)
            x = pad.c.x - pad.l / 2.0
            y = pad.c.y - pad.w / 2.0
            @io.puts %Q{\t\t<rect width="%.3f" height="%.3f" x="%.3f" y="%.3f" style="stroke:none"/>} % [pad.l, pad.w, x, y]
        end

        def render_layer(layer)
            @io.puts %Q{\t<g id="%s" style="stroke:%s;fill:%s">} % [layer.name, layer.color, layer.color]
            super(layer)
            @io.puts %Q{\t</g>}
        end

        def render_footprint(footprint, layers)
            @io.puts %Q{<?xml version="1.0" encoding="UTF-8"?>}
            #@io.puts %Q{<svg viewBox="-10 -10 20 20" preserveAspectRatio="xMidYMid" style="fill:#000000">}
            @io.puts %Q{<svg width="20cm" height="20cm" viewBox="-10 -10 20 20" preserveAspectRatio="xMidYMid" style="fill:#000000">}
            @io.puts %Q{\t<g id="background" style="fill:#000000"><rect width="20" height="20" x="-10" y="-10" style="stroke:none"/></g>}
            super(footprint, layers)
            @io.puts %Q{</svg>}
        end
    end
end

