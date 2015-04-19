require_relative 'Renderer'

module IPC7351
    class Textfile < Renderer
        def initialize
            super(".txt")
            @f = "% .4fmm"
        end

        def render(footprint, io, layers = nil)
            super(footprint, io, layers)
        end

        def render_path(footprint, layer, path)
            @io.print %Q{\tLine: }
            path.each { |p| @io.print %Q{ (#{@f}, #{@f})} % [p.x, p.y] }
            @io.puts
        end

        def render_circle(footprint, layer, circle)
            @io.puts %Q{\tCircle: (#{@f}, #{@f}) #{@f}\n} % [circle.c.x, circle.c.y, circle.r]
        end

        def render_pad(footprint, layer, pad)
            @io.puts %Q{\tPad: %d (#{@f}, #{@f}) - (#{@f}, #{@f})\n} % [pad.num, pad.c.x, pad.c.y, pad.l, pad.w]
        end

        def render_layer(footprint, layer)
            @io.puts %Q{\tLayer: %s\n} % [layer.name]
            super(footprint, layer)
        end

        def render_footprint(footprint, layers)
            @io.puts %Q{Element: %s - %s\n} % [footprint.refdes, footprint.name]
            @io.puts %Q{(\n}
            super(footprint, layers)
            @io.puts %Q{)\n}
        end
    end
end

