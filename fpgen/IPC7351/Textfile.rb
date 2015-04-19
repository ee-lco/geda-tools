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

        def render_path(path)
            path.each_cons(2) do |pair|
                @io.puts %Q{\tLine: (#{@f}, #{@f}) - (#{@f}, #{@f})\n} % [pair[0].x, pair[0].y, pair[1].x, pair[1].y]
            end
        end

        def render_circle(circle)
            @io.puts %Q{\tCircle: (#{@f}, #{@f}) #{@f}\n} % [circle.c.x, circle.c.y, circle.r]
        end

        def render_pad(pad)
            @io.puts %Q{\tPad: (#{@f}, #{@f}) - (#{@f}, #{@f})\n} % [pad.c.x, pad.c.y, pad.l, pad.w]
        end

        def render_layer(layer)
            @io.puts %Q{\tLayer: %s\n} % [layer.name]
            super(layer)
        end

        def render_footprint(footprint, layers)
            @io.puts %Q{Element: %s - %s\n} % [footprint.refdes, footprint.name]
            @io.puts %Q{(\n}
            super(footprint, layers)
            @io.puts %Q{)\n}
        end
    end
end

