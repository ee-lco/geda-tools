require 'forwardable'
require_relative 'Layer'

module IPC7351
    class Footprint
        include Enumerable
        extend Forwardable
        def_delegators :@layers, :[], :empty?, :include?, :length, :member?, :size
        attr_reader :name, :filename, :refdes, :settings

        def initialize(spec, settings, env)
            @spec = Hash[*spec.map { |key, value| [key, MinMax.new(value)] }.flatten]

            @bl = @spec.include?("D1") ? @spec["D1"] : @spec["D"]
            @bw = @spec.include?("E1") ? @spec["E1"] : @spec["E"]

            @settings = settings
            @settings = @settings.merge(env || select_env)

            @pads   = PadGroup.new
            @layers = Array.new

            generate_text
            generate_pads
            generate_layers
        end

        def each
            @layers.each { |layer| yield layer }
        end

        def add_layer(name, elements, *rounding)
            rounding = [@settings["%s.rounding" % name]] if rounding.empty?
            layer = Layer.new(name, @settings["%s.color" % name], elements.map { |elt| elt.round(*rounding) })
            @layers.push(layer)
            return layer
        end

        def generate_text
            @name     = "%s (%s)" % [@can_name, @ipc_name]
            @filename = @ipc_name
        end

        def generate_layers
            courtyard
            assembly
            silkscreen
            copper

            return self
        end

        def copper
            rounding = [@settings["pads.rounding.placement"], @settings["pads.rounding.size"]]
            ["soldermask", "stencil", "copper"].each do |layer|
                expansion = "%s.expansion" % [layer]
                pads = @pads
                pads = pads.expand(@settings[expansion]) if @settings.include?(expansion)
                pads = pads.sort
                add_layer(layer, pads, *rounding)
            end

            return self
        end

        def courtyard
            lw       = @settings["courtyard.width"]
            clr      = @settings["courtyard.excess"]

            body = Shape.rectangle(@bl["max"], @bw["max"])

            bounds = Bounds.new(body.bounds, @pads.bounds).expand(clr)

            add_layer("courtyard", [Shape.rectangle(bounds.min, bounds.max, lw)])

            return self
        end

        def assembly
            max_mark_size_abs = 1.27
            max_mark_size_rel = 1 / 3.0

            lw      = @settings["assembly.width"]
            outline = @settings["assembly.body"]

            bl = @bl[outline]
            bw = @bw[outline]

            if @mark
                mark_size = [[bl, bw].min * max_mark_size_rel, max_mark_size_abs].min
                mark_pos  = Geometry.corner(@pads[1].pos, @pads[1].pos.prev)
            end

            points = []
            Geometry.corners.each do |corner|
                point = Point.new(0, 0).translate(corner, bl / 2.0, bw / 2.0)
                if (corner == mark_pos)
                    corner.sides.reverse.each do |side|
                        points.push point.translate(side.opposite, mark_size, mark_size)
                    end
                else
                    points.push point
                end
            end

            add_layer("assembly", [Shape.polygon(*points, lw)])

            return self
        end

        def f_x10(f)
            return (f * 10).truncate
        end

        def f_x100(f)
            return (f * 100).truncate
        end
    end
end

