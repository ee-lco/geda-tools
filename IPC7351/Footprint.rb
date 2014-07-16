require 'forwardable'

module IPC7351
    class Footprint
        include Enumerable
        extend Forwardable
        def_delegators :@layers, :[], :empty?, :include?, :length, :member?, :size
        attr_reader :name, :filename, :refdes

        def initialize(spec, settings, env)
            @spec = Hash[*spec.map { |key, value| [key, MinMax.new(value)] }.flatten]

            @bl = @spec.include?("D1") ? @spec["D1"] : @spec["D"]
            @bw = @spec.include?("E1") ? @spec["E1"] : @spec["E"]

            @settings = settings
            @env      = env || select_env

            @pads   = Pads.new
            @layers = Array.new

            generate_text
            generate_pads
            generate_layers
        end

        def each
            @layers.each { |layer| yield layer }
        end

        def add_layer(name, elements, *rounding)
            rounding = [@settings["%s.rounding" % name]] if rounding.length == 0
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
                pads = @pads
                pads = pads.expand(@settings["%s.expansion" % layer]) if @settings.include?("%s.expansion")
                pads = pads.sort
                add_layer(layer, pads, *rounding)
            end

            return self
        end

        def courtyard
            lw       = @settings["courtyard.width"]
            clr      = @env["courtyard.excess"]
            rounding = @env["courtyard.rounding"]

            body = Path.rectangle(@bl["max"], @bw["max"])

            bounds = Geometry::Bounds.new(body.bounds, @pads.bounds).expand(clr)

            add_layer("courtyard", [Drawable.rectangle(bounds.min, bounds.max, lw)], rounding)

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

            add_layer("assembly", [Drawable.polygon(points, lw)])

            return self
        end

        def pads_by_side
            pads = {}
            Geometry.sides.each do |side|
                pads[side] = @pads.at_pos(side)
            end

            return pads
        end

        def get_dims(bl, bw, bclr, pads, pclr, lw)
            dims = {}
            Geometry.sides.each do |side|
                body = Geometry::Bounds.new(Path.rectangle(bl, bw)).
                                        expand(bclr).
                                        rotate_from(side)

                dims[side] = {}

                dims[side]["y1"] = pads[side].bounds.rotate_from(side).max.y if !pads[side].empty?
                y2 = [-body.max.y]
                y2.push(pads[side.prev].bounds.expand(pclr).rotate_from(side).min.y) if !pads[side.prev].empty?
                y2.push(pads[side.next].bounds.expand(pclr).rotate_from(side).min.y) if !pads[side.next].empty?
                dims[side]["y2"] = y2.min
                dims[side]["y3"] = pads[side].bounds.rotate_from(side).min.y + lw / 2.0 if !pads[side].empty?
            end

            return dims
        end

        def f_x10(f)
            return (f * 10).truncate
        end

        def f_x100(f)
            return (f * 100).truncate
        end
    end
end

