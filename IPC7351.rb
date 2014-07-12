#encoding: UTF-8
require 'forwardable'
require 'matrix'

module IPC7351
    class Settings
        extend Forwardable
        def_delegators :@settings, :[], :each, :each_key, :each_pair, :each_value
        def_delegators :@settings, :has_key?, :include?, :key?, :member?
        def_delegators :@settings, :keys, :values

        def initialize(settings)
            @settings = settings
        end

        def to_s
            s = String.new
            @settings.keys.sort.each { |key| s += "%s=%s\n" % [key, @settings[key]] }
            return s
        end

        def select(name)
            name = "%s." % name
            return Settings.new(
                        Hash[*@settings.select { |key, value| key.start_with?(name) }.
                            map { |key, value| [key.sub(name, ""), value] }.flatten])
        end
    end

    class Environment < Settings
        def initialize(*args)
            case args.length
            when 2
                type = args[0]
                name = args[1]
                env  = Environments[name][type]
            when 6, 7
                env  = args
            else
                raise ArgumentError
            end
            jt, jh, js, cyx, cyr, silk_lw, silk_clr = *env
            silk_clr = silk_lw if silk_clr.nil?

            super({
                "fillet.toe"           => jt,
                "fillet.heel"          => jh,
                "fillet.side"          => js,
                "courtyard.excess"     => cyx,
                "courtyard.rounding"   => cyr,
                "silkscreen.width"     => silk_lw,
                "silkscreen.clearance" => silk_clr,
                })
        end
    end

    Environments = {
        "Rectangular End Cap Length < 1.6 mm" => {
            "L"   => [ 0.10,  0.00, -0.05, 0.12, 0.01, 0.10],
            "N"   => [ 0.20,  0.00,  0.00, 0.15, 0.01, 0.12],
            "M"   => [ 0.30,  0.00,  0.05, 0.20, 0.01, 0.15],
        },
        "Rectangular End Cap Length >= 1.6 mm" => {
            "L"   => [ 0.15,  0.00, -0.05, 0.12, 0.01, 0.10],
            "N"   => [ 0.35,  0.00,  0.00, 0.25, 0.01, 0.12],
            "M"   => [ 0.55,  0.00,  0.05, 0.50, 0.01, 0.15],
        },
        "Gull Wing Pitch <= 0.625 mm" => {
            "L"   => [ 0.15,  0.05, -0.04, 0.12, 0.01, 0.10],
            "N"   => [ 0.35,  0.15, -0.02, 0.25, 0.01, 0.12],
            "M"   => [ 0.55,  0.25,  0.01, 0.50, 0.01, 0.15],
        },
        "Gull Wing Pitch > 0.625 mm" => {
            "L"   => [ 0.15,  0.25,  0.01, 0.12, 0.01, 0.10],
            "N"   => [ 0.35,  0.35,  0.03, 0.25, 0.01, 0.12],
            "M"   => [ 0.55,  0.45,  0.05, 0.50, 0.01, 0.15],
        },
        "Outward L Lead Pitch <= 0.625 mm" => {
            "L"   => [ 0.15,  0.05, -0.04, 0.12, 0.01, 0.10],
            "N"   => [ 0.35,  0.15, -0.02, 0.25, 0.01, 0.12],
            "M"   => [ 0.55,  0.25,  0.01, 0.50, 0.01, 0.15],
        },
        "Outward L Lead Pitch > 0.625 mm" => {
            "L"   => [ 0.15,  0.05,  0.01, 0.12, 0.01, 0.10],
            "N"   => [ 0.35,  0.15,  0.03, 0.25, 0.01, 0.12],
            "M"   => [ 0.55,  0.25,  0.05, 0.50, 0.01, 0.15],
        },
        "Flat Lead" => {
            "L"   => [ 0.10,  0.00, -0.05, 0.12, 0.01, 0.10],
            "N"   => [ 0.20,  0.00,  0.00, 0.15, 0.01, 0.12],
            "M"   => [ 0.30,  0.00,  0.05, 0.20, 0.01, 0.15],
        },
    };

    class MinMax < Hash
        N = /([+-]?(?:\d+(?:\.\d*)?|\d*\.\d+))/

        def initialize(measurements)
            meas = measurements
            case meas
            when MinMax
                min = meas.min
                max = meas.max
            when String
                case meas
                when /#{N}\s*(?:~|±|\+\/?-)\s*#{N}/
                    # 2.00 ~ 0.20
                    nom  = $1.to_f
                    htol = $2.to_f.abs
                    min = nom - htol
                    max = nom + htol
                when /#{N}\s*-\s*#{N}\s*(?:\.\.)?\s*\+\s*#{N}/
                    # 2.00 -0.20..+0.20
                    nom  = $1.to_f
                    ntol = $2.to_f.abs
                    ptol = $3.to_f.abs
                    min = nom - ntol
                    max = nom + ptol
                when /#{N}\s*\.\.\s*#{N}/
                    # 1.80..2.20
                    min = $1.to_f
                    max = $2.to_f
                when /#{N}/
                    # 2.00
                    min = $1.to_f
                    max = $1.to_f
                else
                    raise ArgumentError
                end
            when Hash
                if meas.include?("min") && meas.include?("max")
                    min = meas["min"].to_f
                    max = meas["max"].to_f
                elsif meas.include?("nom") && meas.include?("tol")
                    min = meas["nom"].to_f - meas["tol"].to_f.abs / 2.0
                    max = meas["nom"].to_f + meas["tol"].to_f.abs / 2.0
                elsif meas.include?("nom") && meas.include?("-tol") && meas.include?("+tol")
                    min = meas["nom"].to_f - meas["-tol"].to_f.abs
                    max = meas["nom"].to_f + meas["+tol"].to_f.abs
                elsif meas.include?("min") && meas.include?("tol")
                    min = meas["min"].to_f
                    max = meas["min"].to_f + meas["tol"].to_f.abs
                elsif meas.include?("max") && meas.include?("tol")
                    min = meas["max"].to_f - meas["tol"].to_f.abs
                    max = meas["max"].to_f
                elsif meas.include?("min")
                    min = meas["min"].to_f
                    max = meas["min"].to_f
                elsif meas.include?("max")
                    min = meas["max"].to_f
                    max = meas["max"].to_f
                elsif meas.include?("nom")
                    min = meas["nom"].to_f
                    max = meas["nom"].to_f
                else
                    raise ArgumentError
                end
            else
                raise ArgumentError
            end

            self["min"] = min
            self["max"] = max
            self["nom"] = (min + max) / 2.0
            self["tol"] = max - min
        end

        def min
            return self["min"]
        end

        def max
            return self["max"]
        end

        def nom
            return (self["min"] + self["max"]) / 2.0
        end

        def tol
            return self["max"] - self["min"]
        end
    end

    module Transformable
        def translate(pos, x, y)
            return transform(pos.translation(x, y))
        end

        def rotate(from_pos, to_pos)
            return transform(from_pos.rotation_to(to_pos))
        end

        def rotate_to(to_pos)
            return rotate(Geometry::side("bottom"), to_pos)
        end

        def rotate_from(from_pos)
            return rotate(from_pos, Geometry.side("bottom"))
        end
    end

    module Geometry
        class Position
            attr_reader :name

            def initialize(name, translation)
                @name, @translation = name, translation
            end

            def ==(other)
                return to_s == other.to_s
            end

            def to_s
                return @name
            end

            def translation(dx = 1, dy = 1)
                return Matrix[[1, 0, @translation[0, 2] * dx], [0, 1, @translation[1, 2] * dy], [0, 0, 1]]
            end
        end

        Center = Position.new("center", Matrix[[0, 0, 0], [0, 0, 0], [0, 0, 1]])
        def center
            return Center
        end

        class PositionGroup < Array
        end

        class PositionGroupMember < Position
            attr_reader :prev, :next, :opposite

            def initialize(group, name, translation)
                super(name, translation)
                @group = group
                @group.push(self)
            end

            def rotation(rot_matrix, pos)
                rotation = Matrix.identity(3)
                while pos != self
                    rotation = rotation * rot_matrix
                    pos = pos.next
                end
                return rotation
            end

            def rotation_from(pos)
                rot_ccw = Matrix[[ 0, -1,  0], [ 1,  0,  0], [  0,  0,  1]]
                return rotation(rot_ccw, pos)
            end

            def rotation_to(pos)
                rot_cw = Matrix[[ 0,  1,  0], [-1,  0,  0], [  0,  0,  1]]
                return rotation(rot_cw, pos)
            end

            def index(name = @name)
                return @group.find_index { |pos| pos.name == name }
            end

            def prev
                return @group[(index - 1) % @group.length]
            end

            def next
                return @group[(index + 1) % @group.length]
            end

            def opposite
                return @group[(index + @group.length / 2) % @group.length]
            end

            def neighbors
                return [@prev, @next]
            end
        end

        Sides = PositionGroup.new
        class Side < PositionGroupMember
            attr_reader :per_axis, :par_axis

            def initialize(name, translation, per_axis, par_axis)
                super(Sides, name, translation)
                @per_axis, @par_axis = per_axis, par_axis
            end

            def self.instance(name)
                return Sides.find { |side| side.name == name }
            end
        end

        Side.new("bottom", Matrix[[ 1,  0,  0], [ 0,  1, -1], [ 0,  0,  1]], Matrix[[ 0, -1,  0]], Matrix[[ 1,  0,  0]])
        Side.new("right",  Matrix[[ 1,  0,  1], [ 0,  1,  0], [ 0,  0,  1]], Matrix[[ 1,  0,  0]], Matrix[[ 0, -1,  0]])
        Side.new("top",    Matrix[[ 1,  0,  0], [ 0,  1,  1], [ 0,  0,  1]], Matrix[[ 0,  1,  0]], Matrix[[-1,  0,  0]])
        Side.new("left",   Matrix[[ 1,  0, -1], [ 0,  1,  0], [ 0,  0,  1]], Matrix[[-1,  0,  0]], Matrix[[ 0,  1,  0]])
        def self.sides
            return Sides
        end
        def self.side(name)
            return Side.instance(name)
        end

        Corners = PositionGroup.new
        class Corner < PositionGroupMember
            attr_reader :sides

            def initialize(name, translation, side1, side2)
                super(Corners, name, translation)
                @sides = [side1, side2]
            end

            def self.instance(*args)
                case args.length
                when 1
                    name = args[0]
                    return Corners.find { |corner| corner.name == name }
                when 2
                    side1, side2 = *args
                    return Corners.find { |corner| corner.sides.include?(side1) && corner.sides.include?(side2) }
                end
            end
        end

        Corner.new("bottom-left",  Matrix[[ 1,  0, -1], [ 0,  1, -1], [ 0,  0,  1]], side("left"),   side("bottom"))
        Corner.new("bottom-right", Matrix[[ 1,  0,  1], [ 0,  1, -1], [ 0,  0,  1]], side("bottom"), side("right"))
        Corner.new("top-right",    Matrix[[ 1,  0,  1], [ 0,  1,  1], [ 0,  0,  1]], side("right"),  side("top"))
        Corner.new("top-left",     Matrix[[ 1,  0, -1], [ 0,  1,  1], [ 0,  0,  1]], side("top"),    side("left"))
        def self.corners
            return Corners
        end
        def self.corner(*args)
            return Corner.instance(*args)
        end

        class Bounds
            include Transformable
            attr_reader :min, :max

            def initialize(*args)
                x = []
                y = []
                args.flatten.each do |arg|
                    case arg
                    when Point
                        x.push(arg.x)
                        y.push(arg.y)
                    when Bounds
                        x.push(arg.min.x, arg.max.x)
                        y.push(arg.min.y, arg.max.y)
                    else
                        raise ArgumentError
                    end
                end
                @min = Point.new(x.min, y.min)
                @max = Point.new(x.max, y.max)
            end

            def expand(dx, dy = dx)
                return Bounds.new(Point.new(@min.x - dx, @min.y - dy), Point.new(@max.x + dx, @max.y + dy))
            end

            def transform(matrix)
                return Bounds.new(@min.transform(matrix), @max.transform(matrix))
            end
        end

        def self.round(val, prec)
            return (val / prec).round * prec
        end
    end

    class Point
        include Transformable
        attr_reader :x, :y

        def initialize(*args)
            case args.length
            when 1
                arg = args[0]
                case arg
                when Point
                    @x = arg.x
                    @y = arg.y
                when Matrix
                    @x = arg[0, 0]
                    @y = arg[1, 0]
                else
                    raise ArgumentError
                end
            when 2
                @x = args[0]
                @y = args[1]
            else
                raise ArgumentError
            end
        end

        def to_v
            return Matrix.column_vector([@x, @y, 1])
        end

        def to_m
            return Matrix.diagonal(@x, @y, 1)
        end

        def to_s
            return "(%f, %f)" % [@x, @y]
        end

        def transform(matrix)
            return Point.new(matrix * to_v)
        end

        def round(placement)
            return Point.new(Geometry.round(@x, placement),
                             Geometry.round(@y, placement))
        end
    end

    class Path < Array
        include Transformable

        def initialize(points, termination)
            push *points
            if termination == :closed
                push first
            end
        end

        def self.line(*args)
            case args.length
            when 2
                p1, p2 = *args
            when 4
                x1, y1, x2, y2 = *args
                p1 = Point.new(x1, y1)
                p2 = Point.new(x2, y2)
            else
                raise ArgumentError
            end

            return Path.new([p1, p2], :open)
        end

        def self.rectangle(*args)
            case args.length
            when 2
                case args[0]
                when Point
                    p1, p2 = *args
                when Numeric
                    l, w = *args
                    p1 = Point.new(-l / 2.0, -w / 2.0)
                    p2 = Point.new( l / 2.0,  w / 2.0)
                else
                    raise ArgumentError
                end
            when 3
                c, l, w = *args
                p1 = Point.new(c.x - l / 2.0, c.y - w / 2.0)
                p2 = Point.new(c.x + l / 2.0, c.y + w / 2.0)
            when 4
                raise ArgumentError
                p1, p2, p3, p4 = *args
                p2 = p3
            else
                raise ArgumentError
            end

            return Path.new([Point.new(p1.x, p1.y), Point.new(p2.x, p1.y),
                                 Point.new(p2.x, p2.y), Point.new(p1.x, p2.y)],
                                :closed)
        end

        def self.polygon(points)
            return Path.new(points, :closed)
        end

        def transform(matrix)
            return Path.new(map { |point| point.transform(matrix) }, :open)
        end

        def bounds
            return Geometry::Bounds.new(*self)
        end

        def round(placement)
            return Path.new(map { |point| point.round(placement) }, :open)
        end
    end

    class Drawable < Path
        attr_reader :lw

        def initialize(points, termination, lw)
            super(points, termination)
            @lw = lw
        end

        def self.line(*args)
            lw = args.pop
            return Drawable.new(Path.line(*args), :open, lw)
        end

        def self.rectangle(*args)
            lw = args.pop
            return Drawable.new(Path.rectangle(*args), :open, lw)
        end

        def self.polygon(*args)
            lw = args.pop
            return Drawable.new(Path.polygon(*args), :open, lw)
        end

        def transform(matrix)
            return Drawable.new(map { |point| point.transform(matrix) }, :open, @lw)
            ## @todo why doesn't this work?
            #return Drawable.new(super.transform(matrix), :open, @lw)
        end

        def round(placement)
            return Drawable.new(map { |point| point.round(placement) }, :open, @lw)
        end
    end

    class Pad
        attr_reader :num, :name, :pos, :c, :l, :w

        def initialize(num, name, pos, c, l, w)
            @num, @name, @pos, @c, @l, @w = num, name, pos, c, l, w
        end
        
        def expand(dx, dy = dx)
            return Pad.new(@num, @name, @pos, @c, @l + dx * 2, @w + dy * 2)
        end

        def bounds
             return Path.rectangle(@c, @l, @w).bounds
        end

        def round(placement, size)
            return Pad.new(@num, @name, @pas,
                           Point.new(Geometry.round(@c.x, placement),
                                     Geometry.round(@c.y, placement)),
                           Geometry.round(@l, size),
                           Geometry.round(@w, size))
        end
    end

    class PadFactory
        def initialize(env, tols, l, w, t, t1 = nil)
            xyc = PadFactory.get_xyc(env, tols, l, w, t, t1)

            @l = xyc["x"]
            @w = xyc["y"]
            @c = xyc["c"]
        end

        def self.rms(*vals)
            return Math.sqrt(vals.inject(0) {|sum, val| sum + val * val})
        end

        def self.get_srms(l, w, t)
            s    = MinMax.new({"min" => l.min - 2 * t.max, "max" => l.max - 2 * t.min})
            srms = MinMax.new({"nom" => s.nom, "tol" => PadFactory.rms(l.tol, t.tol, t.tol)})
$stderr.puts s
$stderr.puts srms
            return srms
        end

        def self.get_minmax(env, tols, l, w, t)
            srms = PadFactory.get_srms(l, w, t)

            fillets = env.select("fillet")

            zmax = l.min    + 2 * fillets["toe"]  + PadFactory.rms(l.tol,    *tols.values)
            gmin = srms.max - 2 * fillets["heel"] - PadFactory.rms(srms.tol, *tols.values)
            xmax = w.min    + 2 * fillets["side"] + PadFactory.rms(w.tol,    *tols.values)
$stderr.puts zmax
$stderr.puts gmin
$stderr.puts xmax

            return { "zmax" => zmax, "gmin" => gmin, "xmax" => xmax }
        end

        def self.get_xyc(env, tols, l, w, t, t1 = nil)
            minmax = PadFactory.get_minmax(env, tols, l, w, t)

            zmax = minmax["zmax"]
            gmin = minmax["gmin"]
            xmax = minmax["xmax"]

            x = xmax
            y = (zmax - gmin) / 2.0
            c = (zmax + gmin) / 4.0
            if !t1.nil?
                y += t1.nom
                c -= t1.nom / 2.0
            end
$stderr.puts x
$stderr.puts y
$stderr.puts c

            return { "x" => x, "y" => y, "c" => c }
        end

        def pads(first, count, pitch, pos, names = nil)
            pads = Pads.new

            names = *(first...first+count).map { |name| name.to_s } if names.nil?

            num = first
            for i in 0...count
                c   = Point.new((i - (count - 1) / 2.0) * pitch, -@c).rotate_to(pos)
                dim = Point.new(@l, @w).rotate_to(pos)
                l   = dim.x.abs
                w   = dim.y.abs

                unless names[i].nil?
                    pads.add(Pad.new(num, names[i], pos, c, l, w))
                    num += 1
                end
            end
            return pads
        end

        def pad(num, name, pos)
            return Pads.new.add(Pad.new(num, name, pos, @c, @l, @w))
        end
    end

    class Pads
        include Enumerable
        extend Forwardable
        def_delegators :@pads, :[], :empty?, :include?, :length, :member?, :size

        def initialize
            @pads = Hash.new
        end

        def each
            sort.each { |pad| yield pad }
        end

        def add(pads)
            case pads
            when Pad
                pads = [pads]
            when Pads
                pads = pads.entries
            when Array
            else
                raise ArgumentError
            end
            pads.each { |pad| @pads[pad.num] = pad.clone }

            return self
        end

        def sort
            return @pads.values.sort { |pad1, pad2| pad1.num <=> pad2.num }
        end

        def at_pos(pos)
            return Pads.new.add(@pads.values.select { |pad| pad.pos == pos })
        end

        def expand(dx, dy = dx)
            return Pads.new.add(@pads.values.map { |pad| pad.expand(dx, dy) })
        end

        def bounds
            return Geometry::Bounds.new(@pads.values.map { |pad| pad.bounds })
        end

        def round(placement, size)
            return Pads.new.add(@pads.values.map { |pad| pad.round(placement, size) })
        end
    end

    class Layer
        include Enumerable
        extend Forwardable
        def_delegators :@elements, :[], :empty?, :include?, :length, :member?, :size
        attr_reader :name, :color

        def initialize(name, color, elements, *rounding)
            @name = name
            @color = color
            @elements = elements.map { |elt| elt.clone }
        end

        def each
            @elements.each { |elt| yield elt }
        end
    end

    class Footprint
        include Enumerable
        extend Forwardable
        def_delegators :@layers, :[], :empty?, :include?, :length, :member?, :size
        attr_reader :name, :refdes, :desc

        def initialize(spec, settings, env)
            @spec = Hash[*spec.map { |key, value| [key, MinMax.new(value)] }.flatten]

            @bl = @spec.include?("D1") ? @spec["D1"] : @spec["D"]
            @bw = @spec.include?("E1") ? @spec["E1"] : @spec["E"]

            @settings = settings
            @env      = env || select_env
            @tols     = @settings.select("tolerance")

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

    class Renderer
        def initialize(extension)
            @extension = extension
        end

        def render(footprint, io, layers)
            @io = io
            render_footprint(footprint, layers)
        end

        def save(*footprints)
            footprints.each { |fp| File.open(fp.name + @extension, "w") { |file| render(fp, file) } }
        end
 
        def render_element(elt)
            case elt
            when Drawable
                render_drawable(elt)
            when Pad
                render_pad(elt)
            else
                raise ArgumentError
            end
        end

        def render_layer(layer)
            layer.each { |elt| render_element(elt) }
        end

        def render_footprint(footprint, layers)
            layers = footprint.collect { |layer| layer.name } if layers.nil?
            footprint.select { |layer| layers.include?(layer.name) }.each { |layer| render_layer(layer) }
        end
    end
end

