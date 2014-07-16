require 'forwardable'

module IPC7351
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
            return Pad.new(@num, @name, @pos, @c, @l, @w)
        end
    end

    class PadFactory
        def initialize(settings, env, l, w, t, t1 = nil)
            xyc = PadFactory.get_xyc(settings, env, l, w, t, t1)

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

            return srms
        end

        def self.get_minmax(settings, env, l, w, t)
            srms = PadFactory.get_srms(l, w, t)

            fillets  = env.select("fillet")
            tols     = settings.select("tolerance")

            zmax = l.min    + 2 * fillets["toe"]  + PadFactory.rms(l.tol,    *tols.values)
            gmin = srms.max - 2 * fillets["heel"] - PadFactory.rms(srms.tol, *tols.values)
            xmax = w.min    + 2 * fillets["side"] + PadFactory.rms(w.tol,    *tols.values)

            return { "zmax" => zmax, "gmin" => gmin, "xmax" => xmax }
        end

        def self.get_xyc(settings, env, l, w, t, t1 = nil)
            minmax = PadFactory.get_minmax(settings, env, l, w, t)

            zmax = minmax["zmax"]
            gmin = minmax["gmin"]
            xmax = minmax["xmax"]

            x = xmax
            y = (zmax - gmin) / 2.0
            c = (zmax + gmin) / 2.0
            if !t1.nil?
                y += t1.nom
                c -= t1.nom
            end

            rounding = settings.select("pads.rounding")
            x = Geometry::round(x, rounding["size"])
            y = Geometry::round(y, rounding["size"])
            c = Geometry::round(c, rounding["placement"]) / 2.0

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
end

