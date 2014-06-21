require 'stringio'

class MinMax
    attr_reader :min, :max

    N = /([+-]?(?:\d+(?:\.\d*)?|\d*\.\d+))/

    def initialize(measurements)
        meas = measurements
        case meas
        when MinMax
            @min = meas.min
            @max = meas.max
        when String
            case meas
            when /#{N}\s*(?:~|±|\+\/?-)\s*#{N}/
                # 2.00 ~ 0.20
                nom = $1.to_f
                tol = $2.to_f.abs
                @min = nom - tol
                @max = nom + tol
            when /#{N}\s*-\s*#{N}\s*(?:\.\.)?\s*\+\s*#{N}/
                # 2.00 -0.20..+0.20
                nom = $1.to_f
                ntol = $2.to_f.abs
                ptol = $3.to_f.abs
                @min = nom - ntol
                @max = nom + ptol
            when /#{N}\s*\.\.\s*#{N}/
                # 1.80..2.20
                @min = $1.to_f
                @max = $2.to_f
            when /#{N}/
                # 2.00
                @min = $1.to_f
                @max = $1.to_f
            else
                raise ArgumentError
            end
        when Hash
            if meas.include?("min") && meas.include?("max")
                @min = meas["min"].to_f
                @max = meas["max"].to_f
            elsif meas.include?("min") && meas.include?("tol")
                @min = meas["min"].to_f
                @max = meas["min"].to_f + meas["tol"].to_f.abs
            elsif meas.include?("nom") && meas.include?("tol")
                @min = meas["nom"].to_f - meas["tol"].to_f.abs / 2
                @max = meas["nom"].to_f + meas["tol"].to_f.abs / 2
            elsif meas.include?("nom") && meas.include?("-tol") && meas.include?("+tol")
                @min = meas["nom"].to_f - meas["-tol"].to_f.abs
                @max = meas["nom"].to_f + meas["+tol"].to_f.abs
            elsif meas.include?("min")
                @min = meas["min"].to_f
                @max = meas["min"].to_f
            elsif meas.include?("max")
                @min = meas["max"].to_f
                @max = meas["max"].to_f
            elsif meas.include?("nom")
                @min = meas["nom"].to_f
                @max = meas["nom"].to_f
            else
                raise ArgumentError
            end
        else
            raise ArgumentError
        end
    end

    def nom
        return (@min + @max) / 2
    end

    def tol
        return @max - @min
    end
end

module IPC7351
    class Tolerances
        attr_reader :f, :p
        alias :fabrication :f
        alias :placement   :p

        def initialize(f, p)
            @f = f
            @p = p
        end
    end

    class Environment
        attr_reader :jt, :jh, :js, :cyx, :cyr
        alias :toe_fillet         :jt
        alias :heel_fillet        :jh
        alias :side_fillet        :js
        alias :courtyard_excess   :cyx
        alias :courtyard_rounding :cyr

        def initialize(*args)
            case args.length
            when 2
                @jt, @jh, @js, @cyx, @cyr = Environments[args[0]][args[1]]
            when 5
                @jt, @jh, @js, @cyx, @cyr = args
            else
                raise ArgumentError
            end
        end
    end

    Environments = {
        "Rectangular End Cap Length < 1.6 mm" => {
            "L"   => [ 0.10,  0.00, -0.05, 0.12, 0.01],
            "N"   => [ 0.20,  0.00,  0.00, 0.15, 0.01],
            "M"   => [ 0.30,  0.00,  0.05, 0.20, 0.01],
        },
        "Rectangular End Cap Length >= 1.6 mm" => {
            "L"   => [ 0.15,  0.00, -0.05, 0.10, 0.01],
            "N"   => [ 0.35,  0.00,  0.00, 0.25, 0.01],
            "M"   => [ 0.55,  0.00,  0.05, 0.50, 0.01],
        },
        "Gull Wing Pitch <= 0.625 mm" => {
            "L"   => [ 0.15,  0.05, -0.04, 0.12, 0.01],
            "N"   => [ 0.35,  0.15, -0.02, 0.25, 0.01],
            "M"   => [ 0.55,  0.25,  0.01, 0.50, 0.01],
        },
        "Gull Wing Pitch > 0.625 mm" => {
            "L"   => [ 0.15,  0.25,  0.01, 0.12, 0.01],
            "N"   => [ 0.35,  0.35,  0.03, 0.25, 0.01],
            "M"   => [ 0.55,  0.45,  0.05, 0.50, 0.01],
        },
        "Flat Lead" => {
            "L"   => [ 0.10,  0.00, -0.05, 0.12, 0.01],
            "N"   => [ 0.20,  0.00,  0.00, 0.15, 0.01],
            "M"   => [ 0.30,  0.00,  0.05, 0.20, 0.01],
        },
    };
end

class Calculator
    # l         L       pin toe-to-toe distance
    # s         S       pin heel-to-heel distance
    # z         Z       pad toe-toe distance
    # zmax      Zmax
    # g         G       pad heel-to-heel distance
    # gmin      Gmin
    #           E       pin pitch
    # w         W       pin width
    # @y        Y       pad length
    # @x        X       pad height
    #           M       pad/pin overlap
    #           N       pad/adjacent pin clearance
    # t         T       pin length
    # tols.f    F       fabrication tolerance
    # tols.p    P       placement tolerance
    # l.tol     Cl      toe-to-toe pin tolerance
    # srms.tol  Cs      heel-to-heel pin tolerance
    # w.tol     Cw      pin width tolerance
    # env.jt    Jt      toe fillet
    # env.jh    Jh      heel fillet
    # env.js    Js      side fillet
    #           K       standoff height (trim pads protruding under the body if K is small)
    #           H       component height
    #           A       component width
    #           B       component depth
    #           V1      courtyard width
    #           V2      courtyard depth
    #           R1      silkscreen width
    #           R2      silkscreen depth
    attr_reader :name

    def initialize(l, w, t, env, tols)
        l     = MinMax.new(l)
        w     = MinMax.new(w)
        t     = MinMax.new(t)

        s     = MinMax.new({"min" => l.min - 2 * t.max, "max" => l.max - 2 * t.min})
        srms  = MinMax.new({"nom" => s.nom, "tol" => rms(l.tol, t.tol, t.tol)})

        @zmax = l.min    + 2 * env.jt + rms(l.tol,    tols.f, tols.p)
        @gmin = srms.max - 2 * env.jh - rms(srms.tol, tols.f, tols.p)
        @xmax = w.min    + 2 * env.js + rms(w.tol,    tols.f, tols.p)
    end

    def rms(*vals)
        return Math.sqrt(vals.inject(0) {|sum, val| sum + val * val})
    end

    def round(val, prec)
        return (val / prec).round * prec
    end
end

#
# Terminal
#   Settings
#       Fab Tol (+/-)               0.05
#       Place Tol (+/-)             0.025
#       Pad place round-off         0.01
#       Pad size round-off          0.01
#       Mask X (+/-)                0.00
#       Mask Y (+/-)                0.00
#       Stencil X (+/-)             0.00
#       Stencil Y (+/-)             0.00
#   Environment
#       Most
#           Courtyard excess        0.50
#           Courtyard place round   0.01
#           Toe                     0.55
#           Heel                    0.00
#           Side                    0.05
#      Nominal 
#           Courtyard excess        0.25
#           Courtyard place round   0.01
#           Toe                     0.35
#           Heel                    0.00
#           Side                    0.00
#       Most
#           Courtyard excess        0.12
#           Courtyard place round   0.01
#           Toe                     0.15
#           Heel                    0.00
#           Side                   -0.05
#   Rules
#       Min. space pad-to-pad           0.15
#       Min. space pad-to-thermal tab   0.20
#       Min gang solder mask web        0.08
#       Gang solder mask shape          Contour (Block)
#       Minimum trim standoff height    0.00
#       Corner size (% of pad width)    20
#       Corner size limit               0.25
#       Corner size round               0.01
#       Thermal tab mask (+/-)          0.00
#       Thermal tab stencil (%)         50
#   Drafting
#       Silkscreen outline width                0.10
#       Silkscreen clearance                    0.10
#       Map silkscreen outline to body          Max (Nom, Min)
#       Silkscreen outline placement round-off  0.01
#       Silkscreen ref.des height               1.00
#       Silkscreen ref.des width (% of height)  10
#       Assembly outline width                  0.12
#       Map assembly outline to body            Max (Nom, Min)
#       Assembly ref.des height min             0.50
#       Assembly ref.des height max             2.00
#       Assembly ref.des width (% of height)    10
#       Courtyard outline width                 0.05
#       Include origin marker                   Yes/No
#       3D Model outline width                  0.001
#   Footprint
#       Dimensions
#           Use manufacuter's recommended dims  Yes/No
#               S (=Gmin), L (=Y), W (=X), L1 (=Y1)
#       Calculation
#           L, S, T (Z, S, Y)
#           Zref, Gref, Yref

class Chip < Calculator
    def initialize(type, dims, env, tols)
        @bl   = MinMax.new(dims["D"])
        @bw   = MinMax.new(dims["E"])
        @bh   = MinMax.new(dims["A"])
        @ll   = MinMax.new(dims["L"])
        @env  = IPC7351::Environment.new(@bl.nom < 1.6 ? "Rectangular End Cap Length < 1.6 mm" : "Rectangular End Cap Length >= 1.6 mm", env)
        @tols = tols
        super(@bl, @bw, @ll, @env, @tols)

        types = {
            "CAP" => { "pins" => ["1", "2"], "refdes" => "C", "polarized" => false },
            "DIO" => { "pins" => ["C", "A"], "refdes" => "D", "polarized" => true },
            "IND" => { "pins" => ["1", "2"], "refdes" => "L", "polarized" => false },
            "RES" => { "pins" => ["1", "2"], "refdes" => "R", "polarized" => false },
            "FUS" => { "pins" => ["1", "2"], "refdes" => "F", "polarized" => false },
        }

        @name      = "%sC%02.0f%02.0fX%.0f%s" % [type, (@bl.nom * 10).truncate, (@bw.nom * 10).truncate, (@bh.max * 100).truncate, env]
        @pins      = types[type]["pins"]
        @refdes    = types[type]["refdes"]
        @polarized = types[type]["polarized"]
        @x         = @xmax
        @y         = (@zmax - @gmin) / 2.0
        @c         = (@zmax + @gmin) / 4.0
        @v1        = @zmax + 2 * @env.cyx
        @v2        = @xmax + 2 * @env.cyx
    end

    def generate(gen, active_layers = [])
        part = gen::Part.new(gen, "", "", @refdes)
        part["copper"] << gen::Pad.new(gen, @pins[0], -@c, 0, @y, @x)
        part["copper"] << gen::Pad.new(gen, @pins[1],  @c, 0, @y, @x)
        # Silk screen
        if (@polarized)
            part["silkscreen"] << gen::Line.new(gen, -@c - @y / 2.0 + 0.05, -@x / 2.0 - 0.1, @c - @y / 2.0, -@x / 2.0 - 0.1, 0.25)
            part["silkscreen"] << gen::Line.new(gen, -@c - @y / 2.0 + 0.05,  @x / 2.0 + 0.1, @c - @y / 2.0,  @x / 2.0 + 0.1, 0.25)
        else
            part["silkscreen"] << gen::Line.new(gen, -@c + @y / 2.0 + 0.05, -@x / 2.0 - 0.1, @c - @y / 2.0, -@x / 2.0 - 0.1, 0.25)
            part["silkscreen"] << gen::Line.new(gen, -@c + @y / 2.0 + 0.05,  @x / 2.0 + 0.1, @c - @y / 2.0,  @x / 2.0 + 0.1, 0.25)
        end
        # Assembly Outline
        part["assembly"] << gen::Rectangle.new(gen, 0, 0, @bl.max, @bw.max, 0.1)
        # Courtyard
        part["courtyard"] << gen::Rectangle.new(gen, 0, 0, @v1, @v2, 0.025)
        return part.generate(active_layers)
    end
end

class GullWing < Calculator
    def initialize(type, dims, pins, env, tols)
        @bl   = MinMax.new(dims["D"])
        @pw   = MinMax.new(dims["E"])
        @bw   = MinMax.new(dims["E1"])
        @bh   = MinMax.new(dims["A"])
        @be   = MinMax.new(dims["A1"])
        @ll   = MinMax.new(dims["L"])
        @lw   = MinMax.new(dims["b"])
        @lp   = MinMax.new(dims["e"])
        @pins = pins
        @env  = IPC7351::Environment.new(@lp.nom <= 0.625 ? "Gull Wing Pitch <= 0.625 mm" : "Gull Wing Pitch > 0.625 mm", env)
        @tols = tols
        super(@pw, @lw, @ll, @env, @tols)

        @name   = "%s%.0fP%02.0fX%.0f-%d%s" % [type, (@lp.nom * 100).truncate, (@pw.nom * 100).truncate, (@bh.max * 100).truncate, pins, env]
        @refdes = "U";
        @x      = @xmax
        @y      = (@zmax - @gmin) / 2.0
        @c      = (@zmax + @gmin) / 4.0
        @v1     = @zmax + 2 * @env.cyx
        @v2     = @bl.max + 2 * @env.cyx
    end

    def generate(gen, active_layers)
        part = gen::Part.new(gen, "", "", @refdes)
        case @pins
        when 3
            part["copper"] << gen::Pad.new(gen, "1", -@lp.nom,  @c, @x, @y)
            part["copper"] << gen::Pad.new(gen, "2",  @lp.nom,  @c, @x, @y)
            part["copper"] << gen::Pad.new(gen, "3",  0,       -@c, @x, @y)
        when 5
            for i in 0...3
                part["copper"] << gen::Pad.new(gen, "#{i + 1}",             (-1 + i) * @lp.nom,  @c, @x, @y)
                case i
                when 0
                    part["copper"] << gen::Pad.new(gen, "5", (-1 + i) * @lp.nom, -@c, @x, @y)
                when 1
                when 2
                    part["copper"] << gen::Pad.new(gen, "4", (-1 + i) * @lp.nom, -@c, @x, @y)
                end
            end
        else
            for i in 0...(@pins / 2)
                part["copper"] << gen::Pad.new(gen, "#{i + 1}",             (-(@pins / 2 - 1) / 2.0 + i) * @lp.nom,  @c, @x, @y)
                part["copper"] << gen::Pad.new(gen, "#{@pins - i}", (-(@pins / 2 - 1) / 2.0 + i) * @lp.nom, -@c, @x, @y)
            end
        end
        # Silk screen
        part["silkscreen"] << gen::Line.new(gen, -@bl.max / 2.0, -@bw.max / 2.0, -@bl.max / 2.0, @zmax / 2.0, 0.25)
        part["silkscreen"] << gen::Line.new(gen,  @bl.max / 2.0, -@bw.max / 2.0,  @bl.max / 2.0, @bw.max / 2.0, 0.25)
        # Assembly Outline
        part["assembly"] << gen::AssemblyOutline.new(gen, 0, 0, @bl.max, @bw.max, 0.1)
        # Courtyard
        part["courtyard"] << gen::Courtyard.new(gen, 0, 0, @v2, @v1)
        return part.generate(active_layers)
    end
end

class FlatLead < Calculator
    def initialize(type, dims, pins, env, tols)
        @bl   = MinMax.new(dims["D"])
        @pw   = MinMax.new(dims["E"])
        @bw   = MinMax.new(dims["E1"])
        @bh   = MinMax.new(dims["A"])
        @ll   = MinMax.new(dims["L"])
        @lw   = MinMax.new(dims["b"])
        @lp   = MinMax.new(dims["e"])
        @pins = pins
        @env  = IPC7351::Environment.new("Flat Lead", env)
        @tols = tols
        super(@pw, @lw, @ll, @env, @tols)

        if pins == 2
            @name = "%s%.0fP%02.0f%02.0fX%.0f%s" % [type, (@lp.nom * 100).truncate, (@bw.nom * 10).truncate, (@bh.max * 100).truncate, env]
        else
            @name = "%s%.0fP%02.0fX%.0f-%d%s" % [type, (@lp.nom * 100).truncate, (@pw.nom * 100).truncate, (@bh.max * 100).truncate, pins, env]
        end
        @refdes = "U";
        @x      = @xmax
        @y      = (@zmax - @gmin) / 2.0
        @c      = (@zmax + @gmin) / 4.0
        @v1     = @zmax + 2 * @env.cyx
        @v2     = @bl.max + 2 * @env.cyx
    end

    def generate(gen, active_layers)
        part = gen::Part.new(gen, "", "", @refdes)
        case @pins
        when 3
            part["copper"] << gen::Pad.new(gen, "1", -@lp.nom,  @c, @x, @y)
            part["copper"] << gen::Pad.new(gen, "2",  @lp.nom,  @c, @x, @y)
            part["copper"] << gen::Pad.new(gen, "3",  0,       -@c, @x, @y)
        when 5
            for i in 0...3
                part["copper"] << gen::Pad.new(gen, "#{i + 1}",             (-1 + i) * @lp.nom,  @c, @x, @y)
                case i
                when 0
                    part["copper"] << gen::Pad.new(gen, "5", (-1 + i) * @lp.nom, -@c, @x, @y)
                when 1
                when 2
                    part["copper"] << gen::Pad.new(gen, "4", (-1 + i) * @lp.nom, -@c, @x, @y)
                end
            end
        else
            for i in 0...(@pins / 2)
                part["copper"] << gen::Pad.new(gen, "#{i + 1}",             (-(@pins / 2 - 1) / 2.0 + i) * @lp.nom,  @c, @x, @y)
                part["copper"] << gen::Pad.new(gen, "#{@pins - i}", (-(@pins / 2 - 1) / 2.0 + i) * @lp.nom, -@c, @x, @y)
            end
        end
        # Silk screen
        part["silkscreen"] << gen::Line.new(gen, -@bl.max / 2.0, -@bw.max / 2.0, -@bl.max / 2.0, @zmax / 2.0, 0.25)
        part["silkscreen"] << gen::Line.new(gen,  @bl.max / 2.0, -@c + @y / 2.0,  @bl.max / 2.0, @c - @y / 2.0, 0.25)
        # Assembly Outline
        part["assembly"] << gen::AssemblyOutline.new(gen, 0, 0, @bl.max, @bw.max, 0.1)
        # Courtyard
        part["courtyard"] << gen::Courtyard.new(gen, 0, 0, @v2, @v1)
        return part.generate(active_layers)
    end
end

class FlatLeadChip < Calculator
    def initialize(name, type, dims, env, tols)
        @pl   = MinMax.new(dims["D"])
        @bl   = MinMax.new(dims["D1"])
        @bw   = MinMax.new(dims["E"])
        @bh   = MinMax.new(dims["A"])
        @ll   = MinMax.new(dims["L"])
        @lw   = MinMax.new(dims["b"])
        @env  = IPC7351::Environment.new("Flat Lead", env)
        @tols = tols
        super(@pl, @lw, @ll, @env, @tols)

        types = {
            "CAP" => { "pins" => ["1", "2"], "refdes" => "C", "polarized" => false },
            "DIO" => { "pins" => ["C", "A"], "refdes" => "D", "polarized" => true },
            "IND" => { "pins" => ["1", "2"], "refdes" => "L", "polarized" => false },
            "RES" => { "pins" => ["1", "2"], "refdes" => "R", "polarized" => false },
            "FUS" => { "pins" => ["1", "2"], "refdes" => "F", "polarized" => false },
        }

        @name      = "%s%02.0f%02.0fX%.0f%s" % [name, (@pl.nom * 10).truncate, (@bw.nom * 10).truncate, (@bh.max * 100).truncate, env]
        @pins      = types[type]["pins"]
        @refdes    = types[type]["refdes"]
        @polarized = types[type]["polarized"]
        @x         = @xmax
        @y         = (@zmax - @gmin) / 2.0
        @c         = (@zmax + @gmin) / 4.0
        @v1        = @zmax + 2 * @env.cyx
        @v2        = @xmax + 2 * @env.cyx
    end

    def generate(gen, active_layers = [])
        part = gen::Part.new(gen, "", "", @refdes)
        part["copper"] << gen::Pad.new(gen, @pins[0], -@c, 0, @y, @x)
        part["copper"] << gen::Pad.new(gen, @pins[1],  @c, 0, @y, @x)
        # Silk screen
        #part["silkscreen"] << gen::Line.new(gen, -@bl.max / 2.0, -@bw.max / 2.0,   @bl.max / 2.0, -@bw.max / 2.0,  0.25)
        #part["silkscreen"] << gen::Line.new(gen, -@bl.max / 2.0,  @bw.max / 2.0,   @bl.max / 2.0,  @bw.max / 2.0,  0.25)
        #part["silkscreen"] << gen::Line.new(gen, -@bl.max / 2.0, -@bw.max / 2.0,  -@bl.max / 2.0, -@x / 2.0 - 0.1, 0.25)
        #part["silkscreen"] << gen::Line.new(gen,  @bl.max / 2.0, -@bw.max / 2.0,   @bl.max / 2.0, -@x / 2.0 - 0.1, 0.25)
        #part["silkscreen"] << gen::Line.new(gen, -@bl.max / 2.0,  @bw.max / 2.0,  -@bl.max / 2.0,  @x / 2.0 + 0.1, 0.25)
        #part["silkscreen"] << gen::Line.new(gen,  @bl.max / 2.0,  @bw.max / 2.0,   @bl.max / 2.0,  @x / 2.0 + 0.1, 0.25)
        #part["silkscreen"] << gen::Line.new(gen, -@pl.max / 2.0, -@x / 2.0 - 0.1, -@bl.max / 2.0, -@x / 2.0 - 0.1, 0.25)
        #part["silkscreen"] << gen::Line.new(gen, -@pl.max / 2.0,  @x / 2.0 + 0.1, -@bl.max / 2.0,  @x / 2.0 + 0.1, 0.25)
        if @polarized
            part["silkscreen"] << gen::Line.new(gen, -@c - @y / 2.0 + 0.05, -@bw.max / 2.0, @bl.max / 2.0, -@bw.max / 2.0, 0.25)
            part["silkscreen"] << gen::Line.new(gen, -@c - @y / 2.0 + 0.05,  @bw.max / 2.0, @bl.max / 2.0,  @bw.max / 2.0, 0.25)
        else
            part["silkscreen"] << gen::Line.new(gen, -@bl.max / 2.0, -@bw.max / 2.0, @bl.max / 2.0, -@bw.max / 2.0, 0.25)
            part["silkscreen"] << gen::Line.new(gen, -@bl.max / 2.0,  @bw.max / 2.0, @bl.max / 2.0,  @bw.max / 2.0, 0.25)
        end
        # Assembly Outline
        part["assembly"] << gen::Rectangle.new(gen, 0, 0, @bl.max, @bw.max, 0.1)
        # Courtyard
        part["courtyard"] << gen::Rectangle.new(gen, 0, 0, @v1, @v2, 0.025)
        return part.generate(active_layers)
    end
end

module Generator
    class Part
        def initialize(gen, name, description, refdes)
            @gen = gen
            @name = name
            @description = description
            @refdes = refdes
            @layers = []
            @layer_names = []
            add_layer("copper")
            add_layer("solder-mask")
            add_layer("stencil")
            add_layer("silkscreen")
            add_layer("assembly")
            add_layer("courtyard")
            add_layer("outline")
        end

        def add_layer(name)
            @layers.push([])
            @layer_names.push(name)
        end

        def [](idx)
            case idx
            when Integer
                return @layers[idx]
            when String
                return @layers[@layer_names.index(idx)]
            else
                raise ArgumentError
            end
        end

        def []=(idx, val)
            case idx
            when Integer
                @layers[idx] = val
            when String
                @layers[@layer_names.index(idx)] = val
            else
                raise ArgumentError
            end
        end

        def generate(active_layers = nil)
            part = []
            part.push(header)
            @layers.each_index do |idx|
                layer      = @layers[idx]
                layer_name = @layer_names[idx]

                if active_layers.nil? || active_layers.include?(layer_name)
                    part.push(layer_header(layer_name))
                    part.push(layer.map { |object| object.generate })
                    part.push(layer_footer(layer_name))
                end
            end
            part.push(footer)

            return part.flatten.inject("") { |str, elt| str + elt }
        end
    end

    class Element
        def initialize(gen)
            @gen = gen
        end
    end

    class Pad < Element
        def initialize(gen, name, cx, cy, width, height)
            super(gen)
            @name = name
            @w    = [width, height].min
            @x1   = cx - (width - @w) / 2.0
            @x2   = cx + (width - @w) / 2.0
            @y1   = cy - (height - @w) / 2.0
            @y2   = cy + (height - @w) / 2.0
        end
    end

    class Line < Element
        def initialize(gen, x1, y1, x2, y2, w)
            super(gen)
            @x1 = x1
            @y1 = y1
            @x2 = x2
            @y2 = y2
            @w = w
        end
    end

    class Poly < Element
        def initialize(gen, points, w)
            super(gen)
            @points = points
            @w = w
        end

        def generate
            return [
                @points.each_cons(2).map { |line| @gen::Line.new(@gen, line[0].x, line[0].y, line[1].x, line[1].y, @w).generate },
                @gen::Line.new(@gen, @points.last.x, @points.last.y, @points.first.x, @points.first.y, @w).generate,
            ]
        end
    end

    class Rectangle < Poly
        def initialize(gen, cx, cy, width, height, w)
            x1 = cx - width / 2.0
            x2 = cx + width / 2.0
            y1 = cy - height / 2.0
            y2 = cy + height / 2.0

            points = [
                gen::Point.new(x1, y1),
                gen::Point.new(x2, y1),
                gen::Point.new(x2, y2),
                gen::Point.new(x1, y2),
            ]

            super(gen, points, w)
        end
    end

    class Arc < Element
        def initialize(gen, cx, cy, width, height, start_angle, delta_angle, w)
            super(gen)
            @cx          = cx
            @cy          = cy
            @width       = width
            @height      = height
            @start_angle = start_angle
            @delta_angle = delta_angle
            @w           = w
        end
    end

    class Circle < Element
        def initialize(gen, cx, cy, radius, w)
            super(gen)
            @cx     = cx
            @cy     = cy
            @radius = radius
            @w      = w
        end

        def generate
            return @gen::Arc.new(@gen, @cx, @cy, @radius / 2.0, @radius / 2.0, 0, 360, @w).generate
        end
    end

    class AssemblyOutline < Poly
        def initialize(gen, cx, cy, width, height, w)
            x1 = cx - width / 2.0
            x2 = cx + width / 2.0
            y1 = cy - height / 2.0
            y2 = cy + height / 2.0

            points = [
                gen::Point.new(x1, y1),
                gen::Point.new(x2, y1),
                gen::Point.new(x2, y2),
                gen::Point.new(x1 + 1.27, y2),
                gen::Point.new(x1, y2 - 1.27),
            ]
                
            super(gen, points, w)
        end
    end

    class Courtyard < Element
        def initialize(gen, cx, cy, width, height)
            super(gen)

            @cx     = cx
            @cy     = cy
            @width  = width
            @height = height
        end

        def generate
            return [
                @gen::Circle.new(@gen, @cx, @cy, 0.5, 0.01).generate,
                @gen::Line.new(@gen, @cx, @cy - 0.4, @cx, @cy + 0.4, 0.01).generate,
                @gen::Line.new(@gen, @cx - 0.4, @cy, @cx + 0.4, @cy, 0.01).generate,
                @gen::Rectangle.new(@gen, @cx, @cy, @width, @height, 0.025).generate,
            ]
        end
    end

    class Point
        attr_reader :x, :y

        def initialize(x, y)
            @x = x
            @y = y
        end
    end
end

module PCB
    include Generator

    class Part < Generator::Part
        def header
            return [
                %Q{Element["" "" "%s1" "" 0.000mm 0.000mm 0.000mm 0.000mm 0 100 ""]\n} % [@refdes],
                %Q{(\n},
            ]
        end

        def footer 
            return %Q{)\n}
        end

        def layer_header(name)
            return %Q{\t# #{name}\n}
        end

        def layer_footer(name)
            return %Q{}
        end
    end

    class Pad < Generator::Pad
        def generate
            f = "% .3fmm"
            return %Q{\tPad[#{f} #{f} #{f} #{f} #{f} 0.250mm 1.500mm "" "%s" "square"]\n} % [@x1, @y1, @x2, @y2, @w, @name]
        end
    end

    class Line < Generator::Line
        def generate
            f = "% .3fmm"
            return %Q{\tElementLine[#{f} #{f} #{f} #{f} #{f}]\n} % [@x1, @y1, @x2, @y2, @w]
        end
    end

    class Arc < Generator::Arc
        def generate
            f = "% .3fmm"
            return %Q{\tElementArc[#{f} #{f} #{f} #{f} % .3f % .3f #{f}]\n} % [@cx, @cy, @width, @height, @start_angle, @delta_angle, @w]
        end
    end
end

def save(part)
    puts part.name
    File.open(part.name + ".fp", "w") { |file| file.puts(part.generate(PCB, ["copper", "solder-mask", "silkscreen"])) }
    #File.open(part.name + ".fp", "w") { |file| file.puts(part.generate(PCB, ["copper", "solder-mask", "silkscreen", "assembly"])) }
end

tol = IPC7351::Tolerances.new(0.1, 0.05);
[
    Chip.new("CAP", { "D" =>  "0.60 ~ 0.03",   "E" => "0.30 ~ 0.03",  "A" => "0.30 ~ 0.03",       "L" => "0.15 ~ 0.05"},  "N", tol), # 0201
    Chip.new("CAP", { "D" =>  "1.00 ~ 0.10",   "E" => "0.50 ~ 0.10",  "A" => "0.50 ~ 0.10",       "L" => "0.25 ~ 0.15"},  "N", tol), # 0402
    Chip.new("CAP", { "D" =>  "1.60 ~ 0.15",   "E" => "0.80 ~ 0.15",  "A" => "0.65 ~ 0.15",       "L" => "0.35 ~ 0.20"},  "N", tol), # 0603
    Chip.new("CAP", { "D" =>  "2.00 ~ 0.20",   "E" => "1.25 ~ 0.20",  "A" => { "max" => "1.25" }, "L" => "0.50 ~ 0.25"},  "N", tol), # 0805
    Chip.new("CAP", { "D" =>  "3.20 ~ 0.30",   "E" => "1.60 ~ 0.30",  "A" => { "max" => "1.25" }, "L" => "0.50 ~ 0.25"},  "N", tol), # 1206
    Chip.new("CAP", { "D" =>  "3.20 ~ 0.30",   "E" => "2.50 ~ 0.30",  "A" => { "max" => "1.60" }, "L" => "0.50 ~ 0.25"},  "N", tol), # 1210
    Chip.new("CAP", { "D" =>  "4.50 ~ 0.40",   "E" => "3.20 ~ 0.40",  "A" => { "max" => "2.50" }, "L" => "0.60 ~ 0.35"},  "N", tol), # 1812
 
    Chip.new("RES", { "D" =>  "0.60 ~ 0.03",   "E" => "0.30 ~ 0.03",  "A" => "0.25 ~ 0.05",       "L" => "0.15 ~ 0.05"},  "N", tol), # 0201
    Chip.new("RES", { "D" =>  "1.00 ~ 0.10",   "E" => "0.50 ~ 0.10",  "A" => "0.35 ~ 0.10",       "L" => "0.25 ~ 0.10"},  "N", tol), # 0402
    Chip.new("RES", { "D" =>  "1.60 ~ 0.15",   "E" => "0.80 ~ 0.15",  "A" => "0.45 ~ 0.10",       "L" => "0.30 ~ 0.20"},  "N", tol), # 0603
    Chip.new("RES", { "D" =>  "2.00 ~ 0.20",   "E" => "1.25 ~ 0.20",  "A" => "0.45 ~ 0.15",       "L" => "0.50 ~ 0.20"},  "N", tol), # 0805
    Chip.new("RES", { "D" =>  "3.20 ~ 0.20",   "E" => "1.60 ~ 0.20",  "A" => "0.55 ~ 0.15",       "L" => "0.50 ~ 0.20"},  "N", tol), # 1206
    Chip.new("RES", { "D" =>  "3.20 ~ 0.20",   "E" => "2.50 ~ 0.20",  "A" => "0.55 ~ 0.15",       "L" => "0.50 ~ 0.20"},  "N", tol), # 1210
    Chip.new("RES", { "D" =>  "3.20 ~ 0.20",   "E" => "4.60 ~ 0.20",  "A" => "0.55 ~ 0.15",       "L" => "0.50 ~ 0.20"},  "N", tol), # 1218
    Chip.new("RES", { "D" =>  "4.50 ~ 0.20",   "E" => "3.20 ~ 0.20",  "A" => "0.55 ~ 0.20",       "L" => "0.60 ~ 0.25"},  "N", tol), # 1812
    Chip.new("RES", { "D" =>  "5.00 ~ 0.20",   "E" => "2.50 ~ 0.20",  "A" => "0.60 ~ 0.20",       "L" => "0.60 ~ 0.25"},  "N", tol), # 2010
    Chip.new("RES", { "D" =>  "6.30 ~ 0.20",   "E" => "3.20 ~ 0.20",  "A" => "0.60 ~ 0.20",       "L" => "0.60 ~ 0.25"},  "N", tol), # 2512

    Chip.new("FUS", { "D" =>  "1.40 ..  1.80", "E" => "0.60 .. 1.00", "A" => "0.35 .. 0.95",      "L" => "0.10 .. 0.50"}, "N", tol), # 0603
    Chip.new("FUS", { "D" =>  "2.00 ..  2.20", "E" => "1.30 .. 1.50", "A" => "0.44 .. 1.04",      "L" => "0.25 .. 0.75"}, "N", tol), # 0805
    Chip.new("FUS", { "D" =>  "3.00 ..  3.40", "E" => "1.37 .. 1.80", "A" => "0.28 .. 1.10",      "L" => "0.25 .. 0.75"}, "N", tol), # 1206
    Chip.new("FUS", { "D" =>  "3.00 ..  3.43", "E" => "2.35 .. 2.80", "A" => "0.28 .. 1.22",      "L" => "0.25 .. 0.75"}, "N", tol), # 1210
    Chip.new("FUS", { "D" =>  "4.37 ..  4.73", "E" => "3.07 .. 3.41", "A" => "0.28 .. 1.94",      "L" => "0.25 .. 0.95"}, "N", tol), # 1812
    Chip.new("FUS", { "D" =>  "4.72 ..  5.44", "E" => "4.22 .. 4.93", "A" => "0.63 .. 1.74",      "L" => "0.25 .. 0.36"}, "N", tol), # 2018
    Chip.new("FUS", { "D" =>  "6.73 ..  7.98", "E" => "4.80 .. 5.44", "A" => { "max" => "3.18" }, "L" => "0.56 .. 0.71"}, "N", tol), # 2920
    Chip.new("FUS", { "D" =>  "8.00 ..  9.40", "E" => "6.00 .. 67.1", "A" => { "max" => "3.00" }, "L" => "0.56 .. 0.71"}, "N", tol), # 3425
    Chip.new("FUS", { "D" => "11.15 .. 11.51", "E" => "4.83 .. 5.33", "A" => "0.33 .. 0.53",      "L" => "0.51 .. 1.02"}, "N", tol), # 4420

    GullWing.new("SOT",  { "D" =>  "2.80 ..  3.00", "E" => "2.30 .. 2.50",       "E1" => "1.20 .. 1.40", "A" => { "max" => "1.10" }, "A1" => "0.013 .. 0.10",
                           "b" =>  "0.37 ..  0.51", "e" => { "nom" => "0.915" }, "L"  => "0.25 .. 0.55"}, 3,  "N", tol),                                     # SOT23-3
    GullWing.new("SOT",  { "D" =>  "2.75 ..  3.05", "E" => "2.55 .. 3.05",       "E1" => "1.45 .. 1.75", "A" => { "max" => "1.10" }, "A1" => "0.00 .. 0.10",
                           "b" =>  "0.30 ..  0.50", "e" => { "nom" => "0.95" },  "L"  => "0.30 .. 0.60"}, 5,  "N", tol),                                     # SOT23-5
    GullWing.new("SOT",  { "D" =>  "2.75 ..  3.05", "E" => "2.55 .. 3.05",       "E1" => "1.45 .. 1.75", "A" => { "max" => "1.10" }, "A1" => "0.00 .. 0.10",
                           "b" =>  "0.30 ..  0.50", "e" => { "nom" => "0.95" },  "L"  => "0.30 .. 0.60"}, 6,  "N", tol),                                     # SOT23-6

    GullWing.new("SOIC", { "D" =>  "4.80 ..  5.00", "E" => "5.80 .. 6.20",      "E1" => "3.80 .. 4.00", "A" => { "max" => "1.75" }, "A1" => "0.10 .. 0.25",
                           "b" =>  "0.31 ..  0.51", "e" => { "nom" => "1.27" }, "L"  => "0.40 .. 1.27"}, 8,  "N", tol),                                     # SOIC-8
    GullWing.new("SOIC", { "D" =>  "6.05 ..  6.25", "E" => "5.80 .. 6.20",      "E1" => "3.80 .. 4.00", "A" => { "max" => "1.75" }, "A1" => "0.10 .. 0.25",
                           "b" =>  "0.31 ..  0.51", "e" => { "nom" => "1.27" }, "L"  => "0.40 .. 1.27"}, 10, "N", tol),                                     # SOIC-10
    GullWing.new("SOIC", { "D" =>  "7.30 ..  7.50", "E" => "5.80 .. 6.20",      "E1" => "3.80 .. 4.00", "A" => { "max" => "1.75" }, "A1" => "0.10 .. 0.25",
                           "b" =>  "0.31 ..  0.51", "e" => { "nom" => "1.27" }, "L"  => "0.40 .. 1.27"}, 12, "N", tol),                                     # SOIC-12
    GullWing.new("SOIC", { "D" =>  "8.55 ..  8.75", "E" => "5.80 .. 6.20",      "E1" => "3.80 .. 4.00", "A" => { "max" => "1.75" }, "A1" => "0.10 .. 0.25",
                           "b" =>  "0.31 ..  0.51", "e" => { "nom" => "1.27" }, "L"  => "0.40 .. 1.27"}, 14, "N", tol),                                     # SOIC-14
    GullWing.new("SOIC", { "D" =>  "9.80 .. 10.00", "E" => "5.80 .. 6.20",      "E1" => "3.80 .. 4.00", "A" => { "max" => "1.75" }, "A1" => "0.10 .. 0.25",
                           "b" =>  "0.31 ..  0.51", "e" => { "nom" => "1.27" }, "L"  => "0.40 .. 1.27"}, 16, "N", tol),                                     # SOIC-16
    GullWing.new("SOIC", { "D" => "11.05 .. 11.25", "E" => "5.80 .. 6.20",      "E1" => "3.80 .. 4.00", "A" => { "max" => "1.75" }, "A1" => "0.10 .. 0.25",
                           "b" =>  "0.31 ..  0.51", "e" => { "nom" => "1.27" }, "L"  => "0.40 .. 1.27"}, 18, "N", tol),                                     # SOIC-18
    GullWing.new("SOIC", { "D" => "12.30 .. 12.50", "E" => "5.80 .. 6.20",      "E1" => "3.80 .. 4.00", "A" => { "max" => "1.75" }, "A1" => "0.10 .. 0.25",
                           "b" =>  "0.31 ..  0.51", "e" => { "nom" => "1.27" }, "L"  => "0.40 .. 1.27"}, 20, "N", tol),                                     # SOIC-20

    GullWing.new("SMD", { "D" =>  "8.13 .. 8.51", "E" => "9.80 .. 10.30", "E1" => "6.20 .. 6.50", "A" => { "max" => "2.60" }, "A1" => "0.076 .. 0.33",
                          "b" =>  "1.02 .. 1.20", "e" => "5.00 ..  5.20", "L"  => "1.02 .. 1.53"}, 4,  "N", tol),                                     # SMD-4 (Taiwan Semi DBLS203G)

#    FlatLead.new("SOTFL",{ "D" => " 2.80 ..  3.20", "E" => "2.30 .. 2.70",      "E1" => "1.80 .. 2.20", "A" => { "max" => "1.00" },
#                           "b" =>  "0.40 ..  0.60", "e" => { "nom" => "1.27" }, "L"  => "0.40 .. 0.60"}, 6, "N", tol),

    FlatLeadChip.new("SODFL", "DIO", { "D" => "4.80 .. 5.59",      "D1" => "4.00 .. 4.60", "E" => "2.29 .. 2.92",
                                       "A" => { "max" => "2.30" }, "b"  => "1.27 .. 1.63", "L" => "0.76 .. 1.52"}, "N", tol), # SMA
    FlatLeadChip.new("SODFL", "DIO", { "D" => "5.00 .. 5.59",      "D1" => "4.06 .. 4.57", "E" => "3.30 .. 3.94",
                                       "A" => { "max" => "2.50" }, "b"  => "1.96 .. 2.21", "L" => "0.76 .. 1.52"}, "N", tol), # SMB
    FlatLeadChip.new("SODFL", "DIO", { "D" => "7.75 .. 8.13",      "D1" => "6.60 .. 7.11", "E" => "5.59 .. 6.22",
                                       "A" => { "max" => "2.50" }, "b"  => "2.75 .. 3.18", "L" => "0.76 .. 1.52"}, "N", tol), # SMC

    FlatLeadChip.new("SM_2", "RES", { "D" => { "max" =>  "7.9"}, "D1" =>  "6.7 ~ 0.3", "E" => "4.0 ~ 0.3",
                                      "A" => "3.55 ~ 0.3",       "b"  =>  "1.4 ~ 0.3", "L" => "1.5 ~ 0.3"}, "N", tol), # TE Connectivity SM_2 resistor
    FlatLeadChip.new("SM_3", "RES", { "D" => { "max" => "12.0"}, "D1" => "10.5 ~ 0.3", "E" => "5.5 ~ 0.3",
                                      "A" => "5.0  ~ 0.3",       "b"  =>  "1.7 ~ 0.3", "L" => "2.3 ~ 0.3"}, "N", tol), # TE Connectivity SM_3 resistor
    FlatLeadChip.new("SM_5", "RES", { "D" => { "max" => "17.0"}, "D1" => "13.5 ~ 0.3", "E" => "7.3 ~ 0.3",
                                      "A" => "6.8  ~ 0.3",       "b"  => "1.7 ~ 0.3",  "L" => "2.5 ~ 0.3"}, "N", tol), # TE Connectivity SM_5 resistor
].each { |part| save(part) }
#chip = Chip.new("RES", {
#    "D" => "2.00 ~ 0.20",
#    "E" => "1.25 ~ 0.20",
#    "A" => "0.55 ~ 0.15",
#    "L" => "0.50 ~ 0.20"}, "N", IPC7351::Tolerances.new(0.1, 0.05))

#puts chip.generate(PCB, ["outline", "courtyard", "polarized"])

#soic14 = GullWing.new("SOIC14", {
#    "D"  => "8.55 .. 8.75",
#    "E"  => "5.80 .. 6.20",
#    "E1" => "3.80 .. 4.00",
#    "b"  => "0.31 .. 0.51",
#    "e"  => "1.27",
#    "A"  => "1.75",
#    "A1" => "0.10 .. 0.25",
#    "L"  => "0.40 .. 1.27"}, 14, "N", IPC7351::Tolerances.new(0.1, 0.05))
#puts soic14.generate(PCB, ["copper", "solder-mask", "silkscreen"])

