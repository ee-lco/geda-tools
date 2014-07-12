require 'IPC7351'

module IPC7351
    class SO < Footprint
        include Leaded
        include DualRowPads
        include GullWing

        def initialize(type, pins, spec, settings, env = nil)
            @type = type
            @pins = pins
            @mark = true
            super(spec, settings, env)
        end

        def self.soic(pins, settings, env = nil)
            # Texas Instruments D package
            d = pins / 2 * 1.25 - 0.1
            spec = {
                "D"    => { "nom" => d, "tol" => 0.20 },
                "E"    => "5.80 .. 6.20",
                "E1"   => "3.80 .. 4.00",
                "A"    => { "max" => "1.75" },
                "A1"   => "0.10 .. 0.25",
                "b"    => "0.31 .. 0.51",
                "e"    => { "nom" => "1.27" },
                "L"    => "0.40 .. 1.27",
            }
            $stderr.puts spec["D"]
            return SO.new("SOIC", pins, spec, settings, env)
        end

        def self.soicw(pins, settings, env = nil)
            # Texas Instruments DW package
            d  = pins / 2 * 1.25 + 0.3
            d += 0.1 if (pins > 20)
            spec = {
                "D"    => { "nom" => d, "tol" => 0.40 },
                "E"    => "9.97 .. 10.63",
                "E1"   => "7.40 ..  7.60",
                "A"    => { "max" => "2.65" },
                "A1"   => "0.10 ..  0.30",
                "b"    => "0.31 ..  0.51",
                "e"    => { "nom" => "1.27" },
                "L"    => "0.40 ..  1.27",
            }
            return SO.new("SOIC", pins, spec, settings, env)
        end

        def generate_text
            @name   = "%s%dP%dX%d-%d%s" % [@type, f_x100(@spec["e"].nom), f_x100(@spec["E"].nom), f_x100(@spec["A"].max), @pins, @settings["environment"]]
            @refdes = "U"
            @desc   = "%s-%d" % [@type, @pins]
        end
    end

    class Chip < Footprint
        include DualNoLead
        include DualPads
        include RectEndCaps

        def initialize(type, spec, settings, env = nil)
            @type = type
            @mark = ["CAPP", "DIO", "LED"].include?(@type)
            super(spec, settings, env)
        end

        def generate_text
            @name   = "%sC%d%dX%d%s" % [@type, f_x10(@spec["D"]["nom"]), f_x10(@spec["E"]["nom"]), f_x100(@spec["A"]["max"]), @settings["environment"]]
            @refdes = {
                "CAP"  => "C",
                "CAPP" => "C",
                "DIO"  => "D",
                "IND"  => "L",
                "LED"  => "D",
                "RES"  => "R",
                "FUS"  => "F",
            }[@type]
            @desc   = "TBD"
        end
    end
end

