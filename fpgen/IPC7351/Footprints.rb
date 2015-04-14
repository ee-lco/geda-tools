require_relative 'BaseFootprints'

module IPC7351
    class QFP < Footprint
        include Silkscreen::Leaded
        include Pads::QuadRow
        include Leads::GullWing

        def initialize(type, can_name, pins, spec, settings, env = nil)
            @type, @can_name, @pins = type, can_name, pins
            @mark = true
            super(spec, settings, env)
        end

        def generate_text
            @ipc_name = "%s%dP%dX%dX%d-%dL%d%s" % [@type, f_x100(@spec["e"].nom), f_x100(@spec["D"].nom), f_x100(@spec["E"].nom), f_x100(@spec["A"].max), @pins, f_x100(@spec["L"].max - @spec["L"].min), @settings["environment"]]
            @can_name = "%s-%d" % [@can_name, @pins]
            @refdes   = "U"
            super()
        end
    end

    class SO < Footprint
        include Silkscreen::Leaded
        include Pads::DualRow
        include Leads::GullWing

        def initialize(type, can_name, pins, spec, settings, env = nil)
            @type, @can_name, @pins = type, can_name, pins
            @mark = true
            super(spec, settings, env)
        end

        def generate_text
            @ipc_name = "%s%dP%dX%d-%d%s" % [@type, f_x100(@spec["e"].nom), f_x100(@spec["E"].nom), f_x100(@spec["A"].max), @pins, @settings["environment"]]
            @can_name = "%s-%d" % [@can_name, @pins]
            @refdes   = "U"
            super()
        end
    end

    class SOFL < Footprint
        include Silkscreen::Leaded
        include Pads::DualRow
        include Leads::FlatLead

        def initialize(type, can_name, pins, spec, settings, env = nil)
            @type, @can_name, @pins = type, can_name, pins
            @mark = true
            super(spec, settings, env)
        end

        def generate_text
            @ipc_name = "%sFL%dP%dX%d-%d%s" % [@type, f_x100(@spec["e"].nom), f_x100(@spec["E"].nom), f_x100(@spec["A"].max), @pins, @settings["environment"]]
            @can_name = "%s-%d" % [@can_name, @pins]
            @refdes   = "U"
            super()
        end
    end

    class SOD < Footprint
        include Silkscreen::Leaded
        include Pads::Dual
        include Leads::GullWing

        def initialize(can_name, spec, settings, env = nil)
            @type = "DIO"
            @can_name = can_name
            @mark = true
            super(spec, settings, env)
        end

        def generate_text
            @ipc_name = "SOD%d%dX%d%s" % [f_x10(@spec["D"]["nom"]), f_x10(@spec["E"]["nom"]), f_x100(@spec["A"]["max"]), @settings["environment"]]
            @refdes = "D"
            super()
        end
    end

    class SODFL < Footprint
        include Silkscreen::Leaded
        include Pads::Dual
        include Leads::FlatLead

        def initialize(can_name, spec, settings, env = nil)
            @type = "DIO"
            @can_name = can_name
            @mark = true
            super(spec, settings, env)
        end

        def generate_text
            @ipc_name = "SODFL%d%dX%d%s" % [f_x10(@spec["D"]["nom"]), f_x10(@spec["E"]["nom"]), f_x100(@spec["A"]["max"]), @settings["environment"]]
            @refdes = "D"
            super()
        end
    end

    class Chip < Footprint
        include Silkscreen::DualNoLead
        include Pads::Dual
        include Leads::RectEndCap

        def initialize(type, can_name, suffix, spec, settings, env = nil)
            @type, @can_name, @suffix = type, can_name, suffix
            @mark = ["P"].include?(@suffix) || ["DIO", "LED"].include?(@type)
            super(spec, settings, env)
        end

        def generate_text
            @ipc_name = "%sC%s%02d%02dX%d%s" % [@type, @suffix || "", f_x10(@spec["D"]["nom"]), f_x10(@spec["E"]["nom"]), f_x100(@spec["A"]["max"]), @settings["environment"]]
            @refdes = {
                "CAP"  => "C",
                "DIO"  => "D",
                "IND"  => "L",
                "LED"  => "D",
                "RES"  => "R",
                "FUS"  => "F",
            }[@type]
            super()
        end
    end

    class MoldedBody < Footprint
        include Silkscreen::Leaded
        include Pads::Dual
        include Leads::InwardL

        def initialize(type, can_name, suffix, spec, settings, env = nil)
            @type, @can_name, @suffix = type, can_name, suffix
            @mark = ["P"].include?(@suffix) || ["DIO", "LED"].include?(@type)
            super(spec, settings, env)
        end

        def generate_text
            @ipc_name = "%sM%s%d%dX%d%s" % [@type, @suffix || "", f_x10(@spec["D"]["nom"]), f_x10(@spec["E"]["nom"]), f_x100(@spec["A"]["max"]), @settings["environment"]]
            @refdes = {
                "CAP"  => "C",
                "DIO"  => "D",
                "IND"  => "L",
                "LED"  => "D",
                "RES"  => "R",
                "FUS"  => "F",
            }[@type]
            super()
        end
    end

    class Crystal < Footprint
        include Silkscreen::Leaded
        include Pads::Dual
        include Leads::UnderBodyOutwardL

        def initialize(can_name, spec, settings, env = nil)
            @can_name = can_name
            @mark = false
            super(spec, settings, env)
        end

        def generate_text
            @ipc_name = "XTAL%dX%dX%dD%dL%d%s" % [f_x100(@spec["D1"]["nom"]), f_x100(@spec["E"]["nom"]), f_x100(@spec["A"]["max"]), f_x100(@spec["D"]["nom"]), f_x100(@spec["L"]["nom"]), @settings["environment"]]
            @refdes = "Y"
            super()
        end
    end
end

