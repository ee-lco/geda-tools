require_relative 'IPC7351/Footprints'

require_relative 'IPC7351/Environments'
require_relative 'IPC7351/Settings'

require_relative 'IPC7351/GedaPCB'
require_relative 'IPC7351/SVG'
require_relative 'IPC7351/Textfile'


class FPG
    def initialize(settings, env = nil)
        @settings, @env = settings, env
    end

    def capc
        {
            "0201" => { "D" => "0.60 ~ 0.05", "E" => " 0.30 ~ 0.05", "A" => { "max" => 0.35 }, "L" => "0.15 ~ 0.05" },
            "0402" => { "D" => "1.00 ~ 0.10", "E" => " 0.50 ~ 0.10", "A" => { "max" => 0.60 }, "L" => "0.30 ~ 0.15" },
            "0603" => { "D" => "1.60 ~ 0.15", "E" => " 0.80 ~ 0.15", "A" => { "max" => 1.00 }, "L" => "0.35 ~ 0.15" },
            "0805" => { "D" => "2.00 ~ 0.20", "E" => " 1.25 ~ 0.20", "A" => { "max" => 1.50 }, "L" => "0.50 ~ 0.25" },
            "1206" => { "D" => "3.20 ~ 0.25", "E" => " 1.60 ~ 0.25", "A" => { "max" => 1.75 }, "L" => "0.50 ~ 0.25" },
            "1210" => { "D" => "3.20 ~ 0.25", "E" => " 2.50 ~ 0.25", "A" => { "max" => 1.75 }, "L" => "0.50 ~ 0.25" },
            "1808" => { "D" => "4.60 ~ 0.40", "E" => " 2.00 ~ 0.25", "A" => { "max" => 2.20 }, "L" => "0.60 ~ 0.35" },
            "1812" => { "D" => "4.60 ~ 0.40", "E" => " 3.20 ~ 0.30", "A" => { "max" => 2.20 }, "L" => "0.60 ~ 0.35" },
            "1825" => { "D" => "4.50 ~ 0.40", "E" => " 6.40 ~ 0.40", "A" => { "max" => 2.20 }, "L" => "0.60 ~ 0.35" },
            "2220" => { "D" => "5.60 ~ 0.40", "E" => " 5.08 ~ 0.40", "A" => { "max" => 2.75 }, "L" => "0.60 ~ 0.35" },
            "2225" => { "D" => "5.60 ~ 0.40", "E" => " 6.35 ~ 0.40", "A" => { "max" => 2.75 }, "L" => "0.60 ~ 0.35" },
            "3640" => { "D" => "9.00 ~ 0.40", "E" => "10.20 ~ 0.40", "A" => { "max" => 2.75 }, "L" => "0.60 ~ 0.35" },
        # Vishay VJ series
            #"0201" => { "D" => "0.60 ~ 0.03",        "E" => " 0.30 ~ 0.03",        "A" => { "max" => 0.33 }, "L" => "0.15 ~  0.05" },
            #"0402" => { "D" => "1.00  -0.05..+0.10", "E" => " 0.50  -0.05..+0.10", "A" => { "max" => 0.60 }, "L" => "0.10 .. 0.41" },
            #"0603" => { "D" => "1.60 ~ 0.15",        "E" => " 0.80 ~ 0.15",        "A" => { "max" => 0.92 }, "L" => "0.30 .. 0.46" },
            #"0805" => { "D" => "2.00 ~ 0.20",        "E" => " 1.25 ~ 0.20",        "A" => { "max" => 1.45 }, "L" => "0.25 .. 0.72" },
            #"1206" => { "D" => "3.20 ~ 0.25",        "E" => " 1.60 ~ 0.25",        "A" => { "max" => 1.70 }, "L" => "0.25 .. 0.71" },
            #"1210" => { "D" => "3.20 ~ 0.25",        "E" => " 2.50 ~ 0.25",        "A" => { "max" => 1.70 }, "L" => "0.25 .. 0.71" },
            #"1808" => { "D" => "4.57 ~ 0.30",        "E" => " 2.03 ~ 0.25",        "A" => { "max" => 2.18 }, "L" => "0.25 .. 0.76" },
            #"1812" => { "D" => "4.50 ~ 0.30",        "E" => " 3.20 ~ 0.20",        "A" => { "max" => 2.18 }, "L" => "0.25 .. 0.76" },
            #"1825" => { "D" => "4.50 ~ 0.30",        "E" => " 6.40 ~ 0.25",        "A" => { "max" => 2.18 }, "L" => "0.25 .. 0.76" },
            #"2220" => { "D" => "5.59 ~ 0.25",        "E" => " 5.08 ~ 0.25",        "A" => { "max" => 0.92 }, "L" => "0.30 .. 0.46" },
            #"2225" => { "D" => "5.59 ~ 0.25",        "E" => " 6.35 ~ 0.25",        "A" => { "max" => 0.92 }, "L" => "0.30 .. 0.46" },
            #"3640" => { "D" => "9.14 ~ 0.38",        "E" => "10.20 ~ 0.38",        "A" => { "max" => 2.18 }, "L" => "0.25 .. 0.76" },
        # Murata
            #"0402" => { "D" => "1.00 ~ 0.05",        "E" => " 0.50 ~ 0.05",        "A" => { "max" => 0.55 }, "L" => "0.30 ~ 0.10" },
            #"0603" => { "D" => "1.60 ~ 0.15",        "E" => " 0.80 ~ 0.15",        "A" => { "max" => 0.95 }, "L" => "0.35 ~ 0.15" },
            #"0805" => { "D" => "2.00 ~ 0.20",        "E" => " 1.25 ~ 0.20",        "A" => { "max" => 1.45 }, "L" => "0.50 ~ 0.25" },
            #"1206" => { "D" => "3.20 ~ 0.20",        "E" => " 1.60 ~ 0.20",        "A" => { "max" => 1.80 }, "L" => "0.50 ~ 0.25" },
            #"1210" => { "D" => "3.20 ~ 0.20",        "E" => " 2.50 ~ 0.20",        "A" => { "max" => 2.80 }, "L" => "0.50 ~ 0.25" },
            #"1808" => { "D" => "4.70 ~ 0.50",        "E" => " 2.00 ~ 0.20",        "A" => { "max" => 1.15 }, "L" => "0.60 ~ 0.35" },
            #"1812" => { "D" => "4.50 ~ 0.30",        "E" => " 3.20 ~ 0.30",        "A" => { "max" => 2.78 }, "L" => "0.60 ~ 0.35" },
            #"1825" => { "D" => "4.50 ~ 0.30",        "E" => " 6.40 ~ 0.40",        "A" => { "max" => 1.65 }, "L" => "0.60 ~ 0.35" },
            #"2220" => { "D" => "5.70 ~ 0.40",        "E" => " 5.00 ~ 0.40",        "A" => { "max" => 2.65 }, "L" => "0.60 ~ 0.35" },
            #"2225" => { "D" => "5.60 ~ 0.40",        "E" => " 6.40 ~ 0.40",        "A" => { "max" => 1.55 }, "L" => "0.60 ~ 0.35" },
        }.collect do |name, spec|
            next IPC7351::Chip.new("CAP", name, nil, spec, @settings, @env)
        end
    end

    def fusc
        # TE Connectivity SMD series
        # Note: exact sizes vary, overall min/max is used
        {
            "0603" => { "D" => "1.40 .. 1.80", "E" => "0.60 .. 1.00", "A" => "0.35 .. 0.85", "L" => "0.10 .. 0.50" },
            "0805" => { "D" => "2.00 .. 2.20", "E" => "1.30 .. 1.50", "A" => "0.44 .. 1.20", "L" => "0.25 .. 0.75" },
            "1206" => { "D" => "3.00 .. 3.40", "E" => "1.37 .. 1.80", "A" => "0.28 .. 1.10", "L" => "0.25 .. 0.75" },
            "1210" => { "D" => "3.00 .. 3.43", "E" => "2.35 .. 2.80", "A" => "0.28 .. 1.22", "L" => "0.25 .. 0.75" },
            "1812" => { "D" => "4.37 .. 4.83", "E" => "3.07 .. 3.41", "A" => "0.28 .. 1.94", "L" => "0.25 .. 0.95" },
        }.collect do |name, spec|
            next IPC7351::Chip.new("FUS", name, nil, spec, @settings, @env)
        end
    end

    def ledc
        # OSRAM
        {
            "0402" => { "D" => "0.90 .. 1.10", "E" => "0.40 .. 0.60", "A" => { "max" => "0.40" }, "L" => "0.15 .. 0.35" },
            "0603" => { "D" => "1.50 .. 1.70", "E" => "0.70 .. 0.90", "A" => { "max" => "0.40" }, "L" => "0.30 .. 0.50" },
            "0805" => { "D" => "1.90 .. 2.10", "E" => "1.15 .. 1.35", "A" => { "max" => "0.90" }, "L" => "0.30 .. 0.50" },
            "1206" => { "D" => "3.10 .. 3.30", "E" => "1.50 .. 1.70", "A" => { "max" => "1.20" }, "L" => "0.40 .. 0.60" },
        }.collect do |name, spec|
            next IPC7351::Chip.new("LED", name, nil, spec, @settings, @env)
        end
    end

    def resc
        {
            "01005" => { "D" => "0.40 ~ 0.03", "E" => "0.20 ~ 0.03", "A" => { "max" => "0.15" }, "L" => "0.10 ~ 0.03" },
            "0201"  => { "D" => "0.60 ~ 0.05", "E" => "0.30 ~ 0.05", "A" => { "max" => "0.30" }, "L" => "0.15 ~ 0.05" },
            "0402"  => { "D" => "1.00 ~ 0.10", "E" => "0.50 ~ 0.10", "A" => { "max" => "0.40" }, "L" => "0.25 ~ 0.10" },
            "0603"  => { "D" => "1.60 ~ 0.15", "E" => "0.80 ~ 0.15", "A" => { "max" => "0.55" }, "L" => "0.30 ~ 0.15" },
            "0805"  => { "D" => "2.00 ~ 0.20", "E" => "1.25 ~ 0.20", "A" => { "max" => "0.70" }, "L" => "0.40 ~ 0.20" },
            "1206"  => { "D" => "3.20 ~ 0.25", "E" => "1.60 ~ 0.25", "A" => { "max" => "0.70" }, "L" => "0.50 ~ 0.20" },
            "1210"  => { "D" => "3.20 ~ 0.25", "E" => "2.50 ~ 0.25", "A" => { "max" => "0.70" }, "L" => "0.50 ~ 0.20" },
            "1225"  => { "D" => "3.20 ~ 0.25", "E" => "6.40 ~ 0.25", "A" => { "max" => "0.70" }, "L" => "0.60 ~ 0.20" },
            "1812"  => { "D" => "4.50 ~ 0.30", "E" => "3.20 ~ 0.30", "A" => { "max" => "0.70" }, "L" => "0.50 ~ 0.20" },
            "2010"  => { "D" => "5.00 ~ 0.30", "E" => "2.50 ~ 0.30", "A" => { "max" => "0.70" }, "L" => "0.60 ~ 0.20" },
            "2512"  => { "D" => "6.40 ~ 0.30", "E" => "3.20 ~ 0.30", "A" => { "max" => "0.70" }, "L" => "0.60 ~ 0.20" },
        # Vishay CRCW series
            #"0402" => { "D" => "1.00 ~ 0.05",        "E" => "0.50 ~ 0.05", "A" => "0.35 ~ 0.05", "L" => "0.25 ~ 0.05"       },
            #"0603" => { "D" => "1.55  -0.05..+0.10", "E" => "0.85 ~ 0.10", "A" => "0.45 ~ 0.05", "L" => "0.30 ~ 0.20"       },
            #"0805" => { "D" => "2.00  -0.10..+0.20", "E" => "1.25 ~ 0.15", "A" => "0.45 ~ 0.05", "L" => "0.30 -0.10..+0.20" },
            #"1206" => { "D" => "3.20  -0.20..+0.10", "E" => "1.60 ~ 0.15", "A" => "0.55 ~ 0.05", "L" => "0.45 ~ 0.20"       },
            #"1210" => { "D" => "3.20 ~ 0.20",        "E" => "2.50 ~ 0.20", "A" => "0.55 ~ 0.05", "L" => "0.45 ~ 0.20"       },
            #"1218" => { "D" => "3.20  -0.20..+0.10", "E" => "4.60 ~ 0.15", "A" => "0.55 ~ 0.05", "L" => "0.45 ~ 0.20"       },
            #"2010" => { "D" => "5.00 ~ 0.15",        "E" => "2.50 ~ 0.15", "A" => "0.60 ~ 0.10", "L" => "0.60 ~ 0.20"       },
            #"2512" => { "D" => "6.30 ~ 0.20",        "E" => "3.15 ~ 0.15", "A" => "0.60 ~ 0.10", "L" => "0.60 ~ 0.20"       },
        # Panasonic ERJ series
            #"01005" => { "D" => "0.40 ~ 0.02",        "E" => "0.20 ~ 0.02",        "A" => "0.13 ~ 0.03", "L" => "0.10 ~ 0.03" },
            #"0201"  => { "D" => "0.60 ~ 0.03",        "E" => "0.30 ~ 0.03",        "A" => "0.23 ~ 0.03", "L" => "0.15 ~ 0.05" },
            #"0402"  => { "D" => "1.00 ~ 0.05",        "E" => "0.50 ~ 0.05",        "A" => "0.35 ~ 0.05", "L" => "0.25 ~ 0.05" },
            #"0603"  => { "D" => "1.60 ~ 0.15",        "E" => "0.80  -0.05..+0.15", "A" => "0.45 ~ 0.10", "L" => "0.30 ~ 0.15" },
            #"0805"  => { "D" => "2.00 ~ 0.20",        "E" => "1.25 ~ 0.10",        "A" => "0.60 ~ 0.10", "L" => "0.40 ~ 0.20" },
            #"1206"  => { "D" => "3.20  -0.20..+0.05", "E" => "1.60  -0.15..+0.05", "A" => "0.60 ~ 0.10", "L" => "0.50 ~ 0.20" },
            #"1210"  => { "D" => "3.20 ~ 0.20",        "E" => "2.50 ~ 0.20",        "A" => "0.60 ~ 0.10", "L" => "0.50 ~ 0.20" },
            #"1812"  => { "D" => "4.50 ~ 0.20",        "E" => "3.20 ~ 0.20",        "A" => "0.60 ~ 0.10", "L" => "0.50 ~ 0.20" },
            #"2010"  => { "D" => "5.00 ~ 0.20",        "E" => "2.50 ~ 0.20",        "A" => "0.60 ~ 0.10", "L" => "0.60 ~ 0.20" },
            #"2512"  => { "D" => "6.40 ~ 0.20",        "E" => "3.20 ~ 0.20",        "A" => "0.60 ~ 0.10", "L" => "0.60 ~ 0.20" },
        }.collect do |name, spec|
            next IPC7351::Chip.new("RES", name, nil, spec, @settings, @env)
        end
    end

    def sod
        {
            "SOD123"   => { "D" => "3.50 .. 3.90", "D1" => "2.50 .. 2.90", "E" => "1.40 .. 1.80", "A" => { "max" => "0.94" }, "A1" => "0.00 .. 0.10", "b" => "0.50 .. 0.75", "L" => "0.25 .. 0.40" },
            "SOD323"   => { "D" => "2.30 .. 2.70", "D1" => "1.60 .. 1.80", "E" => "1.15 .. 1.35", "A" => { "max" => "0.80" }, "A1" => "0.00 .. 0.10", "b" => "0.25 .. 0.40", "L" => "0.20 .. 0.40" },
        }.collect do |name, spec|
            next IPC7351::SOD.new(name, spec, @settings, @env)
        end
    end

    def sodfl
        {
            "SOD123FL" => { "D" => "3.40 .. 3.60", "D1" => "2.50 .. 2.70", "E" => "1.50 .. 1.70", "A" => { "max" => "0.90" },                         "b" => "0.55 .. 1.10", "L" => "0.35 .. 0.95" },
            "SOD323FL" => { "D" => "2.30 .. 2.70", "D1" => "1.60 .. 1.80", "E" => "1.15 .. 1.35", "A" => { "max" => "0.65" },                         "b" => "0.30 .. 0.50", "L" => "0.30 .. 0.50" },
            "SOD523FL" => { "D" => "1.50 .. 1.70", "D1" => "1.10 .. 1.30", "E" => "0.70 .. 0.90", "A" => { "max" => "0.50" },                         "b" => "0.25 .. 0.35", "L" => "0.15 .. 0.30" },
            "SOD723FL" => { "D" => "1.35 .. 1.45", "D1" => "0.95 .. 1.05", "E" => "0.55 .. 0.65", "A" => { "max" => "0.49" },                         "b" => "0.25 .. 0.32", "L" => "0.15 .. 0.25" },
            "SOD923FL" => { "D" => "0.95 .. 1.05", "D1" => "0.75 .. 0.85", "E" => "0.55 .. 0.65", "A" => { "max" => "0.34" },                         "b" => "0.15 .. 0.25", "L" => "0.15 .. 0.25" },
        }.collect do |name, spec|
            next IPC7351::SODFL.new(name, spec, @settings, @env)
        end
        # Diodes Inc. SODx23
            #"SOD123"   => { "D" => "3.55 .. 3.85", "D1" => "2.55 .. 2.85", "E" => "1.40 .. 1.70", "A" => "1.00 .. 1.35", "A1" => "0.00 .. 0.10", "b" => { "nom" => "0.55" }, "L" => "0.25 .. 0.40" },
            #"SOD323"   => { "D" => "2.30 .. 2.70", "D1" => "1.60 .. 1.80", "E" => "1.20 .. 1.40", "A" => "1.00 .. 1.10", "A1" => "0.00 .. 0.10", "b" => "0.25 .. 0.35", "L" => "0.20 .. 0.40" },
            #"SOD523FL" => { "D" => "1.50 .. 1.70", "D1" => "1.10 .. 1.30", "E" => "0.70 .. 0.90", "A" => "0.55 .. 0.65", "b"  => "0.25 .. 0.35", "L" => "0.10 .. 0.30" },
        # NXP SODx23
            #"SOD123FL" => { "D" => "3.40 .. 3.60", "D1" => "2.50 .. 2.70", "E" => "1.50 .. 1.70", "A" => "1.00 .. 1.20", "b"  => "0.55 .. 0.70", "L" => "0.35 .. 0.55" },
            #"SOD323"   => { "D" => "2.30 .. 2.70", "D1" => "1.60 .. 1.80", "E" => "1.15 .. 1.35", "A" => "0.80 .. 1.10", "A1" => { "max" => "0.05" }, "b" => "0.25 .. 0.40", "L" => "0.15 .. 0.45" },
            #"SOD323FL" => { "D" => "2.30 .. 2.70", "D1" => "1.60 .. 1.80", "E" => "1.15 .. 1.35", "A" => "0.65 .. 0.80", "b"  => "0.25 .. 0.40", "L" => "0.30 .. 0.50" },
            #"SOD523FL" => { "D" => "1.55 .. 1.65", "D1" => "1.15 .. 1.25", "E" => "0.75 .. 0.85", "A" => "0.58 .. 0.65", "b"  => "0.26 .. 0.34", "L" => "0.15 .. 0.30" },
            #"SOD723FL" => { "D" => "1.35 .. 1.45", "D1" => "0.95 .. 1.05", "E" => "0.55 .. 0.65", "A" => "0.49 .. 0.55", "b"  => "0.25 .. 0.32", "L" => "0.13 .. 0.27" },
        # ON Semiconductor SODx23
            #"SOD123"   => { "D" => "3.56 .. 3.86", "D1" => "2.54 .. 2.84", "E" => "1.40 .. 1.80", "A" => "0.94 .. 1.35", "A1" => "0.00 .. 0.10", "b" => "0.51 .. 0.71", "L" => { "min" => "0.25" } },
            #"SOD123FL" => { "D" => "3.40 .. 3.60", "D1" => "2.50 .. 2.70", "E" => "1.50 .. 1.65", "A" => "0.90 .. 0.98", "A1" => "0.00 .. 0.10", "b" => "0.70 .. 1.10", "L" => "0.55 .. 0.95" },
            #"SOD323"   => { "D" => "2.30 .. 2.70", "D1" => "1.60 .. 1.80", "E" => "1.15 .. 1.35", "A" => "0.80 .. 1.00", "A1" => "0.00 .. 0.10", "b" => "0.25 .. 0.40", "L" => { "min" => "0.08" } },
            #"SOD323FL" => { "D" => "2.30 .. 2.70", "D1" => "1.60 .. 1.80", "E" => "1.15 .. 1.35", "A" => "0.65 .. 0.80", "b"  => "0.25 .. 0.35", "L" => "0.30 .. 0.50" },
            #"SOD523FL" => { "D" => "1.50 .. 1.70", "D1" => "1.10 .. 1.30", "E" => "0.70 .. 0.90", "A" => "0.50 .. 0.70", "b"  => "0.25 .. 0.35", "L" => { "nom" => "0.30" } },
            #"SOD723FL" => { "D" => "1.35 .. 1.45", "D1" => "0.95 .. 1.05", "E" => "0.55 .. 0.65", "A" => "0.49 .. 0.55", "b"  => "0.25 .. 0.32", "L" => "0.15 .. 0.25" },
            #"SOD923FL" => { "D" => "0.95 .. 1.05", "D1" => "0.75 .. 0.85", "E" => "0.55 .. 0.65", "A" => "0.34 .. 0.43", "b"  => "0.15 .. 0.25", "L" => { "nom" => "0.19" } },
    end

    def diom
        {
            "SMA" => { "D" => "4.80 .. 5.60", "E" => "2.29 .. 2.92", "A" => "1.97 .. 2.30", "b" => "1.25 .. 1.65", "L" => "0.76 .. 1.52" },
            "SMB" => { "D" => "5.00 .. 5.60", "E" => "3.30 .. 3.95", "A" => "2.00 .. 2.50", "b" => "1.95 .. 2.21", "L" => "0.76 .. 1.52" },
            "SMC" => { "D" => "7.75 .. 8.13", "E" => "5.59 .. 6.22", "A" => "1.90 .. 2.62", "b" => "2.75 .. 3.20", "L" => "0.76 .. 1.52" },
        # Vishay SMA/SMB/SMC
            #"SMA" => { "D" => "4.93 .. 5.28", "E" => "2.54 .. 2.79", "A" => "1.98 .. 2.29", "b" => "1.25 .. 1.65", "L" => "0.76 .. 1.52" },
            #"SMB" => { "D" => "5.21 .. 5.59", "E" => "3.30 .. 3.94", "A" => "2.13 .. 2.44", "b" => "1.95 .. 2.20", "L" => "0.76 .. 1.52" },
            #"SMC" => { "D" => "7.75 .. 8.13", "E" => "5.59 .. 6.22", "A" => "2.06 .. 2.62", "b" => "2.90 .. 3.20", "L" => "0.76 .. 1.52" },
        # Diodes Inc. SMA/SMB/SMC
            #"SMA" => { "D" => "4.80 .. 5.59", "E" => "2.29 .. 2.92", "A" => "2.01 .. 2.30", "b" => "1.27 .. 1.63", "L" => "0.76 .. 1.52" },
            #"SMB" => { "D" => "5.00 .. 5.59", "E" => "3.30 .. 3.94", "A" => "2.00 .. 2.50", "b" => "1.96 .. 2.21", "L" => "0.76 .. 1.52" },
            #"SMC" => { "D" => "7.75 .. 8.13", "E" => "5.59 .. 6.22", "A" => "2.00 .. 2.50", "b" => "2.75 .. 3.18", "L" => "0.76 .. 1.52" },
        # ON Semiconductor SMA/SMB/SMC
            #"SMA"    => { "D" => "4.83 .. 5.59", "E" => "2.29 .. 2.92", "A" => "1.97 .. 2.20", "b" => "1.27 .. 1.63", "L" => "0.76 .. 1.52" },
            #"SMA-FL" => { "D" => "4.80 .. 5.40", "D1" => "4.00 .. 4.60", "E" => "2.40 .. 2.80", "A" => "0.90 .. 1.10", "b" => "1.25 .. 1.65", "L" => "0.70 .. 1.10" },
            #"SMB"    => { "D" => "5.21 .. 5.60", "E" => "3.30 .. 3.95", "A" => "1.95 .. 2.47", "b" => "1.96 .. 2.20", "L" => "0.76 .. 1.60" },
            #"SMC"    => { "D" => "7.75 .. 8.13", "E" => "5.59 .. 6.10", "A" => "1.90 .. 2.41", "b" => "2.92 .. 3.07", "L" => "0.76 .. 1.27" },
        }.collect do |name, spec|
            next IPC7351::MoldedBody.new("DIO", name, nil, spec, @settings, @env)
        end
    end

    def indm
        {
        # Vishay IHLP-3232CZ-01/11
            "IHLP-3232CZ" => { "D" => "8.64 ~ 0.254", "E" => "8.18 ~ 0.076", "A" => { "max" => "3.0" }, "b" => "5.08 ~ 0.076", "L" => "1.372 ~ 0.318" },
        }.collect do |name, spec|
            next IPC7351::MoldedBody.new("IND", name, nil, spec, @settings, @env)
        end
    end

    def ind_misc
        {
        # Bourns SRF0703A
            #"SRF0703A" => { "D" => "7.5 ~ 0.1", "E" => "7.9 ~ 0.1", "E1" => "7.5 ~ 0.1", "A" => "3.4 ~ 0.2", "A1" => "0.0 .. 0.1", "b" => "0.95 ~ 0.05", "e" => { "nom" => "1.95" }, "L" => "2.5 ~ 0.25" },
            "SRF0703A" => { "D" => "7.5 ~ 0.1", "E" => "8.9 ~ 0.1", "E1" => "7.5 ~ 0.1", "A" => "3.4 ~ 0.2", "A1" => "0.0 .. 0.1", "b" => "1.20 ~ 0.05", "e" => { "nom" => "1.90" }, "L" => "3.0 ~ 0.25" },
        }.collect do |name, spec|
            next IPC7351::SO.new("IND", name, 4, spec, @settings, @env)
        end
    end


    def sot
        # Texas Instruments DBV package
        # Note: D of SOT23-3 is 2.80 .. 3.00
        [3, 5, 6].collect do |pins|
            spec = {
                "D"    => "2.75 .. 3.05",
                "E"    => "2.60 .. 3.00",
                "E1"   => "1.45 .. 1.75",
                "A"    => { "max" => "1.45" },
                "A1"   => "0.00 .. 0.15",
                "b"    => "0.30 .. 0.50",
                "e"    => { "nom" => "0.95" },
                "L"    => "0.30 .. 0.55",
            }
            next IPC7351::SO.new("SOT", "SOT23", pins, spec, @settings, @env)
        end
    end

    def soic
        # Texas Instruments D package
        d = {
             8 => 4.90,
            10 => 6.15,
            12 => 7.40,
            14 => 8.65,
            16 => 9.90,
        }
        d.keys.collect do |pins|
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
            next IPC7351::SO.new("SOIC", "SOIC", pins, spec, @settings, @env)
        end
    end

    def soicw
        # Texas Instruments DW package
        d = {
            16 => 10.30,
            18 => 11.55,
            20 => 12.80,
            24 => 15.40,
            28 => 17.90,
        }
        d.keys.collect do |pins|
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
            next IPC7351::SO.new("SOIC", "SOICW", pins, spec, @settings, @env)
        end
    end

    def tssop
        # Texas Instruments PW package
        d = {
             8 => 3.00,
            14 => 5.00,
            16 => 5.00,
            20 => 6.50,
            24 => 7.80,
            28 => 9.70,
        }
        d.keys.collect do |pins|
            spec = {
                "D"    => { "nom" => d[pins], "tol" => 0.20 },
                "E"    => "6.30 .. 6.50",
                "E1"   => "4.30 .. 4.50",
                "A"    => { "max" => "1.20" },
                "A1"   => "0.05 ..  0.15",
                "b"    => "0.19 ..  0.30",
                "e"    => { "nom" => "0.65" },
                "L"    => "0.50 ..  0.75",
            }
            next IPC7351::SO.new("TSSOP", "TSSOP", pins, spec, @settings, @env)
        end
    end

    def msop
        # Texas Instruments DGK annd DGS packages
        pin_specs = {
             #8 => { "e" => { "nom" => "0.65" }, "b" => "0.25 .. 0.38" },
             8 => { "e" => { "nom" => "0.65" }, "b" => "0.25 .. 0.30" },
            10 => { "e" => { "nom" => "0.50" }, "b" => "0.17 .. 0.27" },
        }
        pin_specs.keys.collect do |pins|
            spec = {
                "D"    => "2.90 .. 3.10",
                "E"    => "4.75 .. 5.05",
                "E1"   => "2.90 .. 3.10",
                "A"    => { "max" => "1.10" },
                "A1"   => "0.05 ..  0.15",
                "L"    => "0.50 ..  0.75",
            }.merge(pin_specs[pins])
            next IPC7351::SO.new("MSOP", "MSOP", pins, spec, @settings, @env)
        end

    end

    def qfp
        # ST QFP package
        d = {
             48 =>  9.00,
             64 => 12.00,
            100 => 16.00,
            144 => 22.00,
            176 => 26.00,
            208 => 30.00,
        }
        d.keys.collect do |pins|
            spec = {
                "D"    => { "nom" => d[pins],        "tol" => 0.40 },
                "D1"   => { "nom" => d[pins] - 2.00, "tol" => 0.40 },
                "E"    => { "nom" => d[pins],        "tol" => 0.40 },
                "E1"   => { "nom" => d[pins] - 2.00, "tol" => 0.40 },
                "A"    => { "max" => "1.60" },
                "A1"   => "0.05 ..  0.15",
                "b"    => "0.17 ..  0.27",
                "e"    => { "nom" => "0.50" },
                "L"    => "0.45 ..  0.75",
            }
            next IPC7351::QFP.new("QFP", "QFP", pins, spec, @settings, @env)
        end
    end

    def crystal
        # TXC HC-49S
        {
            "HC49-US" => { "D" => "12.7 ~ 0.5", "D1" => "11.4 ~ 0.5", "E" => "4.35 ~ 0.5", "A" => { "max" => "4.10" }, "b" => "0.8 ~ 0.2", "L" => "4.0 ~ 0.5" },
        }.collect do |name, spec|
            next IPC7351::Crystal.new(name, spec, @settings, @env)
        end
    end

    def te_sm_x
        # TE Connectivity SM_2, _3, _5
        {
            "SM_2" => { "D" => { "max" =>  "7.9"}, "D1" =>  "6.7 ~ 0.3", "E" => "4.0 ~ 0.3", "A" => "3.55 ~ 0.3", "b" => "1.4 ~ 0.3", "L" => "1.5 ~ 0.3"},
            "SM_3" => { "D" => { "max" => "12.0"}, "D1" => "10.5 ~ 0.3", "E" => "5.5 ~ 0.3", "A" => "5.0  ~ 0.3", "b" => "1.7 ~ 0.3", "L" => "2.3 ~ 0.3"},
            "SM_5" => { "D" => { "max" => "17.0"}, "D1" => "13.5 ~ 0.3", "E" => "7.3 ~ 0.3", "A" => "6.8  ~ 0.3", "b" => "1.7 ~ 0.3", "L" => "2.5 ~ 0.3"},
        }.collect do |name, spec|
            next IPC7351::MoldedBody.new("RES", name, nil, spec, @settings, @env)
        end
    end

    def ts_dbls
        # Taiwan Semiconductor DBLS package
        {
            "DBLS" => {
                        "pins" => 4,
                        "spec" => {
                                    "D" => "8.13 .. 8.51", "E" => "9.80 .. 10.30", "E1" => "6.20 .. 6.50",
                                    "A" => { "max" => "2.60" }, "A1" => "0.076 .. 0.33",
                                    "b" => "1.02 .. 1.20", "e" => "5.00 ..  5.20", "L"  => "1.02 .. 1.53"}
            }
        }.collect do |name, spec|
            next IPC7351::SO.new("SOP", name, spec["pins"], spec["spec"], @settings, @env)
        end
    end
end

default_settings = IPC7351::Settings.new({
    "environment"               => "N",

    "tolerance.fabrication"     => 0.10,
    "tolerance.placement"       => 0.05,

    "pads.rounding.size"        => 0.05,
    "pads.rounding.placement"   => 0.05,
    "pads.clearance.pad"        => 0.20,
    "pads.clearance.tab"        => 0.20,

    "copper.color"              => "#ff4040",

    "soldermask.color"          => "#c0ff40",
    "soldermask.expansion"      => 0.00,

    "stencil.color"             => "#2020ff",
    "stencil.expansion"         => -0.05,

    "silkscreen.color"          => "#ffffff",
    "silkscreen.rounding"       => 0.01,
    "silkscreen.body"           => "max",

    "assembly.width"            => 0.12,
    "assembly.color"            => "#00ffff",
    "assembly.rounding"         => 0.01,
    "assembly.body"             => "max",

    "courtyard.width"           => 0.05,
    "courtyard.color"           => "#808080",
    "courtyard.marker"          => "yes",
})


renderers = [ IPC7351::GedaPCB.new, IPC7351::SVG.new, IPC7351::Textfile.new ]

["L", "N", "M", "HS"].each do |env|
    settings = default_settings.strong_merge({ "environment" => env })
    fpg = FPG.new(settings)

    renderers.each do |renderer|
        renderer.save(*[
            fpg.capc,
            fpg.fusc,
            fpg.ledc,
            fpg.resc,
            fpg.sod,
            fpg.sodfl,
            fpg.diom,
            fpg.indm,
            fpg.ind_misc,
            fpg.sot,
            fpg.soic,
            fpg.soicw,
            fpg.tssop,
            fpg.msop,
            fpg.qfp,
            fpg.crystal,
            fpg.te_sm_x,
            fpg.ts_dbls,
        ].flatten)
    end
end

