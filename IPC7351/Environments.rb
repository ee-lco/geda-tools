require_relative 'Settings'

module IPC7351
    class Environment < Settings
        def initialize(*args)
            case args.length
            when 2, 3
                type   = args[0]
                name   = args[1]
                inward = args[2] || false
                env  = Environments[name][type]
            when 6, 7
                env    = args
                inward = false
            else
                raise ArgumentError
            end
            jt, jh, js, cyx, cyr, silk_lw, silk_clr = *env
            silk_clr = silk_lw if silk_clr.nil?

            ## @todo quick hack: swap Jt and Jh to support inward leads
            jt, jh = jh, jt if inward

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
            "HS"  => [ 0.30,  0.00,  0.05, 0.20, 0.01, 0.20],
        },
        "Rectangular End Cap Length >= 1.6 mm" => {
            "L"   => [ 0.15,  0.00, -0.05, 0.12, 0.01, 0.10],
            "N"   => [ 0.35,  0.00,  0.00, 0.25, 0.01, 0.12],
            "M"   => [ 0.55,  0.00,  0.05, 0.50, 0.01, 0.15],
            "HS"  => [ 0.35,  0.00,  0.00, 0.50, 0.01, 0.20],
        },
        "Gull Wing Pitch <= 0.625 mm" => {
            "L"   => [ 0.15,  0.05, -0.04, 0.12, 0.01, 0.10],
            "N"   => [ 0.35,  0.15, -0.02, 0.25, 0.01, 0.12],
            "M"   => [ 0.55,  0.25,  0.01, 0.50, 0.01, 0.15],
            "HS"  => [ 0.55,  0.25,  0.01, 0.50, 0.01, 0.20],
        },
        "Gull Wing Pitch > 0.625 mm" => {
            "L"   => [ 0.15,  0.25,  0.01, 0.12, 0.01, 0.10],
            "N"   => [ 0.35,  0.35,  0.03, 0.25, 0.01, 0.12],
            "M"   => [ 0.55,  0.45,  0.05, 0.50, 0.01, 0.15],
            "HS"  => [ 0.35,  0.35,  0.03, 0.50, 0.01, 0.20],
        },
        "Outward L Lead Pitch <= 0.625 mm" => {
            "L"   => [ 0.15,  0.05, -0.04, 0.12, 0.01, 0.10],
            "N"   => [ 0.35,  0.15, -0.02, 0.25, 0.01, 0.12],
            "M"   => [ 0.55,  0.25,  0.01, 0.50, 0.01, 0.15],
            "HS"  => [ 0.55,  0.25,  0.01, 0.50, 0.01, 0.20],
        },
        "Outward L Lead Pitch > 0.625 mm" => {
            "L"   => [ 0.15,  0.05,  0.01, 0.12, 0.01, 0.10],
            "N"   => [ 0.35,  0.15,  0.03, 0.25, 0.01, 0.12],
            "M"   => [ 0.55,  0.25,  0.05, 0.50, 0.01, 0.15],
            "HS"  => [ 0.35,  0.15,  0.03, 0.50, 0.01, 0.20],
        },
        "Inward Flat Ribbon L" => {
            "L"   => [ 0.07,  0.20, -0.10, 0.12, 0.01, 0.10],
            "N"   => [ 0.15,  0.50, -0.05, 0.25, 0.01, 0.12],
            "M"   => [ 0.25,  0.80,  0.01, 0.50, 0.01, 0.15],
            "HS"  => [ 0.15,  0.50, -0.05, 0.50, 0.01, 0.20],
        },
        "Flat Lead" => {
            "L"   => [ 0.10,  0.00, -0.05, 0.12, 0.01, 0.10],
            "N"   => [ 0.20,  0.00,  0.00, 0.15, 0.01, 0.12],
            "M"   => [ 0.30,  0.00,  0.05, 0.20, 0.01, 0.15],
            "HS"  => [ 0.20,  0.00,  0.00, 0.20, 0.01, 0.20],
        },
    };
end

