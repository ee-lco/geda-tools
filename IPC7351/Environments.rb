require_relative 'Settings'

module IPC7351
    class Environment < Settings
        def initialize(type, name, inward = false)
            jt, jh, js, cyx, cyr  = *Environments[name][type]
            ## @todo quick hack: swap Jt and Jh to support inward leads
            jt, jh = jh, jt if inward

            env = {
                "fillet.toe"           => jt,
                "fillet.heel"          => jh,
                "fillet.side"          => js,
                "courtyard.excess"     => cyx,
                "courtyard.rounding"   => cyr,
            }.merge(Environments["Silkscreen"][type])

            super(env)
        end
    end

    Environments = {
        "Silkscreen" => {
            "L"   => { "silkscreen.width" => 0.10,  "silkscreen.clearance" => 0.10, "silkscreen.dot" => 0.30 },
            "N"   => { "silkscreen.width" => 0.12,  "silkscreen.clearance" => 0.12, "silkscreen.dot" => 0.40 },
            "M"   => { "silkscreen.width" => 0.15,  "silkscreen.clearance" => 0.15, "silkscreen.dot" => 0.50 },
            "HS"  => { "silkscreen.width" => 0.205, "silkscreen.clearance" => 0.20, "silkscreen.dot" => 0.40 },
        },

        "Rectangular End Cap Length < 1.6 mm" => {
            "L"   => [ 0.10,  0.00, -0.05, 0.12, 0.01],
            "N"   => [ 0.20,  0.00,  0.00, 0.15, 0.01],
            "M"   => [ 0.30,  0.00,  0.05, 0.20, 0.01],
            "HS"  => [ 0.30,  0.00,  0.05, 0.20, 0.01],
        },
        "Rectangular End Cap Length >= 1.6 mm" => {
            "L"   => [ 0.15,  0.00, -0.05, 0.12, 0.01],
            "N"   => [ 0.35,  0.00,  0.00, 0.25, 0.01],
            "M"   => [ 0.55,  0.00,  0.05, 0.50, 0.01],
            "HS"  => [ 0.35,  0.00,  0.00, 0.50, 0.01],
        },
        "Gull Wing Pitch <= 0.625 mm" => {
            "L"   => [ 0.15,  0.05, -0.04, 0.12, 0.01],
            "N"   => [ 0.35,  0.15, -0.02, 0.25, 0.01],
            "M"   => [ 0.55,  0.25,  0.01, 0.50, 0.01],
            "HS"  => [ 0.55,  0.25,  0.01, 0.50, 0.01],
        },
        "Gull Wing Pitch > 0.625 mm" => {
            "L"   => [ 0.15,  0.25,  0.01, 0.12, 0.01],
            "N"   => [ 0.35,  0.35,  0.03, 0.25, 0.01],
            "M"   => [ 0.55,  0.45,  0.05, 0.50, 0.01],
            "HS"  => [ 0.35,  0.35,  0.03, 0.50, 0.01],
        },
        "Outward L Lead Pitch <= 0.625 mm" => {
            "L"   => [ 0.15,  0.05, -0.04, 0.12, 0.01],
            "N"   => [ 0.35,  0.15, -0.02, 0.25, 0.01],
            "M"   => [ 0.55,  0.25,  0.01, 0.50, 0.01],
            "HS"  => [ 0.55,  0.25,  0.01, 0.50, 0.01],
        },
        "Outward L Lead Pitch > 0.625 mm" => {
            "L"   => [ 0.15,  0.05,  0.01, 0.12, 0.01],
            "N"   => [ 0.35,  0.15,  0.03, 0.25, 0.01],
            "M"   => [ 0.55,  0.25,  0.05, 0.50, 0.01],
            "HS"  => [ 0.35,  0.15,  0.03, 0.50, 0.01],
        },
        "Inward Flat Ribbon L" => {
            "L"   => [ 0.07,  0.20, -0.10, 0.12, 0.01],
            "N"   => [ 0.15,  0.50, -0.05, 0.25, 0.01],
            "M"   => [ 0.25,  0.80,  0.01, 0.50, 0.01],
            "HS"  => [ 0.15,  0.50, -0.05, 0.50, 0.01],
        },
        "Flat Lead" => {
            "L"   => [ 0.10,  0.00, -0.05, 0.12, 0.01],
            "N"   => [ 0.20,  0.00,  0.00, 0.15, 0.01],
            "M"   => [ 0.30,  0.00,  0.05, 0.20, 0.01],
            "HS"  => [ 0.20,  0.00,  0.00, 0.20, 0.01],
        },
    };
end

