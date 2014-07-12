Settings = IPC7351::Settings.new({
    "environment" => "N",

    "tolerance.fabrication" => 0.10,
    "tolerance.placement" => 0.05,

    "pads.rounding.size" => 0.01 ,
    "pads.rounding.placement" => 0.01 ,
    "pads.clearance.pad" => 0.15,
    "pads.clearance.tab" => 0.20,

    "copper.color" => "#ff4040",

    "soldermask.color" => "#c0ff40",
    "soldermask.expansion" => 0.00,

    "stencil.color" => "#2020ff",
    "stencil.expansion" => 0.00,

    "silkscreen.color" => "#ffffff",
    "silkscreen.rounding" => 0.01,
    "silkscreen.body" => "max",

    "assembly.width" => 0.12,
    "assembly.color" => "#00ffff",
    "assembly.rounding" => 0.01,
    "assembly.body" => "max",

    "courtyard.width" => 0.05,
    "courtyard.color" => "#808080",
    "courtyard.marker" => "yes",
})
