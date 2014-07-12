require 'IPC7351'
require 'BaseFootprints'
require 'Footprints'
require 'GedaPCB'
require 'SVG'
require 'Settings'

IPC7351::GedaPCB.new.save(*[
#IPC7351::SVG.new.save(*[
#    IPC7351::SO.soic(14, Settings),
#    IPC7351::Chip.new("RES", { "D" => "2.00 ~ 0.20", "E" => "1.25 ~ 0.20", "A" => "0.45 ~ 0.15", "L" => "0.50 ~ 0.20" }, Settings),
#    IPC7351::Chip.new("RES", { "D" => "2.00 ~ 0.00", "E" => "1.00 ~ 0.00", "A" => "1.00 ~ 0.00", "L" => "0.50 ~ 0.00" }, Settings),
    IPC7351::SO.new("SOP", 8, { "e" => "1 ~ 0", "A" => "1 ~ 0", "A1" => "0 ~ 0", "b" => "0.5 ~ 0.1", "D" => "5 ~ 0.5", "E" => "4 ~ 0.5", "E1" => "3 ~ 0.5", "L" => "1 ~ 0.2" }, Settings),
#    IPC7351::SO.new("SOP", 10, { "e" => "1 ~ 0", "A" => "1 ~ 0", "A1" => "0 ~ 0", "b" => "0.5 ~ 0.1", "D" => "5 ~ 0.5", "E" => "4 ~ 0.5", "E1" => "3 ~ 0.5", "L" => "1 ~ 0.2" }, Settings),
    IPC7351::SO.new("SOP", 10, { "e" => "1 ~ 0", "A" => "1 ~ 0", "A1" => "0 ~ 0", "b" => "0.5 ~ 0.1", "D" => "4.5 ~ 0.5", "E" => "4 ~ 0.5", "E1" => "3 ~ 0.5", "L" => "1 ~ 0.2" }, Settings),
])

