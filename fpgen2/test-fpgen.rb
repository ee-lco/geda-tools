require 'fpgen'
include FPGen

spsf = SmdPadStackFactory.new
tpsf = ThtPadStackFactory.new
ipsf = IpcPadStackFactory.new
smd = spsf.make_smd(:rectangle, 2, 1)
tht = tpsf.make_tht(:round, 0.5)
ipc = ipsf.make_ipc(Tolerance.new(10, 10.65), Tolerance.new(0.33, 0.51), Tolerance.new(0.40, 1.27))

txt = Text.new
svg = SVG.new
gbr = Gerber.new
[txt, svg, gbr].each do |rdr|
  rdr.render("testout/smd", smd)
  rdr.render("testout/tht", tht)
end
